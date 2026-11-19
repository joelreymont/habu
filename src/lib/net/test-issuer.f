split-str
\ test-issuer.f - a token issuer for tests on Habu's HTTP server, never a
\ production one: it answers on loopback as Entra v2 does, with OIDC discovery,
\ a JWK Set and a token endpoint for client credentials, On-Behalf-Of and the
\ refresh grant, and signs RS256 with JWS-FIXTURE's key under kid
\ "test-issuer". Issued tokens carry tid "test-tenant" for client credentials.
\ A refresh token is a JWS the issuer signed whose aud is the client id; a
\ refresh answers a new one for the same client, oid and tid. A client stated
\ with an empty secret is public: it presents none and may only refresh. The
\ host states its clients with CLIENT+ and calls ROUTES before HTTP:START; MINT
\ signs any claims text, a refresh token's included.
\ STORAGE CLASS. PROCESS-WIDE: the clients, set up by the first CLIENT+ and
\ written before HTTP serves; the fixture key's n and e, written at load; and
\ rows indexed by HTTP:WORKER-SLOT, whose buffers a slot's first request makes
\ and the process keeps.
\ https://learn.microsoft.com/entra/identity-platform/v2-oauth2-on-behalf-of-flow
\ https://learn.microsoft.com/entra/identity-platform/v2-oauth2-auth-code-flow

require src/lib/error.f
require src/lib/string.f
require src/lib/span.f
require src/lib/fmt.f
require src/lib/memory.f
require src/lib/byte-buffer.f
require src/lib/heap.f
require src/lib/vector.f
require src/lib/base64.f
require src/lib/json.f
require src/lib/utf8-scalar.f
require src/lib/crypto/evp.f
require src/lib/crypto/jws-fixture.f
require src/lib/net/http.f
require src/lib/uri.f

package TEST-ISSUER
private

3600 constant LIFE-S
$100 constant FIRST-CAP

: KID$ ( -- str ) s" test-issuer" ;
: TENANT$ ( -- str ) s" test-tenant" ;

\ The fixture key's n and e as unsigned big-endian bytes; decoded base64url is
\ shorter than its text.
JWS-FIXTURE:MODULUS$ nip BUFFER: MODULUS-ROW
variable MODULUS-U
JWS-FIXTURE:EXPONENT$ nip BUFFER: EXPONENT-ROW
variable EXPONENT-U

: RAW ( ^u8 n ^u8 n -- n )
   {: text:^ tu:n row:^ ru:n :}
   text tu row ru SPAN:MAKE BASE64:DECODE-URL ;

JWS-FIXTURE:MODULUS$ MODULUS-ROW JWS-FIXTURE:MODULUS$ nip RAW MODULUS-U !
JWS-FIXTURE:EXPONENT$ EXPONENT-ROW JWS-FIXTURE:EXPONENT$ nip RAW EXPONENT-U !

: HEADER$ ( -- str ) S\" {\"alg\":\"RS256\",\"kid\":\"test-issuer\",\"typ\":\"JWT\"}" ;

: VEC$ ( ^VEC:vector<u8> -- ^u8 n ) VEC:SPAN SPAN:$ ;
: NEW-VEC ( -- ^VEC:vector<u8> ) HEAP:ALLOCATOR 0 [type] u8 VEC:NEW ;

\ The clients: each id's offset in CLIENTS, its secret right after it.
TYPED-VARIABLE CLIENTS ^VEC:vector<u8>
variable CLIENT-COUNT
DYNAMIC-BUFFER CLIENT-AT n
DYNAMIC-BUFFER ID-LEN n
DYNAMIC-BUFFER SECRET-LEN n

: ID$ ( n -- ^u8 n )
   {: k:n :}
   CLIENTS @ VEC$ drop k CLIENT-AT @ + k ID-LEN @ ;

: SECRET$ ( n -- str )
   {: k:n :}
   CLIENTS @ VEC$ drop k CLIENT-AT @ + k ID-LEN @ + k SECRET-LEN @ ;

: KNOWN? ( str str -- bool )
   {: id:^ iu:n secret:^ su:n :}
   CLIENT-COUNT @ 0 ?do
      i ID$ id iu STR= i SECRET$ secret su STR= and if true unloop exit then
   loop false ;


\ A worker's vectors, by index.
0 constant FORM                     \ the form's wanted values, decoded
1 constant NOTE                     \ a form name, decoded
2 constant SCRATCH                  \ an assertion's or refresh token's decoded claims and signature
3 constant TOKEN                    \ the token issued
4 constant RENEWAL                  \ the refresh token issued
5 constant VECS

\ The form fields /token reads, by index.
0 constant CLIENT-ID
1 constant CLIENT-SECRET
2 constant GRANT-TYPE
3 constant SCOPE
4 constant ASSERTION
5 constant TOKEN-USE
6 constant REFRESH-TOKEN
7 constant FIELDS

HTTP:MAX-WORKERS VECS * TYPED-BUFFER HEADS ^VEC:vector<u8>
\ The claims of the token being issued: a BUF, which the JSON writer fills.
HTTP:MAX-WORKERS BUF:HDR-BYTES * BUFFER: CLAIMS-HEADS
HTTP:MAX-WORKERS TYPED-BUFFER MADE bool
HTTP:MAX-WORKERS TYPED-BUFFER READER JSON:cursor
HTTP:MAX-WORKERS TYPED-BUFFER WRITER JSON:writer
HTTP:MAX-WORKERS TYPED-BUFFER WHY SPAN:span<u8>      \ an error answer's code
HTTP:MAX-WORKERS TYPED-BUFFER ASSERTED SPAN:span<u8> \ the assertion's or refresh token's claims, in SCRATCH
HTTP:MAX-WORKERS FIELDS * TYPED-BUFFER FIELD-AT n    \ a value's offset in FORM, -1 absent
HTTP:MAX-WORKERS FIELDS * TYPED-BUFFER FIELD-LEN n

: SLOT ( -- n ) HTTP:WORKER-SLOT ;
: CURSOR ( -- ^JSON:cursor ) SLOT READER ;
: VEC-OF ( n -- ^VEC:vector<u8> ) SLOT VECS * + HEADS @ ;
: CLAIMS ( -- ^n ) SLOT BUF:HDR-BYTES * CLAIMS-HEADS + CELL-VIEW ;
: CLAIMS$ ( -- ^u8 n ) CLAIMS BUF:SPAN$ BUF:BLEN>N ;
: SLOT-FIELD ( n -- n ) SLOT FIELDS * + ;

: READY ( -- )
   SLOT MADE @ if exit then
   VECS 0 ?do NEW-VEC SLOT VECS * i + HEADS ! loop
   CLAIMS FIRST-CAP BUF:N>BLEN BUF:INIT
   true SLOT MADE ! ;


\ The first index of the byte at or after from, else the text's length.
: FIND ( ^u8 n n n -- n )
   {: t:^ u:n from:n byte:n :}
   u from ?do t i + c@ byte = if i unloop exit then loop u ;

: NAME$ ( n -- str )
   {: k:n :}
   k CLIENT-ID = if s" client_id" exit then
   k CLIENT-SECRET = if s" client_secret" exit then
   k GRANT-TYPE = if s" grant_type" exit then
   k SCOPE = if s" scope" exit then
   k ASSERTION = if s" assertion" exit then
   k TOKEN-USE = if s" requested_token_use" exit then
   s" refresh_token" ;

\ The field a decoded name is, else -1.
: WHICH ( ^u8 n -- n )
   {: a:^ u:n :}
   FIELDS 0 ?do a u i NAME$ STR= if i unloop exit then loop -1 ;

\ One name=value pair, a wanted name's value decoded into FORM; false when the
\ form already holds that name (RFC 6749 3.1).
: PAIR ( ^u8 n -- bool )
   {: p:^ pu:n :}
   p pu 0 [char] = FIND {: eq:n :}
   NOTE VEC-OF 0 VEC:RESIZE
   p eq NOTE VEC-OF URI:FORM-DECODE+
   NOTE VEC-OF VEC$ WHICH {: k:n :}
   k 0< if true exit then
   k SLOT-FIELD FIELD-AT @ 0< 0= if false exit then
   FORM VEC-OF VEC:COUNT {: at:n :}
   eq pu < if p eq + 1+ pu eq - 1- FORM VEC-OF URI:FORM-DECODE+ then
   at k SLOT-FIELD FIELD-AT !
   FORM VEC-OF VEC:COUNT at - k SLOT-FIELD FIELD-LEN !
   true ;

\ False for a form naming a wanted field twice.
: FORM! ( str -- bool )
   {: b:^ bu:n :}
   FORM VEC-OF 0 VEC:RESIZE
   FIELDS 0 ?do -1 i SLOT-FIELD FIELD-AT ! loop
   0 begin dup bu <= while
      {: from:n :}
      b bu from [char] & FIND {: end:n :}
      b from + end from - PAIR 0= if false exit then
      end 1+
   repeat drop true ;


\ A field's decoded value, borrowed from FORM; false when the form lacks it.
: FIELD$ ( n -- str bool )
   SLOT-FIELD {: f:n :}
   f FIELD-AT @ {: at:n :}
   at 0< if s" " false exit then
   FORM VEC-OF VEC$ drop at + f FIELD-LEN @ true ;

: FIELD= ( n str -- bool )
   {: k:n w:^ wu:n :}
   k FIELD$ 0= if 2drop false exit then
   w wu STR= ;

\ A form without client_secret presents a public client's empty one.
: CLIENT? ( -- bool )
   CLIENT-ID FIELD$ 0= if 2drop false exit then {: id:^ iu:n :}
   CLIENT-SECRET FIELD$ drop {: secret:^ su:n :}
   id iu secret su KNOWN? ;

\ Under CLIENT?: the client is a public one, its secret empty.
: PUBLIC? ( -- bool ) CLIENT-SECRET FIELD$ drop nip 0= ;

: OBO? ( -- bool )
   GRANT-TYPE s" urn:ietf:params:oauth:grant-type:jwt-bearer" FIELD=
   TOKEN-USE s" on_behalf_of" FIELD= and ;

\ The scope without a trailing /.default, as Entra names the audience.
: AUDIENCE$ ( -- str )
   SCOPE FIELD$ drop {: s:^ su:n :}
   s su s" /.default" ENDS-WITH? if s su 9 - exit then
   s su ;


\ Malformed JSON or base64url: the assertion's or refresh token's fault.
: MALFORMED? ( n -- bool )
   {: code:n :}
   code E-JSON-READ-LAST >= code E-JSON-READ-FIRST <= and
   code E-BASE64-LAST >= code E-BASE64-FIRST <= and or ;

\ One step over untrusted text: malformed is false, any other throw passes.
: STEP ( [ bool -- bool ] -- bool )
   false swap catch {: ok code:n :}
   code 0= if ok exit then
   code MALFORMED? if false exit then
   code throw ;

\ The reader on an object text's member, false when the text is not an object
\ or lacks the key; the caller closes the reader either way.
: MEMBER ( ^u8 n ^u8 n -- JSON:reader bool )
   {: t:^ tu:n k:^ ku:n :}
   CURSOR t tu JSON:READ-INIT
   JSON:READ-NEXT JSON:T-OBJ <> if false exit then
   k ku JSON:READ-FIND-KEY ;

: MEMBER-IS? ( str str str -- bool )
   {: t:^ tu:n k:^ ku:n w:^ wu:n :}
   t tu k ku MEMBER 0= if JSON:READ-CLOSE false exit then
   JSON:READ-TOKEN JSON:T-STR <> if JSON:READ-CLOSE false exit then
   w wu JSON:READ-STR-EQ? {: same:bool :} JSON:READ-CLOSE same ;


: MEMBER-INT ( str str -- n bool )
   MEMBER 0= if JSON:READ-CLOSE 0 false exit then
   JSON:READ-TOKEN JSON:T-INT <> if JSON:READ-CLOSE 0 false exit then
   JSON:READ-INT {: v:n :} JSON:READ-CLOSE v true ;

\ A string member as sent, quotes and escapes included; false when absent or
\ not a string.
: MEMBER-RAW ( str str -- str bool )
   MEMBER 0= if JSON:READ-CLOSE s" " false exit then
   JSON:READ-TOKEN JSON:T-STR <> if JSON:READ-CLOSE s" " false exit then
   JSON:READ-SPAN$ {: a:^ u:n :} JSON:READ-CLOSE a 1- u 2 + true ;

\ Whether a text is one JSON object; malformed JSON throws the reader's code.
: OBJECT? ( str -- bool )
   CURSOR -rot JSON:READ-INIT
   JSON:READ-NEXT JSON:T-OBJ = if JSON:READ-FINISH true exit then
   JSON:READ-CLOSE false ;


\ This server's URL with the path; borrowed from the task's SB.
: URL$ ( str -- str )
   {: path:^ pu:n :}
   SB-RESET s" http://127.0.0.1:" SB-APPEND HTTP:PORT FMT:SB-U path pu SB-APPEND SB$ ;

\ The bytes appended as unpadded base64url, whose length is exactly (4u+2)/3.
\ The bytes must not lie in the vector, whose block the growth may move.
: SEGMENT+ ( str ^VEC:vector<u8> -- )
   {: a:^ u:n v:^ :}
   v VEC:COUNT {: old:n :}
   v old u 4 * 2 + 3 / + VEC:ENSURE
   a u v VEC:SPAN old SPAN:SKIP BASE64:ENCODE-URL drop ;

\ The signature over the vector's text appended as its third segment.
: SIGNED+ ( ^VEC:vector<u8> ^u8 NUM:alloc-byte-len -- )
   MEM:ALLOCATION>SPAN SPAN:$ {: v:^ out:^ cap:n :}
   JWS-FIXTURE:PEM$ v VEC$ out cap CRYPTO:RS256-SIGN {: su:n :}
   [char] . v VEC:PUSH c!
   out su v SEGMENT+ ;

public

\ A compact JWS of the claims text, signed RS256 with the fixture key under the
\ issuer's kid, borrowed from the vector until its next write. Claims must not
\ lie in the vector.
: MINT ( str ^VEC:vector<u8> -- str )
   {: claims:^ cu:n v:^ :}
   v 0 VEC:RESIZE
   HEADER$ v SEGMENT+
   [char] . v VEC:PUSH c!
   claims cu v SEGMENT+
   v MODULUS-U @ MEM:BYTES-ALLOC-LEN [: SIGNED+ ;] MEM:WITH-BYTES
   v VEC$ ;

private

: ASSERTED$ ( -- str ) SLOT ASSERTED @ SPAN:$ ;

\ The assertion's or refresh token's claims and signature decoded into
\ SCRATCH, the claims kept in ASSERTED: the signed text's length and the
\ signature, true for three segments and a modulus-wide signature.
: SPLIT ( ^u8 n -- n SPAN:span<u8> bool )
   {: t:^ u:n :}
   SCRATCH VEC-OF {: v:^ :}
   v 0 VEC:RESIZE  v u VEC:ENSURE
   v VEC:SPAN {: room :}
   t u 0 [char] . FIND {: d1:n :}
   t u d1 1+ [char] . FIND {: d2:n :}
   d2 u = if 0 room false exit then
   t u d2 1+ [char] . FIND u <> if 0 room false exit then
   t d1 + 1+ d2 d1 - 1- room BASE64:DECODE-URL {: cn:n :}
   room 0 cn SPAN:SUB SLOT ASSERTED !
   room cn u cn - SPAN:SUB {: rest :}
   t d2 + 1+ u d2 - 1- rest BASE64:DECODE-URL {: sn:n :}
   d2 rest 0 sn SPAN:SUB sn MODULUS-U @ = ;

\ The token in the field, an assertion or a refresh token, verifies against the
\ fixture key, and its claims are one object whose aud is the client id and
\ whose exp is later than now.
: VERIFIED? ( n -- bool )
   FIELD$ 0= if 2drop false exit then {: t:^ u:n :}
   t u SPLIT 0= if drop drop false exit then {: signed:n sig :}
   MODULUS-ROW MODULUS-U @ EXPONENT-ROW EXPONENT-U @
   t signed sig SPAN:$ CRYPTO:RS256-VERIFY? 0= if false exit then
   ASSERTED$ OBJECT? 0= if false exit then
   ASSERTED$ s" aud" CLIENT-ID FIELD$ drop MEMBER-IS? 0= if false exit then
   ASSERTED$ s" exp" MEMBER-INT 0= if drop false exit then
   epoch-seconds > ;


\ The claims every issued token carries; the writer is left inside the object.
: CLAIMS-OPEN ( -- ^JSON:writer )
   epoch-seconds {: now:n :}
   SLOT WRITER CLAIMS JSON:WRITE-OPEN-BUF
   JSON:WRITE-OBJECT-START
   s" iss" s" " URL$ JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" aud" AUDIENCE$ JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" azp" CLIENT-ID FIELD$ drop JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" iat" now JSON:WRITE-FIELD-INT JSON:WRITE-COMMA
   s" nbf" now JSON:WRITE-FIELD-INT JSON:WRITE-COMMA
   s" exp" now LIFE-S + JSON:WRITE-FIELD-INT ;

: CLAIMS-CLOSE ( ^JSON:writer -- )
   JSON:WRITE-OBJECT-END JSON:WRITE-CLOSE ;

\ The client acts as itself.
: CREDENTIALS ( -- )
   CLAIMS-OPEN
   JSON:WRITE-COMMA s" oid" CLIENT-ID FIELD$ drop JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA s" tid" TENANT$ JSON:WRITE-FIELD-S
   CLAIMS-CLOSE ;

\ The assertion's or refresh token's claim copied as sent, when it has one.
: COPIED ( ^JSON:writer ^u8 n -- ^JSON:writer )
   {: k:^ ku:n :}
   ASSERTED$ k ku MEMBER-RAW 0= if 2drop exit then {: v:^ vu:n :}
   JSON:WRITE-COMMA k ku v vu JSON:WRITE-FIELD-RAW ;

\ The client acts for the user of the assertion or refresh token.
: ON-BEHALF ( -- )
   CLAIMS-OPEN s" oid" COPIED s" tid" COPIED CLAIMS-CLOSE ;

\ The refresh token's claims: the client's, for the presented one's oid and tid.
: RENEWAL-CLAIMS ( -- )
   SLOT WRITER CLAIMS JSON:WRITE-OPEN-BUF
   JSON:WRITE-OBJECT-START
   s" aud" CLIENT-ID FIELD$ drop JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" exp" epoch-seconds LIFE-S + JSON:WRITE-FIELD-INT
   s" oid" COPIED s" tid" COPIED CLAIMS-CLOSE ;

: TOKEN-FIELDS ( ^JSON:writer -- ^JSON:writer )
   s" access_token" TOKEN VEC-OF VEC$ JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" token_type" s" Bearer" JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" expires_in" LIFE-S JSON:WRITE-FIELD-INT ;

: ISSUED-BODY ( ^JSON:writer -- ^JSON:writer )
   JSON:WRITE-OBJECT-START TOKEN-FIELDS JSON:WRITE-OBJECT-END ;

: RENEWED-BODY ( ^JSON:writer -- ^JSON:writer )
   JSON:WRITE-OBJECT-START TOKEN-FIELDS JSON:WRITE-COMMA
   s" refresh_token" RENEWAL VEC-OF VEC$ JSON:WRITE-FIELD-S
   JSON:WRITE-OBJECT-END ;

: MINTED ( n -- ) {: into:n :} CLAIMS$ into VEC-OF MINT 2drop ;

: ISSUE ( HTTP:response -- )
   TOKEN MINTED
   [: ISSUED-BODY ;] HTTP:JSON! ;

: RENEW ( HTTP:response -- )
   ON-BEHALF TOKEN MINTED
   RENEWAL-CLAIMS RENEWAL MINTED
   [: RENEWED-BODY ;] HTTP:JSON! ;

: ERROR-BODY ( ^JSON:writer -- ^JSON:writer )
   JSON:WRITE-OBJECT-START s" error" SLOT WHY @ SPAN:$ JSON:WRITE-FIELD-S JSON:WRITE-OBJECT-END ;

\ The error code is a literal.
: REFUSE ( HTTP:response n ^u8 n -- )
   {: answer:HTTP:response status:n why:^ wu:n :}
   why wu SPAN:MAKE SLOT WHY !
   answer status HTTP:STATUS!
   answer [: ERROR-BODY ;] HTTP:JSON! ;

: FORM-TYPE? ( HTTP:request -- bool )
   s" Content-Type" HTTP:HEADER-OF$ 0= if 2drop false exit then {: t:^ u:n :}
   t u 0 [char] ; FIND {: end:n :}
   t end TRIM s" application/x-www-form-urlencoded" STR=CI ;

\ The grant the form asks for, issued or refused as Entra v2 answers; no
\ audience, or one that is not UTF-8 (claims are JSON text), no token
\ (RFC 6749 §5.2). A public client may only refresh.
: GRANT ( HTTP:response -- )
   {: answer:HTTP:response :}
   GRANT-TYPE s" client_credentials" FIELD= {: credentials:bool :}
   GRANT-TYPE s" refresh_token" FIELD= {: refreshing:bool :}
   credentials OBO? or refreshing or 0= if answer 400 s" unsupported_grant_type" REFUSE exit then
   refreshing 0= PUBLIC? and if answer 401 s" invalid_client" REFUSE exit then
   AUDIENCE$ {: aud:^ au:n :}
   au 0= aud au UTF8:VALID? 0= or if answer 400 s" invalid_request" REFUSE exit then
   credentials if CREDENTIALS answer ISSUE exit then
   refreshing if
      [: drop REFRESH-TOKEN VERIFIED? ;] STEP 0= if answer 400 s" invalid_grant" REFUSE exit then
      answer RENEW exit
   then
   [: drop ASSERTION VERIFIED? ;] STEP 0= if answer 400 s" invalid_grant" REFUSE exit then
   ON-BEHALF answer ISSUE ;

: TOKEN-ROUTE ( HTTP:request HTTP:response -- )
   {: asked:HTTP:request answer:HTTP:response :}
   READY
   asked FORM-TYPE? 0= if answer 400 s" invalid_request" REFUSE exit then
   asked HTTP:BODY$ FORM! 0= if answer 400 s" invalid_request" REFUSE exit then
   CLIENT? 0= if answer 401 s" invalid_client" REFUSE exit then
   answer GRANT ;

: MINT-ROUTE ( HTTP:request HTTP:response -- )
   {: asked:HTTP:request answer:HTTP:response :}
   READY
   asked HTTP:BODY$ TOKEN VEC-OF MINT {: t:^ u:n :}
   answer s" Content-Type" s" text/plain" HTTP:HEADER!
   answer t u HTTP:BODY! ;

: DISCOVERY-BODY ( ^JSON:writer -- ^JSON:writer )
   JSON:WRITE-OBJECT-START
   s" issuer" s" " URL$ JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" jwks_uri" s" /keys" URL$ JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" token_endpoint" s" /token" URL$ JSON:WRITE-FIELD-S
   JSON:WRITE-OBJECT-END ;

: KEYS-BODY ( ^JSON:writer -- ^JSON:writer )
   JSON:WRITE-OBJECT-START
   s" keys" JSON:WRITE-KEY JSON:WRITE-ARRAY-START JSON:WRITE-OBJECT-START
   s" kty" s" RSA" JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" use" s" sig" JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" alg" s" RS256" JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" kid" KID$ JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" n" JWS-FIXTURE:MODULUS$ JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" e" JWS-FIXTURE:EXPONENT$ JSON:WRITE-FIELD-S
   JSON:WRITE-OBJECT-END JSON:WRITE-ARRAY-END JSON:WRITE-OBJECT-END ;

public

\ Registers the issuer's routes on the HTTP server being set up.
: ROUTES ( -- )
   s" GET" s" /.well-known/openid-configuration" [: nip [: DISCOVERY-BODY ;] HTTP:JSON! ;] HTTP:ROUTE
   s" GET" s" /keys" [: nip [: KEYS-BODY ;] HTTP:JSON! ;] HTTP:ROUTE
   s" POST" s" /token" [: TOKEN-ROUTE ;] HTTP:ROUTE
   s" POST" s" /mint" [: MINT-ROUTE ;] HTTP:ROUTE ;

\ A client id and secret /token accepts, copied; an empty secret states a
\ public client, which presents none and may only refresh. E-HTTP-STATE while
\ HTTP serves.
: CLIENT+ ( ^u8 n ^u8 n -- )
   {: id:^ iu:n secret:^ su:n :}
   HTTP:RUNNING? if E-HTTP-STATE throw then
   CLIENT-COUNT @ {: k:n :}
   k 0= if NEW-VEC CLIENTS ! then
   k 1+ CLIENT-AT-RESERVE
   k 1+ ID-LEN-RESERVE
   k 1+ SECRET-LEN-RESERVE
   CLIENTS @ VEC:COUNT k CLIENT-AT !
   iu k ID-LEN !
   su k SECRET-LEN !
   id iu CLIENTS @ VEC:APPEND
   secret su CLIENTS @ VEC:APPEND
   k 1+ CLIENT-COUNT ! ;

;package
