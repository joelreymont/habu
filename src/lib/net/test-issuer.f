\ test-issuer.f - a token issuer for tests on Habu's HTTP server, never a
\ production one: it answers on loopback as Entra v2 does, with OIDC discovery,
\ a JWK Set and a token endpoint for client credentials and On-Behalf-Of, and
\ signs RS256 with JWS-FIXTURE's key under kid "test-issuer". Issued tokens
\ carry tid "test-tenant" for client credentials. The host states its clients
\ with CLIENT+ and calls ROUTES before HTTP:START; MINT signs any claims text.
\ STORAGE CLASS. PROCESS-WIDE: the clients, set up by the first CLIENT+ and
\ written before HTTP serves; the fixture key's n and e, written at load; and
\ rows indexed by HTTP:WORKER-SLOT, whose buffers a slot's first request makes
\ and the process keeps.
\ https://learn.microsoft.com/entra/identity-platform/v2-oauth2-on-behalf-of-flow

require src/lib/errors.f
require src/lib/string.f
require src/lib/span.f
require src/lib/fmt.f
require src/lib/memory.f
require src/lib/byte-buffer.f
require src/lib/base64.f
require src/lib/json-read.f
require src/lib/json-write.f
require src/lib/utf8-scalar.f
require src/lib/crypto/evp.f
require src/lib/crypto/jws-fixture.f
require src/lib/net/http.f

package TEST-ISSUER
private

3600 constant LIFE-S                \ an issued token's lifetime, its expires_in
$100 constant FIRST-CAP

: KID$ ( -- ptr u8 n ) s" test-issuer" ;
: TENANT$ ( -- ptr u8 n ) s" test-tenant" ;

\ The fixture key's n and e as unsigned big-endian bytes; decoded base64url is
\ shorter than its text.
JWS-FIXTURE:MODULUS$ nip BUFFER: MODULUS-ROW
variable MODULUS-U
JWS-FIXTURE:EXPONENT$ nip BUFFER: EXPONENT-ROW
variable EXPONENT-U

: RAW ( ptr u8 n ptr u8 n -- n )
   {: text:ptr tu:n row:ptr ru:n :}
   text tu row ru SPAN:MAKE BASE64:DECODE-URL ;

JWS-FIXTURE:MODULUS$ MODULUS-ROW JWS-FIXTURE:MODULUS$ nip RAW MODULUS-U !
JWS-FIXTURE:EXPONENT$ EXPONENT-ROW JWS-FIXTURE:EXPONENT$ nip RAW EXPONENT-U !

: HEADER$ ( -- ptr u8 n ) S\" {\"alg\":\"RS256\",\"kid\":\"test-issuer\",\"typ\":\"JWT\"}" ;

: BUF$ ( ptr n -- ptr u8 n ) BUF:SPAN$ BUF:BLEN>N ;
: BUF+ ( ptr u8 n ptr n -- ) {: a:ptr u:n buf:ptr :} a u BUF:N>BLEN buf BUF:APPEND-SPAN ;

\ The clients: each id's offset in CLIENTS, its secret right after it.
create CLIENTS-HEAD BUF:HDR-BYTES allot
: CLIENTS ( -- ptr n ) CLIENTS-HEAD CELL-VIEW ;
variable CLIENT-COUNT
DYNAMIC-BUFFER CLIENT-AT n
DYNAMIC-BUFFER ID-LEN n
DYNAMIC-BUFFER SECRET-LEN n

: ID$ ( n -- ptr u8 n )
   {: k:n :}
   CLIENTS BUF$ drop k CLIENT-AT @ + k ID-LEN @ ;

: SECRET$ ( n -- ptr u8 n )
   {: k:n :}
   CLIENTS BUF$ drop k CLIENT-AT @ + k ID-LEN @ + k SECRET-LEN @ ;

: KNOWN? ( ptr u8 n ptr u8 n -- bool )
   {: id:ptr iu:n secret:ptr su:n :}
   CLIENT-COUNT @ 0 ?do
      i ID$ id iu STR= i SECRET$ secret su STR= and if true unloop exit then
   loop false ;


\ A worker's buffers, by index.
0 constant FORM                     \ the form's wanted values, decoded
1 constant NOTE                     \ a form name, decoded
2 constant SCRATCH                  \ an assertion's decoded claims and signature
3 constant CLAIMS                   \ the claims of the token being issued
4 constant TOKEN                    \ the token issued
5 constant BUFS

\ The form fields /token reads, by index.
0 constant CLIENT-ID
1 constant CLIENT-SECRET
2 constant GRANT-TYPE
3 constant SCOPE
4 constant ASSERTION
5 constant TOKEN-USE
6 constant FIELDS

HTTP:MAX-WORKERS BUFS * BUF:HDR-BYTES * BUFFER: HEADS
HTTP:MAX-WORKERS TYPED-BUFFER MADE bool
HTTP:MAX-WORKERS TYPED-BUFFER READER JR:cursor
HTTP:MAX-WORKERS TYPED-BUFFER WRITER JSON-WRITE:writer
HTTP:MAX-WORKERS TYPED-BUFFER WHY SPAN:span<u8>      \ an error answer's code
HTTP:MAX-WORKERS TYPED-BUFFER ASSERTED SPAN:span<u8> \ the assertion's claims, in SCRATCH
HTTP:MAX-WORKERS FIELDS * TYPED-BUFFER FIELD-AT n    \ a value's offset in FORM, -1 absent
HTTP:MAX-WORKERS FIELDS * TYPED-BUFFER FIELD-LEN n

: SLOT ( -- n ) HTTP:WORKER-SLOT ;
: CURSOR ( -- ptr JR:cursor ) SLOT READER ;
: BUF-OF ( n -- ptr n ) SLOT BUFS * + BUF:HDR-BYTES * HEADS + CELL-VIEW ;
: SLOT-FIELD ( n -- n ) SLOT FIELDS * + ;

: READY ( -- )
   SLOT MADE @ if exit then
   BUFS 0 ?do i BUF-OF FIRST-CAP BUF:N>BLEN BUF:INIT loop
   true SLOT MADE ! ;


\ The first index of the byte at or after from, else the text's length.
: FIND ( ptr u8 n n n -- n )
   {: t:ptr u:n from:n byte:n :}
   u from ?do t i + c@ byte = if i unloop exit then loop u ;

\ The byte at at decoded, and how many bytes it took: + is a space and %XX a
\ byte; a % without two hex digits after it is itself (WHATWG URL 1.3).
: DECODED ( ptr u8 n n -- n n )
   {: a:ptr u:n at:n :}
   a at + c@ {: c:n :}
   c [char] + = if 32 1 exit then
   c [char] % <> if c 1 exit then
   at 2 + u >= if c 1 exit then
   a at + 1+ c@ {: hi:n :}
   a at + 2 + c@ {: lo:n :}
   hi STR-HEX-DIGIT? lo STR-HEX-DIGIT? and 0= if c 1 exit then
   hi STR-HEX-DIGIT-VALUE 16 * lo STR-HEX-DIGIT-VALUE + 3 ;

\ application/x-www-form-urlencoded text appended decoded.
: DECODE+ ( ptr u8 n ptr n -- )
   {: a:ptr u:n buf:ptr :}
   0 begin dup u < while
      {: at:n :}
      a u at DECODED {: byte:n took:n :}
      byte buf BUF:APPEND-BYTE
      at took +
   repeat drop ;

: NAME$ ( n -- ptr u8 n )
   {: k:n :}
   k CLIENT-ID = if s" client_id" exit then
   k CLIENT-SECRET = if s" client_secret" exit then
   k GRANT-TYPE = if s" grant_type" exit then
   k SCOPE = if s" scope" exit then
   k ASSERTION = if s" assertion" exit then
   s" requested_token_use" ;

\ The field a decoded name is, else -1.
: WHICH ( ptr u8 n -- n )
   {: a:ptr u:n :}
   FIELDS 0 ?do a u i NAME$ STR= if i unloop exit then loop -1 ;

\ One name=value pair, a wanted name's value decoded into FORM; false when the
\ form already holds that name (RFC 6749 3.1).
: PAIR ( ptr u8 n -- bool )
   {: p:ptr pu:n :}
   p pu 0 [char] = FIND {: eq:n :}
   NOTE BUF-OF BUF:CLEAR
   p eq NOTE BUF-OF DECODE+
   NOTE BUF-OF BUF$ WHICH {: k:n :}
   k 0< if true exit then
   k SLOT-FIELD FIELD-AT @ 0< 0= if false exit then
   FORM BUF-OF BUF:LEN@ BUF:BLEN>N {: at:n :}
   eq pu < if p eq + 1+ pu eq - 1- FORM BUF-OF DECODE+ then
   at k SLOT-FIELD FIELD-AT !
   FORM BUF-OF BUF:LEN@ BUF:BLEN>N at - k SLOT-FIELD FIELD-LEN !
   true ;

\ False for a form naming a wanted field twice.
: FORM! ( ptr u8 n -- bool )
   {: b:ptr bu:n :}
   FORM BUF-OF BUF:CLEAR
   FIELDS 0 ?do -1 i SLOT-FIELD FIELD-AT ! loop
   0 begin dup bu <= while
      {: from:n :}
      b bu from [char] & FIND {: end:n :}
      b from + end from - PAIR 0= if false exit then
      end 1+
   repeat drop true ;


\ A field's decoded value, borrowed from FORM; false when the form lacks it.
: FIELD$ ( n -- ptr u8 n bool )
   SLOT-FIELD {: f:n :}
   f FIELD-AT @ {: at:n :}
   at 0< if s" " false exit then
   FORM BUF-OF BUF$ drop at + f FIELD-LEN @ true ;

: FIELD= ( n ptr u8 n -- bool )
   {: k:n w:ptr wu:n :}
   k FIELD$ 0= if 2drop false exit then
   w wu STR= ;

: CLIENT? ( -- bool )
   CLIENT-ID FIELD$ 0= if 2drop false exit then {: id:ptr iu:n :}
   CLIENT-SECRET FIELD$ 0= if 2drop false exit then {: secret:ptr su:n :}
   id iu secret su KNOWN? ;

: OBO? ( -- bool )
   GRANT-TYPE s" urn:ietf:params:oauth:grant-type:jwt-bearer" FIELD=
   TOKEN-USE s" on_behalf_of" FIELD= and ;

\ The scope without a trailing /.default, as Entra names the audience.
: AUDIENCE$ ( -- ptr u8 n )
   SCOPE FIELD$ drop {: s:ptr su:n :}
   s su s" /.default" ENDS-WITH? if s su 9 - exit then
   s su ;


\ Malformed JSON or base64url: the assertion's fault.
: MALFORMED? ( n -- bool )
   {: code:n :}
   code E-JR-LAST >= code E-JR-FIRST <= and
   code E-BASE64-LAST >= code E-BASE64-FIRST <= and or ;

\ One step over untrusted text: malformed is false, any other throw passes.
: STEP ( [ bool -- bool ] -- bool )
   false swap catch {: ok:bool code:n :}
   code 0= if ok exit then
   code MALFORMED? if false exit then
   code throw ;

\ The reader on an object text's member, false when the text is not an object
\ or lacks the key; the caller closes the reader either way.
: MEMBER ( ptr u8 n ptr u8 n -- JR:reader bool )
   {: t:ptr tu:n k:ptr ku:n :}
   CURSOR t tu JR:INIT
   JR:NEXT JR:T-OBJ <> if false exit then
   k ku JR:FIND-KEY ;

: MEMBER-IS? ( ptr u8 n ptr u8 n ptr u8 n -- bool )
   {: t:ptr tu:n k:ptr ku:n w:ptr wu:n :}
   t tu k ku MEMBER 0= if JR:CLOSE false exit then
   JR:TOKEN JR:T-STR <> if JR:CLOSE false exit then
   w wu JR:STR-EQ? {: same:bool :} JR:CLOSE same ;


: MEMBER-INT ( ptr u8 n ptr u8 n -- n bool )
   MEMBER 0= if JR:CLOSE 0 false exit then
   JR:TOKEN JR:T-INT <> if JR:CLOSE 0 false exit then
   JR:INT {: v:n :} JR:CLOSE v true ;

\ A string member as sent, quotes and escapes included, for FIELD-RAW; false
\ when absent or not a string.
: MEMBER-RAW ( ptr u8 n ptr u8 n -- ptr u8 n bool )
   MEMBER 0= if JR:CLOSE s" " false exit then
   JR:TOKEN JR:T-STR <> if JR:CLOSE s" " false exit then
   JR:SPAN$ {: a:ptr u:n :} JR:CLOSE a 1- u 2 + true ;

\ Whether a text is one JSON object; malformed JSON throws the reader's code.
: OBJECT? ( ptr u8 n -- bool )
   CURSOR -rot JR:INIT
   JR:NEXT JR:T-OBJ = if JR:FINISH true exit then
   JR:CLOSE false ;


\ This server's URL with the path; borrowed from the task's SB.
: URL$ ( ptr u8 n -- ptr u8 n )
   {: path:ptr pu:n :}
   SB-RESET s" http://127.0.0.1:" SB-APPEND HTTP:PORT FMT:SB-U path pu SB-APPEND SB$ ;

\ The bytes appended as unpadded base64url, whose length is exactly (4u+2)/3.
: SEGMENT+ ( ptr u8 n ptr n -- )
   {: a:ptr u:n buf:ptr :}
   buf u 4 * 2 + 3 / BUF:N>BLEN BUF:GROW {: room :}
   a u room BASE64:ENCODE-URL drop ;

\ The signature over the BUF's text appended as its third segment; the bytes
\ are WITH-BYTES's scratch.
: SIGNED+ ( ptr n ptr u8 NUM:alloc-byte-len -- )
   MEM:ALLOCATION>SPAN SPAN:$ {: buf:ptr out:ptr cap:n :}
   JWS-FIXTURE:PEM$ buf BUF$ out cap CRYPTO:RS256-SIGN {: su:n :}
   [char] . buf BUF:APPEND-BYTE
   out su buf SEGMENT+ ;

public

\ A compact JWS of the claims text, signed RS256 with the fixture key under the
\ issuer's kid, borrowed from the BUF until its next write. Claims lying in the
\ BUF throw E-BUF-BOUNDS.
: MINT ( ptr u8 n ptr n -- ptr u8 n )
   {: claims:ptr cu:n buf:ptr :}
   claims cu buf BUF:BORROWED? if E-BUF-BOUNDS throw then
   buf BUF:CLEAR
   HEADER$ buf SEGMENT+
   [char] . buf BUF:APPEND-BYTE
   claims cu buf SEGMENT+
   buf MODULUS-U @ MEM:BYTES-ALLOC-LEN [: SIGNED+ ;] MEM:WITH-BYTES
   buf BUF$ ;

private

: ASSERTED$ ( -- ptr u8 n ) SLOT ASSERTED @ SPAN:$ ;

\ The assertion's claims and signature decoded into SCRATCH, the claims kept in
\ ASSERTED: the signed text's length and the signature, true for three segments
\ and a modulus-wide signature.
: SPLIT ( ptr u8 n -- n SPAN:span<u8> bool )
   {: t:ptr u:n :}
   SCRATCH BUF-OF BUF:CLEAR
   SCRATCH BUF-OF u BUF:N>BLEN BUF:GROW {: room :}
   t u 0 [char] . FIND {: d1:n :}
   t u d1 1+ [char] . FIND {: d2:n :}
   d2 u = if 0 room false exit then
   t u d2 1+ [char] . FIND u <> if 0 room false exit then
   t d1 + 1+ d2 d1 - 1- room BASE64:DECODE-URL {: cn:n :}
   room 0 cn SPAN:SUB SLOT ASSERTED !
   room cn u cn - SPAN:SUB {: rest :}
   t d2 + 1+ u d2 - 1- rest BASE64:DECODE-URL {: sn:n :}
   d2 rest 0 sn SPAN:SUB sn MODULUS-U @ = ;

\ The assertion verifies against the fixture key, and its claims are one
\ object whose aud is the client id and whose exp is later than now.
: VERIFIED? ( -- bool )
   ASSERTION FIELD$ 0= if 2drop false exit then {: t:ptr u:n :}
   t u SPLIT 0= if drop drop false exit then {: signed:n sig :}
   MODULUS-ROW MODULUS-U @ EXPONENT-ROW EXPONENT-U @
   t signed sig SPAN:$ CRYPTO:RS256-VERIFY? 0= if false exit then
   ASSERTED$ OBJECT? 0= if false exit then
   ASSERTED$ s" aud" CLIENT-ID FIELD$ drop MEMBER-IS? 0= if false exit then
   ASSERTED$ s" exp" MEMBER-INT 0= if drop false exit then
   epoch-seconds > ;


\ The claims every issued token carries; the writer is left inside the object.
: CLAIMS-OPEN ( -- ptr JSON-WRITE:writer )
   epoch-seconds {: now:n :}
   SLOT WRITER CLAIMS BUF-OF JSON-WRITE:OPEN-BUF
   JSON-WRITE:OBJECT-START
   s" iss" s" " URL$ JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" aud" AUDIENCE$ JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" azp" CLIENT-ID FIELD$ drop JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" iat" now JSON-WRITE:FIELD-INT JSON-WRITE:COMMA
   s" nbf" now JSON-WRITE:FIELD-INT JSON-WRITE:COMMA
   s" exp" now LIFE-S + JSON-WRITE:FIELD-INT ;

: CLAIMS-CLOSE ( ptr JSON-WRITE:writer -- )
   JSON-WRITE:OBJECT-END JSON-WRITE:CLOSE ;

\ The client acts as itself.
: CREDENTIALS ( -- )
   CLAIMS-OPEN
   JSON-WRITE:COMMA s" oid" CLIENT-ID FIELD$ drop JSON-WRITE:FIELD-S
   JSON-WRITE:COMMA s" tid" TENANT$ JSON-WRITE:FIELD-S
   CLAIMS-CLOSE ;

\ The assertion's claim copied as sent, when it has one.
: COPIED ( ptr JSON-WRITE:writer ptr u8 n -- ptr JSON-WRITE:writer )
   {: k:ptr ku:n :}
   ASSERTED$ k ku MEMBER-RAW 0= if 2drop exit then {: v:ptr vu:n :}
   JSON-WRITE:COMMA k ku v vu JSON-WRITE:FIELD-RAW ;

\ The client acts for the assertion's user.
: ON-BEHALF ( -- )
   CLAIMS-OPEN s" oid" COPIED s" tid" COPIED CLAIMS-CLOSE ;

: ISSUED-BODY ( ptr JSON-WRITE:writer -- ptr JSON-WRITE:writer )
   JSON-WRITE:OBJECT-START
   s" access_token" TOKEN BUF-OF BUF$ JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" token_type" s" Bearer" JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" expires_in" LIFE-S JSON-WRITE:FIELD-INT
   JSON-WRITE:OBJECT-END ;

: ISSUE ( HTTP:response -- )
   CLAIMS BUF-OF BUF$ TOKEN BUF-OF MINT 2drop
   [: ISSUED-BODY ;] HTTP:JSON! ;

: ERROR-BODY ( ptr JSON-WRITE:writer -- ptr JSON-WRITE:writer )
   JSON-WRITE:OBJECT-START s" error" SLOT WHY @ SPAN:$ JSON-WRITE:FIELD-S JSON-WRITE:OBJECT-END ;

\ The error code is a literal.
: REFUSE ( HTTP:response n ptr u8 n -- )
   {: answer:HTTP:response status:n why:ptr wu:n :}
   why wu SPAN:MAKE SLOT WHY !
   answer status HTTP:STATUS!
   answer [: ERROR-BODY ;] HTTP:JSON! ;

\ The media type, parameters aside, is a form's, compared case-insensitively.
: FORM-TYPE? ( HTTP:request -- bool )
   s" Content-Type" HTTP:HEADER-OF$ 0= if 2drop false exit then {: t:ptr u:n :}
   t u 0 [char] ; FIND {: end:n :}
   t end TRIM s" application/x-www-form-urlencoded" STR=CI ;

\ The grant the form asks for, issued or refused as Entra v2 answers; no
\ audience, or one that is not UTF-8 (claims are JSON text), no token
\ (RFC 6749 §5.2).
: GRANT ( HTTP:response -- )
   {: answer:HTTP:response :}
   GRANT-TYPE s" client_credentials" FIELD= {: credentials:bool :}
   credentials OBO? or 0= if answer 400 s" unsupported_grant_type" REFUSE exit then
   AUDIENCE$ {: aud:ptr au:n :}
   au 0= aud au UTF8:VALID? 0= or if answer 400 s" invalid_request" REFUSE exit then
   credentials if CREDENTIALS answer ISSUE exit then
   [: drop VERIFIED? ;] STEP 0= if answer 400 s" invalid_grant" REFUSE exit then
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
   asked HTTP:BODY$ TOKEN BUF-OF MINT {: t:ptr u:n :}
   answer s" Content-Type" s" text/plain" HTTP:HEADER!
   answer t u HTTP:BODY! ;

: DISCOVERY-BODY ( ptr JSON-WRITE:writer -- ptr JSON-WRITE:writer )
   JSON-WRITE:OBJECT-START
   s" issuer" s" " URL$ JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" jwks_uri" s" /keys" URL$ JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" token_endpoint" s" /token" URL$ JSON-WRITE:FIELD-S
   JSON-WRITE:OBJECT-END ;

: KEYS-BODY ( ptr JSON-WRITE:writer -- ptr JSON-WRITE:writer )
   JSON-WRITE:OBJECT-START
   s" keys" JSON-WRITE:KEY JSON-WRITE:ARRAY-START JSON-WRITE:OBJECT-START
   s" kty" s" RSA" JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" use" s" sig" JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" alg" s" RS256" JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" kid" KID$ JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" n" JWS-FIXTURE:MODULUS$ JSON-WRITE:FIELD-S JSON-WRITE:COMMA
   s" e" JWS-FIXTURE:EXPONENT$ JSON-WRITE:FIELD-S
   JSON-WRITE:OBJECT-END JSON-WRITE:ARRAY-END JSON-WRITE:OBJECT-END ;

public

\ Registers the issuer's routes on the HTTP server being set up.
: ROUTES ( -- )
   s" GET" s" /.well-known/openid-configuration" [: nip [: DISCOVERY-BODY ;] HTTP:JSON! ;] HTTP:ROUTE
   s" GET" s" /keys" [: nip [: KEYS-BODY ;] HTTP:JSON! ;] HTTP:ROUTE
   s" POST" s" /token" [: TOKEN-ROUTE ;] HTTP:ROUTE
   s" POST" s" /mint" [: MINT-ROUTE ;] HTTP:ROUTE ;

\ A client id and secret /token accepts, copied. E-HTTP-STATE while HTTP
\ serves.
: CLIENT+ ( ptr u8 n ptr u8 n -- )
   {: id:ptr iu:n secret:ptr su:n :}
   HTTP:RUNNING? if E-HTTP-STATE throw then
   CLIENT-COUNT @ {: k:n :}
   k 0= if CLIENTS FIRST-CAP BUF:N>BLEN BUF:INIT then
   k 1+ CLIENT-AT-RESERVE
   k 1+ ID-LEN-RESERVE
   k 1+ SECRET-LEN-RESERVE
   CLIENTS BUF:LEN@ BUF:BLEN>N k CLIENT-AT !
   iu k ID-LEN !
   su k SECRET-LEN !
   id iu CLIENTS BUF+
   secret su CLIENTS BUF+
   k 1+ CLIENT-COUNT ! ;

;package
