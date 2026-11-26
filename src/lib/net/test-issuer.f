\ test-issuer.f - a token issuer for tests on Habu's HTTP server, never a
\ production one: it answers on loopback as Entra v2 does, with OIDC discovery,
\ a JWK Set, an authorization endpoint and a token endpoint for client
\ credentials, On-Behalf-Of, the authorization code grant and the refresh
\ grant, and signs RS256 with JWS-FIXTURE's key under kid "test-issuer".
\ Issued tokens carry tid "test-tenant" for client credentials and codes.
\ /authorize signs the person in at once as the login_hint's oid (a fixed one
\ without it; "denied" declines) and redirects with a code, a JWS the issuer
\ signed whose use is "code", which holds the request's redirect URI, PKCE
\ challenge (S256 only), nonce and scope for 600 s. The code grant redeems it
\ for a confidential client when the redirect URI matches and the verifier
\ hashes to the challenge, answering an access token, a refresh token and an
\ ID token carrying the nonce; a code may be redeemed again until it expires.
\ A refresh token is a JWS the issuer signed whose aud is the client id; a
\ refresh answers a new one for the same client, oid and tid, and rejects a
\ code. A client stated with an empty secret is public: it presents none,
\ may only refresh and has no codes. The host states its clients with CLIENT+
\ and calls ROUTES before HTTP:START; MINT signs any claims text, a refresh
\ token's included. LIFE! sets the seconds each token minted from then on
\ lives, 3600 until set, while HTTP serves as well. FORGED! true makes each ID
\ token minted from then on carry a signature the key did not make, false
\ until set, while HTTP serves as well.
\ STORAGE CLASS. PROCESS-WIDE: the clients, set up by the first CLIENT+ and
\ written before HTTP serves; the lifetime LIFE! writes; the flag FORGED!
\ writes; the fixture key's n and e, written at load; and rows indexed by
\ HTTP:WORKER-SLOT, whose buffers a slot's first request makes and the process
\ keeps.
\ https://learn.microsoft.com/entra/identity-platform/v2-oauth2-on-behalf-of-flow
\ https://learn.microsoft.com/entra/identity-platform/v2-oauth2-auth-code-flow

require src/lib/error.f
require src/lib/string.f
require src/lib/span.f
require src/lib/fmt.f
require src/lib/heap.f
require src/lib/vector.f
require src/lib/base64.f
require src/lib/json.f
require src/lib/utf8-scalar.f
require src/lib/crypto/evp.f
require src/lib/crypto/jws-fixture.f
require src/lib/net/http.f
require src/lib/uri.f
require src/lib/net/http-arena.f
require src/lib/net/http-request.f
require src/lib/net/http-response.f
require src/lib/net/http-router.f
require src/lib/json/read.f
require src/lib/json/value.f
require src/lib/json/write.f

package TEST-ISSUER
private

variable LIFE-S
3600 LIFE-S !
TYPED-VARIABLE FORGING bool
600 constant CODE-S                 \ an authorization code's lifetime
$20 constant DIGEST-BYTES           \ a SHA-256 digest
43 constant CHALLENGE-LEN           \ a digest in unpadded base64url

: KID$ ( -- str ) s" test-issuer" ;
: TENANT$ ( -- str ) s" test-tenant" ;

\ The fixture key's n and e as unsigned big-endian bytes; decoded base64url is
\ shorter than its text.
JWS-FIXTURE:MODULUS$ STR:LEN BUFFER: MODULUS-ROW
variable MODULUS-U
JWS-FIXTURE:EXPONENT$ STR:LEN BUFFER: EXPONENT-ROW
variable EXPONENT-U

: RAW ( str ^u8 n -- n )
   {: text:str row:^ ru:n :}
   text row ru SPAN:MAKE BASE64:DECODE-URL ;

JWS-FIXTURE:MODULUS$ MODULUS-ROW JWS-FIXTURE:MODULUS$ STR:LEN RAW MODULUS-U !
JWS-FIXTURE:EXPONENT$ EXPONENT-ROW JWS-FIXTURE:EXPONENT$ STR:LEN RAW EXPONENT-U !

: HEADER$ ( -- str ) S\" {\"alg\":\"RS256\",\"kid\":\"test-issuer\",\"typ\":\"JWT\"}" ;

: VEC$ ( ^VEC:vector<u8> -- str ) VEC:BYTES ;
: NEW-VEC ( -- ^VEC:vector<u8> ) HEAP:ALLOCATOR 0 [type] u8 VEC:NEW ;

\ The clients: each id's offset in CLIENTS, its secret right after it.
TYPED-VARIABLE CLIENTS ^VEC:vector<u8>
variable CLIENT-COUNT
DYNAMIC-BUFFER CLIENT-AT n
DYNAMIC-BUFFER ID-LEN n
DYNAMIC-BUFFER SECRET-LEN n

: ID$ ( n -- str )
   {: k:n :}
   CLIENTS @ VEC$ STR:DATA k CLIENT-AT @ + k ID-LEN @ >str ;

: SECRET$ ( n -- str )
   {: k:n :}
   CLIENTS @ VEC$ STR:DATA k CLIENT-AT @ + k ID-LEN @ +
   k SECRET-LEN @ >str ;

: KNOWN? ( str str -- bool )
   {: id:str secret:str :}
   CLIENT-COUNT @ 0 ?do
      i ID$ id STR:= i SECRET$ secret STR:= and if true unloop exit then
   loop false ;

\ A stated client with a secret, which /authorize hands codes.
: CONFIDENTIAL? ( str -- bool )
   {: id:str :}
   CLIENT-COUNT @ 0 ?do
      i ID$ id STR:= i SECRET$ STR:LEN 0<> and if true unloop exit then
   loop false ;


\ A worker's vectors, by index.
0 constant FORM                     \ the form's wanted values, decoded
1 constant NOTE                     \ a form name, decoded; then a redirect, digest or audience
2 constant SCRATCH                  \ an assertion's or refresh token's decoded claims and signature
3 constant TOKEN                    \ the token issued
4 constant RENEWAL                  \ the refresh token issued
5 constant CLAIMED                  \ the claims of the token being issued, which the JSON writer fills
6 constant IDENTITY                 \ the ID token issued
7 constant VECS

\ The form fields /token reads and the query fields /authorize reads, by index.
0 constant CLIENT-ID
1 constant CLIENT-SECRET
2 constant GRANT-TYPE
3 constant SCOPE
4 constant ASSERTION
5 constant TOKEN-USE
6 constant REFRESH-TOKEN
7 constant AUTH-CODE
8 constant VERIFIER
9 constant REDIRECT-URI
10 constant RESPONSE-TYPE
11 constant CLIENT-STATE
12 constant NONCE
13 constant CHALLENGE
14 constant CHALLENGE-METHOD
15 constant LOGIN-HINT
16 constant FIELDS

HTTP:MAX-WORKERS VECS * TYPED-BUFFER HEADS ^VEC:vector<u8>
HTTP:MAX-WORKERS TYPED-BUFFER MADE bool
HTTP:MAX-WORKERS TYPED-BUFFER WRITER JSON:writer
HTTP:MAX-WORKERS TYPED-BUFFER WHY SPAN:span<u8>      \ an error answer's code
HTTP:MAX-WORKERS TYPED-BUFFER PRESENTED JSON:value   \ the assertion's or refresh token's claims, parsed, in SCRATCH
HTTP:MAX-WORKERS FIELDS * TYPED-BUFFER FIELD-AT n    \ a value's offset in FORM, -1 absent
HTTP:MAX-WORKERS FIELDS * TYPED-BUFFER FIELD-LEN n

: SLOT ( -- n ) HTTP:WORKER-SLOT ;
: VEC-OF ( n -- ^VEC:vector<u8> ) SLOT VECS * + HEADS @ ;
: CLAIMS ( -- ^VEC:vector<u8> ) CLAIMED VEC-OF ;
: CLAIMS$ ( -- str ) CLAIMS VEC$ ;
: SLOT-FIELD ( n -- n ) SLOT FIELDS * + ;

: READY ( -- )
   SLOT MADE @ if exit then
   VECS 0 ?do NEW-VEC SLOT VECS * i + HEADS ! loop
   true SLOT MADE ! ;


\ The first index of the byte at or after from, else the text's length.
: FIND ( str n n -- n )
   {: s:str from:n byte:n :}
   s STR:LEN from ?do
      s STR:DATA i + c@ byte = if i unloop exit then
   loop s STR:LEN ;

: NAME$ ( n -- str )
   {: k:n :}
   k CLIENT-ID = if s" client_id" exit then
   k CLIENT-SECRET = if s" client_secret" exit then
   k GRANT-TYPE = if s" grant_type" exit then
   k SCOPE = if s" scope" exit then
   k ASSERTION = if s" assertion" exit then
   k TOKEN-USE = if s" requested_token_use" exit then
   k REFRESH-TOKEN = if s" refresh_token" exit then
   k AUTH-CODE = if s" code" exit then
   k VERIFIER = if s" code_verifier" exit then
   k REDIRECT-URI = if s" redirect_uri" exit then
   k RESPONSE-TYPE = if s" response_type" exit then
   k CLIENT-STATE = if s" state" exit then
   k NONCE = if s" nonce" exit then
   k CHALLENGE = if s" code_challenge" exit then
   k CHALLENGE-METHOD = if s" code_challenge_method" exit then
   s" login_hint" ;

\ The field a decoded name is, else -1.
: WHICH ( str -- n )
   {: s:str :}
   FIELDS 0 ?do s i NAME$ STR:= if i unloop exit then loop -1 ;

\ One name=value pair, a wanted name's value decoded into FORM; false when the
\ form already holds that name (RFC 6749 3.1).
: PAIR ( str -- bool )
   {: p:str :}
   p 0 [char] = FIND {: eq:n :}
   NOTE VEC-OF 0 VEC:RESIZE
   p STR:DATA eq >str NOTE VEC-OF URI:FORM-DECODE+
   NOTE VEC-OF VEC$ WHICH {: k:n :}
   k 0< if true exit then
   k SLOT-FIELD FIELD-AT @ 0< 0= if false exit then
   FORM VEC-OF VEC:COUNT {: at:n :}
   eq p STR:LEN < if
      p STR:DATA eq + 1+ p STR:LEN eq - 1- >str
      FORM VEC-OF URI:FORM-DECODE+
   then
   at k SLOT-FIELD FIELD-AT !
   FORM VEC-OF VEC:COUNT at - k SLOT-FIELD FIELD-LEN !
   true ;

\ False for a form naming a wanted field twice.
: FORM! ( str -- bool )
   {: b:str :}
   FORM VEC-OF 0 VEC:RESIZE
   FIELDS 0 ?do -1 i SLOT-FIELD FIELD-AT ! loop
   0 begin dup b STR:LEN <= while
      {: from:n :}
      b from [char] & FIND {: end:n :}
      b STR:DATA from + end from - >str PAIR 0= if false exit then
      end 1+
   repeat drop true ;


\ A field's decoded value, borrowed from FORM; false when the form lacks it.
: FIELD$ ( n -- str bool )
   SLOT-FIELD {: f:n :}
   f FIELD-AT @ {: at:n :}
   at 0< if s" " false exit then
   FORM VEC-OF VEC$ STR:DATA at + f FIELD-LEN @ >str true ;

: FIELD= ( n str -- bool )
   {: k:n w:str :}
   k FIELD$ 0= if drop false exit then
   w STR:= ;

\ A form without client_secret presents a public client's empty one.
: CLIENT? ( -- bool )
   CLIENT-ID FIELD$ 0= if drop false exit then {: id:str :}
   CLIENT-SECRET FIELD$ drop {: secret:str :}
   id secret KNOWN? ;

\ Under CLIENT?: the client is a public one, its secret empty.
: PUBLIC? ( -- bool ) CLIENT-SECRET FIELD$ drop STR:LEN 0= ;

: OBO? ( -- bool )
   GRANT-TYPE s" urn:ietf:params:oauth:grant-type:jwt-bearer" FIELD=
   TOKEN-USE s" on_behalf_of" FIELD= and ;

\ The scope without a trailing /.default, as Entra names the audience.
: AUDIENCE$ ( -- str )
   SCOPE FIELD$ drop {: s:str :}
   s s" /.default" STR:ENDS-WITH? if
      s STR:DATA s STR:LEN 9 - >str exit
   then
   s ;


\ Input's fault: a JSON reader code JSON:INPUT-FAULT? names, or malformed
\ base64url.
: MALFORMED? ( n -- bool )
   {: code:n :}
   code JSON:INPUT-FAULT?
   code E-BASE64-LAST >= code E-BASE64-FIRST <= and or ;

\ One step over untrusted text: malformed is false, any other throw passes.
: STEP ( [ bool -- bool ] -- bool )
   false swap catch {: ok code:n :}
   code 0= if ok exit then
   code MALFORMED? if false exit then
   code throw ;

\ Whether the member is a string equal to the text; false when the object
\ lacks it.
: MEMBER-IS? ( JSON:value str str -- bool )
   {: obj k:str w:str :}
   obj k JSON:FIND MATCH option
      none OF false ENDOF
      some OF w JSON:STRING= ENDOF
   ;MATCH ;

: INT-OF ( JSON:value -- n bool )
   {: v :}
   v JSON:INTEGER? 0= if 0 false exit then
   v JSON:>N true ;

\ An integer member that fits a cell; false when it is absent or no such
\ number.
: MEMBER-INT ( JSON:value str -- n bool )
   JSON:FIND MATCH option
      none OF 0 false ENDOF
      some OF INT-OF ENDOF
   ;MATCH ;

\ A string as sent, quotes and escapes included; false for another kind.
: STRING-TEXT ( JSON:value -- str bool )
   {: v :}
   v JSON:STRING? 0= if s" " false exit then
   v JSON:TEXT$ true ;

\ A string member as sent; false when it is absent or not a string.
: MEMBER-RAW ( JSON:value str -- str bool )
   JSON:FIND MATCH option
      none OF s" " false ENDOF
      some OF STRING-TEXT ENDOF
   ;MATCH ;


\ This server's URL with the path; borrowed from the task's SB.
: URL$ ( str -- str )
   {: path:str :}
   SB-RESET s" http://127.0.0.1:" SB-APPEND HTTP:PORT FMT:SB-U path SB-APPEND SB$ ;

\ The bytes appended as unpadded base64url, whose length is exactly (4u+2)/3.
\ The bytes must not lie in the vector, whose block the growth may move.
: SEGMENT+ ( str ^VEC:vector<u8> -- )
   {: s:str v:^ :}
   v VEC:COUNT {: old:n :}
   v old s STR:LEN 4 * 2 + 3 / + VEC:ENSURE
   s v VEC:SPAN old SPAN:SKIP BASE64:ENCODE-URL drop ;

\ The signature's first byte changed, so the key made no such signature.
: FLIPPED ( ^VEC:vector<u8> -- )
   VEC:SPAN SPAN:$ STR:DATA {: at :}
   at c@ 1 xor at c! ;

\ The signature over the vector's text appended as its third segment, by way
\ of a vector this word makes and deletes; forged when the flag is true.
: SIGNED+ ( ^VEC:vector<u8> bool -- )
   {: v:^ forge:bool :}
   NEW-VEC {: sig:^ :}
   [: JWS-FIXTURE:PEM$ v VEC$ sig CRYPTO:RS256-SIGN
      forge if sig FLIPPED then
      [char] . v VEC:PUSH c!
      sig VEC$ v SEGMENT+ ;]
   [: sig VEC:DELETE ;] finally ;

\ MINT's JWS, its signature forged when the flag is true.
: JWS ( str ^VEC:vector<u8> bool -- str )
   {: claims:str v:^ forge:bool :}
   v 0 VEC:RESIZE
   HEADER$ v SEGMENT+
   [char] . v VEC:PUSH c!
   claims v SEGMENT+
   v forge SIGNED+
   v VEC$ ;

public

\ A compact JWS of the claims text, signed RS256 with the fixture key under the
\ issuer's kid, borrowed from the vector until its next write. Claims must not
\ lie in the vector.
: MINT ( str ^VEC:vector<u8> -- str ) false JWS ;

private

\ The claims, the signed text's length and the signature, both decoded into
\ SCRATCH; true for three segments and a modulus-wide signature.
: SPLIT ( str -- SPAN:span<u8> n SPAN:span<u8> bool )
   {: s:str :}
   SCRATCH VEC-OF {: v:^ :}
   v 0 VEC:RESIZE  v s STR:LEN VEC:ENSURE
   v VEC:SPAN {: room :}
   s 0 [char] . FIND {: d1:n :}
   s d1 1+ [char] . FIND {: d2:n :}
   d2 s STR:LEN = if room 0 room false exit then
   s d2 1+ [char] . FIND s STR:LEN <> if room 0 room false exit then
   s STR:DATA d1 + 1+ d2 d1 - 1- >str room BASE64:DECODE-URL {: cn:n :}
   room cn s STR:LEN cn - SPAN:SUB {: rest :}
   s STR:DATA d2 + 1+ s STR:LEN d2 - 1- >str
   rest BASE64:DECODE-URL {: sn:n :}
   room 0 cn SPAN:SUB d2 rest 0 sn SPAN:SUB sn MODULUS-U @ = ;

\ The token in the field, an assertion or a refresh token, verifies against the
\ fixture key, and its claims are one object whose aud is the client id and
\ whose exp is later than now.
: VERIFIED? ( n -- bool )
   FIELD$ 0= if drop false exit then {: t:str :}
   t SPLIT {: body signed:n sig ok:bool :}
   ok 0= if false exit then
   MODULUS-ROW MODULUS-U @ >str EXPONENT-ROW EXPONENT-U @ >str
   t STR:DATA signed >str sig SPAN:$ CRYPTO:RS256-VERIFY?
   0= if false exit then
   body SPAN:$ JSON:PARSE {: c :}
   c JSON:OBJECT? 0= if false exit then
   c SLOT PRESENTED !
   c s" aud" CLIENT-ID FIELD$ drop MEMBER-IS? 0= if false exit then
   c s" exp" MEMBER-INT 0= if drop false exit then
   epoch-seconds > ;


\ The claims every issued token carries, for the audience; the writer is left
\ inside the object.
: AUD-OPEN ( str -- ^JSON:writer )
   {: aud:str :}
   epoch-seconds {: now:n :}
   SLOT WRITER CLAIMS JSON:OPEN
   JSON:WRITE-OBJECT-START
   s" iss" s" " URL$ JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" aud" aud JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" azp" CLIENT-ID FIELD$ drop JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" iat" now JSON:WRITE-FIELD-INT JSON:WRITE-COMMA
   s" nbf" now JSON:WRITE-FIELD-INT JSON:WRITE-COMMA
   s" exp" now LIFE-S @ + JSON:WRITE-FIELD-INT ;

\ AUD-OPEN for the audience the scope names.
: CLAIMS-OPEN ( -- ^JSON:writer ) AUDIENCE$ AUD-OPEN ;

: CLAIMS-CLOSE ( ^JSON:writer -- )
   JSON:WRITE-OBJECT-END JSON:CLOSE ;

\ The client acts as itself.
: CREDENTIALS ( -- )
   CLAIMS-OPEN
   JSON:WRITE-COMMA s" oid" CLIENT-ID FIELD$ drop JSON:WRITE-FIELD-S
   JSON:WRITE-COMMA s" tid" TENANT$ JSON:WRITE-FIELD-S
   CLAIMS-CLOSE ;

\ The assertion's, refresh token's or code's claim from copied as sent under
\ the name, when it has one.
: COPIED-AS ( ^JSON:writer str str -- ^JSON:writer )
   {: k:str from:str :}
   SLOT PRESENTED @ from MEMBER-RAW 0= if drop exit then {: v:str :}
   JSON:WRITE-COMMA k v JSON:WRITE-FIELD-RAW ;

: COPIED ( ^JSON:writer str -- ^JSON:writer ) dup COPIED-AS ;

\ The client acts for the user of the assertion or refresh token.
: ON-BEHALF ( -- )
   CLAIMS-OPEN s" oid" COPIED s" tid" COPIED CLAIMS-CLOSE ;

\ The refresh token's claims: the client's, for the presented one's oid and tid.
: RENEWAL-CLAIMS ( -- )
   SLOT WRITER CLAIMS JSON:OPEN
   JSON:WRITE-OBJECT-START
   s" aud" CLIENT-ID FIELD$ drop JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" exp" epoch-seconds LIFE-S @ + JSON:WRITE-FIELD-INT
   s" oid" COPIED s" tid" COPIED CLAIMS-CLOSE ;

: TOKEN-FIELDS ( ^JSON:writer -- ^JSON:writer )
   s" access_token" TOKEN VEC-OF VEC$ JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" token_type" s" Bearer" JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" expires_in" LIFE-S @ JSON:WRITE-FIELD-INT ;

: ISSUED-BODY ( ^JSON:writer -- ^JSON:writer )
   JSON:WRITE-OBJECT-START TOKEN-FIELDS JSON:WRITE-OBJECT-END ;

: RENEWED-BODY ( ^JSON:writer -- ^JSON:writer )
   JSON:WRITE-OBJECT-START TOKEN-FIELDS JSON:WRITE-COMMA
   s" refresh_token" RENEWAL VEC-OF VEC$ JSON:WRITE-FIELD-S
   JSON:WRITE-OBJECT-END ;

: SIGNED-IN-BODY ( ^JSON:writer -- ^JSON:writer )
   JSON:WRITE-OBJECT-START TOKEN-FIELDS JSON:WRITE-COMMA
   s" refresh_token" RENEWAL VEC-OF VEC$ JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" id_token" IDENTITY VEC-OF VEC$ JSON:WRITE-FIELD-S
   JSON:WRITE-OBJECT-END ;

: MINTED ( n -- ) {: into:n :} CLAIMS$ into VEC-OF MINT drop ;

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
: REJECT ( HTTP:response n str -- )
   {: answer:HTTP:response status:n why:str :}
   why str> SPAN:MAKE SLOT WHY !
   answer status HTTP:STATUS!
   answer [: ERROR-BODY ;] HTTP:JSON! ;

: FORM-TYPE? ( HTTP:request -- bool )
   s" Content-Type" HTTP:HEADER-OF$ 0= if drop false exit then {: t:str :}
   t 0 [char] ; FIND {: end:n :}
   t STR:DATA end >str STR:TRIM s" application/x-www-form-urlencoded" STR:=CI ;

\ Whether the space-separated list holds the word.
: LISTS? ( str str -- bool )
   {: list:str w:str :}
   0 begin {: start:n :}
      list $20 start STR:SPLIT-NEXT {: part:str next:n found:bool :}
      found 0= if false exit then
      part w STR:= if true exit then
      next
   again ;

\ A scope OpenID Connect defines, which names no resource.
: OIDC? ( str -- bool )
   {: s:str :}
   s s" openid" STR:= s s" offline_access" STR:= or
   s s" profile" STR:= or s s" email" STR:= or ;

\ The text before the last `/`, else the whole text.
: STEM ( str -- str )
   {: s:str :}
   s STR:LEN begin dup 0 > while
      1-
      dup s STR:DATA + c@ [char] / = if s STR:DATA swap >str exit then
   repeat drop s ;

\ The resource the first scope in the list that is not OIDC?'s names, as
\ STEM cuts it; false when there is none.
: RESOURCE ( str -- str bool )
   {: list:str :}
   0 begin {: start:n :}
      list $20 start STR:SPLIT-NEXT {: part:str next:n found:bool :}
      found 0= if s" " false exit then
      part STR:LEN 0<> part OIDC? 0= and if part STEM true exit then
      next
   again ;

\ The presented token is an authorization code.
: CODE? ( -- bool ) SLOT PRESENTED @ s" use" s" code" MEMBER-IS? ;

\ The code grant's audience: the resource the code's scope names, else the
\ client id; borrowed from NOTE.
: GRANTED$ ( -- str )
   NOTE VEC-OF {: v:^ :}
   v 0 VEC:RESIZE
   SLOT PRESENTED @ s" scope" JSON:FIND MATCH option
      none OF ENDOF
      some OF v JSON:APPEND-STRING ENDOF
   ;MATCH
   v VEC$ RESOURCE if exit then
   drop CLIENT-ID FIELD$ drop ;

\ The ID token's claims (OpenID Connect Core 2): the code's person, tenant and
\ nonce, for the client.
: IDENTITY-CLAIMS ( -- )
   epoch-seconds {: now:n :}
   SLOT WRITER CLAIMS JSON:OPEN
   JSON:WRITE-OBJECT-START
   s" iss" s" " URL$ JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" aud" CLIENT-ID FIELD$ drop JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" iat" now JSON:WRITE-FIELD-INT JSON:WRITE-COMMA
   s" exp" now LIFE-S @ + JSON:WRITE-FIELD-INT
   s" sub" s" oid" COPIED-AS s" oid" COPIED s" tid" COPIED s" nonce" COPIED
   CLAIMS-CLOSE ;

: SIGNED-IN ( HTTP:response -- )
   GRANTED$ AUD-OPEN s" oid" COPIED s" tid" COPIED CLAIMS-CLOSE TOKEN MINTED
   RENEWAL-CLAIMS RENEWAL MINTED
   IDENTITY-CLAIMS CLAIMS$ IDENTITY VEC-OF FORGING @ JWS drop
   [: SIGNED-IN-BODY ;] HTTP:JSON! ;

\ The code's code_challenge is the verifier's SHA-256 in unpadded base64url
\ (RFC 7636 4.6), worked out in NOTE.
: PROVEN? ( -- bool )
   VERIFIER FIELD$ 0= if drop false exit then {: verifier:str :}
   NOTE VEC-OF {: v:^ :}
   v SHA256-CTX-BYTES DIGEST-BYTES + CHALLENGE-LEN + VEC:RESIZE
   v VEC:SPAN SPAN:$ STR:DATA {: ctx :}
   ctx SHA256-CTX-BYTES + {: digest :}
   ctx verifier digest SHA256-IN
   digest DIGEST-BYTES >str
   v VEC:SPAN SHA256-CTX-BYTES DIGEST-BYTES + SPAN:SKIP BASE64:ENCODE-URL {: n:n :}
   SLOT PRESENTED @ s" code_challenge" digest DIGEST-BYTES + n >str MEMBER-IS? ;

\ The form's code redeemed (RFC 6749 4.1.3): the issuer's own code for the
\ client, unexpired, for the same redirect URI and proven by the verifier.
: REDEEM ( HTTP:response -- )
   {: answer:HTTP:response :}
   [: drop AUTH-CODE VERIFIED? ;] STEP 0= if answer 400 s" invalid_grant" REJECT exit then
   CODE? 0= if answer 400 s" invalid_grant" REJECT exit then
   REDIRECT-URI FIELD$ 0= if drop answer 400 s" invalid_grant" REJECT exit then {: r:str :}
   SLOT PRESENTED @ s" redirect_uri" r MEMBER-IS? 0= if answer 400 s" invalid_grant" REJECT exit then
   PROVEN? 0= if answer 400 s" invalid_grant" REJECT exit then
   answer SIGNED-IN ;

\ The grant the form asks for, issued or rejected as Entra v2 answers; no
\ audience, or one that is not UTF-8 (claims are JSON text), no token
\ (RFC 6749 §5.2). A public client may only refresh. A code names its
\ audience in its own scope.
: GRANT ( HTTP:response -- )
   {: answer:HTTP:response :}
   GRANT-TYPE s" client_credentials" FIELD= {: credentials:bool :}
   GRANT-TYPE s" refresh_token" FIELD= {: refreshing:bool :}
   GRANT-TYPE s" authorization_code" FIELD= {: coding:bool :}
   credentials OBO? or refreshing or coding or 0= if answer 400 s" unsupported_grant_type" REJECT exit then
   refreshing 0= PUBLIC? and if answer 401 s" invalid_client" REJECT exit then
   coding if answer REDEEM exit then
   AUDIENCE$ {: aud:str :}
   aud STR:LEN 0= aud UTF8:VALID? 0= or if answer 400 s" invalid_request" REJECT exit then
   credentials if CREDENTIALS answer ISSUE exit then
   refreshing if
      [: drop REFRESH-TOKEN VERIFIED? ;] STEP 0= if answer 400 s" invalid_grant" REJECT exit then
      CODE? if answer 400 s" invalid_grant" REJECT exit then
      answer RENEW exit
   then
   [: drop ASSERTION VERIFIED? ;] STEP 0= if answer 400 s" invalid_grant" REJECT exit then
   ON-BEHALF answer ISSUE ;

: TOKEN-ROUTE ( HTTP:request HTTP:response -- )
   {: asked:HTTP:request answer:HTTP:response :}
   READY
   asked FORM-TYPE? 0= if answer 400 s" invalid_request" REJECT exit then
   asked HTTP:BODY$ FORM! 0= if answer 400 s" invalid_request" REJECT exit then
   CLIENT? 0= if answer 401 s" invalid_client" REJECT exit then
   answer GRANT ;

: MINT-ROUTE ( HTTP:request HTTP:response -- )
   {: asked:HTTP:request answer:HTTP:response :}
   READY
   asked HTTP:BODY$ TOKEN VEC-OF MINT {: t:str :}
   answer s" Content-Type" s" text/plain" HTTP:HEADER!
   answer t HTTP:BODY! ;

\ An http or https URL with a host, of printable ASCII without a fragment, so
\ a Location header carries it as it is.
: REDIRECT? ( str -- bool )
   {: u:str :}
   u STR:LEN 0 ?do
      u STR:DATA i + c@ {: c:n :}
      c $21 < c $7E > or c [char] # = or if false unloop exit then
   loop
   false [: drop u URI:ORIGIN drop drop drop true ;] catch {: ok code:n :}
   code 0= if ok exit then
   code E-URI-HTTP = code E-URI-HOST = or if false exit then
   code throw ;

: URL-BYTE? ( n -- bool )
   {: c:n :}
   c [char] A >= c [char] Z <= and
   c [char] a >= c [char] z <= and or
   c [char] 0 >= c [char] 9 <= and or
   c [char] - = or c [char] _ = or ;

\ An S256 challenge: a digest in unpadded base64url.
: CHALLENGE? ( str -- bool )
   {: s:str :}
   s STR:LEN CHALLENGE-LEN <> if false exit then
   s STR:LEN 0 ?do s STR:DATA i + c@ URL-BYTE? 0= if false unloop exit then loop
   true ;

\ The field is present, not empty and UTF-8, as claims text needs.
: TEXT? ( n -- bool )
   FIELD$ 0= if drop false exit then {: s:str :}
   s STR:LEN 0<> s UTF8:VALID? and ;

\ The error a query with a good client and redirect URI is sent back with,
\ else false (RFC 6749 4.1.2.1).
: FAULT ( -- str bool )
   RESPONSE-TYPE FIELD$ 0= if drop s" invalid_request" true exit then
   s" code" STR:= 0= if s" unsupported_response_type" true exit then
   SCOPE TEXT? 0= if s" invalid_request" true exit then
   SCOPE FIELD$ drop s" openid" LISTS? 0= if s" invalid_request" true exit then
   CLIENT-STATE FIELD$ drop STR:LEN 0= if s" invalid_request" true exit then
   NONCE TEXT? 0= if s" invalid_request" true exit then
   CHALLENGE FIELD$ drop CHALLENGE? 0= if s" invalid_request" true exit then
   CHALLENGE-METHOD s" S256" FIELD= 0= if s" invalid_request" true exit then
   LOGIN-HINT FIELD$ if
      UTF8:VALID? 0= if s" invalid_request" true exit then
   else drop then
   s" " false ;

\ The person signed in: the login hint, else a fixed oid.
: OID$ ( -- str )
   LOGIN-HINT FIELD$ {: h:str there:bool :}
   there h STR:LEN 0<> and if h exit then
   s" test-person" ;

\ NOTE as the redirect URI and the separator its query takes next.
: LOCATION ( -- )
   NOTE VEC-OF {: v:^ :}
   REDIRECT-URI FIELD$ drop {: r:str :}
   v 0 VEC:RESIZE
   r v VEC:APPEND
   r 0 [char] ? FIND r STR:LEN < if [char] & else [char] ? then v VEC:PUSH c! ;

\ name=value appended to NOTE, the value query-encoded; neither lies in NOTE.
: PARAM+ ( str str -- )
   {: k:str w:str :}
   NOTE VEC-OF {: v:^ :}
   k v VEC:APPEND
   [char] = v VEC:PUSH c!
   w v URI:QUERY-ENCODE ;

\ The request's state appended after `&`, when it has one.
: STATE+ ( -- )
   CLIENT-STATE FIELD$ 0= if drop exit then {: st:str :}
   [char] & NOTE VEC-OF VEC:PUSH c!
   s" state" st PARAM+ ;

: REDIRECT ( HTTP:response -- )
   {: answer:HTTP:response :}
   answer 302 HTTP:STATUS!
   answer s" Location" NOTE VEC-OF VEC$ HTTP:HEADER! ;

: SENT-BACK ( HTTP:response str -- )
   {: answer:HTTP:response why:str :}
   LOCATION s" error" why PARAM+ STATE+ answer REDIRECT ;

\ The code's claims: what redeeming it checks and what its answer carries.
: CODE-CLAIMS ( -- )
   SLOT WRITER CLAIMS JSON:OPEN
   JSON:WRITE-OBJECT-START
   s" aud" CLIENT-ID FIELD$ drop JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" use" s" code" JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" redirect_uri" REDIRECT-URI FIELD$ drop JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" code_challenge" CHALLENGE FIELD$ drop JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" nonce" NONCE FIELD$ drop JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" scope" SCOPE FIELD$ drop JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" oid" OID$ JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" tid" TENANT$ JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" exp" epoch-seconds CODE-S + JSON:WRITE-FIELD-INT
   CLAIMS-CLOSE ;

\ The person signed in, sent back with a code minted into TOKEN.
: AUTHORIZED ( HTTP:response -- )
   {: answer:HTTP:response :}
   CODE-CLAIMS TOKEN MINTED
   LOCATION s" code" TOKEN VEC-OF VEC$ PARAM+ STATE+ answer REDIRECT ;

\ A sign-in request (RFC 6749 4.1.1, OpenID Connect Core 3.1.2.1): an
\ unknown or public client, a bad or missing redirect URI, or a field given
\ twice is answered here; any other fault is sent back to the redirect URI.
: AUTHORIZE-ROUTE ( HTTP:request HTTP:response -- )
   {: asked:HTTP:request answer:HTTP:response :}
   READY
   asked HTTP:QUERY$ FORM! 0= if answer 400 s" invalid_request" REJECT exit then
   CLIENT-ID FIELD$ 0= if drop answer 400 s" unauthorized_client" REJECT exit then
   CONFIDENTIAL? 0= if answer 400 s" unauthorized_client" REJECT exit then
   REDIRECT-URI FIELD$ 0= if drop answer 400 s" invalid_request" REJECT exit then
   REDIRECT? 0= if answer 400 s" invalid_request" REJECT exit then
   FAULT {: why:str bad:bool :}
   bad if answer why SENT-BACK exit then
   LOGIN-HINT s" denied" FIELD= if answer s" access_denied" SENT-BACK exit then
   answer AUTHORIZED ;

: DISCOVERY-BODY ( ^JSON:writer -- ^JSON:writer )
   JSON:WRITE-OBJECT-START
   s" issuer" s" " URL$ JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" authorization_endpoint" s" /authorize" URL$ JSON:WRITE-FIELD-S JSON:WRITE-COMMA
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
   s" GET" s" /authorize" [: AUTHORIZE-ROUTE ;] HTTP:ROUTE
   s" POST" s" /token" [: TOKEN-ROUTE ;] HTTP:ROUTE
   s" POST" s" /mint" [: MINT-ROUTE ;] HTTP:ROUTE ;

\ The seconds each token minted from then on lives, its exp and expires_in.
: LIFE! ( n -- ) LIFE-S ! ;

\ True makes each ID token minted from then on carry a signature the fixture
\ key did not make; false signs them again.
: FORGED! ( bool -- ) FORGING ! ;

\ A client id and secret /token accepts, copied; an empty secret states a
\ public client, which presents none and may only refresh. E-HTTP-STATE while
\ HTTP serves.
: CLIENT+ ( str str -- )
   {: id:str secret:str :}
   HTTP:RUNNING? if E-HTTP-STATE throw then
   CLIENT-COUNT @ {: k:n :}
   k 0= if NEW-VEC CLIENTS ! then
   k 1+ CLIENT-AT-RESERVE
   k 1+ ID-LEN-RESERVE
   k 1+ SECRET-LEN-RESERVE
   CLIENTS @ VEC:COUNT k CLIENT-AT !
   id STR:LEN k ID-LEN !
   secret STR:LEN k SECRET-LEN !
   id CLIENTS @ VEC:APPEND
   secret CLIENTS @ VEC:APPEND
   k 1+ CLIENT-COUNT ! ;

;package
