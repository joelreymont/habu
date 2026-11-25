\ test-issuer-test.f - TEST-ISSUER over a real loopback port: discovery, the key
\ set, client credentials verified against the served key, invalid_client for
\ a confidential client without its secret and for a public client asking for
\ client credentials or On-Behalf-Of, unsupported_grant_type, invalid_request
\ for a repeated field and a wrong media type, On-Behalf-Of with a minted
\ assertion and its invalid_grant refusals, /mint against MINT, and
\ tools/test-issuer.f as a child process.
\ BEARER's client against it: TOKEN before CLIENT!, for each grant admitted by
\ ADMIT? with the caller's claims, a 400 and a 401 refusal, the 400 leaving no
\ byte of the longer answer before it past the vector's count; REFRESH from a
\ public client admitted with the refresh token's oid, again with the new
\ refresh token it answered, an expired and a foreign refresh token refused
\ invalid_grant; a closed port,
\ and on /token-fault the form's exact bytes, the answer's lifetime as a
\ number and as a digit string, and the answers TOKEN refuses: a 503, a 429
\ and a 302, a 2xx that is not JSON, one without access_token, one whose
\ access_token is not a string, one without expires_in, one whose expires_in
\ is a string not all digits, one whose expires_in is 0, and a 200 and a 503
\ past TOKEN-CAP, each leaving only zero bytes in the vector's storage;
\ REFRESH's form from a confidential and a public client, an answer without
\ refresh_token, an escaped refresh token decoded after an escaped token,
\ and a refresh_token that is not a string, E-RESPONSE logged for REFRESH
\ and ignored by TOKEN. CURRENT on /token-count: a token kept per
\ scope, asked again when it lasts LEEWAY-S or less, asked once for four tasks
\ at once, a failed ask's code thrown to the tasks waiting on it, and asked
\ again after a failed ask and after CLIENT!.
\ IDENTITY-TOKEN on the IMDS route, which answers 400 unless the request
\ has Metadata: true, the api-version and the case's resource as sent:
\ a token appended after the vector's bytes and kept across CLIENT!, asked
\ again when it lasts LEEWAY-S or less, asked once for four tasks at once,
\ and a 404, a 410 and a 410 past TOKEN-CAP as E-UNAVAILABLE, a 400 as
\ E-REFUSED.
\ Run: bin/hb --load src/lib/net/test-issuer-test.f

require src/lib/test.f
require src/lib/test/wait.f
require src/lib/string.f
require src/lib/span.f
require src/lib/fmt.f
require src/lib/aio.f
require src/lib/base64.f
require src/lib/heap.f
require src/lib/vector.f
require src/lib/date.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/test/stderr.f
require src/lib/memory.f
require src/lib/json.f
require src/lib/process.f
require src/lib/spawn.f
require src/lib/engine-candidate.f
require src/lib/crypto/evp.f
require src/lib/crypto/jws-fixture.f
require src/lib/net/curl.f
require src/lib/net/http.f
require src/lib/net/test-issuer.f
require src/lib/net/bearer.f

package TEST-ISSUER-TEST
private

$7F000001 constant LOOPBACK
2 constant TWO-WORKERS
500 constant IDLE-MS
600 constant LIFE-S                 \ how far past now an assertion's exp lies
3600 constant ISSUED-S              \ an issued token's lifetime
8 constant EXPONENT-CAP

TYPED-VARIABLE CFG HTTP:config

: SERVE-ON ( n -- )
   {: port:n :}
   LOOPBACK CFG HTTP-CONFIG:ADDRESS !
   port CFG HTTP-CONFIG:PORT !
   TWO-WORKERS CFG HTTP-CONFIG:WORKERS !
   IDLE-MS >MS CFG HTTP-CONFIG:IDLE !
   CFG HTTP:START ;

$2000 constant CAP
TYPED-VARIABLE BODY ^VEC:vector<u8>
CAP BUFFER: FORM
variable FORM-U
CAP BUFFER: TOKEN
variable TOKEN-U
CAP BUFFER: CLAIMS
variable CLAIMS-U
CAP BUFFER: HEAD
variable HEAD-U
CRYPTO:RS256-MAX-BYTES BUFFER: SIGNATURE
variable SIGNATURE-U
CRYPTO:RS256-MAX-BYTES BUFFER: MODULUS
variable MODULUS-U
EXPONENT-CAP BUFFER: EXPONENT
variable EXPONENT-U
$40 BUFFER: KID
variable KID-U
CAP BUFFER: ASSERTION
variable ASSERTION-U
$100 BUFFER: LINE
variable LINE-U
variable CHILD-OUT                  \ the read end of the child's stdout, an fd number

TYPED-VARIABLE READER JSON:cursor
TYPED-VARIABLE MINTED ^VEC:vector<u8>
: MAKE-MINTED ( -- ) HEAP:ALLOCATOR 0 [type] u8 VEC:NEW MINTED ! ;
MAKE-MINTED

$80 constant URL-CAP
URL-CAP BUFFER: ISSUER-URL
variable ISSUER-URL-U
URL-CAP BUFFER: KEYS-URL
variable KEYS-URL-U
URL-CAP BUFFER: ENDPOINT
variable ENDPOINT-U
URL-CAP BUFFER: FAULT-URL
variable FAULT-URL-U
URL-CAP BUFFER: COUNT-URL
variable COUNT-URL-U
variable PORT-KEPT                  \ the server's port, across its restart and stop
CAP BUFFER: AUTH
variable AUTH-U
TYPED-VARIABLE GOT-VEC ^VEC:vector<u8>
: GOT ( -- ^VEC:vector<u8> ) GOT-VEC @ ;
: GOT-NEW ( -- ) HEAP:ALLOCATOR 0 [type] u8 VEC:NEW GOT-VEC ! ;
GOT-NEW

\ Whether GOT's storage past its count holds only zero bytes. ENSURE to the
\ capacity moves nothing, and GOT keeps its count.
: GOT-ZEROED? ( -- bool )
   GOT {: v:^ :}
   v VEC:COUNT {: len:n :}
   v v VEC:CAP VEC:ENSURE
   v VEC:SPAN len SPAN:SKIP SPAN:$ {: s:str :}
   true s STR:LEN 0 ?do s STR:DATA i + c@ 0<> if drop false then loop
   v len VEC:RESIZE ;

\ /token-fault's answers, by FAULT-MODE.
0 constant ECHO                     \ keeps the form in POSTED and answers a token
1 constant BUSY
2 constant NOT-JSON
3 constant OVERSIZED
4 constant NOT-STRING
5 constant NO-LIFETIME
6 constant LIFETIME-STRING
7 constant LIFETIME-ZERO
8 constant LIMITED
9 constant NO-TOKEN
10 constant MOVED
11 constant BUSY-OVERSIZED
12 constant RENEWED                 \ an escaped token and an escaped refresh token
13 constant RENEWAL-NUMBER          \ a refresh_token that is a number
14 constant REPEATED                \ access_token named twice
15 constant LIFETIME-NOT-DIGITS
variable FAULT-MODE
CAP BUFFER: POSTED
variable POSTED-U
$10001 constant PAST-CAP            \ one byte past BEARER's TOKEN-CAP
PAST-CAP BUFFER: OVERSIZE

: BODY$ ( -- str ) BODY @ VEC:BYTES ;
: TOKEN$ ( -- str ) TOKEN TOKEN-U @ >str ;
: CLAIMS$ ( -- str ) CLAIMS CLAIMS-U @ >str ;
: ASSERTION$ ( -- str ) ASSERTION ASSERTION-U @ >str ;

\ The text copied into a row, answering its length.
: COPY ( str ^u8 -- n )
   {: s:str row:^ :}
   s STR:DATA row s STR:LEN BYTE-COPY s STR:LEN ;


\ A string member of an object text as sent, empty when absent.
: MEMBER$ ( str str -- str )
   {: t:str k:str :}
   READER t JSON:READ-INIT JSON:READ-NEXT drop
   k JSON:READ-FIND-KEY 0= if JSON:READ-CLOSE s" " exit then
   JSON:READ-TOKEN JSON:T-STR <> if JSON:READ-CLOSE s" " exit then
   JSON:READ-SPAN$ {: a:^ u:n :} JSON:READ-CLOSE a u >str ;

\ An integer member of an object text, -1 when absent.
: MEMBER-INT ( str str -- n )
   {: t:str k:str :}
   READER t JSON:READ-INIT JSON:READ-NEXT drop
   k JSON:READ-FIND-KEY 0= if JSON:READ-CLOSE -1 exit then
   JSON:READ-TOKEN JSON:T-INT <> if JSON:READ-CLOSE -1 exit then
   JSON:READ-INT {: v:n :} JSON:READ-CLOSE v ;

\ A string member of the body's first key, as sent.
: KEY$ ( str -- str )
   {: k:str :}
   READER BODY$ JSON:READ-INIT JSON:READ-NEXT drop
   s" keys" JSON:READ-FIND-KEY drop JSON:READ-NEXT drop
   k JSON:READ-FIND-KEY drop JSON:READ-SPAN$ {: a:^ u:n :} JSON:READ-CLOSE a u >str ;


: OK ( CURL:status -- )
   MATCH CURL:status
      ok OF true ENDOF
      failed OF drop false ENDOF
   ;MATCH TTRUE ;

: URL ( str -- str )
   {: path:str :}
   SB-RESET s" http://127.0.0.1:" SB-APPEND HTTP:PORT FMT:SB-U path SB-APPEND SB$ ;

: OPEN-AT ( str -- CURL:handle )
   {: url:str :}
   CURL:INIT
   MATCH CURL:init-result
      ready OF ENDOF
      failed OF drop CURL:EMPTY-HANDLE ENDOF
   ;MATCH {: h:CURL:handle :}
   h url CURL:URL! OK h ;

: OPEN ( str -- CURL:handle ) URL OPEN-AT ;

\ The response's status and its body in BODY; -1 for a body past CAP, -2 for
\ a failed transfer.
: FINISH ( CURL:handle -- n )
   {: h:CURL:handle :}
   BODY @ 0 VEC:RESIZE
   h CAP >LEN BODY @ CURL:PERFORM
   MATCH CURL:fetch-result
      response OF drop CURL:HTTP-STATUS>N ENDOF
      truncated OF drop drop -1 ENDOF
      failed OF drop -2 ENDOF
   ;MATCH
   h CURL:CLEANUP ;

: GET ( str -- n ) OPEN FINISH ;

\ The body posted with the media type; the body is copied by libcurl.
: POST ( str str str -- n )
   {: path:str type:str b:str :}
   path OPEN {: h:CURL:handle :}
   h type CURL:HEADER+ OK
   h b CURL:BODY! OK
   h FINISH ;

: FORM-TYPE ( -- str ) s" Content-Type: application/x-www-form-urlencoded" ;

: FORM-RESET ( -- ) 0 FORM-U ! ;
: FORM+ ( str -- ) FORM FORM-U @ + COPY FORM-U +! ;
: FORM$ ( -- str ) FORM FORM-U @ >str ;
: ASK ( -- n ) s" /token" FORM-TYPE FORM$ POST ;

\ The client's credentials, its secret percent-encoded.
: APP ( -- ) FORM-RESET s" client_id=app&client_secret=s3cr%26t%2B1" FORM+ ;
: CREDENTIALS ( -- ) APP s" &grant_type=client_credentials&scope=api%3A%2F%2Fup%2F.default" FORM+ ;

\ An On-Behalf-Of request carrying the assertion, without a scope.
: UNSCOPED ( -- )
   APP
   s" &grant_type=urn%3Aietf%3Aparams%3Aoauth%3Agrant-type%3Ajwt-bearer" FORM+
   s" &requested_token_use=on_behalf_of&assertion=" FORM+
   ASSERTION$ FORM+ ;

: ON-BEHALF ( -- ) UNSCOPED s" &scope=api%3A%2F%2Fdown%2F.default" FORM+ ;

\ The body is the error answer.
: REFUSED ( n n str -- )
   {: status:n want:n code:str :}
   SB-RESET S\" {\"error\":\"" SB-APPEND code SB-APPEND S\" \"}" SB-APPEND
   BODY$ SB$ T$=
   status want T= ;


\ The index of the first dot at or after from, else the text's length.
: DOT ( str n -- n )
   {: s:str from:n :}
   s STR:LEN from ?do s STR:DATA i + c@ [char] . = if i unloop exit then loop s STR:LEN ;

: DECODED ( str ^u8 n -- n ) SPAN:MAKE BASE64:DECODE-URL ;

\ The issued token copied from the answer; its header, claims and signature
\ decoded.
: ISSUED ( -- )
   BODY$ s" access_token" MEMBER$ TOKEN COPY TOKEN-U !
   TOKEN$ {: t:str :}
   t 0 DOT {: d1:n :}
   t d1 1+ DOT {: d2:n :}
   t STR:DATA d1 >str HEAD CAP DECODED HEAD-U !
   t STR:DATA d1 + 1+ d2 d1 - 1- >str CLAIMS CAP DECODED CLAIMS-U !
   t STR:DATA d2 + 1+ t STR:LEN d2 - 1- >str
   SIGNATURE CRYPTO:RS256-MAX-BYTES DECODED SIGNATURE-U ! ;

\ The issued token verifies against the key /keys served.
: VERIFIES ( -- )
   TOKEN$ {: t:str :}
   t 0 DOT {: d1:n :}
   t d1 1+ DOT {: d2:n :}
   MODULUS MODULUS-U @ >str EXPONENT EXPONENT-U @ >str t STR:DATA d2 >str
   SIGNATURE SIGNATURE-U @ >str CRYPTO:RS256-VERIFY? TTRUE
   HEAD HEAD-U @ >str s" kid" MEMBER$ KID KID-U @ >str T$=
   HEAD HEAD-U @ >str s" alg" MEMBER$ s" RS256" T$=
   HEAD HEAD-U @ >str s" typ" MEMBER$ s" JWT" T$= ;

\ The answer is a 200 bearer token; the claims every issued token carries.
: ANSWERED ( n str -- )
   {: status:n aud:str :}
   status 200 T=
   BODY$ s" token_type" MEMBER$ s" Bearer" T$=
   BODY$ s" expires_in" MEMBER-INT ISSUED-S T=
   ISSUED VERIFIES
   CLAIMS$ s" iss" MEMBER$ s" " URL T$=
   CLAIMS$ s" aud" MEMBER$ aud T$=
   CLAIMS$ s" azp" MEMBER$ s" app" T$=
   CLAIMS$ s" iat" MEMBER-INT {: iat:n :}
   iat epoch-seconds 5 - >= TTRUE
   CLAIMS$ s" nbf" MEMBER-INT iat T=
   CLAIMS$ s" exp" MEMBER-INT iat ISSUED-S + T= ;

\ An assertion for the client, minted into ASSERTION.
: ASSERT ( str n -- )
   {: aud:str exp:n :}
   SB-RESET S\" {\"aud\":\"" SB-APPEND aud SB-APPEND S\" \",\"exp\":" SB-APPEND
   exp FMT:SB-INT S\" ,\"oid\":\"user-1\",\"tid\":\"tenant-1\"}" SB-APPEND
   SB$ MINTED @ TEST-ISSUER:MINT ASSERTION COPY ASSERTION-U ! ;

: LATER ( -- n ) epoch-seconds LIFE-S + ;

\ One signature byte changed, away from the last character's padding bits.
: FLIP ( -- )
   ASSERTION ASSERTION-U @ + 10 - {: at:^ :}
   at c@ [char] A = if [char] B else [char] A then at c! ;


: DISCOVERY ( -- )
   s" discovery names the issuer, its key set and its token endpoint" T-LABEL
   s" /.well-known/openid-configuration" GET 200 T=
   BODY$ s" issuer" MEMBER$ s" " URL T$=
   BODY$ s" jwks_uri" MEMBER$ s" /keys" URL T$=
   BODY$ s" token_endpoint" MEMBER$ s" /token" URL T$= ;

\ The served key is kept for VERIFIES.
: KEYS ( -- )
   s" /keys holds the fixture's n and e as an RS256 signing key" T-LABEL
   s" /keys" GET 200 T=
   s" kty" KEY$ s" RSA" T$=
   s" use" KEY$ s" sig" T$=
   s" alg" KEY$ s" RS256" T$=
   s" n" KEY$ JWS-FIXTURE:MODULUS$ T$=
   s" e" KEY$ JWS-FIXTURE:EXPONENT$ T$=
   s" kid" KEY$ KID COPY KID-U !
   s" n" KEY$ MODULUS CRYPTO:RS256-MAX-BYTES DECODED MODULUS-U !
   s" e" KEY$ EXPONENT EXPONENT-CAP DECODED EXPONENT-U ! ;

: CLIENT-CREDENTIALS ( -- )
   s" client credentials issue a token signed with the served key" T-LABEL
   CREDENTIALS ASK s" api://up" ANSWERED
   CLAIMS$ s" oid" MEMBER$ s" app" T$=
   CLAIMS$ s" tid" MEMBER$ s" test-tenant" T$=
   s" a form media type in capitals with a charset is accepted" T-LABEL
   CREDENTIALS
   s" /token" s" Content-Type: Application/X-WWW-Form-URLEncoded ; charset=UTF-8" FORM$ POST
   s" api://up" ANSWERED ;

: CLIENTS ( -- )
   s" a wrong secret is invalid_client" T-LABEL
   FORM-RESET s" client_id=app&client_secret=s3cr&grant_type=client_credentials" FORM+
   ASK 401 s" invalid_client" REFUSED
   s" an unknown client is invalid_client" T-LABEL
   FORM-RESET s" client_id=other&client_secret=s3cr%26t%2B1&grant_type=client_credentials" FORM+
   ASK 401 s" invalid_client" REFUSED
   s" a form's + is a space" T-LABEL
   FORM-RESET s" client_id=app&client_secret=s3cr%26t+1&grant_type=client_credentials" FORM+
   ASK 401 s" invalid_client" REFUSED
   s" a confidential client that leaves out its secret is invalid_client" T-LABEL
   FORM-RESET s" client_id=app&grant_type=client_credentials&scope=api%3A%2F%2Fup%2F.default" FORM+
   ASK 401 s" invalid_client" REFUSED
   s" a public client asking for client credentials is invalid_client" T-LABEL
   FORM-RESET s" client_id=cli&grant_type=client_credentials&scope=api%3A%2F%2Fup%2F.default" FORM+
   ASK 401 s" invalid_client" REFUSED ;

: REQUESTS ( -- )
   s" an unknown grant is unsupported_grant_type" T-LABEL
   APP s" &grant_type=password" FORM+
   ASK 400 s" unsupported_grant_type" REFUSED
   s" a field given twice is invalid_request" T-LABEL
   CREDENTIALS s" &client_id=app" FORM+
   ASK 400 s" invalid_request" REFUSED
   s" a body that is not a form is invalid_request" T-LABEL
   CREDENTIALS s" /token" s" Content-Type: text/plain" FORM$ POST
   400 s" invalid_request" REFUSED
   s" a scope that names no audience is invalid_request" T-LABEL
   APP s" &grant_type=client_credentials" FORM+
   ASK 400 s" invalid_request" REFUSED
   APP s" &grant_type=client_credentials&scope=" FORM+
   ASK 400 s" invalid_request" REFUSED
   APP s" &grant_type=client_credentials&scope=/.default" FORM+
   ASK 400 s" invalid_request" REFUSED
   s" app" LATER ASSERT UNSCOPED ASK 400 s" invalid_request" REFUSED
   s" a scope that is not UTF-8 is invalid_request" T-LABEL
   APP s" &grant_type=client_credentials&scope=%FF%FE" FORM+
   ASK 400 s" invalid_request" REFUSED ;

: OBO ( -- )
   s" On-Behalf-Of copies the assertion's oid and tid" T-LABEL
   s" app" LATER ASSERT ON-BEHALF ASK s" api://down" ANSWERED
   CLAIMS$ s" oid" MEMBER$ s" user-1" T$=
   CLAIMS$ s" tid" MEMBER$ s" tenant-1" T$=
   s" an assertion with a changed signature is invalid_grant" T-LABEL
   s" app" LATER ASSERT FLIP ON-BEHALF ASK 400 s" invalid_grant" REFUSED
   s" an assertion for another audience is invalid_grant" T-LABEL
   s" other" LATER ASSERT ON-BEHALF ASK 400 s" invalid_grant" REFUSED
   s" an expired assertion is invalid_grant" T-LABEL
   s" app" epoch-seconds 10 - ASSERT ON-BEHALF ASK 400 s" invalid_grant" REFUSED
   s" a public client asking On-Behalf-Of is invalid_client" T-LABEL
   s" cli" LATER ASSERT
   FORM-RESET s" client_id=cli&grant_type=urn%3Aietf%3Aparams%3Aoauth%3Agrant-type%3Ajwt-bearer" FORM+
   s" &requested_token_use=on_behalf_of&scope=api%3A%2F%2Fdown%2F.default&assertion=" FORM+ ASSERTION$ FORM+
   ASK 401 s" invalid_client" REFUSED ;

: MINTS ( -- )
   s" /mint answers MINT's token for the same claims" T-LABEL
   S\" {\"sub\":\"s-1\",\"n\":7}" {: c:str :}
   s" /mint" s" Content-Type: application/json" c POST 200 T=
   BODY$ c MINTED @ TEST-ISSUER:MINT T$= ;


: ISSUER$ ( -- str ) ISSUER-URL ISSUER-URL-U @ >str ;
: KEYS-URL$ ( -- str ) KEYS-URL KEYS-URL-U @ >str ;
: ENDPOINT$ ( -- str ) ENDPOINT ENDPOINT-U @ >str ;
: FAULT-URL$ ( -- str ) FAULT-URL FAULT-URL-U @ >str ;
: COUNT-URL$ ( -- str ) COUNT-URL COUNT-URL-U @ >str ;

\ The running server's port and URLs, kept for its restart and stop.
: ADDRESSES ( -- )
   HTTP:PORT PORT-KEPT !
   s" " URL ISSUER-URL COPY ISSUER-URL-U !
   s" /keys" URL KEYS-URL COPY KEYS-URL-U !
   s" /token" URL ENDPOINT COPY ENDPOINT-U !
   s" /token-fault" URL FAULT-URL COPY FAULT-URL-U !
   s" /token-count" URL COUNT-URL COPY COUNT-URL-U ! ;

\ A JSON token answer whose access_token runs past BEARER's TOKEN-CAP.
: OVERSIZE-FILL ( -- )
   S\" {\"access_token\":\"" OVERSIZE COPY {: hu:n :}
   PAST-CAP 2 - hu ?do [char] a OVERSIZE i + c! loop
   [char] " OVERSIZE PAST-CAP 2 - + c!
   [char] } OVERSIZE PAST-CAP 1- + c! ;

OVERSIZE-FILL

\ The admitted caller's claim the body names, 404 when it has none.
: CLAIM-ROUTE ( HTTP:request HTTP:response -- )
   {: asked:HTTP:request answer:HTTP:response :}
   asked answer BEARER:ADMIT? 0= if exit then
   asked HTTP:BODY$ BEARER:CLAIM$ 0= if drop answer 404 HTTP:STATUS! exit then
   {: v:str :} answer v HTTP:BODY! ;

: FAULT-ROUTE ( HTTP:request HTTP:response -- )
   {: asked:HTTP:request answer:HTTP:response :}
   FAULT-MODE @ {: mode:n :}
   mode BUSY = if answer 503 HTTP:STATUS! exit then
   mode LIMITED = if answer 429 HTTP:STATUS! exit then
   mode MOVED = if answer 302 HTTP:STATUS! exit then
   mode NO-TOKEN = if answer S\" {\"expires_in\":60}" HTTP:BODY! exit then
   mode NOT-JSON = if answer s" not json" HTTP:BODY! exit then
   mode REPEATED = if
      answer S\" {\"access_token\":\"t\",\"access_token\":\"u\",\"expires_in\":60}"
      HTTP:BODY! exit
   then
   mode OVERSIZED = if answer OVERSIZE PAST-CAP >str HTTP:BODY! exit then
   mode BUSY-OVERSIZED = if answer 503 HTTP:STATUS! answer OVERSIZE PAST-CAP >str HTTP:BODY! exit then
   mode NOT-STRING = if answer S\" {\"access_token\":1,\"expires_in\":60}" HTTP:BODY! exit then
   mode NO-LIFETIME = if answer S\" {\"access_token\":\"t\"}" HTTP:BODY! exit then
   mode LIFETIME-STRING = if answer S\" {\"access_token\":\"t\",\"expires_in\":\"3600\"}" HTTP:BODY! exit then
   mode LIFETIME-NOT-DIGITS = if answer S\" {\"access_token\":\"t\",\"expires_in\":\"36x\"}" HTTP:BODY! exit then
   mode LIFETIME-ZERO = if answer S\" {\"access_token\":\"t\",\"expires_in\":0}" HTTP:BODY! exit then
   mode RENEWED = if answer S\" {\"access_token\":\"fault\\/token\",\"expires_in\":60,\"refresh_token\":\"r\\/2\\u0041\"}" HTTP:BODY! exit then
   mode RENEWAL-NUMBER = if answer S\" {\"access_token\":\"t\",\"expires_in\":60,\"refresh_token\":1}" HTTP:BODY! exit then
   asked HTTP:BODY$ POSTED COPY POSTED-U !
   answer S\" {\"expires_in\":60,\"access_token\":\"fault\\/token\"}" HTTP:BODY! ;

\ /token-count's state: the POSTs it has answered or holds, a HOLD that keeps
\ each answer waiting while set, a FAIL that makes each answer a 503 while
\ set, and the expires_in each answer states.
variable ASKS
variable HOLD
variable FAIL
variable LIFE
$40 constant REPLY-CAP
$10 constant REPLY-SLOTS
REPLY-CAP REPLY-SLOTS * BUFFER: REPLIES

\ The text appended at the row's length: the row and its new length.
: PUT ( ^u8 n str -- ^u8 n )
   {: at:^ u:n t:str :}
   t at u + COPY drop at u t STR:LEN + ;

\ The answer for the count, in a slot of its own, so answers in flight at
\ once share no bytes.
: COUNTED ( n -- str )
   {: k:n :}
   REPLIES k REPLY-SLOTS mod REPLY-CAP * + 0
   S\" {\"expires_in\":" PUT  LIFE @ FMT:INT$ PUT
   S\" ,\"access_token\":\"t" PUT  k FMT:INT$ PUT  S\" \"}" PUT >str ;

: COUNT-ROUTE ( HTTP:request HTTP:response -- )
   {: asked:HTTP:request answer:HTTP:response :}
   1 ASKS atomic-add 1+ {: k:n :}
   [: HOLD atomic@ 0= ;] TEST-WAIT:POLL
   FAIL atomic@ 0<> if answer 503 HTTP:STATUS! exit then
   answer k COUNTED HTTP:BODY! ;

\ The IMDS route's state: the tokens it has answered or holds, the resource
\ a case asks for, and the status it answers, 200 for a token.
variable IMDS-ASKS
variable IMDS-STATUS
200 IMDS-STATUS !
variable IMDS-OVERSIZED             \ while set, another status has a body past TOKEN-CAP
URL-CAP BUFFER: RESOURCE
variable RESOURCE-U
: RESOURCE$ ( -- str ) RESOURCE RESOURCE-U @ >str ;
: RESOURCE! ( str -- ) RESOURCE COPY RESOURCE-U ! ;

\ The token answer for the count, expires_in as a digit string as IMDS
\ sends it.
: IMDS-COUNTED ( n -- str )
   {: k:n :}
   REPLIES k REPLY-SLOTS mod REPLY-CAP * + 0
   S\" {\"access_token\":\"i" PUT  k FMT:INT$ PUT
   S\" \",\"expires_in\":\"" PUT  LIFE @ FMT:INT$ PUT  S\" \"}" PUT >str ;

\ Whether the query's parameter, decoded into the vector, is the text.
: PARAM-IS? ( HTTP:request str str ^VEC:vector<u8> -- bool )
   {: asked:HTTP:request k:str want:str v:^ :}
   v 0 VEC:RESIZE
   asked k v HTTP:PARAM+ 0= if false exit then
   v VEC:BYTES want STR:= ;

: IMDS-ASKED? ( HTTP:request ^VEC:vector<u8> -- bool )
   {: asked:HTTP:request v:^ :}
   asked s" Metadata" HTTP:HEADER-OF$ 0= if drop false exit then
   s" true" STR:= 0= if false exit then
   asked s" api-version" s" 2018-02-01" v PARAM-IS? 0= if false exit then
   asked s" resource" RESOURCE$ v PARAM-IS? ;

: IMDS-ROUTE ( HTTP:request HTTP:response -- )
   {: asked:HTTP:request answer:HTTP:response :}
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW {: v:^ :}
   asked v IMDS-ASKED? {: ok:bool :}
   v VEC:DELETE
   ok 0= if answer 400 HTTP:STATUS! exit then
   IMDS-STATUS @ {: status:n :}
   status 200 <> if
      answer status HTTP:STATUS!
      IMDS-OVERSIZED @ 0<> if answer OVERSIZE PAST-CAP >str HTTP:BODY! then
      exit
   then
   1 IMDS-ASKS atomic-add 1+ {: k:n :}
   [: HOLD atomic@ 0= ;] TEST-WAIT:POLL
   answer k IMDS-COUNTED HTTP:BODY! ;

: AUTH+ ( str -- ) AUTH AUTH-U @ + COPY AUTH-U +! ;

\ The token presented to /claim, asking for the named claim: the status, the
\ claim in BODY.
: CLAIM ( str str -- n )
   {: t:str name:str :}
   0 AUTH-U ! s" Authorization: Bearer " AUTH+ t AUTH+
   s" /claim" OPEN {: h:CURL:handle :}
   h AUTH AUTH-U @ >str CURL:HEADER+ OK
   h s" Content-Type: text/plain" CURL:HEADER+ OK
   h name CURL:BODY! OK
   h FINISH ;

: UP ( -- str ) s" api://up/.default" ;
: OWN-TOKEN ( -- str n ) UP BEARER-GRANT:OWN GOT BEARER:TOKEN ;
: ZEN ( -- str ) s" api://zen/.default" ;
: FAULT-TOKEN ( -- str n )
   ZEN s" a.b-c_d~e" dup STR:DATA swap STR:LEN BEARER-GRANT:FOR-PERSON
   GOT BEARER:TOKEN ;
\ /token-fault answering as the mode makes TOKEN throw the code.
: FAULT-THROWS ( n n -- )
   {: mode:n code:n :}
   mode FAULT-MODE !
   [: FAULT-TOKEN drop drop ;] code TTHROWSQ ;

create LOG-PATH PATH-CAP allot      \ a case's stderr capture file
variable LOG-PATH-U
TYPED-VARIABLE LOG-B ^VEC:vector<u8>  \ the capture file's bytes, read back
$A constant LF

: LOG-PATH$ ( -- str ) LOG-PATH LOG-PATH-U @ >str ;

: LOG-INIT ( -- )
   s" test-issuer-test" FS:MKTEMP-DIR {: dir:str :}
   dir FS:CLEANUP-TREE+
   dir s" stderr.txt" LOG-PATH FS:JOIN-PATH LOG-PATH-U !
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW LOG-B ! ;

\ The whole capture file, valid until the next read.
: LOG$ ( -- str )
   LOG-PATH$ FS:FILE-SIZE {: size:n :}
   LOG-B @ size VEC:RESIZE
   LOG-B @ VEC:BYTES {: dst:str :}
   LOG-PATH$ dst STR:DATA dst STR:LEN FS:READ-ALL size T=
   LOG-B @ VEC:BYTES ;

: LINES ( -- n )
   LOG$ {: s:str :}
   0 s STR:LEN 0 ?do s STR:DATA i + c@ LF = if 1+ then loop ;

: CAPTURED ( [ -- ] -- )
   LOG-PATH$ STDERR:CAPTURE
   [: STDERR:;CAPTURE ;] finally ;

\ The one captured line after its stamp, empty when the capture is shorter.
: LINE$ ( -- str )
   LOG$ {: s:str :}
   s STR:LEN DATE:TIME-LEN < if s STR:DATA 0 >str exit then
   s STR:DATA DATE:TIME-LEN + s STR:LEN DATE:TIME-LEN - >str ;

: BEARER-GRANTS ( -- )
   s" TOKEN before CLIENT! is E-HTTP-STATE" T-LABEL
   [: OWN-TOKEN drop drop ;] E-HTTP-STATE TTHROWSQ
   ENDPOINT$ s" app" s" s3cr&t+1" BEARER:CLIENT!
   s" TOKEN's client credentials token is admitted, azp the client, for expires_in" T-LABEL
   OWN-TOKEN ISSUED-S T= s" azp" CLAIM 200 T=
   BODY$ s" app" T$=
   s" TOKEN for a person is admitted with the assertion's oid" T-LABEL
   s" app" LATER ASSERT
   UP ASSERTION ASSERTION-U @ BEARER-GRANT:FOR-PERSON GOT BEARER:TOKEN drop s" oid" CLAIM 200 T=
   BODY$ s" user-1" T$=
   s" a 400 is E-REFUSED, the vector holding the issuer's answer" T-LABEL
   [: UP s" a.b.c" dup STR:DATA swap STR:LEN BEARER-GRANT:FOR-PERSON
      GOT BEARER:TOKEN drop drop ;] BEARER:E-REFUSED TTHROWSQ
   GOT VEC:BYTES S\" {\"error\":\"invalid_grant\"}" T$=
   s" it leaves no byte of the earlier answer past the vector's count" T-LABEL
   GOT-ZEROED? TTRUE
   s" a wrong secret is E-REFUSED, the vector holding the issuer's answer" T-LABEL
   ENDPOINT$ s" app" s" wrong" BEARER:CLIENT!
   [: [: OWN-TOKEN drop drop ;] BEARER:E-REFUSED TTHROWSQ ;] CAPTURED
   GOT VEC:BYTES S\" {\"error\":\"invalid_client\"}" T$=
   s" a refusal logs the issuer's status and answer once, tagged bearer" T-LABEL
   LINES 1 T=
   FORM-RESET S\"  error bearer: token request answered 401: " FORM+
   S\" {\"error\":\"invalid_client\"}\n" FORM+
   LINE$ FORM$ T$=
   ENDPOINT$ s" cli" s" " BEARER:CLIENT!
   s" REFRESH from a public client is admitted with the refresh token's oid, for expires_in" T-LABEL
   s" cli" LATER ASSERT
   UP ASSERTION$ GOT BEARER:REFRESH ISSUED-S T= {: r:str :}
   s" oid" CLAIM 200 T= BODY$ s" user-1" T$=
   s" its new refresh token refreshes again" T-LABEL
   r ASSERTION$ T$<>
   r ASSERTION COPY ASSERTION-U !
   ASSERTION-U @ 0 > TTRUE
   UP ASSERTION$ GOT BEARER:REFRESH ISSUED-S T= drop
   s" oid" CLAIM 200 T= BODY$ s" user-1" T$=
   s" an expired refresh token is E-REFUSED, the vector holding invalid_grant" T-LABEL
   s" cli" epoch-seconds 10 - ASSERT
   [: UP ASSERTION$ GOT BEARER:REFRESH drop drop drop ;] BEARER:E-REFUSED TTHROWSQ
   GOT VEC:BYTES S\" {\"error\":\"invalid_grant\"}" T$=
   s" another client's refresh token is E-REFUSED, the vector holding invalid_grant" T-LABEL
   s" app" LATER ASSERT
   [: UP ASSERTION$ GOT BEARER:REFRESH drop drop drop ;] BEARER:E-REFUSED TTHROWSQ
   GOT VEC:BYTES S\" {\"error\":\"invalid_grant\"}" T$= ;

: BEARER-FAULTS ( -- )
   FAULT-URL$ s" app" s" s3cr&t+1" BEARER:CLIENT!
   s" TOKEN posts the On-Behalf-Of form percent-encoded and decodes the token and an expires_in sent before it" T-LABEL
   ECHO FAULT-MODE !
   FAULT-TOKEN 60 T= s" fault/token" T$=
   FORM-RESET
   s" grant_type=urn%3Aietf%3Aparams%3Aoauth%3Agrant-type%3Ajwt-bearer" FORM+
   s" &client_id=app&client_secret=s3cr%26t%2B1&assertion=a.b-c_d%7Ee" FORM+
   s" &scope=api%3A%2F%2Fzen%2F.default&requested_token_use=on_behalf_of" FORM+
   POSTED POSTED-U @ >str FORM$ T$=
   s" a 503 is E-UNAVAILABLE" T-LABEL
   BUSY BEARER:E-UNAVAILABLE FAULT-THROWS
   s" a 429 is E-UNAVAILABLE" T-LABEL
   LIMITED BEARER:E-UNAVAILABLE FAULT-THROWS
   s" a 302 is E-RESPONSE" T-LABEL
   MOVED BEARER:E-RESPONSE FAULT-THROWS
   s" a 200 that is not JSON is E-RESPONSE" T-LABEL
   [: NOT-JSON BEARER:E-RESPONSE FAULT-THROWS ;] CAPTURED
   s" its line says the answer is not JSON" T-LABEL
   LINES 1 T=
   FORM-RESET s"  error bearer: token answer 200 is not JSON " FORM+
   S\" naming each member once\n" FORM+
   LINE$ FORM$ T$=
   s" a 200 naming access_token twice is E-RESPONSE" T-LABEL
   REPEATED BEARER:E-RESPONSE FAULT-THROWS
   s" a 200 without access_token is E-RESPONSE" T-LABEL
   NO-TOKEN BEARER:E-RESPONSE FAULT-THROWS
   s" a 200 whose access_token is not a string is E-RESPONSE" T-LABEL
   NOT-STRING BEARER:E-RESPONSE FAULT-THROWS
   s" a 200 without expires_in is E-RESPONSE" T-LABEL
   [: NO-LIFETIME BEARER:E-RESPONSE FAULT-THROWS ;] CAPTURED
   s" its line names the status and the member, never the answer" T-LABEL
   LINES 1 T=
   FORM-RESET s"  error bearer: token answer 200 lacks " FORM+
   S\" a positive integer expires_in\n" FORM+
   LINE$ FORM$ T$=
   s" TOKEN reads an expires_in sent as a digit string" T-LABEL
   LIFETIME-STRING FAULT-MODE !
   FAULT-TOKEN 3600 T= s" t" T$=
   s" a 200 whose expires_in is a string not all digits is E-RESPONSE" T-LABEL
   LIFETIME-NOT-DIGITS BEARER:E-RESPONSE FAULT-THROWS
   s" a 200 whose expires_in is 0 is E-RESPONSE" T-LABEL
   LIFETIME-ZERO BEARER:E-RESPONSE FAULT-THROWS
   s" a 200 past TOKEN-CAP is E-RESPONSE" T-LABEL
   [: OVERSIZED BEARER:E-RESPONSE FAULT-THROWS ;] CAPTURED
   s" its one line names the status and TOKEN-CAP" T-LABEL
   LINES 1 T=
   FORM-RESET S\"  error bearer: token answer 200 past 65536 bytes\n" FORM+
   LINE$ FORM$ T$=
   s" it leaves no answer byte in the vector's storage" T-LABEL
   GOT-ZEROED? TTRUE
   s" a 503 past TOKEN-CAP is E-UNAVAILABLE" T-LABEL
   [: BUSY-OVERSIZED BEARER:E-UNAVAILABLE FAULT-THROWS ;] CAPTURED
   s" its one line names the status and TOKEN-CAP" T-LABEL
   LINES 1 T=
   FORM-RESET S\"  error bearer: token answer 503 past 65536 bytes\n" FORM+
   LINE$ FORM$ T$=
   s" it leaves no answer byte in the vector's storage" T-LABEL
   GOT-ZEROED? TTRUE
   s" REFRESH decodes the token, and an answer without refresh_token gives an empty one" T-LABEL
   ECHO FAULT-MODE !
   ZEN s" r.1-x" GOT BEARER:REFRESH 60 T= STR:LEN 0 T= s" fault/token" T$=
   s" REFRESH from a confidential client posts its secret in the refresh form" T-LABEL
   FORM-RESET s" grant_type=refresh_token&client_id=app&client_secret=s3cr%26t%2B1" FORM+
   s" &refresh_token=r.1-x&scope=api%3A%2F%2Fzen%2F.default" FORM+
   POSTED POSTED-U @ >str FORM$ T$=
   s" REFRESH decodes an escaped refresh token after an escaped token" T-LABEL
   RENEWED FAULT-MODE !
   ZEN s" r.1-x" GOT BEARER:REFRESH 60 T= s" r/2A" T$= s" fault/token" T$=
   s" a 200 whose refresh_token is not a string is E-RESPONSE, its one line naming the member" T-LABEL
   RENEWAL-NUMBER FAULT-MODE !
   [: [: ZEN s" r.1-x" GOT BEARER:REFRESH drop drop drop ;] BEARER:E-RESPONSE TTHROWSQ ;] CAPTURED
   LINES 1 T=
   FORM-RESET s"  error bearer: token answer 200 lacks " FORM+ S\" a refresh_token string\n" FORM+
   LINE$ FORM$ T$=
   s" TOKEN ignores a refresh_token that is not a string" T-LABEL
   FAULT-TOKEN 60 T= s" t" T$=
   ECHO FAULT-MODE !
   FAULT-URL$ s" cli" s" " BEARER:CLIENT!
   s" REFRESH from a public client posts no client_secret" T-LABEL
   ZEN s" r.1-x" GOT BEARER:REFRESH drop drop drop
   FORM-RESET s" grant_type=refresh_token&client_id=cli" FORM+
   s" &refresh_token=r.1-x&scope=api%3A%2F%2Fzen%2F.default" FORM+
   POSTED POSTED-U @ >str FORM$ T$= ;

8 constant ASKERS
4 constant AT-ONCE                  \ the tasks the asked-at-once cases run
$40 constant SEEN-CAP
\ The token text each asker ran on; slot ASKERS is the main task's.
SEEN-CAP ASKERS 1+ * BUFFER: SEEN
ASKERS 1+ TYPED-BUFFER SEEN-U n
ASKERS TYPED-BUFFER CAUGHT n        \ the code each waiter's CURRENT threw
variable ENTERED                    \ the tasks that have begun their ask
TASK:MIN-STACK TASK:TASK ASKER-0
TASK:MIN-STACK TASK:TASK ASKER-1
TASK:MIN-STACK TASK:TASK ASKER-2
TASK:MIN-STACK TASK:TASK ASKER-3
TASK:MIN-STACK TASK:TASK ASKER-4
TASK:MIN-STACK TASK:TASK ASKER-5
TASK:MIN-STACK TASK:TASK ASKER-6
TASK:MIN-STACK TASK:TASK ASKER-7
ASKERS TYPED-BUFFER TASKS ^n

: TASKS! ( -- )
   ASKER-0 0 TASKS !  ASKER-1 1 TASKS !  ASKER-2 2 TASKS !
   ASKER-3 3 TASKS !  ASKER-4 4 TASKS !  ASKER-5 5 TASKS !
   ASKER-6 6 TASKS !  ASKER-7 7 TASKS ! ;
TASKS!

: SEEN$ ( n -- str ) {: i:n :} SEEN i SEEN-CAP * + i SEEN-U @ >str ;

: SAW ( str n -- )
   {: t:str i:n :}
   t SEEN i SEEN-CAP * + COPY i SEEN-U ! ;

\ CURRENT's token for the scope, copied into slot i.
: CURRENT-INTO ( str n -- )
   {: scope:str i:n :}
   scope [: i SAW ;] BEARER:CURRENT ;

: CURRENT$ ( str -- str ) ASKERS CURRENT-INTO ASKERS SEEN$ ;

: ASKING ( n -- ) LIFE ! 0 ASKS atomic! ;

: COUNT-CLIENT ( -- ) COUNT-URL$ s" app" s" s3cr&t+1" BEARER:CLIENT! ;
: KEPT ( -- str ) s" api://kept/.default" ;
: DUE ( -- str ) s" api://due/.default" ;
: SHARED ( -- str ) s" api://shared/.default" ;
: FAILING ( -- str ) s" api://failing/.default" ;

: ASKER ( n -- )
   {: i:n :}
   1 ENTERED atomic-add drop
   SHARED i CURRENT-INTO 0 TASK:RETURN ;

\ Catches its CURRENT's code in the slot its place in ENTERED gives it.
: WAITER ( -- )
   1 ENTERED atomic-add {: i:n :}
   [: FAILING i CURRENT-INTO ;] catch i CAUGHT !
   0 TASK:RETURN ;

: JOINED ( ^n -- )
   TASK:JOIN MATCH result
      ok OF ENDOF
      err OF throw ENDOF
   ;MATCH 0 T= ;

: ALL-JOINED ( n -- ) 0 ?do i TASKS @ JOINED loop ;

\ Waits until n tasks have begun their ask and the count has one.
: ALL-ASKING ( n ^n -- )
   {: n:n asks:^ :}
   [: ENTERED atomic@ n = asks atomic@ 1 >= and ;] TEST-WAIT:POLL ;

: ASKED-AT-ONCE ( -- )
   s" CURRENT from four tasks at once asks once, and each runs on its token" T-LABEL
   3600 ASKING  0 ENTERED atomic!  1 HOLD atomic!
   [: 0 ASKER ;] ASKER-0 TASK:ACTIVATE
   [: 1 ASKER ;] ASKER-1 TASK:ACTIVATE
   [: 2 ASKER ;] ASKER-2 TASK:ACTIVATE
   [: 3 ASKER ;] ASKER-3 TASK:ACTIVATE
   AT-ONCE ASKS ALL-ASKING
   0 HOLD atomic!
   AT-ONCE ALL-JOINED
   AT-ONCE 0 ?do i SEEN$ s" t1" T$= loop
   ASKS atomic@ 1 T= ;

: WAITED-ON-FAILURE ( -- )
   s" tasks that waited on a failed ask throw its code, asking no more" T-LABEL
   3600 ASKING  0 ENTERED atomic!  1 FAIL atomic!  1 HOLD atomic!
   ASKERS 0 ?do [: WAITER ;] i TASKS @ TASK:ACTIVATE loop
   ASKERS ASKS ALL-ASKING
   0 HOLD atomic!
   ASKERS ALL-JOINED
   ASKERS 0 ?do i CAUGHT @ BEARER:E-UNAVAILABLE T= loop
   \ A task that reads the asks count only after the failed ask ended asks
   \ again, as a call begun after it: the count is bounded, not one.
   ASKS atomic@ ASKERS < TTRUE
   s" a CURRENT that begins after a failed ask asks again" T-LABEL
   0 FAIL atomic!  3600 ASKING
   FAILING CURRENT$ s" t1" T$=  ASKS atomic@ 1 T= ;

: BEARER-CURRENT ( -- )
   COUNT-CLIENT
   s" CURRENT keeps a scope's token while it lasts, asking once per scope" T-LABEL
   3600 ASKING
   KEPT CURRENT$ s" t1" T$=  KEPT CURRENT$ s" t1" T$=  ASKS atomic@ 1 T=
   s" api://other/.default" CURRENT$ s" t2" T$=  ASKS atomic@ 2 T=
   s" CLIENT! drops every kept token, so the next CURRENT asks" T-LABEL
   COUNT-CLIENT
   KEPT CURRENT$ s" t3" T$=  ASKS atomic@ 3 T=
   s" CURRENT asks again for a token that lasts LEEWAY-S or less" T-LABEL
   30 ASKING
   DUE CURRENT$ s" t1" T$=  DUE CURRENT$ s" t2" T$=
   ASKED-AT-ONCE
   WAITED-ON-FAILURE
   s" a failed ask throws TOKEN's code, and the next CURRENT asks again" T-LABEL
   FAULT-URL$ s" app" s" s3cr&t+1" BEARER:CLIENT!
   BUSY FAULT-MODE !
   [: DUE CURRENT$ drop ;] BEARER:E-UNAVAILABLE TTHROWSQ
   ECHO FAULT-MODE !
   COUNT-CLIENT 30 ASKING
   DUE CURRENT$ s" t1" T$=  ASKS atomic@ 1 T= ;

\ The IMDS origin: the test server's own.
: ORIGIN$ ( -- str ) ISSUER$ ;
: GOT$ ( -- str ) GOT VEC:BYTES ;
: IDENTITY ( -- ) ORIGIN$ RESOURCE$ GOT BEARER:IDENTITY-TOKEN ;
: IMDS-ASKING ( n -- ) LIFE ! 0 IMDS-ASKS atomic! ;

\ IDENTITY-TOKEN's token for the resource, copied into slot i.
: IMDS-ASKER ( n -- )
   {: i:n :}
   1 ENTERED atomic-add drop
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW {: v:^ :}
   ORIGIN$ RESOURCE$ v BEARER:IDENTITY-TOKEN
   v VEC:BYTES i SAW  v VEC:DELETE  0 TASK:RETURN ;

: IMDS-AT-ONCE ( -- )
   s" IDENTITY-TOKEN from four tasks at once asks once" T-LABEL
   s" https://storage.azure.com/" RESOURCE!
   3600 IMDS-ASKING  0 ENTERED atomic!  1 HOLD atomic!
   [: 0 IMDS-ASKER ;] ASKER-0 TASK:ACTIVATE
   [: 1 IMDS-ASKER ;] ASKER-1 TASK:ACTIVATE
   [: 2 IMDS-ASKER ;] ASKER-2 TASK:ACTIVATE
   [: 3 IMDS-ASKER ;] ASKER-3 TASK:ACTIVATE
   AT-ONCE IMDS-ASKS ALL-ASKING
   0 HOLD atomic!
   AT-ONCE ALL-JOINED
   AT-ONCE 0 ?do i SEEN$ s" i1" T$= loop
   IMDS-ASKS atomic@ 1 T= ;

\ IMDS answering the status makes IDENTITY-TOKEN throw the code.
: IMDS-THROWS ( n n -- )
   {: status:n code:n :}
   status IMDS-STATUS !
   [: IDENTITY ;] code TTHROWSQ ;

: BEARER-IDENTITY ( -- )
   s" IDENTITY-TOKEN appends the IMDS token after the vector's bytes" T-LABEL
   s" https://ossrdbms-aad.database.windows.net" RESOURCE!
   3600 IMDS-ASKING
   GOT 0 VEC:RESIZE  s" pw:" GOT VEC:APPEND
   IDENTITY GOT$ s" pw:i1" T$=  IMDS-ASKS atomic@ 1 T=
   s" a second IDENTITY-TOKEN asks nothing more" T-LABEL
   GOT 0 VEC:RESIZE  IDENTITY GOT$ s" i1" T$=  IMDS-ASKS atomic@ 1 T=
   s" CLIENT! keeps identity tokens" T-LABEL
   COUNT-CLIENT
   GOT 0 VEC:RESIZE  IDENTITY GOT$ s" i1" T$=  IMDS-ASKS atomic@ 1 T=
   s" IDENTITY-TOKEN asks again for a token that lasts LEEWAY-S or less" T-LABEL
   s" https://vault.azure.net/?a=1&b=2+3" RESOURCE!
   30 IMDS-ASKING
   GOT 0 VEC:RESIZE  IDENTITY GOT$ s" i1" T$=
   GOT 0 VEC:RESIZE  IDENTITY GOT$ s" i2" T$=
   IMDS-AT-ONCE
   s" https://management.azure.com/" RESOURCE!
   s" an IMDS 404 is E-UNAVAILABLE" T-LABEL
   404 BEARER:E-UNAVAILABLE IMDS-THROWS
   s" an IMDS 410 is E-UNAVAILABLE" T-LABEL
   410 BEARER:E-UNAVAILABLE IMDS-THROWS
   s" an IMDS 410 past TOKEN-CAP is E-UNAVAILABLE" T-LABEL
   1 IMDS-OVERSIZED !
   410 BEARER:E-UNAVAILABLE IMDS-THROWS
   0 IMDS-OVERSIZED !
   s" an IMDS 400 is E-REFUSED" T-LABEL
   400 BEARER:E-REFUSED IMDS-THROWS
   200 IMDS-STATUS !  GOT 0 VEC:RESIZE ;

\ Run once the server has stopped: its port is closed.
: BEARER-UNREACHED ( -- )
   s" an endpoint on a closed loopback port is E-TRANSFER" T-LABEL
   ENDPOINT$ s" app" s" s3cr&t+1" BEARER:CLIENT!
   [: OWN-TOKEN drop drop ;] BEARER:E-TRANSFER TTHROWSQ ;

8000001 constant PAST-CURL-INPUT          \ CURL_MAX_INPUT_LENGTH + 1

\ An endpoint of PAST-CURL-INPUT bytes; the caller releases it with
\ MEM:FREE-SPAN.
: LONG-URL ( -- SPAN:span<u8> )
   PAST-CURL-INPUT MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN {: url :}
   [char] a url SPAN:FILL
   s" http://127.0.0.1:1/" url SPAN:COPY
   url ;

: BEARER-LONG ( -- )
   s" an endpoint libcurl refuses to set is E-TRANSFER, logged with curl 43"
   T-LABEL
   LONG-URL {: url :}
   url SPAN:$ s" app" s" s3cr&t+1" BEARER:CLIENT!
   [: [: OWN-TOKEN drop drop ;] BEARER:E-TRANSFER TTHROWSQ ;] CAPTURED
   ENDPOINT$ s" app" s" s3cr&t+1" BEARER:CLIENT!   \ CLIENT! borrows the URL
   url MEM:FREE-SPAN
   LINES 1 T=
   LINE$ s"  error bearer: token request failed: curl 43: " STR:STARTS-WITH? TTRUE ;


\ The child's stdout read as far as it has written; true once a line ended or
\ the pipe closed.
: LINE? ( -- bool )
   CHILD-OUT @ LINE LINE-U @ + $100 LINE-U @ - read {: k:n :}
   k 0= if true exit then
   k 0 > if k LINE-U +! then
   LINE-U @ 0 > if LINE LINE-U @ + 1- c@ 10 = exit then
   false ;

\ The URL the issuer line names, empty when the child printed none.
: CHILD-URL ( -- str )
   LINE LINE-U @ >str s" issuer " STR:STARTS-WITH? TTRUE
   LINE-U @ 8 - {: u:n :}
   u 0 > if LINE 7 + u >str exit then
   s" " ;

\ A process boundary is under test: the tool's command line, its issuer line,
\ and its exit when stdin ends.
: TOOL ( -- )
   s" the tool names its issuer, serves it and exits 0 when stdin ends" T-LABEL
   FD:PIPE-PAIR {: in-r:fd in-w:fd :}
   FD:PIPE-PAIR {: out-r:fd out-w:fd :}
   ENGINE-CANDIDATE:PATH$ PROCESS:NEW
   s" --load" PROCESS:ARG
   s" tools/test-issuer.f" PROCESS:ARG
   s" --" PROCESS:ARG
   s" --client" PROCESS:ARG
   s" app" PROCESS:ARG
   s" s3cret" PROCESS:ARG
   0 >FD in-r PROCESS:FD
   1 >FD out-w PROCESS:FD
   PROCESS:SPAWN {: pid :}
   in-r FD>N close
   out-w FD>N close
   out-r PROCESS:NONBLOCK!
   out-r FD>N CHILD-OUT !
   0 LINE-U !
   [: LINE? ;] TEST-WAIT:POLL
   CHILD-URL {: url:str :}
   SB-RESET url SB-APPEND s" /.well-known/openid-configuration" SB-APPEND
   SB$ OPEN-AT FINISH 200 T=
   BODY$ s" issuer" MEMBER$ url T$=
   in-w FD>N close
   pid PROCESS:WAIT PROCESS:STATUS>RC RC>N 0 T=
   out-r FD>N close ;


\ BEARER:ISSUER! names this server's URL and is refused while HTTP serves, so
\ the server starts once to take a port and serves again on it.
: SERVE-CASES ( -- )
   AIO:START LOG-INIT
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW BODY !
   s" app" s" s3cr&t+1" TEST-ISSUER:CLIENT+
   s" cli" s" " TEST-ISSUER:CLIENT+
   HTTP:ROUTES-RESET
   TEST-ISSUER:ROUTES
   s" POST" s" /claim" [: CLAIM-ROUTE ;] HTTP:ROUTE
   s" POST" s" /token-fault" [: FAULT-ROUTE ;] HTTP:ROUTE
   s" POST" s" /token-count" [: COUNT-ROUTE ;] HTTP:ROUTE
   s" GET" s" /metadata/identity/oauth2/token" [: IMDS-ROUTE ;] HTTP:ROUTE
   0 SERVE-ON
   ADDRESSES
   HTTP:STOP
   ISSUER$ s" api://up" KEYS-URL$ BEARER:ISSUER!
   PORT-KEPT @ SERVE-ON
   DISCOVERY KEYS CLIENT-CREDENTIALS CLIENTS REQUESTS OBO MINTS
   BEARER-GRANTS BEARER-CURRENT BEARER-IDENTITY BEARER-FAULTS
   HTTP:STOP
   BEARER-UNREACHED BEARER-LONG
   TOOL
   AIO:STOP ;

T-RESET
SERVE-CASES
T-REPORT

;package
