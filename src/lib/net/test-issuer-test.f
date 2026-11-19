split-str
\ test-issuer-test.f - TEST-ISSUER over a real loopback port: discovery, the key
\ set, client credentials verified against the served key, invalid_client for
\ a confidential client without its secret and for a public client asking for
\ client credentials or On-Behalf-Of, unsupported_grant_type, invalid_request
\ for a repeated field and a wrong media type, On-Behalf-Of with a minted
\ assertion and its invalid_grant refusals, /mint against MINT, and
\ tools/test-issuer.f as a child process.
\ BEARER's client against it: TOKEN before CLIENT!, for each grant admitted by
\ ADMIT? with the caller's claims, a 400 and a 401 refusal, the 400 leaving no
\ byte of the longer answer before it past the BUF's length, an assertion inside
\ the BUF; REFRESH from a public client admitted with the refresh token's oid,
\ again with the new refresh token it answered, an expired and a foreign
\ refresh token refused invalid_grant, a refresh token inside the BUF; a closed port,
\ and on /token-fault the form's exact bytes, the answer's lifetime and the
\ answers TOKEN refuses: a 503, a 429 and a 302, a 2xx that is not JSON, one
\ without access_token, one whose access_token is not a string, one without
\ expires_in, one whose expires_in is a string, one whose expires_in is 0, and
\ a 200 and a 503 past TOKEN-CAP, each leaving only zero bytes in the BUF's
\ storage; REFRESH's form from a confidential and a public client, an answer
\ without refresh_token, an escaped refresh token decoded after an escaped
\ token, and a refresh_token that is not a string, E-RESPONSE logged for
\ REFRESH and ignored by TOKEN. CURRENT on /token-count: a token kept per
\ scope, asked again when it lasts LEEWAY-S or less, asked once for four tasks
\ at once, a failed ask's code thrown to the tasks waiting on it, and asked
\ again after a failed ask and after CLIENT!.
\ Run: bin/hb --load src/lib/net/test-issuer-test.f

require src/lib/test.f
require src/lib/test/wait.f
require src/lib/string.f
require src/lib/span.f
require src/lib/fmt.f
require src/lib/aio.f
require src/lib/base64.f
require src/lib/byte-buffer.f
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

$2000 constant CAP
CAP BUFFER: BODY
variable BODY-U
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
create GOT-HEAD BUF:HDR-BYTES allot
: GOT ( -- ^n ) GOT-HEAD CELL-VIEW ;
GOT $100 BUF:N>BLEN BUF:INIT

\ Whether GOT's storage past its length holds only zero bytes. The GROW spans that
\ storage within the capacity, so nothing moves, and GOT keeps its length.
: GOT-ZEROED? ( -- bool )
   GOT BUF:LEN@ {: len:NUM:byte-len :}
   GOT GOT BUF:CAP@ BUF:BLEN>N len BUF:BLEN>N - BUF:N>BLEN BUF:GROW SPAN:$ {: p:^ u:n :}
   true u 0 ?do p i + c@ 0<> if drop false then loop
   len GOT BUF:LEN! ;

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
variable FAULT-MODE
CAP BUFFER: POSTED
variable POSTED-U
$10001 constant PAST-CAP            \ one byte past BEARER's TOKEN-CAP
PAST-CAP BUFFER: OVERSIZE

: BODY$ ( -- ^u8 n ) BODY BODY-U @ ;
: TOKEN$ ( -- ^u8 n ) TOKEN TOKEN-U @ ;
: CLAIMS$ ( -- str ) CLAIMS CLAIMS-U @ ;
: ASSERTION$ ( -- str ) ASSERTION ASSERTION-U @ ;

\ The text copied into a row, answering its length.
: COPY ( ^u8 n ^u8 -- n )
   {: a:^ u:n row:^ :}
   a row u BYTE-COPY u ;


\ A string member of an object text as sent, empty when absent.
: MEMBER$ ( str str -- str )
   {: t:^ tu:n k:^ ku:n :}
   READER t tu JSON:READ-INIT JSON:READ-NEXT drop
   k ku JSON:READ-FIND-KEY 0= if JSON:READ-CLOSE s" " exit then
   JSON:READ-TOKEN JSON:T-STR <> if JSON:READ-CLOSE s" " exit then
   JSON:READ-SPAN$ {: a:^ u:n :} JSON:READ-CLOSE a u ;

\ An integer member of an object text, -1 when absent.
: MEMBER-INT ( str str -- n )
   {: t:^ tu:n k:^ ku:n :}
   READER t tu JSON:READ-INIT JSON:READ-NEXT drop
   k ku JSON:READ-FIND-KEY 0= if JSON:READ-CLOSE -1 exit then
   JSON:READ-TOKEN JSON:T-INT <> if JSON:READ-CLOSE -1 exit then
   JSON:READ-INT {: v:n :} JSON:READ-CLOSE v ;

\ A string member of the body's first key, as sent.
: KEY$ ( str -- str )
   {: k:^ ku:n :}
   READER BODY$ JSON:READ-INIT JSON:READ-NEXT drop
   s" keys" JSON:READ-FIND-KEY drop JSON:READ-NEXT drop
   k ku JSON:READ-FIND-KEY drop JSON:READ-SPAN$ {: a:^ u:n :} JSON:READ-CLOSE a u ;


: OK ( CURL:status -- )
   MATCH CURL:status
      ok OF true ENDOF
      failed OF drop false ENDOF
   ;MATCH TTRUE ;

: URL ( ^u8 n -- ^u8 n )
   {: path:^ pu:n :}
   SB-RESET s" http://127.0.0.1:" SB-APPEND HTTP:PORT FMT:SB-U path pu SB-APPEND SB$ ;

: OPEN-AT ( str -- CURL:handle )
   {: url:^ uu:n :}
   CURL:INIT
   MATCH CURL:init-result
      ready OF ENDOF
      failed OF drop -1 CURL:>HANDLE ENDOF
   ;MATCH {: h:CURL:handle :}
   h url uu CURL:URL! OK h ;

: OPEN ( ^u8 n -- CURL:handle ) URL OPEN-AT ;

\ The response's status and its body in BODY; -1 for a body past CAP, -2 for
\ a failed transfer.
: FINISH ( CURL:handle -- n )
   {: h:CURL:handle :}
   0 BODY-U !
   h BODY CAP >LEN CURL:PERFORM
   MATCH CURL:fetch-result
      response OF {: status:CURL:http-status got:len :}
         got LEN>N BODY-U ! status CURL:HTTP-STATUS>N ENDOF
      truncated OF drop drop -1 ENDOF
      failed OF drop -2 ENDOF
   ;MATCH
   h CURL:CLEANUP ;

: GET ( ^u8 n -- n ) OPEN FINISH ;

\ The body posted with the media type; the body is copied by libcurl.
: POST ( ^u8 n ^u8 n ^u8 n -- n )
   {: path:^ pu:n type:^ tu:n b:^ bu:n :}
   path pu OPEN {: h:CURL:handle :}
   h type tu CURL:HEADER+ OK
   h b bu CURL:BODY! OK
   h FINISH ;

: FORM-TYPE ( -- str ) s" Content-Type: application/x-www-form-urlencoded" ;

: FORM-RESET ( -- ) 0 FORM-U ! ;
: FORM+ ( str -- ) FORM FORM-U @ + COPY FORM-U +! ;
: FORM$ ( -- str ) FORM FORM-U @ ;
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
: REFUSED ( n n ^u8 n -- )
   {: status:n want:n code:^ cu:n :}
   SB-RESET S\" {\"error\":\"" SB-APPEND code cu SB-APPEND S\" \"}" SB-APPEND
   BODY$ SB$ T$=
   status want T= ;


\ The index of the first dot at or after from, else the text's length.
: DOT ( ^u8 n n -- n )
   {: t:^ u:n from:n :}
   u from ?do t i + c@ [char] . = if i unloop exit then loop u ;

: DECODED ( ^u8 n ^u8 n -- n ) SPAN:MAKE BASE64:DECODE-URL ;

\ The issued token copied from the answer; its header, claims and signature
\ decoded.
: ISSUED ( -- )
   BODY$ s" access_token" MEMBER$ TOKEN COPY TOKEN-U !
   TOKEN$ {: t:^ u:n :}
   t u 0 DOT {: d1:n :}
   t u d1 1+ DOT {: d2:n :}
   t d1 HEAD CAP DECODED HEAD-U !
   t d1 + 1+ d2 d1 - 1- CLAIMS CAP DECODED CLAIMS-U !
   t d2 + 1+ u d2 - 1- SIGNATURE CRYPTO:RS256-MAX-BYTES DECODED SIGNATURE-U ! ;

\ The issued token verifies against the key /keys served.
: VERIFIES ( -- )
   TOKEN$ {: t:^ u:n :}
   t u 0 DOT {: d1:n :}
   t u d1 1+ DOT {: d2:n :}
   MODULUS MODULUS-U @ EXPONENT EXPONENT-U @ t d2 SIGNATURE SIGNATURE-U @
   CRYPTO:RS256-VERIFY? TTRUE
   HEAD HEAD-U @ s" kid" MEMBER$ KID KID-U @ T$=
   HEAD HEAD-U @ s" alg" MEMBER$ s" RS256" T$=
   HEAD HEAD-U @ s" typ" MEMBER$ s" JWT" T$= ;

\ The answer is a 200 bearer token; the claims every issued token carries.
: ANSWERED ( n ^u8 n -- )
   {: status:n aud:^ au:n :}
   status 200 T=
   BODY$ s" token_type" MEMBER$ s" Bearer" T$=
   BODY$ s" expires_in" MEMBER-INT ISSUED-S T=
   ISSUED VERIFIES
   CLAIMS$ s" iss" MEMBER$ s" " URL T$=
   CLAIMS$ s" aud" MEMBER$ aud au T$=
   CLAIMS$ s" azp" MEMBER$ s" app" T$=
   CLAIMS$ s" iat" MEMBER-INT {: iat:n :}
   iat epoch-seconds 5 - >= TTRUE
   CLAIMS$ s" nbf" MEMBER-INT iat T=
   CLAIMS$ s" exp" MEMBER-INT iat ISSUED-S + T= ;

\ An assertion for the client, minted into ASSERTION.
: ASSERT ( ^u8 n n -- )
   {: aud:^ au:n exp:n :}
   SB-RESET S\" {\"aud\":\"" SB-APPEND aud au SB-APPEND S\" \",\"exp\":" SB-APPEND
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
   S\" {\"sub\":\"s-1\",\"n\":7}" {: c:^ cu:n :}
   s" /mint" s" Content-Type: application/json" c cu POST 200 T=
   BODY$ c cu MINTED @ TEST-ISSUER:MINT T$= ;


: ISSUER$ ( -- str ) ISSUER-URL ISSUER-URL-U @ ;
: KEYS-URL$ ( -- str ) KEYS-URL KEYS-URL-U @ ;
: ENDPOINT$ ( -- str ) ENDPOINT ENDPOINT-U @ ;
: FAULT-URL$ ( -- str ) FAULT-URL FAULT-URL-U @ ;
: COUNT-URL$ ( -- str ) COUNT-URL COUNT-URL-U @ ;

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
   asked HTTP:BODY$ BEARER:CLAIM$ 0= if 2drop answer 404 HTTP:STATUS! exit then
   {: v:^ vu:n :} answer v vu HTTP:BODY! ;

: FAULT-ROUTE ( HTTP:request HTTP:response -- )
   {: asked:HTTP:request answer:HTTP:response :}
   FAULT-MODE @ {: mode:n :}
   mode BUSY = if answer 503 HTTP:STATUS! exit then
   mode LIMITED = if answer 429 HTTP:STATUS! exit then
   mode MOVED = if answer 302 HTTP:STATUS! exit then
   mode NO-TOKEN = if answer S\" {\"expires_in\":60}" HTTP:BODY! exit then
   mode NOT-JSON = if answer s" not json" HTTP:BODY! exit then
   mode OVERSIZED = if answer OVERSIZE PAST-CAP HTTP:BODY! exit then
   mode BUSY-OVERSIZED = if answer 503 HTTP:STATUS! answer OVERSIZE PAST-CAP HTTP:BODY! exit then
   mode NOT-STRING = if answer S\" {\"access_token\":1,\"expires_in\":60}" HTTP:BODY! exit then
   mode NO-LIFETIME = if answer S\" {\"access_token\":\"t\"}" HTTP:BODY! exit then
   mode LIFETIME-STRING = if answer S\" {\"access_token\":\"t\",\"expires_in\":\"3600\"}" HTTP:BODY! exit then
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
   {: at:^ u:n t:^ tu:n :}
   t tu at u + COPY drop at u tu + ;

\ The answer for the count, in a slot of its own, so answers in flight at
\ once share no bytes.
: COUNTED ( n -- ^u8 n )
   {: k:n :}
   REPLIES k REPLY-SLOTS mod REPLY-CAP * + 0
   S\" {\"expires_in\":" PUT  LIFE @ FMT:INT$ PUT
   S\" ,\"access_token\":\"t" PUT  k FMT:INT$ PUT  S\" \"}" PUT ;

: COUNT-ROUTE ( HTTP:request HTTP:response -- )
   {: asked:HTTP:request answer:HTTP:response :}
   1 ASKS atomic-add 1+ {: k:n :}
   [: HOLD atomic@ 0= ;] TEST-WAIT:POLL
   FAIL atomic@ 0<> if answer 503 HTTP:STATUS! exit then
   answer k COUNTED HTTP:BODY! ;

: AUTH+ ( str -- ) AUTH AUTH-U @ + COPY AUTH-U +! ;

\ The token presented to /claim, asking for the named claim: the status, the
\ claim in BODY.
: CLAIM ( ^u8 n ^u8 n -- n )
   {: t:^ tu:n name:^ nu:n :}
   0 AUTH-U ! s" Authorization: Bearer " AUTH+ t tu AUTH+
   s" /claim" OPEN {: h:CURL:handle :}
   h AUTH AUTH-U @ CURL:HEADER+ OK
   h s" Content-Type: text/plain" CURL:HEADER+ OK
   h name nu CURL:BODY! OK
   h FINISH ;

: UP ( -- ^u8 n ) s" api://up/.default" ;
: OWN-TOKEN ( -- str n ) UP BEARER-GRANT:OWN GOT BEARER:TOKEN ;
: ZEN ( -- str ) s" api://zen/.default" ;
: FAULT-TOKEN ( -- str n )
   ZEN s" a.b-c_d~e" BEARER-GRANT:FOR-PERSON GOT BEARER:TOKEN ;
\ /token-fault answering as the mode makes TOKEN throw the code.
: FAULT-THROWS ( n n -- )
   {: mode:n code:n :}
   mode FAULT-MODE !
   [: FAULT-TOKEN drop 2drop ;] code TTHROWSQ ;

create LOG-PATH PATH-CAP allot      \ a case's stderr capture file
variable LOG-PATH-U
create LOG-B BUF:HDR-BYTES allot    \ the capture file's bytes, read back
$A constant LF

: LOG-PATH$ ( -- str ) LOG-PATH LOG-PATH-U @ ;

: LOG-INIT ( -- )
   s" test-issuer-test" FS:MKTEMP-DIR {: dir:^ dir-u:n :}
   dir dir-u FS:CLEANUP-TREE+
   dir dir-u s" stderr.txt" LOG-PATH FS:JOIN-PATH LOG-PATH-U !
   LOG-B 1 BUF:N>BLEN BUF:INIT ;

\ The whole capture file, valid until the next read.
: LOG$ ( -- str )
   LOG-PATH$ FS:FILE-SIZE {: size:n :}
   LOG-B BUF:CLEAR
   LOG-PATH$ LOG-B size BUF:N>BLEN BUF:GROW SPAN:$ FS:READ-ALL size T=
   LOG-B BUF:SPAN$ BUF:BLEN>N ;

: LINES ( -- n )
   LOG$ {: a:^ u:n :}
   0 u 0 ?do a i + c@ LF = if 1+ then loop ;

: CAPTURED ( [ -- ] -- )
   LOG-PATH$ STDERR:CAPTURE
   [: STDERR:;CAPTURE ;] finally ;

\ The one captured line after its stamp, empty when the capture is shorter.
: LINE$ ( -- str )
   LOG$ {: a:^ u:n :}
   u DATE:TIME-LEN < if a 0 exit then
   a DATE:TIME-LEN + u DATE:TIME-LEN - ;

: BEARER-GRANTS ( -- )
   s" TOKEN before CLIENT! is E-HTTP-STATE" T-LABEL
   [: OWN-TOKEN drop 2drop ;] E-HTTP-STATE TTHROWSQ
   ENDPOINT$ s" app" s" s3cr&t+1" BEARER:CLIENT!
   s" TOKEN's client credentials token is admitted, azp the client, for expires_in" T-LABEL
   OWN-TOKEN ISSUED-S T= s" azp" CLAIM 200 T=
   BODY$ s" app" T$=
   s" TOKEN for a person is admitted with the assertion's oid" T-LABEL
   s" app" LATER ASSERT
   UP ASSERTION$ BEARER-GRANT:FOR-PERSON GOT BEARER:TOKEN drop s" oid" CLAIM 200 T=
   BODY$ s" user-1" T$=
   s" a 400 is E-REFUSED, the BUF holding the issuer's answer" T-LABEL
   [: UP s" a.b.c" BEARER-GRANT:FOR-PERSON GOT BEARER:TOKEN drop 2drop ;] BEARER:E-REFUSED TTHROWSQ
   GOT BUF:SPAN$ BUF:BLEN>N S\" {\"error\":\"invalid_grant\"}" T$=
   s" it leaves no byte of the earlier answer past the BUF's length" T-LABEL
   GOT-ZEROED? TTRUE
   s" a wrong secret is E-REFUSED, the BUF holding the issuer's answer" T-LABEL
   ENDPOINT$ s" app" s" wrong" BEARER:CLIENT!
   [: [: OWN-TOKEN drop 2drop ;] BEARER:E-REFUSED TTHROWSQ ;] CAPTURED
   GOT BUF:SPAN$ BUF:BLEN>N S\" {\"error\":\"invalid_client\"}" T$=
   s" a refusal logs the issuer's status and answer once, tagged bearer" T-LABEL
   LINES 1 T=
   FORM-RESET S\"  error bearer: token request answered 401: " FORM+
   S\" {\"error\":\"invalid_client\"}\n" FORM+
   LINE$ FORM$ T$=
   ENDPOINT$ s" cli" s" " BEARER:CLIENT!
   s" REFRESH from a public client is admitted with the refresh token's oid, for expires_in" T-LABEL
   s" cli" LATER ASSERT
   UP ASSERTION$ GOT BEARER:REFRESH ISSUED-S T= {: r:^ ru:n :}
   s" oid" CLAIM 200 T= BODY$ s" user-1" T$=
   s" its new refresh token refreshes again" T-LABEL
   r ru ASSERTION$ T$<>
   r ru ASSERTION COPY ASSERTION-U !
   ASSERTION-U @ 0 > TTRUE
   UP ASSERTION$ GOT BEARER:REFRESH ISSUED-S T= 2drop
   s" oid" CLAIM 200 T= BODY$ s" user-1" T$=
   s" an expired refresh token is E-REFUSED, the BUF holding invalid_grant" T-LABEL
   s" cli" epoch-seconds 10 - ASSERT
   [: UP ASSERTION$ GOT BEARER:REFRESH drop 2drop 2drop ;] BEARER:E-REFUSED TTHROWSQ
   GOT BUF:SPAN$ BUF:BLEN>N S\" {\"error\":\"invalid_grant\"}" T$=
   s" another client's refresh token is E-REFUSED, the BUF holding invalid_grant" T-LABEL
   s" app" LATER ASSERT
   [: UP ASSERTION$ GOT BEARER:REFRESH drop 2drop 2drop ;] BEARER:E-REFUSED TTHROWSQ
   GOT BUF:SPAN$ BUF:BLEN>N S\" {\"error\":\"invalid_grant\"}" T$= ;

false [if] \ off until dot habu-add-the-checks-a49d054d
\ Runs after BEARER-GRANTS in SERVE-CASES.
: BEARER-BOUNDS ( -- )
   s" an assertion inside the BUF is E-BUF-BOUNDS" T-LABEL
   [: UP GOT BUF:SPAN$ BUF:BLEN>N BEARER-GRANT:FOR-PERSON GOT BEARER:TOKEN drop 2drop ;] E-BUF-BOUNDS TTHROWSQ
   s" a refresh token inside the BUF is E-BUF-BOUNDS" T-LABEL
   [: UP GOT BUF:SPAN$ BUF:BLEN>N GOT BEARER:REFRESH drop 2drop 2drop ;] E-BUF-BOUNDS TTHROWSQ ;
[then]

: BEARER-FAULTS ( -- )
   FAULT-URL$ s" app" s" s3cr&t+1" BEARER:CLIENT!
   s" TOKEN posts the On-Behalf-Of form percent-encoded and decodes the token and an expires_in sent before it" T-LABEL
   ECHO FAULT-MODE !
   FAULT-TOKEN 60 T= s" fault/token" T$=
   FORM-RESET
   s" grant_type=urn%3Aietf%3Aparams%3Aoauth%3Agrant-type%3Ajwt-bearer" FORM+
   s" &client_id=app&client_secret=s3cr%26t%2B1&assertion=a.b-c_d%7Ee" FORM+
   s" &scope=api%3A%2F%2Fzen%2F.default&requested_token_use=on_behalf_of" FORM+
   POSTED POSTED-U @ FORM$ T$=
   s" a 503 is E-UNAVAILABLE" T-LABEL
   BUSY BEARER:E-UNAVAILABLE FAULT-THROWS
   s" a 429 is E-UNAVAILABLE" T-LABEL
   LIMITED BEARER:E-UNAVAILABLE FAULT-THROWS
   s" a 302 is E-RESPONSE" T-LABEL
   MOVED BEARER:E-RESPONSE FAULT-THROWS
   s" a 200 that is not JSON is E-RESPONSE" T-LABEL
   NOT-JSON BEARER:E-RESPONSE FAULT-THROWS
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
   s" a 200 whose expires_in is a string is E-RESPONSE" T-LABEL
   LIFETIME-STRING BEARER:E-RESPONSE FAULT-THROWS
   s" a 200 whose expires_in is 0 is E-RESPONSE" T-LABEL
   LIFETIME-ZERO BEARER:E-RESPONSE FAULT-THROWS
   s" a 200 past TOKEN-CAP is E-RESPONSE" T-LABEL
   [: OVERSIZED BEARER:E-RESPONSE FAULT-THROWS ;] CAPTURED
   s" its one line names the status and TOKEN-CAP" T-LABEL
   LINES 1 T=
   FORM-RESET S\"  error bearer: token answer 200 past 65536 bytes\n" FORM+
   LINE$ FORM$ T$=
   s" it leaves no answer byte in the BUF's storage" T-LABEL
   GOT-ZEROED? TTRUE
   s" a 503 past TOKEN-CAP is E-UNAVAILABLE" T-LABEL
   [: BUSY-OVERSIZED BEARER:E-UNAVAILABLE FAULT-THROWS ;] CAPTURED
   s" its one line names the status and TOKEN-CAP" T-LABEL
   LINES 1 T=
   FORM-RESET S\"  error bearer: token answer 503 past 65536 bytes\n" FORM+
   LINE$ FORM$ T$=
   s" it leaves no answer byte in the BUF's storage" T-LABEL
   GOT-ZEROED? TTRUE
   s" REFRESH decodes the token, and an answer without refresh_token gives an empty one" T-LABEL
   ECHO FAULT-MODE !
   ZEN s" r.1-x" GOT BEARER:REFRESH 60 T= 0 T= drop s" fault/token" T$=
   s" REFRESH from a confidential client posts its secret in the refresh form" T-LABEL
   FORM-RESET s" grant_type=refresh_token&client_id=app&client_secret=s3cr%26t%2B1" FORM+
   s" &refresh_token=r.1-x&scope=api%3A%2F%2Fzen%2F.default" FORM+
   POSTED POSTED-U @ FORM$ T$=
   s" REFRESH decodes an escaped refresh token after an escaped token" T-LABEL
   RENEWED FAULT-MODE !
   ZEN s" r.1-x" GOT BEARER:REFRESH 60 T= s" r/2A" T$= s" fault/token" T$=
   s" a 200 whose refresh_token is not a string is E-RESPONSE, its one line naming the member" T-LABEL
   RENEWAL-NUMBER FAULT-MODE !
   [: [: ZEN s" r.1-x" GOT BEARER:REFRESH drop 2drop 2drop ;] BEARER:E-RESPONSE TTHROWSQ ;] CAPTURED
   LINES 1 T=
   FORM-RESET s"  error bearer: token answer 200 lacks " FORM+ S\" a refresh_token string\n" FORM+
   LINE$ FORM$ T$=
   s" TOKEN ignores a refresh_token that is not a string" T-LABEL
   FAULT-TOKEN 60 T= s" t" T$=
   ECHO FAULT-MODE !
   FAULT-URL$ s" cli" s" " BEARER:CLIENT!
   s" REFRESH from a public client posts no client_secret" T-LABEL
   ZEN s" r.1-x" GOT BEARER:REFRESH 2drop 2drop drop
   FORM-RESET s" grant_type=refresh_token&client_id=cli" FORM+
   s" &refresh_token=r.1-x&scope=api%3A%2F%2Fzen%2F.default" FORM+
   POSTED POSTED-U @ FORM$ T$= ;

8 constant ASKERS
4 constant AT-ONCE                  \ the tasks the asked-at-once case runs
$40 constant SEEN-CAP
\ The token text each asker ran on; slot ASKERS is the main task's.
SEEN-CAP ASKERS 1+ * BUFFER: SEEN
ASKERS 1+ TYPED-BUFFER SEEN-U n
ASKERS TYPED-BUFFER CAUGHT n        \ the code each waiter's CURRENT threw
variable ENTERED                    \ the tasks that have begun their CURRENT
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

: SEEN$ ( n -- str ) {: i:n :} SEEN i SEEN-CAP * + i SEEN-U @ ;

: SAW ( ^u8 n n -- )
   {: t:^ tu:n i:n :}
   t tu SEEN i SEEN-CAP * + COPY i SEEN-U ! ;

\ CURRENT's token for the scope, copied into slot i.
: CURRENT-INTO ( ^u8 n n -- )
   {: scope:^ su:n i:n :}
   scope su [: i SAW ;] BEARER:CURRENT ;

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

\ Waits until n tasks have begun their CURRENT and one has asked.
: ALL-ASKING ( n -- )
   {: n:n :}
   [: ENTERED atomic@ n = ASKS atomic@ 1 >= and ;] TEST-WAIT:POLL ;

: ASKED-AT-ONCE ( -- )
   s" CURRENT from four tasks at once asks once, and each runs on its token" T-LABEL
   3600 ASKING  0 ENTERED atomic!  1 HOLD atomic!
   [: 0 ASKER ;] ASKER-0 TASK:ACTIVATE
   [: 1 ASKER ;] ASKER-1 TASK:ACTIVATE
   [: 2 ASKER ;] ASKER-2 TASK:ACTIVATE
   [: 3 ASKER ;] ASKER-3 TASK:ACTIVATE
   AT-ONCE ALL-ASKING
   0 HOLD atomic!
   AT-ONCE ALL-JOINED
   AT-ONCE 0 ?do i SEEN$ s" t1" T$= loop
   ASKS atomic@ 1 T= ;

: WAITED-ON-FAILURE ( -- )
   s" tasks that waited on a failed ask throw its code, asking no more" T-LABEL
   3600 ASKING  0 ENTERED atomic!  1 FAIL atomic!  1 HOLD atomic!
   ASKERS 0 ?do [: WAITER ;] i TASKS @ TASK:ACTIVATE loop
   ASKERS ALL-ASKING
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
   [: DUE CURRENT$ 2drop ;] BEARER:E-UNAVAILABLE TTHROWSQ
   ECHO FAULT-MODE !
   COUNT-CLIENT 30 ASKING
   DUE CURRENT$ s" t1" T$=  ASKS atomic@ 1 T= ;

\ Run once the server has stopped: its port is closed.
: BEARER-UNREACHED ( -- )
   s" an endpoint on a closed loopback port is E-TRANSFER" T-LABEL
   ENDPOINT$ s" app" s" s3cr&t+1" BEARER:CLIENT!
   [: OWN-TOKEN drop 2drop ;] BEARER:E-TRANSFER TTHROWSQ ;

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
   [: [: OWN-TOKEN drop 2drop ;] BEARER:E-TRANSFER TTHROWSQ ;] CAPTURED
   ENDPOINT$ s" app" s" s3cr&t+1" BEARER:CLIENT!   \ CLIENT! borrows the URL
   url MEM:FREE-SPAN
   LINES 1 T=
   LINE$ s"  error bearer: token request failed: curl 43: " STARTS-WITH? TTRUE ;


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
   LINE LINE-U @ s" issuer " STARTS-WITH? TTRUE
   LINE-U @ 8 - {: u:n :}
   u 0 > if LINE 7 + u exit then
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
   CHILD-URL {: url:^ uu:n :}
   SB-RESET url uu SB-APPEND s" /.well-known/openid-configuration" SB-APPEND
   SB$ OPEN-AT FINISH 200 T=
   BODY$ s" issuer" MEMBER$ url uu T$=
   in-w FD>N close
   pid PROCESS:WAIT PROCESS:STATUS>RC RC>N 0 T=
   out-r FD>N close ;


\ BEARER:ISSUER! names this server's URL and is refused while HTTP serves, so
\ the server starts once to take a port and serves again on it.
: SERVE-CASES ( -- )
   AIO:START LOG-INIT
   s" app" s" s3cr&t+1" TEST-ISSUER:CLIENT+
   s" cli" s" " TEST-ISSUER:CLIENT+
   HTTP:ROUTES-RESET
   TEST-ISSUER:ROUTES
   s" POST" s" /claim" [: CLAIM-ROUTE ;] HTTP:ROUTE
   s" POST" s" /token-fault" [: FAULT-ROUTE ;] HTTP:ROUTE
   s" POST" s" /token-count" [: COUNT-ROUTE ;] HTTP:ROUTE
   LOOPBACK 0 TWO-WORKERS IDLE-MS HTTP:START
   ADDRESSES
   HTTP:STOP
   ISSUER$ s" api://up" KEYS-URL$ BEARER:ISSUER!
   LOOPBACK PORT-KEPT @ TWO-WORKERS IDLE-MS HTTP:START
   DISCOVERY KEYS CLIENT-CREDENTIALS CLIENTS REQUESTS OBO MINTS
   BEARER-GRANTS BEARER-CURRENT BEARER-FAULTS
   HTTP:STOP
   BEARER-UNREACHED BEARER-LONG
   TOOL
   AIO:STOP ;

T-RESET
SERVE-CASES
T-REPORT

;package
