\ test-issuer-test.f - TEST-ISSUER over a real loopback port: discovery, the key
\ set, client credentials verified against the served key, invalid_client,
\ unsupported_grant_type, invalid_request for a repeated field and a wrong
\ media type, On-Behalf-Of with a minted assertion and its invalid_grant
\ refusals, /mint against MINT, and tools/test-issuer.f as a child process.
\ Run: bin/hb --load src/lib/net/test-issuer-test.f

require src/lib/test.f
require src/lib/test/wait.f
require src/lib/string.f
require src/lib/span.f
require src/lib/fmt.f
require src/lib/aio.f
require src/lib/base64.f
require src/lib/byte-buffer.f
require src/lib/json-read.f
require src/lib/process.f
require src/lib/spawn.f
require src/lib/engine-candidate.f
require src/lib/crypto/evp.f
require src/lib/crypto/jws-fixture.f
require src/lib/net/curl.f
require src/lib/net/http.f
require src/lib/net/test-issuer.f

package TEST-ISSUER-TEST
private

$7F000001 constant LOOPBACK
2 constant TWO-WORKERS
500 constant IDLE-MS
1000000 constant NS-PER-MS
15000 constant WAIT-MS              \ the bound on every wait, a child engine's start included
600 constant LIFE-S                 \ how far past now an assertion's exp lies
3600 constant ISSUED-S              \ an issued token's lifetime
28 constant CURLE-TIMED-OUT         \ CURLE_OPERATION_TIMEDOUT
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

TYPED-VARIABLE READER JR:cursor
create MINTED-HEAD BUF:HDR-BYTES allot
: MINTED ( -- ptr n ) MINTED-HEAD CELL-VIEW ;
MINTED $100 BUF:N>BLEN BUF:INIT

\ The end of a wait's WAIT-MS window, an absolute mono-ns.
: DEADLINE ( -- n ) mono-ns WAIT-MS NS-PER-MS * + ;

: BODY$ ( -- ptr u8 n ) BODY BODY-U @ ;
: TOKEN$ ( -- ptr u8 n ) TOKEN TOKEN-U @ ;
: CLAIMS$ ( -- ptr u8 n ) CLAIMS CLAIMS-U @ ;
: ASSERTION$ ( -- ptr u8 n ) ASSERTION ASSERTION-U @ ;

\ The text copied into a row, answering its length.
: COPY ( ptr u8 n ptr u8 -- n )
   {: a:ptr u:n row:ptr :}
   a row u BYTE-COPY u ;


\ JSON members.

\ A string member of an object text as sent, empty when absent.
: MEMBER$ ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: t:ptr tu:n k:ptr ku:n :}
   READER t tu JR:INIT JR:NEXT drop
   k ku JR:FIND-KEY 0= if JR:CLOSE s" " exit then
   JR:TOKEN JR:T-STR <> if JR:CLOSE s" " exit then
   JR:SPAN$ {: a:ptr u:n :} JR:CLOSE a u ;

\ An integer member of an object text, -1 when absent.
: MEMBER-INT ( ptr u8 n ptr u8 n -- n )
   {: t:ptr tu:n k:ptr ku:n :}
   READER t tu JR:INIT JR:NEXT drop
   k ku JR:FIND-KEY 0= if JR:CLOSE -1 exit then
   JR:TOKEN JR:T-INT <> if JR:CLOSE -1 exit then
   JR:INT {: v:n :} JR:CLOSE v ;

\ A string member of the body's first key, as sent.
: KEY$ ( ptr u8 n -- ptr u8 n )
   {: k:ptr ku:n :}
   READER BODY$ JR:INIT JR:NEXT drop
   s" keys" JR:FIND-KEY drop JR:NEXT drop
   k ku JR:FIND-KEY drop JR:SPAN$ {: a:ptr u:n :} JR:CLOSE a u ;


\ The client.

: OK ( CURL:status -- )
   MATCH CURL:status
      ok OF true ENDOF
      failed OF drop false ENDOF
   ;MATCH TTRUE ;

: URL ( ptr u8 n -- ptr u8 n )
   {: path:ptr pu:n :}
   SB-RESET s" http://127.0.0.1:" SB-APPEND HTTP:PORT FMT:SB-U path pu SB-APPEND SB$ ;

: OPEN-AT ( ptr u8 n -- CURL:handle )
   {: url:ptr uu:n :}
   CURL:INIT
   MATCH CURL:init-result
      ready OF ENDOF
      failed OF drop -1 CURL:>HANDLE ENDOF
   ;MATCH {: h:CURL:handle :}
   h url uu CURL:URL! OK
   h WAIT-MS >MS CURL:TIMEOUT! OK h ;

: OPEN ( ptr u8 n -- CURL:handle ) URL OPEN-AT ;

\ The response's status and its body in BODY; -1 for a body past CAP, -2 for
\ a failed transfer. CURLE 28 is libcurl's timeout, OPEN-AT's WAIT-MS on every
\ request, met by a starved or stopped process: E-PROC-TIMEOUT, not a FAIL.
: FINISH ( CURL:handle -- n )
   {: h:CURL:handle :}
   0 BODY-U !
   h BODY CAP >LEN CURL:PERFORM
   MATCH CURL:fetch-result
      response OF {: status:CURL:http-status got:len :}
         got LEN>N BODY-U ! status CURL:HTTP-STATUS>N ENDOF
      truncated OF drop drop -1 ENDOF
      failed OF {: rc:CURL:code :}
         rc CURL:CODE>N CURLE-TIMED-OUT = if
            s" test-issuer-test FINISH" TEST-WAIT:EXPIRED
         then
         -2 ENDOF
   ;MATCH
   h CURL:CLEANUP ;

: GET ( ptr u8 n -- n ) OPEN FINISH ;

\ The body posted with the media type; the body is copied by libcurl.
: POST ( ptr u8 n ptr u8 n ptr u8 n -- n )
   {: path:ptr pu:n type:ptr tu:n b:ptr bu:n :}
   path pu OPEN {: h:CURL:handle :}
   h type tu CURL:HEADER+ OK
   h b bu CURL:BODY! OK
   h FINISH ;

: FORM-TYPE ( -- ptr u8 n ) s" Content-Type: application/x-www-form-urlencoded" ;

: FORM-RESET ( -- ) 0 FORM-U ! ;
: FORM+ ( ptr u8 n -- ) FORM FORM-U @ + COPY FORM-U +! ;
: FORM$ ( -- ptr u8 n ) FORM FORM-U @ ;
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
: REFUSED ( n n ptr u8 n -- )
   {: status:n want:n code:ptr cu:n :}
   SB-RESET S\" {\"error\":\"" SB-APPEND code cu SB-APPEND S\" \"}" SB-APPEND
   BODY$ SB$ T$=
   status want T= ;


\ Tokens.

\ The index of the first dot at or after from, else the text's length.
: DOT ( ptr u8 n n -- n )
   {: t:ptr u:n from:n :}
   u from ?do t i + c@ [char] . = if i unloop exit then loop u ;

: DECODED ( ptr u8 n ptr u8 n -- n ) SPAN:MAKE BASE64:DECODE-URL ;

\ The issued token copied from the answer; its header, claims and signature
\ decoded.
: ISSUED ( -- )
   BODY$ s" access_token" MEMBER$ TOKEN COPY TOKEN-U !
   TOKEN$ {: t:ptr u:n :}
   t u 0 DOT {: d1:n :}
   t u d1 1+ DOT {: d2:n :}
   t d1 HEAD CAP DECODED HEAD-U !
   t d1 + 1+ d2 d1 - 1- CLAIMS CAP DECODED CLAIMS-U !
   t d2 + 1+ u d2 - 1- SIGNATURE CRYPTO:RS256-MAX-BYTES DECODED SIGNATURE-U ! ;

\ The issued token verifies against the key /keys served.
: VERIFIES ( -- )
   TOKEN$ {: t:ptr u:n :}
   t u 0 DOT {: d1:n :}
   t u d1 1+ DOT {: d2:n :}
   MODULUS MODULUS-U @ EXPONENT EXPONENT-U @ t d2 SIGNATURE SIGNATURE-U @
   CRYPTO:RS256-VERIFY? TTRUE
   HEAD HEAD-U @ s" kid" MEMBER$ KID KID-U @ T$=
   HEAD HEAD-U @ s" alg" MEMBER$ s" RS256" T$=
   HEAD HEAD-U @ s" typ" MEMBER$ s" JWT" T$= ;

\ The answer is a 200 bearer token; the claims every issued token carries.
: ANSWERED ( n ptr u8 n -- )
   {: status:n aud:ptr au:n :}
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

\ An assertion for the client, minted by MINT into ASSERTION.
: ASSERT ( ptr u8 n n -- )
   {: aud:ptr au:n exp:n :}
   SB-RESET S\" {\"aud\":\"" SB-APPEND aud au SB-APPEND S\" \",\"exp\":" SB-APPEND
   exp FMT:SB-INT S\" ,\"oid\":\"user-1\",\"tid\":\"tenant-1\"}" SB-APPEND
   SB$ MINTED TEST-ISSUER:MINT ASSERTION COPY ASSERTION-U ! ;

: LATER ( -- n ) epoch-seconds LIFE-S + ;

\ One signature byte changed, away from the last character's padding bits.
: FLIP ( -- )
   ASSERTION ASSERTION-U @ + 10 - {: at:ptr :}
   at c@ [char] A = if [char] B else [char] A then at c! ;


\ The cases.

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
   s" app" epoch-seconds 10 - ASSERT ON-BEHALF ASK 400 s" invalid_grant" REFUSED ;

: MINTS ( -- )
   s" /mint answers MINT's token for the same claims" T-LABEL
   S\" {\"sub\":\"s-1\",\"n\":7}" {: c:ptr cu:n :}
   s" /mint" s" Content-Type: application/json" c cu POST 200 T=
   BODY$ c cu MINTED TEST-ISSUER:MINT T$= ;


\ The tool's child process.

\ The child's stdout read as far as it has written; true once a line ended or
\ the pipe closed.
: LINE? ( -- bool )
   CHILD-OUT @ LINE LINE-U @ + $100 LINE-U @ - read {: k:n :}
   k 0= if true exit then
   k 0 > if k LINE-U +! then
   LINE-U @ 0 > if LINE LINE-U @ + 1- c@ 10 = exit then
   false ;

\ The URL the issuer line names, empty when the child printed none.
: CHILD-URL ( -- ptr u8 n )
   LINE LINE-U @ s" issuer " STARTS-WITH? TTRUE
   LINE-U @ 8 - {: u:n :}
   u 0 > if LINE 7 + u exit then
   s" " ;

\ A process boundary is under test: the tool's command line, its issuer line,
\ and its exit when stdin ends.
: TOOL ( -- )
   s" the tool names its issuer, serves it and exits 0 when stdin ends" T-LABEL
   PIPE-PAIR {: in-r:fd in-w:fd :}
   PIPE-PAIR {: out-r:fd out-w:fd :}
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
   out-r PROC-NONBLOCK!
   out-r FD>N CHILD-OUT !
   0 LINE-U !
   [: LINE? ;] DEADLINE s" test-issuer-test TOOL" TEST-WAIT:POLL
   CHILD-URL {: url:ptr uu:n :}
   SB-RESET url uu SB-APPEND s" /.well-known/openid-configuration" SB-APPEND
   SB$ OPEN-AT FINISH 200 T=
   BODY$ s" issuer" MEMBER$ url uu T$=
   in-w FD>N close
   pid WAIT-MS >MS PROC-WAIT-BOUNDED PROC-OUTCOME>RC RC>N 0 T=
   out-r FD>N close ;


: SERVE-CASES ( -- )
   AIO:START
   s" app" s" s3cr&t+1" TEST-ISSUER:CLIENT+
   HTTP:ROUTES-RESET
   TEST-ISSUER:ROUTES
   LOOPBACK 0 TWO-WORKERS IDLE-MS HTTP:START
   DISCOVERY KEYS CLIENT-CREDENTIALS CLIENTS REQUESTS OBO MINTS
   HTTP:STOP
   TOOL
   AIO:STOP ;

T-RESET
SERVE-CASES
T-REPORT

;package
