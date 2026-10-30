\ bearer-test.f - BEARER:ADMIT? over a real loopback port: ADMIT? before
\ ISSUER! and ISSUER! while serving, the 503 of a key set that cannot be
\ fetched, key selection among an EC key, an encryption key and the token's,
\ the 401 challenges, malformed tokens, alg none, crit and HS256, repeated
\ member names, aud, iss, exp and nbf, claim strings, CLAIM$ after the admitted
\ request and in a task it spawns, the refetch window, and the cached and
\ unknown kids answered while a fetch is in flight.
\ Run: bin/hb --load src/lib/net/bearer-test.f

require src/lib/test.f
require src/lib/test/wait.f
require src/lib/string.f
require src/lib/span.f
require src/lib/fmt.f
require src/lib/aio.f
require src/lib/task.f
require src/lib/base64.f
require src/lib/json-write.f
require src/lib/crypto/evp.f
require src/lib/crypto/jws-fixture.f
require src/lib/net/curl.f
require src/lib/net/http.f
require src/lib/net/bearer.f

package BEARER-TEST
private

$7F000001 constant LOOPBACK
3 constant THREE-WORKERS
500 constant IDLE-MS
1000000 constant NS-PER-MS
3000 constant WAIT-MS               \ the bound on every wait, past REFETCH-MS
1000 constant REFETCH-MS            \ BEARER's window between fetches
2000 constant STALL-MS              \ the bound on a stalled /keys, under WAIT-MS
600 constant LIFE-S                 \ how far past now a current token's exp lies
28 constant CURLE-TIMED-OUT         \ CURLE_OPERATION_TIMEDOUT

$1000 constant CAP
CAP BUFFER: BODY
variable BODY-U
CAP BUFFER: HEADS
variable HEADS-U

$1000 constant TOKEN-CAP
TOKEN-CAP SPAN-BUFFER: TOKEN
variable TOKEN-U
CRYPTO:RS256-MAX-BYTES BUFFER: SIGNATURE
32 constant HS256-BYTES
TOKEN-CAP $40 + BUFFER: AUTH
variable AUTH-U

\ The two key sets /keys answers: SET-ONE at 0, SET-TWO at SET-CAP.
$800 constant SET-CAP
SET-CAP 2 * SPAN-BUFFER: SETS
variable ONE-U
variable TWO-U

$40 constant URL-CAP
URL-CAP BUFFER: KEYS-URL
variable KEYS-URL-U
variable PORT

\ /keys answers 500 (0), SET-ONE (1), SET-TWO (2), or SET-TWO once stalled (3).
variable MODE
variable HITS                       \ /keys requests served
variable EARLY-CODE                 \ what ADMIT? threw before ISSUER!
variable FETCHED-BY                 \ a mono-ns after the last fetch returned
variable DUE                        \ when that fetch's window has passed, a mono-ns
variable RELEASED                   \ the test lets the stalled /keys answer
variable STALL-END                  \ RELEASED as the stalled /keys saw it on answering
TYPED-VARIABLE PENDING CURL:handle  \ the request ASKER sends
TASK:MIN-STACK TASK:TASK ASKER
TASK:MIN-STACK TASK:TASK CLAIMER    \ asks CLAIM$ for the admitted /spawn

\ The end of a wait's WAIT-MS window, an absolute mono-ns.
: DEADLINE ( -- n ) mono-ns WAIT-MS NS-PER-MS * + ;

\ The bytes copied into the span at off; answers the offset past them.
: PUT ( ptr u8 n SPAN:span<u8> n -- n )
   {: a:ptr u:n row off:n :}
   a row off u SPAN:SUB SPAN:$ drop u BYTE-COPY
   off u + ;


\ The key sets.

: SET+ ( ptr u8 n n -- n ) SETS swap PUT ;

: RSA-KEY+ ( ptr u8 n n -- n )
   {: kid:ptr ku:n off:n :}
   S\" {\"kty\":\"RSA\",\"kid\":\"" off SET+
   kid ku rot SET+
   S\" \",\"use\":\"sig\",\"alg\":\"RS256\",\"n\":\"" rot SET+
   JWS-FIXTURE:MODULUS$ rot SET+
   S\" \",\"e\":\"" rot SET+
   JWS-FIXTURE:EXPONENT$ rot SET+
   S\" \"}" rot SET+ ;

\ An EC key and an encryption key named a, whose n would not verify, before
\ the signing key a.
: SET-ONE-FILL ( -- )
   S\" {\"keys\":[{\"kty\":\"EC\",\"kid\":\"a\",\"crv\":\"P-256\",\"x\":\"AA\",\"y\":\"AA\"}," 0 SET+
   S\" {\"kty\":\"RSA\",\"kid\":\"a\",\"use\":\"enc\",\"n\":\"AQAB\",\"e\":\"AQAB\"}," rot SET+
   s" a" rot RSA-KEY+
   s" ]}" rot SET+ ONE-U ! ;

: SET-TWO-FILL ( -- )
   S\" {\"keys\":[" SET-CAP SET+
   s" a" rot RSA-KEY+
   s" ," rot SET+
   s" b" rot RSA-KEY+
   s" ]}" rot SET+ SET-CAP - TWO-U ! ;

: SET-ONE$ ( -- ptr u8 n ) SETS 0 ONE-U @ SPAN:SUB SPAN:$ ;
: SET-TWO$ ( -- ptr u8 n ) SETS SET-CAP TWO-U @ SPAN:SUB SPAN:$ ;


\ The routes.

: CLAIM ( ptr JSON-WRITE:writer ptr u8 n -- ptr JSON-WRITE:writer )
   {: name:ptr nu:n :}
   name nu BEARER:CLAIM$ {: v:ptr vu:n has:bool :}
   has if name nu v vu JSON-WRITE:FIELD-S exit then
   name nu JSON-WRITE:FIELD-NULL ;

: CLAIMS-BODY ( ptr JSON-WRITE:writer -- ptr JSON-WRITE:writer )
   JSON-WRITE:OBJECT-START
   s" azp" CLAIM JSON-WRITE:COMMA
   s" oid" CLAIM JSON-WRITE:COMMA
   s" tid" CLAIM
   JSON-WRITE:OBJECT-END ;

: AZP-BODY ( ptr JSON-WRITE:writer -- ptr JSON-WRITE:writer )
   JSON-WRITE:OBJECT-START s" azp" CLAIM JSON-WRITE:OBJECT-END ;

: GUARDED-ROUTE ( HTTP:request HTTP:response -- )
   {: asked:HTTP:request answer:HTTP:response :}
   asked answer BEARER:ADMIT? if answer [: CLAIMS-BODY ;] HTTP:JSON! then ;

: PLAIN-ROUTE ( HTTP:request HTTP:response -- )
   nip [: AZP-BODY ;] HTTP:JSON! ;

: CLAIM-FLAG ( -- ) s" azp" BEARER:CLAIM$ nip nip bool>n TASK:RETURN ;

\ Answers CLAIMER's flag; a throw in CLAIMER answers 500.
: SPAWN-ROUTE ( HTTP:request HTTP:response -- )
   {: asked:HTTP:request answer:HTTP:response :}
   asked answer BEARER:ADMIT? 0= if exit then
   ['] CLAIM-FLAG CLAIMER TASK:ACTIVATE
   CLAIMER TASK:JOIN MATCH result
      ok OF 0<> if answer s" true" HTTP:BODY! else answer s" false" HTTP:BODY! then ENDOF
      err OF throw ENDOF
   ;MATCH ;

\ Holds /keys until the test sets RELEASED or STALL-MS has passed.
: STALL ( -- )
   mono-ns STALL-MS NS-PER-MS * +
   [: RELEASED atomic@ 0<> over mono-ns <= or ;] DEADLINE s" bearer-test STALL"
   TEST-WAIT:POLL drop
   RELEASED atomic@ STALL-END atomic! ;

: KEYS-ROUTE ( HTTP:request HTTP:response -- )
   nip {: answer:HTTP:response :}
   1 HITS atomic-add drop
   MODE atomic@ 0= if answer 500 HTTP:STATUS! exit then
   MODE atomic@ 1 = if answer SET-ONE$ HTTP:BODY! exit then
   MODE atomic@ 3 = if STALL then
   answer SET-TWO$ HTTP:BODY! ;

: EARLY-ROUTE ( HTTP:request HTTP:response -- )
   [: 2dup BEARER:ADMIT? drop ;] catch
   {: asked:HTTP:request answer:HTTP:response code:n :}
   code EARLY-CODE atomic! ;


\ The client.

\ A setter's refusal throws: an assertion here would take the case's T-LABEL.
: OK ( CURL:status -- )
   MATCH CURL:status
      ok OF ENDOF
      failed OF drop CURL:E-STATE throw ENDOF
   ;MATCH ;

: URL ( ptr u8 n -- ptr u8 n )
   {: path:ptr pu:n :}
   SB-RESET s" http://127.0.0.1:" SB-APPEND HTTP:PORT FMT:SB-U path pu SB-APPEND SB$ ;

: OPEN ( ptr u8 n -- CURL:handle )
   {: path:ptr pu:n :}
   CURL:INIT
   MATCH CURL:init-result
      ready OF ENDOF
      failed OF drop -1 CURL:>HANDLE ENDOF
   ;MATCH {: h:CURL:handle :}
   h path pu URL CURL:URL! OK
   h WAIT-MS >MS CURL:TIMEOUT! OK h ;

: ADD ( CURL:handle ptr u8 n -- CURL:handle )
   {: h:CURL:handle a:ptr u:n :}
   h a u CURL:HEADER+ OK h ;

\ The Authorization carrying the minted token.
: BEARING ( CURL:handle -- CURL:handle ) AUTH AUTH-U @ ADD ;

: HEADS-LEN ( CURL:header-result -- n )
   MATCH CURL:header-result
      complete OF LEN>N ENDOF
      truncated OF drop -1 ENDOF
   ;MATCH ;

\ The response's status, its body in BODY and its header block in HEADS; -1
\ for a body past CAP, -2 for a failed transfer. CURLE 28 is libcurl's
\ timeout, met by a starved or stopped process: E-PROC-TIMEOUT, not a FAIL.
: PERFORM ( CURL:handle -- n )
   {: h:CURL:handle :}
   0 BODY-U ! 0 HEADS-U !
   h BODY CAP >LEN CURL:PERFORM
   MATCH CURL:fetch-result
      response OF {: status:CURL:http-status got:len :}
         got LEN>N BODY-U ! status CURL:HTTP-STATUS>N ENDOF
      truncated OF drop drop -1 ENDOF
      failed OF {: rc:CURL:code :}
         rc CURL:CODE>N CURLE-TIMED-OUT = if
            s" bearer-test PERFORM" TEST-WAIT:EXPIRED
         then
         -2 ENDOF
   ;MATCH {: status:n :}
   status 0 > if h HEADS CAP >LEN CURL:HEADERS HEADS-LEN HEADS-U ! then
   status ;

: FINISH ( CURL:handle -- n )
   {: h:CURL:handle :}
   h PERFORM h CURL:CLEANUP ;

\ The guarded route asked with the minted token.
: SEND ( -- n ) s" /claims" OPEN BEARING FINISH ;

\ An empty 401 challenging with the value.
: REFUSED ( n ptr u8 n -- )
   {: status:n want:ptr wu:n :}
   SB-RESET s" WWW-Authenticate: " SB-APPEND want wu SB-APPEND 13 SB-APPEND-C 10 SB-APPEND-C
   HEADS HEADS-U @ SB$ CONTAINS? TTRUE
   BODY-U @ 0 T=
   status 401 T= ;

: CHALLENGED ( n -- ) s" Bearer" REFUSED ;
: INVALID ( n -- ) S\" Bearer error=\"invalid_token\"" REFUSED ;

: ANSWERED ( n ptr u8 n -- )
   {: status:n want:ptr wu:n :}
   BODY BODY-U @ want wu T$=
   status 200 T= ;


\ Tokens.

: TOKEN+ ( ptr u8 n -- ) TOKEN TOKEN-U @ PUT TOKEN-U ! ;
: DOT+ ( -- ) s" ." TOKEN+ ;
: TOKEN$ ( -- ptr u8 n ) TOKEN 0 TOKEN-U @ SPAN:SUB SPAN:$ ;

: ENCODED+ ( ptr u8 n -- )
   TOKEN TOKEN-U @ TOKEN-CAP TOKEN-U @ - SPAN:SUB BASE64:ENCODE-URL
   TOKEN-U @ + TOKEN-U ! ;

: AUTHORIZE ( -- )
   s" Authorization: Bearer " {: p:ptr pu:n :}
   p AUTH pu BYTE-COPY
   TOKEN$ {: t:ptr tu:n :}
   t AUTH pu + tu BYTE-COPY
   pu tu + AUTH-U ! ;

\ The header's and claims' segments and the dot after them.
: SIGNED ( ptr u8 n ptr u8 n -- )
   {: h:ptr hu:n c:ptr cu:n :}
   0 TOKEN-U !
   h hu ENCODED+ DOT+ c cu ENCODED+ ;

: MINT ( ptr u8 n ptr u8 n -- )
   SIGNED
   JWS-FIXTURE:PEM$ TOKEN$ SIGNATURE CRYPTO:RS256-MAX-BYTES CRYPTO:RS256-SIGN
   {: su:n :}
   DOT+ SIGNATURE su ENCODED+ AUTHORIZE ;

\ Validly signed HS256, keyed with the issuer's public modulus.
: HS-MINT ( ptr u8 n ptr u8 n -- )
   SIGNED
   JWS-FIXTURE:MODULUS$ TOKEN$ SIGNATURE HS256-BYTES CRYPTO:HMAC-SHA256
   DOT+ SIGNATURE HS256-BYTES ENCODED+ AUTHORIZE ;

: UNSIGNED ( ptr u8 n ptr u8 n -- ) SIGNED DOT+ AUTHORIZE ;

: RAW-TOKEN ( ptr u8 n -- ) 0 TOKEN-U ! TOKEN+ AUTHORIZE ;

\ One signature byte changed, away from the last character's padding bits.
: FLIP ( -- )
   TOKEN$ + 10 - {: at :}
   at c@ [char] A = if [char] B else [char] A then at c!
   AUTHORIZE ;

: CLAIMS ( ptr u8 n n ptr u8 n -- ptr u8 n )
   {: pre:ptr pu:n exp:n post:ptr postu:n :}
   SB-RESET pre pu SB-APPEND exp FMT:SB-INT post postu SB-APPEND SB$ ;

: ISS-AUD ( -- ptr u8 n )
   S\" {\"iss\":\"https://issuer.test\",\"aud\":\"api://bearer-test\",\"exp\":" ;
: WHO ( -- ptr u8 n ) S\" ,\"azp\":\"app-1\",\"oid\":\"o-1\",\"tid\":\"t-1\"}" ;
: WANT-WHO ( -- ptr u8 n ) S\" {\"azp\":\"app-1\",\"oid\":\"o-1\",\"tid\":\"t-1\"}" ;
: LATER ( -- n ) epoch-seconds LIFE-S + ;
: CURRENT ( -- ptr u8 n ) ISS-AUD LATER WHO CLAIMS ;
: KID-A ( -- ptr u8 n ) S\" {\"alg\":\"RS256\",\"kid\":\"a\"}" ;
: KID-B ( -- ptr u8 n ) S\" {\"alg\":\"RS256\",\"kid\":\"b\"}" ;
: FETCHED ( -- ) mono-ns FETCHED-BY ! ;


\ The cases.

: WAIT-WINDOW ( -- )
   FETCHED-BY @ REFETCH-MS NS-PER-MS * + DUE !
   [: DUE @ mono-ns <= ;] DEADLINE s" bearer-test WINDOW" TEST-WAIT:POLL ;

: UNAVAILABLE ( -- )
   s" a key set that cannot be fetched answers an empty 503" T-LABEL
   0 MODE atomic!
   KID-A CURRENT MINT SEND {: status:n :}
   FETCHED
   BODY-U @ 0 T=
   status 503 T=
   HITS atomic@ 1 T=
   s" an empty set inside the failed fetch's window answers 503 and fetches nothing" T-LABEL
   SEND 503 T=
   HITS atomic@ 1 T= ;

: ADMITTED ( -- )
   s" the signing key among an EC and an encryption key admits a current token" T-LABEL
   1 MODE atomic!
   WAIT-WINDOW
   KID-A CURRENT MINT SEND WANT-WHO ANSWERED FETCHED
   HITS atomic@ 2 T= ;

: UNKNOWN-KID ( -- )
   s" an unknown kid within the window is invalid and fetches nothing" T-LABEL
   2 MODE atomic!
   KID-B CURRENT MINT SEND INVALID
   HITS atomic@ 2 T= ;

: NO-TOKEN ( -- )
   s" no Authorization challenges Bearer" T-LABEL
   s" /claims" OPEN FINISH CHALLENGED
   s" two Authorization headers challenge Bearer" T-LABEL
   KID-A CURRENT MINT
   s" /claims" OPEN BEARING BEARING FINISH CHALLENGED
   s" a Basic Authorization challenges Bearer" T-LABEL
   s" /claims" OPEN s" Authorization: Basic eDp5" ADD FINISH CHALLENGED ;

: MALFORMED ( -- )
   s" a token of one segment is invalid" T-LABEL
   s" e30" RAW-TOKEN SEND INVALID
   s" a token of two segments is invalid" T-LABEL
   s" e30.e30" RAW-TOKEN SEND INVALID
   s" a token of four segments is invalid" T-LABEL
   KID-A CURRENT MINT s" .e30" TOKEN+ AUTHORIZE SEND INVALID
   s" a segment that is not base64url is invalid" T-LABEL
   s" e3!.e30.e30" RAW-TOKEN SEND INVALID
   s" a header that is not an object is invalid" T-LABEL
   s" [1]" CURRENT MINT SEND INVALID
   s" claims that are not an object are invalid" T-LABEL
   KID-A s" []" MINT SEND INVALID
   s" claims that are not JSON are invalid" T-LABEL
   KID-A S\" {\"iss\"" MINT SEND INVALID ;

: SIGNATURES ( -- )
   s" a changed signature byte is invalid" T-LABEL
   KID-A CURRENT MINT FLIP SEND INVALID
   s" alg none is invalid" T-LABEL
   S\" {\"alg\":\"none\",\"kid\":\"a\"}" CURRENT UNSIGNED SEND INVALID
   s" alg none over a valid RS256 signature is invalid" T-LABEL
   S\" {\"alg\":\"none\",\"kid\":\"a\"}" CURRENT MINT SEND INVALID
   s" a header with crit is invalid" T-LABEL
   S\" {\"alg\":\"RS256\",\"kid\":\"a\",\"crit\":[\"urn:x\"],\"urn:x\":1}" CURRENT MINT
   SEND INVALID
   s" HS256 keyed with the public modulus is invalid" T-LABEL
   S\" {\"alg\":\"HS256\",\"kid\":\"a\"}" CURRENT HS-MINT SEND INVALID
   s" a header repeating kid is invalid" T-LABEL
   S\" {\"alg\":\"RS256\",\"kid\":\"a\",\"kid\":\"b\"}" CURRENT MINT SEND INVALID ;

: AUDIENCES ( -- )
   s" an aud array holding the audience is admitted" T-LABEL
   KID-A
   S\" {\"iss\":\"https://issuer.test\",\"aud\":[\"other\",\"api://bearer-test\"],\"exp\":"
   LATER WHO CLAIMS MINT SEND WANT-WHO ANSWERED
   s" an aud array without the audience is invalid" T-LABEL
   KID-A S\" {\"iss\":\"https://issuer.test\",\"aud\":[\"other\"],\"exp\":"
   LATER WHO CLAIMS MINT SEND INVALID
   s" another aud is invalid" T-LABEL
   KID-A S\" {\"iss\":\"https://issuer.test\",\"aud\":\"other\",\"exp\":"
   LATER WHO CLAIMS MINT SEND INVALID
   s" another iss is invalid" T-LABEL
   KID-A S\" {\"iss\":\"https://other.test\",\"aud\":\"api://bearer-test\",\"exp\":"
   LATER WHO CLAIMS MINT SEND INVALID
   s" claims repeating iss are invalid" T-LABEL
   KID-A
   S\" {\"iss\":\"https://issuer.test\",\"iss\":\"https://other.test\",\"aud\":\"api://bearer-test\",\"exp\":"
   LATER WHO CLAIMS MINT SEND INVALID ;

\ exp a second inside the leeway, sent within one epoch second: the server's
\ now is the one the token was minted at.
: EDGE-TRY ( n -- n bool )
   drop epoch-seconds {: was:n :}
   KID-A ISS-AUD was 59 - WHO CLAIMS MINT SEND
   epoch-seconds was = ;

: TIMES ( -- )
   s" exp at the leeway's edge is invalid" T-LABEL
   KID-A ISS-AUD epoch-seconds 60 - WHO CLAIMS MINT SEND INVALID
   s" exp a second inside the leeway is admitted" T-LABEL
   0 [: EDGE-TRY ;] DEADLINE s" bearer-test EDGE" TEST-WAIT:POLL WANT-WHO ANSWERED
   s" nbf beyond the leeway is invalid" T-LABEL
   SB-RESET ISS-AUD SB-APPEND LATER FMT:SB-INT
   S\" ,\"nbf\":" SB-APPEND epoch-seconds 120 + FMT:SB-INT WHO SB-APPEND
   KID-A SB$ MINT SEND INVALID
   s" exp as a string is invalid" T-LABEL
   SB-RESET
   S\" {\"iss\":\"https://issuer.test\",\"aud\":\"api://bearer-test\",\"exp\":\"" SB-APPEND
   LATER FMT:SB-INT S\" \"" SB-APPEND WHO SB-APPEND
   KID-A SB$ MINT SEND INVALID ;

: STRINGS ( -- )
   s" an escaped claim string is answered decoded" T-LABEL
   KID-A ISS-AUD LATER
   S\" ,\"azp\":\"app-1\\\"x\",\"oid\":\"o-1\",\"tid\":\"t-1\"}" CLAIMS MINT SEND
   S\" {\"azp\":\"app-1\\\"x\",\"oid\":\"o-1\",\"tid\":\"t-1\"}" ANSWERED
   s" absent and non-string claims answer false" T-LABEL
   KID-A ISS-AUD LATER S\" ,\"azp\":\"app-1\",\"tid\":7}" CLAIMS MINT SEND
   S\" {\"azp\":\"app-1\",\"oid\":null,\"tid\":null}" ANSWERED ;

\ The unguarded route on the admitted request's kept-alive connection, which
\ the same worker serves.
: STALE-CLAIM ( -- )
   s" CLAIM$ after the admitted request answers false" T-LABEL
   KID-A CURRENT MINT
   s" /claims" OPEN BEARING {: h:CURL:handle :}
   h PERFORM WANT-WHO ANSWERED
   h s" /plain" URL CURL:URL! OK
   h PERFORM S\" {\"azp\":null}" ANSWERED
   h CURL:CLEANUP ;

: SPAWNED-CLAIM ( -- )
   s" CLAIM$ in a task the admitted request spawns answers false" T-LABEL
   KID-A CURRENT MINT s" /spawn" OPEN BEARING FINISH s" false" ANSWERED ;

: ASK ( -- ) PENDING @ FINISH TASK:RETURN ;

\ Past the window ASKER's kid b fetches and /keys stalls; meanwhile the cached
\ kid a answers and kid b fails without a second fetch. Set two then admits b.
: WINDOW ( -- )
   s" a fetch in flight does not delay a cached kid" T-LABEL
   WAIT-WINDOW
   3 MODE atomic!
   KID-B CURRENT MINT s" /claims" OPEN BEARING PENDING !
   ['] ASK ASKER TASK:ACTIVATE
   [: HITS atomic@ 3 = ;] DEADLINE s" bearer-test HITS" TEST-WAIT:POLL
   KID-A CURRENT MINT SEND WANT-WHO ANSWERED
   s" an unknown kid while a fetch is in flight is invalid and fetches nothing" T-LABEL
   2 MODE atomic!                      \ a second fetch would answer set two at once
   KID-B CURRENT MINT SEND INVALID
   HITS atomic@ 3 T=
   1 RELEASED atomic!
   s" past the window an unknown kid refetches and the new set admits it" T-LABEL
   ASKER TASK:JOIN MATCH result
      ok OF WANT-WHO ANSWERED ENDOF
      err OF throw ENDOF
   ;MATCH
   s" the stalled fetch was released, not expired" T-LABEL
   STALL-END atomic@ 1 T= ;


: ROUTES ( -- )
   HTTP:ROUTES-RESET
   s" GET" s" /early" [: EARLY-ROUTE ;] HTTP:ROUTE
   s" GET" s" /claims" [: GUARDED-ROUTE ;] HTTP:ROUTE
   s" GET" s" /plain" [: PLAIN-ROUTE ;] HTTP:ROUTE
   s" GET" s" /spawn" [: SPAWN-ROUTE ;] HTTP:ROUTE
   s" GET" s" /keys" [: KEYS-ROUTE ;] HTTP:ROUTE ;

\ Three workers: ADMIT? fetches /keys from this same server and waits for the
\ answer, and WINDOW holds that fetcher and the stalled /keys while a third
\ answers the cached kid.
: EARLY ( -- )
   LOOPBACK 0 THREE-WORKERS IDLE-MS HTTP:START
   s" ADMIT? before ISSUER! throws E-HTTP-STATE" T-LABEL
   s" /early" OPEN FINISH drop
   EARLY-CODE atomic@ E-HTTP-STATE T=
   s" ISSUER! while HTTP serves throws E-HTTP-STATE" T-LABEL
   [: s" a" s" b" s" c" BEARER:ISSUER! ;] E-HTTP-STATE TTHROWSQ
   HTTP:PORT PORT !
   s" /keys" URL {: a:ptr u:n :}
   a KEYS-URL u BYTE-COPY u KEYS-URL-U !
   HTTP:STOP ;

\ The key set's URL names the port the first START's port 0 chose, and
\ ISSUER! is refused while serving: the server starts again on that port.
: SERVE-CASES ( -- )
   AIO:START
   SET-ONE-FILL SET-TWO-FILL
   ROUTES
   EARLY
   s" https://issuer.test" s" api://bearer-test" KEYS-URL KEYS-URL-U @ BEARER:ISSUER!
   LOOPBACK PORT @ THREE-WORKERS IDLE-MS HTTP:START
   UNAVAILABLE ADMITTED UNKNOWN-KID NO-TOKEN MALFORMED SIGNATURES AUDIENCES
   TIMES STRINGS STALE-CLAIM SPAWNED-CLAIM WINDOW
   HTTP:STOP
   AIO:STOP ;

T-RESET
SERVE-CASES
T-REPORT

;package
