\ bearer-test.f - BEARER:ADMIT? over a real loopback port: ADMIT? before
\ ISSUER! and ISSUER! or METADATA! while serving, the 503 of a key set that
\ cannot be fetched, key selection among an EC key, an encryption key and the
\ token's, the 401 challenges, plain and naming METADATA!'s URL, its scope or
\ both, a METADATA! URL holding a quote or CR LF or lacking the well-known
\ segment and a scope holding a quote or two spaces refused, ISSUER!, CLIENT!
\ and METADATA! refusing http but to a loopback IP literal, the metadata
\ route's body, malformed tokens, alg none, crit and HS256, repeated member
\ names, aud, iss, exp and nbf, claim strings, CLAIM-HOLDS? on string, scope,
\ number and absent claims and an array holding nested values and an escape,
\ CLAIM$ after the admitted request, CLAIM$ and CLAIM-HOLDS? in a task it
\ spawns, the refetch window, a cached kid answered while a fetch is in
\ flight, an unknown kid and an empty set waiting for that fetch, and a
\ fetched key set repeating a member refused while the cached one stays.
\ Run: bin/hb --load src/lib/net/bearer-test.f

require src/lib/test.f
require src/lib/test/wait.f
require src/lib/string.f
require src/lib/span.f
require src/lib/fmt.f
require src/lib/aio.f
require src/lib/task.f
require src/lib/heap.f
require src/lib/vector.f
require src/lib/base64.f
require src/lib/json.f
require src/lib/crypto/evp.f
require src/lib/crypto/jws-fixture.f
require src/lib/net/curl.f
require src/lib/net/http.f
require src/lib/net/bearer.f

\ White-box reads no public word answers, defined into BEARER-TEST so BEARER's
\ public surface gains nothing.
package BEARER
: BEARER-TEST:WAITERS ( -- n ) [: WAITERS @ ;] LOCKED ;
;package

package BEARER-TEST
private

$7F000001 constant LOOPBACK
\ ADMIT? fetches /keys from this same server: WINDOW and FRESH hold that
\ fetcher and the stalled /keys while a third worker serves another request.
3 constant THREE-WORKERS
500 constant IDLE-MS
1000000 constant NS-PER-MS
1000 constant REFETCH-MS            \ BEARER's window between fetches
600 constant LIFE-S                 \ how far past now a current token's exp lies

TYPED-VARIABLE CFG HTTP:config

: SERVE-ON ( n -- )
   {: port:n :}
   LOOPBACK CFG HTTP-CONFIG:ADDRESS !
   port CFG HTTP-CONFIG:PORT !
   THREE-WORKERS CFG HTTP-CONFIG:WORKERS !
   IDLE-MS >MS CFG HTTP-CONFIG:IDLE !
   CFG HTTP:START ;

$1000 constant CAP
TYPED-VARIABLE BODY ^VEC:vector<u8>
TYPED-VARIABLE HEADS ^VEC:vector<u8>
10 SPAN-BUFFER: HELD

$1000 constant TOKEN-CAP
TOKEN-CAP SPAN-BUFFER: TOKEN
variable TOKEN-U
TOKEN-CAP $40 + BUFFER: AUTH
variable AUTH-U

\ The three key sets /keys answers: SET-ONE at 0, SET-TWO at SET-CAP,
\ SET-THREE at twice SET-CAP.
$800 constant SET-CAP
SET-CAP 3 * SPAN-BUFFER: SETS
variable ONE-U
variable TWO-U
variable THREE-U

$40 constant URL-CAP
URL-CAP BUFFER: KEYS-URL
variable KEYS-URL-U
variable PORT

\ /keys answers 500 (0), SET-ONE (1), SET-TWO (2), SET-TWO once stalled (3),
\ or SET-THREE (4).
variable MODE
variable HITS                       \ /keys requests served
variable EARLY-CODE                 \ what ADMIT? threw before ISSUER!
variable FETCHED-BY                 \ a mono-ns after the last fetch returned
variable DUE                        \ when that fetch's window has passed, a mono-ns
variable RELEASED                   \ the test lets the stalled /keys answer
TYPED-VARIABLE PENDING CURL:handle  \ the request ASKER sends
\ ASKER's body, apart from BODY, which the main task's requests fill while
\ ASKER's is stalled.
TYPED-VARIABLE ASKED-BODY ^VEC:vector<u8>
TASK:MIN-STACK TASK:TASK ASKER
\ WAITER sends WAITING while ASKER's fetch is stalled, its body apart from
\ BODY.
TYPED-VARIABLE WAITED-BODY ^VEC:vector<u8>
TYPED-VARIABLE WAITING CURL:handle
TASK:MIN-STACK TASK:TASK WAITER

: VECTORS ( -- )
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW BODY !
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW HEADS !
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW ASKED-BODY !
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW WAITED-BODY ! ;

: BODY$ ( -- str ) BODY @ VEC:BYTES ;
TASK:MIN-STACK TASK:TASK CLAIMER

\ The bytes copied into the span at off; answers the offset past them.
: PUT ( str SPAN:span<u8> n -- n )
   {: s:str row off:n :}
   s STR:LEN {: u:n :}
   s STR:DATA row off u SPAN:SUB SPAN:$ STR:DATA u BYTE-COPY
   off u + ;


: SET+ ( str n -- n ) SETS swap PUT ;

: RSA-KEY+ ( str n -- n )
   {: kid:str off:n :}
   S\" {\"kty\":\"RSA\",\"kid\":\"" off SET+
   kid swap SET+
   S\" \",\"use\":\"sig\",\"alg\":\"RS256\",\"n\":\"" swap SET+
   JWS-FIXTURE:MODULUS$ swap SET+
   S\" \",\"e\":\"" swap SET+
   JWS-FIXTURE:EXPONENT$ swap SET+
   S\" \"}" swap SET+ ;

\ An EC key and an encryption key named a, whose n would not verify, before
\ the signing key a.
: SET-ONE-FILL ( -- )
   S\" {\"keys\":[{\"kty\":\"EC\",\"kid\":\"a\",\"crv\":\"P-256\",\"x\":\"AA\",\"y\":\"AA\"}," 0 SET+
   S\" {\"kty\":\"RSA\",\"kid\":\"a\",\"use\":\"enc\",\"n\":\"AQAB\",\"e\":\"AQAB\"}," swap SET+
   s" a" swap RSA-KEY+
   s" ]}" swap SET+ ONE-U ! ;

: SET-TWO-FILL ( -- )
   S\" {\"keys\":[" SET-CAP SET+
   s" a" swap RSA-KEY+
   s" ," swap SET+
   s" b" swap RSA-KEY+
   s" ]}" swap SET+ SET-CAP - TWO-U ! ;

\ The signing key c in the first of two keys members, the second empty: a
\ reader keeping either copy fails REPEATED-SET.
: SET-THREE-FILL ( -- )
   S\" {\"keys\":[" SET-CAP 2 * SET+
   s" c" swap RSA-KEY+
   S\" ],\"keys\":[]}" swap SET+ SET-CAP 2 * - THREE-U ! ;

: SET-ONE$ ( -- str ) SETS 0 ONE-U @ SPAN:SUB SPAN:$ ;
: SET-TWO$ ( -- str ) SETS SET-CAP TWO-U @ SPAN:SUB SPAN:$ ;
: SET-THREE$ ( -- str ) SETS SET-CAP 2 * THREE-U @ SPAN:SUB SPAN:$ ;


: CLAIM ( ^JSON:writer str -- ^JSON:writer )
   {: name:str :}
   name BEARER:CLAIM$ {: v:str has:bool :}
   has if name v JSON:WRITE-FIELD-S exit then
   name JSON:WRITE-FIELD-NULL ;

: CLAIMS-BODY ( ^JSON:writer -- ^JSON:writer )
   JSON:WRITE-OBJECT-START
   s" azp" CLAIM JSON:WRITE-COMMA
   s" oid" CLAIM JSON:WRITE-COMMA
   s" tid" CLAIM
   JSON:WRITE-OBJECT-END ;

: AZP-BODY ( ^JSON:writer -- ^JSON:writer )
   JSON:WRITE-OBJECT-START s" azp" CLAIM JSON:WRITE-OBJECT-END ;

: GUARDED-ROUTE ( HTTP:request HTTP:response -- )
   {: asked:HTTP:request answer:HTTP:response :}
   asked answer BEARER:ADMIT? if answer [: CLAIMS-BODY ;] HTTP:JSON! then ;

: PLAIN-ROUTE ( HTTP:request HTTP:response -- )
   nip [: AZP-BODY ;] HTTP:JSON! ;

: FLAG! ( bool n -- )
   {: at:n :}
   if [char] t else [char] f then
   HELD at 1 SPAN:SUB SPAN:$ STR:DATA c! ;

: HELD! ( str str n -- )
   {: at:n :}
   BEARER:CLAIM-HOLDS? at FLAG! ;

: APP-1? ( str bool -- bool )
   if s" app-1" STR:= else drop false then ;

: ROLES-ROUTE ( HTTP:request HTTP:response -- )
   {: asked:HTTP:request answer:HTTP:response :}
   asked answer BEARER:ADMIT? 0= if exit then
   s" azp" BEARER:CLAIM$ {: azp:str found:bool :}
   s" roles" s" Areas.Read" 0 HELD!
   s" roles" s" Areas.Write" 1 HELD!
   s" azp" s" app-1" 2 HELD!
   s" tid" s" 7" 3 HELD!
   s" oid" s" o-1" 4 HELD!
   s" scp" s" b" 5 HELD!
   s" scp" s" c" 6 HELD!
   s" scp" s" d" 7 HELD!
   s" scp" s" a b" 8 HELD!
   azp found APP-1? 9 FLAG!
   answer HELD SPAN:$ HTTP:BODY! ;

: CLAIM-FLAG ( -- )
   s" azp" BEARER:CLAIM$ nip
   s" azp" s" app-1" BEARER:CLAIM-HOLDS? or bool>n TASK:RETURN ;

\ Answers CLAIMER's flag; a throw in CLAIMER answers 500.
: SPAWN-ROUTE ( HTTP:request HTTP:response -- )
   {: asked:HTTP:request answer:HTTP:response :}
   asked answer BEARER:ADMIT? 0= if exit then
   ['] CLAIM-FLAG CLAIMER TASK:ACTIVATE
   CLAIMER TASK:JOIN MATCH result
      ok OF 0<> if answer s" true" HTTP:BODY! else answer s" false" HTTP:BODY! then ENDOF
      err OF throw ENDOF
   ;MATCH ;

: STALL ( -- )
   [: RELEASED atomic@ 0<> ;] TEST-WAIT:POLL ;

: KEYS-ROUTE ( HTTP:request HTTP:response -- )
   nip {: answer:HTTP:response :}
   1 HITS atomic-add drop
   MODE atomic@ 0= if answer 500 HTTP:STATUS! exit then
   MODE atomic@ 1 = if answer SET-ONE$ HTTP:BODY! exit then
   MODE atomic@ 4 = if answer SET-THREE$ HTTP:BODY! exit then
   MODE atomic@ 3 = if STALL then
   answer SET-TWO$ HTTP:BODY! ;

: EARLY-ROUTE ( HTTP:request HTTP:response -- )
   [: 2dup BEARER:ADMIT? drop ;] catch
   {: asked:HTTP:request answer:HTTP:response code:n :}
   code EARLY-CODE atomic! ;


\ A setter's refusal throws: the case cannot make its request on that handle.
: OK ( CURL:status -- )
   MATCH CURL:status
      ok OF ENDOF
      failed OF drop CURL:E-STATE throw ENDOF
   ;MATCH ;

: URL ( str -- str )
   {: path:str :}
   SB-RESET s" http://127.0.0.1:" SB-APPEND HTTP:PORT FMT:SB-U
   path SB-APPEND SB$ ;

: OPEN ( str -- CURL:handle )
   {: path:str :}
   CURL:INIT
   MATCH CURL:init-result
      ready OF ENDOF
      failed OF drop CURL:EMPTY-HANDLE ENDOF
   ;MATCH {: h:CURL:handle :}
   h path URL CURL:URL! OK h ;

: ADD ( CURL:handle str -- CURL:handle )
   {: h:CURL:handle s:str :}
   h s CURL:HEADER+ OK h ;

: BEARING ( CURL:handle -- CURL:handle ) AUTH AUTH-U @ >str ADD ;

\ The response's status, its body in the emptied vector; -1 for a body past
\ CAP, -2 for a failed transfer.
: TAKEN ( CURL:handle ^VEC:vector<u8> -- n )
   {: h:CURL:handle v:^ :}
   v 0 VEC:RESIZE
   h CAP >LEN v CURL:PERFORM
   MATCH CURL:fetch-result
      response OF drop CURL:HTTP-STATUS>N ENDOF
      truncated OF drop drop -1 ENDOF
      failed OF drop -2 ENDOF
   ;MATCH ;

: PERFORM ( CURL:handle -- n )
   {: h:CURL:handle :}
   HEADS @ 0 VEC:RESIZE
   h BODY @ TAKEN {: status:n :}
   status 0 > if h HEADS @ CURL:HEADERS then
   status ;

: FINISH ( CURL:handle -- n )
   {: h:CURL:handle :}
   h PERFORM h CURL:CLEANUP ;

: SEND ( -- n ) s" /claims" OPEN BEARING FINISH ;

: REFUSED ( n str -- )
   {: status:n want:str :}
   SB-RESET s" WWW-Authenticate: " SB-APPEND want SB-APPEND
   13 SB-APPEND-C 10 SB-APPEND-C
   HEADS @ VEC:BYTES SB$ STR:CONTAINS? TTRUE
   BODY @ VEC:COUNT 0 T=
   status 401 T= ;

: CHALLENGED ( n -- ) s" Bearer" REFUSED ;
: INVALID ( n -- ) S\" Bearer error=\"invalid_token\"" REFUSED ;
: NAMED ( n -- )
   S\" Bearer resource_metadata=\"https://bearer-test.example/.well-known/oauth-protected-resource\""
   REFUSED ;
: NAMED-INVALID ( n -- )
   S\" Bearer error=\"invalid_token\", resource_metadata=\"https://bearer-test.example/.well-known/oauth-protected-resource\""
   REFUSED ;
: BOTH ( n -- )
   S\" Bearer resource_metadata=\"https://bearer-test.example/.well-known/oauth-protected-resource/mcp\", scope=\"tender.read tender.write offline_access\""
   REFUSED ;
: BOTH-INVALID ( n -- )
   S\" Bearer error=\"invalid_token\", resource_metadata=\"https://bearer-test.example/.well-known/oauth-protected-resource/mcp\", scope=\"tender.read tender.write offline_access\""
   REFUSED ;
: SCOPED ( n -- ) S\" Bearer scope=\"tender.read tender.write offline_access\"" REFUSED ;
: SCOPED-INVALID ( n -- )
   S\" Bearer error=\"invalid_token\", scope=\"tender.read tender.write offline_access\"" REFUSED ;

: ANSWERED ( n str -- )
   {: status:n want:str :}
   BODY$ want T$=
   status 200 T= ;


: TOKEN+ ( str -- ) TOKEN TOKEN-U @ PUT TOKEN-U ! ;
: DOT+ ( -- ) s" ." TOKEN+ ;
: TOKEN$ ( -- str ) TOKEN 0 TOKEN-U @ SPAN:SUB SPAN:$ ;

: ENCODED+ ( str -- )
   TOKEN TOKEN-U @ TOKEN-CAP TOKEN-U @ - SPAN:SUB BASE64:ENCODE-URL
   TOKEN-U @ + TOKEN-U ! ;

: AUTHORIZE ( -- )
   s" Authorization: Bearer " {: p:str :}  p STR:LEN {: pu:n :}
   p STR:DATA AUTH pu BYTE-COPY
   TOKEN$ {: t:str :}  t STR:LEN {: tu:n :}
   t STR:DATA AUTH pu + tu BYTE-COPY
   pu tu + AUTH-U ! ;

: SIGNED ( str str -- )
   {: h:str c:str :}
   0 TOKEN-U !
   h ENCODED+ DOT+ c ENCODED+ ;

: NEW-VEC ( -- ^VEC:vector<u8> ) HEAP:ALLOCATOR 0 [type] u8 VEC:NEW ;

\ The signature the quotation appends to a fresh vector, as the token's
\ third segment; the vector is deleted on every path.
: SIGNATURE+ ( [ ^VEC:vector<u8> -- ] -- ) {: sign :}
   NEW-VEC {: v:^ :}
   [: v sign execute  DOT+ v VEC:BYTES ENCODED+ ;]
   [: v VEC:DELETE ;] finally ;

: RS-SIGNS ( ^VEC:vector<u8> -- ) {: v:^ :}
   JWS-FIXTURE:PEM$ TOKEN$ v CRYPTO:RS256-SIGN ;

: HS-SIGNS ( ^VEC:vector<u8> -- ) {: v:^ :}
   JWS-FIXTURE:MODULUS$ TOKEN$ v CRYPTO:HMAC-SHA256 ;

: MINT ( str str -- ) SIGNED [: RS-SIGNS ;] SIGNATURE+ AUTHORIZE ;

: HS-MINT ( str str -- ) SIGNED [: HS-SIGNS ;] SIGNATURE+ AUTHORIZE ;

: UNSIGNED ( str str -- ) SIGNED DOT+ AUTHORIZE ;

: RAW-TOKEN ( str -- ) 0 TOKEN-U ! TOKEN+ AUTHORIZE ;

\ One signature byte changed, away from the last character's padding bits.
: FLIP ( -- )
   TOKEN$ {: t:str :}  t STR:DATA t STR:LEN + 10 - {: at :}
   at c@ [char] A = if [char] B else [char] A then at c!
   AUTHORIZE ;

: CLAIMS ( str n str -- str )
   {: pre:str exp:n post:str :}
   SB-RESET pre SB-APPEND exp FMT:SB-INT post SB-APPEND SB$ ;

: ISS-AUD ( -- str )
   S\" {\"iss\":\"https://issuer.test\",\"aud\":\"api://bearer-test\",\"exp\":" ;
: WHO ( -- str ) S\" ,\"azp\":\"app-1\",\"oid\":\"o-1\",\"tid\":\"t-1\"}" ;
: WANT-WHO ( -- str ) S\" {\"azp\":\"app-1\",\"oid\":\"o-1\",\"tid\":\"t-1\"}" ;
: LATER ( -- n ) epoch-seconds LIFE-S + ;
: CURRENT ( -- str ) ISS-AUD LATER WHO CLAIMS ;
: KID-A ( -- str ) S\" {\"alg\":\"RS256\",\"kid\":\"a\"}" ;
: KID-B ( -- str ) S\" {\"alg\":\"RS256\",\"kid\":\"b\"}" ;
: KID-C ( -- str ) S\" {\"alg\":\"RS256\",\"kid\":\"c\"}" ;
: FETCHED ( -- ) mono-ns FETCHED-BY ! ;


: WAIT-WINDOW ( -- )
   FETCHED-BY @ REFETCH-MS NS-PER-MS * + DUE !
   [: DUE @ mono-ns <= ;] TEST-WAIT:POLL ;

: UNAVAILABLE ( -- )
   s" a key set that cannot be fetched answers an empty 503" T-LABEL
   0 MODE atomic!
   KID-A CURRENT MINT SEND {: status:n :}
   FETCHED
   BODY @ VEC:COUNT 0 T=
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
   s" an aud string holding the audience among other words is invalid" T-LABEL
   KID-A S\" {\"iss\":\"https://issuer.test\",\"aud\":\"api://bearer-test other\",\"exp\":"
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
   0 [: EDGE-TRY ;] TEST-WAIT:POLL WANT-WHO ANSWERED
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

: HOLDINGS ( -- )
   s" CLAIM-HOLDS? finds a string as a claim, a word of a scope or an array element past nested values, decoded, and nothing else, and leaves CLAIM$'s answer" T-LABEL
   KID-A ISS-AUD LATER
   S\" ,\"azp\":\"app-1\",\"roles\":[{\"r\":\"Areas.Write\"},[\"Areas.Write\"],\"Areas\\u002eRead\"],\"tid\":7,\"scp\":\"a b c\"}"
   CLAIMS MINT s" /roles" OPEN BEARING FINISH s" tftffttfft" ANSWERED ;

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
   s" CLAIM$ and CLAIM-HOLDS? in a task the admitted request spawns answer false" T-LABEL
   KID-A CURRENT MINT s" /spawn" OPEN BEARING FINISH s" false" ANSWERED ;

: SENT ( CURL:handle ^VEC:vector<u8> -- n )
   {: h:CURL:handle v:^ :}
   h v TAKEN h CURL:CLEANUP ;

: ASK ( -- ) PENDING @ ASKED-BODY @ SENT TASK:RETURN ;

: WAIT-ASK ( -- ) WAITING @ WAITED-BODY @ SENT TASK:RETURN ;

\ Sends AUTH's request from WAITER and releases the stalled /keys once
\ BEARER counts it as a waiter or it has been answered.
: HOLD ( -- )
   s" /claims" OPEN BEARING WAITING !
   ['] WAIT-ASK WAITER TASK:ACTIVATE
   [: WAITER TASK:DONE? BEARER-TEST:WAITERS 0 > or ;] TEST-WAIT:POLL
   1 RELEASED atomic! ;

: ASKED ( -- )
   ASKER TASK:JOIN MATCH result
      ok OF 200 T= ENDOF
      err OF throw ENDOF
   ;MATCH
   ASKED-BODY @ VEC:BYTES WANT-WHO T$= ;

: WAITED ( -- )
   WAITER TASK:JOIN MATCH result
      ok OF 200 T= ENDOF
      err OF throw ENDOF
   ;MATCH
   WAITED-BODY @ VEC:BYTES WANT-WHO T$= ;

: WINDOW ( -- )
   s" a fetch in flight does not delay a cached kid" T-LABEL
   WAIT-WINDOW
   3 MODE atomic!
   KID-B CURRENT MINT s" /claims" OPEN BEARING PENDING !
   ['] ASK ASKER TASK:ACTIVATE
   [: HITS atomic@ 3 = ;] TEST-WAIT:POLL
   KID-A CURRENT MINT SEND WANT-WHO ANSWERED
   2 MODE atomic!                      \ a second fetch would answer set two at once
   KID-B CURRENT MINT HOLD
   s" past the window an unknown kid refetches and the new set admits it" T-LABEL
   ASKED
   s" an unknown kid sent while a fetch is in flight waits for it and is admitted" T-LABEL
   WAITED
   HITS atomic@ 3 T= ;

\ ISSUER! has emptied the key set, as a new server process holds it.
: FRESH ( -- )
   0 RELEASED atomic!
   3 MODE atomic!
   KID-A CURRENT MINT s" /claims" OPEN BEARING PENDING !
   ['] ASK ASKER TASK:ACTIVATE
   [: HITS atomic@ 4 = ;] TEST-WAIT:POLL
   KID-A CURRENT MINT HOLD
   s" with an empty set the first token's fetch admits it" T-LABEL
   ASKED
   s" with an empty set a token sent while that fetch is in flight waits and is admitted" T-LABEL
   WAITED
   HITS atomic@ 4 T= ;

\ FRESH's fetch has returned, so the window runs from now at the latest.
\ SERVE-CASES states MCP$ and SCOPE$ for this run: challenges name both.
: REPEATED-SET ( -- )
   s" a fetched key set naming a member twice is not accepted" T-LABEL
   FETCHED WAIT-WINDOW
   4 MODE atomic!
   KID-C CURRENT MINT SEND BOTH-INVALID
   HITS atomic@ 5 T=
   s" the cached key set stays after a fetched one is refused" T-LABEL
   KID-A CURRENT MINT SEND WANT-WHO ANSWERED
   HITS atomic@ 5 T= ;

: METADATA$ ( -- str ) s" https://bearer-test.example/.well-known/oauth-protected-resource" ;
: MCP$ ( -- str ) s" https://bearer-test.example/.well-known/oauth-protected-resource/mcp" ;
: REPLACED$ ( -- str ) s" https://replaced.test/.well-known/oauth-protected-resource" ;
: SCOPE$ ( -- str ) s" tender.read tender.write" ;
: OFFLINE$ ( -- str ) s" tender.read tender.write offline_access" ;

: METADATA-SCOPE ( -- )
   s" with a URL and a scope no Authorization challenges Bearer naming both" T-LABEL
   s" /claims" OPEN FINISH BOTH
   s" with a URL and a scope a changed signature byte is invalid, naming both" T-LABEL
   KID-A CURRENT MINT FLIP SEND BOTH-INVALID
   s" the metadata route names the resource below the well-known segment, the issuer and the scopes" T-LABEL
   s" /.well-known/oauth-protected-resource/mcp" OPEN FINISH
   S\" {\"resource\":\"https://bearer-test.example/mcp\",\"authorization_servers\":[\"https://issuer.test\"],\"scopes_supported\":[\"tender.read\",\"tender.write\",\"offline_access\"]}"
   ANSWERED ;

: SCOPE-ONLY ( -- )
   s" with only a scope, which names offline_access, no Authorization challenges Bearer naming it once" T-LABEL
   s" /claims" OPEN FINISH SCOPED
   s" with only a scope a changed signature byte is invalid, naming the scope" T-LABEL
   KID-A CURRENT MINT FLIP SEND SCOPED-INVALID ;

: METADATA ( -- )
   s" with METADATA! no Authorization challenges Bearer naming the metadata" T-LABEL
   s" /claims" OPEN FINISH NAMED
   s" with METADATA! a changed signature byte is invalid, naming the metadata" T-LABEL
   KID-A CURRENT MINT FLIP SEND NAMED-INVALID
   s" the metadata route names the authority with no path, and no scopes_supported for an empty scope" T-LABEL
   s" /.well-known/oauth-protected-resource" OPEN FINISH
   S\" {\"resource\":\"https://bearer-test.example\",\"authorization_servers\":[\"https://issuer.test\"]}"
   ANSWERED ;

: NAMED-HOST$ ( -- str ) s" http://bearer-test.example/keys" ;
: LOCALHOST$ ( -- str ) s" http://localhost:8080/keys" ;
: V4$ ( -- str ) s" http://127.0.0.1:8080/keys" ;
: V6$ ( -- str ) s" http://[::1]:8080/keys" ;
: SECURE$ ( -- str ) s" https://bearer-test.example/keys" ;

: ISSUED-AT ( str -- )
   {: url:str :}
   s" https://issuer.test" s" api://bearer-test" url BEARER:ISSUER! ;

: CLIENT-AT ( str -- ) s" app" s" " BEARER:CLIENT! ;

\ Leaves ISSUER! and CLIENT! stating SECURE$.
: URL-CASES ( -- )
   s" ISSUER! rejects an http URL to a named host" T-LABEL
   [: NAMED-HOST$ ISSUED-AT ;] BEARER:E-OPERAND TTHROWSQ
   s" ISSUER! rejects http://localhost" T-LABEL
   [: LOCALHOST$ ISSUED-AT ;] BEARER:E-OPERAND TTHROWSQ
   s" ISSUER! rejects an http iss to a named host" T-LABEL
   [: s" http://bearer-test.example" s" api://bearer-test" SECURE$ BEARER:ISSUER! ;]
   BEARER:E-OPERAND TTHROWSQ
   s" ISSUER! passes URI's rejection of user information" T-LABEL
   [: s" http://127.0.0.1@evil.example/keys" ISSUED-AT ;] E-URI-HOST TTHROWSQ
   s" CLIENT! rejects an http URL to a named host" T-LABEL
   [: NAMED-HOST$ CLIENT-AT ;] BEARER:E-OPERAND TTHROWSQ
   s" CLIENT! rejects http://localhost" T-LABEL
   [: LOCALHOST$ CLIENT-AT ;] BEARER:E-OPERAND TTHROWSQ
   s" ISSUER! and CLIENT! accept http to 127.0.0.1 and [::1], and https" T-LABEL
   V4$ ISSUED-AT V6$ ISSUED-AT SECURE$ ISSUED-AT
   V4$ CLIENT-AT V6$ CLIENT-AT SECURE$ CLIENT-AT ;

: METADATA-AT ( str -- ) SCOPE$ BEARER:METADATA! ;

: METADATA-ACCEPTS ( -- )
   s" METADATA! accepts http to 127.0.0.1 and [::1]" T-LABEL
   s" http://127.0.0.1:8080/.well-known/oauth-protected-resource" METADATA-AT
   s" http://[::1]:8080/.well-known/oauth-protected-resource/mcp" METADATA-AT ;

: METADATA-REJECTS ( -- )
   s" METADATA! rejects an http URL to a named host" T-LABEL
   [: s" http://bearer-test.example/.well-known/oauth-protected-resource" METADATA-AT ;]
   BEARER:E-OPERAND TTHROWSQ
   s" METADATA! rejects http://localhost" T-LABEL
   [: s" http://localhost:8080/.well-known/oauth-protected-resource" METADATA-AT ;]
   BEARER:E-OPERAND TTHROWSQ
   s" METADATA! rejects a URL without the well-known segment" T-LABEL
   [: s" https://bearer-test.example/mcp" METADATA-AT ;] BEARER:E-OPERAND TTHROWSQ ;


: ROUTES ( -- )
   HTTP:ROUTES-RESET
   s" GET" s" /early" [: EARLY-ROUTE ;] HTTP:ROUTE
   s" GET" s" /claims" [: GUARDED-ROUTE ;] HTTP:ROUTE
   s" GET" s" /plain" [: PLAIN-ROUTE ;] HTTP:ROUTE
   s" GET" s" /spawn" [: SPAWN-ROUTE ;] HTTP:ROUTE
   s" GET" s" /roles" [: ROLES-ROUTE ;] HTTP:ROUTE
   s" GET" s" /keys" [: KEYS-ROUTE ;] HTTP:ROUTE
   s" GET" s" /.well-known/oauth-protected-resource" [: BEARER:RESOURCE-METADATA ;] HTTP:ROUTE
   s" GET" s" /.well-known/oauth-protected-resource/mcp" [: BEARER:RESOURCE-METADATA ;]
   HTTP:ROUTE ;

: EARLY ( -- )
   0 SERVE-ON
   s" ADMIT? before ISSUER! throws E-HTTP-STATE" T-LABEL
   s" /early" OPEN FINISH drop
   EARLY-CODE atomic@ E-HTTP-STATE T=
   s" ISSUER! while HTTP serves throws E-HTTP-STATE" T-LABEL
   [: s" a" s" b" s" c" BEARER:ISSUER! ;] E-HTTP-STATE TTHROWSQ
   s" METADATA! while HTTP serves throws E-HTTP-STATE" T-LABEL
   [: METADATA$ SCOPE$ BEARER:METADATA! ;] E-HTTP-STATE TTHROWSQ
   HTTP:PORT PORT !
   s" /keys" URL {: keys:str :}  keys STR:LEN {: u:n :}
   keys STR:DATA KEYS-URL u BYTE-COPY u KEYS-URL-U !
   HTTP:STOP ;

: ISSUE ( -- )
   s" https://issuer.test" s" api://bearer-test" KEYS-URL KEYS-URL-U @ >str
   BEARER:ISSUER! ;

\ The key set's URL names the port the first START's port 0 chose, and
\ ISSUER! is refused while serving: the server starts again on that port.
: SERVE-CASES ( -- )
   AIO:START
   VECTORS
   SET-ONE-FILL SET-TWO-FILL SET-THREE-FILL
   ROUTES
   EARLY
   ISSUE
   \ Empty texts after a stated URL and scope restore the plain challenges,
   \ which CHALLENGED and INVALID assert.
   METADATA$ SCOPE$ BEARER:METADATA!
   s" " s" " BEARER:METADATA!
   PORT @ SERVE-ON
   UNAVAILABLE ADMITTED UNKNOWN-KID NO-TOKEN MALFORMED SIGNATURES AUDIENCES
   TIMES STRINGS HOLDINGS STALE-CLAIM SPAWNED-CLAIM WINDOW
   HTTP:STOP
   URL-CASES
   ISSUE
   \ METADATA! is refused while serving, so its challenges are immutable
   \ configuration for a server's run: each statement gets a start of its own.
   MCP$ SCOPE$ BEARER:METADATA!
   PORT @ SERVE-ON
   FRESH REPEATED-SET METADATA-SCOPE
   HTTP:STOP
   s" " OFFLINE$ BEARER:METADATA!
   PORT @ SERVE-ON
   SCOPE-ONLY
   HTTP:STOP
   METADATA-ACCEPTS
   \ The second statement replaces the first.
   REPLACED$ SCOPE$ BEARER:METADATA!
   METADATA$ s" " BEARER:METADATA!
   \ A refused call installs neither of its texts: METADATA$'s challenges stay
   \ in force, as METADATA asserts.
   s" METADATA! with a quote in its URL throws E-OPERAND" T-LABEL
   [: s\" https://bearer-test.example/.well-known/oauth-protected-resource/\"x"
      SCOPE$ BEARER:METADATA! ;] BEARER:E-OPERAND TTHROWSQ
   s" METADATA! with CR LF in its URL throws E-OPERAND" T-LABEL
   [: s\" https://bearer-test.example/.well-known/oauth-protected-resource/\r\nX-Injected: 1"
      SCOPE$ BEARER:METADATA! ;] BEARER:E-OPERAND TTHROWSQ
   s" METADATA! with a byte above $7E in its URL throws E-OPERAND" T-LABEL
   [: s\" https://bearer-test.example/.well-known/oauth-protected-resource/\xFF"
      SCOPE$ BEARER:METADATA! ;] BEARER:E-OPERAND TTHROWSQ
   s" METADATA! with a quote in its scope throws E-OPERAND" T-LABEL
   [: REPLACED$ s\" tender.read \"x" BEARER:METADATA! ;] BEARER:E-OPERAND TTHROWSQ
   s" METADATA! with two spaces between scope tokens throws E-OPERAND" T-LABEL
   [: REPLACED$ s" tender.read  tender.write" BEARER:METADATA! ;] BEARER:E-OPERAND TTHROWSQ
   METADATA-REJECTS
   PORT @ SERVE-ON
   METADATA
   HTTP:STOP
   AIO:STOP ;

T-RESET
SERVE-CASES
T-REPORT

;package
