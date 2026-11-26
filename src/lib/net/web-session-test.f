\ web-session-test.f - WEB-SESSION over a real loopback port that also serves
\ TEST-ISSUER, the test as the browser: CURL follows no redirect and sends each
\ Cookie by hand. A page with no session redirects to the authorize URL with the
\ attempt cookie, clearing the oldest when the browser sent four and never a
\ name whose suffix is no challenge; the issuer's code redeemed at the callback
\ makes a session whose cookie the API and the page routes admit, their bodies
\ the person's access token; no cookie and an unknown session are 401
\ signed_out; another Origin is 403 origin_rejected; a state naming no cookie, a
\ cookie whose verifier does not hash to it and a cleared cookie's second
\ callback are 400 no_sign_in; a declined sign-in is 403 sign_in_rejected after
\ one log line, clearing the cookie; a code the issuer never made is 400
\ sign_in_failed and an ID token of another iss 502, each clearing the cookie;
\ an unreachable token endpoint is 503, keeping the attempt, which signs in once
\ the endpoint is back; a stale session is renewed by its refresh token, 503 and
\ kept while the token endpoint is unreachable, and dropped, 401 signed_out,
\ when the issuer rejects the refresh; END clears the session cookie, with or
\ without one, and the ended session is 401 signed_out, as is a session unused
\ longer than idle, and the most milliseconds an idle holds admit a session; a
\ target with a byte outside $21..$7E, or whose state would pass an eighth of a
\ request head, returns to `/`, and so does a callback's state that names
\ another host or passes that eighth; START while serving and before ISSUER! and
\ CLIENT! is E-HTTP-STATE, and an http authorize URL to a named host, one with a
\ fragment, a redirect URI with a query or a brace, a scope with two spaces and
\ an idle of 0 are E-OPERAND naming the setting; a second START on the redirect
\ path is E-ROUTE and leaves the first in force, and after HTTP:ROUTES-RESET a
\ START routes the callback again.
\ Run: bin/hb --load src/lib/net/web-session-test.f

require src/lib/net/http-request.f
require src/lib/net/http-response.f
require src/lib/net/http-router.f
require src/lib/test/assert.f
require src/lib/test/stderr.f
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
require src/lib/json.f
require src/lib/json/read.f
require src/lib/reason.f
require src/lib/uri.f
require src/lib/net/curl.f
require src/lib/net/http.f
require src/lib/net/http-arena.f
require src/lib/net/test-issuer.f
require src/lib/net/bearer.f
require src/lib/net/web-session.f

package WEB-SESSION-TEST
private
EXPORT BEARER:LEEWAY-S

$7F000001 constant LOOPBACK
2 constant TWO-WORKERS              \ the callback's, and the issuer's for its REDEEM
500 constant IDLE-MS
43 constant SECRET-LEN

TYPED-VARIABLE CFG HTTP:config
TYPED-VARIABLE SIGN-IN WEB-SESSION:config
TYPED-VARIABLE WRONG WEB-SESSION:config

: SERVE-ON ( n -- )
   {: port:n :}
   LOOPBACK CFG HTTP-CONFIG:ADDRESS !
   port CFG HTTP-CONFIG:PORT !
   TWO-WORKERS CFG HTTP-CONFIG:WORKERS !
   IDLE-MS >MS CFG HTTP-CONFIG:IDLE !
   CFG HTTP:START ;

$2000 constant CAP
TYPED-VARIABLE BODY ^VEC:vector<u8>
TYPED-VARIABLE HEADS ^VEC:vector<u8>
TYPED-VARIABLE KEPT ^VEC:vector<u8>   \ what a case keeps across its requests
TYPED-VARIABLE TEXT ^VEC:vector<u8>   \ a header line or URL a case composes
CAP BUFFER: CLAIMS
variable CLAIMS-U
TYPED-VARIABLE READER JSON:cursor

$80 constant URL-CAP
URL-CAP BUFFER: ISSUER-URL
variable ISSUER-URL-U
URL-CAP BUFFER: KEYS-URL
variable KEYS-URL-U
URL-CAP BUFFER: ENDPOINT
variable ENDPOINT-U
URL-CAP BUFFER: AUTHORIZE
variable AUTHORIZE-U
URL-CAP BUFFER: BACK
variable BACK-U
URL-CAP BUFFER: BACK-QUERY
variable BACK-QUERY-U
CAP BUFFER: STATE
variable STATE-U
SECRET-LEN BUFFER: SESSION-ID
SECRET-LEN BUFFER: ENDING           \ the session ENDED ends, SESSION-ID's kept
CAP BUFFER: CALLBACK-URL
variable CALLBACK-U
variable PORT-KEPT                  \ the server's port, across its restart

: NEW-VEC ( -- ^VEC:vector<u8> ) HEAP:ALLOCATOR 0 [type] u8 VEC:NEW ;
: BODY$ ( -- str ) BODY @ VEC:BYTES ;
: STATE$ ( -- str ) STATE STATE-U @ >str ;
: CHALLENGE$ ( -- str ) STATE SECRET-LEN >str ;
: SESSION-ID$ ( -- str ) SESSION-ID SECRET-LEN >str ;
: ENDING$ ( -- str ) ENDING SECRET-LEN >str ;
: CALLBACK$ ( -- str ) CALLBACK-URL CALLBACK-U @ >str ;

\ The text copied into a row, answering its length.
: COPY ( str ^u8 -- n )
   {: s:str row:^ :}
   s STR:DATA row s STR:LEN BYTE-COPY s STR:LEN ;

: OK ( CURL:status -- )
   MATCH CURL:status
      ok OF true ENDOF
      failed OF drop false ENDOF
   ;MATCH TTRUE ;

: URL ( str -- str )
   {: path:str :}
   SB-RESET s" http://127.0.0.1:" SB-APPEND HTTP:PORT FMT:SB-U
   path SB-APPEND SB$ ;

: ADDRESSES ( -- )
   HTTP:PORT PORT-KEPT !
   s" " URL ISSUER-URL COPY ISSUER-URL-U !
   s" /keys" URL KEYS-URL COPY KEYS-URL-U !
   s" /token" URL ENDPOINT COPY ENDPOINT-U !
   s" /authorize" URL AUTHORIZE COPY AUTHORIZE-U !
   s" /callback" URL BACK COPY BACK-U !
   s" /callback?to=1" URL BACK-QUERY COPY BACK-QUERY-U ! ;

: ISSUER$ ( -- str ) ISSUER-URL ISSUER-URL-U @ >str ;
: KEYS-URL$ ( -- str ) KEYS-URL KEYS-URL-U @ >str ;
: ENDPOINT$ ( -- str ) ENDPOINT ENDPOINT-U @ >str ;
: AUTHORIZE$ ( -- str ) AUTHORIZE AUTHORIZE-U @ >str ;
: BACK$ ( -- str ) BACK BACK-U @ >str ;
: BACK-QUERY$ ( -- str ) BACK-QUERY BACK-QUERY-U @ >str ;

: OPEN-AT ( str -- CURL:handle )
   {: url:str :}
   CURL:INIT
   MATCH CURL:init-result
      ready OF ENDOF
      failed OF drop CURL:EMPTY-HANDLE ENDOF
   ;MATCH {: h:CURL:handle :}
   h url CURL:URL! OK h ;

: ADD ( CURL:handle str -- CURL:handle )
   {: h:CURL:handle s:str :}
   h s CURL:HEADER+ OK h ;

\ The response's status, its body in BODY and its head in HEADS; -1 for a
\ body past CAP, -2 for a failed transfer. A redirect is not followed.
: FINISH ( CURL:handle -- n )
   {: h:CURL:handle :}
   BODY @ 0 VEC:RESIZE
   HEADS @ 0 VEC:RESIZE
   h CAP >LEN BODY @ CURL:PERFORM
   MATCH CURL:fetch-result
      response OF drop CURL:HTTP-STATUS>N h HEADS @ CURL:HEADERS ENDOF
      truncated OF drop drop -1 ENDOF
      failed OF drop -2 ENDOF
   ;MATCH
   h CURL:CLEANUP ;

: GET-AT ( str -- n ) OPEN-AT FINISH ;
: GET ( str -- n ) URL GET-AT ;

\ A Cookie header of the name and value.
: COOKIE ( str str -- str )
   {: name:str value:str :}
   TEXT @ 0 VEC:RESIZE
   s" Cookie: " TEXT @ VEC:APPEND  name TEXT @ VEC:APPEND
   [char] = TEXT @ VEC:PUSH c!  value TEXT @ VEC:APPEND
   TEXT @ VEC:BYTES ;

: SESSION ( -- str ) s" __Host-session" SESSION-ID$ COOKIE ;

\ The path asked with the session's cookie.
: ASKED ( str -- n ) URL OPEN-AT SESSION ADD FINISH ;

\ The offset just past the key's first copy in the text, else -1.
: AFTER ( str str -- n )
   {: t:str k:str :}
   t STR:LEN k STR:LEN - 1+ 0 max 0 ?do
      t STR:DATA i + k STR:LEN >str k STR:= if i k STR:LEN + unloop exit then
   loop -1 ;

\ The text up to the byte's first copy, else all of it.
: UPTO ( str n -- str )
   {: s:str c:n :}
   s STR:LEN 0 ?do
      s STR:DATA i + c@ c = if s STR:DATA i >str unloop exit then
   loop s ;

\ The value of the head's first line after the key, empty without one.
: HEAD-AFTER$ ( str -- str )
   {: k:str :}
   HEADS @ VEC:BYTES {: hd:str :}
   hd k AFTER {: at:n :}
   at 0< if s" " exit then
   hd STR:DATA at + hd STR:LEN at - >str 13 UPTO ;

: LOCATION$ ( -- str ) s" Location: " HEAD-AFTER$ ;

\ The Set-Cookie line setting the name, after `name=`.
: SET$ ( str -- str )
   {: name:str :}
   TEXT @ 0 VEC:RESIZE
   s" Set-Cookie: " TEXT @ VEC:APPEND  name TEXT @ VEC:APPEND
   [char] = TEXT @ VEC:PUSH c!
   TEXT @ VEC:BYTES HEAD-AFTER$ ;

: SIGNIN-NAME ( -- str )
   SB-RESET s" __Host-signin-" SB-APPEND CHALLENGE$ SB-APPEND SB$ ;

\ The Location's text after the key up to the next `&`, empty without it.
: SENT$ ( str -- str )
   {: k:str :}
   LOCATION$ {: l:str :}
   l k AFTER {: at:n :}
   at 0< if s" " exit then
   l STR:DATA at + l STR:LEN at - >str [char] & UPTO ;

: ATTEMPT-ATTRIBUTES ( -- str )
   s" ; Path=/; Max-Age=600; Secure; HttpOnly; SameSite=Lax" ;

\ The authorize URL the attempt of challenge CHALLENGE$ names, the page's
\ target in base64url given.
: AUTHORIZE-SENT ( str -- str )
   {: target:str :}
   KEPT @ 0 VEC:RESIZE
   AUTHORIZE$ KEPT @ VEC:APPEND
   s" ?response_type=code&client_id=app&redirect_uri=" KEPT @ VEC:APPEND
   BACK$ KEPT @ URI:QUERY-ENCODE
   s" &scope=api%3A%2F%2Fup%2Fread%20openid%20offline_access&state="
   KEPT @ VEC:APPEND
   CHALLENGE$ KEPT @ VEC:APPEND  [char] . KEPT @ VEC:PUSH c!
   target KEPT @ VEC:APPEND  s" &nonce=" KEPT @ VEC:APPEND
   CHALLENGE$ KEPT @ VEC:APPEND  s" &code_challenge=" KEPT @ VEC:APPEND
   CHALLENGE$ KEPT @ VEC:APPEND
   s" &code_challenge_method=S256" KEPT @ VEC:APPEND
   KEPT @ VEC:BYTES ;

\ The 302's state, the challenge and `.` first, kept in STATE.
: STATE-KEPT ( -- )
   s" &state=" SENT$ {: st:str :}
   st STR:LEN SECRET-LEN > TTRUE
   st STR:DATA SECRET-LEN + c@ [char] . T=
   st STATE COPY STATE-U ! ;

\ The page asked with no cookie: a 302 whose state is kept in STATE.
: ATTEMPTED ( str -- ) GET 302 T= STATE-KEPT ;

\ The attempt cookie's value, kept in KEPT.
: ATTEMPT-VALUE ( -- str )
   SIGNIN-NAME SET$ {: line:str :}
   line ATTEMPT-ATTRIBUTES STR:ENDS-WITH? TTRUE
   KEPT @ 0 VEC:RESIZE
   line STR:DATA line STR:LEN ATTEMPT-ATTRIBUTES STR:LEN - >str
   KEPT @ VEC:APPEND
   KEPT @ VEC:BYTES ;

\ The attempt's Location followed as the person of the hint, the callback
\ URL the issuer redirects to kept in CALLBACK-URL.
: AUTHORIZED ( str -- )
   {: hint:str :}
   TEXT @ 0 VEC:RESIZE
   LOCATION$ TEXT @ VEC:APPEND  s" &login_hint=" TEXT @ VEC:APPEND
   hint TEXT @ VEC:APPEND
   TEXT @ VEC:BYTES GET-AT 302 T=
   LOCATION$ CALLBACK-URL COPY CALLBACK-U ! ;

\ The callback asked with the attempt cookie's value.
: CALLED ( str -- n )
   {: value:str :}
   CALLBACK$ OPEN-AT {: h:CURL:handle :}
   h SIGNIN-NAME value COOKIE ADD FINISH ;

\ The page signed in as the person of the hint: its callback's status.
: SIGNED ( str str -- n )
   {: page:str hint:str :}
   page ATTEMPTED  ATTEMPT-VALUE {: value:str :}
   hint AUTHORIZED
   value CALLED ;

: CLEARED$ ( -- str ) s" ; Path=/; Max-Age=0; Secure; HttpOnly; SameSite=Lax" ;

\ The body's claims, decoded into CLAIMS.
: TOKEN-CLAIMS ( -- )
   BODY$ {: t:str :}
   t [char] . UPTO STR:LEN 1+ {: from:n :}
   t STR:DATA from + t STR:LEN from - >str [char] . UPTO {: c:str :}
   c CLAIMS CAP SPAN:MAKE BASE64:DECODE-URL CLAIMS-U ! ;

: MEMBER$ ( str str -- str )
   {: t:str k:str :}
   READER t JSON:READ-INIT JSON:READ-NEXT drop
   k JSON:READ-FIND-KEY 0= if JSON:READ-CLOSE s" " exit then
   JSON:READ-TOKEN JSON:T-STR <> if JSON:READ-CLOSE s" " exit then
   JSON:READ-SPAN$ {: s:str :} JSON:READ-CLOSE s ;

: OID$ ( -- str ) TOKEN-CLAIMS CLAIMS CLAIMS-U @ >str s" oid" MEMBER$ ;

: ANSWERED-AS ( n n str -- )
   {: got:n want:n code:str :}
   got want T=
   BODY$ code STR:STARTS-WITH? TTRUE ;

\ The routes the guards protect: 200 with the token as the body.
: ANSWER ( HTTP:response ^VEC:vector<u8> -- )
   {: answer:HTTP:response v:^ :}
   answer 200 HTTP:STATUS!
   answer v VEC:BYTES HTTP:BODY! ;

\ The token each route answers stays in its worker's vector until that
\ worker's next request, which the response is sent before.
HTTP:MAX-WORKERS TYPED-BUFFER GIVEN ^VEC:vector<u8>

: GIVEN-VEC ( -- ^VEC:vector<u8> ) HTTP:WORKER-SLOT GIVEN @ ;

: API-ROUTE ( HTTP:request HTTP:response -- )
   {: asked:HTTP:request answer:HTTP:response :}
   asked answer GIVEN-VEC WEB-SESSION:ADMIT? 0= if exit then
   answer GIVEN-VEC ANSWER ;

: PAGE-ROUTE ( HTTP:request HTTP:response -- )
   {: asked:HTTP:request answer:HTTP:response :}
   asked answer GIVEN-VEC WEB-SESSION:ADMIT-PAGE? 0= if exit then
   answer GIVEN-VEC ANSWER ;

: SIGNOUT-ROUTE ( HTTP:request HTTP:response -- )
   {: asked:HTTP:request answer:HTTP:response :}
   asked answer WEB-SESSION:END
   answer 204 HTTP:STATUS! ;

create LOG-PATH PATH-CAP allot      \ a case's stderr capture file
variable LOG-PATH-U
TYPED-VARIABLE LOG-B ^VEC:vector<u8>  \ the capture file's bytes, read back
$A constant LF

: LOG-PATH$ ( -- str ) LOG-PATH LOG-PATH-U @ >str ;

: LOG-INIT ( -- )
   s" web-session-test" FS:MKTEMP-DIR {: dir:str :}
   dir FS:CLEANUP-TREE+
   dir s" stderr.txt" LOG-PATH FS:JOIN-PATH LOG-PATH-U !
   NEW-VEC LOG-B ! ;

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

: SAID ( str -- ) BEARER:E-OPERAND REASON:TEXT$ swap STR:STARTS-WITH? TTRUE ;

: WRONG-START ( -- ) WRONG WEB-SESSION:START ;

\ WRONG as SIGN-IN, its field then changed by the caller.
: WRONG-RESET ( -- )
   AUTHORIZE$ WRONG WEB--SESSION-CONFIG:AUTHORIZE !
   BACK$ WRONG WEB--SESSION-CONFIG:REDIRECT !
   s" api://up/read" WRONG WEB--SESSION-CONFIG:SCOPE !
   600000 >MS WRONG WEB--SESSION-CONFIG:IDLE ! ;

: UNSTATED ( -- )
   s" START before ISSUER! and CLIENT! is E-HTTP-STATE" T-LABEL
   WRONG-RESET
   [: WRONG-START ;] E-HTTP-STATE TTHROWSQ ;

: SETTINGS ( -- )
   s" an http authorize URL to a named host is E-OPERAND naming it" T-LABEL
   WRONG-RESET
   s" http://issuer.example/authorize" WRONG WEB--SESSION-CONFIG:AUTHORIZE !
   [: WRONG-START ;] BEARER:E-OPERAND TTHROWSQ
   s" WEB-SESSION:START's authorize URL `http://issuer.example/authorize`: expected https"
   SAID
   s" a redirect URI with a query is E-OPERAND naming it" T-LABEL
   WRONG-RESET
   BACK-QUERY$ WRONG WEB--SESSION-CONFIG:REDIRECT !
   [: WRONG-START ;] BEARER:E-OPERAND TTHROWSQ
   s" WEB-SESSION:START's redirect URI `" SAID
   BEARER:E-OPERAND REASON:TEXT$ s" found byte $3F" STR:CONTAINS? TTRUE
   s" a scope with two spaces is E-OPERAND naming it" T-LABEL
   WRONG-RESET
   s" api://up/read  more" WRONG WEB--SESSION-CONFIG:SCOPE !
   [: WRONG-START ;] BEARER:E-OPERAND TTHROWSQ
   s" WEB-SESSION:START's scope `api://up/read  more`: expected tokens" SAID
   s" an idle of 0 is E-OPERAND naming it" T-LABEL
   WRONG-RESET
   0 >MS WRONG WEB--SESSION-CONFIG:IDLE !
   [: WRONG-START ;] BEARER:E-OPERAND TTHROWSQ
   s" WEB-SESSION:START's idle 0 ms: expected at least 1 ms" SAID ;

: FOUND ( str -- ) BEARER:E-OPERAND REASON:TEXT$ swap STR:CONTAINS? TTRUE ;

: MARKED ( -- )
   s" a redirect URI with a brace is E-OPERAND naming it" T-LABEL
   WRONG-RESET
   s" http://127.0.0.1/items/{id}" WRONG WEB--SESSION-CONFIG:REDIRECT !
   [: WRONG-START ;] BEARER:E-OPERAND TTHROWSQ
   s" WEB-SESSION:START's redirect URI `http://127.0.0.1/items/{id}`: " SAID
   s" found byte $7B" FOUND  s" percent-encode it" FOUND
   s" an authorize URL with a fragment is E-OPERAND naming it" T-LABEL
   WRONG-RESET
   s" http://127.0.0.1/authorize#top" WRONG WEB--SESSION-CONFIG:AUTHORIZE !
   [: WRONG-START ;] BEARER:E-OPERAND TTHROWSQ
   s" WEB-SESSION:START's authorize URL `http://127.0.0.1/authorize#top`: "
   SAID
   s" found byte $23" FOUND  s" drop the fragment" FOUND ;

: STARTED-IDLE ( ms -- )
   SIGN-IN WEB--SESSION-CONFIG:IDLE !
   AUTHORIZE$ SIGN-IN WEB--SESSION-CONFIG:AUTHORIZE !
   BACK$ SIGN-IN WEB--SESSION-CONFIG:REDIRECT !
   s" api://up/read" SIGN-IN WEB--SESSION-CONFIG:SCOPE !
   SIGN-IN WEB-SESSION:START ;

: STARTED ( -- ) $7FFFFFFFFFFFFFFF >MS STARTED-IDLE ;

: ATTEMPTS ( -- )
   s" a page with no cookie redirects to the authorize URL" T-LABEL
   s" /page?x=1" ATTEMPTED
   LOCATION$ s" L3BhZ2U_eD0x" AUTHORIZE-SENT T$=
   s" its attempt cookie holds the verifier alone" T-LABEL
   ATTEMPT-VALUE STR:LEN SECRET-LEN T= ;

$40 BUFFER: NAMED

\ An attempt cookie's name whose suffix is shaped as a challenge: the digit,
\ then `A` to SECRET-LEN bytes.
: ATTEMPT-NAME ( n -- str )
   {: k:n :}
   s" __Host-signin-" NAMED COPY {: p:n :}
   SECRET-LEN 0 ?do [char] A NAMED p + i + c! loop
   [char] 0 k + NAMED p + c!
   NAMED p SECRET-LEN + >str ;

: CROWDED ( -- )
   s" a fifth attempt clears the oldest earlier one alone" T-LABEL
   s" /page" URL OPEN-AT {: h:CURL:handle :}
   SB-RESET s" Cookie: __Host-signin-x=1; " SB-APPEND
   1 ATTEMPT-NAME SB-APPEND s" =1; other=x; " SB-APPEND
   2 ATTEMPT-NAME SB-APPEND s" =2; " SB-APPEND
   3 ATTEMPT-NAME SB-APPEND s" =3; " SB-APPEND
   4 ATTEMPT-NAME SB-APPEND s" =4" SB-APPEND
   h SB$ ADD FINISH 302 T=
   STATE-KEPT
   1 ATTEMPT-NAME SET$ CLEARED$ T$=
   2 ATTEMPT-NAME SET$ s" " T$=
   3 ATTEMPT-NAME SET$ s" " T$=
   4 ATTEMPT-NAME SET$ s" " T$=
   s" other" SET$ s" " T$=
   s" a name whose suffix is no challenge is neither counted nor echoed" T-LABEL
   s" __Host-signin-x" SET$ s" " T$=
   ATTEMPT-VALUE drop ;

: SIGN-INS ( -- )
   s" the callback makes a session and returns to the page" T-LABEL
   s" /page?x=1" s" user-9" SIGNED 302 T=
   LOCATION$ s" /page?x=1" URL T$=
   s" __Host-session" SET$ {: line:str :}
   line STR:LEN SECRET-LEN > TTRUE
   line STR:DATA SECRET-LEN + line STR:LEN SECRET-LEN - >str
   s" ; Path=/; Secure; HttpOnly; SameSite=Lax" T$=
   line STR:DATA SESSION-ID SECRET-LEN BYTE-COPY
   SIGNIN-NAME SET$ CLEARED$ T$=
   s" the API route admits the session with the person's token" T-LABEL
   s" /api" ASKED 200 T=
   OID$ s" user-9" T$=
   s" the page route admits it too" T-LABEL
   s" /page" ASKED 200 T=
   OID$ s" user-9" T$=
   s" a cleared cookie's second callback is 400 no_sign_in" T-LABEL
   CALLBACK$ GET-AT 400 s" no_sign_in" ANSWERED-AS ;

: SIGNED-OUT ( -- )
   s" no cookie is 401 signed_out" T-LABEL
   s" /api" GET 401 s" signed_out" ANSWERED-AS
   s" an unknown session is 401 signed_out" T-LABEL
   s" /api" URL OPEN-AT
   s" __Host-session" s" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" COOKIE ADD
   FINISH 401 s" signed_out" ANSWERED-AS ;

: ORIGINS ( -- )
   s" another Origin is 403 origin_rejected" T-LABEL
   s" /api" URL OPEN-AT SESSION ADD s" Origin: http://evil.example" ADD
   FINISH 403 s" origin_rejected" ANSWERED-AS
   s" the redirect URI's own Origin is admitted" T-LABEL
   SB-RESET s" Origin: " SB-APPEND s" " URL SB-APPEND SB$ {: o:str :}
   s" /api" URL OPEN-AT SESSION ADD o ADD FINISH 200 T= ;

: UNPROVEN ( -- )
   s" a state naming no cookie is 400 no_sign_in" T-LABEL
   s" /callback?code=c&state=BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB.Lw" GET
   400 s" no_sign_in" ANSWERED-AS
   s" a cookie whose verifier does not hash to the challenge is 400 no_sign_in"
   T-LABEL
   s" /page" ATTEMPTED  ATTEMPT-VALUE drop
   s" user-9" AUTHORIZED
   s" AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" CALLED
   400 s" no_sign_in" ANSWERED-AS ;

: DECLINED ( -- )
   s" a declined sign-in is 403 sign_in_rejected, logged, the cookie cleared"
   T-LABEL
   [: s" /page" s" denied" SIGNED 403 s" sign_in_rejected" ANSWERED-AS ;]
   CAPTURED
   SIGNIN-NAME SET$ CLEARED$ T$=
   LINES 1 T=
   LOG$ s" error web-session: the issuer answered a sign-in with error access_denied"
   STR:CONTAINS? TTRUE ;

\ The longest target whose state, the challenge, `.` and the target in
\ base64url, fits an eighth of a request head.
HTTP:HEAD-CAP 8 / SECRET-LEN - 1- 3 * 4 / constant EDGE-LEN
EDGE-LEN 1+ BUFFER: EDGE-PAGE

\ A target of the length: `/page?x=`, then `a`s.
: EDGE$ ( n -- str )
   {: len:n :}
   s" /page?x=" EDGE-PAGE COPY {: at:n :}
   len at ?do [char] a EDGE-PAGE i + c! loop
   EDGE-PAGE len >str ;

: TARGETS ( -- )
   s" a target with a byte outside $21..$7E returns to /" T-LABEL
   S\" /page?x=\xC3\xA9" s" user-9" SIGNED 302 T=
   LOCATION$ s" /" URL T$=
   s" the longest target whose state fits HEAD-CAP 8 / returns to itself"
   T-LABEL
   EDGE-LEN EDGE$ s" user-9" SIGNED 302 T=
   LOCATION$ EDGE-LEN EDGE$ URL T$=
   s" a target one byte longer is sent as / and returns to /" T-LABEL
   EDGE-LEN 1+ EDGE$ s" user-9" SIGNED 302 T=
   STATE-U @ SECRET-LEN 3 + T=  STATE$ s" Lw" STR:ENDS-WITH? TTRUE
   LOCATION$ s" /" URL T$= ;

\ The kept callback URL, whose state comes last, with the state's text after
\ the challenge and `.` replaced by the base64url text.
: RETARGETED ( str -- )
   {: tail:str :}
   CALLBACK$ {: u:str :}
   u s" &state=" AFTER SECRET-LEN + 1+ {: at:n :}
   TEXT @ 0 VEC:RESIZE
   u STR:DATA at >str TEXT @ VEC:APPEND  tail TEXT @ VEC:APPEND
   TEXT @ VEC:BYTES CALLBACK-URL COPY CALLBACK-U ! ;

\ The callback of a fresh attempt with its state retargeted: its status.
: RETARGET-CALLED ( str -- n )
   {: tail:str :}
   s" /page" ATTEMPTED  ATTEMPT-VALUE {: value:str :}
   s" user-9" AUTHORIZED
   tail RETARGETED
   value CALLED ;

\ A target's base64url making a state longer than an eighth of a request
\ head: `L2Fh` (`/aa`), then `YWFh`s (`aaa`).
HTTP:HEAD-CAP 8 / SECRET-LEN - 1- 4 / 1+ 4 * constant OVERLONG-LEN
OVERLONG-LEN BUFFER: OVERLONG

: OVERLONG$ ( -- str )
   s" L2Fh" OVERLONG COPY drop
   OVERLONG-LEN 4 / 1 ?do s" YWFh" OVERLONG i 4 * + COPY drop loop
   OVERLONG OVERLONG-LEN >str ;

: RETARGETS ( -- )
   s" a callback whose state's target is @evil.example/ returns to /" T-LABEL
   s" QGV2aWwuZXhhbXBsZS8" RETARGET-CALLED 302 T=
   LOCATION$ s" /" URL T$=
   s" a callback whose state passes HEAD-CAP 8 / returns to /" T-LABEL
   OVERLONG$ RETARGET-CALLED 302 T=
   LOCATION$ s" /" URL T$= ;

\ The callback URL of the attempt of state STATE$ with a code the issuer
\ never made, kept in CALLBACK-URL.
: FORGED ( -- )
   TEXT @ 0 VEC:RESIZE
   s" /callback?code=forged&state=" TEXT @ VEC:APPEND
   STATE$ TEXT @ VEC:APPEND
   TEXT @ VEC:BYTES URL CALLBACK-URL COPY CALLBACK-U ! ;

: UNREDEEMED ( -- )
   s" a code the issuer never made is 400 sign_in_failed, the cookie cleared"
   T-LABEL
   s" /page" ATTEMPTED  ATTEMPT-VALUE {: value:str :}
   FORGED
   value CALLED 400 s" sign_in_failed" ANSWERED-AS
   SIGNIN-NAME SET$ CLEARED$ T$= ;

: ENDPOINT-AT ( str -- ) s" app" s" s3cr&t+1" BEARER:CLIENT! ;

: UNREACHED ( -- )
   s" an unreachable token endpoint is 503, the attempt kept" T-LABEL
   s" http://127.0.0.1:1/token" ENDPOINT-AT
   s" /page" ATTEMPTED  ATTEMPT-VALUE {: value:str :}
   s" user-9" AUTHORIZED
   value CALLED 503 T=
   SIGNIN-NAME SET$ STR:LEN 0 T=
   s" the kept attempt signs in once the endpoint is back" T-LABEL
   ENDPOINT$ ENDPOINT-AT
   value CALLED 302 T=
   SIGNIN-NAME SET$ CLEARED$ T$= ;

\ The body's token's exp claim, -1 without one.
: EXP ( -- n )
   TOKEN-CLAIMS CLAIMS CLAIMS-U @ >str {: t:str :}
   READER t JSON:READ-INIT JSON:READ-NEXT drop
   s" exp" JSON:READ-FIND-KEY 0= if JSON:READ-CLOSE -1 exit then
   JSON:READ-TOKEN JSON:T-INT <> if JSON:READ-CLOSE -1 exit then
   JSON:READ-INT {: v:n :} JSON:READ-CLOSE v ;

\ A sign-in whose token lives 30 seconds, stale at once as 30 less LEEWAY-S
\ is below 0, its session kept in SESSION-ID; tokens minted after it live 30
\ seconds until LIFE! sets another.
: STALE-SIGNED ( -- )
   30 TEST-ISSUER:LIFE!
   s" /page" s" user-9" SIGNED 302 T=
   s" __Host-session" SET$ STR:DATA SESSION-ID SECRET-LEN BYTE-COPY ;

: RENEWED ( -- )
   s" a stale session is renewed by its refresh token" T-LABEL
   STALE-SIGNED 3600 TEST-ISSUER:LIFE!
   s" /api" ASKED 200 T=
   EXP epoch-seconds - LEEWAY-S > TTRUE
   KEPT @ 0 VEC:RESIZE  BODY$ KEPT @ VEC:APPEND
   s" the renewed session is fresh: its token is answered again" T-LABEL
   s" /api" ASKED 200 T=
   BODY$ KEPT @ VEC:BYTES T$= ;

: UNRENEWED ( -- )
   s" a renewal that cannot reach the issuer is 503, the session kept" T-LABEL
   STALE-SIGNED 3600 TEST-ISSUER:LIFE!
   s" http://127.0.0.1:1/token" ENDPOINT-AT
   s" /api" ASKED 503 T=
   s" /page" ASKED 503 T=
   s" the kept session is renewed once the endpoint is back" T-LABEL
   ENDPOINT$ ENDPOINT-AT
   s" /api" ASKED 200 T= ;

: REVOKED ( -- )
   s" a refresh the issuer rejects drops the session: 401 signed_out" T-LABEL
   STALE-SIGNED
   ENDPOINT$ s" app" s" wrong" BEARER:CLIENT!
   s" /api" ASKED 401 s" signed_out" ANSWERED-AS
   s" the dropped session stays signed out with the client restored" T-LABEL
   ENDPOINT$ ENDPOINT-AT
   s" /api" ASKED 401 s" signed_out" ANSWERED-AS
   3600 TEST-ISSUER:LIFE! ;

\ The page signed in, its session's id kept in the row.
: SESSION-KEPT ( ^u8 -- )
   {: row:^ :}
   s" /page" s" user-9" SIGNED 302 T=
   s" __Host-session" SET$ STR:DATA row SECRET-LEN BYTE-COPY ;

\ The path asked with the cookie of the session ENDED ends.
: ENDING-ASKED ( str -- n )
   URL OPEN-AT s" __Host-session" ENDING$ COOKIE ADD FINISH ;

: ENDED ( -- )
   s" END drops the session and clears its cookie" T-LABEL
   ENDING SESSION-KEPT
   s" /signout" ENDING-ASKED 204 T=
   s" __Host-session" SET$ CLEARED$ T$=
   s" the ended session's cookie is 401 signed_out" T-LABEL
   s" /api" ENDING-ASKED 401 s" signed_out" ANSWERED-AS
   s" END with no cookie clears the cookie" T-LABEL
   s" /signout" GET 204 T=
   s" __Host-session" SET$ CLEARED$ T$= ;

: ISS-AS ( str -- ) s" api://up" KEYS-URL$ BEARER:ISSUER! ;

\ ISSUER! is rejected while HTTP serves.
: MISSTATED ( -- )
   s" an ID token of another iss is 502, the cookie cleared" T-LABEL
   s" http://127.0.0.1/elsewhere" ISS-AS
   PORT-KEPT @ SERVE-ON
   s" /page" s" user-9" SIGNED 502 T=
   SIGNIN-NAME SET$ CLEARED$ T$=
   HTTP:STOP
   ISSUER$ ISS-AS ;

: ROUTES ( -- )
   TEST-ISSUER:ROUTES
   s" GET" s" /api" [: API-ROUTE ;] HTTP:ROUTE
   s" GET" s" /page" [: PAGE-ROUTE ;] HTTP:ROUTE
   s" GET" s" /signout" [: SIGNOUT-ROUTE ;] HTTP:ROUTE ;

: SECOND-START ( -- )
   s" a second START on the redirect path is E-ROUTE" T-LABEL
   WRONG-RESET  s" api://up/write" WRONG WEB--SESSION-CONFIG:SCOPE !
   [: WRONG-START ;] HTTP:E-ROUTE TTHROWSQ
   PORT-KEPT @ SERVE-ON
   s" the first START's session and config stay in force" T-LABEL
   s" /api" ASKED 200 T=
   s" /page" ATTEMPTED  LOCATION$ s" L3BhZ2U" AUTHORIZE-SENT T$=
   HTTP:STOP ;

: RESTARTED ( -- )
   s" after ROUTES-RESET a START routes the callback again" T-LABEL
   HTTP:ROUTES-RESET  ROUTES
   [: STARTED ;] catch 0 T=
   PORT-KEPT @ SERVE-ON
   s" /page" s" user-9" SIGNED 302 T=
   HTTP:STOP ;

: IDLED ( -- )
   s" a session unused longer than idle is 401 signed_out" T-LABEL
   HTTP:ROUTES-RESET  ROUTES
   1 >MS STARTED-IDLE
   PORT-KEPT @ SERVE-ON
   SESSION-ID SESSION-KEPT
   mono-ns 1000000 + {: past:n :} begin mono-ns past > until
   s" /api" ASKED 401 s" signed_out" ANSWERED-AS
   HTTP:STOP ;

\ ISSUER! names this server's URL and is rejected while HTTP serves, so the
\ server starts once to take a port and serves again on it. SECOND-START
\ asks with the session SESSION-ID keeps, which UNRENEWED leaves held.
: SERVE-CASES ( -- )
   AIO:START LOG-INIT
   NEW-VEC BODY !  NEW-VEC HEADS !  NEW-VEC KEPT !  NEW-VEC TEXT !
   HTTP:MAX-WORKERS 0 ?do NEW-VEC i GIVEN ! loop
   s" app" s" s3cr&t+1" TEST-ISSUER:CLIENT+
   HTTP:ROUTES-RESET  ROUTES
   0 SERVE-ON
   ADDRESSES
   HTTP:STOP
   UNSTATED
   ISSUER$ s" api://up" KEYS-URL$ BEARER:ISSUER!
   ENDPOINT$ s" app" s" s3cr&t+1" BEARER:CLIENT!
   SETTINGS MARKED
   STARTED
   PORT-KEPT @ SERVE-ON
   s" START while serving is E-HTTP-STATE" T-LABEL
   [: SIGN-IN WEB-SESSION:START ;] E-HTTP-STATE TTHROWSQ
   ATTEMPTS CROWDED SIGN-INS SIGNED-OUT ORIGINS UNPROVEN DECLINED TARGETS
   RETARGETS UNREDEEMED UNREACHED RENEWED REVOKED UNRENEWED ENDED
   HTTP:STOP
   MISSTATED SECOND-START RESTARTED IDLED
   AIO:STOP ;

T-RESET
SERVE-CASES
T-REPORT

;package
