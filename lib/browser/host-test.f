\ host-test.f - BROWSER-HOST:ROUTES answered over a real loopback port. CURL
\ fetches /, /host.js and /turn.js from Habu's HTTP server; each is answered
\ 200 with its content type, and its body is the bytes of its file in
\ lib/browser, read here from the tree's root.
\ Run: bin/hb --load lib/browser/host-test.f

require lib/test.f
require lib/string.f
require lib/fmt.f
require lib/fs.f
require lib/aio.f
require lib/net/curl.f
require lib/net/http.f
require lib/browser/host.f

package BROWSER-HOST-TEST
private

$7F000001 constant LOOPBACK
1 constant ONE-WORKER
500 constant IDLE-MS
$10000 constant CAP                    \ room for any of the page's files
4096 constant HEAD-CAP
CAP BUFFER: BODY
CAP BUFFER: WANT
HEAD-CAP BUFFER: HEAD
variable BODY-U
variable HEAD-U

: OK ( CURL:status -- )
   MATCH CURL:status
      ok OF true ENDOF
      failed OF drop false ENDOF
   ;MATCH TTRUE ;

: URL ( ptr u8 n -- ptr u8 n )
   {: path:ptr u:n :}
   SB-RESET  s" http://127.0.0.1:" SB-APPEND  HTTP:PORT FMT:SB-U  path u SB-APPEND
   SB$ ;

\ The response's status, its body in BODY; -1 for a body past CAP, -2 for a
\ failed transfer.
: PERFORM ( CURL:handle -- n )
   BODY CAP >LEN CURL:PERFORM
   MATCH CURL:fetch-result
      response OF {: status:CURL:http-status got:len :}
         got LEN>N BODY-U ! status CURL:HTTP-STATUS>N ENDOF
      truncated OF drop drop -1 ENDOF
      failed OF drop -2 ENDOF
   ;MATCH ;

\ The response's header block into HEAD; empty past HEAD-CAP.
: HEAD! ( CURL:handle -- )
   HEAD HEAD-CAP >LEN CURL:HEADERS
   MATCH CURL:header-result
      complete OF LEN>N HEAD-U ! ENDOF
      truncated OF drop 0 HEAD-U ! ENDOF
   ;MATCH ;

\ GET of the path: answers its status, its body in BODY and its head in HEAD.
: GET ( ptr u8 n -- n )
   {: path:ptr u:n :}
   CURL:INIT
   MATCH CURL:init-result
      ready OF ENDOF
      failed OF drop -1 CURL:>HANDLE ENDOF
   ;MATCH {: h:CURL:handle :}
   h path u URL CURL:URL! OK
   h PERFORM {: status:n :}
   0 HEAD-U !
   status 0 > if  h HEAD!  then
   h CURL:CLEANUP
   status ;

\ The path answered 200 with the content type, its body the file's bytes.
: SERVED ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: path:ptr pu:n file:ptr fu:n type:ptr tu:n :}
   file fu WANT CAP READ-ALL {: u:n :}
   path pu GET 200 T=
   SB-RESET  s" Content-Type: " SB-APPEND  type tu SB-APPEND  S\" \r\n" SB-APPEND
   HEAD HEAD-U @ SB$ CONTAINS? TTRUE
   BODY BODY-U @ WANT u T$= ;

: SERVE-CASES ( -- )
   AIO:START
   HTTP:ROUTES-RESET
   BROWSER-HOST:ROUTES
   LOOPBACK 0 ONE-WORKER IDLE-MS HTTP:START
   s" GET / answers lib/browser/index.html as text/html" T-LABEL
   s" /" s" lib/browser/index.html" s" text/html; charset=utf-8" SERVED
   s" GET /host.js answers lib/browser/host.js as text/javascript" T-LABEL
   s" /host.js" s" lib/browser/host.js" s" text/javascript; charset=utf-8" SERVED
   s" GET /turn.js answers lib/browser/turn.js as text/javascript" T-LABEL
   s" /turn.js" s" lib/browser/turn.js" s" text/javascript; charset=utf-8" SERVED
   HTTP:STOP
   AIO:STOP ;

T-RESET
SERVE-CASES
T-REPORT

;package
