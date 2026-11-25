\ host-test.f - BROWSER-HOST:ROUTES answered over a real loopback port. CURL
\ fetches /, /host.js, /viewport.js, /turn.js and /page.js from Habu's HTTP
\ server; each is answered 200 with its content type, and its body is the bytes
\ of its file in src/lib/browser, read here from the tree's root.
\ Run: bin/hb --load src/lib/browser/host-test.f

require src/lib/net/http-router.f
require src/lib/test/assert.f
require src/lib/test/wait.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/fs.f
require src/lib/aio.f
require src/lib/heap.f
require src/lib/vector.f
require src/lib/net/curl.f
require src/lib/net/http.f
require src/lib/browser/host.f

package BROWSER-HOST-TEST
private

$7F000001 constant LOOPBACK
1 constant ONE-WORKER
500 constant IDLE-MS
TYPED-VARIABLE BODY ^VEC:vector<u8>
TYPED-VARIABLE WANT ^VEC:vector<u8>
TYPED-VARIABLE HEAD ^VEC:vector<u8>

TYPED-VARIABLE CFG HTTP:config

: SERVER-CONFIG ( -- ^HTTP:config )
   LOOPBACK CFG HTTP-CONFIG:ADDRESS !
   0 CFG HTTP-CONFIG:PORT !
   ONE-WORKER CFG HTTP-CONFIG:WORKERS !
   IDLE-MS >MS CFG HTTP-CONFIG:IDLE !
   CFG ;

: OK ( CURL:status -- )
   MATCH CURL:status
      ok OF true ENDOF
      failed OF drop false ENDOF
   ;MATCH TTRUE ;

: URL ( str -- str )
   {: path:str :}
   SB-RESET  s" http://127.0.0.1:" SB-APPEND  HTTP:PORT FMT:SB-U  path SB-APPEND
   SB$ ;

28 constant CURLE-TIMED-OUT             \ CURLE_OPERATION_TIMEDOUT

\ The response's status, its body in BODY; -1 for a body past most bytes, -2 for a
\ failed transfer, whose libcurl code goes to stderr. These handles set no
\ wall-clock limit, so CURLE 28 is libcurl's default 300 s connect timeout, met by
\ a starved or stopped process: it throws E-PROC-TIMEOUT, a timeout and not a FAIL.
: PERFORM ( CURL:handle n -- n ) {: h:CURL:handle most:n :}
   BODY @ 0 VEC:RESIZE
   h most >LEN BODY @ CURL:PERFORM
   MATCH CURL:fetch-result
      response OF drop CURL:HTTP-STATUS>N ENDOF
      truncated OF drop drop -1 ENDOF
      failed OF {: rc:CURL:code :}
         SB-RESET
         s" host-test: libcurl code " SB-APPEND
         rc CURL:CODE>N FMT:SB-U
         S\" \n" SB-APPEND
         2 SB$ write drop
         rc CURL:CODE>N CURLE-TIMED-OUT = if
            s" host-test PERFORM" TEST-WAIT:EXPIRED
         then
         -2 ENDOF
   ;MATCH ;

\ GET of the path, taking up to most bytes of body: answers its status, its body
\ in BODY and its head in HEAD.
: GET ( str n -- n )
   {: path:str most:n :}
   CURL:INIT
   MATCH CURL:init-result
      ready OF ENDOF
      failed OF drop CURL:EMPTY-HANDLE ENDOF
   ;MATCH {: h:CURL:handle :}
   h path URL CURL:URL! OK
   h most PERFORM {: status:n :}
   HEAD @ 0 VEC:RESIZE
   status 0 > if  h HEAD @ CURL:HEADERS  then
   h CURL:CLEANUP
   status ;

\ The path answered 200 with the content type, its body the file's bytes.
: SERVED ( str str str -- )
   {: path:str file:str type:str :}
   file FS:FILE-SIZE {: size:n :}
   WANT @ size VEC:RESIZE
   file  WANT @ 0 VEC:AT  size FS:READ-ALL size T=
   path size 1 + GET 200 T=
   SB-RESET  s" Content-Type: " SB-APPEND  type SB-APPEND  S\" \r\n" SB-APPEND
   HEAD @ VEC:BYTES SB$ STR:CONTAINS? TTRUE
   BODY @ VEC:BYTES WANT @ VEC:BYTES T$= ;

: SERVE-CASES ( -- )
   AIO:START
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW BODY !
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW HEAD !
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW WANT !
   HTTP:ROUTES-RESET
   BROWSER-HOST:ROUTES
   SERVER-CONFIG HTTP:START
   s" GET / answers src/lib/browser/index.html as text/html" T-LABEL
   s" /" s" src/lib/browser/index.html" s" text/html; charset=utf-8" SERVED
   s" GET /host.js answers src/lib/browser/host.js as text/javascript" T-LABEL
   s" /host.js" s" src/lib/browser/host.js" s" text/javascript; charset=utf-8" SERVED
   s" GET /viewport.js answers src/lib/browser/viewport.js as text/javascript" T-LABEL
   s" /viewport.js" s" src/lib/browser/viewport.js" s" text/javascript; charset=utf-8" SERVED
   s" GET /turn.js answers src/lib/browser/turn.js as text/javascript" T-LABEL
   s" /turn.js" s" src/lib/browser/turn.js" s" text/javascript; charset=utf-8" SERVED
   s" GET /page.js answers src/lib/browser/page.js as text/javascript" T-LABEL
   s" /page.js" s" src/lib/browser/page.js" s" text/javascript; charset=utf-8" SERVED
   HTTP:STOP
   AIO:STOP ;

T-RESET
SERVE-CASES
T-REPORT

;package
