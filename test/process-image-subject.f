\ The process caches must be fresh in every restored generation.
require lib/process-command.f
require lib/signal.f
require lib/test.f

package PROCESS-IMAGE-SUBJECT
private

variable CAPTURE-ROWS

\ The first restored USE fixes the row count. Further captures of these same
\ registries must reuse their DATA storage and its existing declarations.
: ADDRESS-ROWS ( -- n )
   data-base SNAP-RELOC:XTCELL-N-CELL + @ ;


: CHECK-CAPTURE-ROWS ( -- )
   CAPTURE-ROWS @ 0= if ADDRESS-ROWS CAPTURE-ROWS ! then
   s" repeated capture preserves address declarations" T-LABEL
   ADDRESS-ROWS CAPTURE-ROWS @ T= ;

: CHECK-DEFAULT ( -- )
   s" HABU_IMAGE_DEFAULT" >LEN PROC-ENV-DEFAULT$? TTRUE
   LEN>N s" default-value" T$= ;

: COMMAND ( -- )
   PROC-CMD:RESET
   PROC-CMD:ENV-HERMETIC
   s" HABU_IMAGE_ENV" >LEN s" command-value" >LEN PROC-CMD:ENV+
   s" -c" >LEN PROC-CMD:ARG+
   S\" test \q$HABU_IMAGE_ENV\q = command-value && printf process-image-ok" >LEN
   PROC-CMD:ARG+
   s" /bin/sh" >LEN 10000 >MS PROC-CMD:RUN-OUTCOME
   PROC-OUTCOME>RC RC>N 0 T=
   PROC-CMD:OUT$ s" process-image-ok" T$=
   PROC-CMD:ERR$ 0 T= drop ;

: CHECK-ROWS ( -- )
   PROC-ARGV-MAX 1+ 0 ?do
      i PROC-ARGV-TABLE @ 0= TTRUE
   loop ;

public

: CLEAN ( -- )
   T-RESET
   s" restored signals are uninitialised before any descriptor access" T-LABEL
   [: SIGNAL:FD drop ;] E-SIGNAL-STATE TTHROWSQ
   [: 0 >MS SIGNAL:WAIT drop ;] E-SIGNAL-STATE TTHROWSQ
   [: SIGNAL:RELEASE ;] E-SIGNAL-STATE TTHROWSQ
   s" restored environment sizing is unmeasured" T-LABEL
   PROC-ENV-INHERITED-N @ 0 T=
   PROC-ENV-CAP-N @ 0 T=
   PROC-ENV-BUF-CAP-N @ 0 T=
   PROC-ARGV-BUF@ 0= TTRUE
   PROC-ENV-TABLE@ 0= TTRUE
   PROC-ENV-BUF@ 0= TTRUE
   PROC-ENV-DEF-TABLE@ 0= TTRUE
   PROC-ENV-DEF-BUF@ 0= TTRUE
   PROC-ARGV-N @ COUNT>N 0 T= PROC-ARGV-OFF @ OFF>N 0 T=
   PROC-ENV-N @ COUNT>N 0 T= PROC-ENV-OFF @ OFF>N 0 T=
   PROC-ENV-DEF-N @ COUNT>N 0 T= PROC-ENV-DEF-OFF @ OFF>N 0 T=
   s" HABU_IMAGE_DEFAULT" >LEN PROC-ENV-DEFAULT$? TFALSE 2drop
   CHECK-ROWS
   T-REPORT ;

: ENV-SIZE ( n -- ) {: expected:n :}
   s" environment ceiling comes from this child's envp" T-LABEL
   PROC-ENV-INHERITED expected T=
   PROC-ENV-CAP expected PROC-ENV-EXTRA + T=
   PROC-ENV-BUF-CAP PROC-ENVP-BYTES PROC-ENV-EXTRA-BYTES + T=
   T-REPORT ;

\ This path publishes a pointer row without allocating the argv byte cache.
\ Repeated preparation also checks that an already completed cleanup is inert.
: EMPTY-ARGV ( -- )
   s" /bin/sh" >LEN PROC-ARGV-PREPARE 2drop
   IMAGE-LIFECYCLE:PREPARE IMAGE-LIFECYCLE:PREPARE
   CLEAN ;

: USE ( -- )
   T-RESET
   s" restored signal initialization opens a working self-pipe" T-LABEL
   AIO:START
   SIGNAL:INIT
   SIGNAL:SIGUSR1 SIGNAL:CATCH
   getpid SIGNAL:SIGUSR1 kill 0 T=
   100 >MS SIGNAL:WAIT MATCH SIGNAL:signal-result
      signal OF SIGNAL:SIGUSR1 T= ENDOF
      timeout OF false TTRUE ENDOF
   ;MATCH
   AIO:STOP
   PROC-ENV-DEFAULT-RESET
   s" HABU_IMAGE_DEFAULT" >LEN s" default-value" >LEN PROC-ENV-DEFAULT+
   CHECK-DEFAULT
   COMMAND
   CHECK-DEFAULT
   \ Leave signals armed, active rows and all five mappings for capture to clear.
   PROC-ARGV-ENV-RESET
   s" pending-argument" >LEN PROC-ARGV+
   s" HABU_IMAGE_ENV" >LEN s" pending-value" >LEN PROC-ENV+
   PROC-ARGV-N @ COUNT>N 1 T= PROC-ENV-N @ COUNT>N 1 T=
   CHECK-CAPTURE-ROWS
   T-REPORT ;

;package
