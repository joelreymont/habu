\ The process caches must be fresh in every restored generation.
require src/lib/process-command.f
require src/lib/signal.f
require src/lib/test.f
require src/lib/test/wait.f

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

: COMMAND-RUN ( CMD:command -- CMD:command )
   CMD:ENV-HERMETIC
   s" HABU_IMAGE_ENV" s" command-value" CMD:ENV+
   s" -c" CMD:ARG+
   S\" test \q$HABU_IMAGE_ENV\q = command-value && printf process-image-ok"
   CMD:ARG+
   s" /bin/sh" 10000 >MS CMD:RUN-OUTCOME
   PROC-OUTCOME>RC RC>N 0 T=
   CMD:OUT$ s" process-image-ok" T$=
   CMD:ERR$ 0 T= drop ;

: COMMAND ( -- ) [: COMMAND-RUN ;] CMD:WITH-COMMAND ;

\ A row staged here lands in storage this process mapped: the saved process's
\ table and row bytes were released at capture.
: CHECK-ROWS ( -- )
   s" restored argv storage takes a row" T-LABEL
   PROC-ARGV-RESET
   s" restored" >LEN PROC-ARGV+
   1 >IDX PROC-ARGV-SLOT @ 8 s" restored" T$=
   PROC-ARGV-RESET
   s" restored environment storage takes a row and a default" T-LABEL
   PROC-ENV-RESET PROC-ENV-DEFAULT-RESET
   s" HABU_RESTORED" >LEN s" x" >LEN PROC-ENV+
   0 >IDX PROC-ENV-SLOT @ 15 s" HABU_RESTORED=x" T$=
   s" HABU_RESTORED" >LEN s" y" >LEN PROC-ENV-DEFAULT+
   0 >IDX PROC-ENV-DEF-SLOT @ 15 s" HABU_RESTORED=y" T$=
   PROC-ENV-RESET PROC-ENV-DEFAULT-RESET ;

public

: CLEAN ( -- )
   T-RESET
   s" restored signals are uninitialised before any descriptor access" T-LABEL
   [: SIGNAL:FD drop ;] E-SIGNAL-STATE TTHROWSQ
   [: 0 >MS SIGNAL:WAIT drop ;] E-SIGNAL-STATE TTHROWSQ
   [: SIGNAL:RELEASE ;] E-SIGNAL-STATE TTHROWSQ
   s" restored process rows are released" T-LABEL
   PROC-ARGV-N @ COUNT>N 0 T=
   PROC-ENV-N @ COUNT>N 0 T=
   PROC-ENV-DEF-N @ COUNT>N 0 T=
   s" HABU_IMAGE_DEFAULT" >LEN PROC-ENV-DEFAULT$? TFALSE 2drop
   CHECK-ROWS
   T-REPORT ;

\ No descriptor comes through an image: the image was saved with the signal
\ self-pipe open, and a restored process still hands the first pipe it opens the
\ lowest number past the standard three. Run before USE, whose AIO:START opens a
\ descriptor of its own.
: FIRST-FD ( -- )
   T-RESET
   s" a restored process holds no descriptor past the standard three" T-LABEL
   SIGNAL:INIT
   SIGNAL:FD FD>N 3 T=
   SIGNAL:RELEASE
   T-REPORT ;

\ This path publishes a pointer row without allocating argv row bytes.
\ Repeated preparation also checks that an already completed cleanup is inert.
: EMPTY-ARGV ( -- )
   s" /bin/sh" >LEN PROC-ARGV-PREPARE 2drop
   IMAGE-LIFECYCLE:PREPARE IMAGE-LIFECYCLE:PREPARE
   CLEAN ;

private

: USE-CASES ( -- )
   T-RESET
   s" restored signal initialization opens a working self-pipe" T-LABEL
   AIO:START
   SIGNAL:INIT
   SIGNAL:SIGUSR1 SIGNAL:CATCH
   getpid SIGNAL:SIGUSR1 kill 0 T=
   100 >MS SIGNAL:WAIT MATCH SIGNAL:signal-result
      signal OF SIGNAL:SIGUSR1 T= ENDOF
      timeout OF s" process-image-subject USE-CASES" TEST-WAIT:EXPIRED ENDOF
   ;MATCH
   AIO:STOP
   PROC-ENV-DEFAULT-RESET
   s" HABU_IMAGE_DEFAULT" >LEN s" default-value" >LEN PROC-ENV-DEFAULT+
   CHECK-DEFAULT
   COMMAND
   CHECK-DEFAULT
   \ Leave signals armed, active rows and every mapping for capture to clear.
   PROC-ARGV-ENV-RESET
   s" pending-argument" >LEN PROC-ARGV+
   s" HABU_IMAGE_ENV" >LEN s" pending-value" >LEN PROC-ENV+
   PROC-ARGV-N @ COUNT>N 1 T= PROC-ENV-N @ COUNT>N 1 T=
   CHECK-CAPTURE-ROWS
   T-REPORT ;

public

\ The signal wait and COMMAND's capture each have a deadline, and a throw code
\ cannot cross the process boundary: this child exits PROC-TIMEOUT-RC for
\ E-PROC-TIMEOUT so test/process-image.f throws it again (src/lib/process.f).
: USE ( -- )
   [: USE-CASES ;] catch {: code:n :}
   code E-PROC-TIMEOUT = if
      s" process-image: a deadline expired in the restored child" PROC-TIMEOUT-RC die
   then
   code 0<> if code throw then ;

;package
