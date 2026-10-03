\ process-tree-test.f - a walk the kernel refuses says so.
\
\     bin/hb --load lib/process-tree-test.f
\
\ KILL-TREE decides who is in a tree from what the kernel answers, so a refusal
\ must never read as "nobody there". On macOS libproc answers a refused listing
\ as an empty one and a refused record as none, and only errno tells either
\ from the truth. Measured here: a zombie or a missing pid leaves ESRCH, and a
\ sandbox that denies the call leaves EPERM. This test runs a WALKER - this
\ file again, with `walker` as its one argument - under sandbox-exec, once
\ denying process-info-listpids and once process-info-pidinfo. The walker
\ spawns a victim and asks KILL-TREE to end it. A second walker, `capture`,
\ runs under the listing denial two captures of a sleeper whose deadline
\ passes, so each capture's early end walks a tree the kernel will not list.
\
\ THE WAYS THIS CAN FAIL, written down before the fix:
\
\  1. A REFUSED LISTING READ AS NO CHILDREN. The walk goes quiet at once, kills
\     the one process it was named and returns as if that were the whole tree.
\  2. A REFUSED RECORD READ AS AN ENDED PROCESS. The named process counts as
\     settled, a live child of it as a zombie, and the walk returns quiet.
\  3. A THROW THAT LEAVES THE NAMED PROCESS BEHIND. The walk gives up before
\     it kills what it found, and the victim is left stopped.
\ Asserted by the walker in each case: KILL-TREE throws E-PROC-OUTPUT, and the
\ victim died of SIGKILL.
\  4. A REFUSED WALK THAT REPLACES A CAPTURE'S ANSWER. A capture that ends its
\     child early walks the child's tree (lib/process.f PROC-KILL-CAPTURE). A
\     walk that throws there went on through the capture's cleanup, so its
\     code replaced the capture's own: a deadline read as E-PROC-OUTPUT, and a
\     deadline read as data threw it rather than answering the timeout.
\ Asserted by the capture walker: a capture past its deadline throws
\ E-PROC-TIMEOUT, one read as data answers the timeout outcome, and each
\ sleeper died of SIGKILL and was reaped; and here, that the walker's stderr is
\ one line per capture naming the refused walk's E-PROC-OUTPUT.
\  5. A REFUSED WALK THAT STRANDS A SUPERVISED SESSION. TEARDOWN kills each of
\     a session's three pids with its tree (lib/process-pty-io.f IO-KILL-REAP),
\     and a throw out of it would strand the linear teardown token and leak
\     the slot.
\ Asserted by a third walker, `pty`, under the listing denial: TEARDOWN
\ returns and the target is reaped; and here, that its stderr is one line per
\ supervised pid naming the refused walk.
\
\ HOSTS. macOS only: sandbox-exec is the way here to make the kernel refuse.
\ The Linux walk reads /proc through open, whose failure the engine reports
\ with no errno (lib/process-tree.f says what it takes one to mean).
\
\ NOTHING IS LEFT. The victim is the walker's own child, so its pid is not
\ handed on before the walker reaps it. Whatever the walk did, the walker sends
\ it SIGTERM and then SIGCONT, which end it even if it was left stopped, and
\ reaps it; a victim that was not killed is then reaped as dead of SIGTERM. A
\ captured sleeper is the capture's to reap, and a walker stuck reaping one
\ the walk left alive is ended with it at WALKER-MS.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/test/outcome.f
require lib/process.f
require lib/process-argv.f
require lib/process-pty-io.f
require lib/process-env.f
require lib/engine-candidate.f
require lib/process-tree.f

package PROC-TREE-TEST

15 constant SIGTERM
19 constant SIGCONT-MACOS
30000 constant WALKER-MS             \ one engine boot and one walk, on a loaded host
1 constant CAPTURE-MS                \ passes long before the sleeper's 30 s
2 constant CAPTURES                  \ the capture walker's, each walking once
3 constant SUPERVISED                \ the pty walker's: target, anchor and monitor
$4000 constant CAP

create OUT CAP allot
create ERR CAP allot
create SANDBOX FS-PATH-CAP allot
variable VICTIM
variable TARGET

: KILLED-BY? ( outcome n -- bool ) {: sig:n :}
   MATCH outcome
      exited OF drop false ENDOF
      signaled OF sig = ENDOF
      timeout OF false ENDOF
   ;MATCH ;

\ ---- the walker, under the sandbox ------------------------------------------

: VICTIM-START ( -- )
   PROC-ARGV-RESET
   s" 30" >LEN PROC-ARGV+
   s" /bin/sleep" >LEN -1 >FD -1 >FD -1 >FD PROC-SPAWN-ARGV-IO PID>N VICTIM ! ;

: VICTIM-KILL-TREE ( -- )
   VICTIM @ >PID PROC-TREE:KILL-TREE ;

: WALK ( -- )
   T-RESET
   VICTIM-START
   s" refused: KILL-TREE throws E-PROC-OUTPUT" T-LABEL
   ['] VICTIM-KILL-TREE E-PROC-OUTPUT TTHROWS
   VICTIM @ >PID SIGTERM PROC-KILL-RAW drop
   VICTIM @ >PID SIGCONT-MACOS PROC-KILL-RAW drop
   s" refused: the named process died of SIGKILL" T-LABEL
   VICTIM @ >PID PROC-WAIT-OUTCOME SIGKILL KILLED-BY? TTRUE
   T-REPORT ;

\ ---- the capture walker, under the sandbox ----------------------------------
\
\ The walker's own capture buffers are free here: it captures nothing else.

: SLEEPER-ARGS ( -- )
   PROC-ARGV-RESET
   s" 30" >LEN PROC-ARGV+ ;

\ A sleeper captured past its deadline, which reaches no result.
: SLEEPER-CAPTURE ( -- )
   SLEEPER-ARGS
   s" /bin/sleep" >LEN OUT CAP >LEN ERR CAP >LEN CAPTURE-MS >MS
   RUN-ARGV-CAPTURE MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE 2drop ENDOF
     err OF PCAP-FAILED:UNMAKE drop 2drop ENDOF
   ;MATCH ;

\ The same deadline read as data.
: SLEEPER-OUTCOME ( -- )
   SLEEPER-ARGS
   s" /bin/sleep" >LEN OUT CAP >LEN ERR CAP >LEN CAPTURE-MS >MS
   RUN-ARGV-CAPTURE-OUTCOME T-OUTCOME-TIMEOUT 2drop ;

: SLEEPER-REAPED ( ptr u8 n -- )
   {: label:ptr labelu:n :}
   label labelu T-LABEL
   PROC-PID @ PROC-NO-PID T=
   PROC-STATUS @ PROC-STATUS>RC RC>N 128 SIGKILL + T= ;

: CAPTURE-WALK ( -- )
   T-RESET
   s" refused: a capture past its deadline throws E-PROC-TIMEOUT" T-LABEL
   [: SLEEPER-CAPTURE ;] E-PROC-TIMEOUT TTHROWSQ
   s" refused: that sleeper died of SIGKILL and was reaped" SLEEPER-REAPED
   s" refused: a deadline read as data answers the timeout outcome" T-LABEL
   [: SLEEPER-OUTCOME ;] 0 TTHROWSQ
   s" refused: that sleeper died of SIGKILL and was reaped" SLEEPER-REAPED
   T-REPORT ;

\ ---- the pty walker, under the sandbox --------------------------------------

: PTY-WALK ( -- )
   T-RESET
   s" /usr/bin/true" >LEN PROCESS-PTY:SPAWN
   PROCESS-PTY:LAUNCH
   PROCESS-PTY:TARGET PID>N TARGET !
   PROCESS-PTY:TEARDOWN
   s" refused: a torn-down session's target was reaped" T-LABEL
   TARGET @ 0 kill-errno ESRCH# negate T=
   T-REPORT ;

\ ---- the cases --------------------------------------------------------------

: SANDBOX$ ( -- ptr u8 n )
   s" sandbox-exec" >LEN SANDBOX FIND-EXECUTABLE MATCH option
      none OF s" process-tree-test: required executable missing on PATH: sandbox-exec" 1 die ENDOF
      some OF LEN>N ENDOF
   ;MATCH
   SANDBOX swap ;

: CAPTURE>RC ( result<pcap:captured,pcap:failed> -- n n n )   \ outn errn rc
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} o LEN>N e LEN>N 0 ENDOF
     err OF PCAP-FAILED:UNMAKE  {: o:len e:len c:rc :} o LEN>N e LEN>N c RC>N ENDOF
   ;MATCH ;

\ This file again as the walker named mode, under a profile that denies one
\ operation: its stdout and stderr lengths and its exit code.
: SANDBOXED ( ptr u8 n ptr u8 n -- n n n )
   {: profile:ptr profileu:n mode:ptr modeu:n :}
   PROC-ARGV-RESET
   s" -p" >LEN PROC-ARGV+
   profile profileu >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ >LEN PROC-ARGV+
   s" --load" >LEN PROC-ARGV+
   s" lib/process-tree-test.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   mode modeu >LEN PROC-ARGV+
   SANDBOX$ >LEN OUT CAP >LEN ERR CAP >LEN WALKER-MS >MS RUN-ARGV-CAPTURE
   CAPTURE>RC ;

\ A walker's own report is its verdict; on a failure it is printed here.
: VERDICT ( n n n ptr u8 n -- )
   {: outn:n errn:n rc:n label:ptr labelu:n :}
   label labelu T-LABEL
   rc 0 T=
   rc 0<> if
      s" the walker's output:" type cr
      OUT outn type ERR errn type cr
   then ;

: REFUSED ( ptr u8 n ptr u8 n -- )
   {: profile:ptr profileu:n label:ptr labelu:n :}
   profile profileu s" walker" SANDBOXED label labelu VERDICT ;

\ What stderr gets from lib/process.f for n walks the kernel refused.
: WALK-LINES$ ( n -- ptr u8 n ) {: walks:n :}
   SB-RESET
   walks 0 ?do
      s" process: process tree of a killed child not walked, throw " SB-APPEND
      E-PROC-OUTPUT FMT:SB-INT
      $0A SB-APPEND-C
   loop
   SB$ ;

: REFUSED-CAPTURE ( -- )
   s" (version 1)(allow default)(deny process-info-listpids)"
   s" capture" SANDBOXED {: outn:n errn:n rc:n :}
   outn errn rc
   s" a capture whose tree walk is refused keeps its own answer" VERDICT
   s" the walker's stderr names each refused walk" T-LABEL
   ERR errn CAPTURES WALK-LINES$ T$= ;

: REFUSED-PTY ( -- )
   s" (version 1)(allow default)(deny process-info-listpids)"
   s" pty" SANDBOXED {: outn:n errn:n rc:n :}
   outn errn rc
   s" a session whose tree walks are refused is torn down" VERDICT
   s" the walker's stderr names each refused walk" T-LABEL
   ERR errn SUPERVISED WALK-LINES$ T$= ;

: CASES ( -- )
   T-RESET
   HB-TARGET-MACOS? 0= if
      s" process-tree-test: the refusal cases need macOS sandbox-exec" type cr
      exit
   then
   s" (version 1)(allow default)(deny process-info-listpids)"
   s" a refused listing throws, and the named process still dies" REFUSED
   s" (version 1)(allow default)(deny process-info-pidinfo)"
   s" a refused record throws, and the named process still dies" REFUSED
   REFUSED-CAPTURE
   REFUSED-PTY ;

public

: MAIN ( -- )
   SCRIPT-ARGC 0 > if
      0 SCRIPT-ARGV$ s" walker" STR= if WALK exit then
      0 SCRIPT-ARGV$ s" capture" STR= if CAPTURE-WALK exit then
      0 SCRIPT-ARGV$ s" pty" STR= if PTY-WALK exit then
      s" process-tree-test: unknown mode" 64 die
   then
   CASES
   T-REPORT ;

;package

PROC-TREE-TEST:MAIN
