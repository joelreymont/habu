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
\ spawns a victim and asks KILL-TREE to end it.
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
\
\ HOSTS. macOS only: sandbox-exec is the way here to make the kernel refuse.
\ The Linux walk reads /proc through open, whose failure the engine reports
\ with no errno (lib/process-tree.f says what it takes one to mean).
\
\ NOTHING IS LEFT. The victim is the walker's own child, so its pid is not
\ handed on before the walker reaps it. Whatever the walk did, the walker sends
\ it SIGTERM and then SIGCONT, which end it even if it was left stopped, and
\ reaps it; a victim that was not killed is then reaped as dead of SIGTERM.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/engine-candidate.f
require lib/process-tree.f

package PROC-TREE-TEST

15 constant SIGTERM
19 constant SIGCONT-MACOS
30000 constant WALKER-MS             \ one engine boot and one walk, on a loaded host
$4000 constant CAP

create OUT CAP allot
create ERR CAP allot
create SANDBOX FS-PATH-CAP allot
variable VICTIM

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

\ The walker under a profile that denies one operation. Its own report is its
\ verdict; on a failure it is printed here.
: REFUSED ( ptr u8 n ptr u8 n -- ) {: profile:ptr profileu:n label:ptr labelu:n :}
   PROC-ARGV-RESET
   s" -p" >LEN PROC-ARGV+
   profile profileu >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ >LEN PROC-ARGV+
   s" --load" >LEN PROC-ARGV+
   s" lib/process-tree-test.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   s" walker" >LEN PROC-ARGV+
   SANDBOX$ >LEN OUT CAP >LEN ERR CAP >LEN WALKER-MS >MS RUN-ARGV-CAPTURE
   CAPTURE>RC {: outn:n errn:n rc:n :}
   label labelu T-LABEL
   rc 0 T=
   rc 0<> if
      s" the walker's output:" type cr
      OUT outn type ERR errn type cr
   then ;

: CASES ( -- )
   T-RESET
   HB-TARGET-MACOS? 0= if
      s" process-tree-test: the refusal cases need macOS sandbox-exec" type cr
      exit
   then
   s" (version 1)(allow default)(deny process-info-listpids)"
   s" a refused listing throws, and the named process still dies" REFUSED
   s" (version 1)(allow default)(deny process-info-pidinfo)"
   s" a refused record throws, and the named process still dies" REFUSED ;

public

: MAIN ( -- )
   SCRIPT-ARGC 0 > if
      0 SCRIPT-ARGV$ s" walker" STR= if WALK exit then
      s" process-tree-test: unknown mode" 64 die
   then
   CASES
   T-REPORT ;

;package

PROC-TREE-TEST:MAIN
