\ gate-common-test.f - the capture checks of test/gate-common-lib.f, through the
\ real load path. GE-CAPTURE-ACTION runs in process.

require src/lib/errors.f
require src/lib/string.f
require src/lib/span.f
require src/lib/process.f
require src/lib/process-fork.f
require src/lib/test.f
require test/gate-common.f

package GATE-COMMON-TEST

\ Past a pipe's 64 KiB and GT-OUT-CAP alike: refused as truncated, not blocked.
\ A throwing action keeps its code and output; neither leaves a file in the
\ gate root.
70000 constant BIG-BYTES

variable ROOT-FILES

: BIG ( -- )
   BIG-BYTES 0 ?do s" x" type loop ;

: FAILING ( -- )
   s" out" type
   2 s" err" GT-WRITE-FD
   7 throw ;

: ROOT-FILE+ ( ptr u8 n -- )
   2drop ROOT-FILES @ 1+ ROOT-FILES ! ;

: ROOT-FILES# ( -- n )
   0 ROOT-FILES !
   GT-ROOT [: ROOT-FILE+ ;] FS:WALK-FILES
   ROOT-FILES @ ;

: BIG-CAPTURE ( -- )
   [: BIG ;] GE-CAPTURE-ACTION ;

: OVER-CAP ( -- )
   s" capture past GT-OUT-CAP" T-LABEL
   [: BIG-CAPTURE ;] E-PROC-TRUNCATED TTHROWSQ
   s" capture past GT-OUT-CAP leaves no file" T-LABEL
   ROOT-FILES# 0 T= ;

: THROWN ( -- )
   [: FAILING ;] GE-CAPTURE-ACTION
   s" throwing action code" T-LABEL
   GT-RC@ 7 T=
   s" throwing action stdout" T-LABEL
   GT-OUT$ s" out" T$=
   s" throwing action stderr" T-LABEL
   GT-ERR$ s" err" T$=
   s" throwing action leaves no file" T-LABEL
   ROOT-FILES# 0 T= ;

\ ---- what a child spawned in a capture inherits ------------------------------
\ A child engine prints every descriptor from 3 up open in it. Spawned outside a
\ capture and from an action inside one it prints the same lines: the streams a
\ capture saves and the files it reads back close on exec.
256 constant FDS-CAP

create FDS-BEFORE FDS-CAP allot
create FDS-INSIDE FDS-CAP allot
variable FDS-BEFORE-U
variable FDS-INSIDE-U
variable FDS-INSIDE-RC

: FDS-SRC ( -- ptr u8 n )
   s" 1 constant F-GETFD  : FDS ( -- ) 256 3 ?do i F-GETFD 0 fcntl 0 >= if i . then loop ;  FDS" ;

: FDS-CHILD ( -- )
   GE-HB-RESET
   GE-HB$ FDS-SRC GE-TIMEOUT-MS GE-RUN-STDIN ;

: FDS-KEEP ( ptr u8 ptr n -- ) {: dst:ptr up:ptr :}
   GT-OUT$ {: a:ptr u:n :}
   u FDS-CAP > if E-STR-CAPACITY throw then
   a dst u BYTE-COPY
   u up ! ;

: FDS-SPAWN ( -- )
   FDS-CHILD
   GT-RC@ FDS-INSIDE-RC !
   FDS-INSIDE FDS-INSIDE-U FDS-KEEP ;

: CLOEXEC ( -- )
   FDS-CHILD
   s" descriptor child outside a capture" T-LABEL
   GT-RC@ 0 T=
   FDS-BEFORE FDS-BEFORE-U FDS-KEEP
   [: FDS-SPAWN ;] GE-CAPTURE-ACTION
   s" capture spawning a descriptor child" T-LABEL
   GT-RC@ 0 T=
   s" descriptor child inside a capture" T-LABEL
   FDS-INSIDE-RC @ 0 T=
   s" a child spawned in a capture inherits no held descriptor" T-LABEL
   FDS-INSIDE FDS-INSIDE-U @ FDS-BEFORE FDS-BEFORE-U @ T$= ;

\ ---- output a child the action leaves running writes late --------------------
\ The action fills LATE-OUT, forks a writer adding four bytes to the captured
\ stdout and waits LATE-WAIT-NS, a microsecond more each trial, so the bytes
\ land before, during or after the read. A capture reads the action's own bytes
\ or throws E-PROC-TRUNCATED. Checking the size before the read (FS:FILE-SIZE
\ then FS:READ-ALL) ended about one trial in four with E-FS-CAPACITY.
8 constant LATE-CAP
64 constant LATE-TRIALS
1000 constant LATE-STEP-NS

LATE-CAP SPAN-BUFFER: LATE-OUT
LATE-CAP SPAN-BUFFER: LATE-ERR

variable LATE-WAIT-NS
variable LATE-PID                        \ the trial's writer, 0 before its fork
variable LATE-ODD                        \ the first code no capture may end with

: LATE-WRITER ( -- )
   s" late" type
   s" " 0 die ;

: LATE-SPIN ( n -- ) {: ns:n :}
   mono-ns ns + begin mono-ns over >= until drop ;

: LATE-ACTION ( -- )
   LATE-CAP 0 ?do s" x" type loop
   PROC-FORK:CHECKED {: pid:pid :}
   pid PID>N 0= if LATE-WRITER then
   pid PID>N LATE-PID !
   LATE-WAIT-NS @ LATE-SPIN ;

\ The action's own code is thrown too: it is as odd as a wrong read.
: LATE-CAPTURE ( -- )
   [: LATE-ACTION ;] GT-ROOT LATE-OUT LATE-ERR GT-CAPTURE-ACTION
   nip nip dup 0 <> if throw then drop ;

: LATE-REAP ( -- )
   LATE-PID @ 0 > if LATE-PID @ >PID PROC-WAIT-OUTCOME drop then ;

: LATE-NOTE ( n -- ) {: code:n :}
   code 0= if exit then
   code E-PROC-TRUNCATED = if exit then
   LATE-ODD @ 0= if code LATE-ODD ! then ;

: LATE-TRIAL ( n -- ) {: wait:n :}
   wait LATE-WAIT-NS !
   0 LATE-PID !
   [: LATE-CAPTURE ;] catch {: code:n :}
   LATE-REAP
   code LATE-NOTE ;

: LATE ( -- )
   0 LATE-ODD !
   LATE-TRIALS 0 ?do i LATE-STEP-NS * LATE-TRIAL loop
   s" late output is read or truncated" T-LABEL
   LATE-ODD @ 0 T=
   s" late output leaves no file" T-LABEL
   ROOT-FILES# 0 T= ;

\ ---- the nesting bound -------------------------------------------------------
\ GT-CAP-NEST-MAX captures may be open at once: the next is refused with
\ E-TBL-BOUNDS before it takes a frame; each capture around it answers that code
\ as its action's and closes as usual, so NESTED after it finds every frame
\ free.
32 SPAN-BUFFER: DIVE-OUT
32 SPAN-BUFFER: DIVE-ERR

variable DIVE-LEFT                       \ captures still to open

defer DIVE ( -- )

: DIVE-IN ( -- )
   DIVE-LEFT @ 1- DIVE-LEFT !
   [: DIVE ;] GT-ROOT DIVE-OUT DIVE-ERR GT-CAPTURE-ACTION
   nip nip throw ;

: BOUND ( -- )
   [: DIVE-IN ;] is DIVE
   GT-CAP-NEST-MAX 1+ DIVE-LEFT !
   s" a capture past GT-CAP-NEST-MAX" T-LABEL
   [: DIVE ;] E-TBL-BOUNDS TTHROWSQ
   s" the capture refused is the one past GT-CAP-NEST-MAX" T-LABEL
   DIVE-LEFT @ 0 T= ;

\ ---- a capture nested in another's action ------------------------------------
\ The inner capture reads what the action wrote while it ran; the outer one
\ reads the rest, from before and after the inner capture.
64 SPAN-BUFFER: INNER-OUT
64 SPAN-BUFFER: INNER-ERR

variable INNER-OUT-U
variable INNER-ERR-U
variable INNER-CODE

: INNER ( -- )
   s" inner-out" type
   2 s" inner-err" GT-WRITE-FD ;

: OUTER ( -- )
   s" outer-out-1 " type
   2 s" outer-err-1 " GT-WRITE-FD
   [: INNER ;] GT-ROOT INNER-OUT INNER-ERR GT-CAPTURE-ACTION
   INNER-CODE !
   LEN>N INNER-ERR-U !
   LEN>N INNER-OUT-U !
   s" outer-out-2" type
   2 s" outer-err-2" GT-WRITE-FD ;

: NESTED ( -- )
   [: OUTER ;] GE-CAPTURE-ACTION
   s" nested outer code" T-LABEL
   GT-RC@ 0 T=
   s" nested outer stdout" T-LABEL
   GT-OUT$ s" outer-out-1 outer-out-2" T$=
   s" nested outer stderr" T-LABEL
   GT-ERR$ s" outer-err-1 outer-err-2" T$=
   s" nested inner code" T-LABEL
   INNER-CODE @ 0 T=
   s" nested inner stdout" T-LABEL
   INNER-OUT SPAN:$ drop INNER-OUT-U @ s" inner-out" T$=
   s" nested inner stderr" T-LABEL
   INNER-ERR SPAN:$ drop INNER-ERR-U @ s" inner-err" T$=
   s" nested capture leaves no file" T-LABEL
   ROOT-FILES# 0 T= ;

public

: MAIN ( -- )
   T-RESET
   s" hb-gate-common" GT-START
   THROWN
   OVER-CAP
   CLOEXEC
   LATE
   BOUND
   NESTED
   GT-CLEANUP
   T-REPORT ;

;package

GATE-COMMON-TEST:MAIN
