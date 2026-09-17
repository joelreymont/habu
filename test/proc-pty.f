\ proc-pty.f — focused native process/PTY harness. Run with:
\   bin/hb --load lib/errors.f lib/process.f test/proc-pty.f
\ The engine bakes neither POLL-IN nor the process primitives below them, so the
\ two library files are part of the invocation; test/runtime-regression-test.f
\ (GE-PROCESS-PTY) drives exactly this line, and the engine to test can follow
\ as `-- <path>` or in HABU_UNDER_TEST.

package PROC-PTY

$20007454 constant TIOCPTYGRANT
$40807453 constant TIOCPTYGNAME
$20007452 constant TIOCPTYUNLK
$40045431 constant LINUX-TIOCSPTLCK
$80045430 constant LINUX-TIOCGPTN
2 constant PTY-OPEN-RDWR
10 constant PTY-POLL-MS
$4E20 constant PTY-WAIT-MS         \ one wait runs at most this long on the clock
1 constant PTY-QUIET-POLLS
$1000 constant PTY-RBUF-CAP
$80 constant PTY-KEEP-TAIL         \ bytes kept when a full buffer is compacted

create RBUF PTY-RBUF-CAP allot
create NL 1 allot
create EOT 1 allot
create CH 1 allot
create PTYNAME 128 allot

variable #FAIL
variable #CASE
variable RN
variable WEND                      \ end of the window an absence claim reads; -1 with no barrier
variable QUIET
variable IN-R
variable IN-W
variable OUT-R
variable OUT-W
variable ERR-R
variable ERR-W
variable PID
variable MFD
variable SFD
variable PTY-U
variable PTYNUM

: HB-ARG? ( -- bool )
   SCRIPT-ARGC 0 > ;

: HB-EXE$ ( -- ptr u8 n )
   HB-ARG? if 0 SCRIPT-ARGV$ exit then
   s" HABU_UNDER_TEST" GETENV dup 0= if
      2drop s" bin/hb" exit
   then ;

: RBUF-DUMP ( -- )
   s" rbuf bytes: " type RN @ . cr
   RN @ 0 > if
      s" rbuf:" type cr
      RBUF RN @ type cr
   then ;

: T-FAIL ( -- )
   [char] F emit #CASE @ . cr
   RBUF-DUMP
   #FAIL @ 1 + #FAIL ! ;

: T= {: got want :} ( n n -- )
   #CASE @ 1 + #CASE !
   got want <> if T-FAIL then ;

: TTRUE ( bool -- )
   #CASE @ 1 + #CASE !
   0= if T-FAIL then ;

\ Clearing the buffer retires the barrier with it: the bytes a claim would have
\ read are gone.
: RCLR ( -- )
   0 RN !
   -1 WEND ! ;

\ A full buffer keeps its tail rather than dropping everything: a marker split
\ across the compaction survives whole in what is kept, and every older byte has
\ already been searched. PTY-KEEP-TAIL is far longer than any marker below.
: KEEP-TAIL! ( -- )
   RN @ PTY-RBUF-CAP PTY-KEEP-TAIL - < if exit then
   RBUF RN @ PTY-KEEP-TAIL - + RBUF PTY-KEEP-TAIL BYTE-COPY
   PTY-KEEP-TAIL RN !
   -1 WEND ! ;

\ Append one read and report it, keeping room for the next one.
: READ+ {: fd :} ( fd -- n )
   KEEP-TAIL!
   fd FD>N RBUF RN @ + PTY-RBUF-CAP RN @ - read {: got :}
   got 0 > if RN @ got + RN ! then
   got ;

\ A pipe hands over what the writer has flushed, not what it will write. Read to
\ the far end's close, so a child that answers in pieces is never truncated into
\ a wrong verdict by one short read.
: READ-ALL {: fd :} ( fd -- )
   begin fd READ+ 0 > while repeat ;

\ One poll and, when the descriptor has bytes, one read: above zero for bytes
\ appended, 0 for a quiet poll, below zero once the far side is gone -- a ready
\ descriptor that reads nothing is the hang-up.
: READ-STEP {: fd :} ( fd -- n )
   fd PTY-POLL-MS >MS POLL-IN COUNT>N 0 = if 0 exit then
   fd READ+ 0 > if 1 exit then
   -1 ;

: MATCH-AT {: ha:ptr na:ptr nu off :} ( ptr u8 ptr u8 n n -- bool )
   0 0=
   nu 0 ?do
      ha off + i + c@  na i + c@  <> if drop 0 0= 0= leave then
   loop ;

: CONTAINS? {: ha:ptr hu na:ptr nu :} ( ptr u8 n ptr u8 n -- bool )
   nu 0= if 0 0= exit then
   hu nu < if 0 0= 0= exit then
   hu nu - 1 + 0 ?do
      ha na nu i MATCH-AT if 0 0= unloop exit then
   loop
   0 0= 0= ;

\ Offset of the first occurrence at or after `from`, or -1. Where a barrier
\ lands is what bounds the window an absence claim may read.
: FIND-AT {: from a:ptr u :} ( n ptr u8 n -- n )
   RN @ u - from < if -1 exit then
   RN @ u - 1 + from ?do
      RBUF a u i MATCH-AT if i unloop exit then
   loop
   -1 ;

: TCONTAINS {: a:ptr u :} ( ptr u8 n -- )
   RBUF RN @ a u CONTAINS? TTRUE ;

: FD-WRITE {: fd a:ptr u :} ( fd ptr u8 n -- )
   fd FD>N a u write u T= ;

: FD-WRITE-LN {: fd a:ptr u :} ( fd ptr u8 n -- )
   fd a u FD-WRITE
   fd NL 1 FD-WRITE ;

\ Read until PTY-QUIET-POLLS polls in a row bring nothing, the far side hangs
\ up, or the deadline passes. The deadline is absolute and is tested before each
\ poll, so the drain overruns it by at most the one poll already in flight.
: DRAIN {: fd :} ( fd -- )
   RCLR
   0 QUIET !
   PTY-WAIT-MS >MS PROC-DEADLINE-AT {: deadline :}
   begin QUIET @ PTY-QUIET-POLLS <  deadline PROC-LEFT-MS MS>N 0 >  and while
      fd READ-STEP {: n :}
      n 0 < if exit then
      n 0 > if 0 QUIET ! else QUIET @ 1 + QUIET ! then
   repeat ;

: MFD-DRAIN ( -- )
   MFD @ >FD DRAIN ;

: RBUF-HAS? {: a:ptr u :} ( ptr u8 n -- bool )
   RBUF RN @ a u CONTAINS? ;

\ Keep reading the master until the text appears, the child hangs up, or the
\ wait's deadline passes. A child engine answers in as many pieces as the host's
\ scheduling chooses -- its line editor redraws on every keystroke, and a loaded
\ box splits one echo across dozens of reads -- so no count of polls bounds the
\ wait. A partial read is not an answer and not a failure: only the text, the
\ hang-up and the clock end the loop.
: EXPECT-WAIT? {: a:ptr u :} ( ptr u8 n -- bool )
   PTY-WAIT-MS >MS PROC-DEADLINE-AT {: deadline :}
   begin
      a u RBUF-HAS? if 0 0= exit then
      deadline PROC-LEFT-MS MS>N 0 = if 0 0= 0= exit then
      MFD @ >FD READ-STEP 0 < if a u RBUF-HAS? exit then
   again ;

: SEND-C ( n -- ) {: c:n :}
   c CH c!
   MFD @ >FD CH 1 FD-WRITE ;

: SEND-S {: a:ptr u :} ( ptr u8 n -- )
   MFD @ >FD a u FD-WRITE ;

: SEND-LN {: a:ptr u :} ( ptr u8 n -- )
   MFD @ >FD a u FD-WRITE-LN ;

: SEND-ESC ( n -- )
   27 SEND-C
   91 SEND-C
   SEND-C ;

: STEP-LN {: a:ptr u :} ( ptr u8 n -- )
   a u SEND-LN
   MFD-DRAIN ;

: STEP-S {: a:ptr u :} ( ptr u8 n -- )
   a u SEND-S
   MFD-DRAIN ;

: EXPECT ( ptr u8 n -- )
   EXPECT-WAIT? TTRUE ;

\ Wait for a marker and close an absence claim's window at its end. The marker
\ has to be one the child prints PAST the point where the rejected text could
\ have appeared; a failed wait leaves no window, so the claim behind it is
\ refused as well.
: BARRIER {: a:ptr u :} ( ptr u8 n -- )
   a u EXPECT
   0 a u FIND-AT dup 0 < if drop exit then
   u + WEND ! ;

\ The barrier for a claim about the line before it: the child prints the hex
\ literal in decimal, so the answer's digits are in nothing the line editor
\ echoed back and only the evaluation can satisfy the wait. The REPL reads the
\ next line after finishing the one before it, so this answer proves that line
\ is over -- diagnostics, trailer and all.
: PROBE-BARRIER ( -- )
   s" $BEEF ." SEND-LN
   s" 48879" BARRIER ;

\ An absence claim reads the window a barrier closed, never the whole buffer:
\ RBUF[0,WEND) ends at a marker the child printed past the point where the
\ rejected text would have been, so the claim reads the child's answer instead
\ of a buffer that is merely still empty. With no barrier there is no window
\ and the claim is refused, not granted.
: REJECT? {: a:ptr u :} ( ptr u8 n -- bool )
   WEND @ 0 < if false exit then
   RBUF WEND @ a u CONTAINS? 0= ;

: REJECT ( ptr u8 n -- )
   REJECT? TTRUE ;

: EXPECT-OK ( -- )
   s"  ok" EXPECT ;

: EXPECT-PROMPT ( -- )
   s" habu> " EXPECT ;

: CAPTURE-PIPES ( -- )
   PIPE-PAIR IN-W ! IN-R !
   PIPE-PAIR OUT-W ! OUT-R !
   PIPE-PAIR ERR-W ! ERR-R !
   IN-W @ >FD FD-CLOEXEC!
   OUT-R @ >FD FD-CLOEXEC!
   ERR-R @ >FD FD-CLOEXEC! ;

: CAPTURE-START-HB ( -- )
   HB-EXE$ >LEN IN-R @ >FD OUT-W @ >FD ERR-W @ >FD PROC-SPAWN-IO PID !
   PID @ 0 > TTRUE ;

: CAPTURE-CLOSE-CHILD-ENDS ( -- )
   IN-R @ close
   OUT-W @ close
   ERR-W @ close ;

: CAPTURE-SEND-SOURCE ( -- )
   IN-W @ >FD s" 2 3 + ." FD-WRITE-LN
   IN-W @ close ;

: CAPTURE-EXPECT-RC ( -- )
   PID @ >PID PROC-WAIT-RC MATCH result ok OF 0 T= ENDOF err OF drop 1 0 T= ENDOF ;MATCH ;

: CAPTURE-EXPECT-OUT ( -- )
   RCLR
   OUT-R @ >FD READ-ALL
   s" 5" TCONTAINS
   OUT-R @ close ;

: CAPTURE-EXPECT-ERR ( -- )
   RCLR
   ERR-R @ >FD READ-ALL
   RN @ 0 T=
   ERR-R @ close ;

: CAPTURE-VERIFY ( -- )
   CAPTURE-EXPECT-RC
   CAPTURE-EXPECT-OUT
   CAPTURE-EXPECT-ERR ;

: CAPTURE-HB ( -- )
   CAPTURE-PIPES
   CAPTURE-START-HB
   CAPTURE-CLOSE-CHILD-ENDS
   CAPTURE-SEND-SOURCE
   CAPTURE-VERIFY ;

: PTY-PATH-C ( n -- ) {: c :}
   c PTYNAME PTY-U @ + c!
   PTY-U @ 1 + PTY-U ! ;

: PTY-PATH+ ( ptr u8 n -- ) {: a:ptr u :}
   0 begin dup u < while
      dup a + c@ PTY-PATH-C
      1 +
   repeat drop ;

: PTY-PATH-U+ ( n -- ) {: n :}
   n 10 >= if n 10 / recurse then
   n 10 mod 48 + PTY-PATH-C ;

: PTY-PATH-BUILD ( -- )
   0 PTY-U !
   s" /dev/pts/" PTY-PATH+
   PTYNUM @ PTY-PATH-U+
   0 PTY-PATH-C ;

: OPEN-PTY-MASTER ( n -- ) {: flags :}
   s" /dev/ptmx" >LEN PROC-PATHZ flags 0 open MFD !
   MFD @ 2 > TTRUE
   MFD @ >FD FD-CLOEXEC! ;

: OPEN-PTY-DARWIN ( -- )
   PTY-OPEN-RDWR OPEN-PTY-MASTER
   MFD @ TIOCPTYGRANT NULL$ drop ioctl 0 T=
   MFD @ TIOCPTYUNLK NULL$ drop ioctl 0 T=
   MFD @ TIOCPTYGNAME PTYNAME ioctl 0 T=
   PTYNAME PTY-OPEN-RDWR 0 open SFD !
   SFD @ 2 > TTRUE ;

: OPEN-PTY-LINUX-MASTER ( -- )
   PTY-OPEN-RDWR OPEN-PTY-MASTER
   0 PTYNUM !
   MFD @ LINUX-TIOCSPTLCK PTYNUM ioctl 0 T=
   MFD @ LINUX-TIOCGPTN PTYNUM ioctl 0 T= ;

: OPEN-PTY-LINUX-SLAVE ( -- )
   PTY-PATH-BUILD
   PTYNAME PTY-OPEN-RDWR 0 open SFD !
   SFD @ 2 > TTRUE ;

: OPEN-PTY-LINUX ( -- )
   OPEN-PTY-LINUX-MASTER
   OPEN-PTY-LINUX-SLAVE ;

: PTY-TARGET-UNKNOWN ( -- )
   s" proc-pty: unknown target" 64 die ;

: OPEN-PTY ( -- )
   HB-TARGET-LINUX? if OPEN-PTY-LINUX exit then
   HB-TARGET-MACOS? if OPEN-PTY-DARWIN exit then
   PTY-TARGET-UNKNOWN ;

: PTY-START-HB ( -- )
   OPEN-PTY
   HB-EXE$ >LEN SFD @ >FD SFD @ >FD SFD @ >FD PROC-SPAWN-IO PID !
   PID @ 0 > TTRUE
   SFD @ close
   MFD-DRAIN ;

: PTY-PROMPT ( -- )
   s"  ok" EXPECT
   s" habu> " EXPECT ;

\ --- the two boot modes must enumerate ONE dictionary --------------------------
\ Dot habu-decide-arm-the-5234727b (USER RULING 2026-08-11: one dictionary surface
\ for every boot mode). The engine's baked REPL words are seeded into the
\ dictionary at boot; that seed used to run at the interactive REPL entry
\ alone and now runs at the end of the engine prefix on every boot. Every case in
\ this file already depends on the interactive half - without the seed there is no
\ prompt to type at - so what is left to prove is that the BATCH dictionary is the
\ SAME SET, not merely a set that also answers.
\
\ test/aot-seed-surface.f folds every dictionary record name into one number.
\ SEED-BATCH runs it through this engine as a PIPED program and keeps the number
\ it printed; PTY-SEED-SURFACE types the same line at the terminal and requires
\ the same number back. Neither side hardcodes it - it moves with every tree edit
\ - and a mode that seeded a different set, in a different order, or at a
\ different point cannot produce it.
\
\ The batch half lives here rather than in test/aot-seed-batch-suite.f (which owns
\ the batch-only cases) because the comparison needs both modes in ONE process,
\ this file is the tree's one cross-platform PTY driver, and its pipe vehicle is
\ four lines away. The pty case must run FIRST, before any case types a definition
\ into the interactive dictionary, or it would be comparing two different moments.
create SEEDNUM 64 allot   variable SEEDNUM-U   variable SEEDI

: SEEDNUM$ ( -- ptr u8 n )    SEEDNUM SEEDNUM-U @ ;
: SEED-LINE$ ( -- ptr u8 n )  S\" s\" test/aot-seed-surface.f\" required" ;

: SEED-BYTE ( -- n )
   SEEDI @ RN @ < if RBUF SEEDI @ + c@ exit then 0 ;

: SEED-TOKEN! ( -- )                 \ first whitespace-delimited token of RBUF
   0 SEEDNUM-U !  0 SEEDI !
   begin SEED-BYTE dup 0 <> swap 33 < and while  SEEDI @ 1 + SEEDI !  repeat
   begin SEED-BYTE 32 > SEEDNUM-U @ 64 < and while
      SEED-BYTE SEEDNUM SEEDNUM-U @ + c!
      SEEDNUM-U @ 1 + SEEDNUM-U !
      SEEDI @ 1 + SEEDI !
   repeat ;

: SEED-BATCH ( -- )
   CAPTURE-PIPES
   CAPTURE-START-HB
   CAPTURE-CLOSE-CHILD-ENDS
   IN-W @ >FD SEED-LINE$ FD-WRITE-LN
   IN-W @ close
   RCLR
   OUT-R @ >FD READ-ALL
   SEED-TOKEN!
   OUT-R @ close
   ERR-R @ close
   PID @ >PID PROC-WAIT-RC MATCH result ok OF 0 T= ENDOF err OF drop 1 0 T= ENDOF ;MATCH
   SEEDNUM-U @ 0 > TTRUE ;

: PTY-SEED-SURFACE ( -- )
   SEED-LINE$ STEP-LN
   SEEDNUM$ EXPECT
   s"  ok" EXPECT ;

: PTY-ARITH ( -- )
   s" 1 2 + ." STEP-LN
   s" 3" EXPECT
   s"  ok" EXPECT
   s" habu> " EXPECT ;

: PTY-UNKNOWN ( -- )
   s" frobnicate" STEP-LN
   s" E-UNDEFINED: frobnicate" EXPECT
   s" ?" EXPECT
   s" habu> " EXPECT
   PROBE-BARRIER
   s"  ok" REJECT ;

: PTY-SQUARE ( -- )
   s" : SQ dup * ;" STEP-LN
   s"  ok" EXPECT
   s" 7 SQ ." STEP-LN
   s" 49" EXPECT
   s"  ok" EXPECT ;

\ Certified word on an empty interpret stack: named underdepth reject, then
\ the REPL recovers and the next line evaluates (LDIAGRET recovery leg; dot
\ habu-habu-certified-words-84e84eaf).
: PTY-UNDERDEPTH ( -- )
   s" SQ" STEP-LN
   s" hb: interpret stack underdepth: SQ" EXPECT
   s" habu> " EXPECT
   PROBE-BARRIER
   s"  ok" REJECT
   s" 6 SQ ." STEP-LN
   s" 36" EXPECT
   s"  ok" EXPECT ;

: PTY-BACKSPACE ( -- )
   s" 1 2 + .." SEND-S
   127 SEND-C
   10 SEND-C
   MFD-DRAIN
   s" 3" EXPECT
   s"  ok" EXPECT ;

: PTY-CANCEL ( -- )
   s" garbage" SEND-S
   3 SEND-C
   MFD-DRAIN
   s" habu> " EXPECT
   PROBE-BARRIER
   s" garbage?" REJECT ;

: PTY-EDIT-SEED ( -- )
   s" 5 ." STEP-LN
   s" 5" EXPECT
   s"  ok" EXPECT ;

: PTY-EDIT-LEFT3 ( -- )
   s" 13 ." SEND-S
   68 SEND-ESC
   68 SEND-ESC
   68 SEND-ESC ;

: PTY-EDIT-INSERT-RUN ( -- )
   48 SEND-C
   10 SEND-C
   MFD-DRAIN
   s" 103" EXPECT
   s"  ok" EXPECT ;

: PTY-EDIT-HOME ( -- )
   PTY-EDIT-SEED
   PTY-EDIT-LEFT3
   PTY-EDIT-INSERT-RUN ;

: PTY-HISTORY-UP ( -- )
   65 SEND-ESC
   10 SEND-C
   MFD-DRAIN
   s" 103" EXPECT
   s"  ok" EXPECT ;

\ The engine bakes the REPL and nothing else (src/habu/native-runtime.f,
\ src/habu/stdin.f): a session that wants breakpoints, watch cells or the token
\ stepper loads them here, which is also how a REPL user reaches them.
: PTY-DEBUGGER-LOAD ( -- )
   s" require src/habu/debug.f" STEP-LN
   s"  ok" EXPECT ;

: PTY-BP-SOURCE ( -- )
   s" : SQB dup * ;" STEP-LN
   s"  ok" EXPECT
   s" : IN1 1 + ;" STEP-LN
   s"  ok" EXPECT ;

: PTY-BP-ARM-ONESHOT ( -- )
   s" ' SQB BP+" STEP-LN
   s"  ok" EXPECT
   s" ' IN1 BP+" STEP-LN
   s"  ok" EXPECT ;

: PTY-BP-RUN-SQ ( -- )
   s" 7 SQB ." STEP-LN
   s" habu-bp:" EXPECT
   s" 49" EXPECT ;

: PTY-BP-RUN-IN1 ( -- )
   s" 9 IN1 ." STEP-LN
   s" habu-bp:" EXPECT
   s" 10" EXPECT ;

: PTY-BP-RUN-SQ-CLEARED ( -- )
   s" 6 SQB ." STEP-LN
   s" 36" BARRIER                     \ the answer, past where a hit would have printed
   s" habu-bp:" REJECT ;

: PTY-BP-ONESHOT ( -- )
   PTY-BP-ARM-ONESHOT
   PTY-BP-RUN-SQ
   PTY-BP-RUN-IN1
   PTY-BP-RUN-SQ-CLEARED ;

: PTY-PB-SOURCE ( -- )
   s" : PB dup + ;" STEP-LN
   s"  ok" EXPECT ;

: PTY-PB-ARM ( -- )
   s" ' PB BP*" STEP-LN
   s"  ok" EXPECT ;

: PTY-PB-FIRST ( -- )
   s" 5 PB ." STEP-LN
   s" habu-bp:" EXPECT
   s" 10" EXPECT ;

: PTY-PB-SECOND ( -- )
   s" 6 PB ." STEP-LN
   s" habu-bp:" EXPECT
   s" 12" EXPECT ;

: PTY-PB-CLEAR ( -- )
   s" ' PB BP-" STEP-LN
   s"  ok" EXPECT ;

: PTY-BP-PERSISTENT ( -- )
   PTY-PB-SOURCE
   PTY-PB-ARM
   PTY-PB-FIRST
   PTY-PB-SECOND
   PTY-PB-CLEAR ;

: PTY-WATCH-VAR ( -- )
   s" variable WV" STEP-LN
   s"  ok" EXPECT
   s" 17 WV !" STEP-LN
   s"  ok" EXPECT ;

: PTY-WATCH-ADD ( -- )
   s" WV BPW+" STEP-LN
   s"  ok" EXPECT ;

: PTY-WATCH-WORD ( -- )
   s" : WID dup WV @ + ;" STEP-LN
   s"  ok" EXPECT ;

: PTY-WATCH-SOURCE ( -- )
   PTY-WATCH-VAR
   PTY-WATCH-ADD
   PTY-WATCH-WORD ;

: PTY-WATCH-ARM ( -- )
   s" ' WID BP+" STEP-LN
   s"  ok" EXPECT ;

: PTY-WATCH-RUN ( -- )
   s" 2 WID ." STEP-LN
   s" habu-bp-stack:" EXPECT
   s" habu-bp-watch:" EXPECT
   s" 0000000000000011" EXPECT
   s" 19" EXPECT ;

: PTY-WATCH-CLEAR ( -- )
   s" WV BPW-" STEP-LN
   s"  ok" EXPECT ;

: PTY-WATCHPOINT ( -- )
   PTY-WATCH-SOURCE
   PTY-WATCH-ARM
   PTY-WATCH-RUN
   PTY-WATCH-CLEAR ;

: PTY-BPN-ARM ( -- )
   s" 2 ' PB BPN" STEP-LN
   s"  ok" EXPECT ;

: PTY-BPN-SKIP ( -- )
   s" 3 PB ." STEP-LN
   s" 6" BARRIER                      \ the answer, past where a hit would have printed
   s" habu-bp:" REJECT ;

: PTY-BPN-FIRE ( -- )
   s" 3 PB ." STEP-LN
   s" habu-bp:" EXPECT
   s" 6" EXPECT ;

: PTY-BPN-CLEAR ( -- )
   s" ' PB BP-" STEP-LN
   s"  ok" EXPECT ;

: PTY-BP-NTH ( -- )
   PTY-BPN-ARM
   PTY-BPN-SKIP
   PTY-BPN-SKIP
   PTY-BPN-FIRE
   PTY-BPN-CLEAR ;

: PTY-DEFINE-F0-F2 ( -- )
   s" : F0 0 ;" STEP-LN
   s"  ok" EXPECT
   s" : F1 1 ;" STEP-LN
   s"  ok" EXPECT
   s" : F2 2 ;" STEP-LN
   s"  ok" EXPECT ;

: PTY-DEFINE-F3-F4 ( -- )
   s" : F3 3 ;" STEP-LN
   s"  ok" EXPECT
   s" : F4 4 ;" STEP-LN
   s"  ok" EXPECT ;

: PTY-DEFINE-F0-F4 ( -- )
   PTY-DEFINE-F0-F2
   PTY-DEFINE-F3-F4 ;

: PTY-DEFINE-F5-F6 ( -- )
   s" : F5 5 ;" STEP-LN
   s"  ok" EXPECT
   s" : F6 6 ;" STEP-LN
   s"  ok" EXPECT ;

: PTY-DEFINE-F7-F8 ( -- )
   s" : F7 7 ;" STEP-LN
   s"  ok" EXPECT
   s" : F8 8 ;" STEP-LN
   s"  ok" EXPECT ;

: PTY-DEFINE-F5-F8 ( -- )
   PTY-DEFINE-F5-F6
   PTY-DEFINE-F7-F8 ;

: PTY-BP-F0-F2 ( -- )
   s" ' F0 BP+" STEP-LN
   s"  ok" EXPECT
   s" ' F1 BP+" STEP-LN
   s"  ok" EXPECT
   s" ' F2 BP+" STEP-LN
   s"  ok" EXPECT ;

: PTY-BP-F3-F4 ( -- )
   s" ' F3 BP+" STEP-LN
   s"  ok" EXPECT
   s" ' F4 BP+" STEP-LN
   s"  ok" EXPECT ;

: PTY-BP-F0-F4 ( -- )
   PTY-BP-F0-F2
   PTY-BP-F3-F4 ;

: PTY-BP-F5-F6 ( -- )
   s" ' F5 BP+" STEP-LN
   s"  ok" EXPECT
   s" ' F6 BP+" STEP-LN
   s"  ok" EXPECT ;

: PTY-BP-F7-F8 ( -- )
   s" ' F7 BP+" STEP-LN
   s"  ok" EXPECT
   s" ' F8 BP+" STEP-LN
   s" table full" EXPECT ;

: PTY-BP-F5-F8 ( -- )
   PTY-BP-F5-F6
   PTY-BP-F7-F8 ;

: PTY-BP-TABLE-FULL ( -- )
   PTY-DEFINE-F0-F4
   PTY-DEFINE-F5-F8
   PTY-BP-F0-F4
   PTY-BP-F5-F8 ;

: PTY-STEP-BASELINE ( -- )
   s" 5 ." STEP-LN
   s" 5" EXPECT
   s"  ok" EXPECT ;

: PTY-STEP-TOKENS ( -- )
   s" step 2 3 + ." STEP-LN
   s" step> 2" EXPECT
   s" step> 3" EXPECT
   s" step> +" EXPECT
   s" 5" EXPECT ;

: PTY-STEP-DEFINE ( -- )
   s" step : SD dup * ;" STEP-LN
   s"  ok" EXPECT ;

: PTY-STEP-RUN ( -- )
   s" 4 SD ." STEP-LN
   s" 16" EXPECT ;

: PTY-STEP-RECOVER ( -- )
   s" 8 ." STEP-LN
   s" 8" EXPECT
   s"  ok" EXPECT ;

: PTY-STEPPER ( -- )
   PTY-STEP-BASELINE
   PTY-STEP-TOKENS
   PTY-STEP-DEFINE
   PTY-STEP-RUN
   PTY-STEP-RECOVER ;

: PTY-THROW-LINE ( -- )
   s" 99 throw" STEP-LN
   s" ?" EXPECT
   s" habu> " EXPECT
   PROBE-BARRIER
   s"  ok" REJECT ;

: PTY-THROW-AFTER ( -- )
   s" 6 ." STEP-LN
   s" 6" EXPECT
   s"  ok" EXPECT ;

\ tty-REPL parity for the LCOMPILEDIE compile-error family (dot
\ habu-convert-residual-compile-f460b9f2). A recoverable compile die typed at the
\ interactive REPL now recovers the session exactly like an undefined word
\ (PTY-UNKNOWN / LDIAGRET), instead of exiting the tty. Before the parity change
\ the same dup-def line exit-group'd and killed the REPL. dup-def is the
\ rollback-past-a-published-record case: the first PRDUP commits on its own line;
\ the second line's failure rolls back to that line's start, so PRDUP survives.
: PTY-COMPILE-DEF ( -- )
   s" : PRDUP ( -- ) ;" STEP-LN
   s"  ok" EXPECT ;

: PTY-COMPILE-RECOVER ( -- )
   s" : PRDUP ( -- ) ;" STEP-LN
   s" duplicate definition: PRDUP" EXPECT
   s" habu> " EXPECT
   PROBE-BARRIER
   s"  ok" REJECT ;

: PTY-COMPILE-AFTER ( -- )
   s" 7 ." STEP-LN
   s" 7" EXPECT
   s"  ok" EXPECT ;

: PTY-COMPILE-RECOVERY ( -- )
   PTY-COMPILE-DEF
   PTY-COMPILE-RECOVER
   PTY-COMPILE-AFTER ;

\ Package-scope rollback at the tty REPL (dot habu-recovery-pkg-scope-e0bd98e2). A
\ line that opens a package and then aborts mid-definition must roll the open-package
\ scope back to the line-start scope (global here), NOT leave the package dangling
\ open so later defines land in it silently. Before the fix the aborted
\ `package PRP public : PRFOO NOPEWORD ;` left PRP open, and the next `package PRQ`
\ nest-rejected. Now: the failing line recovers, a FRESH package opens cleanly, and
\ the next define lands global and runs.
: PTY-PKGSCOPE-FAIL ( -- )
   s" package PRP public : PRFOO ( -- ) NOPEWORD ;" STEP-LN
   s" E-UNDEFINED: NOPEWORD" EXPECT
   s" habu> " EXPECT
   PROBE-BARRIER
   s"  ok" REJECT ;

: PTY-PKGSCOPE-FRESH ( -- )                        \ a dangling PRP would make this `package` nest-reject (no " ok")
   s" package PRQ ;package" STEP-LN
   s"  ok" EXPECT ;

: PTY-PKGSCOPE-GLOBAL ( -- )                       \ the next define lands global and runs; a checked reference confirms checker scope is in step
   s" : PRDONE ( -- n ) 4242 ;  : PRUSE ( -- n ) PRDONE ;  PRUSE ." STEP-LN
   s" 4242" EXPECT
   s"  ok" EXPECT ;

: PTY-PKGSCOPE-RECOVERY ( -- )
   PTY-PKGSCOPE-FAIL
   PTY-PKGSCOPE-FRESH
   PTY-PKGSCOPE-GLOBAL ;

\ The barrier itself, against the live child. An absence claim over a buffer the
\ child has not answered into is granted by the harness's own silence: a drain
\ that returned on its first quiet poll leaves exactly that buffer. One leg has
\ no barrier behind it and must be REFUSED; the other has one and must still
\ catch the text when the child really printed it.
: PTY-REJECT-UNBARRIERED ( -- )
   s" frobnicate" STEP-LN
   RCLR                               \ the child's answer is not in the buffer
   s"  ok" REJECT? 0= TTRUE           \ so the claim is refused
   PROBE-BARRIER
   s"  ok" REJECT? TTRUE ;            \ and granted once a barrier closes the window

: PTY-REJECT-CATCHES ( -- )
   s" 5 ." STEP-LN                    \ a line that DOES print the trailer
   PROBE-BARRIER
   s"  ok" REJECT? 0= TTRUE ;         \ the window holds it, so the claim is refuted

: PTY-REJECT-BARRIER ( -- )
   PTY-REJECT-UNBARRIERED
   PTY-REJECT-CATCHES ;

: PTY-THROW-RECOVERY ( -- )
   PTY-THROW-LINE
   PTY-THROW-AFTER ;

\ ^D becomes EOF only while the terminal is RAW, which it is only while the
\ child sits in its line editor. A ^D that arrives while the line before it is
\ still executing is taken by the restored canonical discipline as an
\ empty-line EOF instead, never becomes a key, and leaves the child waiting for
\ one that never comes -- with the harness waiting on its exit, that is a hang,
\ not a failure. Only REDRAW prints the prompt and only after RAW-ON, so a
\ prompt printed after the buffer was cleared is proof that the editor holds
\ the terminal. The key that asks for one is DEL: in the editor it deletes
\ nothing from an empty line and redraws, and in canonical mode it is VERASE
\ over an empty buffer, so neither mode can act on it or read it as a signal.
: PTY-EDITOR-READY ( -- )
   RCLR
   127 SEND-C
   s" habu> " EXPECT ;

: PTY-STOP-HB ( -- )
   PTY-EDITOR-READY
   4 SEND-C
   PID @ >PID PROC-WAIT-RC MATCH result ok OF 0 T= ENDOF err OF drop 1 0 T= ENDOF ;MATCH
   MFD @ close ;

: PTY-BASIC ( -- )
   SEED-BATCH                        \ the pipe-mode fold, before this file owns a pty child
   PTY-START-HB
   PTY-PROMPT
   PTY-SEED-SURFACE                  \ FIRST at the prompt: before any case types a definition
   PTY-ARITH
   PTY-UNKNOWN
   PTY-SQUARE
   PTY-UNDERDEPTH ;

: PTY-EDITOR ( -- )
   PTY-BACKSPACE
   PTY-CANCEL
   PTY-EDIT-HOME
   PTY-HISTORY-UP ;

: PTY-BREAKPOINTS ( -- )
   PTY-DEBUGGER-LOAD
   PTY-BP-SOURCE
   PTY-BP-ONESHOT
   PTY-BP-PERSISTENT
   PTY-WATCHPOINT
   PTY-BP-NTH
   PTY-BP-TABLE-FULL ;

: PTY-TOOLS ( -- )
   PTY-STEPPER
   PTY-THROW-RECOVERY
   PTY-COMPILE-RECOVERY
   PTY-PKGSCOPE-RECOVERY
   PTY-REJECT-BARRIER ;

: PTY-HB ( -- )
   PTY-BASIC
   PTY-EDITOR
   PTY-BREAKPOINTS
   PTY-TOOLS
   PTY-STOP-HB ;

: REPORT ( -- )
   #FAIL @ 0 = if s" PASS: process/pty primitives" type cr exit then
   #FAIL @ . s" proc-pty: failures" 1 die ;

10 NL c!
4 EOT c!
CAPTURE-HB
PTY-HB
REPORT
;package
