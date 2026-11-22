split-str
\ process-test.f -- focused tests for src/lib/process.f.
\ Run: bin/hb --load src/lib/error.f src/lib/string.f src/lib/test.f src/lib/memory.f src/lib/fs.f src/lib/fs-mutate.f src/lib/process.f src/lib/process-argv.f src/lib/process-test.f

require src/lib/error.f
require src/lib/string.f
require src/lib/test.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-fork.f
require src/lib/process-argv.f
require src/lib/signal.f
require src/lib/task.f
require src/lib/test/ending.f
require src/lib/test/subject.f
require src/lib/spawn.f
require src/lib/fmt.f

variable PT-R
variable PT-W
variable PT-IN-R
variable PT-IN-W
variable PT-OUT-R
variable PT-OUT-W
variable PT-ROOT-U
variable PT-CAPTURE-OK-U
variable PT-CAPTURE-LONG-U
variable PT-CAPTURE-HANG-U
variable PT-CAPTURE-ERR-LONG-U
variable PT-CAPTURE-FALSE-U
variable PT-CAPTURE-EOF-U
variable PT-TERM-MARKER-U
variable PT-RUN-IN-U
variable PT-RUN-OUT-U
variable PT-FORK-CELL
create PT-BUF 32 allot
create PT-OUT 32 allot
create PT-ERR 32 allot
PATH-CAP 1 + constant PT-PWD-CAP     \ a working directory and its line end
create PT-PWD-OUT PT-PWD-CAP allot
create PT-ROOT-BUF PATH-CAP allot
create PT-CAPTURE-OK-BUF PATH-CAP allot
create PT-CAPTURE-LONG-BUF PATH-CAP allot
create PT-CAPTURE-HANG-BUF PATH-CAP allot
create PT-CAPTURE-ERR-LONG-BUF PATH-CAP allot
create PT-CAPTURE-FALSE-BUF PATH-CAP allot
create PT-CAPTURE-EOF-BUF PATH-CAP allot
create PT-TERM-MARKER-BUF PATH-CAP allot
create PT-RUN-IN-BUF PATH-CAP allot
create PT-RUN-OUT-BUF PATH-CAP allot
create PT-SUB-OUT 64 allot
create PT-SUB-ERR 256 allot
4096 constant PT-CHUNK                   \ one PROCESS:STDIN-CHUNK-CAP write
1024 constant PT-FILL-MAX                \ bounds the pipe fill at 4 MiB
create PT-CHUNK-BUF PT-CHUNK allot
create PT-DRAIN-BUF PT-CHUNK allot

2 constant PT-ENOENT
300 constant PT-SHORT-TIMEOUT-MS

: PT-COPY! ( str ^u8 ^n -- ) {: a:^ u dst:^ lenp:^ :}
   a dst u BYTE-COPY
   u lenp ! ;

: PT-PATH! ( str str ^u8 ^n -- ) {: pa:^ pu na:^ nu dst:^ lenp:^ :}
   pa pu na nu dst FS:JOIN-PATH lenp ! ;

: PT-ROOT ( -- str )
   PT-ROOT-BUF PT-ROOT-U @ ;

: PT-CAPTURE-OK ( -- str )
   PT-CAPTURE-OK-BUF PT-CAPTURE-OK-U @ ;

: PT-CAPTURE-LONG ( -- str )
   PT-CAPTURE-LONG-BUF PT-CAPTURE-LONG-U @ ;

: PT-CAPTURE-HANG ( -- str )
   PT-CAPTURE-HANG-BUF PT-CAPTURE-HANG-U @ ;

: PT-CAPTURE-ERR-LONG ( -- str )
   PT-CAPTURE-ERR-LONG-BUF PT-CAPTURE-ERR-LONG-U @ ;

: PT-CAPTURE-FALSE ( -- str )
   PT-CAPTURE-FALSE-BUF PT-CAPTURE-FALSE-U @ ;

: PT-CAPTURE-EOF ( -- str )
   PT-CAPTURE-EOF-BUF PT-CAPTURE-EOF-U @ ;

: PT-TERM-MARKER ( -- str )
   PT-TERM-MARKER-BUF PT-TERM-MARKER-U @ ;

: PT-RUN-IN ( -- str )
   PT-RUN-IN-BUF PT-RUN-IN-U @ ;

: PT-RUN-OUT ( -- str )
   PT-RUN-OUT-BUF PT-RUN-OUT-U @ ;

: PT-CAPTURE>N ( result<pcap:captured,pcap:failed> -- n n n )   \ outn errn code (0 on clean exit)
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} o LEN>N e LEN>N 0 ENDOF
     err OF PCAP-FAILED:UNMAKE  {: o:len e:len c:rc :} o LEN>N e LEN>N c RC>N ENDOF
   ;MATCH ;

: PT-ROOT! ( -- )
   s" habu-process" FS:MKTEMP-DIR {: a:^ u :}
   a u PT-ROOT-BUF PT-ROOT-U PT-COPY! ;

: PT-PATHS! ( -- )
   PT-ROOT s" capture-ok.f" PT-CAPTURE-OK-BUF PT-CAPTURE-OK-U PT-PATH!
   PT-ROOT s" capture-long.f" PT-CAPTURE-LONG-BUF PT-CAPTURE-LONG-U PT-PATH!
   PT-ROOT s" capture-hang.f" PT-CAPTURE-HANG-BUF PT-CAPTURE-HANG-U PT-PATH!
   PT-ROOT s" capture-err-long.f" PT-CAPTURE-ERR-LONG-BUF PT-CAPTURE-ERR-LONG-U PT-PATH!
   PT-ROOT s" capture-false.f" PT-CAPTURE-FALSE-BUF PT-CAPTURE-FALSE-U PT-PATH!
   PT-ROOT s" capture-eof.f" PT-CAPTURE-EOF-BUF PT-CAPTURE-EOF-U PT-PATH!
   PT-ROOT s" term-marker" PT-TERM-MARKER-BUF PT-TERM-MARKER-U PT-PATH!
   PT-ROOT s" run-in" PT-RUN-IN-BUF PT-RUN-IN-U PT-PATH!
   PT-ROOT s" run-out" PT-RUN-OUT-BUF PT-RUN-OUT-U PT-PATH! ;

: PT-WRITE-SCRIPT ( str str -- ) {: path:^ pathu src:^ srcu :}
   path pathu src srcu FS:WRITE-ALL
   path pathu FS:CLEANUP+ ;

: PT-WRITE-SCRIPTS ( -- )
   PT-CAPTURE-OK s\" 111 emit 117 emit 116 emit create E 101 c, 114 c, 114 c, 2 E 3 write drop s\q \q 7 die" PT-WRITE-SCRIPT
   PT-CAPTURE-LONG s" 97 emit 98 emit 99 emit 100 emit 101 emit 102 emit" PT-WRITE-SCRIPT
   PT-CAPTURE-HANG s" : HANG ( -- ) begin again ; HANG" PT-WRITE-SCRIPT
   PT-CAPTURE-ERR-LONG s" create E 97 c, 98 c, 99 c, 100 c, 101 c, 102 c, 2 E 6 write drop" PT-WRITE-SCRIPT
   PT-CAPTURE-FALSE s\" s\q \q 1 die" PT-WRITE-SCRIPT
   PT-CAPTURE-EOF s" 111 emit 107 emit 1 close 2 close : PT-WAIT ( -- ) mono-ns 1000000000 + begin dup mono-ns > while repeat drop ; PT-WAIT" PT-WRITE-SCRIPT ;

: PT-PREPARE ( -- )
   FS:CLEANUP-RESET
   PT-ROOT!
   PT-ROOT FS:CLEANUP-DIR+
   PT-PATHS!
   PT-WRITE-SCRIPTS
   PT-TERM-MARKER FS:CLEANUP+             \ the stay and plain cases leave it
   PT-RUN-IN FS:CLEANUP+
   PT-RUN-OUT FS:CLEANUP+ ;

: PT-CLEANUP ( -- )
   FS:CLEANUP-RUN
   PT-ROOT FS:EXISTS? TFALSE ;

: PT-RUN-HB-SCRIPT ( str ^u8 n ^u8 n n -- n n n )
   {: script:^ scriptu out:^ outcap err:^ errcap timeout :}
   PROC-ARGV-RESET
   script scriptu  >LEN PROC-ARGV+
   s" bin/hb" out outcap >LEN err errcap >LEN timeout >MS RUN-ARGV-CAPTURE
   PT-CAPTURE>N ;

: PT-RUN-HB-SCRIPT-OUTCOME ( str ^u8 n ^u8 n n -- len len PROCESS:ending )
   {: script:^ scriptu out:^ outcap err:^ errcap timeout :}
   PROC-ARGV-RESET
   script scriptu  >LEN PROC-ARGV+
   s" bin/hb" out outcap >LEN err errcap >LEN timeout >MS RUN-ARGV-CAPTURE-OUTCOME ;

: PT-RUN-CAPTURE ( str ^u8 n ^u8 n n -- n n n )
   {: path:^ pathu out:^ outcap err:^ errcap timeout :}
   path pathu out outcap >LEN err errcap >LEN timeout >MS PROCESS:RUN-CAPTURE
   PT-CAPTURE>N ;

: PT-READ ( n -- n )
   PT-BUF 32 read ;

: TEST-SPAWN-FAIL ( -- )
   s" /no/such/habu-process-test" -1 >FD -1 >FD -1 >FD PROCESS:SPAWN-IO drop ;

: TEST-SPAWN-RAW-MISSING ( -- )
   s" /no/such/habu-process-test" PROCESS:PATHZ
   -1 >FD -1 >FD -1 >FD PROCESS:SPAWN-RAW PID>N {: code:n :}
   code 0 < TTRUE
   HB-TARGET-MACOS? if code PT-ENOENT negate T= then ;

\ A process is never its own child, so the wait fails whatever launched it;
\ a wait on any child blocks under a launcher that leaves one, as caffeinate.
: TEST-WAIT-BAD ( -- )
   getpid >PID PROCESS:WAIT drop ;

: TEST-POLL-WAIT ( -- )
   PT-R @ 1 >MS PROCESS:POLL-IN-OR-TIMEOUT drop ;

: TEST-PATHZ ( -- )
   s" /usr/bin/true" PROCESS:PATHZ dup ZLEN 13 T=
   13 + c@ 0 T=
   s" /usr/bin/true" PROCESS:RUN-RC MATCH result
     ok  OF 0 T= ENDOF                          \ true exits clean -> ok(0)
     err OF drop 1 0 T= ENDOF                    \ unexpected failure
   ;MATCH
   s" /usr/bin/false" PROCESS:RUN-RC MATCH result
     ok  OF drop 1 0 T= ENDOF                     \ unexpected clean exit
     err OF 1 T= ENDOF                             \ false exits 1 -> err(1)
   ;MATCH ;

: TEST-SPAWN-WAIT ( -- )
   TEST-SPAWN-RAW-MISSING
   s" /usr/bin/true" -1 >FD -1 >FD -1 >FD PROCESS:SPAWN-IO
   PROCESS:WAIT PROCESS:STATUS>RC RC>N 0 T=
   [: TEST-SPAWN-FAIL ;] E-PROC-SPAWN TTHROWSQ ;

: TEST-PROC-WAIT-RC ( -- )
   s" /usr/bin/false" -1 >FD -1 >FD -1 >FD PROCESS:SPAWN-IO
   PROCESS:WAIT PROCESS:STATUS>RC RC>N 1 T= ;

: PT-FORK-EXIT ( n -- )
   s" " rot die ;

: TEST-PROC-FORK-WAIT ( -- )
   FD:PIPE-PAIR PT-W ! PT-R !
   PROC-FORK:CHECKED PID>N {: pid:n :}
   pid 0= if
      PT-R @ close
      PT-W @ s" f" write 1 <> if 2 PT-FORK-EXIT then
      PT-W @ close
      0 PT-FORK-EXIT
   then
   PT-W @ close
   PT-R @ -1 >MS PROCESS:POLL-IN COUNT>N 1 T=
   PT-R @ PT-READ 1 T=
   PT-BUF c@ 102 T=
   pid >PID PROCESS:WAIT PROCESS:STATUS>RC RC>N 0 T=
   PT-R @ close ;

: TEST-PROC-FORK-COW ( -- )
   7 PT-FORK-CELL !
   PROC-FORK:CHECKED PID>N {: pid:n :}
   pid 0= if
      9 PT-FORK-CELL !
      0 PT-FORK-EXIT
   then
   pid >PID PROCESS:WAIT PROCESS:STATUS>RC RC>N 0 T=
   PT-FORK-CELL @ 7 T= ;

: TEST-PROC-WAIT-OUTCOME-EXIT ( -- )
   s" /usr/bin/false" -1 >FD -1 >FD -1 >FD PROCESS:SPAWN-IO PROCESS:WAIT
   MATCH PROCESS:status
     exited OF 1 T= ENDOF                           \ /usr/bin/false -> exited 1
     signaled OF drop 1 0 T= ENDOF
   ;MATCH ;

: TEST-PROC-WAIT-OUTCOME-SIGNAL ( -- )
   PROC-ARGV-RESET
   s" -c"  >LEN PROC-ARGV+
   s" kill -TERM $$"  >LEN PROC-ARGV+
   s" /bin/sh" -1 >FD -1 >FD -1 >FD PROC-SPAWN-ARGV-IO PROCESS:WAIT
   MATCH PROCESS:status
     exited OF drop 1 0 T= ENDOF
     signaled OF 15 T= ENDOF                        \ SIGTERM death -> signaled 15
   ;MATCH ;

: TEST-PROC-OUTCOME>RC ( -- )
   7 PROCESS-ENDING:EXITED PROCESS:ENDING>RC RC>N 7 T=
   PROCESS:SIGKILL PROCESS-ENDING:SIGNALED PROCESS:ENDING>RC RC>N 137 T=      \ a deliberate kill
   [: PROCESS-ENDING:TIMEOUT PROCESS:ENDING>RC drop ;] E-PROC-TIMEOUT TTHROWSQ ;   \ a deadline

\ Negative checked regressions: a status or ending is not a loose (kind code)
\ pair, an ending does not compare with `=`, and no raw n or pair poses as one.
: TEST-PROC-OUTCOME-TYPES ( -- )
   s" PTP1 ( PROCESS:status -- PROCESS:ending ) PROCESS:STATUS>ENDING" CHECK-QUIET-CANDIDATE! -1 T=
   s" PTN1 ( PROCESS:status -- n n ) PROCESS:STATUS>ENDING" CHECK-QUIET-CANDIDATE! 0 T=
   s" PTN6 ( n -- PROCESS:ending ) PROCESS:STATUS>ENDING" CHECK-QUIET-CANDIDATE! 0 T=
   s" PTN2 ( n n -- rc ) PROCESS:ENDING>RC" CHECK-QUIET-CANDIDATE! 0 T=
   s" PTN3 ( PROCESS:ending PROCESS:ending -- bool ) =" CHECK-QUIET-CANDIDATE! 0 T=
   s" PTN4 ( pid -- n n ) PROCESS:WAIT" CHECK-QUIET-CANDIDATE! 0 T=
   s" PTN5 ( str ^u8 len ^u8 len ms -- len len n n ) RUN-ARGV-CAPTURE-OUTCOME" CHECK-QUIET-CANDIDATE! 0 T=
   s" PTP2 ( str ^u8 len ^u8 len ms -- len len PROCESS:ending ) RUN-ARGV-CAPTURE-OUTCOME" CHECK-QUIET-CANDIDATE! -1 T= ;

\ A signal-killed child's rc is 128+sig, never a masked rc 0.
: TEST-PROC-WAIT-SIGNAL-RC ( -- )
   PROC-ARGV-RESET
   s" -c"  >LEN PROC-ARGV+
   s" kill -KILL $$"  >LEN PROC-ARGV+
   s" /bin/sh" -1 >FD -1 >FD -1 >FD PROC-SPAWN-ARGV-IO
   PROCESS:WAIT PROCESS:STATUS>RC RC>N 137 T= ;   \ SIGKILL -> 128+9

: TEST-WAIT-FAIL ( -- )
   [: TEST-WAIT-BAD ;] E-PROC-WAIT TTHROWSQ ;

: TEST-PIPE ( -- )
   FD:PIPE-PAIR PT-W ! PT-R !
   PT-R @ FD:FD-CLOEXEC!
   PT-W @ FD:FD-CLOEXEC!
   PT-R @ 0 >MS PROCESS:POLL-IN COUNT>N 0 T=
   PT-W @ s" x" write 1 T=
   PT-R @ 100 >MS PROCESS:POLL-IN COUNT>N 1 T=
   PT-R @ PT-READ 1 T=
   PT-R @ close
   PT-W @ close ;

: TEST-WRITE-CLOSED-PIPE-NOSIGPIPE ( -- )
   FD:PIPE-PAIR PT-W ! PT-R !
   PT-W @ FD:FD-NOSIGPIPE!
   PT-R @ close
   PT-W @ s" 12345678901234567890123456789012" write -1 T=
   PT-W @ close ;

\ O_NONBLOCK is $800 on Linux and 4 on macOS, and F_SETFL drops a bit the host
\ does not know without saying so, so the only honest proof that PROCESS:NONBLOCK!
\ armed anything is the flag read back.
: PT-NONBLOCK-ARMED? ( fd -- bool ) {: fd :}
   fd FD>N FD:F-GETFL 0 fcntl {: flags :}
   flags 0 < if 0 0= 0= exit then
   flags FD:O-NONBLOCK and FD:O-NONBLOCK = ;


\ Chunks that land before the pipe refuses one. PT-FILL-MAX bounds the loop, but
\ a BLOCKING descriptor never reaches that bound: it stops inside write and
\ stays there. Every caller proves PT-NONBLOCK-ARMED? first for that reason.
: PT-FILL-FD ( n -- n ) {: wfd:n :}
   0 begin dup PT-FILL-MAX < while
      wfd PT-CHUNK-BUF PT-CHUNK write 0 < if exit then
      1+
   repeat ;


\ Reads the chunks the fill wrote back out, emptying the pipe. Draining a single
\ chunk is not enough to make room: a host page holds several of them (four to
\ a 16 KiB page here), and a slot frees only when its whole page is consumed.
: PT-DRAIN-FD ( n n -- n ) {: rfd:n chunks:n :}
   0 begin dup chunks < while
      rfd PT-DRAIN-BUF PT-CHUNK read PT-CHUNK <> if exit then
      1+
   repeat ;


\ The flag readback and the behaviour it buys: a full pipe refuses the next
\ write instead of blocking in it, and the same descriptor writes again once the
\ reader drains. That recovery is what separates back pressure from a broken
\ pipe, which no drain would fix and which this engine cannot tell apart by
\ errno -- every failed write answers a bare -1.
: TEST-PROC-NONBLOCK-ARMED ( -- )
   FD:PIPE-PAIR PT-W ! PT-R !
   PT-W @ PROCESS:NONBLOCK!
   PT-W @ PT-NONBLOCK-ARMED? dup TTRUE
   if
      PT-W @ PT-FILL-FD {: filled:n :}
      filled 0 > TTRUE
      filled PT-FILL-MAX < TTRUE
      PT-R @ filled PT-DRAIN-FD filled T=
      PT-W @ PT-CHUNK-BUF PT-CHUNK write PT-CHUNK T=
   then
   PT-R @ close
   PT-W @ close ;


\ The capture loop's stdin writer under back pressure. A refused write must
\ leave the child's stdin open and its offset untouched, so the next POLLOUT
\ under the capture deadline resumes the feed; closing there would truncate the
\ child's input without a word.
: TEST-PROC-STDIN-BACKPRESSURE ( -- )
   PROCESS:CAPTURE-RESET
   PROCESS:SETUP-STDIN-FDS
   PROCESS:IN-W @ >FD PT-NONBLOCK-ARMED? dup TTRUE
   if
      PROCESS:IN-W @ PT-FILL-FD {: filled:n :}
      filled 0 > TTRUE
      PT-CHUNK-BUF PT-CHUNK PROCESS:WRITE-STDIN
      PROCESS:IN-W @ 0 >= TTRUE
      PROCESS:IN-OFF @ 0 T=
      PROCESS:IN-R @ filled PT-DRAIN-FD filled T=
      PT-CHUNK-BUF PT-CHUNK PROCESS:WRITE-STDIN
      PROCESS:IN-OFF @ PT-CHUNK T=
   then
   PROCESS:CLOSE-STDIN-FDS ;


: TEST-POLL-TIMEOUT ( -- )
   FD:PIPE-PAIR PT-W ! PT-R !
   [: TEST-POLL-WAIT ;] E-PROC-TIMEOUT TTHROWSQ
   PT-R @ close
   PT-W @ close ;

: TEST-POLL-TWO ( -- )
   FD:PIPE-PAIR {: a-r:fd a-w:fd :}
   FD:PIPE-PAIR {: b-r:fd b-w:fd :}
   a-r FD:POLLIN 0 >IDX PROCESS:PFD-AT!
   b-r FD:POLLIN 1 >IDX PROCESS:PFD-AT!
   [: 2 20 >MS PROCESS:POLL drop ;] E-PROC-TIMEOUT TTHROWSQ
   b-w FD>N s" b" write 1 T=
   2 1000 >MS PROCESS:POLL COUNT>N 1 T=
   1 >IDX PROCESS:PFD-REVENTS FD:POLLIN and FD:POLLIN T=
   0 >IDX PROCESS:PFD-REVENTS 0 T=
   a-w FD>N s" a" write 1 T=
   2 1000 >MS PROCESS:POLL COUNT>N 2 T=
   5 2 ?do PROCESS:NO-FD >FD 0 i >IDX PROCESS:PFD-AT! loop
   5 0 >MS PROCESS:POLL COUNT>N 2 T=
   a-r FD>N close  a-w FD>N close  b-r FD>N close  b-w FD>N close ;

false [if] \ off until dot habu-add-the-checks-a49d054d
: TEST-POLL-BOUNDS ( -- )
   [: 0 0 >MS PROCESS:POLL drop ;] E-A-BOUNDS TTHROWSQ
   [: 6 0 >MS PROCESS:POLL drop ;] E-A-BOUNDS TTHROWSQ
   [: 0 >FD 0 5 >IDX PROCESS:PFD-AT! ;] E-A-BOUNDS TTHROWSQ ;
[then]

: TEST-RUN-ARGV-CAPTURE-EXACT-CAP ( -- )
   PT-CAPTURE-OK PT-OUT 3 PT-ERR 3 PROCESS:WAIT-FOREVER MS>N PT-RUN-HB-SCRIPT 7 T= 3 T= 3 T=
   PT-OUT 3 s" out" T$=
   PT-ERR 3 s" err" T$= ;

: TEST-RUN-CAPTURE-TRUNCATED ( -- )
   s" /usr/bin/yes" PT-OUT 3 PT-ERR 32 PROCESS:WAIT-FOREVER MS>N PT-RUN-CAPTURE 2drop drop ;

: TEST-RUN-ARGV-CAPTURE-TRUNCATED ( -- )
   PT-CAPTURE-LONG PT-OUT 3 PT-ERR 32 PROCESS:WAIT-FOREVER MS>N PT-RUN-HB-SCRIPT 2drop drop ;

: TEST-RUN-ARGV-CAPTURE-TIMEOUT ( -- )
   PT-CAPTURE-HANG PT-OUT 32 PT-ERR 32 PT-SHORT-TIMEOUT-MS PT-RUN-HB-SCRIPT 2drop drop ;

: TEST-RUN-ARGV-CAPTURE-ERR-TRUNCATED ( -- )
   PT-CAPTURE-ERR-LONG PT-OUT 32 PT-ERR 3 PROCESS:WAIT-FOREVER MS>N PT-RUN-HB-SCRIPT 2drop drop ;

: TEST-RUN-CAPTURE-FALSE ( -- )
   s" /usr/bin/false" PT-OUT 32 PT-ERR 32 PROCESS:WAIT-FOREVER MS>N PT-RUN-CAPTURE 1 T= 0 T= 0 T= ;

: TEST-RUN-ARGV-CAPTURE-FALSE ( -- )
   PT-CAPTURE-FALSE PT-OUT 32 PT-ERR 32 PROCESS:WAIT-FOREVER MS>N PT-RUN-HB-SCRIPT 1 T= 0 T= 0 T= ;

: TEST-RUN-ARGV-CAPTURE-OUTCOME-EXIT ( -- )
   PT-CAPTURE-FALSE PT-OUT 32 PT-ERR 32 PROCESS:WAIT-FOREVER MS>N PT-RUN-HB-SCRIPT-OUTCOME
   {: outu:len erru:len oc :}
   PT-CAPTURE-FALSE PT-OUT outu LEN>N PT-ERR erru LEN>N oc 1 T-ENDING-EXITED=
   erru LEN>N 0 T= outu LEN>N 0 T= ;

: TEST-RUN-ARGV-CAPTURE-OUTCOME-TIMEOUT ( -- )
   PT-CAPTURE-HANG PT-OUT 32 PT-ERR 32 PT-SHORT-TIMEOUT-MS PT-RUN-HB-SCRIPT-OUTCOME
   T-ENDING-TIMEOUT LEN>N 0 T= LEN>N 0 T=
   PROCESS:CAPTURE-ENDING T-ENDING-TIMEOUT ;             \ derived getter agrees

\ The deadline starts once stdout holds bytes: a host may start the child late.
: PT-EOF-START ( -- )
   PROC-ARGV-RESET
   PT-CAPTURE-EOF >LEN PROC-ARGV+
   [: s" bin/hb" PROC-ARGV-PREPARE
      PROCESS:WAIT-FOREVER PROCESS:CAPTURE-BEGIN
      PROC-SPAWN-ARGV-CAPTURE ;]
   [: PROC-ARGV-RESET ;] finally
   PROCESS:OUT-R @ >FD -1 >MS PROCESS:POLL-IN COUNT>N 1 T=
   PT-SHORT-TIMEOUT-MS >MS PROCESS:CAPTURE-DEADLINE! ;

\ The child closes its output pipes but remains alive for one second. The
\ prefix proves it reached that path before the shorter capture deadline.
: PT-RUN-EARLY-EOF ( -- )
   PT-EOF-START
   PT-OUT 32 >LEN PT-ERR 32 >LEN PROCESS:RUN-CAPTURE-LOOP
   PROCESS:CAPTURE-FINISH-RC PT-CAPTURE>N drop 2drop ;

: TEST-CAPTURE-EARLY-EOF ( -- )
   [: PT-RUN-EARLY-EOF ;] E-PROC-TIMEOUT TTHROWSQ
   PROCESS:OUT-LEN @ 2 T= PT-OUT 2 s" ok" T$=
   PROCESS:PID @ PROCESS:NO-PID T=
   PROCESS:OUT-R @ PROCESS:NO-FD T= PROCESS:ERR-R @ PROCESS:NO-FD T=
   PT-EOF-START
   PT-OUT 32 >LEN PT-ERR 32 >LEN PROCESS:RUN-CAPTURE-ENDING-LOOP
   PROCESS:CAPTURE-FINISH-ENDING
   T-ENDING-TIMEOUT LEN>N 0 T= LEN>N 2 T=
   PT-OUT 2 s" ok" T$=
   PROCESS:PID @ PROCESS:NO-PID T= ;

\ Signal-death capture: the reap path derives signaled(sig) from the raw wait
\ status alone -- no stored pair. The API return and the derived getter agree.
: TEST-RUN-ARGV-CAPTURE-OUTCOME-SIGNAL ( -- )
   s" /bin/sh -c kill -KILL $$" {: cmd:^ cmdu:n :}
   PROC-ARGV-RESET
   s" -c" >LEN PROC-ARGV+
   s" kill -KILL $$" >LEN PROC-ARGV+
   s" /bin/sh" PT-OUT 32 >LEN PT-ERR 32 >LEN PROCESS:WAIT-FOREVER RUN-ARGV-CAPTURE-OUTCOME
   {: outu:len erru:len oc :}
   cmd cmdu PT-OUT outu LEN>N PT-ERR erru LEN>N oc PROCESS:SIGKILL T-ENDING-SIGNALED=
   erru LEN>N 0 T= outu LEN>N 0 T=
   cmd cmdu PT-OUT outu LEN>N PT-ERR erru LEN>N PROCESS:CAPTURE-ENDING PROCESS:SIGKILL T-ENDING-SIGNALED= ;

\ A deliberately tiny deadline proves that capture reports a timeout. The child
\ never ends: the ok fixture finishes in about 9 ms on this host, so a race
\ against a 10 ms deadline measures the engine's boot time and goes red whenever
\ the child wins.
: PT-STARVED-CAPTURE ( -- )
   PROC-ARGV-RESET
   PT-CAPTURE-HANG >LEN PROC-ARGV+
   s" bin/hb" PT-OUT 32 >LEN PT-ERR 32 >LEN 10 >MS RUN-ARGV-CAPTURE
   PT-CAPTURE>N drop drop drop ;

: TEST-STARVED-TIMEOUT ( -- )
   [: PT-STARVED-CAPTURE ;] E-PROC-TIMEOUT TTHROWSQ ;

: TEST-RUN-CAPTURE-FD-CLEANUP ( -- )
   0 begin dup 80 < while
      s" /usr/bin/true" PT-OUT 32 PT-ERR 32 PROCESS:WAIT-FOREVER MS>N PT-RUN-CAPTURE 0 T= 0 T= 0 T=
      1+
   repeat drop ;

: TEST-RUN-IO-CAT ( -- )
   FD:PIPE-PAIR PT-IN-W ! PT-IN-R !
   FD:PIPE-PAIR PT-OUT-W ! PT-OUT-R !
   PT-IN-W @ s" cat-in" write 6 T=
   PT-IN-W @ close
   s" /bin/cat" PT-IN-R @ PT-OUT-W @ -1 >FD PROCESS:RUN-IO-RC MATCH result
     ok  OF 0 T= ENDOF                          \ cat exits clean -> ok(0)
     err OF drop 1 0 T= ENDOF                    \ signaled/nonzero -> unexpected failure
   ;MATCH
   PT-IN-R @ close
   PT-OUT-W @ close
   PT-OUT-R @ PT-READ 6 T=
   PT-BUF 6 s" cat-in" T$=
   PT-OUT-R @ close ;

: TEST-RUN-IO-FALSE ( -- )                          \ /usr/bin/false exits 1 -> err(1)
   s" /usr/bin/false" -1 >FD -1 >FD -1 >FD PROCESS:RUN-IO-RC MATCH result
     ok  OF drop 1 0 T= ENDOF                     \ unexpected clean exit
     err OF 1 T= ENDOF                             \ false exits 1 -> err(1)
   ;MATCH ;

\ Both arms of the capture result, a real child per arm: a clean exit matches
\ ok(captured) with the two lengths; a nonzero exit matches err(failed) with the
\ same lengths and the code.
: TEST-RUN-CAPTURE-RESULT-OK ( -- )                 \ /bin/pwd exits clean -> ok(captured)
   s" /bin/pwd" PT-PWD-OUT PT-PWD-CAP >LEN PT-ERR 32 >LEN PROCESS:WAIT-FOREVER PROCESS:RUN-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} o LEN>N 0 > TTRUE  e LEN>N 0 T= ENDOF
     err OF PCAP-FAILED:UNMAKE 2drop drop 1 0 T= ENDOF                \ pwd must exit clean
   ;MATCH ;

: TEST-RUN-CAPTURE-RESULT-ERR ( -- )                \ /usr/bin/false exits 1 -> err(failed,code=1)
   s" /usr/bin/false" PT-OUT 32 >LEN PT-ERR 32 >LEN PROCESS:WAIT-FOREVER PROCESS:RUN-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE 2drop 1 0 T= ENDOF                  \ false must not exit clean
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :} o LEN>N 0 T=  e LEN>N 0 T=  c RC>N 1 T= ENDOF
   ;MATCH ;

\ Negative checked regression: the return is the pcap result, not a (len len rc)
\ triple, and the ok arm cannot leak the failed payload.
: TEST-RUN-CAPTURE-RESULT-TYPES ( -- )
   s" PTC1 ( str ^u8 len ^u8 len ms -- result<pcap:captured,pcap:failed> ) PROCESS:RUN-CAPTURE" CHECK-QUIET-CANDIDATE! -1 T=
   s" PTC2 ( str ^u8 len ^u8 len ms -- len len rc ) PROCESS:RUN-CAPTURE" CHECK-QUIET-CANDIDATE! 0 T=
   s" PTC3 ( result<pcap:captured,pcap:failed> -- pcap:captured ) MATCH result ok OF ENDOF err OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T= ;

: TEST-PROC-READ-NEG-LEN ( -- )
   -1 PT-CAPTURE-OK-U !
   PT-R PT-OUT 32 >LEN PT-CAPTURE-OK-U PROCESS:READ-STREAM ;

: TEST-PROC-READ-HIGH-LEN ( -- )
   33 PT-CAPTURE-OK-U !
   PT-R PT-OUT 32 >LEN PT-CAPTURE-OK-U PROCESS:READ-STREAM ;

8 constant PT-Z-CAP
create PT-Z PT-Z-CAP allot

: TEST-ZCOPY-EXACT ( -- )
   s" a copy fits when its bytes and NUL fit the destination" T-LABEL
   s" 1234567" PT-Z PT-Z-CAP >LEN PROCESS:ZCOPY ZLEN 7 T= ;

: TEST-ZCOPY-FULL ( -- )
   s" 12345678" PT-Z PT-Z-CAP >LEN PROCESS:ZCOPY drop ;

: TEST-ZCOPY-NEG-LEN ( -- )
   PT-BUF -1 PT-Z PT-Z-CAP >LEN PROCESS:ZCOPY drop ;

\ A length whose NUL wraps the sum must not reach the copy loop.
: TEST-ZCOPY-HUGE-LEN ( -- )
   PT-BUF MEM-MAX-N PT-Z PT-Z-CAP >LEN PROCESS:ZCOPY drop ;

100 constant PT-HOLD-MS                  \ a window the held child outlasts
$100000000 constant PT-PID-WRAP          \ 2^32: what a C int pid_t drops
1000 constant PT-LONGEST-SLACK-MS        \ room for the later mono-ns read the wait makes
1 constant PT-NOT-A-CHILD                \ init or launchd: waitpid answers ECHILD

variable PT-WAIT-PID
variable PT-HOLD-PID                     \ the held child
variable PT-HOLD-IN                      \ its stdin's write end: closing it releases the child
variable PT-HOLD-OUT                     \ the read end its stdout and stderr share

\ A child of /bin/sh running the script, on the test's own stdio.
: PT-SH ( str -- pid )
   {: s:^ su:n :}
   PROC-ARGV-RESET
   s" -c" >LEN PROC-ARGV+
   s su >LEN PROC-ARGV+
   s" /bin/sh" -1 >FD -1 >FD -1 >FD PROC-SPAWN-ARGV-IO ;

: PT-HOLD-SCRIPT ( -- str )
   s" exec >&- 2>&-; read x; exit 5" ;

\ A child that closes stdout and stderr at once, then lives until its stdin
\ closes, and exits 5. Every pipe end is close-on-exec, so the child holds only
\ the copies on its fds 0, 1 and 2: EOF at the read end means it closed both.
: PT-HOLD ( -- )
   FD:PIPE-PAIR {: in-r:fd in-w:fd :}
   FD:PIPE-PAIR {: out-r:fd out-w:fd :}
   in-r FD:FD-CLOEXEC!  in-w FD:FD-CLOEXEC!  out-r FD:FD-CLOEXEC!  out-w FD:FD-CLOEXEC!
   PROC-ARGV-RESET
   s" -c" >LEN PROC-ARGV+
   PT-HOLD-SCRIPT >LEN PROC-ARGV+
   s" /bin/sh" in-r out-w out-w PROC-SPAWN-ARGV-IO PID>N PT-HOLD-PID !
   in-r FD>N close
   out-w FD>N close
   in-w FD>N PT-HOLD-IN !
   out-r FD>N PT-HOLD-OUT ! ;

: PT-HOLD-RELEASE ( -- )
   PT-HOLD-IN @ close ;

\ What one read at the shared end takes once it is readable: 0 is EOF.
: PT-HOLD-EOF ( -- n )
   PT-HOLD-OUT @ >FD -1 >MS PROCESS:POLL-IN COUNT>N 1 T=
   PT-HOLD-OUT @ PT-READ ;

\ The pid and the window as plain cells, so a case can name any value.
: PT-WAIT-AT ( n n -- PROCESS:ending )
   {: pid:n ms:n :}
   pid >PID ms >MS PROCESS:WAIT-BOUNDED ;

: PT-WAIT-HOLD ( n -- PROCESS:ending )
   PT-HOLD-PID @ swap PT-WAIT-AT ;

\ The held child's exit, read once it is released.
: PT-HOLD-EXITED= ( PROCESS:ending -- )
   {: oc :}
   PT-HOLD-SCRIPT s" " s" " oc 5 T-ENDING-EXITED= ;

: PT-TIMEOUT? ( PROCESS:ending -- bool )
   MATCH PROCESS:ending
     exited OF drop 0 0= 0= ENDOF
     signaled OF drop 0 0= 0= ENDOF
     timeout OF 0 0= ENDOF
   ;MATCH ;

\ ms 0, one probe a turn, after the release: the first probe that finds the exit
\ answers it. A wait that read its deadline before it probed would answer timeout
\ every turn, and this would never return.
: PT-HOLD-REAP-AT-ZERO ( -- PROCESS:ending )
   begin
      0 PT-WAIT-HOLD {: oc :}
      oc PT-TIMEOUT? 0= if oc exit then
      1 >MS TASK:SLEEP
   again ;

: PT-WAIT-AGAIN ( -- )
   PT-WAIT-PID @ PT-HOLD-MS PT-WAIT-AT drop ;

\ The first window whose nanoseconds carry mono-ns past the largest cell. The
\ wait reads mono-ns after this does, so its own bound is no larger.
: PT-PAST-CELL-MS ( -- n )
   MEM-MAX-N mono-ns - PROCESS:NS-PER-MS / 1 + ;

\ The largest window WAIT-BOUNDED takes: a case waiting for an exit passes it.
: PT-LONGEST-MS ( -- n )
   PT-PAST-CELL-MS PT-LONGEST-SLACK-MS - ;

\ Exit 0, exit 7 and SIGTERM, each read exactly; a reaped pid is no child of
\ ours any more.
: TEST-WAIT-BOUNDED-STATUS ( -- )
   s" exit 0" PT-SH {: child:pid :}
   child PID>N PT-WAIT-PID !
   child PT-LONGEST-MS >MS PROCESS:WAIT-BOUNDED {: oc0 :}
   s" exit 0" s" " s" " oc0 0 T-ENDING-EXITED=
   [: PT-WAIT-AGAIN ;] E-PROC-WAIT TTHROWSQ
   s" exit 7" PT-SH PT-LONGEST-MS >MS PROCESS:WAIT-BOUNDED {: oc7 :}
   s" exit 7" s" " s" " oc7 7 T-ENDING-EXITED=
   s" kill -TERM $$" PT-SH PT-LONGEST-MS >MS PROCESS:WAIT-BOUNDED {: ocs :}
   s" kill -TERM $$" s" " s" " ocs 15 T-ENDING-SIGNALED= ;

\ The run's evidence line for the EOF case.
: PT-HOLD-EVIDENCE ( n n PROCESS:ending -- )
   {: eof:n waited:n oc :}
   s" wait-bounded: read " type eof FMT:.INT
   s"  at EOF; ms 0 -> timeout; ms " type PT-HOLD-MS FMT:.INT
   s"  -> timeout after " type waited PROCESS:NS-PER-MS / FMT:.INT
   s"  ms; released, ms 0 -> rc " type oc PROCESS:ENDING>RC RC>N FMT:.INT cr ;

\ EOF is not exit. The held child has closed stdout and stderr, yet ms 0 probes
\ once and a 100 ms window waits it out, each answering timeout and taking
\ nothing; released, it exits 5 to us, so the pid stayed ours throughout.
: TEST-WAIT-BOUNDED-EOF ( -- )
   PT-HOLD
   PT-HOLD-EOF {: eof:n :}
   0 PT-WAIT-HOLD {: probed :}
   mono-ns {: started:n :}
   PT-HOLD-MS PT-WAIT-HOLD {: held :}
   mono-ns started - {: waited:n :}
   PT-HOLD-RELEASE
   PT-HOLD-REAP-AT-ZERO {: done :}
   PT-HOLD-OUT @ close
   eof 0 T=
   probed T-ENDING-TIMEOUT
   held T-ENDING-TIMEOUT
   waited PT-HOLD-MS PROCESS:NS-PER-MS * >= TTRUE
   done PT-HOLD-EXITED=
   eof waited done PT-HOLD-EVIDENCE ;

\ Refused before any waitpid, while the held child lives: pids 0 and -1, a pid
\ whose low 32 bits are the child's and one whose C int is minus the child's,
\ its process group; a negative window, the largest cell and the first window
\ past the cell. Released, the child still exits 5 to us.
: TEST-WAIT-BOUNDED-REFUSALS ( -- )
   PT-HOLD
   [: 0 PT-HOLD-MS PT-WAIT-AT drop ;] E-PROC-WAIT TTHROWSQ
   [: -1 PT-HOLD-MS PT-WAIT-AT drop ;] E-PROC-WAIT TTHROWSQ
   [: PT-HOLD-PID @ PT-PID-WRAP + PT-HOLD-MS PT-WAIT-AT drop ;] E-PROC-WAIT TTHROWSQ
   [: PT-PID-WRAP PT-HOLD-PID @ - PT-HOLD-MS PT-WAIT-AT drop ;] E-PROC-WAIT TTHROWSQ
   [: -1 PT-WAIT-HOLD drop ;] E-PROC-TIMEOUT TTHROWSQ
   [: MEM-MAX-N PT-WAIT-HOLD drop ;] E-PROC-TIMEOUT TTHROWSQ
   [: PT-PAST-CELL-MS PT-WAIT-HOLD drop ;] E-PROC-TIMEOUT TTHROWSQ
   PT-HOLD-RELEASE
   PT-LONGEST-MS PT-WAIT-HOLD {: done :}
   PT-HOLD-OUT @ close
   done PT-HOLD-EXITED= ;

\ The bound is where the cell ends: the longest admitted window reaches
\ waitpid, which answers ECHILD for a pid that is no child of ours.
: TEST-WAIT-BOUNDED-EDGE ( -- )
   [: PT-NOT-A-CHILD PT-LONGEST-MS PT-WAIT-AT drop ;] E-PROC-WAIT TTHROWSQ ;

\ The deadline every capture, poll and signal wait derives refuses the first
\ window past the cell, rather than wrapping to a deadline already gone.
: TEST-DEADLINE-PAST-CELL ( -- )
   [: PT-PAST-CELL-MS >MS PROCESS:DEADLINE-AT drop ;] E-PROC-TIMEOUT TTHROWSQ ;

\ Cut down to whole milliseconds, a wait for what is left of a 3 ms window
\ wakes before its deadline in most rounds.
: PT-LEFT-REACHED? ( -- bool )
   3 >MS PROCESS:DEADLINE-AT {: deadline:n :}
   1 deadline PROCESS:LEFT-MS MS>N deadline PROCESS:POLL-RESTART drop
   mono-ns deadline >= ;

: TEST-WAIT-REACHES-DEADLINE ( -- )
   PROCESS:NO-FD >FD 0 PROCESS:PFD!
   0 10 0 ?do PT-LEFT-REACHED? 0= if 1+ then loop 0 T= ;

\ A child's stdio end may be close-on-exec, and one above 2 may be unmarked: the
\ spawn keeps each on its stdio number and closes the source in the child.
256 constant PT-SPAWN-ERR-CAP            \ room for cat's complaint on a red run
create PT-SPAWN-ERR PT-SPAWN-ERR-CAP allot

\ Same number: the child program closes its fd 0, opens the capture's stdin pipe
\ there and runs /bin/cat on it (test/process-stdin-zero-child.f). A spawn that
\ left the close-on-exec flag on fd 0 hands cat a closed stdin.
: TEST-SPAWN-SAME-NUMBER ( -- )
   s" test/process-stdin-zero-child.f" PT-OUT 32 PT-SPAWN-ERR PT-SPAWN-ERR-CAP
   PROCESS:WAIT-FOREVER MS>N PT-RUN-HB-SCRIPT {: outn errn code :}
   s" spawn-stdio: same-number stdin: rc " type code FMT:.INT
   s"  out " type PT-OUT outn type
   s"  err " type PT-SPAWN-ERR errn type cr
   code 0 T=
   errn 0 T=
   PT-OUT outn s" stdin-zero" T$= ;

: PT-UNMARKED-SCRIPT ( -- str )
   s" exec >&-; read x" ;

\ Unmarked source: the stdout pipe's write end sits above 2 without
\ close-on-exec. The child closes its stdout and lives on reading stdin, so the
\ read end sees EOF only if the spawn closed the source in the child. The kill
\ and the signal it answers prove the child was alive at EOF.
: TEST-SPAWN-UNMARKED-SOURCE ( -- )
   FD:PIPE-PAIR {: in-r:fd in-w:fd :}
   FD:PIPE-PAIR {: out-r:fd out-w:fd :}
   in-r FD:FD-CLOEXEC!  in-w FD:FD-CLOEXEC!  out-r FD:FD-CLOEXEC!
   PROC-ARGV-RESET
   s" -c" >LEN PROC-ARGV+
   PT-UNMARKED-SCRIPT >LEN PROC-ARGV+
   s" /bin/sh" in-r out-w -1 >FD PROC-SPAWN-ARGV-IO {: child:pid :}
   in-r FD>N close
   out-w FD>N close
   out-r -1 >MS PROCESS:POLL-IN COUNT>N {: ready:n :}
   ready 1 = if out-r FD>N PT-READ else -1 then {: got:n :}
   child PROCESS:SIGKILL PROCESS:KILL-RAW RC>N {: killed:n :}
   child PROCESS:WAIT PROCESS:STATUS>ENDING {: oc :}
   in-w FD>N close
   out-r FD>N close
   s" spawn-stdio: unmarked source: fd " type out-w FD>N FMT:.INT
   s"  ready " type ready FMT:.INT
   s"  read " type got FMT:.INT cr
   out-w FD>N 2 > TTRUE
   ready 1 T=
   got 0 T=
   killed 0 T=
   PT-UNMARKED-SCRIPT s" " s" " oc PROCESS:SIGKILL T-ENDING-SIGNALED= ;

\ Stands in for a spawn whose request is refused.
: PT-REFUSED-SPAWN ( -- pid )
   E-PROC-PATH throw ;

: PT-THROWING-CAPTURE ( -- )
   PROCESS:WAIT-FOREVER PROCESS:CAPTURE-BEGIN
   [: PT-REFUSED-SPAWN ;] PROCESS:CAPTURE-ADOPT-SPAWN ;

: TEST-CAPTURE-SPAWN-THROWS ( -- )
   [: PT-THROWING-CAPTURE ;] E-PROC-PATH TTHROWSQ
   PROCESS:IN-R @ PROCESS:NO-FD T=  PROCESS:IN-W @ PROCESS:NO-FD T=
   PROCESS:OUT-R @ PROCESS:NO-FD T=  PROCESS:OUT-W @ PROCESS:NO-FD T=
   PROCESS:ERR-R @ PROCESS:NO-FD T=  PROCESS:ERR-W @ PROCESS:NO-FD T= ;

PROCESS-SYMBOLS
FUNCTION: PT-GET-LIMIT getrlimit ( n ^u8 -- i32 ) ;FUNCTION
FUNCTION: PT-SET-LIMIT setrlimit ( n ^u8 -- i32 ) ;FUNCTION
: PT-RLIMIT-NOFILE ( -- n ) HB-TARGET-MACOS? if 8 else 7 then ;
create PT-LIMITS 16 allot                \ struct rlimit: soft, then hard

: PT-SOFT-LIMIT! ( n -- ) {: soft:n :}
   PT-RLIMIT-NOFILE PT-LIMITS PT-GET-LIMIT 0 T=
   soft PT-LIMITS !
   PT-RLIMIT-NOFILE PT-LIMITS PT-SET-LIMIT 0 T= ;

: PT-NO-FD-CAPTURE ( -- )
   s" /usr/bin/true" PT-OUT 32 PT-ERR 32 PROCESS:WAIT-FOREVER MS>N
   PT-RUN-CAPTURE drop 2drop ;

\ With a descriptor limit of 0 the capture's first pipe cannot open: the child
\ never starts, and nothing the capture opened stays open.
: TEST-CAPTURE-SETUP-REFUSED ( -- )
   PT-RLIMIT-NOFILE PT-LIMITS PT-GET-LIMIT 0 T=
   PT-LIMITS @ {: saved:n :}
   0 PT-SOFT-LIMIT!
   [: PT-NO-FD-CAPTURE ;] catch {: code:n :}
   saved PT-SOFT-LIMIT!
   code E-PROC-SPAWN T=
   PROCESS:PID @ PROCESS:NO-PID T=
   PROCESS:OUT-R @ PROCESS:NO-FD T=  PROCESS:OUT-W @ PROCESS:NO-FD T= ;

\ test/process-term-child.f removes the marker only in its SIGTERM answer.
10000 constant PT-TERM-GRACE-MS          \ src/lib/process.f END-GRACE-MS
5 constant PT-READY-U                    \ the child's `ready`

\ A capture of the child in mode, with no deadline. A child that ends before
\ `ready` fails the case without a throw, so the case still kills it.
: PT-TERM-START ( str -- ) {: mode:^ modeu:n :}
   PT-TERM-MARKER s" marker" FS:WRITE-ALL
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/process-term-child.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   PT-TERM-MARKER >LEN PROC-ARGV+
   mode modeu >LEN PROC-ARGV+
   s" bin/hb" PROC-ARGV-PREPARE {: pathz:^ argv:^ :}
   PROCESS:WAIT-FOREVER PROCESS:CAPTURE-BEGIN
   pathz argv PROC-SPAWN-ARGV-CAPTURE
   PROCESS:OUT-R @ >FD -1 >MS PROCESS:POLL-IN COUNT>N
   {: ready:n :}
   ready 1 = if PROCESS:OUT-R @ PT-BUF PT-READY-U read else -1 then
   PT-READY-U T= ;

: PT-TERM-KILL ( -- )
   PROCESS:KILL-CAPTURE
   PROCESS:CLOSE-ALL-CAPTURE-FDS ;

: PT-SINCE-MS ( n -- n )
   mono-ns swap - PROCESS:NS-PER-MS / ;

: PT-TERM-ENDED= ( str n -- ) {: what:^ whatu:n sig:n :}
   what whatu s" " s" " PROCESS:CAPTURE-ENDING sig T-ENDING-SIGNALED= ;

: TEST-KILL-CAPTURE-ASKS ( -- )
   s" exit" PT-TERM-START
   PT-TERM-KILL
   s" kill-capture: a child that catches SIGTERM" SIGNAL:SIGTERM PT-TERM-ENDED=
   s" kill-capture: the child's answer removed the marker" T-LABEL
   PT-TERM-MARKER FS:EXISTS? TFALSE ;

: PT-TERM-RUN-LATE ( -- )
   PT-SHORT-TIMEOUT-MS >MS PROCESS:CAPTURE-DEADLINE!
   PT-OUT 32 >LEN PT-ERR 32 >LEN PROCESS:RUN-CAPTURE-LOOP ;

\ RUN-CAPTURE's loop, past its deadline.
: TEST-CAPTURE-DEADLINE-ASKS ( -- )
   s" exit" PT-TERM-START
   [: PT-TERM-RUN-LATE ;] E-PROC-TIMEOUT TTHROWSQ
   s" capture deadline: a child that catches SIGTERM"
   SIGNAL:SIGTERM PT-TERM-ENDED=
   s" capture deadline: the child's answer removed the marker" T-LABEL
   PT-TERM-MARKER FS:EXISTS? TFALSE ;

\ The bound, then the walk's whole window (PROC-TREE SETTLE-MS) twice over.
: TEST-KILL-CAPTURE-GRACE ( -- )
   s" stay" PT-TERM-START
   mono-ns {: t0:n :}
   PT-TERM-KILL
   t0 PT-SINCE-MS {: ms:n :}
   s" kill-capture: a child that catches SIGTERM and stays"
   PROCESS:SIGKILL PT-TERM-ENDED=
   s" kill-capture: a staying child is held for the grace bound" T-LABEL
   ms PT-TERM-GRACE-MS >= TTRUE
   s" kill-capture: a staying child is killed once the bound passes" T-LABEL
   ms PT-TERM-GRACE-MS 4000 + < TTRUE ;

: TEST-KILL-CAPTURE-PLAIN ( -- )
   s" plain" PT-TERM-START
   mono-ns {: t0:n :}
   PT-TERM-KILL
   t0 PT-SINCE-MS {: ms:n :}
   s" kill-capture: a child that does not catch SIGTERM"
   PROCESS:SIGKILL PT-TERM-ENDED=
   s" kill-capture: a child that does not catch SIGTERM is not held" T-LABEL
   ms PT-TERM-GRACE-MS < TTRUE ;

\ PROCESS:RUN: each of a child's fds 0, 1 and 2 chosen as inherit, null or pipe.
: PT-PIPE ( ^u8 n -- PROCESS:stdio ) SPAN:MAKE PROCESS-STDIO:PIPE ;

: PT-SH-REQUEST ( str -- PROCESS:request ) {: script:^ len :}
   s" /bin/sh" PROCESS:NEW
   s" -c" PROCESS:ARG
   script len PROCESS:ARG ;

\ The exit code, or -1 for a child a signal ended.
: PT-EXITED ( PROCESS:status -- n )
   MATCH PROCESS:status
     exited OF ENDOF
     signaled OF drop -1 ENDOF
   ;MATCH ;

: TEST-RUN-PIPES ( -- )
   s" /bin/cat" PROCESS:NEW
   s" chosen" PT-PIPE PT-OUT 32 PT-PIPE PT-ERR 32 PT-PIPE PROCESS:RUN
   PT-EXITED {: o:len e:len code:n :}
   s" run: pipes on all three streams" T-LABEL
   e LEN>N 0 T=
   PT-OUT o LEN>N s" chosen" T$=
   code 0 T= ;

: TEST-RUN-STDOUT-CAPACITY ( -- )
   s" printf abcdef" PT-SH-REQUEST
   PROCESS-STDIO:NULL PT-OUT 3 PT-PIPE PT-ERR 32 PT-PIPE PROCESS:RUN
   PT-EXITED drop 2drop ;

: TEST-RUN-STDERR-CAPACITY ( -- )
   s" printf abcdef >&2" PT-SH-REQUEST
   PROCESS-STDIO:NULL PT-OUT 32 PT-PIPE PT-ERR 3 PT-PIPE PROCESS:RUN
   PT-EXITED drop 2drop ;

\ The fds of run-in, holding "kept", and of run-out, made empty.
: PT-RUN-FILES ( -- n n )
   PT-RUN-IN s" kept" FS:WRITE-ALL
   PT-RUN-OUT NULL$ FS:WRITE-ALL
   PT-RUN-IN FS:>PATHZ open-rd {: in:n :}
   in 0 < if E-FS-OPEN throw then
   PT-RUN-OUT FS:OPEN-APPEND-FD {: out:n :}
   in out ;

\ The request's FD 0 and 1 name the parent's in and out.
: PT-FILE-SLOTS ( PROCESS:request n n -- PROCESS:request ) {: in out :}
   0 >FD in >FD PROCESS:FD
   1 >FD out >FD PROCESS:FD ;

: PT-RUN-OUT$ ( -- str )
   PT-RUN-OUT PT-PWD-OUT PT-PWD-CAP FS:READ-ALL {: n:n :}
   PT-PWD-OUT n ;

: TEST-RUN-INHERIT-SLOT ( -- )
   PT-RUN-FILES {: in out :}
   s" /bin/cat" PROCESS:NEW in out PT-FILE-SLOTS
   PROCESS-STDIO:INHERIT PROCESS-STDIO:INHERIT PROCESS-STDIO:INHERIT PROCESS:RUN
   PT-EXITED {: o:len e:len code:n :}
   in close  out close
   s" run: inherit keeps the request's FD" T-LABEL
   o LEN>N 0 T=
   e LEN>N 0 T=
   PT-RUN-OUT$ s" kept" T$=
   code 0 T= ;

: TEST-RUN-NULL-SLOT ( -- )
   PT-RUN-FILES {: in out :}
   s" cat >&2; echo out; echo err >&2" PT-SH-REQUEST
   in out PT-FILE-SLOTS
   PROCESS-STDIO:NULL PROCESS-STDIO:NULL PT-ERR 32 PT-PIPE PROCESS:RUN
   PT-EXITED {: o:len e:len code:n :}
   in close  out close
   s" run: null overrides the request's FD" T-LABEL
   o LEN>N 0 T=
   PT-ERR e LEN>N s\" err\n" T$=
   PT-RUN-OUT$ nip 0 T=
   code 0 T= ;

\ FD 0 names in, closed, and FD 3 out. RUN refuses the request before a
\ capture pipe can take in's number, so the child never writes ran to out.
: PT-RUN-CLOSED-FD ( n n -- )
   {: in out :}
   s" printf ran >&3" PT-SH-REQUEST
   0 >FD in >FD PROCESS:FD
   3 >FD out >FD PROCESS:FD
   PROCESS-STDIO:INHERIT PT-OUT 32 PT-PIPE PT-ERR 32 PT-PIPE PROCESS:RUN
   PT-EXITED drop 2drop ;

: TEST-RUN-CLOSED-FD ( -- )
   PT-RUN-FILES {: in out :}
   in close
   s" run: an FD naming a closed parent fd is refused" T-LABEL
   [: in out PT-RUN-CLOSED-FD ;] E-PROC-SPAWN TTHROWSQ
   PROCESS:ERRNO FS:EBADF T=
   out close
   PT-RUN-OUT$ nip 0 T= ;

\ Run in a subject child: with fd 0 closed, cat's inherited stdin is closed too,
\ so cat fails at once rather than read its own stdout pipe.
: PT-RUN-CLOSED-STDIN ( -- )
   0 close
   s" /bin/cat" PROCESS:NEW
   PROCESS-STDIO:INHERIT PT-OUT 32 PT-PIPE PROCESS-STDIO:INHERIT PROCESS:RUN
   PT-EXITED nip nip
   0<> if s" cat-failed" type then ;

: TEST-RUN-CLOSED-STDIN ( -- )
   s" PT-RUN-CLOSED-STDIN" {: src:^ srcu:n :}
   src srcu PT-SUB-OUT 64 >LEN PT-SUB-ERR 256 >LEN 10000 >MS SUBJECT:RUN
   {: o:len e:len ending :}
   s" run: a closed parent stdin stays closed in the child" T-LABEL
   PT-SUB-OUT o LEN>N s" cat-failed" T$=
   ending MATCH PROCESS:ending
     exited OF 0 T= ENDOF
     signaled OF drop 1 0 T= ENDOF
     timeout OF
        src srcu PT-SUB-OUT o LEN>N PT-SUB-ERR e LEN>N SUBJECT:TIMED-OUT
     ENDOF
   ;MATCH ;

: TEST-RUN-MISSING ( -- )
   s" /nonexistent/habu-run" PROCESS:NEW
   PROCESS-STDIO:NULL PT-OUT 32 PT-PIPE PT-ERR 32 PT-PIPE PROCESS:RUN
   PT-EXITED drop 2drop ;

: TEST-RUN-EXIT-CODE ( -- )
   s" exit 3" PT-SH-REQUEST
   PROCESS-STDIO:INHERIT PROCESS-STDIO:INHERIT PROCESS-STDIO:INHERIT PROCESS:RUN
   PT-EXITED nip nip
   s" run: the child's exit code" T-LABEL
   3 T= ;

: PROCESS-TEST-MAIN ( -- )
   T-RESET
   PT-PREPARE
   TEST-PATHZ
   TEST-SPAWN-WAIT
   TEST-PROC-WAIT-RC
   TEST-PROC-FORK-WAIT
   TEST-PROC-FORK-COW
   TEST-PROC-WAIT-OUTCOME-EXIT
   TEST-PROC-WAIT-OUTCOME-SIGNAL
   TEST-PROC-OUTCOME>RC
   TEST-PROC-OUTCOME-TYPES
   TEST-PROC-WAIT-SIGNAL-RC
   TEST-WAIT-FAIL
   TEST-WAIT-BOUNDED-STATUS
   TEST-WAIT-BOUNDED-EOF
   TEST-WAIT-BOUNDED-REFUSALS
   TEST-WAIT-BOUNDED-EDGE
   TEST-DEADLINE-PAST-CELL
   TEST-WAIT-REACHES-DEADLINE
   TEST-SPAWN-SAME-NUMBER
   TEST-SPAWN-UNMARKED-SOURCE
   TEST-PIPE
   TEST-WRITE-CLOSED-PIPE-NOSIGPIPE
   TEST-PROC-NONBLOCK-ARMED
   TEST-PROC-STDIN-BACKPRESSURE
   TEST-POLL-TIMEOUT
   TEST-POLL-TWO
   TEST-RUN-ARGV-CAPTURE-EXACT-CAP
   [: TEST-RUN-CAPTURE-TRUNCATED ;] E-PROC-STDOUT-CAPACITY TTHROWSQ
   [: TEST-RUN-ARGV-CAPTURE-TRUNCATED ;] E-PROC-STDOUT-CAPACITY TTHROWSQ
   [: TEST-RUN-ARGV-CAPTURE-TIMEOUT ;] E-PROC-TIMEOUT TTHROWSQ
   [: TEST-RUN-ARGV-CAPTURE-ERR-TRUNCATED ;] E-PROC-STDERR-CAPACITY TTHROWSQ
   TEST-RUN-CAPTURE-FALSE
   TEST-RUN-ARGV-CAPTURE-FALSE
   TEST-RUN-ARGV-CAPTURE-OUTCOME-EXIT
   TEST-RUN-ARGV-CAPTURE-OUTCOME-TIMEOUT
   TEST-CAPTURE-EARLY-EOF
   TEST-RUN-ARGV-CAPTURE-OUTCOME-SIGNAL
   TEST-STARVED-TIMEOUT
   TEST-RUN-CAPTURE-FD-CLEANUP
   TEST-CAPTURE-SPAWN-THROWS
   TEST-CAPTURE-SETUP-REFUSED
   TEST-RUN-IO-CAT
   TEST-RUN-IO-FALSE
   TEST-RUN-CAPTURE-RESULT-OK
   TEST-RUN-CAPTURE-RESULT-ERR
   TEST-RUN-CAPTURE-RESULT-TYPES
   TEST-KILL-CAPTURE-ASKS
   TEST-CAPTURE-DEADLINE-ASKS
   TEST-KILL-CAPTURE-GRACE
   TEST-KILL-CAPTURE-PLAIN
   TEST-RUN-PIPES
   [: TEST-RUN-STDOUT-CAPACITY ;] E-PROC-STDOUT-CAPACITY TTHROWSQ
   [: TEST-RUN-STDERR-CAPACITY ;] E-PROC-STDERR-CAPACITY TTHROWSQ
   TEST-RUN-INHERIT-SLOT
   TEST-RUN-NULL-SLOT
   TEST-RUN-CLOSED-FD
   TEST-RUN-CLOSED-STDIN
   [: TEST-RUN-MISSING ;] E-PROC-SPAWN TTHROWSQ
   TEST-RUN-EXIT-CODE
   [: TEST-PROC-READ-NEG-LEN ;] E-PROC-OUTPUT TTHROWSQ
   [: TEST-PROC-READ-HIGH-LEN ;] E-PROC-OUTPUT TTHROWSQ
   TEST-ZCOPY-EXACT
   [: TEST-ZCOPY-FULL ;] E-PROC-OUTPUT TTHROWSQ
   [: TEST-ZCOPY-NEG-LEN ;] E-PROC-OUTPUT TTHROWSQ
   [: TEST-ZCOPY-HUGE-LEN ;] E-PROC-OUTPUT TTHROWSQ
   PT-CLEANUP
   T-REPORT
   s" process-test: ok" type cr ;

\ process.f declares nothing outside its packages, so a program that loads it may name its own packages OUTCOME and ENDING.
package OUTCOME ;package
package ENDING ;package
PROCESS-TEST-MAIN
