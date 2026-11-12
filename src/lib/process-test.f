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
require src/lib/process-env.f
require src/lib/process-cwd.f
require src/lib/signal.f
require src/lib/task.f
require src/lib/test/ending.f
require src/lib/test/wait.f
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
4096 constant PT-CHUNK                   \ one PROCESS:STDIN-CHUNK-CAP write
1024 constant PT-FILL-MAX                \ bounds the pipe fill at 4 MiB
create PT-CHUNK-BUF PT-CHUNK allot
create PT-DRAIN-BUF PT-CHUNK allot

2 constant PT-ENOENT
15000 constant PT-HB-TIMEOUT-MS
3000 constant PT-CMD-TIMEOUT-MS
300 constant PT-SHORT-TIMEOUT-MS

: PT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: PT-PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- ) {: pa:ptr pu na:ptr nu dst:ptr lenp:ptr :}
   pa pu na nu dst FS:JOIN-PATH lenp ! ;

: PT-ROOT ( -- ptr u8 n )
   PT-ROOT-BUF PT-ROOT-U @ ;

: PT-CAPTURE-OK ( -- ptr u8 n )
   PT-CAPTURE-OK-BUF PT-CAPTURE-OK-U @ ;

: PT-CAPTURE-LONG ( -- ptr u8 n )
   PT-CAPTURE-LONG-BUF PT-CAPTURE-LONG-U @ ;

: PT-CAPTURE-HANG ( -- ptr u8 n )
   PT-CAPTURE-HANG-BUF PT-CAPTURE-HANG-U @ ;

: PT-CAPTURE-ERR-LONG ( -- ptr u8 n )
   PT-CAPTURE-ERR-LONG-BUF PT-CAPTURE-ERR-LONG-U @ ;

: PT-CAPTURE-FALSE ( -- ptr u8 n )
   PT-CAPTURE-FALSE-BUF PT-CAPTURE-FALSE-U @ ;

: PT-CAPTURE-EOF ( -- ptr u8 n )
   PT-CAPTURE-EOF-BUF PT-CAPTURE-EOF-U @ ;

: PT-TERM-MARKER ( -- ptr u8 n )
   PT-TERM-MARKER-BUF PT-TERM-MARKER-U @ ;

: PT-CAPTURE>N ( result<pcap:captured,pcap:failed> -- n n n )   \ outn errn code (0 on clean exit)
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} o LEN>N e LEN>N 0 ENDOF
     err OF PCAP-FAILED:UNMAKE  {: o:len e:len c:rc :} o LEN>N e LEN>N c RC>N ENDOF
   ;MATCH ;

: PT-ROOT! ( -- )
   s" habu-process" FS:MKTEMP-DIR {: a:ptr u :}
   a u PT-ROOT-BUF PT-ROOT-U PT-COPY! ;

: PT-PATHS! ( -- )
   PT-ROOT s" capture-ok.f" PT-CAPTURE-OK-BUF PT-CAPTURE-OK-U PT-PATH!
   PT-ROOT s" capture-long.f" PT-CAPTURE-LONG-BUF PT-CAPTURE-LONG-U PT-PATH!
   PT-ROOT s" capture-hang.f" PT-CAPTURE-HANG-BUF PT-CAPTURE-HANG-U PT-PATH!
   PT-ROOT s" capture-err-long.f" PT-CAPTURE-ERR-LONG-BUF PT-CAPTURE-ERR-LONG-U PT-PATH!
   PT-ROOT s" capture-false.f" PT-CAPTURE-FALSE-BUF PT-CAPTURE-FALSE-U PT-PATH!
   PT-ROOT s" capture-eof.f" PT-CAPTURE-EOF-BUF PT-CAPTURE-EOF-U PT-PATH!
   PT-ROOT s" term-marker" PT-TERM-MARKER-BUF PT-TERM-MARKER-U PT-PATH! ;

: PT-WRITE-SCRIPT ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu src:ptr srcu :}
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
   PT-TERM-MARKER FS:CLEANUP+ ;            \ the stay and plain cases leave it

: PT-CLEANUP ( -- )
   FS:CLEANUP-RUN
   PT-ROOT FS:EXISTS? TFALSE ;

: PT-RUN-HB-SCRIPT ( ptr u8 n ptr u8 n ptr u8 n n -- n n n )
   {: script:ptr scriptu out:ptr outcap err:ptr errcap timeout :}
   PROC-ARGV-RESET
   script scriptu  >LEN PROC-ARGV+
   s" bin/hb" >LEN out outcap >LEN err errcap >LEN timeout >MS RUN-ARGV-CAPTURE
   PT-CAPTURE>N ;

: PT-RUN-HB-SCRIPT-OUTCOME ( ptr u8 n ptr u8 n ptr u8 n n -- len len PROCESS:ending )
   {: script:ptr scriptu out:ptr outcap err:ptr errcap timeout :}
   PROC-ARGV-RESET
   script scriptu  >LEN PROC-ARGV+
   s" bin/hb" >LEN out outcap >LEN err errcap >LEN timeout >MS RUN-ARGV-CAPTURE-OUTCOME ;

: PT-RUN-CAPTURE ( ptr u8 n ptr u8 n ptr u8 n n -- n n n )
   {: path:ptr pathu out:ptr outcap err:ptr errcap timeout :}
   path pathu >LEN out outcap >LEN err errcap >LEN timeout >MS PROCESS:RUN-CAPTURE
   PT-CAPTURE>N ;

: PT-READ ( n -- n )
   PT-BUF 32 read ;

: TEST-SPAWN-FAIL ( -- )
   s" /no/such/habu-process-test" >LEN -1 >FD -1 >FD -1 >FD PROCESS:SPAWN-IO drop ;

: TEST-SPAWN-RAW-MISSING ( -- )
   s" /no/such/habu-process-test" >LEN PROCESS:PATHZ
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
   s" /usr/bin/true" >LEN PROCESS:PATHZ dup ZLEN 13 T=
   13 + c@ 0 T=
   s" /usr/bin/true" >LEN PROCESS:RUN-RC MATCH result
     ok  OF 0 T= ENDOF                          \ true exits clean -> ok(0)
     err OF drop 1 0 T= ENDOF                    \ unexpected failure
   ;MATCH
   s" /usr/bin/false" >LEN PROCESS:RUN-RC MATCH result
     ok  OF drop 1 0 T= ENDOF                     \ unexpected clean exit
     err OF 1 T= ENDOF                             \ false exits 1 -> err(1)
   ;MATCH ;

: TEST-SPAWN-WAIT ( -- )
   TEST-SPAWN-RAW-MISSING
   s" /usr/bin/true" >LEN -1 >FD -1 >FD -1 >FD PROCESS:SPAWN-IO
   PROCESS:WAIT PROCESS:STATUS>RC RC>N 0 T=
   [: TEST-SPAWN-FAIL ;] E-PROC-SPAWN TTHROWSQ ;

: TEST-PROC-WAIT-RC ( -- )
   s" /usr/bin/false" >LEN -1 >FD -1 >FD -1 >FD PROCESS:SPAWN-IO
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
   PT-R @ 1000 >MS PROCESS:POLL-IN COUNT>N
   dup 0= if s" process-test TEST-PROC-FORK-WAIT" TEST-WAIT:EXPIRED then 1 T=
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
   s" /usr/bin/false" >LEN -1 >FD -1 >FD -1 >FD PROCESS:SPAWN-IO PROCESS:WAIT
   MATCH PROCESS:status
     exited OF 1 T= ENDOF                           \ /usr/bin/false -> exited 1
     signaled OF drop 1 0 T= ENDOF
   ;MATCH ;

: TEST-PROC-WAIT-OUTCOME-SIGNAL ( -- )
   PROC-ARGV-RESET
   s" -c"  >LEN PROC-ARGV+
   s" kill -TERM $$"  >LEN PROC-ARGV+
   s" /bin/sh" >LEN -1 >FD -1 >FD -1 >FD PROC-SPAWN-ARGV-IO PROCESS:WAIT
   MATCH PROCESS:status
     exited OF drop 1 0 T= ENDOF
     signaled OF 15 T= ENDOF                        \ SIGTERM death -> signaled 15
   ;MATCH ;

: TEST-PROC-OUTCOME>RC ( -- )
   7 PROCESS-ENDING:exited PROCESS:ENDING>RC RC>N 7 T=
   PROCESS:SIGKILL PROCESS-ENDING:signaled PROCESS:ENDING>RC RC>N 137 T=      \ a deliberate kill
   [: PROCESS-ENDING:timeout PROCESS:ENDING>RC drop ;] E-PROC-TIMEOUT TTHROWSQ ;   \ a deadline

\ Negative checked regressions: a status or ending is not a loose (kind code)
\ pair, an ending does not compare with `=`, and no raw n or pair poses as one.
: TEST-PROC-OUTCOME-TYPES ( -- )
   s" PTP1 ( PROCESS:status -- PROCESS:ending ) PROCESS:STATUS>ENDING" CHECK-QUIET-CANDIDATE! -1 T=
   s" PTN1 ( PROCESS:status -- n n ) PROCESS:STATUS>ENDING" CHECK-QUIET-CANDIDATE! 0 T=
   s" PTN6 ( n -- PROCESS:ending ) PROCESS:STATUS>ENDING" CHECK-QUIET-CANDIDATE! 0 T=
   s" PTN2 ( n n -- rc ) PROCESS:ENDING>RC" CHECK-QUIET-CANDIDATE! 0 T=
   s" PTN3 ( PROCESS:ending PROCESS:ending -- bool ) =" CHECK-QUIET-CANDIDATE! 0 T=
   s" PTN4 ( pid -- n n ) PROCESS:WAIT" CHECK-QUIET-CANDIDATE! 0 T=
   s" PTN5 ( ptr u8 len ptr u8 len ptr u8 len ms -- len len n n ) RUN-ARGV-CAPTURE-OUTCOME" CHECK-QUIET-CANDIDATE! 0 T=
   s" PTP2 ( ptr u8 len ptr u8 len ptr u8 len ms -- len len PROCESS:ending ) RUN-ARGV-CAPTURE-OUTCOME" CHECK-QUIET-CANDIDATE! -1 T= ;

\ A signal-killed child's rc is 128+sig, never a masked rc 0.
: TEST-PROC-WAIT-SIGNAL-RC ( -- )
   PROC-ARGV-RESET
   s" -c"  >LEN PROC-ARGV+
   s" kill -KILL $$"  >LEN PROC-ARGV+
   s" /bin/sh" >LEN -1 >FD -1 >FD -1 >FD PROC-SPAWN-ARGV-IO
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
      PT-CHUNK-BUF PT-CHUNK >LEN PROCESS:WRITE-STDIN
      PROCESS:IN-W @ 0 >= TTRUE
      PROCESS:IN-OFF @ 0 T=
      PROCESS:IN-R @ filled PT-DRAIN-FD filled T=
      PT-CHUNK-BUF PT-CHUNK >LEN PROCESS:WRITE-STDIN
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
   [: 0 0 >MS PROCESS:POLL drop ;] E-A-BOUNDS TTHROWSQ
   [: 6 0 >MS PROCESS:POLL drop ;] E-A-BOUNDS TTHROWSQ
   [: 0 >FD 0 5 >IDX PROCESS:PFD-AT! ;] E-A-BOUNDS TTHROWSQ
   a-r FD>N close  a-w FD>N close  b-r FD>N close  b-w FD>N close ;

: TEST-RUN-ARGV-CAPTURE-EXACT-CAP ( -- )
   PT-CAPTURE-OK PT-OUT 3 PT-ERR 3 PT-HB-TIMEOUT-MS PT-RUN-HB-SCRIPT 7 T= 3 T= 3 T=
   PT-OUT 3 s" out" T$=
   PT-ERR 3 s" err" T$= ;

: TEST-RUN-CAPTURE-TRUNCATED ( -- )
   s" /usr/bin/yes" PT-OUT 3 PT-ERR 32 PT-CMD-TIMEOUT-MS PT-RUN-CAPTURE 2drop drop ;

: TEST-RUN-ARGV-CAPTURE-TRUNCATED ( -- )
   PT-CAPTURE-LONG PT-OUT 3 PT-ERR 32 PT-HB-TIMEOUT-MS PT-RUN-HB-SCRIPT 2drop drop ;

: TEST-RUN-ARGV-CAPTURE-TIMEOUT ( -- )
   PT-CAPTURE-HANG PT-OUT 32 PT-ERR 32 PT-SHORT-TIMEOUT-MS PT-RUN-HB-SCRIPT 2drop drop ;

: TEST-RUN-ARGV-CAPTURE-ERR-TRUNCATED ( -- )
   PT-CAPTURE-ERR-LONG PT-OUT 32 PT-ERR 3 PT-HB-TIMEOUT-MS PT-RUN-HB-SCRIPT 2drop drop ;

: TEST-RUN-CAPTURE-FALSE ( -- )
   s" /usr/bin/false" PT-OUT 32 PT-ERR 32 PT-CMD-TIMEOUT-MS PT-RUN-CAPTURE 1 T= 0 T= 0 T= ;

: TEST-RUN-ARGV-CAPTURE-FALSE ( -- )
   PT-CAPTURE-FALSE PT-OUT 32 PT-ERR 32 PT-HB-TIMEOUT-MS PT-RUN-HB-SCRIPT 1 T= 0 T= 0 T= ;

: TEST-RUN-ARGV-CAPTURE-OUTCOME-EXIT ( -- )
   PT-CAPTURE-FALSE PT-OUT 32 PT-ERR 32 PT-HB-TIMEOUT-MS PT-RUN-HB-SCRIPT-OUTCOME
   {: outu:len erru:len oc :}
   PT-CAPTURE-FALSE PT-OUT outu LEN>N PT-ERR erru LEN>N oc 1 T-ENDING-EXITED=
   erru LEN>N 0 T= outu LEN>N 0 T= ;

: TEST-RUN-ARGV-CAPTURE-OUTCOME-TIMEOUT ( -- )
   PT-CAPTURE-HANG PT-OUT 32 PT-ERR 32 PT-SHORT-TIMEOUT-MS PT-RUN-HB-SCRIPT-OUTCOME
   T-ENDING-TIMEOUT LEN>N 0 T= LEN>N 0 T=
   PROCESS:CAPTURE-ENDING T-ENDING-TIMEOUT ;             \ derived getter agrees

\ The child closes its output pipes but remains alive for one second. The
\ prefix proves it reached that path before the shorter capture deadline.
: PT-RUN-EARLY-EOF ( -- )
   PT-CAPTURE-EOF PT-OUT 32 PT-ERR 32 PT-SHORT-TIMEOUT-MS PT-RUN-HB-SCRIPT
   drop 2drop ;

: TEST-CAPTURE-EARLY-EOF ( -- )
   [: PT-RUN-EARLY-EOF ;] E-PROC-TIMEOUT TTHROWSQ
   PROCESS:OUT-LEN @ 2 T= PT-OUT 2 s" ok" T$=
   PROCESS:PID @ PROCESS:NO-PID T=
   PROCESS:OUT-R @ PROCESS:NO-FD T= PROCESS:ERR-R @ PROCESS:NO-FD T=
   PT-CAPTURE-EOF PT-OUT 32 PT-ERR 32 PT-SHORT-TIMEOUT-MS PT-RUN-HB-SCRIPT-OUTCOME
   T-ENDING-TIMEOUT LEN>N 0 T= LEN>N 2 T=
   PT-OUT 2 s" ok" T$=
   PROCESS:PID @ PROCESS:NO-PID T= ;

\ Signal-death capture: the reap path derives signaled(sig) from the raw wait
\ status alone -- no stored pair. The API return and the derived getter agree.
: TEST-RUN-ARGV-CAPTURE-OUTCOME-SIGNAL ( -- )
   s" /bin/sh -c kill -KILL $$" {: cmd:ptr cmdu:n :}
   PROC-ARGV-RESET
   s" -c" >LEN PROC-ARGV+
   s" kill -KILL $$" >LEN PROC-ARGV+
   s" /bin/sh" >LEN PT-OUT 32 >LEN PT-ERR 32 >LEN PT-HB-TIMEOUT-MS >MS RUN-ARGV-CAPTURE-OUTCOME
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
   s" bin/hb" >LEN PT-OUT 32 >LEN PT-ERR 32 >LEN 10 >MS RUN-ARGV-CAPTURE
   PT-CAPTURE>N drop drop drop ;

: TEST-STARVED-TIMEOUT ( -- )
   [: PT-STARVED-CAPTURE ;] E-PROC-TIMEOUT TTHROWSQ ;

: TEST-RUN-CAPTURE-FD-CLEANUP ( -- )
   0 begin dup 80 < while
      s" /usr/bin/true" PT-OUT 32 PT-ERR 32 PT-CMD-TIMEOUT-MS PT-RUN-CAPTURE 0 T= 0 T= 0 T=
      1+
   repeat drop ;

: TEST-RUN-IO-CAT ( -- )
   FD:PIPE-PAIR PT-IN-W ! PT-IN-R !
   FD:PIPE-PAIR PT-OUT-W ! PT-OUT-R !
   PT-IN-W @ s" cat-in" write 6 T=
   PT-IN-W @ close
   s" /bin/cat" >LEN PT-IN-R @ PT-OUT-W @ -1 >FD PROCESS:RUN-IO-RC MATCH result
     ok  OF 0 T= ENDOF                          \ cat exits clean -> ok(0)
     err OF drop 1 0 T= ENDOF                    \ signaled/nonzero -> unexpected failure
   ;MATCH
   PT-IN-R @ close
   PT-OUT-W @ close
   PT-OUT-R @ PT-READ 6 T=
   PT-BUF 6 s" cat-in" T$=
   PT-OUT-R @ close ;

: TEST-RUN-IO-FALSE ( -- )                          \ /usr/bin/false exits 1 -> err(1)
   s" /usr/bin/false" >LEN -1 >FD -1 >FD -1 >FD PROCESS:RUN-IO-RC MATCH result
     ok  OF drop 1 0 T= ENDOF                     \ unexpected clean exit
     err OF 1 T= ENDOF                             \ false exits 1 -> err(1)
   ;MATCH ;

\ Both arms of the capture result, a real child per arm: a clean exit matches
\ ok(captured) with the two lengths; a nonzero exit matches err(failed) with the
\ same lengths and the code.
: TEST-RUN-CAPTURE-RESULT-OK ( -- )                 \ /bin/pwd exits clean -> ok(captured)
   s" /bin/pwd" >LEN PT-PWD-OUT PT-PWD-CAP >LEN PT-ERR 32 >LEN PT-CMD-TIMEOUT-MS >MS PROCESS:RUN-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} o LEN>N 0 > TTRUE  e LEN>N 0 T= ENDOF
     err OF PCAP-FAILED:UNMAKE 2drop drop 1 0 T= ENDOF                \ pwd must exit clean
   ;MATCH ;

: TEST-RUN-CAPTURE-RESULT-ERR ( -- )                \ /usr/bin/false exits 1 -> err(failed,code=1)
   s" /usr/bin/false" >LEN PT-OUT 32 >LEN PT-ERR 32 >LEN PT-CMD-TIMEOUT-MS >MS PROCESS:RUN-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE 2drop 1 0 T= ENDOF                  \ false must not exit clean
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :} o LEN>N 0 T=  e LEN>N 0 T=  c RC>N 1 T= ENDOF
   ;MATCH ;

\ Negative checked regression: the return is the pcap result, not a (len len rc)
\ triple, and the ok arm cannot leak the failed payload.
: TEST-RUN-CAPTURE-RESULT-TYPES ( -- )
   s" PTC1 ( ptr u8 len ptr u8 len ptr u8 len ms -- result<pcap:captured,pcap:failed> ) PROCESS:RUN-CAPTURE" CHECK-QUIET-CANDIDATE! -1 T=
   s" PTC2 ( ptr u8 len ptr u8 len ptr u8 len ms -- len len rc ) PROCESS:RUN-CAPTURE" CHECK-QUIET-CANDIDATE! 0 T=
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
   s" 1234567" >LEN PT-Z PT-Z-CAP >LEN PROCESS:ZCOPY ZLEN 7 T= ;

: TEST-ZCOPY-FULL ( -- )
   s" 12345678" >LEN PT-Z PT-Z-CAP >LEN PROCESS:ZCOPY drop ;

: TEST-ZCOPY-NEG-LEN ( -- )
   PT-BUF -1 >LEN PT-Z PT-Z-CAP >LEN PROCESS:ZCOPY drop ;

\ A length whose NUL wraps the sum must not reach the copy loop.
: TEST-ZCOPY-HUGE-LEN ( -- )
   PT-BUF MEM-MAX-N >LEN PT-Z PT-Z-CAP >LEN PROCESS:ZCOPY drop ;

5000 constant PT-WAIT-MS                 \ far above any child here that is meant to end
100 constant PT-HOLD-MS                  \ a window the held child outlasts
$100000000 constant PT-PID-WRAP          \ 2^32: what a C int pid_t drops
1000 constant PT-LONGEST-SLACK-MS        \ room for the later mono-ns read the wait makes
1 constant PT-NOT-A-CHILD                \ init or launchd: waitpid answers ECHILD

variable PT-WAIT-PID
variable PT-HOLD-PID                     \ the held child
variable PT-HOLD-IN                      \ its stdin's write end: closing it releases the child
variable PT-HOLD-OUT                     \ the read end its stdout and stderr share

\ A child of /bin/sh running the script, on the test's own stdio.
: PT-SH ( ptr u8 n -- pid )
   {: s:ptr su:n :}
   PROC-ARGV-RESET
   s" -c" >LEN PROC-ARGV+
   s su >LEN PROC-ARGV+
   s" /bin/sh" >LEN -1 >FD -1 >FD -1 >FD PROC-SPAWN-ARGV-IO ;

: PT-HOLD-SCRIPT ( -- ptr u8 n )
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
   s" /bin/sh" >LEN in-r out-w out-w PROC-SPAWN-ARGV-IO PID>N PT-HOLD-PID !
   in-r FD>N close
   out-w FD>N close
   in-w FD>N PT-HOLD-IN !
   out-r FD>N PT-HOLD-OUT ! ;

: PT-HOLD-RELEASE ( -- )
   PT-HOLD-IN @ close ;

\ What one read at the shared end takes once it is readable: 0 is EOF.
: PT-HOLD-EOF ( -- n )
   PT-HOLD-OUT @ >FD PT-WAIT-MS >MS PROCESS:POLL-IN COUNT>N
   dup 0= if s" process-test PT-HOLD-EOF" TEST-WAIT:EXPIRED then 1 T=
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
\ every turn, and after PT-WAIT-MS this hands that timeout back.
: PT-HOLD-REAP-AT-ZERO ( -- PROCESS:ending )
   mono-ns PT-WAIT-MS PROCESS:NS-PER-MS * + {: until:n :}
   begin
      0 PT-WAIT-HOLD {: oc :}
      oc PT-TIMEOUT? 0= if oc exit then
      mono-ns until > if oc exit then
      1 >MS TASK:SLEEP
   again ;

: PT-WAIT-AGAIN ( -- )
   PT-WAIT-PID @ PT-WAIT-MS PT-WAIT-AT drop ;

\ Exit 0, exit 7 and SIGTERM, each read exactly; a reaped pid is no child of
\ ours any more.
: TEST-WAIT-BOUNDED-STATUS ( -- )
   s" exit 0" PT-SH {: child:pid :}
   child PID>N PT-WAIT-PID !
   child PT-WAIT-MS >MS PROCESS:WAIT-BOUNDED {: oc0 :}
   s" exit 0" s" " s" " oc0 0 T-ENDING-EXITED=
   [: PT-WAIT-AGAIN ;] E-PROC-WAIT TTHROWSQ
   s" exit 7" PT-SH PT-WAIT-MS >MS PROCESS:WAIT-BOUNDED {: oc7 :}
   s" exit 7" s" " s" " oc7 7 T-ENDING-EXITED=
   s" kill -TERM $$" PT-SH PT-WAIT-MS >MS PROCESS:WAIT-BOUNDED {: ocs :}
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

\ The first window whose nanoseconds carry mono-ns past the largest cell. The
\ wait reads mono-ns after this does, so its own bound is no larger.
: PT-PAST-CELL-MS ( -- n )
   MEM-MAX-N mono-ns - PROCESS:NS-PER-MS / 1 + ;

: PT-LONGEST-MS ( -- n )
   PT-PAST-CELL-MS PT-LONGEST-SLACK-MS - ;

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
   PT-WAIT-MS PT-WAIT-HOLD {: done :}
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
   PT-HB-TIMEOUT-MS PT-RUN-HB-SCRIPT {: outn errn code :}
   s" spawn-stdio: same-number stdin: rc " type code FMT:.INT
   s"  out " type PT-OUT outn type
   s"  err " type PT-SPAWN-ERR errn type cr
   code 0 T=
   errn 0 T=
   PT-OUT outn s" stdin-zero" T$= ;

: PT-UNMARKED-SCRIPT ( -- ptr u8 n )
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
   s" /bin/sh" >LEN in-r out-w -1 >FD PROC-SPAWN-ARGV-IO {: child:pid :}
   in-r FD>N close
   out-w FD>N close
   out-r PT-WAIT-MS >MS PROCESS:POLL-IN COUNT>N {: ready:n :}
   ready 1 = if out-r FD>N PT-READ else -1 then {: got:n :}
   child PROCESS:SIGKILL PROCESS:KILL-RAW RC>N {: killed:n :}
   child PT-WAIT-MS >MS PROCESS:WAIT-BOUNDED {: oc :}
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

1 BUFFER: PT-NUL-CWD

\ A cwd whose first byte is NUL passes the cwd bound, but the request reads it
\ as empty and refuses it inside the spawn's catch: E-PROC-PATH.
: PT-NUL-CWD-CAPTURE ( -- )
   PROC-ARGV-RESET
   PROC-ENV-RESET
   s" stale" >LEN PROC-ARGV+
   s" /bin/echo" >LEN PT-NUL-CWD 1 >LEN PT-OUT 32 >LEN PT-ERR 32 >LEN
   PT-CMD-TIMEOUT-MS >MS PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE
   PT-CAPTURE>N drop drop drop ;

\ The next capture stages its argument with no reset: the refused spawn reset
\ the staging.
: TEST-CAPTURE-SPAWN-THROWS ( -- )
   [: PT-NUL-CWD-CAPTURE ;] E-PROC-PATH TTHROWSQ
   PROCESS:IN-R @ PROCESS:NO-FD T=  PROCESS:IN-W @ PROCESS:NO-FD T=
   PROCESS:OUT-R @ PROCESS:NO-FD T=  PROCESS:OUT-W @ PROCESS:NO-FD T=
   PROCESS:ERR-R @ PROCESS:NO-FD T=  PROCESS:ERR-W @ PROCESS:NO-FD T=
   s" fresh" >LEN PROC-ARGV+
   s" /bin/echo" >LEN s" /" >LEN PT-OUT 32 >LEN PT-ERR 32 >LEN
   PT-CMD-TIMEOUT-MS >MS PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE
   PT-CAPTURE>N 0 T= 0 T= 6 T=
   PT-OUT 6 S\" fresh\n" T$= ;

\ test/process-term-child.f removes the marker only in its SIGTERM answer.
10000 constant PT-TERM-GRACE-MS          \ src/lib/process.f END-GRACE-MS
5 constant PT-READY-U                    \ the child's `ready`

\ A capture of the child in mode, with no deadline. A child not `ready` in time
\ fails the case without a throw, so the case still kills it.
: PT-TERM-START ( ptr u8 n -- ) {: mode:ptr modeu:n :}
   PT-TERM-MARKER s" marker" FS:WRITE-ALL
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/process-term-child.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   PT-TERM-MARKER >LEN PROC-ARGV+
   mode modeu >LEN PROC-ARGV+
   s" bin/hb" >LEN PROC-ARGV-PREPARE {: pathz:ptr argv:ptr :}
   PROCESS:WAIT-FOREVER PROCESS:CAPTURE-BEGIN
   pathz argv PROC-SPAWN-ARGV-CAPTURE
   PROCESS:OUT-R @ >FD PT-HB-TIMEOUT-MS >MS PROCESS:POLL-IN COUNT>N
   {: ready:n :}
   ready 1 = if PROCESS:OUT-R @ PT-BUF PT-READY-U read else -1 then
   PT-READY-U T= ;

: PT-TERM-KILL ( -- )
   PROCESS:KILL-CAPTURE
   PROCESS:CLOSE-ALL-CAPTURE-FDS ;

: PT-SINCE-MS ( n -- n )
   mono-ns swap - PROCESS:NS-PER-MS / ;

: PT-TERM-ENDED= ( ptr u8 n n -- ) {: what:ptr whatu:n sig:n :}
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
   [: TEST-RUN-CAPTURE-TRUNCATED ;] E-PROC-TRUNCATED TTHROWSQ
   [: TEST-RUN-ARGV-CAPTURE-TRUNCATED ;] E-PROC-TRUNCATED TTHROWSQ
   [: TEST-RUN-ARGV-CAPTURE-TIMEOUT ;] E-PROC-TIMEOUT TTHROWSQ
   [: TEST-RUN-ARGV-CAPTURE-ERR-TRUNCATED ;] E-PROC-TRUNCATED TTHROWSQ
   TEST-RUN-CAPTURE-FALSE
   TEST-RUN-ARGV-CAPTURE-FALSE
   TEST-RUN-ARGV-CAPTURE-OUTCOME-EXIT
   TEST-RUN-ARGV-CAPTURE-OUTCOME-TIMEOUT
   TEST-CAPTURE-EARLY-EOF
   TEST-RUN-ARGV-CAPTURE-OUTCOME-SIGNAL
   TEST-STARVED-TIMEOUT
   TEST-RUN-CAPTURE-FD-CLEANUP
   TEST-CAPTURE-SPAWN-THROWS
   TEST-RUN-IO-CAT
   TEST-RUN-IO-FALSE
   TEST-RUN-CAPTURE-RESULT-OK
   TEST-RUN-CAPTURE-RESULT-ERR
   TEST-RUN-CAPTURE-RESULT-TYPES
   TEST-KILL-CAPTURE-ASKS
   TEST-CAPTURE-DEADLINE-ASKS
   TEST-KILL-CAPTURE-GRACE
   TEST-KILL-CAPTURE-PLAIN
   [: TEST-PROC-READ-NEG-LEN ;] E-PROC-TRUNCATED TTHROWSQ
   [: TEST-PROC-READ-HIGH-LEN ;] E-PROC-TRUNCATED TTHROWSQ
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
