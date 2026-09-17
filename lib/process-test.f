\ process-test.f -- focused tests for lib/process.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-fork.f
require lib/process-argv.f
require lib/test/outcome.f
require test/checker-assert.f

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
variable PT-CAPTURE-HB-U
variable PT-FORK-CELL
create PT-BUF 32 allot
create PT-OUT 32 allot
create PT-ERR 32 allot
create PT-PWD-OUT 256 allot
create PT-ROOT-BUF FS-PATH-CAP allot
create PT-CAPTURE-OK-BUF FS-PATH-CAP allot
create PT-CAPTURE-LONG-BUF FS-PATH-CAP allot
create PT-CAPTURE-HANG-BUF FS-PATH-CAP allot
create PT-CAPTURE-ERR-LONG-BUF FS-PATH-CAP allot
create PT-CAPTURE-FALSE-BUF FS-PATH-CAP allot
create PT-CAPTURE-HB-BUF FS-PATH-CAP allot
4096 constant PT-CHUNK                   \ one PROC-STDIN-CHUNK-CAP write
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
   pa pu na nu dst JOIN-PATH lenp ! ;

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

: PT-CAPTURE-HB ( -- ptr u8 n )
   PT-CAPTURE-HB-BUF PT-CAPTURE-HB-U @ ;

: PT-CAPTURE>N ( result<pcap:captured,pcap:failed> -- n n n )   \ outn errn code (0 on clean exit)
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} o LEN>N e LEN>N 0 ENDOF
     err OF PCAP-FAILED:UNMAKE  {: o:len e:len c:rc :} o LEN>N e LEN>N c RC>N ENDOF
   ;MATCH ;

: PT-ROOT! ( -- )
   s" habu-process" TMPDIR-MKDIR {: a:ptr u :}
   a u PT-ROOT-BUF PT-ROOT-U PT-COPY! ;

: PT-PATHS! ( -- )
   PT-ROOT s" capture-ok.f" PT-CAPTURE-OK-BUF PT-CAPTURE-OK-U PT-PATH!
   PT-ROOT s" capture-long.f" PT-CAPTURE-LONG-BUF PT-CAPTURE-LONG-U PT-PATH!
   PT-ROOT s" capture-hang.f" PT-CAPTURE-HANG-BUF PT-CAPTURE-HANG-U PT-PATH!
   PT-ROOT s" capture-err-long.f" PT-CAPTURE-ERR-LONG-BUF PT-CAPTURE-ERR-LONG-U PT-PATH!
   PT-ROOT s" capture-false.f" PT-CAPTURE-FALSE-BUF PT-CAPTURE-FALSE-U PT-PATH!
   PT-ROOT s" capture-hb.f" PT-CAPTURE-HB-BUF PT-CAPTURE-HB-U PT-PATH! ;

: PT-WRITE-SCRIPT ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu src:ptr srcu :}
   path pathu src srcu WRITE-ALL
   path pathu CLEANUP+ ;

: PT-WRITE-SCRIPTS ( -- )
   PT-CAPTURE-OK s" 111 emit 117 emit 116 emit create E 101 c, 114 c, 114 c, 2 E 3 write drop 0 0 7 die" PT-WRITE-SCRIPT
   PT-CAPTURE-LONG s" 97 emit 98 emit 99 emit 100 emit 101 emit 102 emit" PT-WRITE-SCRIPT
   PT-CAPTURE-HANG s" : HANG ( -- ) begin again ; HANG" PT-WRITE-SCRIPT
   PT-CAPTURE-ERR-LONG s" create E 97 c, 98 c, 99 c, 100 c, 101 c, 102 c, 2 E 6 write drop" PT-WRITE-SCRIPT
   PT-CAPTURE-FALSE s" 0 0 1 die" PT-WRITE-SCRIPT
   PT-CAPTURE-HB s" 1 2 + . cr" PT-WRITE-SCRIPT ;

: PT-PREPARE ( -- )
   CLEANUP-RESET
   PT-ROOT!
   PT-ROOT CLEANUP-DIR+
   PT-PATHS!
   PT-WRITE-SCRIPTS ;

: PT-CLEANUP ( -- )
   CLEANUP-RUN
   PT-ROOT EXISTS? TFALSE ;

: PT-RUN-HB-SCRIPT ( ptr u8 n ptr u8 n ptr u8 n n -- n n n )
   {: script:ptr scriptu out:ptr outcap err:ptr errcap timeout :}
   PROC-ARGV-RESET
   script scriptu  >LEN PROC-ARGV+
   s" bin/hb" >LEN out outcap >LEN err errcap >LEN timeout >MS RUN-ARGV-CAPTURE
   PT-CAPTURE>N ;

: PT-RUN-HB-SCRIPT-OUTCOME ( ptr u8 n ptr u8 n ptr u8 n n -- len len outcome )
   {: script:ptr scriptu out:ptr outcap err:ptr errcap timeout :}
   PROC-ARGV-RESET
   script scriptu  >LEN PROC-ARGV+
   s" bin/hb" >LEN out outcap >LEN err errcap >LEN timeout >MS RUN-ARGV-CAPTURE-OUTCOME ;

: PT-RUN-CAPTURE ( ptr u8 n ptr u8 n ptr u8 n n -- n n n )
   {: path:ptr pathu out:ptr outcap err:ptr errcap timeout :}
   path pathu >LEN out outcap >LEN err errcap >LEN timeout >MS RUN-CAPTURE
   PT-CAPTURE>N ;

: PT-READ ( n -- n )
   PT-BUF 32 read ;

: TEST-SPAWN-FAIL ( -- )
   s" /no/such/habu-process-test" >LEN -1 >FD -1 >FD -1 >FD PROC-SPAWN-IO drop ;

: TEST-SPAWN-RAW-MISSING ( -- )
   s" /no/such/habu-process-test" >LEN PROC-PATHZ
   -1 >FD -1 >FD -1 >FD PROC-SPAWN-RAW PID>N {: code:n :}
   code 0 < TTRUE
   HB-TARGET-MACOS? if code PT-ENOENT negate T= then ;

: TEST-WAIT-BAD ( -- )
   -1 >PID PROC-WAIT-RC MATCH result ok OF drop ENDOF err OF drop ENDOF ;MATCH ;

: TEST-POLL-WAIT ( -- )
   PT-R @ 1 >MS POLL-IN-OR-TIMEOUT drop ;

: TEST-PATHZ ( -- )
   s" /usr/bin/true" >LEN PROC-PATHZ dup ZLEN 13 T=
   13 + c@ 0 T=
   s" /usr/bin/true" >LEN PROC-RUN-RC MATCH result
     ok  OF 0 T= ENDOF                          \ true exits clean -> ok(0)
     err OF drop 1 0 T= ENDOF                    \ unexpected failure
   ;MATCH
   s" /usr/bin/false" >LEN PROC-RUN-RC MATCH result
     ok  OF drop 1 0 T= ENDOF                     \ unexpected clean exit
     err OF 1 T= ENDOF                             \ false exits 1 -> err(1)
   ;MATCH ;

: TEST-SPAWN-WAIT ( -- )
   TEST-SPAWN-RAW-MISSING
   s" /usr/bin/true" >LEN -1 >FD -1 >FD -1 >FD PROC-SPAWN-IO PROC-WAIT-RC
   MATCH result ok OF 0 T= ENDOF err OF drop 1 0 T= ENDOF ;MATCH
   [: TEST-SPAWN-FAIL ;] E-PROC-SPAWN TTHROWSQ ;

: TEST-PROC-WAIT-STATUS ( -- )
   s" /usr/bin/true" >LEN -1 >FD -1 >FD -1 >FD PROC-SPAWN-IO PROC-WAIT-STATUS 0 T=
   s" /usr/bin/false" >LEN -1 >FD -1 >FD -1 >FD PROC-SPAWN-IO PROC-WAIT-STATUS 256 T= ;

: PT-FORK-EXIT ( n -- )
   s" " rot die ;

: TEST-PROC-FORK-WAIT ( -- )
   PIPE-PAIR PT-W ! PT-R !
   PROC-FORK:CHECKED PID>N {: pid:n :}
   pid 0= if
      PT-R @ close
      PT-W @ s" f" write 1 <> if 2 PT-FORK-EXIT then
      PT-W @ close
      0 PT-FORK-EXIT
   then
   PT-W @ close
   PT-R @ 1000 >MS POLL-IN COUNT>N 1 T=
   PT-R @ PT-READ 1 T=
   PT-BUF c@ 102 T=
   pid >PID PROC-WAIT-RC MATCH result ok OF 0 T= ENDOF err OF drop 1 0 T= ENDOF ;MATCH
   PT-R @ close ;

: TEST-PROC-FORK-COW ( -- )
   7 PT-FORK-CELL !
   PROC-FORK:CHECKED PID>N {: pid:n :}
   pid 0= if
      9 PT-FORK-CELL !
      0 PT-FORK-EXIT
   then
   pid >PID PROC-WAIT-RC MATCH result ok OF 0 T= ENDOF err OF drop 1 0 T= ENDOF ;MATCH
   PT-FORK-CELL @ 7 T= ;

: TEST-PROC-WAIT-OUTCOME-EXIT ( -- )
   s" /usr/bin/false" >LEN -1 >FD -1 >FD -1 >FD PROC-SPAWN-IO PROC-WAIT-OUTCOME
   MATCH outcome
     exited OF 1 T= ENDOF                           \ /usr/bin/false -> exited 1
     signaled OF drop 1 0 T= ENDOF
     timeout OF 1 0 T= ENDOF
   ;MATCH ;

: TEST-PROC-WAIT-OUTCOME-SIGNAL ( -- )
   PROC-ARGV-RESET
   s" -c"  >LEN PROC-ARGV+
   s" kill -TERM $$"  >LEN PROC-ARGV+
   s" /bin/sh" >LEN -1 >FD -1 >FD -1 >FD PROC-SPAWN-ARGV-IO PROC-WAIT-OUTCOME
   MATCH outcome
     exited OF drop 1 0 T= ENDOF
     signaled OF 15 T= ENDOF                        \ SIGTERM death -> signaled 15
     timeout OF 1 0 T= ENDOF
   ;MATCH ;

: TEST-PROC-OUTCOME>RC ( -- )
   7 OUTCOME:EXITED PROC-OUTCOME>RC RC>N 7 T=
   SIGKILL OUTCOME:SIGNALED PROC-OUTCOME>RC RC>N 137 T=
   OUTCOME:TIMEOUT PROC-OUTCOME>RC RC>N 137 T= ;

\ Negative checked regressions: the outcome is not a loose (kind code) pair,
\ does not compare with `=`, and a raw pair cannot pose as one.
: TEST-PROC-OUTCOME-TYPES ( -- )
   s" PTP1 ( n -- outcome ) PROC-STATUS>OUTCOME" CHECK-QUIET-CANDIDATE! -1 T=
   s" PTN1 ( n -- n n ) PROC-STATUS>OUTCOME" CHECK-QUIET-CANDIDATE! 0 T=
   s" PTN2 ( n n -- rc ) PROC-OUTCOME>RC" CHECK-QUIET-CANDIDATE! 0 T=
   s" PTN3 ( outcome outcome -- bool ) =" CHECK-QUIET-CANDIDATE! 0 T=
   s" PTN4 ( pid -- n n ) PROC-WAIT-OUTCOME" CHECK-QUIET-CANDIDATE! 0 T=
   s" PTN5 ( ptr u8 len ptr u8 len ptr u8 len ms -- len len n n ) RUN-ARGV-CAPTURE-OUTCOME" CHECK-QUIET-CANDIDATE! 0 T=
   s" PTP2 ( ptr u8 len ptr u8 len ptr u8 len ms -- len len outcome ) RUN-ARGV-CAPTURE-OUTCOME" CHECK-QUIET-CANDIDATE! -1 T= ;

\ Regression for habu-wait-rc-masks-9ae37cd0: a signal-killed child must report
\ 128+sig end-to-end through PROC-WAIT-RC, never a masked rc 0 (the retired raw
\ `wait-rc` path returned WEXITSTATUS only = 0 for a SIGKILLed child).
: TEST-PROC-WAIT-RC-SIGNAL ( -- )
   PROC-ARGV-RESET
   s" -c"  >LEN PROC-ARGV+
   s" kill -KILL $$"  >LEN PROC-ARGV+
   s" /bin/sh" >LEN -1 >FD -1 >FD -1 >FD PROC-SPAWN-ARGV-IO PROC-WAIT-RC
   MATCH result ok OF drop 1 0 T= ENDOF err OF 137 T= ENDOF ;MATCH ;   \ SIGKILL -> err(128+9)

: TEST-WAIT-FAIL ( -- )
   [: TEST-WAIT-BAD ;] E-PROC-WAIT TTHROWSQ ;

: TEST-PIPE ( -- )
   PIPE-PAIR PT-W ! PT-R !
   PT-R @ FD-CLOEXEC!
   PT-W @ FD-CLOEXEC!
   PT-R @ 0 >MS POLL-IN COUNT>N 0 T=
   PT-W @ s" x" write 1 T=
   PT-R @ 100 >MS POLL-IN COUNT>N 1 T=
   PT-R @ PT-READ 1 T=
   PT-R @ close
   PT-W @ close ;

: TEST-WRITE-CLOSED-PIPE-NOSIGPIPE ( -- )
   PIPE-PAIR PT-W ! PT-R !
   PT-W @ FD-NOSIGPIPE!
   PT-R @ close
   PT-W @ s" 12345678901234567890123456789012" write -1 T=
   PT-W @ close ;

\ habu-give-o-nonblock-cff35c7a. O_NONBLOCK is $800 on Linux and 4 on macOS, and
\ F_SETFL drops a bit the host does not know without saying so, so the only
\ honest proof that PROC-NONBLOCK! armed anything is the flag read back.
: PT-NONBLOCK-ARMED? ( fd -- bool ) {: fd :}
   fd FD>N F-GETFL 0 fcntl {: flags :}
   flags 0 < if 0 0= 0= exit then
   flags O-NONBLOCK and O-NONBLOCK = ;


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
   PIPE-PAIR PT-W ! PT-R !
   PT-W @ PROC-NONBLOCK!
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
\ under the capture deadline resumes the feed; closing there instead truncated
\ the child's input without a word.
: TEST-PROC-STDIN-BACKPRESSURE ( -- )
   PROC-CAPTURE-RESET
   PROC-SETUP-STDIN-FDS
   PROC-IN-W @ >FD PT-NONBLOCK-ARMED? dup TTRUE
   if
      PROC-IN-W @ PT-FILL-FD {: filled:n :}
      filled 0 > TTRUE
      PT-CHUNK-BUF PT-CHUNK >LEN PROC-WRITE-STDIN
      PROC-IN-W @ 0 >= TTRUE
      PROC-IN-OFF @ 0 T=
      PROC-IN-R @ filled PT-DRAIN-FD filled T=
      PT-CHUNK-BUF PT-CHUNK >LEN PROC-WRITE-STDIN
      PROC-IN-OFF @ PT-CHUNK T=
   then
   PROC-CLOSE-STDIN-FDS ;


: TEST-POLL-TIMEOUT ( -- )
   PIPE-PAIR PT-W ! PT-R !
   [: TEST-POLL-WAIT ;] E-PROC-TIMEOUT TTHROWSQ
   PT-R @ close
   PT-W @ close ;

: TEST-RUN-CAPTURE-BASIC ( -- )
   s" /bin/pwd" PT-PWD-OUT 256 PT-ERR 32 PT-CMD-TIMEOUT-MS PT-RUN-CAPTURE 0 T= 0 T= 0 > TTRUE ;

: TEST-RUN-ARGV-CAPTURE-BASIC ( -- )
   PT-CAPTURE-OK PT-OUT 32 PT-ERR 32 PT-HB-TIMEOUT-MS PT-RUN-HB-SCRIPT 7 T= 3 T= 3 T=
   PT-OUT 3 s" out" T$=
   PT-ERR 3 s" err" T$= ;

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
   1 T-OUTCOME-EXITED= LEN>N 0 T= LEN>N 0 T= ;

: TEST-RUN-ARGV-CAPTURE-OUTCOME-TIMEOUT ( -- )
   PT-CAPTURE-HANG PT-OUT 32 PT-ERR 32 PT-SHORT-TIMEOUT-MS PT-RUN-HB-SCRIPT-OUTCOME
   T-OUTCOME-TIMEOUT LEN>N 0 T= LEN>N 0 T=
   PROC-CAPTURE-OUTCOME T-OUTCOME-TIMEOUT ;             \ derived getter agrees

\ Signal-death capture: the reap path derives signaled(sig) from the raw wait
\ status alone -- no stored pair. The API return and the derived getter agree.
: TEST-RUN-ARGV-CAPTURE-OUTCOME-SIGNAL ( -- )
   PROC-ARGV-RESET
   s" -c" >LEN PROC-ARGV+
   s" kill -KILL $$" >LEN PROC-ARGV+
   s" /bin/sh" >LEN PT-OUT 32 >LEN PT-ERR 32 >LEN PT-HB-TIMEOUT-MS >MS RUN-ARGV-CAPTURE-OUTCOME
   SIGKILL T-OUTCOME-SIGNALED= LEN>N 0 T= LEN>N 0 T=
   PROC-CAPTURE-OUTCOME SIGKILL T-OUTCOME-SIGNALED= ;

\ A deliberately tiny deadline proves that capture reports a timeout.
: PT-STARVED-CAPTURE ( -- )
   PROC-ARGV-RESET
   PT-CAPTURE-OK >LEN PROC-ARGV+
   s" bin/hb" >LEN PT-OUT 32 >LEN PT-ERR 32 >LEN 10 >MS RUN-ARGV-CAPTURE
   PT-CAPTURE>N drop drop drop ;

: TEST-STARVED-TIMEOUT ( -- )
   [: PT-STARVED-CAPTURE ;] E-PROC-TIMEOUT TTHROWSQ ;

: TEST-RUN-ARGV-CAPTURE-HB ( -- )
   PT-CAPTURE-HB PT-OUT 32 PT-ERR 32 PT-HB-TIMEOUT-MS PT-RUN-HB-SCRIPT 0 T= 0 T= 3 T=
   PT-OUT c@ 51 T=
   PT-OUT 1 + c@ 10 T=
   PT-OUT 2 + c@ 10 T= ;

: TEST-RUN-CAPTURE-FD-CLEANUP ( -- )
   0 begin dup 80 < while
      s" /usr/bin/true" PT-OUT 32 PT-ERR 32 PT-CMD-TIMEOUT-MS PT-RUN-CAPTURE 0 T= 0 T= 0 T=
      1+
   repeat drop ;

: TEST-RUN-IO-CAT ( -- )
   PIPE-PAIR PT-IN-W ! PT-IN-R !
   PIPE-PAIR PT-OUT-W ! PT-OUT-R !
   PT-IN-W @ s" cat-in" write 6 T=
   PT-IN-W @ close
   s" /bin/cat" >LEN PT-IN-R @ PT-OUT-W @ -1 >FD PROC-RUN-IO-RC MATCH result
     ok  OF 0 T= ENDOF                          \ cat exits clean -> ok(0)
     err OF drop 1 0 T= ENDOF                    \ signaled/nonzero -> unexpected failure
   ;MATCH
   PT-IN-R @ close
   PT-OUT-W @ close
   PT-OUT-R @ PT-READ 6 T=
   PT-BUF 6 s" cat-in" T$=
   PT-OUT-R @ close ;

: TEST-RUN-IO-FALSE ( -- )                          \ /usr/bin/false exits 1 -> err(1)
   s" /usr/bin/false" >LEN -1 >FD -1 >FD -1 >FD PROC-RUN-IO-RC MATCH result
     ok  OF drop 1 0 T= ENDOF                     \ unexpected clean exit
     err OF 1 T= ENDOF                             \ false exits 1 -> err(1)
   ;MATCH ;

\ Direct both-arm coverage for the migrated capture result (a REAL spawned
\ child, per arm): a clean exit MATCHes ok(captured) carrying the two lengths;
\ a nonzero exit MATCHes err(failed) carrying the SAME lengths PLUS the code.
: TEST-RUN-CAPTURE-RESULT-OK ( -- )                 \ /bin/pwd exits clean -> ok(captured)
   s" /bin/pwd" >LEN PT-PWD-OUT 256 >LEN PT-ERR 32 >LEN PT-CMD-TIMEOUT-MS >MS RUN-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} o LEN>N 0 > TTRUE  e LEN>N 0 T= ENDOF
     err OF PCAP-FAILED:UNMAKE 2drop drop 1 0 T= ENDOF                \ pwd must exit clean
   ;MATCH ;

: TEST-RUN-CAPTURE-RESULT-ERR ( -- )                \ /usr/bin/false exits 1 -> err(failed,code=1)
   s" /usr/bin/false" >LEN PT-OUT 32 >LEN PT-ERR 32 >LEN PT-CMD-TIMEOUT-MS >MS RUN-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE 2drop 1 0 T= ENDOF                  \ false must not exit clean
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :} o LEN>N 0 T=  e LEN>N 0 T=  c RC>N 1 T= ENDOF
   ;MATCH ;

\ Negative checked regression: the migrated return is the pcap result, not the
\ retired (len len rc) triple, and the ok arm cannot leak the failed payload.
: TEST-RUN-CAPTURE-RESULT-TYPES ( -- )
   s" PTC1 ( ptr u8 len ptr u8 len ptr u8 len ms -- result<pcap:captured,pcap:failed> ) RUN-CAPTURE" CHECK-QUIET-CANDIDATE! -1 T=
   s" PTC2 ( ptr u8 len ptr u8 len ptr u8 len ms -- len len rc ) RUN-CAPTURE" CHECK-QUIET-CANDIDATE! 0 T=
   s" PTC3 ( result<pcap:captured,pcap:failed> -- pcap:captured ) MATCH result ok OF ENDOF err OF ENDOF ;MATCH" CHECK-QUIET-CANDIDATE! 0 T= ;

: TEST-PROC-READ-NEG-LEN ( -- )
   -1 PT-CAPTURE-OK-U !
   PT-R PT-OUT 32 >LEN PT-CAPTURE-OK-U PROC-READ-STREAM ;

: TEST-PROC-READ-HIGH-LEN ( -- )
   33 PT-CAPTURE-OK-U !
   PT-R PT-OUT 32 >LEN PT-CAPTURE-OK-U PROC-READ-STREAM ;

: PROCESS-TEST-MAIN ( -- )
   T-RESET
   PT-PREPARE
   TEST-PATHZ
   TEST-SPAWN-WAIT
   TEST-PROC-WAIT-STATUS
   TEST-PROC-FORK-WAIT
   TEST-PROC-FORK-COW
   TEST-PROC-WAIT-OUTCOME-EXIT
   TEST-PROC-WAIT-OUTCOME-SIGNAL
   TEST-PROC-OUTCOME>RC
   TEST-PROC-OUTCOME-TYPES
   TEST-PROC-WAIT-RC-SIGNAL
   TEST-WAIT-FAIL
   TEST-PIPE
   TEST-WRITE-CLOSED-PIPE-NOSIGPIPE
   TEST-PROC-NONBLOCK-ARMED
   TEST-PROC-STDIN-BACKPRESSURE
   TEST-POLL-TIMEOUT
   TEST-RUN-CAPTURE-BASIC
   TEST-RUN-ARGV-CAPTURE-BASIC
   TEST-RUN-ARGV-CAPTURE-EXACT-CAP
   [: TEST-RUN-CAPTURE-TRUNCATED ;] E-PROC-TRUNCATED TTHROWSQ
   [: TEST-RUN-ARGV-CAPTURE-TRUNCATED ;] E-PROC-TRUNCATED TTHROWSQ
   [: TEST-RUN-ARGV-CAPTURE-TIMEOUT ;] E-PROC-TIMEOUT TTHROWSQ
   [: TEST-RUN-ARGV-CAPTURE-ERR-TRUNCATED ;] E-PROC-TRUNCATED TTHROWSQ
   TEST-RUN-CAPTURE-FALSE
   TEST-RUN-ARGV-CAPTURE-FALSE
   TEST-RUN-ARGV-CAPTURE-OUTCOME-EXIT
   TEST-RUN-ARGV-CAPTURE-OUTCOME-TIMEOUT
   TEST-RUN-ARGV-CAPTURE-OUTCOME-SIGNAL
   TEST-STARVED-TIMEOUT
   TEST-RUN-ARGV-CAPTURE-HB
   TEST-RUN-CAPTURE-FD-CLEANUP
   TEST-RUN-IO-CAT
   TEST-RUN-IO-FALSE
   TEST-RUN-CAPTURE-RESULT-OK
   TEST-RUN-CAPTURE-RESULT-ERR
   TEST-RUN-CAPTURE-RESULT-TYPES
   [: TEST-PROC-READ-NEG-LEN ;] E-PROC-TRUNCATED TTHROWSQ
   [: TEST-PROC-READ-HIGH-LEN ;] E-PROC-TRUNCATED TTHROWSQ
   PT-CLEANUP
   T-REPORT
   s" process-test: ok" type cr ;

PROCESS-TEST-MAIN
