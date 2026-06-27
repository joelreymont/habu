\ process-test.f -- focused tests for lib/process.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-test.f

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

: PT-CAPTURE>N ( len len rc -- n n n ) {: outu erru rc :}
   outu LEN>N erru LEN>N rc RC>N ;

: PT-OUTCOME>N ( len len n n -- n n n n ) {: outu erru kind code :}
   outu LEN>N erru LEN>N kind code ;

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

: PT-RUN-HB-SCRIPT-OUTCOME ( ptr u8 n ptr u8 n ptr u8 n n -- n n n n )
   {: script:ptr scriptu out:ptr outcap err:ptr errcap timeout :}
   PROC-ARGV-RESET
   script scriptu  >LEN PROC-ARGV+
   s" bin/hb" >LEN out outcap >LEN err errcap >LEN timeout >MS RUN-ARGV-CAPTURE-OUTCOME
   PT-OUTCOME>N ;

: PT-RUN-CAPTURE ( ptr u8 n ptr u8 n ptr u8 n n -- n n n )
   {: path:ptr pathu out:ptr outcap err:ptr errcap timeout :}
   path pathu >LEN out outcap >LEN err errcap >LEN timeout >MS RUN-CAPTURE
   PT-CAPTURE>N ;

: PT-READ ( n -- n )
   PT-BUF 32 read ;

: TEST-SPAWN-FAIL ( -- )
   s" /no/such/habu-process-test" >LEN -1 >FD -1 >FD -1 >FD PROC-SPAWN-IO drop ;

: TEST-WAIT-BAD ( -- )
   -1 >PID PROC-WAIT-RC drop ;

: TEST-POLL-WAIT ( -- )
   PT-R @ 1 >MS POLL-IN-OR-TIMEOUT drop ;

: TEST-PATHZ ( -- )
   s" /usr/bin/true" >LEN PROC-PATHZ dup ZLEN 13 T=
   13 + c@ 0 T=
   s" /usr/bin/true" >LEN PROC-RUN-RC RC>N 0 T=
   s" /usr/bin/false" >LEN PROC-RUN-RC RC>N 1 T= ;

: TEST-SPAWN-WAIT ( -- )
   s" /usr/bin/true" >LEN -1 >FD -1 >FD -1 >FD PROC-SPAWN-IO PROC-WAIT-RC RC>N 0 T=
   [: TEST-SPAWN-FAIL ;] E-PROC-SPAWN TTHROWSQ ;

: TEST-PROC-WAIT-STATUS ( -- )
   s" /usr/bin/true" >LEN -1 >FD -1 >FD -1 >FD PROC-SPAWN-IO PROC-WAIT-STATUS 0 T=
   s" /usr/bin/false" >LEN -1 >FD -1 >FD -1 >FD PROC-SPAWN-IO PROC-WAIT-STATUS 256 T= ;

: TEST-PROC-WAIT-OUTCOME-EXIT ( -- )
   s" /usr/bin/false" >LEN -1 >FD -1 >FD -1 >FD PROC-SPAWN-IO PROC-WAIT-OUTCOME 1 T= PROC-OUTCOME-EXIT T= ;

: TEST-PROC-WAIT-OUTCOME-SIGNAL ( -- )
   PROC-ARGV-RESET
   s" -c"  >LEN PROC-ARGV+
   s" kill -TERM $$"  >LEN PROC-ARGV+
   s" /bin/sh" >LEN -1 >FD -1 >FD -1 >FD PROC-SPAWN-ARGV-IO PROC-WAIT-OUTCOME 15 T= PROC-OUTCOME-SIGNAL T= ;

: TEST-PROC-OUTCOME>RC ( -- )
   PROC-OUTCOME-EXIT 7 PROC-OUTCOME>RC RC>N 7 T=
   PROC-OUTCOME-SIGNAL SIGKILL PROC-OUTCOME>RC RC>N 137 T=
   PROC-OUTCOME-TIMEOUT SIGKILL PROC-OUTCOME>RC RC>N 137 T= ;

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

: TEST-POLL-TIMEOUT ( -- )
   PIPE-PAIR PT-W ! PT-R !
   [: TEST-POLL-WAIT ;] E-PROC-TIMEOUT TTHROWSQ
   PT-R @ close
   PT-W @ close ;

: TEST-RUN-CAPTURE-BASIC ( -- )
   s" /bin/pwd" PT-PWD-OUT 256 PT-ERR 32 1000 PT-RUN-CAPTURE 0 T= 0 T= 0 > TTRUE ;

: TEST-RUN-ARGV-CAPTURE-BASIC ( -- )
   PT-CAPTURE-OK PT-OUT 32 PT-ERR 32 1000 PT-RUN-HB-SCRIPT 7 T= 3 T= 3 T=
   PT-OUT 3 s" out" T$=
   PT-ERR 3 s" err" T$= ;

: TEST-RUN-ARGV-CAPTURE-EXACT-CAP ( -- )
   PT-CAPTURE-OK PT-OUT 3 PT-ERR 3 1000 PT-RUN-HB-SCRIPT 7 T= 3 T= 3 T=
   PT-OUT 3 s" out" T$=
   PT-ERR 3 s" err" T$= ;

: TEST-RUN-CAPTURE-TRUNCATED ( -- )
   s" /usr/bin/yes" PT-OUT 3 PT-ERR 32 1000 PT-RUN-CAPTURE 2drop drop ;

: TEST-RUN-ARGV-CAPTURE-TRUNCATED ( -- )
   PT-CAPTURE-LONG PT-OUT 3 PT-ERR 32 1000 PT-RUN-HB-SCRIPT 2drop drop ;

: TEST-RUN-ARGV-CAPTURE-TIMEOUT ( -- )
   PT-CAPTURE-HANG PT-OUT 32 PT-ERR 32 100 PT-RUN-HB-SCRIPT 2drop drop ;

: TEST-RUN-ARGV-CAPTURE-ERR-TRUNCATED ( -- )
   PT-CAPTURE-ERR-LONG PT-OUT 32 PT-ERR 3 1000 PT-RUN-HB-SCRIPT 2drop drop ;

: TEST-RUN-CAPTURE-FALSE ( -- )
   s" /usr/bin/false" PT-OUT 32 PT-ERR 32 1000 PT-RUN-CAPTURE 1 T= 0 T= 0 T= ;

: TEST-RUN-ARGV-CAPTURE-FALSE ( -- )
   PT-CAPTURE-FALSE PT-OUT 32 PT-ERR 32 1000 PT-RUN-HB-SCRIPT 1 T= 0 T= 0 T= ;

: TEST-RUN-ARGV-CAPTURE-OUTCOME-EXIT ( -- )
   PT-CAPTURE-FALSE PT-OUT 32 PT-ERR 32 1000 PT-RUN-HB-SCRIPT-OUTCOME
   1 T= PROC-OUTCOME-EXIT T= 0 T= 0 T= ;

: TEST-RUN-ARGV-CAPTURE-OUTCOME-TIMEOUT ( -- )
   PT-CAPTURE-HANG PT-OUT 32 PT-ERR 32 100 PT-RUN-HB-SCRIPT-OUTCOME
   SIGKILL T= PROC-OUTCOME-TIMEOUT T= 0 T= 0 T= ;

: TEST-RUN-ARGV-CAPTURE-HB ( -- )
   PT-CAPTURE-HB PT-OUT 32 PT-ERR 32 1000 PT-RUN-HB-SCRIPT 0 T= 0 T= 3 T=
   PT-OUT c@ 51 T=
   PT-OUT 1 + c@ 10 T=
   PT-OUT 2 + c@ 10 T= ;

: TEST-RUN-CAPTURE-FD-CLEANUP ( -- )
   0 begin dup 80 < while
      s" /usr/bin/true" PT-OUT 32 PT-ERR 32 1000 PT-RUN-CAPTURE 0 T= 0 T= 0 T=
      1+
   repeat drop ;

: TEST-RUN-IO-CAT ( -- )
   PIPE-PAIR PT-IN-W ! PT-IN-R !
   PIPE-PAIR PT-OUT-W ! PT-OUT-R !
   PT-IN-W @ s" cat-in" write 6 T=
   PT-IN-W @ close
   s" /bin/cat" >LEN PT-IN-R @ PT-OUT-W @ -1 >FD PROC-RUN-IO-RC RC>N 0 T=
   PT-IN-R @ close
   PT-OUT-W @ close
   PT-OUT-R @ PT-READ 6 T=
   PT-BUF 6 s" cat-in" T$=
   PT-OUT-R @ close ;

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
   TEST-PROC-WAIT-OUTCOME-EXIT
   TEST-PROC-WAIT-OUTCOME-SIGNAL
   TEST-PROC-OUTCOME>RC
   TEST-WAIT-FAIL
   TEST-PIPE
   TEST-WRITE-CLOSED-PIPE-NOSIGPIPE
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
   TEST-RUN-ARGV-CAPTURE-HB
   TEST-RUN-CAPTURE-FD-CLEANUP
   TEST-RUN-IO-CAT
   [: TEST-PROC-READ-NEG-LEN ;] E-PROC-TRUNCATED TTHROWSQ
   [: TEST-PROC-READ-HIGH-LEN ;] E-PROC-TRUNCATED TTHROWSQ
   PT-CLEANUP
   T-REPORT
   s" process-test: ok" type cr ;

PROCESS-TEST-MAIN
