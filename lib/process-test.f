\ process-test.f -- focused tests for lib/process.f.
\ Run: cat lib/errors.f lib/test.f lib/process.f lib/process-test.f | bin/hb

variable PT-R
variable PT-W
variable PT-IN-R
variable PT-IN-W
variable PT-OUT-R
variable PT-OUT-W
create PT-BUF 32 allot
create PT-OUT 32 allot
create PT-ERR 32 allot

: PT-READ ( n -- n )
   PT-BUF 32 read ;

: TEST-SPAWN-FAIL ( -- )
   s" /no/such/habu-process-test" -1 -1 -1 SPAWN-IO drop ;

: TEST-WAIT-BAD ( -- )
   -1 WAIT-RC drop ;

: TEST-POLL-WAIT ( -- )
   PT-R @ 1 POLL-IN-OR-TIMEOUT drop ;

: TEST-PATHZ ( -- )
   s" /usr/bin/true" PATHZ dup ZLEN 13 T=
   13 + c@ 0 T=
   s" /usr/bin/true" RUN-RC 0 T=
   s" /usr/bin/false" RUN-RC 1 T= ;

: TEST-SPAWN-WAIT ( -- )
   s" /usr/bin/true" -1 -1 -1 SPAWN-IO WAIT-RC 0 T=
   ['] TEST-SPAWN-FAIL E-PROC-SPAWN TTHROWS ;

: TEST-WAIT-FAIL ( -- )
   ['] TEST-WAIT-BAD E-PROC-WAIT TTHROWS ;

: TEST-PIPE ( -- )
   PIPE-PAIR PT-W ! PT-R !
   PT-R @ FD-CLOEXEC!
   PT-W @ FD-CLOEXEC!
   PT-R @ 0 POLL-IN 0 T=
   PT-W @ s" x" write 1 T=
   PT-R @ 100 POLL-IN 1 T=
   PT-R @ PT-READ 1 T=
   PT-R @ close
   PT-W @ close ;

: TEST-POLL-TIMEOUT ( -- )
   PIPE-PAIR PT-W ! PT-R !
   ['] TEST-POLL-WAIT E-PROC-TIMEOUT TTHROWS
   PT-R @ close
   PT-W @ close ;

: PT-CAPTURE-OK ( -- ptr u8 n )
   0 SCRIPT-ARGV$ ;

: PT-CAPTURE-LONG ( -- ptr u8 n )
   1 SCRIPT-ARGV$ ;

: PT-CAPTURE-SLEEP ( -- ptr u8 n )
   2 SCRIPT-ARGV$ ;

: PT-CAPTURE-ERR-LONG ( -- ptr u8 n )
   3 SCRIPT-ARGV$ ;

: PT-CAPTURE-FALSE ( -- ptr u8 n )
   4 SCRIPT-ARGV$ ;

: PT-CAPTURE-HB ( -- ptr u8 n )
   5 SCRIPT-ARGV$ ;

: TEST-RUN-CAPTURE-BASIC ( -- )
   PT-CAPTURE-OK PT-OUT 32 PT-ERR 32 1000 RUN-CAPTURE 7 T= 3 T= 3 T=
   PT-OUT 3 s" out" T$=
   PT-ERR 3 s" err" T$= ;

: TEST-RUN-CAPTURE-EXACT-CAP ( -- )
   PT-CAPTURE-OK PT-OUT 3 PT-ERR 3 1000 RUN-CAPTURE 7 T= 3 T= 3 T=
   PT-OUT 3 s" out" T$=
   PT-ERR 3 s" err" T$= ;

: TEST-RUN-CAPTURE-TRUNCATED ( -- )
   PT-CAPTURE-LONG PT-OUT 3 PT-ERR 32 1000 RUN-CAPTURE 2drop drop ;

: TEST-RUN-CAPTURE-TIMEOUT ( -- )
   PT-CAPTURE-SLEEP PT-OUT 32 PT-ERR 32 100 RUN-CAPTURE 2drop drop ;

: TEST-RUN-CAPTURE-ERR-TRUNCATED ( -- )
   PT-CAPTURE-ERR-LONG PT-OUT 32 PT-ERR 3 1000 RUN-CAPTURE 2drop drop ;

: TEST-RUN-CAPTURE-FALSE ( -- )
   PT-CAPTURE-FALSE PT-OUT 32 PT-ERR 32 1000 RUN-CAPTURE 1 T= 0 T= 0 T= ;

: TEST-RUN-CAPTURE-HB ( -- )
   PT-CAPTURE-HB PT-OUT 32 PT-ERR 32 1000 RUN-CAPTURE 0 T= 0 T= 3 T=
   PT-OUT c@ 51 T=
   PT-OUT 1 + c@ 10 T=
   PT-OUT 2 + c@ 10 T= ;

: TEST-RUN-CAPTURE-FD-CLEANUP ( -- )
   0 begin dup 80 < while
      PT-CAPTURE-OK PT-OUT 32 PT-ERR 32 1000 RUN-CAPTURE 7 T= 3 T= 3 T=
      1+
   repeat drop ;

: TEST-RUN-IO-CAT ( -- )
   PIPE-PAIR PT-IN-W ! PT-IN-R !
   PIPE-PAIR PT-OUT-W ! PT-OUT-R !
   PT-IN-W @ s" cat-in" write 6 T=
   PT-IN-W @ close
   s" /bin/cat" PT-IN-R @ PT-OUT-W @ -1 RUN-IO-RC 0 T=
   PT-IN-R @ close
   PT-OUT-W @ close
   PT-OUT-R @ PT-READ 6 T=
   PT-BUF 6 s" cat-in" T$=
   PT-OUT-R @ close ;

: PROCESS-TEST-MAIN ( -- )
   T-RESET
   SCRIPT-ARGC 6 < if s" process-test: missing fixture args" T-EX-FAIL die then
   TEST-PATHZ
   TEST-SPAWN-WAIT
   TEST-WAIT-FAIL
   TEST-PIPE
   TEST-POLL-TIMEOUT
   TEST-RUN-CAPTURE-BASIC
   TEST-RUN-CAPTURE-EXACT-CAP
   ['] TEST-RUN-CAPTURE-TRUNCATED E-PROC-TRUNCATED TTHROWS
   ['] TEST-RUN-CAPTURE-TIMEOUT E-PROC-TIMEOUT TTHROWS
   ['] TEST-RUN-CAPTURE-ERR-TRUNCATED E-PROC-TRUNCATED TTHROWS
   TEST-RUN-CAPTURE-FALSE
   TEST-RUN-CAPTURE-HB
   TEST-RUN-CAPTURE-FD-CLEANUP
   TEST-RUN-IO-CAT
   T-REPORT
   s" process-test: ok" type cr ;

PROCESS-TEST-MAIN
