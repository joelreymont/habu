\ process-test.f -- focused tests for lib/process.f.
\ Run: cat lib/errors.f lib/test.f lib/process.f lib/process-test.f | bin/hb

0 set-check

variable PT-R
variable PT-W
create PT-BUF 32 allot

: PT-READ ( fd -- n )
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

: PROCESS-TEST-MAIN ( -- )
   T-RESET
   TEST-PATHZ
   TEST-SPAWN-WAIT
   TEST-WAIT-FAIL
   TEST-PIPE
   TEST-POLL-TIMEOUT
   T-REPORT
   s" process-test: ok" type cr ;

PROCESS-TEST-MAIN
