\ process-argv-test.f - focused tests for lib/process-argv.f.
\ Run: bin/hb --load lib/errors.f lib/test.f lib/process.f lib/process-argv.f lib/process-argv-test.f

variable PAT-IN-R
variable PAT-IN-W
variable PAT-OUT-R
variable PAT-OUT-W
create PAT-BUF 64 allot
create PAT-CAP-OUT 64 allot
create PAT-CAP-ERR 32 allot
PROC-ARGV-BUF-CAP 1 + constant PAT-LONG-CAP
create PAT-LONG PAT-LONG-CAP allot
variable PAT-I

: PAT-LONG! ( -- )
   0 PAT-I !
   begin PAT-I @ PAT-LONG-CAP < while
      97 PAT-LONG PAT-I @ + c!
      PAT-I @ 1+ PAT-I !
   repeat ;

: PAT-READ ( -- n )
   PAT-OUT-R @ PAT-BUF 64 read ;

: PAT-RUN-PRINTF ( -- )
   PIPE-PAIR PAT-OUT-W ! PAT-OUT-R !
   PROC-ARGV-RESET
   s" %s-%s" PROC-ARGV+
   s" left" PROC-ARGV+
   s" right" PROC-ARGV+
   s" /usr/bin/printf" -1 PAT-OUT-W @ -1 RUN-ARGV-IO-RC 0 T=
   PAT-OUT-W @ close
   PAT-READ 10 T=
   PAT-BUF 10 s" left-right" T$=
   PAT-OUT-R @ close ;

: PAT-RUN-CAT ( -- )
   PIPE-PAIR PAT-IN-W ! PAT-IN-R !
   PIPE-PAIR PAT-OUT-W ! PAT-OUT-R !
   PAT-IN-W @ s" argv-stdin" write 10 T=
   PAT-IN-W @ close
   PROC-ARGV-RESET
   s" /bin/cat" PAT-IN-R @ PAT-OUT-W @ -1 RUN-ARGV-IO-RC 0 T=
   PAT-IN-R @ close
   PAT-OUT-W @ close
   PAT-READ 10 T=
   PAT-BUF 10 s" argv-stdin" T$=
   PAT-OUT-R @ close ;

: PAT-SPAWN-MISSING ( -- )
   PROC-ARGV-RESET
   s" ignored" PROC-ARGV+
   s" /no/such/habu-process-argv-test" -1 -1 -1 SPAWN-ARGV-IO drop ;

: PAT-TOO-MANY-ARGS ( -- )
   PROC-ARGV-RESET
   0 begin dup PROC-ARGV-MAX 1- < while
      s" x" PROC-ARGV+
      1+
   repeat drop
   s" overflow" PROC-ARGV+ ;

: PAT-LONG-ARG ( -- )
   PROC-ARGV-RESET
   s" x" PROC-ARGV+
   PAT-LONG! PAT-LONG PAT-LONG-CAP PROC-ARGV+ ;

: PAT-RUN-ARGV-CAPTURE ( -- )
   PROC-ARGV-RESET
   s" %s:%s" PROC-ARGV+
   s" left" PROC-ARGV+
   s" right" PROC-ARGV+
   s" /usr/bin/printf" PAT-CAP-OUT 64 PAT-CAP-ERR 32 1000 RUN-ARGV-CAPTURE
   0 T= 0 T= 10 T=
   PAT-CAP-OUT 10 s" left:right" T$=
   PROC-ARGV-N @ 0 T= ;

: PAT-RUN-ARGV-CAPTURE-EXACT ( -- )
   PROC-ARGV-RESET
   s" %s" PROC-ARGV+
   s" abc" PROC-ARGV+
   s" /usr/bin/printf" PAT-CAP-OUT 3 PAT-CAP-ERR 0 1000 RUN-ARGV-CAPTURE
   0 T= 0 T= 3 T=
   PAT-CAP-OUT 3 s" abc" T$= ;

: PAT-RUN-ARGV-CAPTURE-TRUNCATED ( -- )
   PROC-ARGV-RESET
   s" %s" PROC-ARGV+
   s" abcd" PROC-ARGV+
   s" /usr/bin/printf" PAT-CAP-OUT 3 PAT-CAP-ERR 0 1000 RUN-ARGV-CAPTURE
   2drop drop ;

: PAT-RUN-ARGV-STDIN-CAPTURE-CAT ( -- )
   PROC-ARGV-RESET
   s" /bin/cat" s" stdin-echo" PAT-CAP-OUT 64 PAT-CAP-ERR 32 1000 RUN-ARGV-STDIN-CAPTURE
   0 T= 0 T= 10 T=
   PAT-CAP-OUT 10 s" stdin-echo" T$=
   PROC-ARGV-N @ 0 T= ;

: PAT-RUN-ARGV-STDIN-CAPTURE-EMPTY ( -- )
   PROC-ARGV-RESET
   s" /bin/cat" s" " PAT-CAP-OUT 64 PAT-CAP-ERR 32 1000 RUN-ARGV-STDIN-CAPTURE
   0 T= 0 T= 0 T= ;

: PAT-RUN-ARGV-STDIN-CAPTURE-FALSE ( -- )
   PROC-ARGV-RESET
   s" /usr/bin/false" s" " PAT-CAP-OUT 64 PAT-CAP-ERR 32 1000 RUN-ARGV-STDIN-CAPTURE
   1 T= 0 T= 0 T= ;

: PAT-RUN-ARGV-STDIN-CAPTURE-TRUNCATED ( -- )
   PROC-ARGV-RESET
   s" /bin/cat" s" abcd" PAT-CAP-OUT 3 PAT-CAP-ERR 32 1000 RUN-ARGV-STDIN-CAPTURE
   2drop drop ;

: PAT-RUN-ARGV-STDIN-CAPTURE-TIMEOUT ( -- )
   PROC-ARGV-RESET
   s" 5" PROC-ARGV+
   s" /bin/sleep" s" " PAT-CAP-OUT 64 PAT-CAP-ERR 32 50 RUN-ARGV-STDIN-CAPTURE
   2drop drop ;

: PAT-RUN-ARGV-STDIN-CAPTURE-OUTCOME-CAT ( -- )
   PROC-ARGV-RESET
   s" /bin/cat" s" stdin-outcome" PAT-CAP-OUT 64 PAT-CAP-ERR 32 1000 RUN-ARGV-STDIN-CAPTURE-OUTCOME
   0 T= PROC-OUTCOME-EXIT T= 0 T= 13 T=
   PAT-CAP-OUT 13 s" stdin-outcome" T$= ;

: PAT-RUN-ARGV-STDIN-CAPTURE-OUTCOME-TIMEOUT ( -- )
   PROC-ARGV-RESET
   s" 5" PROC-ARGV+
   s" /bin/sleep" s" " PAT-CAP-OUT 64 PAT-CAP-ERR 32 50 RUN-ARGV-STDIN-CAPTURE-OUTCOME
   SIGKILL T= PROC-OUTCOME-TIMEOUT T= 0 T= 0 T= ;

: PROCESS-ARGV-TEST-MAIN ( -- )
   T-RESET
   PAT-RUN-PRINTF
   PAT-RUN-CAT
   ['] PAT-SPAWN-MISSING E-PROC-SPAWN TTHROWS
   ['] PAT-TOO-MANY-ARGS E-PROC-OUTPUT TTHROWS
   ['] PAT-LONG-ARG E-PROC-OUTPUT TTHROWS
   PAT-RUN-ARGV-CAPTURE
   PAT-RUN-ARGV-CAPTURE-EXACT
   ['] PAT-RUN-ARGV-CAPTURE-TRUNCATED E-PROC-TRUNCATED TTHROWS
   PAT-RUN-ARGV-STDIN-CAPTURE-CAT
   PAT-RUN-ARGV-STDIN-CAPTURE-EMPTY
   PAT-RUN-ARGV-STDIN-CAPTURE-FALSE
   ['] PAT-RUN-ARGV-STDIN-CAPTURE-TRUNCATED E-PROC-TRUNCATED TTHROWS
   ['] PAT-RUN-ARGV-STDIN-CAPTURE-TIMEOUT E-PROC-TIMEOUT TTHROWS
   PAT-RUN-ARGV-STDIN-CAPTURE-OUTCOME-CAT
   PAT-RUN-ARGV-STDIN-CAPTURE-OUTCOME-TIMEOUT
   T-REPORT
   s" process-argv-test: ok" type cr ;

PROCESS-ARGV-TEST-MAIN
