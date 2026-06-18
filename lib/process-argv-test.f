\ process-argv-test.f - focused tests for lib/process-argv.f.
\ Run: lib/process-argv-test.sh

0 set-check

variable PAT-IN-R
variable PAT-IN-W
variable PAT-OUT-R
variable PAT-OUT-W
create PAT-BUF 64 allot

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
   0 SCRIPT-ARGV$ PROC-ARGV+ ;

: PROCESS-ARGV-TEST-MAIN ( -- )
   T-RESET
   SCRIPT-ARGC 1 < if s" process-argv-test: missing fixture arg" T-EX-FAIL die then
   PAT-RUN-PRINTF
   PAT-RUN-CAT
   ['] PAT-SPAWN-MISSING E-PROC-SPAWN TTHROWS
   ['] PAT-TOO-MANY-ARGS E-PROC-OUTPUT TTHROWS
   ['] PAT-LONG-ARG E-PROC-OUTPUT TTHROWS
   T-REPORT
   s" process-argv-test: ok" type cr ;

PROCESS-ARGV-TEST-MAIN
