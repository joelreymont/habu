\ process-argv-test.f - focused tests for lib/process-argv.f.
\ Run: bin/hb --load lib/errors.f lib/test.f lib/process.f lib/process-argv.f lib/process-argv-test.f

variable PAT-IN-R
variable PAT-IN-W
variable PAT-OUT-R
variable PAT-OUT-W
create PAT-BUF 64 allot
create PAT-CAP-OUT 64 allot
create PAT-CAP-ERR 32 allot
7 constant PAT-WC-COUNT-U
create PAT-WC-COUNT 49 c, 51 c, 49 c, 48 c, 55 c, 50 c, 10 c,
PROC-ARGV-BUF-CAP 1 + constant PAT-LONG-CAP
131072 constant PAT-EARLY-IN-CAP
create PAT-LONG PAT-LONG-CAP allot
create PAT-EARLY-IN PAT-EARLY-IN-CAP allot
variable PAT-I
32 constant PAT-SPACE

: PAT-LONG! ( -- )
   0 PAT-I !
   begin PAT-I @ PAT-LONG-CAP < while
      97 PAT-LONG PAT-I @ + c!
      PAT-I @ 1+ PAT-I !
   repeat ;

: PAT-EARLY-IN! ( -- )
   0 PAT-I !
   begin PAT-I @ PAT-EARLY-IN-CAP < while
      97 PAT-EARLY-IN PAT-I @ + c!
      PAT-I @ 1+ PAT-I !
   repeat ;

: PAT-READ ( -- n )
   PAT-OUT-R @ PAT-BUF 64 read ;

: PAT-CAPTURE>N ( len len rc -- n n n ) {: outu erru rc :}
   outu LEN>N erru LEN>N rc RC>N ;

: PAT-OUTCOME>N ( len len n n -- n n n n ) {: outu erru kind code :}
   outu LEN>N erru LEN>N kind code ;

: PAT-LTRIM-SPACES ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   0 begin dup u < while
      dup a + c@ PAT-SPACE <> if dup a + u rot - exit then
      1+
   repeat drop a 0 ;

: PAT-CAPTURE ( ptr u8 n ptr u8 n ptr u8 n n -- n n n )
   {: path:ptr pathu out:ptr outcap err:ptr errcap timeout :}
   path pathu >LEN out outcap >LEN err errcap >LEN timeout >MS RUN-ARGV-CAPTURE
   PAT-CAPTURE>N ;

: PAT-STDIN-CAPTURE ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n n -- n n n )
   {: path:ptr pathu in:ptr inu out:ptr outcap err:ptr errcap timeout :}
   path pathu >LEN in inu >LEN out outcap >LEN err errcap >LEN timeout >MS RUN-ARGV-STDIN-CAPTURE
   PAT-CAPTURE>N ;

: PAT-STDIN-CAPTURE-OUTCOME ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n n -- n n n n )
   {: path:ptr pathu in:ptr inu out:ptr outcap err:ptr errcap timeout :}
   path pathu >LEN in inu >LEN out outcap >LEN err errcap >LEN timeout >MS RUN-ARGV-STDIN-CAPTURE-OUTCOME
   PAT-OUTCOME>N ;

: PAT-CHECK-WC-COUNT ( n n n -- ) {: outu erru rc :}
   rc 0 T=
   erru 0 T=
   PAT-CAP-OUT outu PAT-LTRIM-SPACES PAT-WC-COUNT PAT-WC-COUNT-U T$= ;

: PAT-RUN-PRINTF ( -- )
   PIPE-PAIR PAT-OUT-W ! PAT-OUT-R !
   PROC-ARGV-RESET
   s" %s-%s"  >LEN PROC-ARGV+
   s" left"  >LEN PROC-ARGV+
   s" right"  >LEN PROC-ARGV+
   s" /usr/bin/printf" >LEN -1 >FD PAT-OUT-W @ -1 >FD RUN-ARGV-IO-RC RC>N 0 T=
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
   s" /bin/cat" >LEN PAT-IN-R @ PAT-OUT-W @ -1 >FD RUN-ARGV-IO-RC RC>N 0 T=
   PAT-IN-R @ close
   PAT-OUT-W @ close
   PAT-READ 10 T=
   PAT-BUF 10 s" argv-stdin" T$=
   PAT-OUT-R @ close ;

: PAT-SPAWN-MISSING ( -- )
   PROC-ARGV-RESET
   s" ignored"  >LEN PROC-ARGV+
   s" /no/such/habu-process-argv-test" >LEN -1 >FD -1 >FD -1 >FD SPAWN-ARGV-IO drop ;

: PAT-TOO-MANY-ARGS ( -- )
   PROC-ARGV-RESET
   0 begin dup PROC-ARGV-MAX 1- < while
      s" x"  >LEN PROC-ARGV+
      1+
   repeat drop
   s" overflow"  >LEN PROC-ARGV+ ;

: PAT-LONG-ARG ( -- )
   PROC-ARGV-RESET
   s" x"  >LEN PROC-ARGV+
   PAT-LONG! PAT-LONG PAT-LONG-CAP  >LEN PROC-ARGV+ ;

: PAT-RUN-ARGV-CAPTURE ( -- )
   PROC-ARGV-RESET
   s" %s:%s"  >LEN PROC-ARGV+
   s" left"  >LEN PROC-ARGV+
   s" right"  >LEN PROC-ARGV+
   s" /usr/bin/printf" PAT-CAP-OUT 64 PAT-CAP-ERR 32 1000 PAT-CAPTURE
   0 T= 0 T= 10 T=
   PAT-CAP-OUT 10 s" left:right" T$=
   PROC-ARGV-N @ 0 T= ;

: PAT-RUN-ARGV-CAPTURE-EXACT ( -- )
   PROC-ARGV-RESET
   s" %s"  >LEN PROC-ARGV+
   s" abc"  >LEN PROC-ARGV+
   s" /usr/bin/printf" PAT-CAP-OUT 3 PAT-CAP-ERR 0 1000 PAT-CAPTURE
   0 T= 0 T= 3 T=
   PAT-CAP-OUT 3 s" abc" T$= ;

: PAT-RUN-ARGV-CAPTURE-TRUNCATED ( -- )
   PROC-ARGV-RESET
   s" %s"  >LEN PROC-ARGV+
   s" abcd"  >LEN PROC-ARGV+
   s" /usr/bin/printf" PAT-CAP-OUT 3 PAT-CAP-ERR 0 1000 PAT-CAPTURE
   2drop drop ;

: PAT-RUN-ARGV-STDIN-CAPTURE-CAT ( -- )
   PROC-ARGV-RESET
   s" /bin/cat" s" stdin-echo" PAT-CAP-OUT 64 PAT-CAP-ERR 32 1000 PAT-STDIN-CAPTURE
   0 T= 0 T= 10 T=
   PAT-CAP-OUT 10 s" stdin-echo" T$=
   PROC-ARGV-N @ 0 T= ;

: PAT-RUN-ARGV-STDIN-CAPTURE-EMPTY ( -- )
   PROC-ARGV-RESET
   s" /bin/cat" s" " PAT-CAP-OUT 64 PAT-CAP-ERR 32 1000 PAT-STDIN-CAPTURE
   0 T= 0 T= 0 T= ;

: PAT-RUN-ARGV-STDIN-CAPTURE-FALSE ( -- )
   PROC-ARGV-RESET
   s" /usr/bin/false" s" " PAT-CAP-OUT 64 PAT-CAP-ERR 32 1000 PAT-STDIN-CAPTURE
   1 T= 0 T= 0 T= ;

: PAT-RUN-ARGV-STDIN-CAPTURE-FALSE-LARGE ( -- )
   PROC-ARGV-RESET
   PAT-EARLY-IN!
   s" /usr/bin/false" PAT-EARLY-IN PAT-EARLY-IN-CAP
   PAT-CAP-OUT 64 PAT-CAP-ERR 32 1000
   PAT-STDIN-CAPTURE
   1 T= 0 T= 0 T= ;

: PAT-RUN-ARGV-STDIN-CAPTURE-WC-LARGE ( -- )
   PROC-ARGV-RESET
   PAT-EARLY-IN!
   s" -c"  >LEN PROC-ARGV+
   s" /usr/bin/wc" PAT-EARLY-IN PAT-EARLY-IN-CAP
   PAT-CAP-OUT 64 PAT-CAP-ERR 32 1000
   PAT-STDIN-CAPTURE
   PAT-CHECK-WC-COUNT ;

: PAT-RUN-ARGV-STDIN-CAPTURE-TRUNCATED ( -- )
   PROC-ARGV-RESET
   s" /bin/cat" s" abcd" PAT-CAP-OUT 3 PAT-CAP-ERR 32 1000 PAT-STDIN-CAPTURE
   2drop drop ;

: PAT-RUN-ARGV-STDIN-CAPTURE-TIMEOUT ( -- )
   PROC-ARGV-RESET
   s" 5"  >LEN PROC-ARGV+
   s" /bin/sleep" s" " PAT-CAP-OUT 64 PAT-CAP-ERR 32 50 PAT-STDIN-CAPTURE
   2drop drop ;

: PAT-RUN-ARGV-STDIN-CAPTURE-OUTCOME-CAT ( -- )
   PROC-ARGV-RESET
   s" /bin/cat" s" stdin-outcome" PAT-CAP-OUT 64 PAT-CAP-ERR 32 1000 PAT-STDIN-CAPTURE-OUTCOME
   0 T= PROC-OUTCOME-EXIT T= 0 T= 13 T=
   PAT-CAP-OUT 13 s" stdin-outcome" T$= ;

: PAT-RUN-ARGV-STDIN-CAPTURE-OUTCOME-TIMEOUT ( -- )
   PROC-ARGV-RESET
   s" 5"  >LEN PROC-ARGV+
   s" /bin/sleep" s" " PAT-CAP-OUT 64 PAT-CAP-ERR 32 50 PAT-STDIN-CAPTURE-OUTCOME
   SIGKILL T= PROC-OUTCOME-TIMEOUT T= 0 T= 0 T= ;

: PAT-RUN-ARGV-STDIN-CAPTURE-OUTCOME-FALSE-LARGE ( -- )
   PROC-ARGV-RESET
   PAT-EARLY-IN!
   s" /usr/bin/false" PAT-EARLY-IN PAT-EARLY-IN-CAP
   PAT-CAP-OUT 64 PAT-CAP-ERR 32 1000
   PAT-STDIN-CAPTURE-OUTCOME
   1 T= PROC-OUTCOME-EXIT T= 0 T= 0 T= ;

: PAT-READ-NEG-LEN ( -- )
   -1 PAT-I !
   PAT-IN-R PAT-CAP-OUT 64 >LEN PAT-I PROC-READ-STREAM ;

: PAT-READ-HIGH-LEN ( -- )
   65 PAT-I !
   PAT-IN-R PAT-CAP-OUT 64 >LEN PAT-I PROC-READ-STREAM ;

: PROCESS-ARGV-TEST-MAIN ( -- )
   T-RESET
   PAT-RUN-PRINTF
   PAT-RUN-CAT
   [: PAT-SPAWN-MISSING ;] E-PROC-SPAWN TTHROWSQ
   [: PAT-TOO-MANY-ARGS ;] E-PROC-OUTPUT TTHROWSQ
   [: PAT-LONG-ARG ;] E-PROC-OUTPUT TTHROWSQ
   PAT-RUN-ARGV-CAPTURE
   PAT-RUN-ARGV-CAPTURE-EXACT
   [: PAT-RUN-ARGV-CAPTURE-TRUNCATED ;] E-PROC-TRUNCATED TTHROWSQ
   PAT-RUN-ARGV-STDIN-CAPTURE-CAT
   PAT-RUN-ARGV-STDIN-CAPTURE-EMPTY
   PAT-RUN-ARGV-STDIN-CAPTURE-FALSE
   PAT-RUN-ARGV-STDIN-CAPTURE-FALSE-LARGE
   PAT-RUN-ARGV-STDIN-CAPTURE-WC-LARGE
   [: PAT-RUN-ARGV-STDIN-CAPTURE-TRUNCATED ;] E-PROC-TRUNCATED TTHROWSQ
   [: PAT-RUN-ARGV-STDIN-CAPTURE-TIMEOUT ;] E-PROC-TIMEOUT TTHROWSQ
   PAT-RUN-ARGV-STDIN-CAPTURE-OUTCOME-CAT
   PAT-RUN-ARGV-STDIN-CAPTURE-OUTCOME-FALSE-LARGE
   PAT-RUN-ARGV-STDIN-CAPTURE-OUTCOME-TIMEOUT
   [: PAT-READ-NEG-LEN ;] E-PROC-TRUNCATED TTHROWSQ
   [: PAT-READ-HIGH-LEN ;] E-PROC-TRUNCATED TTHROWSQ
   T-REPORT
   s" process-argv-test: ok" type cr ;

PROCESS-ARGV-TEST-MAIN
