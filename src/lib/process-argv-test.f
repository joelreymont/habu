\ process-argv-test.f - focused tests for src/lib/process-argv.f.
\ Run: bin/hb --load src/lib/errors.f src/lib/test.f src/lib/process.f src/lib/process-argv.f src/lib/process-argv-test.f

require src/lib/errors.f
require src/lib/test.f
require src/lib/memory.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/process-cwd.f
require src/lib/engine-candidate.f
require src/lib/test/outcome.f

variable PAT-IN-R
variable PAT-IN-W
variable PAT-OUT-R
variable PAT-OUT-W
create PAT-BUF 64 allot
create PAT-CAP-OUT 64 allot
create PAT-CAP-ERR 32 allot
7 constant PAT-WC-COUNT-U
create PAT-WC-COUNT 49 c, 51 c, 49 c, 48 c, 55 c, 50 c, 10 c,
\ Each long argument spans more than one 4096-byte chunk and stays under Linux's
\ 128 KiB single-argument limit; together they outgrow $100 rows that each hold
\ a PATH-CAP path and its NUL.
100000 constant PAT-LONG-CAP
3 constant PAT-LONG-N
PAT-LONG-CAP 1 + PAT-LONG-N * 64 + constant PAT-BIG-CAP
create PAT-BIG-OUT PAT-BIG-CAP allot
variable PAT-AT
\ More than $100 arguments, whose bytes together fill more than one 4096-byte page.
300 constant PAT-MANY-N
16 constant PAT-ARG-TAIL
4 PAT-ARG-TAIL + constant PAT-ARG-U
create PAT-ARG PAT-ARG-U allot
131072 constant PAT-EARLY-IN-CAP
create PAT-LONG PAT-LONG-CAP allot
create PAT-EARLY-IN PAT-EARLY-IN-CAP allot
variable PAT-I
32 constant PAT-SPACE

: PAT-LONG! ( u8 -- )
   {: ch:u8 :}
   0 PAT-I !
   begin PAT-I @ PAT-LONG-CAP < while
      ch PAT-LONG PAT-I @ + c!
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

: PAT-CAPTURE>N ( result<pcap:captured,pcap:failed> -- n n n )   \ outn errn code (0 on clean exit)
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} o LEN>N e LEN>N 0 ENDOF
     err OF PCAP-FAILED:UNMAKE  {: o:len e:len c:rc :} o LEN>N e LEN>N c RC>N ENDOF
   ;MATCH ;

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

: PAT-STDIN-CAPTURE-OUTCOME ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n n -- len len outcome )
   {: path:ptr pathu in:ptr inu out:ptr outcap err:ptr errcap timeout :}
   path pathu >LEN in inu >LEN out outcap >LEN err errcap >LEN timeout >MS RUN-ARGV-STDIN-CAPTURE-OUTCOME ;

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
   s" /usr/bin/printf" >LEN -1 >FD PAT-OUT-W @ -1 >FD PROC-RUN-ARGV-IO-RC
   MATCH result ok OF 0 T= ENDOF err OF drop 1 0 T= ENDOF ;MATCH
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
   s" /bin/cat" >LEN PAT-IN-R @ PAT-OUT-W @ -1 >FD PROC-RUN-ARGV-IO-RC
   MATCH result ok OF 0 T= ENDOF err OF drop 1 0 T= ENDOF ;MATCH
   PAT-IN-R @ close
   PAT-OUT-W @ close
   PAT-READ 10 T=
   PAT-BUF 10 s" argv-stdin" T$=
   PAT-OUT-R @ close ;

: PAT-SPAWN-MISSING ( -- )
   PROC-ARGV-RESET
   s" ignored"  >LEN PROC-ARGV+
   s" /no/such/habu-process-argv-test" >LEN -1 >FD -1 >FD -1 >FD PROC-SPAWN-ARGV-IO drop ;

: PAT-ARG! ( n -- )
   {: k:n :}
   k 1000 / 10 mod 48 + PAT-ARG c!
   k 100 / 10 mod 48 + PAT-ARG 1 + c!
   k 10 / 10 mod 48 + PAT-ARG 2 + c!
   k 10 mod 48 + PAT-ARG 3 + c!
   PAT-ARG-TAIL 0 ?do k 26 mod 97 + PAT-ARG 4 + i + c! loop ;


\ printf "%s," echoes each staged argument followed by a comma.
: PAT-PRINTF-ARGS ( -- n )
   s" /usr/bin/printf" PAT-BIG-OUT PAT-BIG-CAP PAT-CAP-ERR 32 5000 PAT-CAPTURE
   {: outu:n erru:n rc:n :}
   rc 0 T= erru 0 T= outu ;


\ The output at PAT-AT is these bytes and a comma; PAT-AT moves past them.
: PAT-NEXT ( ptr u8 n -- )
   {: a:ptr u:n :}
   PAT-BIG-OUT PAT-AT @ + u a u T$=
   PAT-BIG-OUT PAT-AT @ u + + c@ 44 T=
   PAT-AT @ u 1+ + PAT-AT ! ;


: PAT-MANY-ARGS ( -- )
   s" more than $100 arguments reach the child in order" T-LABEL
   PROC-ARGV-RESET
   s" %s," >LEN PROC-ARGV+
   PAT-MANY-N 0 ?do i PAT-ARG! PAT-ARG PAT-ARG-U >LEN PROC-ARGV+ loop
   PAT-PRINTF-ARGS {: outu:n :}
   0 PAT-AT !
   PAT-MANY-N 0 ?do i PAT-ARG! PAT-ARG PAT-ARG-U PAT-NEXT loop
   PAT-AT @ outu T= ;


: PAT-LONG-ARGS ( -- )
   s" long arguments past $100 paths together reach the child whole" T-LABEL
   PROC-ARGV-RESET
   s" %s," >LEN PROC-ARGV+
   s" x" >LEN PROC-ARGV+
   PAT-LONG-N 0 ?do i 97 + PAT-LONG! PAT-LONG PAT-LONG-CAP >LEN PROC-ARGV+ loop
   s" y" >LEN PROC-ARGV+
   PAT-PRINTF-ARGS {: outu:n :}
   0 PAT-AT !
   s" x" PAT-NEXT
   PAT-LONG-N 0 ?do i 97 + PAT-LONG! PAT-LONG PAT-LONG-CAP PAT-NEXT loop
   s" y" PAT-NEXT
   PAT-AT @ outu T= ;

: PAT-NEG-ARG ( -- )
   PROC-ARGV-RESET PAT-LONG -1 >LEN PROC-ARGV+ ;

: PAT-HUGE-ARG ( -- )
   PROC-ARGV-RESET PAT-LONG MEM-MAX-N >LEN PROC-ARGV+ ;

\ The path rule can fail the same ways: an exact fill refused, one byte past it
\ taken, a negative length taken, and a length whose NUL wraps the sum back into
\ range.
: PAT-PATH-BOUNDS ( -- )
   s" a path fits when its bytes and NUL fit PROC-PATHZ-CAP" T-LABEL
   [: PAT-LONG PROC-PATHZ-CAP 1- >LEN PROC-ARGV-CHECK-PATH ;] catch 0 T=
   [: PAT-LONG PROC-PATHZ-CAP >LEN PROC-ARGV-CHECK-PATH ;] E-PROC-OUTPUT TTHROWSQ
   [: PAT-LONG -1 >LEN PROC-ARGV-CHECK-PATH ;] E-PROC-OUTPUT TTHROWSQ
   [: PAT-LONG MEM-MAX-N >LEN PROC-ARGV-CHECK-PATH ;] E-PROC-OUTPUT TTHROWSQ ;

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
   {: outu:len erru:len oc :}
   s" /bin/cat" PAT-CAP-OUT outu LEN>N PAT-CAP-ERR erru LEN>N oc 0 T-OUTCOME-EXITED=
   erru LEN>N 0 T= outu LEN>N 13 T=
   PAT-CAP-OUT 13 s" stdin-outcome" T$= ;

: PAT-RUN-ARGV-STDIN-CAPTURE-OUTCOME-TIMEOUT ( -- )
   PROC-ARGV-RESET
   s" 5"  >LEN PROC-ARGV+
   s" /bin/sleep" s" " PAT-CAP-OUT 64 PAT-CAP-ERR 32 50 PAT-STDIN-CAPTURE-OUTCOME
   T-OUTCOME-TIMEOUT LEN>N 0 T= LEN>N 0 T= ;

: PAT-RUN-ARGV-STDIN-CAPTURE-OUTCOME-FALSE-LARGE ( -- )
   PROC-ARGV-RESET
   PAT-EARLY-IN!
   s" /usr/bin/false" PAT-EARLY-IN PAT-EARLY-IN-CAP
   PAT-CAP-OUT 64 PAT-CAP-ERR 32 1000
   PAT-STDIN-CAPTURE-OUTCOME {: outu:len erru:len oc :}
   s" /usr/bin/false" PAT-CAP-OUT outu LEN>N PAT-CAP-ERR erru LEN>N oc 1 T-OUTCOME-EXITED=
   erru LEN>N 0 T= outu LEN>N 0 T= ;

\ Direct both-arm coverage for RUN-ARGV-CAPTURE: printf exits clean -> ok(captured)
\ carrying the two lengths; false exits 1 -> err(failed) carrying lengths + code.
: PAT-RUN-ARGV-CAPTURE-RESULT ( -- )
   PROC-ARGV-RESET
   s" %s" >LEN PROC-ARGV+  s" hi" >LEN PROC-ARGV+
   s" /usr/bin/printf" >LEN PAT-CAP-OUT 64 >LEN PAT-CAP-ERR 32 >LEN 1000 >MS RUN-ARGV-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} o LEN>N 2 T=  e LEN>N 0 T= ENDOF
     err OF PCAP-FAILED:UNMAKE 2drop drop 1 0 T= ENDOF
   ;MATCH
   PROC-ARGV-RESET
   s" /usr/bin/false" >LEN PAT-CAP-OUT 64 >LEN PAT-CAP-ERR 32 >LEN 1000 >MS RUN-ARGV-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE 2drop 1 0 T= ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :} o LEN>N 0 T=  e LEN>N 0 T=  c RC>N 1 T= ENDOF
   ;MATCH ;

: PAT-READ-NEG-LEN ( -- )
   -1 PAT-I !
   PAT-IN-R PAT-CAP-OUT 64 >LEN PAT-I PROC-READ-STREAM ;

: PAT-READ-HIGH-LEN ( -- )
   65 PAT-I !
   PAT-IN-R PAT-CAP-OUT 64 >LEN PAT-I PROC-READ-STREAM ;

variable PAT-SAVED-STDIN

: PAT-EOF-RESULT ( result<pcap:captured,pcap:failed> -- )
   PAT-CAPTURE>N 0 T= 0 T= 0 T= ;

\ A bare engine waits for stdin EOF. The parent keeps the pipe writer open,
\ so inheriting its stdin times out; each default capture must supply EOF.
: PAT-IDLE-CAPTURES ( -- )
   PROC-ARGV-RESET
   ENGINE-CANDIDATE:PATH$ >LEN PAT-CAP-OUT 64 >LEN PAT-CAP-ERR 32 >LEN
   3000 >MS RUN-ARGV-CAPTURE PAT-EOF-RESULT
   ENGINE-CANDIDATE:PATH$ >LEN PAT-CAP-OUT 64 >LEN PAT-CAP-ERR 32 >LEN
   3000 >MS RUN-CAPTURE PAT-EOF-RESULT
   PROC-ARGV-RESET PROC-ENV-RESET
   ENGINE-CANDIDATE:PATH$ >LEN PAT-CAP-OUT 64 >LEN PAT-CAP-ERR 32 >LEN
   3000 >MS RUN-ARGV-ENV-CAPTURE PAT-EOF-RESULT
   PROC-ARGV-RESET PROC-ENV-RESET
   ENGINE-CANDIDATE:PATH$ >LEN s" ." >LEN
   PAT-CAP-OUT 64 >LEN PAT-CAP-ERR 32 >LEN
   3000 >MS PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE PAT-EOF-RESULT ;

: PAT-RESTORE-STDIN ( -- )
   PAT-SAVED-STDIN @ 0 dup2 0 < if E-PROC-OUTPUT throw then
   PAT-SAVED-STDIN @ close
   PAT-IN-R @ close PAT-IN-W @ close ;

: PAT-RUN-IDLE-STDIN ( -- )
   s" default captures ignore an idle inherited stdin" T-LABEL
   0 0 3 fcntl dup 0 < if drop E-PROC-OUTPUT throw then PAT-SAVED-STDIN !
   PIPE-PAIR PAT-IN-W ! PAT-IN-R !
   PAT-IN-R @ FD>N 0 dup2 0 < if E-PROC-OUTPUT throw then
   [: PAT-IDLE-CAPTURES ;] [: PAT-RESTORE-STDIN ;] finally ;

: PROCESS-ARGV-TEST-MAIN ( -- )
   T-RESET
   PAT-RUN-IDLE-STDIN
   PAT-RUN-PRINTF
   PAT-RUN-CAT
   [: PAT-SPAWN-MISSING ;] E-PROC-SPAWN TTHROWSQ
   PAT-MANY-ARGS
   PAT-LONG-ARGS
   \ Again after long arguments, so later arguments land in storage a reset kept.
   PAT-MANY-ARGS
   [: PAT-NEG-ARG ;] E-PROC-OUTPUT TTHROWSQ
   [: PAT-HUGE-ARG ;] E-PROC-OUTPUT TTHROWSQ
   PAT-PATH-BOUNDS
   PAT-RUN-ARGV-CAPTURE
   PAT-RUN-ARGV-CAPTURE-RESULT
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
