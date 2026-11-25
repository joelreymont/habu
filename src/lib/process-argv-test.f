\ process-argv-test.f - focused tests for src/lib/process-argv.f.
\ Run: bin/hb --load src/lib/error.f src/lib/process.f src/lib/process-argv.f src/lib/process-argv-test.f

require src/lib/error.f
require src/lib/test/assert.f
require src/lib/memory.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/process-cwd.f
require src/lib/engine-candidate.f
require src/lib/test/ending.f
require src/lib/fd.f

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
$200000 constant PAT-UNREAD-CAP \ 2 MiB: past a default pipe of 16 pages of up to 64 KiB
DYNAMIC-BUFFER PAT-UNREAD u8
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

: PAT-UNREAD$ ( -- str )
   PAT-UNREAD-CAP PAT-UNREAD-RESERVE
   0 PAT-UNREAD PAT-UNREAD-CAP >str ;

: PAT-READ ( -- n )
   PAT-OUT-R @ PAT-BUF 64 read ;

: PAT-CAPTURE>N ( result<pcap:captured,pcap:failed> -- n n n )   \ outn errn code (0 on clean exit)
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} o LEN>N e LEN>N 0 ENDOF
     err OF PCAP-FAILED:UNMAKE  {: o:len e:len c:rc :} o LEN>N e LEN>N c RC>N ENDOF
   ;MATCH ;

: PAT-LTRIM-SPACES ( str -- str ) {: s:str :}
   0 begin dup s STR:LEN < while
      dup s STR:DATA + c@ PAT-SPACE <> if dup s STR:DATA + s STR:LEN rot - >str exit then
      1+
   repeat drop s STR:DATA 0 >str ;

: PAT-CAPTURE ( str ^u8 n ^u8 n n -- n n n )
   {: path:str out:^ outcap err:^ errcap timeout :}
   path out outcap >LEN err errcap >LEN timeout >MS RUN-ARGV-CAPTURE
   PAT-CAPTURE>N ;

: PAT-STDIN-CAPTURE ( str str ^u8 n ^u8 n n -- n n n )
   {: path:str in:str out:^ outcap err:^ errcap timeout :}
   path in out outcap >LEN err errcap >LEN timeout >MS RUN-ARGV-STDIN-CAPTURE
   PAT-CAPTURE>N ;

: PAT-STDIN-CAPTURE-OUTCOME ( str str ^u8 n ^u8 n n -- len len PROCESS:ending )
   {: path:str in:str out:^ outcap err:^ errcap timeout :}
   path in out outcap >LEN err errcap >LEN timeout >MS RUN-ARGV-STDIN-CAPTURE-OUTCOME ;

: PAT-CHECK-WC-COUNT ( n n n -- ) {: outu erru rc :}
   rc 0 T=
   erru 0 T=
   PAT-CAP-OUT outu >str PAT-LTRIM-SPACES PAT-WC-COUNT PAT-WC-COUNT-U >str T$= ;

: PAT-RUN-PRINTF ( -- )
   FD:PIPE-PAIR PAT-OUT-W ! PAT-OUT-R !
   PROC-ARGV-RESET
   s" %s-%s"  PROC-ARGV+
   s" left"  PROC-ARGV+
   s" right"  PROC-ARGV+
   s" /usr/bin/printf" -1 >FD PAT-OUT-W @ -1 >FD PROC-RUN-ARGV-IO-RC
   MATCH result ok OF 0 T= ENDOF err OF drop 1 0 T= ENDOF ;MATCH
   PAT-OUT-W @ close
   PAT-READ 10 T=
   PAT-BUF 10 >str s" left-right" T$=
   PAT-OUT-R @ close ;

: PAT-RUN-CAT ( -- )
   FD:PIPE-PAIR PAT-IN-W ! PAT-IN-R !
   FD:PIPE-PAIR PAT-OUT-W ! PAT-OUT-R !
   PAT-IN-W @ s" argv-stdin" write 10 T=
   PAT-IN-W @ close
   PROC-ARGV-RESET
   s" /bin/cat" PAT-IN-R @ PAT-OUT-W @ -1 >FD PROC-RUN-ARGV-IO-RC
   MATCH result ok OF 0 T= ENDOF err OF drop 1 0 T= ENDOF ;MATCH
   PAT-IN-R @ close
   PAT-OUT-W @ close
   PAT-READ 10 T=
   PAT-BUF 10 >str s" argv-stdin" T$=
   PAT-OUT-R @ close ;

: PAT-SPAWN-MISSING ( -- )
   PROC-ARGV-RESET
   s" ignored"  PROC-ARGV+
   s" /no/such/habu-process-argv-test" -1 >FD -1 >FD -1 >FD PROC-SPAWN-ARGV-IO drop ;

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
: PAT-NEXT ( str -- )
   {: s:str :}
   PAT-BIG-OUT PAT-AT @ + s STR:LEN >str s T$=
   PAT-BIG-OUT PAT-AT @ s STR:LEN + + c@ 44 T=
   PAT-AT @ s STR:LEN 1+ + PAT-AT ! ;


: PAT-MANY-ARGS ( -- )
   s" more than $100 arguments reach the child in order" T-LABEL
   PROC-ARGV-RESET
   s" %s," PROC-ARGV+
   PAT-MANY-N 0 ?do i PAT-ARG! PAT-ARG PAT-ARG-U >str PROC-ARGV+ loop
   PAT-PRINTF-ARGS {: outu:n :}
   0 PAT-AT !
   PAT-MANY-N 0 ?do i PAT-ARG! PAT-ARG PAT-ARG-U >str PAT-NEXT loop
   PAT-AT @ outu T= ;


: PAT-LONG-ARGS ( -- )
   s" long arguments past $100 paths together reach the child whole" T-LABEL
   PROC-ARGV-RESET
   s" %s," PROC-ARGV+
   s" x" PROC-ARGV+
   PAT-LONG-N 0 ?do i 97 + PAT-LONG! PAT-LONG PAT-LONG-CAP >str PROC-ARGV+ loop
   s" y" PROC-ARGV+
   PAT-PRINTF-ARGS {: outu:n :}
   0 PAT-AT !
   s" x" PAT-NEXT
   PAT-LONG-N 0 ?do i 97 + PAT-LONG! PAT-LONG PAT-LONG-CAP >str PAT-NEXT loop
   s" y" PAT-NEXT
   PAT-AT @ outu T= ;

: PAT-NEG-ARG ( -- )
   PROC-ARGV-RESET PAT-LONG -1 >str PROC-ARGV+ ;

: PAT-HUGE-ARG ( -- )
   PROC-ARGV-RESET PAT-LONG MEM-MAX-N >str PROC-ARGV+ ;

: PAT-NUL-ARG ( -- )
   PROC-ARGV-RESET s\" a\zb" PROC-ARGV+ ;

: PAT-PATH-BOUNDS ( -- )
   s" a path fits when its bytes and NUL fit PROCESS:PATHZ-CAP" T-LABEL
   [: PAT-LONG PROCESS:PATHZ-CAP 1- >str PROC-ARGV-CHECK-PATH ;] catch 0 T=
   [: PAT-LONG PROCESS:PATHZ-CAP >str PROC-ARGV-CHECK-PATH ;] E-PROC-OUTPUT TTHROWSQ
   [: PAT-LONG -1 >str PROC-ARGV-CHECK-PATH ;] E-PROC-OUTPUT TTHROWSQ
   [: PAT-LONG MEM-MAX-N >str PROC-ARGV-CHECK-PATH ;] E-PROC-OUTPUT TTHROWSQ ;

: PAT-RUN-ARGV-CAPTURE ( -- )
   PROC-ARGV-RESET
   s" %s:%s"  PROC-ARGV+
   s" left"  PROC-ARGV+
   s" right"  PROC-ARGV+
   s" /usr/bin/printf" PAT-CAP-OUT 64 PAT-CAP-ERR 32 1000 PAT-CAPTURE
   0 T= 0 T= 10 T=
   PAT-CAP-OUT 10 >str s" left:right" T$=
   PROC-ARGV-N @ 0 T= ;

: PAT-RUN-ARGV-CAPTURE-EXACT ( -- )
   PROC-ARGV-RESET
   s" %s"  PROC-ARGV+
   s" abc"  PROC-ARGV+
   s" /usr/bin/printf" PAT-CAP-OUT 3 PAT-CAP-ERR 0 1000 PAT-CAPTURE
   0 T= 0 T= 3 T=
   PAT-CAP-OUT 3 >str s" abc" T$= ;

: PAT-RUN-ARGV-CAPTURE-TRUNCATED ( -- )
   PROC-ARGV-RESET
   s" %s"  PROC-ARGV+
   s" abcd"  PROC-ARGV+
   s" /usr/bin/printf" PAT-CAP-OUT 3 PAT-CAP-ERR 0 1000 PAT-CAPTURE
   2drop drop ;

: PAT-RUN-ARGV-STDIN-CAPTURE-CAT ( -- )
   PROC-ARGV-RESET
   s" /bin/cat" s" stdin-echo" PAT-CAP-OUT 64 PAT-CAP-ERR 32 1000 PAT-STDIN-CAPTURE
   0 T= 0 T= 10 T=
   PAT-CAP-OUT 10 >str s" stdin-echo" T$=
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
   s" /usr/bin/false" PAT-EARLY-IN PAT-EARLY-IN-CAP >str
   PAT-CAP-OUT 64 PAT-CAP-ERR 32 1000
   PAT-STDIN-CAPTURE
   1 T= 0 T= 0 T= ;

: PAT-RUN-ARGV-STDIN-CAPTURE-WC-LARGE ( -- )
   PROC-ARGV-RESET
   PAT-EARLY-IN!
   s" -c"  PROC-ARGV+
   s" /usr/bin/wc" PAT-EARLY-IN PAT-EARLY-IN-CAP >str
   PAT-CAP-OUT 64 PAT-CAP-ERR 32 1000
   PAT-STDIN-CAPTURE
   PAT-CHECK-WC-COUNT ;

: PAT-RUN-ARGV-STDIN-CAPTURE-TRUNCATED ( -- )
   PROC-ARGV-RESET
   s" /bin/cat" s" abcd" PAT-CAP-OUT 3 PAT-CAP-ERR 32 1000 PAT-STDIN-CAPTURE
   2drop drop ;

: PAT-RUN-ARGV-STDIN-CAPTURE-TIMEOUT ( -- )
   PROC-ARGV-RESET
   s" 5"  PROC-ARGV+
   s" /bin/sleep" s" " PAT-CAP-OUT 64 PAT-CAP-ERR 32 50 PAT-STDIN-CAPTURE
   2drop drop ;

: PAT-RUN-ARGV-STDIN-CAPTURE-OUTCOME-CAT ( -- )
   PROC-ARGV-RESET
   s" /bin/cat" s" stdin-outcome" PAT-CAP-OUT 64 PAT-CAP-ERR 32 1000 PAT-STDIN-CAPTURE-OUTCOME
   {: outu:len erru:len oc :}
   s" /bin/cat" PAT-CAP-OUT outu LEN>N >str PAT-CAP-ERR erru LEN>N >str oc 0 T-ENDING-EXITED=
   erru LEN>N 0 T= outu LEN>N 13 T=
   PAT-CAP-OUT 13 >str s" stdin-outcome" T$= ;

: PAT-RUN-ARGV-STDIN-CAPTURE-OUTCOME-TIMEOUT ( -- )
   PROC-ARGV-RESET
   s" 5"  PROC-ARGV+
   s" /bin/sleep" s" " PAT-CAP-OUT 64 PAT-CAP-ERR 32 50 PAT-STDIN-CAPTURE-OUTCOME
   T-ENDING-TIMEOUT LEN>N 0 T= LEN>N 0 T= ;

: PAT-RUN-ARGV-STDIN-CAPTURE-OUTCOME-STDERR-UNREAD ( -- )
   s" a child that ends before reading its input keeps its stderr and exit status" T-LABEL
   PROC-ARGV-RESET
   s" -c" PROC-ARGV+
   s" echo why >&2; exit 3" PROC-ARGV+
   s" /bin/sh" PAT-UNREAD$
   PAT-CAP-OUT 64 PAT-CAP-ERR 32 PROCESS:WAIT-FOREVER MS>N
   PAT-STDIN-CAPTURE-OUTCOME {: outu:len erru:len oc :}
   s" /bin/sh" PAT-CAP-OUT outu LEN>N >str PAT-CAP-ERR erru LEN>N >str oc 3 T-ENDING-EXITED=
   outu LEN>N 0 T= erru LEN>N 4 T=
   PAT-CAP-ERR erru LEN>N >str s\" why\n" T$= ;

: PAT-RUN-ARGV-CAPTURE-RESULT ( -- )
   PROC-ARGV-RESET
   s" %s" PROC-ARGV+  s" hi" PROC-ARGV+
   s" /usr/bin/printf" PAT-CAP-OUT 64 >LEN PAT-CAP-ERR 32 >LEN 1000 >MS RUN-ARGV-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} o LEN>N 2 T=  e LEN>N 0 T= ENDOF
     err OF PCAP-FAILED:UNMAKE 2drop drop 1 0 T= ENDOF
   ;MATCH
   PROC-ARGV-RESET
   s" /usr/bin/false" PAT-CAP-OUT 64 >LEN PAT-CAP-ERR 32 >LEN 1000 >MS RUN-ARGV-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE 2drop 1 0 T= ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :} o LEN>N 0 T=  e LEN>N 0 T=  c RC>N 1 T= ENDOF
   ;MATCH ;

: PAT-READ-NEG-LEN ( -- )
   -1 PAT-I !
   PAT-IN-R PAT-CAP-OUT 64 >LEN PAT-I PROCESS:READ-STREAM ;

: PAT-READ-HIGH-LEN ( -- )
   65 PAT-I !
   PAT-IN-R PAT-CAP-OUT 64 >LEN PAT-I PROCESS:READ-STREAM ;

variable PAT-SAVED-STDIN

: PAT-EOF-RESULT ( result<pcap:captured,pcap:failed> -- )
   PAT-CAPTURE>N 0 T= 0 T= 0 T= ;

\ A bare engine waits for stdin EOF. The parent keeps the pipe writer open,
\ so inheriting its stdin times out; each default capture must supply EOF.
: PAT-IDLE-CAPTURES ( -- )
   PROC-ARGV-RESET
   ENGINE-CANDIDATE:PATH$ PAT-CAP-OUT 64 >LEN PAT-CAP-ERR 32 >LEN
   3000 >MS RUN-ARGV-CAPTURE PAT-EOF-RESULT
   ENGINE-CANDIDATE:PATH$ PAT-CAP-OUT 64 >LEN PAT-CAP-ERR 32 >LEN
   3000 >MS PROCESS:RUN-CAPTURE PAT-EOF-RESULT
   PROC-ARGV-RESET PROC-ENV-RESET
   ENGINE-CANDIDATE:PATH$ PAT-CAP-OUT 64 >LEN PAT-CAP-ERR 32 >LEN
   3000 >MS RUN-ARGV-ENV-CAPTURE PAT-EOF-RESULT
   PROC-ARGV-RESET PROC-ENV-RESET
   ENGINE-CANDIDATE:PATH$ s" ."
   PAT-CAP-OUT 64 >LEN PAT-CAP-ERR 32 >LEN
   3000 >MS PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE PAT-EOF-RESULT ;

: PAT-RESTORE-STDIN ( -- )
   PAT-SAVED-STDIN @ 0 dup2 0 < if E-PROC-OUTPUT throw then
   PAT-SAVED-STDIN @ close
   PAT-IN-R @ close PAT-IN-W @ close ;

: PAT-RUN-IDLE-STDIN ( -- )
   s" default captures ignore an idle inherited stdin" T-LABEL
   0 0 3 fcntl dup 0 < if drop E-PROC-OUTPUT throw then PAT-SAVED-STDIN !
   FD:PIPE-PAIR PAT-IN-W ! PAT-IN-R !
   PAT-IN-R @ FD>N 0 dup2 0 < if E-PROC-OUTPUT throw then
   [: PAT-IDLE-CAPTURES ;] [: PAT-RESTORE-STDIN ;] finally ;

: PAT-STAGE-STALE ( -- )
   s" stale" PROC-ARGV+ ;

: PAT-ECHO-FRESH ( -- )
   s" fresh" PROC-ARGV+
   s" /bin/echo" PAT-CAP-OUT 64 PAT-CAP-ERR 32 1000 PAT-CAPTURE
   0 T= 0 T= {: outu:n :}
   PAT-CAP-OUT outu >str s\" fresh\n" T$= ;

: PAT-REFUSED-RESETS ( -- )
   s" a refused spawn leaves no staged argument" T-LABEL
   PROC-ARGV-RESET PAT-STAGE-STALE
   [: s" " -1 >FD -1 >FD -1 >FD PROC-SPAWN-ARGV-IO drop ;]
   E-PROC-OUTPUT TTHROWSQ
   PAT-ECHO-FRESH
   PAT-STAGE-STALE
   [: s" " PAT-CAP-OUT 64 PAT-CAP-ERR 32 1000 PAT-CAPTURE drop drop drop ;]
   E-PROC-OUTPUT TTHROWSQ
   PAT-ECHO-FRESH ;

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
   PAT-REFUSED-RESETS
   [: PAT-NUL-ARG ;] E-FFI-NUL TTHROWSQ
   PAT-PATH-BOUNDS
   PAT-RUN-ARGV-CAPTURE
   PAT-RUN-ARGV-CAPTURE-RESULT
   PAT-RUN-ARGV-CAPTURE-EXACT
   [: PAT-RUN-ARGV-CAPTURE-TRUNCATED ;] E-PROC-STDOUT-CAPACITY TTHROWSQ
   PAT-RUN-ARGV-STDIN-CAPTURE-CAT
   PAT-RUN-ARGV-STDIN-CAPTURE-EMPTY
   PAT-RUN-ARGV-STDIN-CAPTURE-FALSE
   PAT-RUN-ARGV-STDIN-CAPTURE-FALSE-LARGE
   PAT-RUN-ARGV-STDIN-CAPTURE-WC-LARGE
   [: PAT-RUN-ARGV-STDIN-CAPTURE-TRUNCATED ;] E-PROC-STDOUT-CAPACITY TTHROWSQ
   [: PAT-RUN-ARGV-STDIN-CAPTURE-TIMEOUT ;] E-PROC-TIMEOUT TTHROWSQ
   PAT-RUN-ARGV-STDIN-CAPTURE-OUTCOME-CAT
   PAT-RUN-ARGV-STDIN-CAPTURE-OUTCOME-STDERR-UNREAD
   PAT-RUN-ARGV-STDIN-CAPTURE-OUTCOME-TIMEOUT
   [: PAT-READ-NEG-LEN ;] E-PROC-OUTPUT TTHROWSQ
   [: PAT-READ-HIGH-LEN ;] E-PROC-OUTPUT TTHROWSQ
   PAT-UNREAD-RELEASE
   T-REPORT
   s" process-argv-test: ok" type cr ;

PROCESS-ARGV-TEST-MAIN
