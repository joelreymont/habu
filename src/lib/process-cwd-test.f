\ process-cwd-test.f - focused tests for src/lib/process-cwd.f.
\ Run: bin/hb --load src/lib/error.f src/lib/string.f src/lib/test.f src/lib/fs.f src/lib/fs-mutate.f src/lib/process.f src/lib/process-argv.f src/lib/process-env.f src/lib/process-cwd.f src/lib/process-cwd-test.f

require src/lib/error.f
require src/lib/string.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/process-cwd.f
require src/lib/test/ending.f

package PROCESS-CWD-TEST

4096 constant PCT-CAP
6 constant PCT-OK-U
PROCESS:PATHZ-CAP 1 + constant PCT-LONG-U

create PCT-ROOT PATH-CAP allot
create PCT-REL PATH-CAP allot
create PCT-OUT PCT-CAP allot
create PCT-ERR PCT-CAP allot
create PCT-LONG PCT-LONG-U allot
create PCT-OK 99 c, 119 c, 100 c, 45 c, 111 c, 107 c,

variable PCT-ROOT-U
variable PCT-REL-U
variable PCT-I

: PCT-LONG! ( -- )
   0 PCT-I !
   begin PCT-I @ PCT-LONG-U < while
      97 PCT-LONG PCT-I @ + c!
      PCT-I @ 1+ PCT-I !
   repeat ;

: PCT-RESET ( -- )
   PROC-ARGV-RESET
   PROC-ENV-RESET ;

: PCT-CAPTURE>N ( result<pcap:captured,pcap:failed> -- n n n )   \ outn errn code (0 on clean exit)
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} o LEN>N e LEN>N 0 ENDOF
     err OF PCAP-FAILED:UNMAKE  {: o:len e:len c:rc :} o LEN>N e LEN>N c RC>N ENDOF
   ;MATCH ;

: PCT-CAPTURE ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n n -- n n n )
   {: path:ptr pathu cwd:ptr cwdu out:ptr outcap err:ptr errcap timeout :}
   path pathu >LEN cwd cwdu >LEN out outcap >LEN err errcap >LEN timeout >MS PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE
   PCT-CAPTURE>N ;

: PCT-STDIN-CAPTURE ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n n -- n n n )
   {: path:ptr pathu cwd:ptr cwdu in:ptr inu out:ptr outcap err:ptr errcap timeout :}
   path pathu >LEN cwd cwdu >LEN in inu >LEN out outcap >LEN err errcap >LEN timeout >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE
   PCT-CAPTURE>N ;

: PCT-SETUP-ROOT ( -- )
   FS:CLEANUP-RESET
   s" hb-process-cwd" FS:MKTEMP-DIR {: root:ptr rootu :}
   root PCT-ROOT rootu BYTE-COPY
   rootu PCT-ROOT-U !
   PCT-ROOT PCT-ROOT-U @ FS:CLEANUP-TREE+
   PCT-ROOT PCT-ROOT-U @ s" rel.txt" PCT-REL FS:JOIN-PATH PCT-REL-U !
   PCT-REL PCT-REL-U @ PCT-OK PCT-OK-U FS:WRITE-ALL ;

: PCT-RUN-CAT-CWD ( -- )
   PCT-SETUP-ROOT
   PCT-RESET
   s" rel.txt"  >LEN PROC-ARGV+
   s" /bin/cat" PCT-ROOT PCT-ROOT-U @ PCT-OUT PCT-CAP PCT-ERR PCT-CAP 1000
   PCT-CAPTURE
   0 T= 0 T= PCT-OK-U T=
   PCT-OUT PCT-OK-U PCT-OK PCT-OK-U T$=
   s" README.md" FS:FILE? TTRUE
   s" rel.txt" FS:FILE? TFALSE
   PROC-ARGV-N @ 0 T=
   PROC-ENV-N @ 0 T=
   FS:CLEANUP-RUN ;

: PCT-RUN-STDIN-CAPTURE ( -- )
   PCT-SETUP-ROOT
   PCT-RESET
   s" /bin/cat" PCT-ROOT PCT-ROOT-U @ PCT-OK PCT-OK-U PCT-OUT PCT-CAP PCT-ERR PCT-CAP 1000
   PCT-STDIN-CAPTURE
   0 T= 0 T= PCT-OK-U T=
   PCT-OUT PCT-OK-U PCT-OK PCT-OK-U T$=
   PROC-ARGV-N @ 0 T=
   PROC-ENV-N @ 0 T=
   FS:CLEANUP-RUN ;

: PCT-MISSING-CWD ( -- )
   PCT-RESET
   s" rel.txt"  >LEN PROC-ARGV+
   s" /bin/cat" >LEN s" /no/such/habu-process-cwd-test" >LEN -1 >FD -1 >FD -1 >FD
   PROC-CWD:SPAWN-ARGV-ENV-CWD-IO drop ;

: PCT-EARLY-EOF$ ( -- ptr u8 n )   \ writes ok, closes 1 and 2, spins 60 s, so only its deadline ends it
   s" 111 emit 107 emit 1 close 2 close : PCT-WAIT ( -- ) mono-ns 60000000000 + begin dup mono-ns > while repeat drop ; PCT-WAIT" ;

: PCT-SELF-EOF$ ( -- ptr u8 n )   \ writes ok, closes 1 and 2, spins 50 ms, then ends
   s" 111 emit 107 emit 1 close 2 close : PCT-WAIT ( -- ) mono-ns 50000000 + begin dup mono-ns > while repeat drop ; PCT-WAIT" ;

: PCT-OUT-CLEAR ( -- )   \ so no assertion passes on an earlier capture's bytes
   0 PCT-OUT c!
   0 PCT-OUT 1 + c! ;

\ stdout of a deadline capture: a prefix of ok, as the deadline may fall before
\ the child's first emit or between its two.
: PCT-OUT-OK? ( n -- )
   {: n :}
   n 0 >= n 2 <= and TTRUE
   PCT-OUT n 2 min s" ok" drop n 2 min T$= ;

: PCT-CLEARED ( -- )
   PROCESS:PID @ PROCESS:NO-PID T= PROCESS:IN-W @ PROCESS:NO-FD T= ;

: PCT-EARLY-EOF ( -- )
   PCT-RESET
   PCT-OUT-CLEAR
   s" bin/hb" s" ." PCT-EARLY-EOF$ PCT-OUT PCT-CAP PCT-ERR PCT-CAP 300
   PCT-STDIN-CAPTURE drop 2drop ;

: PCT-OUTCOME ( ptr u8 n ms -- len len PROCESS:ending )
   {: src:ptr srcu:n deadline:ms :}
   PCT-RESET
   PCT-OUT-CLEAR
   s" bin/hb" >LEN s" ." >LEN src srcu >LEN
   PCT-OUT PCT-CAP >LEN PCT-ERR PCT-CAP >LEN deadline
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE-OUTCOME ;

\ The 300 ms captures also cover the child's start, so they claim only what a
\ deadline promises: a timeout, pid and stdin cleared, stdout a prefix of ok.
\ The third child ends by itself under a guard it never reaches, so it alone
\ claims a clean exit with exactly ok read across its early end of output.
: PCT-RUN-EARLY-EOF ( -- )
   [: PCT-EARLY-EOF ;] E-PROC-TIMEOUT TTHROWSQ
   PCT-CLEARED
   PROCESS:OUT-LEN @ PCT-OUT-OK?
   PCT-EARLY-EOF$ 300 >MS PCT-OUTCOME
   T-ENDING-TIMEOUT LEN>N 0 T= LEN>N PCT-OUT-OK?
   PCT-CLEARED
   PCT-SELF-EOF$ 60000 >MS PCT-OUTCOME {: outu:len erru:len oc :}
   PCT-SELF-EOF$ PCT-OUT outu LEN>N PCT-ERR erru LEN>N oc 0 T-ENDING-EXITED=
   erru LEN>N 0 T= outu LEN>N 2 T=
   PCT-OUT 2 s" ok" T$=
   PCT-CLEARED ;

: PCT-RUN-MISSING-CWD ( -- )
   [: PCT-MISSING-CWD ;] E-PROC-SPAWN TTHROWSQ
   PROC-ARGV-N @ 0 T=
   PROC-ENV-N @ 0 T= ;

: PCT-CWD-TOO-LONG ( -- )
   PCT-LONG!
   PCT-LONG PCT-LONG-U >LEN PROC-CWD:CWDZ drop ;

: PCT-RUN-ARGV-ENV-CWD-CAPTURE-RESULT ( -- )
   PCT-RESET
   s" /usr/bin/true" >LEN s" ." >LEN PCT-OUT PCT-CAP >LEN PCT-ERR PCT-CAP >LEN 1000 >MS PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} o LEN>N 0 T=  e LEN>N 0 T= ENDOF
     err OF PCAP-FAILED:UNMAKE 2drop drop 1 0 T= ENDOF
   ;MATCH
   PCT-RESET
   s" /usr/bin/false" >LEN s" ." >LEN PCT-OUT PCT-CAP >LEN PCT-ERR PCT-CAP >LEN 1000 >MS PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE 2drop 1 0 T= ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :} o LEN>N 0 T=  e LEN>N 0 T=  c RC>N 1 T= ENDOF
   ;MATCH ;

: PCT-STAGE-STALE ( -- )
   s" stale" >LEN PROC-ARGV+ ;

: PCT-ECHO-FRESH ( -- )
   s" fresh" >LEN PROC-ARGV+
   s" /bin/echo" s" /" PCT-OUT PCT-CAP PCT-ERR PCT-CAP 1000 PCT-CAPTURE
   0 T= 0 T= {: outu:n :}
   PCT-OUT outu s\" fresh\n" T$= ;

: PCT-REFUSED-RESETS ( -- )
   s" a refused cwd spawn leaves no staged argument" T-LABEL
   PCT-RESET PCT-STAGE-STALE
   [: s" /bin/echo" >LEN s" " >LEN -1 >FD -1 >FD -1 >FD
      PROC-CWD:SPAWN-ARGV-ENV-CWD-IO drop ;]
   E-PROC-OUTPUT TTHROWSQ
   PCT-ECHO-FRESH
   PCT-STAGE-STALE
   [: s" /bin/echo" s" " PCT-OUT PCT-CAP PCT-ERR PCT-CAP 1000 PCT-CAPTURE
      drop drop drop ;]
   E-PROC-OUTPUT TTHROWSQ
   PCT-ECHO-FRESH ;

: PROCESS-CWD-TEST-MAIN ( -- )
   T-RESET
   PCT-RUN-CAT-CWD
   PCT-RUN-ARGV-ENV-CWD-CAPTURE-RESULT
   PCT-RUN-STDIN-CAPTURE
   PCT-RUN-EARLY-EOF
   PCT-RUN-MISSING-CWD
   [: PCT-CWD-TOO-LONG ;] E-PROC-OUTPUT TTHROWSQ
   PCT-REFUSED-RESETS
   T-REPORT
   s" process-cwd-test: ok" type cr ;

PROCESS-CWD-TEST-MAIN

;package
