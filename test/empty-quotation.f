\ A zero-initialized typed quotation cell is readable, but invocation is fatal.
\ Exercise the real parser, checker, compiler, and runtime in child processes.
require lib/string.f
require lib/test.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f

package EMPTY-QUOTATION-TEST
private

$1000 constant CAP
10000 constant TIMEOUT-MS
create OUT CAP allot
create ERR CAP allot
variable OUT-U
variable ERR-U

: HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" GETENV dup 0= if 2drop s" bin/hb" exit then ;

: OUT$ ( -- ptr u8 n ) OUT OUT-U @ ;
: ERR$ ( -- ptr u8 n ) ERR ERR-U @ ;

: SOURCE$ ( n ptr u8 n -- ptr u8 n ) {: tier:n body:ptr size:n :}
   SB-RESET
   tier 0= if s" 0 set-tier " else s" 1 set-tier " then SB-APPEND
   s" package EMPTY-Q TYPED-VARIABLE Q [ -- ] " SB-APPEND
   body size SB-APPEND
   s"  ;package" SB-APPEND
   SB$ ;

: RUN-SOURCE ( ptr u8 n -- n ) {: src:ptr size:n :}
   PROC-ARGV-RESET
   HB$ {: exe:ptr exeu:n :}
   exe exeu >LEN src size >LEN OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :}
        o LEN>N OUT-U ! e LEN>N ERR-U ! 0 ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :}
        o LEN>N OUT-U ! e LEN>N ERR-U ! c RC>N ENDOF
   ;MATCH ;

: FATAL ( n ptr u8 n -- ) {: tier:n body:ptr size:n :}
   tier body size SOURCE$ RUN-SOURCE 86 T=
   ERR$ nip 20 T=
   ERR$ s" hb: unset quotation" CONTAINS? TTRUE
   ERR 19 + c@ 10 T=
   OUT$ s" BODY" CONTAINS? 0= TTRUE ;

: TIER-CASES ( n -- ) {: tier:n :}
   tier s" Q @ drop" SOURCE$ RUN-SOURCE 0 T=
   tier s" : GO ( -- ) Q @ execute ; GO" FATAL
   tier s" : GO ( -- n ) Q @ catch ; GO ." FATAL
   tier s" : GO ( -- ) Q @ [: ;] finally ; GO" FATAL
   tier S\" : BODY ( -- ) s\q BODY\q type 7 throw ; : GO ( -- ) [: BODY ;] Q @ finally ; GO" FATAL
   tier s" require lib/memory.f PTR-VARIABLE POOL-A STACK-ABI:PAGE-BYTES MEM-ALLOC-GUARDED drop POOL-A ! : POOL ( -- ptr u8 ) POOL-A @ ; : GO ( -- ) Q @ POOL STACK-ABI:PAGE-BYTES run-in-stack ; GO" FATAL
   tier s" : GO ( -- ) Q @ execute ; : OUTER ( -- n ) [: GO ;] catch ; OUTER ." FATAL
   tier S\" : BODY ( -- ) s\q BODY\q type ; : INSTALL ( -- ) [: BODY ;] Q ! ; : GO ( -- ) Q @ execute Q @ catch . Q @ [: ;] finally ; INSTALL GO" SOURCE$ RUN-SOURCE 0 T=
   OUT$ nip 14 T=
   OUT$ s" BODYBODY0" CONTAINS? TTRUE
   tier S\" require lib/memory.f PTR-VARIABLE POOL-A STACK-ABI:PAGE-BYTES MEM-ALLOC-GUARDED drop POOL-A ! : POOL ( -- ptr u8 ) POOL-A @ ; : BODY ( -- ) s\q BODY\q type ; : INSTALL ( -- ) [: BODY ;] Q ! ; : GO ( -- ) Q @ POOL STACK-ABI:PAGE-BYTES run-in-stack ; INSTALL GO" SOURCE$ RUN-SOURCE 0 T=
   OUT$ s" BODY" STR= TTRUE
   tier s" defer D ( -- ) D" SOURCE$ RUN-SOURCE 76 T=
   ERR$ s" defer: unset execution vector" CONTAINS? TTRUE ;

public
: RUN ( -- )
   T-RESET
   0 TIER-CASES
   1 TIER-CASES
   T-REPORT
   s" empty-quotation: ok" type cr ;
;package

EMPTY-QUOTATION-TEST:RUN
