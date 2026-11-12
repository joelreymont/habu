\ A zero-initialized typed quotation cell is readable, but invocation is fatal.
\ Exercise the real parser, checker, compiler, and runtime in child processes.
require src/lib/string.f
require src/lib/test.f
require src/lib/process-argv.f
require src/lib/engine-candidate.f
require test/pcap-result.f

package EMPTY-QUOTATION-TEST
private

$1000 constant CAP
10000 constant TIMEOUT-MS
create OUT CAP allot
create ERR CAP allot
variable OUT-U
variable ERR-U

: OUT$ ( -- ptr u8 n ) OUT OUT-U @ ;
: ERR$ ( -- ptr u8 n ) ERR ERR-U @ ;

: SOURCE$ ( ptr u8 n -- ptr u8 n ) {: body:ptr size:n :}
   SB-RESET
   s" package EMPTY-Q TYPED-VARIABLE Q [ -- ] " SB-APPEND
   body size SB-APPEND
   s"  ;package" SB-APPEND
   SB$ ;

: RUN-SOURCE ( ptr u8 n -- n ) {: src:ptr size:n :}
   PROC-ARGV-RESET
   ENGINE-CANDIDATE:PATH$ {: exe:ptr exeu:n :}
   exe exeu >LEN src size >LEN OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE PCAP-RESULT:>N >r ERR-U ! OUT-U ! r> ;

\ The exit's whole line up to the lr's 16 hex digits and its newline.
: LINE$ ( -- ptr u8 n ) s" E-CALLABLE-ABI habu: unset quotation lr " ;

: LR-DIGITS? ( -- bool )
   true  ERR-U @ 1-  LINE$ nip ?do ERR i + c@ STR-HEX-DIGIT? and loop ;

: FATAL ( ptr u8 n -- ) {: body:ptr size:n :}
   body size SOURCE$ RUN-SOURCE 86 T=
   ERR$ nip LINE$ nip 17 + T=
   ERR$ LINE$ STARTS-WITH? TTRUE
   LR-DIGITS? TTRUE
   ERR$ + 1- c@ 10 T=
   OUT$ s" BODY" CONTAINS? 0= TTRUE ;

: CASES ( -- )
   s" Q @ drop" SOURCE$ RUN-SOURCE 0 T=
   s" : GO ( -- ) Q @ execute ; GO" FATAL
   s" : GO ( -- n ) Q @ catch ; GO ." FATAL
   s" : GO ( -- ) Q @ [: ;] finally ; GO" FATAL
   S\" : BODY ( -- ) s\q BODY\q type 7 throw ; : GO ( -- ) [: BODY ;] Q @ finally ; GO" FATAL
   s" require src/lib/memory.f PTR-VARIABLE POOL-A STACK-ABI:PAGE-BYTES MEM:ALLOC-GUARDED drop POOL-A ! : POOL ( -- ptr u8 ) POOL-A @ ; : GO ( -- ) Q @ POOL STACK-ABI:PAGE-BYTES run-in-stack ; GO" FATAL
   s" : GO ( -- ) Q @ execute ; : OUTER ( -- n ) [: GO ;] catch ; OUTER ." FATAL
   S\" : BODY ( -- ) s\q BODY\q type ; : INSTALL ( -- ) [: BODY ;] Q ! ; : GO ( -- ) Q @ execute Q @ catch . Q @ [: ;] finally ; INSTALL GO" SOURCE$ RUN-SOURCE 0 T=
   OUT$ nip 14 T=
   OUT$ s" BODYBODY0" CONTAINS? TTRUE
   S\" require src/lib/memory.f PTR-VARIABLE POOL-A STACK-ABI:PAGE-BYTES MEM:ALLOC-GUARDED drop POOL-A ! : POOL ( -- ptr u8 ) POOL-A @ ; : BODY ( -- ) s\q BODY\q type ; : INSTALL ( -- ) [: BODY ;] Q ! ; : GO ( -- ) Q @ POOL STACK-ABI:PAGE-BYTES run-in-stack ; INSTALL GO" SOURCE$ RUN-SOURCE 0 T=
   OUT$ s" BODY" STR= TTRUE
   s" defer D ( -- ) D" SOURCE$ RUN-SOURCE 76 T=
   ERR$ s" defer: unset execution vector" CONTAINS? TTRUE ;

public
: RUN ( -- )
   T-RESET
   CASES
   T-REPORT
   s" empty-quotation: ok" type cr ;
;package

EMPTY-QUOTATION-TEST:RUN
