split-str
\ code-reclaim.f - the code a rollback gives back, and the created record a
\ definer's does> acts on after a motion retires records.

require src/core/exit-status.f
require src/lib/string.f
require src/lib/test.f
require src/lib/test/ending.f
require src/lib/test/subject.f

package CRECL-TEST

private

: REC ( ^u8 n -- ^n )
   XREF-FIND
   dup XREF-FOUND? 0= if s" code-reclaim: subject not found" EXIT-STATUS:FAILURE die then ;

: REC-START ( str -- n )
   REC XREF-START ;

: REC-INDEX ( str -- n )
   XREF-FIND-CURRENT-INDEX ;

: DEFINED? ( str -- bool )
   XREF-FIND XREF-FOUND? ;

variable P-CP

: PLAIN-CASE ( -- )
   s" : CR-P1 ( -- n ) 1 ;" evaluate-closed
   cp@ P-CP !
   s" a rollback gives back the code of the words it retires" T-LABEL
   s" : CR-P2 ( -- n ) 2 ; : CR-P3 ( -- n ) 3 ; 73 throw" TEST-EVAL:RC 0<> TTRUE
   cp@ P-CP @ T=
   s" CR-P2" DEFINED? TFALSE
   s" CR-P3" DEFINED? TFALSE
   s" CR-P1" DEFINED? TTRUE
   s" CR-P1" TEST-EVAL:N 1 T= ;

variable U-CP
variable U-NDICT

: REUSE-CASE ( -- )
   cp@ U-CP !
   ndict@ U-NDICT !
   s" a new definition reuses the record and code a rollback gave back" T-LABEL
   s" : CR-REUSE ( n -- n ) 1 + ; 73 throw" TEST-EVAL:RC 0<> TTRUE
   s" : CR-REUSE ( n -- n ) 3 * ;" evaluate-closed
   s" CR-REUSE" REC-START U-CP @ T=
   s" CR-REUSE" REC-INDEX U-NDICT @ T=

   s" a caller compiled after reuse reaches the new bytes" T-LABEL
   s" : CR-REUSE-CALL ( n -- n ) CR-REUSE ;" evaluate-closed
   s" 5 CR-REUSE-CALL" TEST-EVAL:N 15 T= ;

\ LASTC-CELL names the record the last create, variable or constant wrote, and
\ a definer's does> gives that record its action. Every motion that lowers the
\ dictionary count retires the records above it, and the next definition reuses
\ their slots, so a definer run after one creates its word over a retired slot
\ and its does> must act on that word. Each program runs in a child.
$1000 constant LC-CAP
LC-CAP BUFFER: LC-OUT
LC-CAP BUFFER: LC-ERR

: LC-EXITS ( str n -- len len ) {: src:^ srcu:n want:n :}
   src srcu LC-OUT LC-CAP >LEN LC-ERR LC-CAP >LEN PROCESS:WAIT-FOREVER SUBJECT:RUN
   {: outu:len erru:len oc :}
   src srcu LC-OUT outu LEN>N LC-ERR erru LEN>N oc want T-ENDING-EXITED=
   outu erru ;

: LC-PRINTS ( str str -- ) {: want:^ wantu:n :}
   0 LC-EXITS {: outu:len erru:len :}
   erru LEN>N 0 T=
   LC-OUT outu LEN>N want wantu T$= ;

\ The program runs to the end and prints this; the definition it refuses reports
\ on stderr.
: LC-OUT-PRINTS ( str str -- ) {: want:^ wantu:n :}
   0 LC-EXITS drop {: outu:len :}
   LC-OUT outu LEN>N want wantu T$= ;

: LASTC-CASE ( -- )
   s" a definer after an evaluate that fails acts on its own word" T-LABEL
   S\" create CR-LC-KEEP 7 ,\n: CR-LC-TRY ( -- n ) [: s\" create CR-LC-GONE 5 , CR-LC-NO-SUCH-WORD\" evaluate-closed ;] catch ;\nCR-LC-TRY drop\n: CR-LC-BEHAVE ( -- ) create 8 , does> ( -- n ) @ ;\nCR-LC-BEHAVE CR-LC-NEW CR-LC-NEW .\n"
   S\" 8\n" LC-OUT-PRINTS

   s" a definer after a definition that fails acts on its own word" T-LABEL
   S\" create CR-LC-KEEP 7 ,\n: CR-LC-BEHAVE ( -- ) create 8 , does> ( -- n ) @ 1 + ;\n: CR-LC-TRY ( -- n ) [: s\" : CR-LC-BAD ( -- ) CR-LC-NO-SUCH-WORD ;\" evaluate-closed ;] catch ;\nCR-LC-TRY drop\nCR-LC-BEHAVE CR-LC-NEW CR-LC-NEW .\n"
   S\" 9\n" LC-OUT-PRINTS

   \ undefine retires a record in place: NDICT stays, the wordlist cell says so.
   s" a definer after an undefine acts on its own word" T-LABEL
   S\" create CR-LC-GONE 7 ,\nundefine CR-LC-GONE\n: CR-LC-BEHAVE ( -- ) create 8 , does> ( -- n ) @ 1 + ;\nCR-LC-BEHAVE CR-LC-NEW CR-LC-NEW . depth .\n"
   S\" 9\n0\n" LC-PRINTS

   s" so does one after an undefine whose body a compiled caller still reaches" T-LABEL
   S\" create CR-LC-GONE 7 ,\n: CR-LC-USE ( -- n ) CR-LC-GONE @ ;\nundefine CR-LC-GONE\n: CR-LC-BEHAVE ( -- ) create 8 , does> ( -- n ) @ 1 + ;\nCR-LC-BEHAVE CR-LC-NEW CR-LC-NEW . CR-LC-USE .\n"
   S\" 9\n7\n" LC-PRINTS ;

public

: RUN ( -- )
   T-RESET
   PLAIN-CASE
   REUSE-CASE
   LASTC-CASE
   T-REPORT ;

;package

CRECL-TEST:RUN
