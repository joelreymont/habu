\ code-reclaim.f - the live-XREF floor used when FORGET gives code back, and
\ the created record a later does> patches.

require lib/string.f
require lib/test.f
require lib/test/outcome.f
require lib/test/subject.f
require src/habu/layout.f

package CRECL-TEST

private

TRUSTED: EV ( ptr u8 n -- )
   evaluate ;

TRUSTED: EV-N ( ptr u8 n -- n )
   evaluate ;

4 constant INSN-BYTES

: REC ( ptr u8 n -- ptr n )
   XREF-FIND
   dup XREF-FOUND? 0= if s" code-reclaim: subject not found" 76 die then ;

: REC-START ( ptr u8 n -- n )
   REC XREF-START ;

: REC-INDEX ( ptr u8 n -- n )
   XREF-FIND-INDEX ;

: DEFINED? ( ptr u8 n -- bool )
   XREF-FIND XREF-FOUND? ;

: FILL ( -- )
   s" : CR-F1 ( n -- n ) dup 3 * swap 7 + + ;" EV
   s" : CR-F2 ( n -- n ) dup 5 * swap 9 + + ;" EV
   s" : CR-F3 ( n -- n ) dup 11 * swap 13 + + ;" EV
   s" : CR-F4 ( n -- n ) dup 17 * swap 19 + + ;" EV ;

: FILL-FORGET ( -- )
   s" CR-F1" FORGET-DEFS-FROM ;

variable A-CP
variable ALIAS-SLOT
variable ALIAS-OCC

: ALIAS-CASE ( -- )
   s" CRECL-SUBJ:CR-LOW" REC DEF-OCC:SELECT {: original-slot:n original-occ:n :}
   s" CRECL-ALIAS:CR-LOW" REC DEF-OCC:SELECT {: alias-slot:n alias-occ:n :}
   original-occ alias-occ T<>
   s" the alias record names the earlier routine" T-LABEL
   s" CRECL-ALIAS:CR-LOW" REC-START  s" CRECL-SUBJ:CR-LOW" REC-START  T=

   s" and follows the word compiled above that routine" T-LABEL
   s" CRECL-ALIAS:CR-LOW" REC-INDEX  s" CRECL-SUBJ:CR-HIGH" REC-INDEX  >  TTRUE
   s" CRECL-SUBJ:CR-HIGH" REC-START  s" CRECL-SUBJ:CR-LOW" REC-START  >  TTRUE

   cp@ A-CP !
   s" CRECL-ALIAS:CR-LOW" FORGET-DEFS-FROM

   s" forgetting the alias retires only its record" T-LABEL
   s" CRECL-ALIAS:CR-LOW" DEFINED? TFALSE
   cp@ A-CP @ T=
   alias-slot ALIAS-SLOT !  alias-occ ALIAS-OCC !
   [: ALIAS-SLOT @ ALIAS-OCC @ DEF-OCC:RESOLVE drop ;] DEF-OCC:E-STALE TTHROWSQ
   original-slot original-occ DEF-OCC:CALLABLE {: old-entry:n :}
   s" CRECL-SUBJ:CR-LOW" REC-START old-entry T=

   FILL
   s" both surviving records keep their code" T-LABEL
   s" CRECL-SUBJ:CR-LOW" EV-N 11 T=
   s" CRECL-SUBJ:CR-HIGH" EV-N 22 T=
   FILL-FORGET ;

variable P-START

: PLAIN-CASE ( -- )
   s" : CR-P1 ( -- n ) 1 ;" EV
   s" : CR-P2 ( -- n ) 2 ;" EV
   s" : CR-P3 ( -- n ) 3 ;" EV
   s" CR-P2" REC-START P-START !
   s" CR-P2" FORGET-DEFS-FROM

   s" an ordinary forget returns the first retired routine's slot" T-LABEL
   cp@ P-START @ T=
   s" CR-P2" DEFINED? TFALSE
   s" CR-P3" DEFINED? TFALSE
   s" CR-P1" DEFINED? TTRUE
   s" CR-P1" EV-N 1 T= ;

variable G-CP

: REFUSE-BODY ( -- )
   s" CR-P1" REC-START CODE-RECLAIM:TRUNCATE ;

: REFUSE-CASE ( -- )
   cp@ G-CP !

   s" a floor at a surviving routine is refused" T-LABEL
   [: REFUSE-BODY ;] CODE-RECLAIM:E-LIVE TTHROWSQ

   s" a floor above the free slot is refused" T-LABEL
   [: cp@ INSN-BYTES + CODE-RECLAIM:TRUNCATE ;] CODE-RECLAIM:E-FLOOR TTHROWSQ

   s" refusals leave the pointer and live routine unchanged" T-LABEL
   cp@ G-CP @ T=
   s" CR-P1" EV-N 1 T= ;

variable U-START
variable U-INDEX

: REUSE-CASE ( -- )
   s" : CR-REUSE ( n -- n ) 1 + ;" EV
   s" CR-REUSE" REC-START U-START !
   s" CR-REUSE" REC-INDEX U-INDEX !
   s" CR-REUSE" FORGET-DEFS-FROM
   s" : CR-REUSE ( n -- n ) 3 * ;" EV

   s" a new definition reuses the reclaimed record and code slot" T-LABEL
   s" CR-REUSE" REC-START U-START @ T=
   s" CR-REUSE" REC-INDEX U-INDEX @ T=

   s" a caller compiled after reuse reaches the new bytes" T-LABEL
   s" : CR-REUSE-CALL ( n -- n ) CR-REUSE ;" EV
   s" 5 CR-REUSE-CALL" EV-N 15 T= ;

\ LASTC-CELL names the record the last create, variable or constant wrote, and
\ does> patches that record. Every motion that lowers the dictionary count
\ retires the records above it, and the next definition reuses their slots, so
\ a LASTC left naming a retired record makes a later does> patch the slot's new
\ owner. Each program runs in a child, where a refusal ends the process.
$1000 constant LC-CAP
20000 constant LC-TIMEOUT-MS
LC-CAP BUFFER: LC-OUT
LC-CAP BUFFER: LC-ERR

\ Runs the program in a subject child and asserts its exit code, leaving the
\ stdout and stderr lengths.
: LC-EXITS ( ptr u8 n n -- len len ) {: src:ptr srcu:n want:n :}
   src srcu LC-OUT LC-CAP >LEN LC-ERR LC-CAP >LEN LC-TIMEOUT-MS >MS SUBJECT:RUN
   {: outu:len erru:len oc :}
   src srcu LC-OUT outu LEN>N LC-ERR erru LEN>N oc want T-OUTCOME-EXITED=
   outu erru ;

\ The program runs to the end and prints this.
: LC-PRINTS ( ptr u8 n ptr u8 n -- ) {: want:ptr wantu:n :}
   0 LC-EXITS {: outu:len erru:len :}
   erru LEN>N 0 T=
   LC-OUT outu LEN>N want wantu T$= ;

\ The program's does> is refused by name, and the process ends there.
: LC-REFUSED ( ptr u8 n -- )
   70 LC-EXITS {: outu:len erru:len :}
   outu LEN>N 0 T=
   LC-ERR erru LEN>N s" hb: does> has no created word" CONTAINS? TTRUE ;

: LASTC-CASE ( -- )
   s" a forget that keeps the created record keeps it for does>" T-LABEL
   S\" create CR-LC-KEEP 7 ,\n: CR-LC-MARK ( -- ) ;\ns\" CR-LC-MARK\" FORGET-DEFS-FROM\n: CR-LC-BEHAVE ( -- ) does> ( -- n ) @ 1 + ;\nCR-LC-BEHAVE CR-LC-KEEP .\n"
   S\" 8\n" LC-PRINTS

   s" a forget that retires the created record leaves does> none to patch" T-LABEL
   S\" create CR-LC-KEEP 7 ,\n: CR-LC-MARK ( -- ) ;\ncreate CR-LC-GONE 5 ,\ns\" CR-LC-MARK\" FORGET-DEFS-FROM\n: CR-LC-BEHAVE ( -- ) does> ( -- n ) @ ;\nCR-LC-BEHAVE\n"
   LC-REFUSED

   s" an evaluate that fails retires its created record the same way" T-LABEL
   S\" create CR-LC-KEEP 7 ,\nTRUSTED: CR-LC-TRY ( -- n ) [: s\" create CR-LC-GONE 5 , CR-LC-NO-SUCH-WORD\" evaluate ;] catch ;\nCR-LC-TRY drop\n: CR-LC-BEHAVE ( -- ) does> ( -- n ) @ ;\nCR-LC-BEHAVE\n"
   LC-REFUSED

   \ undefine retires a record in place: NDICT stays, the wordlist cell says so.
   s" an undefine that retires the created record leaves does> none to patch" T-LABEL
   S\" create CR-LC-GONE 7 ,\nundefine CR-LC-GONE\n: CR-LC-BEHAVE ( -- ) does> ( -- n ) @ 1 + ;\nCR-LC-BEHAVE depth .\n"
   LC-REFUSED

   s" so does one whose body a compiled caller still reaches" T-LABEL
   S\" create CR-LC-GONE 7 ,\n: CR-LC-USE ( -- n ) CR-LC-GONE @ ;\nundefine CR-LC-GONE\n: CR-LC-BEHAVE ( -- ) does> ( -- n ) @ 1 + ;\nCR-LC-BEHAVE CR-LC-USE .\n"
   LC-REFUSED ;

public

: RUN ( -- )
   T-RESET
   ALIAS-CASE
   PLAIN-CASE
   REFUSE-CASE
   REUSE-CASE
   LASTC-CASE
   T-REPORT ;

;package

package CRECL-SUBJ

public

: CR-LOW ( -- n )
   11 ;

: CR-HIGH ( -- n )
   22 ;

;package

package CRECL-ALIAS

public

EXPORT CRECL-SUBJ:CR-LOW

;package

CRECL-TEST:RUN
