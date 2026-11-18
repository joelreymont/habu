split-str
\ Indexed compiler lookup follows live scope and definition changes.
require src/lib/test.f
require src/habu/fnv.f
require src/habu/verify-source.f

17 constant NDB-GLOBAL

package NDB-LEFT
7 constant HIDDEN
10 constant SHARED
public
20 constant SHARED
19 constant NDB-GLOBAL
29 constant SOLO
31 constant AMBIG
41 constant NDB-COLLIDE-608
42 constant NDB-COLLIDE-1146
;package

package NDB-RIGHT
public
30 constant SHARED
32 constant AMBIG
;package

package NDB-IMMEDIATE
private
: REPLAY-RA ( -- ) [: s" : NDB-RA ( -- n ) 5 ;" VERIFY:SOURCE-BUF-IN-SCOPE ;] CHECK:SCOPED ;
public

: REPLAY ( ^u8 n -- ) EFFECT:EMPTY EFFECT:N EFFECT:OUT define-word REPLAY-RA ;

: REPLAY-DOES ( ^u8 n -- )
   EFFECT:EMPTY EFFECT:N EFFECT:IN define-word
   s" create , does> ( -- n )" evaluate-closed REPLAY-RA ;

;package

package NDICT-BINDING-TEST
using NDICT

\ Test-only record identity and retired-wordlist inspection.
\ No raw record is exported.
: RECORD ( ^u8 n n -- ^n ) CHECKER-RESOLVE:WL-PROBE ;
CAST: REF-WORD ( n -- [ -- n ] )
: REF-VALUE ( n n -- n ) DEF-OCC:CALLABLE REF-WORD execute ;
variable ROLL-SLOT
variable ROLL-OCC


: EV ( ^u8 n -- ) evaluate-closed ;


: OPEN-SCOPE ( -- )
   S\" package NDB-LEFT using NDICT s\q SHARED\q FIXED-VALUE 10 T= ;using ;package" EV
   S\" package NDB-LEFT using NDB-RIGHT using NDICT s\q SHARED\q FIXED-VALUE 10 T= ;using ;using ;package" EV
   s" NDB-LEFT:SHARED" FIXED-VALUE 20 T=
   s" NDB-LEFT:HIDDEN" SPELL-START 0 T=
   s" HIDDEN" SPELL-START 0 T=
   s" NDB-LEFT:SHARED:BAD" SPELL-START 0 T=
   s" NDB-NO-PACKAGE:SHARED" SPELL-START 0 T= ;


: AMBIGUOUS ( -- )
   S\" using NDB-LEFT using NDB-RIGHT using NDICT s\q AMBIG\q SPELL-START drop" EV ;


: USED-SCOPE ( -- )
   S\" using NDB-LEFT using NDB-LEFT using NDICT s\q SOLO\q FIXED-VALUE 29 T= ;using ;using ;using" EV
   S\" using NDB-LEFT using NDICT s\q NDB-GLOBAL\q FIXED-VALUE 17 T= ;using ;using" EV
   ['] AMBIGUOUS E-USING-AMBIGUOUS TTHROWS
   s" SOLO" SPELL-START 0 T=
   s" using NDB-LEFT : NDB-USED ( -- n ) SOLO ; ;using NDB-USED 29 T=" EV ;


: HASH-SLOT ( str -- n )
   {: name:^ size:n :}
   FNV:BASIS
   size 0 ?do name i + c@ XREF-FOLD-C FNV:STEP loop
   HIDX-SLOTS 1- and ;


: COLLISIONS ( -- )
   s" NDB-COLLIDE-608" HASH-SLOT s" NDB-COLLIDE-1146" HASH-SLOT T=
   s" NDB-LEFT:NDB-COLLIDE-608" FIXED-VALUE 41 T=
   s" ndb-left:ndb-collide-1146" FIXED-VALUE 42 T=
   s" undefine NDB-LEFT:NDB-COLLIDE-608" EV
   s" NDB-LEFT:NDB-COLLIDE-608" SPELL-START 0 T=
   s" NDB-LEFT:NDB-COLLIDE-1146" FIXED-VALUE 42 T= ;


: REPLAY-PENDING ( -- )
   S\" s\q NDB-REPLAYED\q NDB-IMMEDIATE:REPLAY 81 ; NDB-REPLAYED 81 T=" EV
   S\" s\q NDB-KEEPER\q NDB-IMMEDIATE:REPLAY-DOES @ ; 83 NDB-KEEPER NDB-KEPT NDB-KEPT 83 T=" EV ;


: REDECLARATION ( -- )
   s" 31 constant NDB-REBIND : NDB-BEFORE ( -- n ) NDB-REBIND ;" EV
   s" NDB-REBIND" 0 RECORD DEF-OCC:SELECT {: old-slot:n old-occ:n :}
   s" undefine NDB-REBIND 32 constant NDB-REBIND" EV
   old-slot old-occ REF-VALUE 31 T=
   s" NDB-REBIND" 0 RECORD DEF-OCC:SELECT {: new-slot:n new-occ:n :}
   new-slot new-occ REF-VALUE 32 T=
   old-occ new-occ T<>
   s" : NDB-AFTER ( -- n ) NDB-REBIND ; NDB-BEFORE 31 T= NDB-AFTER 32 T=" EV
   s" NDB-REBIND" FIXED-VALUE 32 T= ;


: RETIRED-LATEST ( -- )
   s" 11 constant NDB-RETIRED" EV
   s" NDB-RETIRED" SPELL-START {: first:n :}
   s" undefine NDB-RETIRED 22 constant NDB-RETIRED" EV
   s" NDB-RETIRED" SPELL-START {: latest:n :}
   s" undefine NDB-RETIRED" EV
   s" NDB-RETIRED" SPELL-START 0 T=
   s" NDB-RETIRED" XREF-RETIRED-WL RECORD XREF-START latest T=
   latest first T<>
   s" NDB-RETIRED" XREF-RETIRED-WL RECORD
   s" NDB-RETIRED" XREF-RETIRED-WL XREF-FIND-WL = TTRUE ;


public
: CAPTURE-ROLLBACK ( -- )
   s" NDB-ROLLBACK" 0 RECORD DEF-OCC:SELECT
   ROLL-OCC !  ROLL-SLOT ! ;
private

: FAIL-EVALUATION ( -- )
   S\" : NDB-ROLLBACK ( -- n ) 71 ; using NDICT s\q NDB-ROLLBACK\q SPELL-START 0<> TTRUE ;using NDICT-BINDING-TEST:CAPTURE-ROLLBACK 73 throw" EV ;


: ROLLBACK-KEPT ( -- )
   ndict@ {: before:n :}
   \ The whole source frame, the successful definition included, keeps its record.
   ['] FAIL-EVALUATION 73 TTHROWS
   ndict@ before 1+ T=
   ROLL-SLOT @ before T=
   s" NDB-ROLLBACK" SPELL-START 0 T=
   s" 74 constant NDB-AFTER-ROLLBACK" EV
   s" NDB-AFTER-ROLLBACK" 0 RECORD before 1+ XREF-REC = TTRUE
   s" NDB-AFTER-ROLLBACK" FIXED-VALUE 74 T=
   \ The occurrence the failed text selected still reaches its word.
   ROLL-SLOT @ ROLL-OCC @ REF-VALUE 71 T= ;


: RUN ( -- )
   T-RESET
   s" package visibility and scope precedence" T-LABEL OPEN-SCOPE T-NEXT
   s" using identity and ambiguity" T-LABEL USED-SCOPE T-NEXT
   s" folded hash collisions retain exact identity" T-LABEL COLLISIONS T-NEXT
   s" a checker replay inside a definition leaves it pending" T-LABEL REPLAY-PENDING T-NEXT
   s" compiled calls retain the binding they captured" T-LABEL REDECLARATION T-NEXT
   s" the retired wordlist returns the latest matching record" T-LABEL RETIRED-LATEST T-NEXT
   s" failed source evaluation keeps its records and occurrences" T-LABEL ROLLBACK-KEPT T-NEXT
   T-REPORT ;

' RUN
;using
;package
execute
