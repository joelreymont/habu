\ Indexed compiler lookup follows live scope and definition changes.
require src/lib/test/assert.f
require src/habu/fnv.f
require src/habu/verify-source.f
require src/lib/fmt.f
require src/compiler/native/dict.f

17 constant NDB-GLOBAL

package NDB-LEFT
7 constant HIDDEN
10 constant SHARED
public
get-current constant WID
20 constant SHARED
19 constant NDB-GLOBAL
29 constant SOLO
31 constant AMBIG
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

: REPLAY ( str -- ) EFFECT:EMPTY EFFECT:N EFFECT:OUT define-word REPLAY-RA ;

: REPLAY-DOES ( str -- )
   EFFECT:EMPTY EFFECT:N EFFECT:IN define-word
   s" create , does> ( -- n )" evaluate-closed REPLAY-RA ;

;package

package NDICT-BINDING-TEST

\ Test-only record identity and retired-wordlist inspection.
\ No raw record is exported.
: RECORD ( str n -- ^n ) CHECKER-RESOLVE:WL-PROBE ;
CAST: REF-WORD ( n -- [ -- n ] )
: REF-VALUE ( n n -- n ) DEF-OCC:CALLABLE REF-WORD execute ;
variable ROLL-SLOT
variable ROLL-OCC


: EV ( str -- ) evaluate-closed ;


: OPEN-SCOPE ( -- )
   S\" package NDB-LEFT using NDICT s\q SHARED\q str> FIXED-VALUE 10 T= ;using ;package" EV
   S\" package NDB-LEFT using NDB-RIGHT using NDICT s\q SHARED\q str> FIXED-VALUE 10 T= ;using ;using ;package" EV
   s" NDB-LEFT:SHARED" str> NDICT:FIXED-VALUE 20 T=
   s" NDB-LEFT:HIDDEN" str> NDICT:SPELL-START 0 T=
   s" HIDDEN" str> NDICT:SPELL-START 0 T=
   s" NDB-LEFT:SHARED:BAD" str> NDICT:SPELL-START 0 T=
   s" NDB-NO-PACKAGE:SHARED" str> NDICT:SPELL-START 0 T= ;


: AMBIGUOUS ( -- )
   S\" using NDB-LEFT using NDB-RIGHT using NDICT s\q AMBIG\q str> SPELL-START drop" EV ;


: USED-SCOPE ( -- )
   S\" using NDB-LEFT using NDB-LEFT using NDICT s\q SOLO\q str> FIXED-VALUE 29 T= ;using ;using ;using" EV
   S\" using NDB-LEFT using NDICT s\q NDB-GLOBAL\q str> FIXED-VALUE 17 T= ;using ;using" EV
   ['] AMBIGUOUS E-USING-AMBIGUOUS TTHROWS
   s" SOLO" str> NDICT:SPELL-START 0 T=
   s" using NDB-LEFT : NDB-USED ( -- n ) SOLO ; ;using NDB-USED 29 T=" EV ;


: HASH-SLOT ( str n -- n )
   {: name:str wid:n :}
   FNV:BASIS
   name STR:LEN 0 ?do name STR:DATA i + c@ STR:ASCII-LOWER FNV:STEP loop
   wid xor HIDX-MIX *  64 HIDX-BITS - rshift ;


4096 constant PAIR-CAP
PAIR-CAP TYPED-BUFFER PAIRS n
128 constant TEXT-CAP
create TEXT-BUF TEXT-CAP allot
variable TEXT-U


: TEXT$ ( -- str ) TEXT-BUF TEXT-U @ >str ;


: TEXT+ ( str -- )
   {: src:str :}
   src STR:DATA TEXT-BUF TEXT-U @ + src STR:LEN BYTE-COPY
   TEXT-U @ src STR:LEN + TEXT-U ! ;


: NUM+ ( n -- ) FMT:INT$ TEXT+ ;


\ TEXT holds the prefix then k's digits.
: SPELL ( str n -- )
   {: p:str k:n :}
   0 TEXT-U !  p TEXT+  k NUM+ ;


: SLOT-OF ( n -- n )
   {: k:n :}
   s" NDB-COLLIDE-" k SPELL TEXT$ NDB-LEFT:WID HASH-SLOT ;


: EARLIER ( n -- n )                   \ an index below k on k's slot, or -1
   {: k:n :}
   k 0 ?do i PAIRS @ k PAIRS @ = if i unloop exit then loop
   -1 ;


\ The odds that no two of PAIR-CAP names share one of the 2^HIDX-BITS slots
\ are e^-32.
: FIND-PAIR ( -- n n bool )
   PAIR-CAP 0 ?do
      i SLOT-OF i PAIRS !
      i EARLIER dup 0< 0= if i true unloop exit then drop
   loop -1 -1 false ;


: DEFINE-PAIR ( n n -- )
   {: a:n b:n :}
   s" package NDB-LEFT public 41 constant NDB-COLLIDE-"
   a SPELL
   s"  42 constant NDB-COLLIDE-" TEXT+ b NUM+ s"  ;package" TEXT+
   TEXT$ EV ;


: CHECK-PAIR ( n n -- )
   {: a:n b:n :}
   a b DEFINE-PAIR
   s" NDB-LEFT:NDB-COLLIDE-" a SPELL TEXT$ str> NDICT:FIXED-VALUE 41 T=
   s" ndb-left:ndb-collide-" b SPELL TEXT$ str> NDICT:FIXED-VALUE 42 T=
   s" undefine NDB-LEFT:NDB-COLLIDE-" a SPELL TEXT$ EV
   s" NDB-LEFT:NDB-COLLIDE-" a SPELL TEXT$ str> NDICT:SPELL-START 0 T=
   s" NDB-LEFT:NDB-COLLIDE-" b SPELL TEXT$ str> NDICT:FIXED-VALUE 42 T= ;


: COLLISIONS ( -- )
   FIND-PAIR dup TTRUE {: a:n b:n found:bool :}
   found if a b CHECK-PAIR then ;


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
   s" NDB-REBIND" str> NDICT:FIXED-VALUE 32 T= ;


: RETIRED-LATEST ( -- )
   s" 11 constant NDB-RETIRED" EV
   s" NDB-RETIRED" str> NDICT:SPELL-START {: first:n :}
   s" undefine NDB-RETIRED 22 constant NDB-RETIRED" EV
   s" NDB-RETIRED" str> NDICT:SPELL-START {: latest:n :}
   s" undefine NDB-RETIRED" EV
   s" NDB-RETIRED" str> NDICT:SPELL-START 0 T=
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
   S\" : NDB-ROLLBACK ( -- n ) 71 ; using NDICT s\q NDB-ROLLBACK\q str> SPELL-START 0<> TTRUE ;using NDICT-BINDING-TEST:CAPTURE-ROLLBACK 73 throw" EV ;


: ROLLBACK-KEPT ( -- )
   ndict@ {: before:n :}
   \ The whole source frame, the successful definition included, keeps its record.
   ['] FAIL-EVALUATION 73 TTHROWS
   ndict@ before 1+ T=
   ROLL-SLOT @ before T=
   s" NDB-ROLLBACK" str> NDICT:SPELL-START 0 T=
   s" 74 constant NDB-AFTER-ROLLBACK" EV
   s" NDB-AFTER-ROLLBACK" 0 RECORD before 1+ XREF-REC = TTRUE
   s" NDB-AFTER-ROLLBACK" str> NDICT:FIXED-VALUE 74 T=
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
;package
execute
