\ Indexed compiler lookup follows live scope and definition changes.
require lib/test.f

17 constant NDB-GLOBAL
1 constant NDB-MUTABLE

package NDB-LEFT
7 constant HIDDEN
10 constant SHARED
public
20 constant SHARED
19 constant NDB-GLOBAL
29 constant SOLO
31 constant AMBIG
41 constant NDB-COLLIDE-548
42 constant NDB-COLLIDE-1022
;package

package NDB-RIGHT
public
30 constant SHARED
32 constant AMBIG
;package

package NDB-IMMEDIATE
using NDICT
public

: CHECKED-SCOPE ( -- ) s" int-mark" CALL-TARGET 0 T= ;
immediate
s" NDB-IMMEDIATE:CHECKED-SCOPE" 0 parse-imm

: TRUSTED-SCOPE ( -- ) s" int-mark" CALL-TARGET 0<> TTRUE ;
immediate
s" NDB-IMMEDIATE:TRUSTED-SCOPE" 0 parse-imm

: RETIRE-BINDING ( -- )
   s" NDB-MUTABLE" SPELL-START 0<> TTRUE
   s" NDB-MUTABLE" UNDEFINE-NAME
   s" NDB-MUTABLE" SPELL-START 0 T= ;
immediate
s" NDB-IMMEDIATE:RETIRE-BINDING" 0 parse-imm

;using
;package

package NDICT-BINDING-TEST
using NDICT

\ Test-only record identity and retired-wordlist inspection. No raw record is
\ exported. Retirement: habu-attr-and-remove-2b13e978.
TRUSTED: RECORD ( ptr u8 n n -- ptr n ) xref-search-wl ;


: EV ( ptr u8 n -- ) INCLUDE-EVALUATE ;


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


: HASH-SLOT ( ptr u8 n -- n )
   {: name:ptr size:n :}
   $CBF29CE484222325
   size 0 ?do name i + c@ XREF-FOLD-C xor $100000001B3 * loop
   HIDX-SLOTS 1- and ;


: COLLISIONS ( -- )
   s" NDB-COLLIDE-548" HASH-SLOT s" NDB-COLLIDE-1022" HASH-SLOT T=
   s" NDB-LEFT:NDB-COLLIDE-548" FIXED-VALUE 41 T=
   s" ndb-left:ndb-collide-1022" FIXED-VALUE 42 T=
   s" undefine NDB-LEFT:NDB-COLLIDE-548" EV
   s" NDB-LEFT:NDB-COLLIDE-548" SPELL-START 0 T=
   s" NDB-LEFT:NDB-COLLIDE-1022" FIXED-VALUE 42 T= ;


: IMMEDIATES ( -- )
   s" NDB-IMMEDIATE:CHECKED-SCOPE" CALL-TARGET 0 T=
   s" : NDB-CHECKED ( -- ) NDB-IMMEDIATE:CHECKED-SCOPE ;" EV
   s" TRUSTED: NDB-TRUSTED ( -- ) NDB-IMMEDIATE:TRUSTED-SCOPE ;" EV
   s" int-mark" CALL-TARGET 0 T=
   s" : NDB-MUTATION ( -- ) NDB-IMMEDIATE:RETIRE-BINDING ;" EV
   s" NDB-MUTABLE" SPELL-START 0 T=
   s" 2 constant NDB-MUTABLE : NDB-MUTATED ( -- n ) NDB-MUTABLE ; NDB-MUTATED 2 T=" EV ;


: REDECLARATION ( -- )
   s" 31 constant NDB-REBIND : NDB-BEFORE ( -- n ) NDB-REBIND ;" EV
   s" undefine NDB-REBIND 32 constant NDB-REBIND" EV
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


: HIDE-REUSE ( -- )
   s" : NDB-HIDE ( -- n ) 51 ; : NDB-HIDE-TAIL ( -- n ) 52 ;" EV
   s" NDB-HIDE" 0 RECORD {: old:ptr :}
   s" NDB-HIDE" HIDE-DEFS-FROM
   s" NDB-HIDE" SPELL-START 0 T=
   s" NDB-HIDE-TAIL" SPELL-START 0 T=
   s" : NDB-HIDE ( -- n ) 53 ;" EV
   s" NDB-HIDE" 0 RECORD old = TTRUE
   s" : NDB-HIDE-CALL ( -- n ) NDB-HIDE ; NDB-HIDE-CALL 53 T=" EV ;


: FORGET-REUSE ( -- )
   s" : NDB-FORGET ( -- n ) 61 ;" EV
   s" NDB-FORGET" 0 RECORD {: old:ptr :}
   old XREF-START {: start:n :}
   s" NDB-FORGET" FORGET-DEFS-FROM
   s" NDB-FORGET" SPELL-START 0 T=
   s" : NDB-FORGET ( -- n ) 62 ;" EV
   s" NDB-FORGET" 0 RECORD old = TTRUE
   s" NDB-FORGET" SPELL-START start T=
   s" : NDB-FORGET-CALL ( -- n ) NDB-FORGET ; NDB-FORGET-CALL 62 T=" EV ;


: FAIL-EVALUATION ( -- )
   S\" : NDB-ROLLBACK ( -- n ) 71 ; using NDICT s\q NDB-ROLLBACK\q SPELL-START 0<> TTRUE 73 throw" EV ;


: ROLLBACK-REUSE ( -- )
   ndict@ {: before:n :}
   \ The whole source frame, including the successful definition, must roll back.
   ['] FAIL-EVALUATION 73 TTHROWS
   ndict@ before T=
   s" 74 constant NDB-AFTER-ROLLBACK" EV
   s" NDB-AFTER-ROLLBACK" 0 RECORD before XREF-REC = TTRUE
   s" NDB-ROLLBACK" SPELL-START 0 T=
   s" NDB-AFTER-ROLLBACK" FIXED-VALUE 74 T= ;


: RUN ( -- )
   T-RESET
   s" package visibility and scope precedence" T-LABEL OPEN-SCOPE T-NEXT
   s" using identity and ambiguity" T-LABEL USED-SCOPE T-NEXT
   s" folded hash collisions retain exact identity" T-LABEL COLLISIONS T-NEXT
   s" immediate compilation re-reads binding and trusted authority" T-LABEL IMMEDIATES T-NEXT
   s" compiled calls retain the binding they captured" T-LABEL REDECLARATION T-NEXT
   s" the retired wordlist returns the latest matching record" T-LABEL RETIRED-LATEST T-NEXT
   s" hide and forget invalidate recyclable slots" T-LABEL HIDE-REUSE FORGET-REUSE T-NEXT
   s" failed source evaluation invalidates recyclable slots" T-LABEL ROLLBACK-REUSE T-NEXT
   T-REPORT ;

' RUN
;using
;package
execute
