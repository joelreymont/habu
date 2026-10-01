\ A stripped image must capture the final value of each fixed quotation field.
\ Whole-record stores replace both sum alternatives before the native link.
package STRIPPED-QUOT-FIELD

STRUCTURE entry 0 DERIVE addr
   FIELD handler [ n -- n ]
;STRUCTURE

STRUCTURE generic-entry 1 DERIVE addr
   FIELD value a
   FIELD handler [ a -- a ]
;STRUCTURE

STRUCTURE hook 0 FIELD handler [ n -- n ] ;STRUCTURE
SUMTYPE choice 0
   VARIANT empty ;VARIANT
   VARIANT scalar n ;VARIANT
   VARIANT present hook ;VARIANT
;SUMTYPE
STRUCTURE holder 0 FIELD choice choice ;STRUCTURE
STRUCTURE envelope 0 FIELD item holder ;STRUCTURE

TYPED-VARIABLE ROW entry
1 LAYOUT-BUFFER LB entry
TYPED-VARIABLE GENERIC-ROW generic-entry<n>
TYPED-VARIABLE PRESENT-ROW envelope
TYPED-VARIABLE SCALAR-ROW envelope
3 LAYOUT-BUFFER BATCH envelope
\ The stripped link refuses code-address values in DATA even for typed scalars.
\ This ordinary scalar still exposes a stale XT declaration after q -> scalar.
variable SCALAR-VALUE

: READ-CHOICE ( envelope -- n )
   ENVELOPE-UNMAKE HOLDER-UNMAKE
   MATCH choice
      empty OF 0 ENDOF
      scalar OF 10 + ENDOF
      present OF HOOK-UNMAKE 35 swap execute ENDOF
   ;MATCH ;

: INIT ( -- )
   [: 7 + ;] ENTRY-MAKE ROW !
   [: 8 + ;] ENTRY-MAKE 0 LB !
   10 [: 7 + ;] GENERIC-ENTRY-MAKE GENERIC-ROW !
   456 SCALAR-VALUE !
   SCALAR-VALUE @ construct choice scalar HOLDER-MAKE ENVELOPE-MAKE PRESENT-ROW !
   [: 7 + ;] HOOK-MAKE construct choice present HOLDER-MAKE ENVELOPE-MAKE PRESENT-ROW !
   [: 9 + ;] HOOK-MAKE construct choice present HOLDER-MAKE ENVELOPE-MAKE SCALAR-ROW !
   SCALAR-VALUE @ construct choice scalar HOLDER-MAKE ENVELOPE-MAKE SCALAR-ROW !
   [: 6 + ;] HOOK-MAKE construct choice present HOLDER-MAKE ENVELOPE-MAKE 0 BATCH !
   SCALAR-VALUE @ construct choice scalar HOLDER-MAKE ENVELOPE-MAKE 0 BATCH !
   SCALAR-VALUE @ construct choice scalar HOLDER-MAKE ENVELOPE-MAKE 1 BATCH !
   [: 8 + ;] HOOK-MAKE construct choice present HOLDER-MAKE ENVELOPE-MAKE 1 BATCH !
   construct choice empty HOLDER-MAKE ENVELOPE-MAKE 2 BATCH ! ;

: ASSERT-EQ ( n n -- )
   <> if s" stripped quotation field: wrong value" 76 die then ;

public
: CHECK ( -- )
   35 ROW ENTRY-HANDLER @ execute 42 ASSERT-EQ
   34 0 LB ENTRY-HANDLER @ execute 42 ASSERT-EQ
   35 GENERIC-ROW GENERIC-ENTRY-HANDLER @ execute 42 ASSERT-EQ
   PRESENT-ROW @ READ-CHOICE 42 ASSERT-EQ
   SCALAR-ROW @ READ-CHOICE SCALAR-VALUE @ 10 + ASSERT-EQ
   0 BATCH @ READ-CHOICE SCALAR-VALUE @ 10 + ASSERT-EQ
   1 BATCH @ READ-CHOICE 43 ASSERT-EQ
   2 BATCH @ READ-CHOICE 0 ASSERT-EQ
   s" quotation fields=ok" type cr ;

INIT
;package

: MAIN ( -- ) STRIPPED-QUOT-FIELD:CHECK ;
