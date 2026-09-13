\ The child loads a fresh checker before this file. The compiler adapter and
\ family readers below are therefore source compiled, even on a baked host.
s" src/habu/layout.f" provided
s" src/core/checker-owner-abi.f" provided
require src/compiler/native/family.f

package OWNER-FAMILY-CHECK
public
s" payload" s" 0 VARIANT vacant ;VARIANT VARIANT pair n n ;VARIANT" CHECKER-DEFSUM
private

: TRUE! ( bool -- ) 0= if 79 throw then ;
: EQ! ( n n -- ) <> if 79 throw then ;

: READERS ( -- )
   s" payload" NFAM:MATCH-FAM TRUE! {: fam:n :}
   s" payload" NFAM:CON-FAM TRUE! fam EQ!
   fam NFAM:WIDTH 3 EQ!
   fam NFAM:VARIANTS 2 EQ!
   fam NFAM:NAME$ s" payload" CORE-STR= TRUE!
   s" pair" fam NFAM:VARIANT TRUE! {: pair:n :}
   pair NFAM:TAG 1 EQ!
   fam pair NFAM:PADS 0 EQ!
   pair NFAM:PAY-CELLS 2 EQ!
   pair NFAM:PAY-TERMS 2 EQ!
   s" vacant" fam NFAM:VARIANT TRUE! {: vacant:n :}
   vacant NFAM:TAG 0 EQ!
   fam vacant NFAM:PADS 2 EQ!
   vacant NFAM:PAY-CELLS 0 EQ!
   vacant NFAM:PAY-TERMS 0 EQ!
   s" absent" fam NFAM:VARIANT nip 0= TRUE!
   s" absent" NFAM:MATCH-FAM nip 0= TRUE! ;

: RUN ( -- )
   tier@ 1 EQ!
   CHECKER-OWNER:BY-NAME? TRUE!
   READERS
   CHECKER-OWNER:CAPTURE-PREPARE
   CHECKER-OWNER:BY-NAME? 0= TRUE!
   READERS ;

RUN
;package
