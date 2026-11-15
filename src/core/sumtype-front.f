\ sumtype-front.f: SUMTYPE's and PRODUCT's front ends, in package TYPE-DECL.
\ It loads after the declaration runner so each declaration runs inside
\ DECLARATION-TRANSACTION:RUN, where SUMTYPE's words come from the constructor
\ participant and PRODUCT's from STRUCTURE-MAKE, as a STRUCTURE's do. Both read
\ their tokens through DECL-FRONT:SOURCE-NEXT, so a tool's replay runs the same
\ front end.

package TYPE-DECL

: TDECL-COLLECT ( ptr u8 n -- bool )   \ closer; false: input ended unterminated
   {: ca:ptr cu:n :}
   BEGIN
      DECL-FRONT:SOURCE-NEXT
      dup 0= IF 2drop RES-FALSE EXIT THEN
      2dup ca cu CORE-STR=CI IF 2drop RES-TRUE EXIT THEN
      TDECL-TOKEN+
   AGAIN ;

: TDECL-SUM-GEN-TXN ( -- )
   [: CHECKER-DEFSUM-BODY GENERATED-DECL-CTOR:ARM ;]
   DECLARATION-TRANSACTION:RUN ;

public

: TDECL-SUMTYPE ( -- )
   DECL-FRONT:SOURCE-NEXT {: na:ptr nu:n :}
   TDECL-CLEAR
   s" ;sumtype" TDECL-COLLECT
   s" sumtype" na nu TDECL-BUF TDECL-U @ TDECL-CTX!
   0= IF [: TDECL-NOEND-BODY ;] TDECL-RUN EXIT THEN
   [: TDECL-SUM-GEN-TXN ;] TDECL-RUN ;

private

: TDECL-PRODUCT-CLOSE ( n -- ) {: fam:n :}
   TDP-EVT @ fam STRUCTURE-MAKE:GENERATE
   fam TFAM:TFAM-DERIVE-INIT? IF fam STRUCTURE-MAKE:ARM THEN ;

: TDECL-PRODUCT-GEN-TXN ( -- )
   [: CHECKER-DEFPRODUCT-BODY TDECL-PRODUCT-CLOSE ;]
   DECLARATION-TRANSACTION:RUN ;

public

: TDECL-PRODUCT ( -- )
   DECL-FRONT:SOURCE-NEXT {: na:ptr nu:n :}
   TDECL-CLEAR
   s" ;product" TDECL-COLLECT
   s" product" na nu TDECL-BUF TDECL-U @ TDECL-CTX!
   0= IF [: TDECL-PRODUCT-NOEND-BODY ;] TDECL-RUN EXIT THEN
   [: TDECL-PRODUCT-GEN-TXN ;] TDECL-RUN ;

;package

using TYPE-DECL

\ Documented globals: each parses its body to its closer at interpret time, so
\ its checked effect is ( -- ).
: SUMTYPE ( -- ) TDECL-SUMTYPE ;
: PRODUCT ( -- ) TDECL-PRODUCT ;

;using
