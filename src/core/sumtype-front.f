\ sumtype-front.f: SUMTYPE's front end, in package TYPE-DECL. It loads after
\ the declaration runner so a SUMTYPE runs inside DECLARATION-TRANSACTION:RUN,
\ whose constructor participant makes its words, and it reads its tokens through
\ DECL-FRONT:SOURCE-NEXT, so a tool's replay runs the same front end.

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

;package

using TYPE-DECL

\ A documented global: it parses its body to ;SUMTYPE at interpret time, so its
\ checked effect is ( -- ).
: SUMTYPE ( -- ) TDECL-SUMTYPE ;

;using
