split-str
\ sumtype-front.f: SUMTYPE's front end, in package TYPE-DECL.
\ It loads after the declaration runner so each declaration runs inside
\ DECLARATION-TRANSACTION:RUN, where SUMTYPE's words come from the constructor
\ participant. It reads its tokens through DECL-FRONT, so a tool's replay runs
\ the same front end.

package TYPE-DECL

: TDECL-COLLECT ( -- bool )   \ false: input ended unterminated
   BEGIN
      DECL-FRONT:SOURCE-NEXT
      dup 0= IF 2drop RES-FALSE EXIT THEN
      2dup s" ;sumtype" STR:=CI IF 2drop RES-TRUE EXIT THEN
      TDECL-TOKEN+
   AGAIN ;

: TDECL-SUM-GEN-TXN ( -- )
   [: CHECKER-DEFSUM-BODY GENERATED-DECL-CTOR:ARM ;]
   DECLARATION-TRANSACTION:RUN ;

: TDECL-SUMTYPE ( -- )
   SRCLOC:TOKEN$ {: kw:^ ku:n :}
   DECL-FRONT:SOURCE-TOKEN {: na:^ nu:n :}
   kw ku na nu REJECTION:NOT-A-NAME
   TDECL-CLEAR
   \ A sum with no name runs to the header's rejection, at its keyword.
   TDECL-COLLECT  nu 0= or
   s" sumtype" na nu TDECL-BUF TDECL-U @ TDECL-CTX!
   nu 0= IF kw ku TDECL-PLACE! THEN
   0= IF [: TDECL-NOEND-BODY ;] TDECL-RUN EXIT THEN
   [: TDECL-SUM-GEN-TXN ;] TDECL-RUN ;

global

\ A documented global: it parses its body to its closer at interpret time, so
\ its checked effect is ( -- ).
: SUMTYPE ( -- ) TDECL-SUMTYPE ;
INTRINSIC-DECLARATION INTRINSIC

;package
