\ sumtype-front.f: SUMTYPE's front end, in package TYPE-DECL.
\ It loads after the declaration runner so each declaration runs inside
\ DECLARATION-TRANSACTION:RUN, where SUMTYPE's words come from the constructor
\ participant. It reads its tokens through DECL-FRONT, so a tool's replay runs
\ the same front end.

package TYPE-DECL

: TDECL-COLLECT ( -- bool )   \ false: input ended unterminated
   BEGIN
      DECL-FRONT:SOURCE-NEXT
      dup STR:LEN 0= IF drop RES-FALSE EXIT THEN
      dup s" ;sumtype" STR:=CI IF drop RES-TRUE EXIT THEN
      TDECL-TOKEN+
   AGAIN ;

: TDECL-SUM-GEN-TXN ( -- )
   [: CHECKER-DEFSUM-BODY GENERATED-DECL-CTOR:ARM ;]
   DECLARATION-TRANSACTION:RUN ;

: TDECL-SUMTYPE ( -- )
   SRCLOC:TOKEN$ >str {: kw:str :}
   DECL-FRONT:SOURCE-TOKEN {: ns:str :}
   kw ns REJECTION:NOT-A-NAME
   TDECL-CLEAR
   \ A sum with no name runs to the header's rejection, at its keyword.
   TDECL-COLLECT  ns STR:LEN 0= or
   s" sumtype" ns TDECL-BUF TDECL-U @ >str TDECL-CTX!
   ns STR:LEN 0= IF kw TDECL-PLACE! THEN
   0= IF [: TDECL-NOEND-BODY ;] TDECL-RUN EXIT THEN
   [: TDECL-SUM-GEN-TXN ;] TDECL-RUN ;

global

\ A documented global: it parses its body to its closer at interpret time, so
\ its checked effect is ( -- ).
: SUMTYPE ( -- ) TDECL-SUMTYPE ;
INTRINSIC-DECLARATION INTRINSIC

;package
