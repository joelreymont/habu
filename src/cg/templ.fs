\ templ.fs — compile a Forth word BODY (stack code) to native ICode. caf's data
\ stack lives in memory at Xds (x19); each primitive is an ICode generator over
\ it. WALK-BODY tokenizes a body string, emitting LIT for numbers and the
\ matching generator for each primitive. This is the bridge: checked-Forth source
\ -> ARM64 machine code. (MVP op set; the CODEGEN-HOOK wiring that feeds it CAP$
\ from real `:` definitions is the remaining integration step.)

require exec.fs

 9 constant T0    10 constant T1    19 constant XDS    31 constant SP

\ data-stack ops (Xds points just past TOS; full-ascending)
: g-push ( reg -- )   XDS 0 STR,  XDS XDS 8 ADDI, ;
: g-pop  ( reg -- )   XDS XDS 8 SUBI,  XDS 0 LDR, ;
: g-lit  ( n -- )     T0 swap LIT64,  T0 g-push ;

\ primitive generators
: p-dup   T0 g-pop  T0 g-push  T0 g-push ;
: p-drop  XDS XDS 8 SUBI, ;
: p-swap  T0 g-pop  T1 g-pop  T0 g-push  T1 g-push ;
: p-over  T0 g-pop  T1 g-pop  T1 g-push  T0 g-push  T1 g-push ;
: p-add   T1 g-pop  T0 g-pop  T0 T0 T1 ADD,  T0 g-push ;
: p-sub   T1 g-pop  T0 g-pop  T0 T0 T1 SUB,  T0 g-push ;
: p-mul   T1 g-pop  T0 g-pop  T0 T0 T1 MUL,  T0 g-push ;
: p-1+    T0 g-pop  T0 T0 1 ADDI,  T0 g-push ;
: p-1-    T0 g-pop  T0 T0 1 SUBI,  T0 g-push ;

\ primitive name -> generator (own wordlist; case-insensitive lookup)
wordlist constant CG-PRIMS
get-current  CG-PRIMS set-current
: DUP   p-dup ;     : DROP  p-drop ;    : SWAP  p-swap ;   : OVER  p-over ;
: +     p-add ;     : -     p-sub ;     : *     p-mul ;
: 1+    p-1+ ;      : 1-    p-1- ;
set-current

: EMIT-PRIM ( a u -- )
   2dup CG-PRIMS search-wordlist ?dup if
      drop nip nip execute
   else  cr ." cg: unknown prim: " type cr  E-NO-ENC throw  then ;

: EMIT-TOKEN ( a u -- )
   2dup s>number? if  2>r 2drop 2r> d>s g-lit
   else  2drop EMIT-PRIM  then ;

: WALK-BODY {: a u | end cur ts :}
   a u + to end   a to cur
   begin
      begin cur end < cur c@ bl = and while cur 1+ to cur repeat
      cur end <
   while
      cur to ts
      begin cur end < cur c@ bl <> and while cur 1+ to cur repeat
      ts  cur ts -  EMIT-TOKEN
   repeat ;

\ Compile a body with one i64 input pushed first; exit(TOS).
: COMPILE-WORD {: ba bu input -- :}
   ICODE-RESET
   SP SP 256 SUBI,        \ reserve 256B data stack on the machine stack
   XDS SP 0 ADDI,         \ Xds = sp
   input g-lit
   ba bu WALK-BODY
   0 g-pop                \ x0 = TOS (exit status = x0 & 0xff)
   16 1 MOVZ,  $80 SVC, ;

: NATIVE-EVAL ( ba bu input -- rc )
   COMPILE-WORD  s" /tmp/caf-word" RUN-EXE ;
