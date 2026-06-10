\ templ.fs — ICode generators for caf primitives and control structures. caf's
\ data stack lives in memory at Xds (x19); each word here appends ICode that
\ operates on it. The CG-PRIMS wordlist maps a body token to its generator;
\ walk.fs drives it. One concern: source-token -> ICode. (Generators only —
\ tokenizing/compilation is walk.fs.)

require asm.fs

 9 constant T0   10 constant T1   11 constant T2
19 constant XDS  31 constant SP   20 constant IDX  21 constant LIM

\ data-stack ops (Xds points just past TOS; full-ascending)
: g-push ( reg -- )  XDS 0 STR,  XDS XDS 8 ADDI, ;
: g-pop  ( reg -- )  XDS XDS 8 SUBI,  XDS 0 LDR, ;
: g-lit  ( n -- )    T0 swap LIT64,  T0 g-push ;

\ arithmetic / stack
: p-dup   T0 g-pop  T0 g-push  T0 g-push ;
: p-drop  XDS XDS 8 SUBI, ;
: p-swap  T0 g-pop  T1 g-pop  T0 g-push  T1 g-push ;
: p-over  T0 g-pop  T1 g-pop  T1 g-push  T0 g-push  T1 g-push ;
: p-nip   T0 g-pop  T1 g-pop  T0 g-push ;
: p-add   T1 g-pop  T0 g-pop  T0 T0 T1 ADD,  T0 g-push ;
: p-sub   T1 g-pop  T0 g-pop  T0 T0 T1 SUB,  T0 g-push ;
: p-mul   T1 g-pop  T0 g-pop  T0 T0 T1 MUL,  T0 g-push ;
: p-div   T1 g-pop  T0 g-pop  T0 T0 T1 SDIV, T0 g-push ;
: p-mod   T1 g-pop  T0 g-pop  T2 T0 T1 SDIV,  T2 T2 T1 MUL,  T0 T0 T2 SUB,  T0 g-push ;
: p-1+    T0 g-pop  T0 T0 1 ADDI,  T0 g-push ;
: p-1-    T0 g-pop  T0 T0 1 SUBI,  T0 g-push ;
: p-neg   T0 g-pop  T0 SP T0 SUB,  T0 g-push ;
: p-and   T1 g-pop  T0 g-pop  T0 T0 T1 AND,  T0 g-push ;
: p-or    T1 g-pop  T0 g-pop  T0 T0 T1 ORR,  T0 g-push ;
: p-xor   T1 g-pop  T0 g-pop  T0 T0 T1 EOR,  T0 g-push ;

\ comparisons -> Forth flag (0 / -1). cset gives 0/1, negate to 0/-1.
: g-cmp ( cond -- )  T1 g-pop  T0 g-pop  T0 T1 CMP,  T0 swap CSET,  T0 SP T0 SUB,  T0 g-push ;
: g-cmp0 ( cond -- ) T0 g-pop  T0 0 CMPI,  T0 swap CSET,  T0 SP T0 SUB,  T0 g-push ;
: p-lt  C-LT g-cmp ;  : p-gt  C-GT g-cmp ;  : p-eq  C-EQ g-cmp ;
: p-le  C-LE g-cmp ;  : p-ge  C-GE g-cmp ;  : p-ne  C-NE g-cmp ;
: p-0=  C-EQ g-cmp0 ; : p-0<  C-LT g-cmp0 ; : p-0> C-GT g-cmp0 ;

\ control-flow stack (compile-time, holds label ids)
variable CF-SP   create CF-STK 64 cells allot
variable EPILOG
: cf-reset ( -- )  0 CF-SP ! ;
: cf-push ( x -- )  CF-STK CF-SP @ cells + !  1 CF-SP +! ;
: cf-pop  ( -- x )  -1 CF-SP +!  CF-STK CF-SP @ cells + @ ;

: c-if    T0 g-pop  NEWLBL dup T0 swap CBZ,  cf-push ;
: c-else  NEWLBL dup B,  cf-pop LBL,  cf-push ;
: c-then  cf-pop LBL, ;
: c-begin NEWLBL dup LBL,  cf-push ;
: c-until T0 g-pop  cf-pop T0 swap CBZ, ;
: c-again cf-pop B, ;
: c-while T0 g-pop  NEWLBL dup T0 swap CBZ,  cf-push ;
: c-repeat cf-pop  cf-pop B,  LBL, ;            \ ( Lexit Lbegin -- ) B Lbegin; place Lexit
: c-do    IDX g-pop  LIM g-pop
          NEWLBL {: lexit :}  NEWLBL {: ltop :}  ltop LBL,
          lexit cf-push  ltop cf-push ;
: c-qdo   IDX g-pop  LIM g-pop
          NEWLBL {: lexit :}  IDX LIM CMP,  C-GE lexit BCOND,
          NEWLBL {: ltop :}  ltop LBL,
          lexit cf-push  ltop cf-push ;
: c-loop  cf-pop {: ltop :}  cf-pop {: lexit :}
          IDX IDX 1 ADDI,  IDX LIM CMP,  C-LT ltop BCOND,  lexit LBL, ;
: c-i     IDX g-push ;
: c-exit  EPILOG @ B, ;

\ token -> generator (own wordlist; gforth lookups are case-insensitive)
wordlist constant CG-PRIMS
get-current  CG-PRIMS set-current
: DUP p-dup ;  : DROP p-drop ;  : SWAP p-swap ;  : OVER p-over ;  : NIP p-nip ;
: + p-add ;  : - p-sub ;  : * p-mul ;  : / p-div ;  : MOD p-mod ;
: 1+ p-1+ ;  : 1- p-1- ;  : NEGATE p-neg ;
: AND p-and ;  : OR p-or ;  : XOR p-xor ;
: < p-lt ;  : > p-gt ;  : = p-eq ;  : <= p-le ;  : >= p-ge ;  : <> p-ne ;
: 0= p-0= ;  : 0< p-0< ;  : 0> p-0> ;
: IF c-if ;  : ELSE c-else ;  : THEN c-then ;
: BEGIN c-begin ;  : UNTIL c-until ;  : AGAIN c-again ;  : WHILE c-while ;  : REPEAT c-repeat ;
: DO c-do ;  : ?DO c-qdo ;  : LOOP c-loop ;  : I c-i ;  : EXIT c-exit ;
set-current
