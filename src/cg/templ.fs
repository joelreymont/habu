\ templ.fs — ICode generators for caf primitives and control structures. caf's
\ data stack lives in memory at Xds (x19); each word here appends ICode that
\ operates on it. The CG-PRIMS wordlist maps a body token to its generator;
\ walk.fs drives it. One concern: source-token -> ICode. (Generators only —
\ tokenizing/compilation is walk.fs.)

require asm.fs

 9 constant T0   10 constant T1   11 constant T2
19 constant XDS  31 constant SP   25 constant RSP

\ data-stack ops (Xds points just past TOS; full-ascending)
: g-push ( reg -- )  XDS 0 STR,  XDS XDS 8 ADDI, ;
: g-pop  ( reg -- )  XDS XDS 8 SUBI,  XDS 0 LDR, ;
: g-lit  ( n -- )    T0 swap LIT64,  T0 g-push ;
\ return stack (grows down; RSP points at top; [RSP]=index, [RSP+8]=limit)
: g-rpush ( reg -- )  RSP RSP 8 SUBI,  RSP 0 STR, ;
: g-rpop  ( reg -- )  RSP 0 LDR,  RSP RSP 8 ADDI, ;
\ carve the data stack (Xds=sp, up) + return stack (RSP=sp+n, down) on the machine
\ stack. n must hold both peaks; data and return grow toward each other (no guard).
: g-prologue {: n -- :}  SP SP n SUBI,  XDS SP 0 ADDI,  RSP SP 0 ADDI,  RSP RSP n ADDI, ;
: g-exit-tos ( -- )  0 g-pop  16 1 MOVZ,  $80 SVC, ;     \ exit(TOS)
: g-exit0    ( -- )  0 0 MOVZ,  16 1 MOVZ,  $80 SVC, ;   \ exit(0)

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
: p-0=  C-EQ g-cmp0 ; : p-0<  C-LT g-cmp0 ; : p-0>  C-GT g-cmp0 ;
: p-0<> C-NE g-cmp0 ; : p-u<  C-CC g-cmp ;  : p-u>  C-HI g-cmp ;

\ more stack ops
: p-rot  T0 g-pop  T1 g-pop  T2 g-pop  T1 g-push  T0 g-push  T2 g-push ;
: p-mrot T0 g-pop  T1 g-pop  T2 g-pop  T0 g-push  T2 g-push  T1 g-push ;
: p-2dup T0 g-pop  T1 g-pop  T1 g-push  T0 g-push  T1 g-push  T0 g-push ;
: p-2drop XDS XDS 16 SUBI, ;
: p-tuck T0 g-pop  T1 g-pop  T0 g-push  T1 g-push  T0 g-push ;
: p-qdup T0 g-pop  T0 g-push  NEWLBL {: l :}  T0 l CBZ,  T0 g-push  l LBL, ;
: p-2swap T0 g-pop T1 g-pop T2 g-pop 12 g-pop  T1 g-push T0 g-push 12 g-push T2 g-push ;

\ more arithmetic
: p-abs  T0 g-pop  T0 0 CMPI,  NEWLBL {: l :}  C-GE l BCOND,  T0 SP T0 SUB,  l LBL,  T0 g-push ;
: p-min  T1 g-pop  T0 g-pop  T0 T1 CMP,  NEWLBL {: l :}  C-LE l BCOND,  T0 T1 0 ADDI,  l LBL,  T0 g-push ;
: p-max  T1 g-pop  T0 g-pop  T0 T1 CMP,  NEWLBL {: l :}  C-GE l BCOND,  T0 T1 0 ADDI,  l LBL,  T0 g-push ;
: p-inv  T0 g-pop  T1 0 MOVN,  T0 T0 T1 EOR,  T0 g-push ;
: p-2*   T0 g-pop  T0 T0 1 LSLI,  T0 g-push ;
: p-2/   T0 g-pop  T0 T0 1 ASRI,  T0 g-push ;
: p-lsh  T1 g-pop  T0 g-pop  T0 T0 T1 LSLV,  T0 g-push ;
: p-rsh  T1 g-pop  T0 g-pop  T0 T0 T1 LSRV,  T0 g-push ;
: p-/mod T1 g-pop  T0 g-pop  T2 T0 T1 SDIV,  12 T2 T1 MUL,  12 T0 12 SUB,  12 g-push  T2 g-push ;

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
\ DO/?DO/LOOP/I keep index+limit on the return stack, so loops nest.
: c-do    T0 g-pop  T1 g-pop  T1 g-rpush  T0 g-rpush     \ push limit, then index
          NEWLBL {: lexit :}  NEWLBL {: ltop :}  ltop LBL,
          lexit cf-push  ltop cf-push ;
: c-qdo   T0 g-pop  T1 g-pop  T1 g-rpush  T0 g-rpush
          NEWLBL {: lexit :}  T0 T1 CMP,
          NEWLBL {: lenter :}  C-LT lenter BCOND,         \ start<limit -> enter
          RSP RSP 16 ADDI,  lexit B,                      \ else drop both, skip
          lenter LBL,  NEWLBL {: ltop :}  ltop LBL,
          lexit cf-push  ltop cf-push ;
: c-loop  cf-pop {: ltop :}  cf-pop {: lexit :}
          T0 RSP 0 LDR,  T0 T0 1 ADDI,  T0 RSP 0 STR,     \ ++index in place
          T1 RSP 8 LDR,  T0 T1 CMP,  C-LT ltop BCOND,     \ index<limit -> loop
          RSP RSP 16 ADDI,  lexit LBL, ;                  \ drop index+limit
: c-i     T0 RSP 0 LDR,  T0 g-push ;
: p->r    T0 g-pop   T0 g-rpush ;
: p-r>    T0 g-rpop  T0 g-push ;
: p-r@    T0 RSP 0 LDR,  T0 g-push ;
: c-exit  EPILOG @ B, ;

\ token -> generator (own wordlist; gforth lookups are case-insensitive)
wordlist constant CG-PRIMS
get-current  CG-PRIMS set-current
: DUP p-dup ;  : DROP p-drop ;  : SWAP p-swap ;  : OVER p-over ;  : NIP p-nip ;
: + p-add ;  : - p-sub ;  : * p-mul ;  : / p-div ;  : MOD p-mod ;
: 1+ p-1+ ;  : 1- p-1- ;  : NEGATE p-neg ;
: AND p-and ;  : OR p-or ;  : XOR p-xor ;
: < p-lt ;  : > p-gt ;  : = p-eq ;  : <= p-le ;  : >= p-ge ;  : <> p-ne ;
: 0= p-0= ;  : 0< p-0< ;  : 0> p-0> ;  : 0<> p-0<> ;  : U< p-u< ;  : U> p-u> ;
: ROT p-rot ;  : -ROT p-mrot ;  : 2DUP p-2dup ;  : 2DROP p-2drop ;
: TUCK p-tuck ;  : ?DUP p-qdup ;  : 2SWAP p-2swap ;
: ABS p-abs ;  : MIN p-min ;  : MAX p-max ;  : INVERT p-inv ;
: 2* p-2* ;  : 2/ p-2/ ;  : LSHIFT p-lsh ;  : RSHIFT p-rsh ;  : /MOD p-/mod ;
: IF c-if ;  : ELSE c-else ;  : THEN c-then ;
: BEGIN c-begin ;  : UNTIL c-until ;  : AGAIN c-again ;  : WHILE c-while ;  : REPEAT c-repeat ;
: DO c-do ;  : ?DO c-qdo ;  : LOOP c-loop ;  : I c-i ;  : EXIT c-exit ;
: >R p->r ;  : R> p-r> ;  : R@ p-r@ ;
set-current
