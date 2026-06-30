\ templ.fs — ICode generators for habu primitives and control structures. habu's
\ data stack lives in memory at Xds (x19); each word here appends ICode that
\ operates on it. The CG-PRIMS wordlist maps a body token to its generator;
\ walk.fs drives it. One concern: source-token -> ICode. (Generators only —
\ tokenizing/compilation is walk.fs.)

require asm.fs
require sys.fs

 9 constant T0   10 constant T1   11 constant T2
19 constant XDS  31 constant SP   25 constant RSP   26 constant HP

\ Bump heap (HERE/ALLOT/,/C,): an mmap'd RW arena whose next-free pointer lives in
\ HP (x26 — outside the VS pool, so it survives spills and calls). G-HEAP-INIT runs
\ once at the program ENTRY (COMPILE-WORD); callees inherit HP.
$100000 constant HEAPSZ

: G-HEAP-INIT ( -- )
   0 0 MOVZ,  1 HEAPSZ LIT64,  2 3 MOVZ,  3 MAP-ANON-PRIVATE LIT64,  4 0 MOVN,  5 0 MOVZ,
   NR-MMAP SYS,  HP 0 0 ADDI, ;     \ mmap RW; HP = base

\ data-stack ops (Xds points just past TOS; full-ascending)
: G-PUSH ( reg -- )  XDS 0 STR,  XDS XDS 8 ADDI, ;

: G-POP  ( reg -- )  XDS XDS 8 SUBI,  XDS 0 LDR, ;

: G-LIT  ( n -- )    T0 swap LIT64,  T0 G-PUSH ;

\ return stack (grows down; RSP points at top; [RSP]=index, [RSP+8]=limit)
: G-RPUSH ( reg -- )  RSP RSP 8 SUBI,  RSP 0 STR, ;

: G-RPOP  ( reg -- )  RSP 0 LDR,  RSP RSP 8 ADDI, ;
\ carve a locals frame (LOCSZ, addressed [sp,#slot*8]) + the data stack (Xds, up)
\ + return stack (RSP=Xds+n, down) on the machine stack. Layout low→high:
\ [sp .. sp+LOCSZ) locals | [Xds .. ) data ↑ | return ↓ from Xds+n.
256 constant LOCSZ                       \ 32 local slots × 8 bytes

: G-PROLOGUE {: n -- :}
   SP SP n LOCSZ + SUBI,  XDS SP LOCSZ ADDI,  RSP XDS 0 ADDI,  RSP RSP n ADDI, ;

: G-EXIT-TOS ( -- )  0 G-POP  NR-EXIT-GROUP SYS, ;     \ exit(TOS)

: G-EXIT0    ( -- )  0 0 MOVZ,  NR-EXIT-GROUP SYS, ;   \ exit(0)

\ Spill-path primitives — ONLY the ops not handled by the register-allocated
\ CG-VS (regstack.fs); arith/shuffle/compare/logical/shift moved there. These run
\ after a v-spill, so they use the memory data stack via g-pop/g-push.
\ Native SDIV by 0 silently yields 0; gforth THROWS. Trap on a zero divisor so a
\ miscompile can't pass off wrong data as a result (exact gforth exit code isn't
\ matched — both error, different mechanism). T1 holds the divisor here.
: G-DIV0? ( -- )  LBL {: lok :}  T1 lok CBNZ,  BRK,  lok LBL, ;

: P-DIV   T1 G-POP  T0 G-POP  G-DIV0?  T0 T0 T1 SDIV, T0 G-PUSH ;

: P-MOD   T1 G-POP  T0 G-POP  G-DIV0?  T2 T0 T1 SDIV,  T2 T2 T1 MUL,  T0 T0 T2 SUB,  T0 G-PUSH ;

: P-QDUP T0 G-POP  T0 G-PUSH  LBL {: l :}  T0 l CBZ,  T0 G-PUSH  l LBL, ;

: P-ABS  T0 G-POP  T0 0 CMPI,  LBL {: l :}  C-GE l BCOND,  T0 SP T0 SUB,  l LBL,  T0 G-PUSH ;

: P-MIN  T1 G-POP  T0 G-POP  T0 T1 CMP,  LBL {: l :}  C-LE l BCOND,  T0 T1 0 ADDI,  l LBL,  T0 G-PUSH ;

: P-MAX  T1 G-POP  T0 G-POP  T0 T1 CMP,  LBL {: l :}  C-GE l BCOND,  T0 T1 0 ADDI,  l LBL,  T0 G-PUSH ;

: P-2/   T0 G-POP  T0 T0 1 ASRI,  T0 G-PUSH ;

: P-/MOD T1 G-POP  T0 G-POP  G-DIV0?  T2 T0 T1 SDIV,  12 T2 T1 MUL,  12 T0 12 SUB,  12 G-PUSH  T2 G-PUSH ;

\ control-flow stack (compile-time, holds label ids)
64 constant CF-MAX
variable CF-SP   create CF-STK CF-MAX cells allot
variable EPILOG  variable LOOP-DEPTH

: CF-RESET ( -- )  0 CF-SP !  0 LOOP-DEPTH ! ;

: CF-PUSH? ( -- )
   CF-SP @ CF-MAX >= if 1 abort" cg: control-flow stack overflow" then ;

: CF-POP? ( -- )
   CF-SP @ 0 <= if 1 abort" cg: control-flow stack underflow" then ;

: CF-PUSH ( x -- )  CF-PUSH?  CF-STK CF-SP @ cells + !  1 CF-SP +! ;

: CF-POP  ( -- x )  CF-POP?  -1 CF-SP +!  CF-STK CF-SP @ cells + @ ;

: C-IF    T0 G-POP  LBL dup T0 swap CBZ,  CF-PUSH ;

: C-ELSE  LBL dup B,  CF-POP LBL,  CF-PUSH ;

: C-THEN  CF-POP LBL, ;

: C-BEGIN LBL dup LBL,  CF-PUSH ;

: C-UNTIL T0 G-POP  CF-POP T0 swap CBZ, ;

: C-AGAIN CF-POP B, ;

: C-WHILE T0 G-POP  LBL dup T0 swap CBZ,  CF-PUSH ;

: C-REPEAT CF-POP  CF-POP B,  LBL, ;            \ ( LEXIT Lbegin -- ) B Lbegin; place LEXIT
\ DO/?DO/LOOP/I keep index+limit on the return stack, so loops nest.
\ Loop index/limit live in REGISTERS (LIDX=x27, LLIM=x28 — outside the VS pool, so
\ they survive the body's spills), not on the return stack: the per-iteration
\ increment/compare is register-only (the big loop win). Nesting saves/restores the
\ enclosing loop's pair on the return stack at entry/exit (not per iteration).
27 constant LIDX   28 constant LLIM

: LOOP-SAVE ( -- )  LOOP-DEPTH @ if  LLIM G-RPUSH  LIDX G-RPUSH  then  1 LOOP-DEPTH +! ;

: LOOP-REST ( -- )  LOOP-DEPTH @ 1 > if  LIDX G-RPOP  LLIM G-RPOP  then  -1 LOOP-DEPTH +! ;

: C-DO    LOOP-SAVE  LIDX G-POP  LLIM G-POP              \ index->x27, limit->x28
          LBL {: LEXIT :}  LBL {: ltop :}  ltop LBL,
          LEXIT CF-PUSH  ltop CF-PUSH ;

: C-QDO   LOOP-SAVE  LIDX G-POP  LLIM G-POP
          LBL {: LEXIT :}  LIDX LLIM CMP,
          LBL {: lenter :}  C-LT lenter BCOND,         \ index<limit -> enter
          LEXIT B,                                        \ else skip (lexit does the restore)
          lenter LBL,  LBL {: ltop :}  ltop LBL,
          LEXIT CF-PUSH  ltop CF-PUSH ;

: C-LOOP  CF-POP {: ltop :}  CF-POP {: LEXIT :}
          LIDX LIDX 1 ADDI,  LIDX LLIM CMP,  C-LT ltop BCOND,   \ ++index; index<limit -> loop
          LEXIT LBL,  LOOP-REST ;

: C-I     LIDX G-PUSH ;

: P->R    T0 G-POP   T0 G-RPUSH ;

: P-R>    T0 G-RPOP  T0 G-PUSH ;

: P-R@    T0 RSP 0 LDR,  T0 G-PUSH ;

: C-EXIT  EPILOG @ B, ;

\ token -> generator (own wordlist; gforth lookups are case-insensitive)
wordlist constant CG-PRIMS
get-current  CG-PRIMS set-current

\ Only ops NOT in the register-allocated CG-VS (regstack.fs) reach here — walk.fs
\ routes the rest to CG-VS first. So this list is the spill-path remainder:
\ division, ?DUP, ABS/MIN/MAX, 2/, and control flow / return stack below.
: / P-DIV ;

: MOD P-MOD ;

: /MOD P-/MOD ;

: 2/ P-2/ ;

: ?DUP P-QDUP ;

: ABS P-ABS ;

: MIN P-MIN ;

: MAX P-MAX ;

: IF C-IF ;

: ELSE C-ELSE ;

: THEN C-THEN ;

: BEGIN C-BEGIN ;

: UNTIL C-UNTIL ;

: AGAIN C-AGAIN ;

: WHILE C-WHILE ;

: REPEAT C-REPEAT ;

: DO C-DO ;

: ?DO C-QDO ;

: LOOP C-LOOP ;

: I C-I ;

: EXIT C-EXIT ;

: >R P->R ;

: R> P-R> ;

: R@ P-R@ ;
set-current
