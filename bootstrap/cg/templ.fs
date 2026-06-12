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
\ HP (x26 — outside the VS pool, so it survives spills and calls). g-heap-init runs
\ once at the program ENTRY (COMPILE-WORD); callees inherit HP.
$100000 constant HEAPSZ

: g-heap-init ( -- )
   0 0 MOVZ,  1 HEAPSZ LIT64,  2 3 MOVZ,  3 $1002 LIT64,  4 0 MOVN,  5 0 MOVZ,
   NR-MMAP SYS,  HP 0 0 ADDI, ;     \ mmap RW; HP = base

\ data-stack ops (Xds points just past TOS; full-ascending)
: g-push ( reg -- )  XDS 0 STR,  XDS XDS 8 ADDI, ;

: g-pop  ( reg -- )  XDS XDS 8 SUBI,  XDS 0 LDR, ;

: g-lit  ( n -- )    T0 swap LIT64,  T0 g-push ;

\ return stack (grows down; RSP points at top; [RSP]=index, [RSP+8]=limit)
: g-rpush ( reg -- )  RSP RSP 8 SUBI,  RSP 0 STR, ;

: g-rpop  ( reg -- )  RSP 0 LDR,  RSP RSP 8 ADDI, ;
\ carve a locals frame (LOCSZ, addressed [sp,#slot*8]) + the data stack (Xds, up)
\ + return stack (RSP=Xds+n, down) on the machine stack. Layout low→high:
\ [sp .. sp+LOCSZ) locals | [Xds .. ) data ↑ | return ↓ from Xds+n.
256 constant LOCSZ                       \ 32 local slots × 8 bytes

: g-prologue {: n -- :}
   SP SP n LOCSZ + SUBI,  XDS SP LOCSZ ADDI,  RSP XDS 0 ADDI,  RSP RSP n ADDI, ;

: g-exit-tos ( -- )  0 g-pop  NR-EXIT SYS, ;     \ exit(TOS)

: g-exit0    ( -- )  0 0 MOVZ,  NR-EXIT SYS, ;   \ exit(0)

\ Spill-path primitives — ONLY the ops not handled by the register-allocated
\ CG-VS (regstack.fs); arith/shuffle/compare/logical/shift moved there. These run
\ after a v-spill, so they use the memory data stack via g-pop/g-push.
\ Native SDIV by 0 silently yields 0; gforth THROWS. Trap on a zero divisor so a
\ miscompile can't pass off wrong data as a result (exact gforth exit code isn't
\ matched — both error, different mechanism). T1 holds the divisor here.
: g-div0? ( -- )  NEWLBL {: lok :}  T1 lok CBNZ,  BRK,  lok LBL, ;

: p-div   T1 g-pop  T0 g-pop  g-div0?  T0 T0 T1 SDIV, T0 g-push ;

: p-mod   T1 g-pop  T0 g-pop  g-div0?  T2 T0 T1 SDIV,  T2 T2 T1 MUL,  T0 T0 T2 SUB,  T0 g-push ;

: p-qdup T0 g-pop  T0 g-push  NEWLBL {: l :}  T0 l CBZ,  T0 g-push  l LBL, ;

: p-abs  T0 g-pop  T0 0 CMPI,  NEWLBL {: l :}  C-GE l BCOND,  T0 SP T0 SUB,  l LBL,  T0 g-push ;

: p-min  T1 g-pop  T0 g-pop  T0 T1 CMP,  NEWLBL {: l :}  C-LE l BCOND,  T0 T1 0 ADDI,  l LBL,  T0 g-push ;

: p-max  T1 g-pop  T0 g-pop  T0 T1 CMP,  NEWLBL {: l :}  C-GE l BCOND,  T0 T1 0 ADDI,  l LBL,  T0 g-push ;

: p-2/   T0 g-pop  T0 T0 1 ASRI,  T0 g-push ;

: p-/mod T1 g-pop  T0 g-pop  g-div0?  T2 T0 T1 SDIV,  12 T2 T1 MUL,  12 T0 12 SUB,  12 g-push  T2 g-push ;

\ control-flow stack (compile-time, holds label ids)
variable CF-SP   create CF-STK 64 cells allot
variable EPILOG  variable LOOP-DEPTH

: cf-reset ( -- )  0 CF-SP !  0 LOOP-DEPTH ! ;

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
\ Loop index/limit live in REGISTERS (LIDX=x27, LLIM=x28 — outside the VS pool, so
\ they survive the body's spills), not on the return stack: the per-iteration
\ increment/compare is register-only (the big loop win). Nesting saves/restores the
\ enclosing loop's pair on the return stack at entry/exit (not per iteration).
27 constant LIDX   28 constant LLIM

: loop-save ( -- )  LOOP-DEPTH @ if  LLIM g-rpush  LIDX g-rpush  then  1 LOOP-DEPTH +! ;

: loop-rest ( -- )  LOOP-DEPTH @ 1 > if  LIDX g-rpop  LLIM g-rpop  then  -1 LOOP-DEPTH +! ;

: c-do    loop-save  LIDX g-pop  LLIM g-pop              \ index->x27, limit->x28
          NEWLBL {: lexit :}  NEWLBL {: ltop :}  ltop LBL,
          lexit cf-push  ltop cf-push ;

: c-qdo   loop-save  LIDX g-pop  LLIM g-pop
          NEWLBL {: lexit :}  LIDX LLIM CMP,
          NEWLBL {: lenter :}  C-LT lenter BCOND,         \ index<limit -> enter
          lexit B,                                        \ else skip (lexit does the restore)
          lenter LBL,  NEWLBL {: ltop :}  ltop LBL,
          lexit cf-push  ltop cf-push ;

: c-loop  cf-pop {: ltop :}  cf-pop {: lexit :}
          LIDX LIDX 1 ADDI,  LIDX LLIM CMP,  C-LT ltop BCOND,   \ ++index; index<limit -> loop
          lexit LBL,  loop-rest ;

: c-i     LIDX g-push ;

: p->r    T0 g-pop   T0 g-rpush ;

: p-r>    T0 g-rpop  T0 g-push ;

: p-r@    T0 RSP 0 LDR,  T0 g-push ;

: c-exit  EPILOG @ B, ;

\ token -> generator (own wordlist; gforth lookups are case-insensitive)
wordlist constant CG-PRIMS
get-current  CG-PRIMS set-current

\ Only ops NOT in the register-allocated CG-VS (regstack.fs) reach here — walk.fs
\ routes the rest to CG-VS first. So this list is the spill-path remainder:
\ division, ?DUP, ABS/MIN/MAX, 2/, and control flow / return stack below.
: / p-div ;

: MOD p-mod ;

: /MOD p-/mod ;

: 2/ p-2/ ;

: ?DUP p-qdup ;

: ABS p-abs ;

: MIN p-min ;

: MAX p-max ;

: IF c-if ;

: ELSE c-else ;

: THEN c-then ;

: BEGIN c-begin ;

: UNTIL c-until ;

: AGAIN c-again ;

: WHILE c-while ;

: REPEAT c-repeat ;

: DO c-do ;

: ?DO c-qdo ;

: LOOP c-loop ;

: I c-i ;

: EXIT c-exit ;

: >R p->r ;

: R> p-r> ;

: R@ p-r@ ;
set-current
