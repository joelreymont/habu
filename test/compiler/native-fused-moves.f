\ native-fused-moves.f - tier 1 carries a data-stack pointer move in the
\ transfer beside it, and the pair it replaced is gone from the body.
\
\ A publish used to be `str Xt,[x19]` followed by `add x19,x19,#8` and a take
\ `sub x19,x19,#8` followed by `ldr Xt,[x19]`. AArch64 writes the base register
\ back as part of a load or a store, so each pair is one instruction:
\ `str Xt,[x19],#8` and `ldr Xt,[x19,#-8]!`. src/compiler/native/select.f emits
\ the fused form where the transfer stands AT the pointer, which is the only
\ cell those two addressing modes reach.
\
\ WHY THE BODIES ARE READ AND NOT DERIVED. Which routine fuses depends on where
\ the placement stands the pointer, and that is a survey over the whole routine.
\ So every count below is decoded out of the word's own baked span through
\ src/habu/xref.f - the same record the compiler resolves a callable against -
\ and every word is also RUN behind a sentinel cell, because a writeback of the
\ wrong amount or in the wrong direction leaves the pointer somewhere else and
\ no instruction census would say so.
\
\ THE NEGATIVE CASE IS THE POINT. `FM-EXCH` takes two cells and leaves two, so
\ the place the caller leaves the pointer and the place it expects it back are
\ one place: the routine stands there, both moves are nothing, and there is
\ nothing to carry. It must contain no fused transfer AND no pointer move at
\ all - a selector that fused a move of zero would write `str Xt,[x19],#0`,
\ which is one instruction where none is needed.
\
\ BOTH REGISTER FILES ARE HERE because both have the two indexed forms, and a
\ double that reaches the caller's stack takes them: `str d0,[x19],#8` and
\ `ldr d0,[x19,#-8]!` differ from their general twins in one opcode bit.

require lib/test.f
require lib/float.f
require src/habu/xref.f

1 set-tier

package NATIVE-FUSED-MOVES
public

\ ---- the words under test ----------------------------------------------------
\ PUBLIC and read back by their QUALIFIED names: XREF-FIND answers an unqualified
\ name out of the global wordlist, which a package's words are not in.

: FM-PUSH1 ( -- n ) 5 ;                  \ publishes one cell and takes none
: FM-EXCH ( n n -- n n ) swap ;          \ takes two and leaves two: no move at all
: FM-GUARD ( n -- ) 0 < if 1 throw then ;   \ takes one, publishes none, calls
: FM-CALLEE ( n -- n ) 1 + ;
: FM-CALL ( n -- n n ) FM-CALLEE dup ;   \ publishes past a call it came back from
: FM-FPUSH ( -- r ) -1 s>f ;             \ the same publish in the other file
: FM-FPOP ( r -- ) 0 s>f f< 0= if 1 throw then ;
: FM-SRC ( -- n ) 7 ;
: FM-PRUNED ( -- n ) FM-SRC 0 > if 5 else 5 then ;

private

\ ---- reading a word's own emitted code ---------------------------------------
\ XREF-N>U8 is src/habu/xref.f's own boundary between an engine address and a
\ readable pointer; this file adds none of its own.
: CODE@ ( n -- n ) {: at:n :}
   at XREF-N>U8 c@
   at 1+ XREF-N>U8 c@ 8 lshift or
   at 2 + XREF-N>U8 c@ 16 lshift or
   at 3 + XREF-N>U8 c@ 24 lshift or ;

\ ---- the six shapes this file counts -----------------------------------------
\ Masks over the fields DDI 0487 gives each form, so a register this file does
\ not name cannot make a word match.
19 constant DS-REG                        \ the data-stack pointer

$FFE00C00 constant IDX-MASK               \ size, file, opcode and the index mode
$F8000400 constant STR-POST                \ str Xt,[Xn],#imm9
$F8400C00 constant LDR-PRE                 \ ldr Xt,[Xn,#imm9]!
$FC000400 constant STRD-POST               \ str Dt,[Xn],#imm9
$FC400C00 constant LDRD-PRE                \ ldr Dt,[Xn,#imm9]!
$FFC00000 constant AS-MASK                 \ the add/sub immediate opcode
$91000000 constant ADD-IMM
$D1000000 constant SUB-IMM
$3E0 constant RN-FIELD                     \ bits 9:5

: RN ( n -- n ) RN-FIELD and 5 rshift ;

: DS-INDEXED? ( n n -- bool ) {: w:n want:n :}
   w IDX-MASK and want <> if false exit then
   w RN DS-REG = ;

\ Both registers are the pointer, which is what makes it a MOVE of the pointer
\ rather than an address computed out of it.
: DS-MOVE? ( n n -- bool ) {: w:n want:n :}
   w AS-MASK and want <> if false exit then
   w RN DS-REG <> if false exit then
   w $1F and DS-REG = ;

0 constant K-PUSH                         \ str Xt,[x19],#imm
1 constant K-POP                          \ ldr Xt,[x19,#imm]!
2 constant K-FPUSH                        \ str Dt,[x19],#imm
3 constant K-FPOP                         \ ldr Dt,[x19,#imm]!
4 constant K-ADD                          \ add x19,x19,#imm
5 constant K-SUB                          \ sub x19,x19,#imm

: MATCHES? ( n n -- bool ) {: w:n kind:n :}
   kind K-PUSH = if w STR-POST DS-INDEXED? exit then
   kind K-POP = if w LDR-PRE DS-INDEXED? exit then
   kind K-FPUSH = if w STRD-POST DS-INDEXED? exit then
   kind K-FPOP = if w LDRD-PRE DS-INDEXED? exit then
   kind K-ADD = if w ADD-IMM DS-MOVE? exit then
   w SUB-IMM DS-MOVE? ;

variable MISSING

: SPAN ( ptr u8 n -- n n ) {: na:ptr nu:n :}
   na nu XREF-FIND {: rec:ptr :}
   rec XREF-FOUND? 0= if
      1 MISSING +!
      s" native-fused-moves: no record for " type na nu type cr
      0 0 exit
   then
   rec XREF-START dup rec XREF-CODE-BYTES + ;

: COUNT-FORM ( ptr u8 n n -- n ) {: na:ptr nu:n kind:n :}
   na nu SPAN {: lo:n hi:n :}
   0
   hi lo - 4 / 0 ?do
      lo i 4 * + CODE@ kind MATCHES? if 1+ then
   loop ;

\ The writeback amount, read off the first transfer of that shape, so a move of
\ the wrong size is a different number here and not a different count.
: WB-OF ( ptr u8 n n -- n ) {: na:ptr nu:n kind:n :}
   na nu SPAN {: lo:n hi:n :}
   0
   hi lo - 4 / 0 ?do
      lo i 4 * + CODE@ {: w:n :}
      w kind MATCHES? if
         drop
         w 12 rshift $1FF and {: imm:n :}
         imm $100 >= if imm $200 - else imm then
         leave
      then
   loop ;

: NO-MOVES ( ptr u8 n -- ) {: na:ptr nu:n :}
   na nu K-ADD COUNT-FORM 0 T=
   na nu K-SUB COUNT-FORM 0 T= ;

\ ---- the cases ---------------------------------------------------------------
: PUSH-CASE ( -- )
   s" a routine that only publishes carries the publish in its store" T-LABEL
   s" NATIVE-FUSED-MOVES:FM-PUSH1" K-PUSH COUNT-FORM 1 T=
   s" NATIVE-FUSED-MOVES:FM-PUSH1" K-PUSH WB-OF 8 T=
   s" NATIVE-FUSED-MOVES:FM-PUSH1" K-POP COUNT-FORM 0 T=
   s" NATIVE-FUSED-MOVES:FM-PUSH1" NO-MOVES
   77 FM-PUSH1 5 T= 77 T= ;

\ The take rides the load of the first argument the body needs, and the argument
\ the diagnostic call publishes rides the store in front of that call - two of
\ the three shapes in one body. The third, the three-cell publish the diagnostic
\ context makes, is a run whose last store does NOT stand at the pointer, so it
\ keeps its `add`: that one instruction is what says the rule is about the cell
\ and not about the run.
: GUARD-CASE ( -- )
   s" a take and a call's publish each ride the transfer at the pointer" T-LABEL
   s" NATIVE-FUSED-MOVES:FM-GUARD" K-POP COUNT-FORM 1 T=
   s" NATIVE-FUSED-MOVES:FM-GUARD" K-POP WB-OF -8 T=
   s" NATIVE-FUSED-MOVES:FM-GUARD" K-PUSH COUNT-FORM 1 T=
   s" NATIVE-FUSED-MOVES:FM-GUARD" K-PUSH WB-OF 8 T=
   s" NATIVE-FUSED-MOVES:FM-GUARD" K-ADD COUNT-FORM 1 T=
   s" NATIVE-FUSED-MOVES:FM-GUARD" K-SUB COUNT-FORM 0 T=
   88 1 FM-GUARD 88 T= ;

: EXCH-CASE ( -- )
   s" a routine whose pointer never moves fuses nothing" T-LABEL
   s" NATIVE-FUSED-MOVES:FM-EXCH" K-PUSH COUNT-FORM 0 T=
   s" NATIVE-FUSED-MOVES:FM-EXCH" K-POP COUNT-FORM 0 T=
   s" NATIVE-FUSED-MOVES:FM-EXCH" NO-MOVES
   3 4 FM-EXCH 3 T= 4 T= ;

: CALL-CASE ( -- )
   s" a routine publishes through its own store after a call returns" T-LABEL
   s" NATIVE-FUSED-MOVES:FM-CALL" K-PUSH COUNT-FORM 1 T=
   s" NATIVE-FUSED-MOVES:FM-CALL" K-PUSH WB-OF 8 T=
   s" NATIVE-FUSED-MOVES:FM-CALL" NO-MOVES
   99 7 FM-CALL 8 T= 8 T= 99 T= ;

\ THE PRUNER'S CASE, and the one that says a fused transfer is not a load with
\ an extra field. Both arms of `FM-PRUNED` hand the join the same value, so the
\ if-conversion writes no comparison at all and the cell the call returned is
\ read by nothing - which is the one operation src/compiler/native/prune.f
\ removes. It may not remove the pointer move with it, so the CALL takes the
\ move back and the body is the one the unfused compiler wrote minus the
\ publish it still fuses. Built the other way round this word does not compile:
\ the allocation validator refuses a data-stack access nothing reads
\ (E-A64RAV-DKEEP).
: PRUNE-CASE ( -- )
   s" a fused transfer nothing reads hands its move back and goes" T-LABEL
   s" NATIVE-FUSED-MOVES:FM-PRUNED" K-POP COUNT-FORM 0 T=
   s" NATIVE-FUSED-MOVES:FM-PRUNED" K-SUB COUNT-FORM 1 T=
   s" NATIVE-FUSED-MOVES:FM-PRUNED" K-ADD COUNT-FORM 0 T=
   s" NATIVE-FUSED-MOVES:FM-PRUNED" K-PUSH COUNT-FORM 1 T=
   55 FM-PRUNED 5 T= 55 T= ;

\ The same two moves in the other register file.
: FLOAT-CASE ( -- )
   s" a double reaching the caller's stack takes the indexed forms too" T-LABEL
   s" NATIVE-FUSED-MOVES:FM-FPUSH" K-FPUSH COUNT-FORM 1 T=
   s" NATIVE-FUSED-MOVES:FM-FPUSH" K-FPUSH WB-OF 8 T=
   s" NATIVE-FUSED-MOVES:FM-FPUSH" K-PUSH COUNT-FORM 0 T=
   s" NATIVE-FUSED-MOVES:FM-FPUSH" NO-MOVES
   s" NATIVE-FUSED-MOVES:FM-FPOP" K-FPOP COUNT-FORM 1 T=
   s" NATIVE-FUSED-MOVES:FM-FPOP" K-FPOP WB-OF -8 T=
   s" NATIVE-FUSED-MOVES:FM-FPOP" K-POP COUNT-FORM 0 T=
   s" NATIVE-FUSED-MOVES:FM-FPOP" K-SUB COUNT-FORM 0 T=
   66 FM-FPUSH FM-FPOP 66 T= ;

public

: RUN ( -- )
   T-RESET
   0 MISSING !
   PUSH-CASE
   GUARD-CASE
   EXCH-CASE
   CALL-CASE
   PRUNE-CASE
   FLOAT-CASE
   s" every word under test has a dictionary record" T-LABEL
   MISSING @ 0 T=
   T-REPORT ;

;package

NATIVE-FUSED-MOVES:RUN
