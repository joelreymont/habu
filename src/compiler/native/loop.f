\ loop.f - the module in which a counted loop that only adds is the arithmetic
\ that loop would have computed. One concern: recognising those loops and
\ writing the module that holds the closed form instead.
\
\ WHY THE PASS EXISTS. tools/codegen-compare.f measures this chain against a
\ clang -O2 twin of every corpus row, and the mechanism attribution of the
\ remaining gap named ONE optimisation class this chain did not have at all:
\ clang computes the ANSWER of a counted loop instead of running it. SUM-TO,
\ MANY-LOCALS and TINY-CALLEE are the three rows that gap belongs to - 120 of
\ the 164 gap bytes and 15.95 of the 21.6 gap nanoseconds when this file was
\ written - and all three are the same shape: a `?do` loop whose whole body adds
\ things to one accumulator. What such a loop leaves is a sum with a closed form,
\ and this file is the pass that writes it.
\
\ WHY IT IS A SOURCE-DIALECT PASS AND NOT A MACHINE ONE. The facts the recogniser
\ needs are the loop's own SSA values - the guard's subtraction, the index and
\ the limit the header takes as arguments, the additions the body makes - and
\ src/compiler/native/elaborate.f puts every one of them in the HIR module. So
\ the earliest place that can see the shape is also the place with the smallest
\ vocabulary to answer for: forty-five opcodes here against seventy-three in the
\ machine dialect. Running here also means the closed form is an ORDINARY HIR
\ program: src/compiler/native/select.f chooses its instructions, fuses its
\ comparison into the branch that reads it and if-converts what it can, and
\ src/compiler/native/combine.f folds its multiply and the addition above it into
\ one multiply-add. None of that had to be written twice.
\
\ AND BECAUSE THE RESULT IS VALIDATED RATHER THAN TRUSTED. What this pass writes
\ goes through the ordinary freeze verifier, the ordinary selector, the ordinary
\ register allocator and src/compiler/native/regalloc-verify.f, none of which
\ knows this pass exists. A closed form that got a register or an edge wrong is
\ caught by something that does not share its reasoning.
\
\ ---- WHAT IS RECOGNISED, IN ONE SENTENCE -------------------------------------
\ A `?do` loop of exactly three blocks - a header, an exit stub and a latch -
\ whose header holds nothing but additions into ONE of its arguments, where each
\ addition's other operand is either the loop index or a value that cannot change
\ with the turn.
\
\ Everything else is declined, and declining is not an error: the scan answers
\ zero, the caller keeps the module it has, and the routine is compiled exactly
\ as it was before this file existed.
\
\ ---- THE SHAPE, BLOCK BY BLOCK -----------------------------------------------
\ This is what src/compiler/native/elaborate.f builds for `?do … loop`, and
\ test/compiler/native-elaborate.f's SUMTO-CASE pins it independently:
\
\   g    the guard. `d = sub(limit, start)` and `brz d -> (sk, pr)`.
\   sk   the skip stub, reached when d is zero. Untouched by this pass.
\   pr   the pre-header. `br pr -> h` handing the header its arguments.
\   h    the header. Takes the live vector, then the index and the limit, then
\        the locals that cross and the memory order when the definition has
\        either. Holds the body, then `one = const 1`, `nx = add(idx, one)`,
\        `f = lt(nx, lim)` and `brz f -> (xt, la)`.
\   xt   the exit stub. `br xt -> jn` handing on the vector.
\   la   the latch. `br la -> h` handing back the vector, `nx` and the limit.
\   jn   the join both exits meet in.
\
\ THE TWO COUNTERS ARE FOUND BY THEIR USE AND NOT BY WHERE THEY SIT. They are the
\ last two arguments only when the body binds no crossing local and touches no
\ memory at all; a definition that loads anything gives every block it opens a
\ memory-order argument after them, and a call gives it one per crossing local
\ (src/compiler/native/elaborate.f OPEN-ARGS-H). So the index is read off the
\ addition that counts and the limit off the comparison that stops, and PLAN-
\ COUNTERS? below records where the pair really is. Everything else the header
\ carries is a CARRIED position, numbered the way the exit stub hands them on,
\ and CARRY-ARG is the one place that turns such a number into an argument.
\
\ THE GUARD IS READ, NOT ASSUMED, and it is the whole reason the trip count can
\ be written down. `brz` takes its FIRST successor when the tested value is zero,
\ so control reaches `pr` exactly when `limit - start` is not zero, which is
\ exactly when `limit` and `start` differ. Without that fact the closed form
\ would be wrong for the empty loop, so this pass refuses any pre-header whose
\ one predecessor is not a guard of that exact shape.
\
\ ---- THE TRIP COUNT, WHICH IS NOT `limit - start` ----------------------------
\ The engine's `?do` skips the loop only when the two are EQUAL. Any other
\ ordering runs the body at least once, and then the test at `loop` is
\ `index + 1 < limit`, signed. So for a start that is not the largest
\ representable integer:
\
\   start = limit   0 turns   - the guard took the other edge and this pass never
\                               reaches the closed form on that path
\   start < limit   limit - start turns
\   start > limit   ONE turn  - the first `index + 1` is already past the limit
\
\ Measured, not derived from the standard: `: T ( n n -- n ) {: seed len :}
\ seed len 0 ?do 1 + 1 + 1 + 1 + loop ;` answers seed+4 at len 1, seed+12 at
\ len 3, and seed+4 at len -1 and len -5.
\
\ START = THE LARGEST INTEGER IS THE CASE THIS TABLE HAS NO ROW FOR, and it is
\ refused rather than guessed at. There `index + 1` wraps to the smallest integer,
\ which IS below the limit, so the loop runs round through nearly the whole
\ integer range and the count is neither `limit - start` nor one. The refusal is
\ structural: the start must be a compile-time constant, and that constant must
\ not be the maximum.
\
\ ---- THE CLOSED FORM ---------------------------------------------------------
\ With `T` turns, an accumulator `acc0` on entry, `K` added every turn and the
\ index added `m` times a turn, the loop leaves
\
\   acc0 + K*T + m*(start*T + T*(T-1)/2)
\
\ because the indices visited are start, start+1, … start+T-1. Every term is
\ computed in wrapping 64-bit arithmetic, which is what the loop itself does, so
\ the answer is the loop's answer bit for bit and not merely mathematically.
\
\ THE HALF IS EXACT WITHOUT A 128-BIT MULTIPLY, and that is what keeps this pass
\ inside the machine forms the chain already has. `T*(T-1)` overflows sixty-four
\ bits long before `T*(T-1)/2` does, so dividing the low half is wrong. But one
\ of `T` and `T-1` is even, so the halving can be done BEFORE the multiply:
\
\   T*(T-1)/2  =  (T >> 1) * (T - 1 + (T & 1))
\
\ For even T the right factor is T-1 and the left is T/2; for odd T the right is
\ T and the left is (T-1)/2. Both are the exact integer product, so both agree
\ with the true value modulo 2^64. Checked at 0, 1, 2, 3, 4 and the maximum.
\
\ THE TWO TRIP COUNTS ARE TWO ARMS AND NOT A SELECT, because the source dialect
\ has no select and the arms want different work. The pre-header ends on
\ `lt(start, limit)`: the taken arm computes the whole closed form with T = d,
\ and the other computes `acc0 + K + m*start`, which is the same formula at T = 1
\ with every product gone. The comparison and the branch are the pair
\ src/compiler/native/select.f already fuses into one compare-and-branch.
\
\ ---- WHY DELETING THE THREE BLOCKS PRESERVES EVERY OBSERVABLE ----------------
\ The header, the exit stub and the latch are removed from the module the pass
\ writes. That is sound because of four facts the recogniser establishes before
\ anything is written, and it would be unsound without any one of them:
\
\   NOTHING ELSE REACHES THEM. The header's predecessors are the pre-header and
\   the latch; the exit stub's and the latch's is the header. So the three blocks
\   are entered only from the loop, and after the pre-header stops branching into
\   the header no edge into any of them survives.
\
\   NOTHING ELSE READS THEIR VALUES. Every value they define is read inside them:
\   the header's arguments by the body, the increment and the comparison; the
\   accumulator's new value by the latch and the exit stub. Those are checked one
\   by one, so a value that escaped the loop would be a refusal and not a dangling
\   reference. The values that LEAVE the loop are the ones the exit stub hands the
\   join, and the arms hand the join the same list.
\
\   THEY HAVE NO EFFECT TO PRESERVE. The header holds only additions, one
\   constant and its terminator - the coverage check below accounts for every
\   operation in it - so there is no store, no load, no call and no trap in the
\   loop. Deleting it removes no event. This is also why a body that touches
\   memory is declined rather than folded: the closed form of its ARITHMETIC
\   would be right and its memory would be gone.
\
\   AND THEY TERMINATE. A counted loop with the shape above runs T turns and
\   stops; the closed form is what it leaves. The one case where that is not
\   obvious is the enormous trip count, and it is still exactly right: a loop the
\   machine would take an hour to run answers in six instructions, which changes
\   how long the program takes and not what it computes.
\
\ ---- WHAT THIS PASS DOES NOT DECIDE ------------------------------------------
\ Which registers anything lands in, whether the comparison fuses into the
\ branch, whether the multiply and the addition above it become one multiply-add,
\ and whether the two arms are worth if-converting. All of those are decided
\ downstream by passes that read the module this one writes.
\
\ ONE LOOP PER FUNCTION, and it is the first one recognised. A body with two
\ foldable loops has its second one compiled as it always was; nothing about it
\ is wrong, it is simply not folded. Raising that is a capacity change here and
\ not a soundness one.

require lib/prelude.f
require lib/errors.f
require src/compiler/digest.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/symbol.f
require src/compiler/ir/type.f
require src/compiler/ir/source.f
require src/compiler/ir/schema.f
require src/compiler/ir/fun.f
require src/compiler/ir/build.f
require src/compiler/native/hir.f
require src/compiler/native/frozen.f

package NLOOP
using NFROZEN
private

\ ---- the bound dialect -------------------------------------------------------
\ One slot per member of the source operation family, so the family stays
\ exhaustive: a member added to HIR:opcode makes this fail to compile until it
\ has a slot and a rule for rebuilding it here too.
46 constant OPCODES-N

0 constant O-CONST
1 constant O-ADD
2 constant O-SUB
3 constant O-MUL
4 constant O-DIV
5 constant O-LT
6 constant O-LE
7 constant O-GT
8 constant O-GE
9 constant O-EQUAL
10 constant O-NE
11 constant O-AND
12 constant O-OR
13 constant O-XOR
14 constant O-LSHIFT
15 constant O-RSHIFT
16 constant O-INVERT
17 constant O-MEM
18 constant O-LOAD
19 constant O-STORE
20 constant O-BLOAD
21 constant O-BSTORE
22 constant O-BR
23 constant O-BRZ
24 constant O-CALL
25 constant O-WORDCALL
26 constant O-RETURN
27 constant O-TRAP
28 constant O-FCONST
29 constant O-FADD
30 constant O-FSUB
31 constant O-FMUL
32 constant O-FDIV
33 constant O-FNEG
34 constant O-FABS
35 constant O-FSQRT
36 constant O-FLT
37 constant O-FGT
38 constant O-FEQ
39 constant O-FLTZ
40 constant O-FEQZ
41 constant O-INTREAL
42 constant O-REALINT
43 constant O-BITSREAL
44 constant O-REALBITS
45 constant O-QUOT

\ One slot per attribute key the dialect declares. This pass writes one of its
\ own - the literal a constant carries - and COPIES every one the elaborator
\ built, and a field copied under the wrong key would be a call reaching the
\ wrong routine.
5 constant KEYS-N
0 constant K-VALUE
1 constant K-ENTRY
2 constant K-IN
3 constant K-OUT
4 constant K-ADDR

0 constant BOUND-NO
1 constant BOUND-YES

\ The longest function name this pass can carry across. A name is copied out of
\ the old module's interner and interned into the new one, because the two
\ modules number their symbols separately.
128 constant NAME-CAP

\ Values in one function and blocks in one function: the ceilings the
\ neighbouring passes keep, for the same reason. Addends is this pass's own -
\ how many separate loop-invariant values one turn may add up - and thirty-two
\ is far past the eight the widest corpus row holds.
NFROZEN:VMAX constant VMAX
NFROZEN:BMAX constant BMAX
32 constant INV-MAX

\ Operations in the one block this pass reads operation by operation. It is the
\ header's own size, so it is bounded by what fits in a block rather than by how
\ many blocks a routine has, and it is the values-per-block ceiling for that
\ reason.
NFROZEN:VMAX constant COV-MAX

here CELL 1- and CELL swap - CELL 1- and allot
variable BND-MODE
BOUND-NO BND-MODE !
variable N-FOLDED                    \ loops this rewrite really closed, counted as it goes

1 TYPED-BUFFER BND-MOD IR-ID:ir-module-id
OPCODES-N TYPED-BUFFER BND-OP IR-ID:ir-symbol-id
KEYS-N TYPED-BUFFER BND-KEY IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-CELL IR-ID:ir-type-id
1 TYPED-BUFFER BND-MEM IR-ID:ir-type-id
1 TYPED-BUFFER BND-REAL IR-ID:ir-type-id

1 TYPED-BUFFER S-CTX IR-CTX:ctx
1 TYPED-BUFFER S-BLD IR-BUILD:builder
1 TYPED-BUFFER S-SID IR-ID:ir-source-id
VMAX TYPED-BUFFER VMAP IR-ID:ir-value-id
create VSET VMAX cells allot
create NAMEBUF NAME-CAP allot

\ ---- the plan one recognised loop is ----------------------------------------
\ Every row is written by the scan and read by the rewrite, and the scan is the
\ only thing that decides anything: by the time a single operation is copied the
\ questions are all answered.
variable P-OK                        \ whether a loop was recognised at all
variable P-G                         \ the guard block
variable P-PR                        \ the pre-header
variable P-H                         \ the header
variable P-XT                        \ the exit stub
variable P-LA                        \ the latch
variable P-JN                        \ the join both exits meet in
variable P-A                         \ how many live values the header carries besides the counters
variable P-IDX                       \ which argument of the header the index is; the limit is the next
variable P-K                         \ which of them is the accumulator
variable P-M                         \ how many times a turn the index is added
variable P-START                     \ the start, as the number its constant carries
variable P-KCONST                    \ the constant part of what one turn adds
variable P-INV-N                     \ how many non-constant values one turn adds
variable P-ONE                       \ the arm that runs the T=1 form, as a new block ordinal
variable P-MANY                      \ the arm that runs the general form

INV-MAX TYPED-BUFFER P-INV IR-ID:ir-value-id
create P-NEW BMAX cells allot        \ old block ordinal -> new ordinal, or -1 when dropped
create P-COV COV-MAX cells allot     \ scratch: which operations of the header are accounted for

\ ---- the slots, read back ----------------------------------------------------
: CTX ( -- IR-CTX:ctx )              0 S-CTX @ ;
: BLD ( -- IR-BUILD:builder )        0 S-BLD @ ;
: SID ( -- IR-ID:ir-source-id )      0 S-SID @ ;

: SLOT-OF ( HIR:opcode -- n )
   MATCH HIR:opcode
      const    OF O-CONST    ENDOF
      add      OF O-ADD      ENDOF
      sub      OF O-SUB      ENDOF
      mul      OF O-MUL      ENDOF
      div      OF O-DIV      ENDOF
      lt       OF O-LT       ENDOF
      le       OF O-LE       ENDOF
      gt       OF O-GT       ENDOF
      ge       OF O-GE       ENDOF
      equal    OF O-EQUAL    ENDOF
      ne       OF O-NE       ENDOF
      and      OF O-AND      ENDOF
      or       OF O-OR       ENDOF
      xor      OF O-XOR      ENDOF
      lshift   OF O-LSHIFT   ENDOF
      rshift   OF O-RSHIFT   ENDOF
      invert   OF O-INVERT   ENDOF
      mem      OF O-MEM      ENDOF
      load     OF O-LOAD     ENDOF
      store    OF O-STORE    ENDOF
      bload    OF O-BLOAD    ENDOF
      bstore   OF O-BSTORE   ENDOF
      br       OF O-BR       ENDOF
      brz      OF O-BRZ      ENDOF
      call     OF O-CALL     ENDOF
      wordcall OF O-WORDCALL ENDOF
      return   OF O-RETURN   ENDOF
      trap     OF O-TRAP     ENDOF
      fconst   OF O-FCONST   ENDOF
      fadd     OF O-FADD     ENDOF
      fsub     OF O-FSUB     ENDOF
      fmul     OF O-FMUL     ENDOF
      fdiv     OF O-FDIV     ENDOF
      fneg     OF O-FNEG     ENDOF
      fabs     OF O-FABS     ENDOF
      fsqrt    OF O-FSQRT    ENDOF
      flt      OF O-FLT      ENDOF
      fgt      OF O-FGT      ENDOF
      feq      OF O-FEQ      ENDOF
      fltz     OF O-FLTZ     ENDOF
      feqz     OF O-FEQZ     ENDOF
      intreal  OF O-INTREAL  ENDOF
      realint  OF O-REALINT  ENDOF
      bitsreal OF O-BITSREAL ENDOF
      realbits OF O-REALBITS ENDOF
      quot     OF O-QUOT     ENDOF
   ;MATCH ;

: SLOT-OPCODE ( n -- HIR:opcode )
   case
      O-CONST    of HIR-OPCODE:CONST    endof
      O-ADD      of HIR-OPCODE:ADD      endof
      O-SUB      of HIR-OPCODE:SUB      endof
      O-MUL      of HIR-OPCODE:MUL      endof
      O-DIV      of HIR-OPCODE:DIV      endof
      O-LT       of HIR-OPCODE:LT       endof
      O-LE       of HIR-OPCODE:LE       endof
      O-GT       of HIR-OPCODE:GT       endof
      O-GE       of HIR-OPCODE:GE       endof
      O-EQUAL    of HIR-OPCODE:EQUAL    endof
      O-NE       of HIR-OPCODE:NE       endof
      O-AND      of HIR-OPCODE:AND      endof
      O-OR       of HIR-OPCODE:OR       endof
      O-XOR      of HIR-OPCODE:XOR      endof
      O-LSHIFT   of HIR-OPCODE:LSHIFT   endof
      O-RSHIFT   of HIR-OPCODE:RSHIFT   endof
      O-INVERT   of HIR-OPCODE:INVERT   endof
      O-MEM      of HIR-OPCODE:MEM      endof
      O-LOAD     of HIR-OPCODE:LOAD     endof
      O-STORE    of HIR-OPCODE:STORE    endof
      O-BLOAD    of HIR-OPCODE:BLOAD    endof
      O-BSTORE   of HIR-OPCODE:BSTORE   endof
      O-BR       of HIR-OPCODE:BR       endof
      O-BRZ      of HIR-OPCODE:BRZ      endof
      O-CALL     of HIR-OPCODE:CALL     endof
      O-WORDCALL of HIR-OPCODE:WORDCALL endof
      O-QUOT     of HIR-OPCODE:QUOT     endof
      O-RETURN   of HIR-OPCODE:RETURN   endof
      O-TRAP     of HIR-OPCODE:TRAP     endof
      O-FCONST   of HIR-OPCODE:FCONST   endof
      O-FADD     of HIR-OPCODE:FADD     endof
      O-FSUB     of HIR-OPCODE:FSUB     endof
      O-FMUL     of HIR-OPCODE:FMUL     endof
      O-FDIV     of HIR-OPCODE:FDIV     endof
      O-FNEG     of HIR-OPCODE:FNEG     endof
      O-FABS     of HIR-OPCODE:FABS     endof
      O-FSQRT    of HIR-OPCODE:FSQRT    endof
      O-FLT      of HIR-OPCODE:FLT      endof
      O-FGT      of HIR-OPCODE:FGT      endof
      O-FEQ      of HIR-OPCODE:FEQ      endof
      O-FLTZ     of HIR-OPCODE:FLTZ     endof
      O-FEQZ     of HIR-OPCODE:FEQZ     endof
      O-INTREAL  of HIR-OPCODE:INTREAL  endof
      O-REALINT  of HIR-OPCODE:REALINT  endof
      O-BITSREAL of HIR-OPCODE:BITSREAL endof
      O-REALBITS of HIR-OPCODE:REALBITS endof
      E-NLOOP-OPCODE throw
   endcase ;

\ Which member of the family this symbol names. An operation of a form outside it
\ has no rule here and is refused rather than copied blind.
: OPCODE-SLOT ( IR-ID:ir-symbol-id -- n )
   {: sym:IR-ID:ir-symbol-id :}
   -1
   OPCODES-N 0 ?do
      sym i BND-OP @ SAME-SYM? if drop i leave then
   loop
   dup 0 < if E-NLOOP-OPCODE throw then ;

\ Which declared key this symbol is. A frozen module carries no attribute under a
\ key its opcode's schema did not declare - the freeze verifier decides that - so
\ this refusal is fail-closed rather than reachable.
: KEY-SLOT-OF ( IR-ID:ir-symbol-id -- n )
   {: sym:IR-ID:ir-symbol-id :}
   -1
   KEYS-N 0 ?do
      sym i BND-KEY @ SAME-SYM? if drop i leave then
   loop
   dup 0 < if E-NLOOP-OPCODE throw then ;

: OP-SLOT ( IR-ID:ir-op-id -- n )
   OPCODE-AT OPCODE-SLOT ;

: OP-IS? ( IR-ID:ir-op-id n -- bool )
   {: id:IR-ID:ir-op-id want:n :}
   id OP-SLOT want = ;

\ ---- the value map -----------------------------------------------------------
\ Old value to new value. A value defined inside the loop this pass deletes binds
\ NOTHING here, so a reader of one that the recogniser failed to account for does
\ not quietly get some other value - it reaches an unset slot and the rewrite is
\ refused. That is the safety net under every "nothing outside reads it" the
\ header above claims.
: VCLEAR ( -- )
   VMAX 0 ?do
      0 i cells VSET + !
   loop ;

: VSLOT ( IR-ID:ir-value-id -- n )
   IR-ID:VALUE-LOCAL
   dup 0 < over VMAX >= or if E-NLOOP-CAP throw then ;

: VBIND ( IR-ID:ir-value-id IR-ID:ir-value-id -- )
   {: src:IR-ID:ir-value-id new:IR-ID:ir-value-id :}
   src VSLOT {: k:n :}
   new k VMAP !
   1 k cells VSET + ! ;

: VOF ( IR-ID:ir-value-id -- IR-ID:ir-value-id )
   VSLOT {: k:n :}
   k cells VSET + @ 0= if E-NLOOP-SHAPE throw then
   k VMAP @ ;

\ ---- reading the frozen module -----------------------------------------------
: SRC-CK ( IR-ID:ir-source-id -- )
   IR-ID:SOURCE-LOCAL 0<> if E-NLOOP-SHAPE throw then ;

: OP-SPAN ( IR-ID:ir-op-id -- IR-SOURCE:span )
   {: id:IR-ID:ir-op-id :}
   id SPAN-AT IR--SOURCE-SPAN:UNMAKE
   {: src:IR-ID:ir-source-id st:n ln:n :}
   src SRC-CK
   BLD SID st ln IR-BUILD:ADD-SPAN ;

: FUN-SPAN ( IR-ID:ir-fun-id -- IR-SOURCE:span )
   {: f:IR-ID:ir-fun-id :}
   V-FUNR VW MKEY f IR-FUN:FSPAN@ IR--SOURCE-SPAN:UNMAKE
   {: src:IR-ID:ir-source-id st:n ln:n :}
   src SRC-CK
   BLD SID st ln IR-BUILD:ADD-SPAN ;

: BLOCK-SPAN ( IR-ID:ir-block-id -- IR-SOURCE:span )
   {: bk:IR-ID:ir-block-id :}
   V-BLKR VW MKEY bk IR-FUN:FBLOCK-SPAN@ IR--SOURCE-SPAN:UNMAKE
   {: src:IR-ID:ir-source-id st:n ln:n :}
   src SRC-CK
   BLD SID st ln IR-BUILD:ADD-SPAN ;

\ The type of one value of the old module, restated in the new one. The two
\ modules number their types separately, so a value's class is carried across by
\ identity and not by ordinal; a value of none of the three is one this pass has
\ no type for.
: TYPE-OF ( IR-ID:ir-value-id -- IR-ID:ir-type-id )
   {: id:IR-ID:ir-value-id :}
   id VALUE-TYPE-AT {: t:IR-ID:ir-type-id :}
   t 0 BND-CELL @ SAME-TYPE? if CTX BLD HIR:CELL-TYPE exit then
   t 0 BND-REAL @ SAME-TYPE? if CTX BLD HIR:REAL-TYPE exit then
   t 0 BND-MEM @ SAME-TYPE? if CTX BLD HIR:MEM-TYPE exit then
   E-NLOOP-SHAPE throw ;

\ ---- the block ordinals of one function --------------------------------------
\ A successor names a block by its ordinal in the MODULE, and this pass renumbers
\ blocks, so every successor it writes goes through the plan's table. The one
\ function this chain compiles starts its blocks at zero, which is checked before
\ a loop is recognised rather than assumed here.
: BLOCK-ORD ( IR-ID:ir-fun-id n -- n )
   BLOCK-AT IR-ID:BLOCK-LOCAL ;

: SUCC-ORD ( IR-ID:ir-op-id n -- n )
   SUCC-AT IR-ID:BLOCK-LOCAL ;

: NEW-ORD ( n -- n )
   {: b:n :}
   b 0 < b BMAX >= or if E-NLOOP-CAP throw then
   b cells P-NEW + @
   dup 0 < if E-NLOOP-SHAPE throw then ;

: DROPPED? ( n -- bool )
   {: b:n :}
   b P-H @ =  b P-XT @ =  or  b P-LA @ =  or ;

\ ---- what one operation carries ----------------------------------------------
: ATTR-BY-KEY ( IR-ID:ir-op-id n -- n bool )
   {: id:IR-ID:ir-op-id want:n :}
   0 false
   id ATTRS-OF 0 ?do
      id i ATTR-KEY-AT KEY-SLOT-OF want = if
         2drop id i ATTR-INT-AT true leave
      then
   loop ;

\ The number this operation puts on the stack, and whether it is a constant at
\ all. Every reader of a literal in this pass goes through here, so "is this a
\ compile-time number" is one question asked in one place.
: CONST-VALUE ( IR-ID:ir-op-id -- n bool )
   {: id:IR-ID:ir-op-id :}
   id O-CONST OP-IS? 0= if 0 false exit then
   id RESULTS-OF 1 <> if 0 false exit then
   id K-VALUE ATTR-BY-KEY ;

\ Where in this block the operation defining a value is, or -1 when the value is
\ not defined by an operation of this block at all - a block argument, or a value
\ from another block.
\ ANY of an operation's results answers, not only a lone one. An operation that
\ leaves two - a load leaves the cell it read AND the memory it read it out of -
\ defines both, and reading "no operation of this block" for one of them would
\ make a value the block really computes look like a value from outside it.
: DEFINES? ( IR-ID:ir-op-id IR-ID:ir-value-id -- bool )
   {: id:IR-ID:ir-op-id v:IR-ID:ir-value-id :}
   false
   id RESULTS-OF 0 ?do
      id i RESULT-AT v SAME-VALUE? if drop true leave then
   loop ;

: DEF-INDEX ( IR-ID:ir-block-id IR-ID:ir-value-id -- n )
   {: bk:IR-ID:ir-block-id v:IR-ID:ir-value-id :}
   -1
   bk OP-COUNT 0 ?do
      bk i OP-AT v DEFINES? if drop i leave then
   loop ;

: ARG-INDEX ( IR-ID:ir-block-id IR-ID:ir-value-id -- n )
   {: bk:IR-ID:ir-block-id v:IR-ID:ir-value-id :}
   -1
   bk ARG-COUNT 0 ?do
      bk i ARG-AT v SAME-VALUE? if drop i leave then
   loop ;

\ The whole function searched for the operation that defines one value, so that a
\ question about a value defined OUTSIDE the loop - is the start a constant, and
\ which number is it - can be asked at all. A value defined by a block argument
\ answers "no operation", which is a real answer and not a failure.
: FUN-DEF ( IR-ID:ir-fun-id IR-ID:ir-value-id -- IR-ID:ir-op-id bool )
   {: f:IR-ID:ir-fun-id v:IR-ID:ir-value-id :}
   f 0 BLOCK-AT TERM-AT false
   f BLOCK-COUNT 0 ?do
      f i BLOCK-AT {: bk:IR-ID:ir-block-id :}
      bk v DEF-INDEX {: d:n :}
      d 0 >= if 2drop bk d OP-AT true leave then
   loop ;

: FUN-CONST ( IR-ID:ir-fun-id IR-ID:ir-value-id -- n bool )
   FUN-DEF {: id:IR-ID:ir-op-id ok:bool :}
   ok 0= if 0 false exit then
   id CONST-VALUE ;

\ ---- how many edges reach a block --------------------------------------------
\ Every terminator of the function asked for its successors, which is the only
\ authority on what reaches what. The two questions below are what makes deleting
\ the loop's blocks sound: a block reached from somewhere this pass did not
\ account for would still be reached after the loop's own edges were removed.
: EDGES-INTO ( IR-ID:ir-fun-id n -- n )
   {: f:IR-ID:ir-fun-id b:n :}
   0
   f BLOCK-COUNT 0 ?do
      f i BLOCK-AT TERM-AT {: t:IR-ID:ir-op-id :}
      t SUCCS-OF 0 ?do
         t i SUCC-ORD b = if 1+ then
      loop
   loop ;

\ The one block whose terminator names this one, when exactly one does, and -1
\ otherwise. A block named twice by the SAME terminator counts twice, which is
\ what makes a two-way branch to one block answer -1 here rather than naming it.
: SOLE-PRED ( IR-ID:ir-fun-id n -- n )
   {: f:IR-ID:ir-fun-id b:n :}
   f b EDGES-INTO 1 <> if -1 exit then
   -1
   f BLOCK-COUNT 0 ?do
      f i BLOCK-AT TERM-AT {: t:IR-ID:ir-op-id :}
      t SUCCS-OF 0 ?do
         t i SUCC-ORD b = if drop j then
      loop
   loop ;

\ ---- the plan ----------------------------------------------------------------
1 TYPED-BUFFER CH-V IR-ID:ir-value-id      \ the value the chain walk stands on

: PLAN-RESET ( -- )
   0 P-OK !
   -1 P-G !  -1 P-PR !  -1 P-H !  -1 P-XT !  -1 P-LA !  -1 P-JN !
   0 P-A !  -1 P-IDX !  -1 P-K !  0 P-M !  0 P-START !  0 P-KCONST !
   0 P-INV-N ! ;

\ WHERE ONE CARRIED VALUE SITS IN THE HEADER'S ARGUMENT LIST. The elaborator
\ opens a loop's header with the live vector first, then the two counters of
\ every loop the edge crosses, then the locals that cross, then the memory order
\ (src/compiler/native/elaborate.f OPEN-ARGS-H). So the counters are the last two
\ arguments only when the body binds no crossing local and touches no memory at
\ all, and they are in the MIDDLE of the list otherwise. Everything else the
\ header carries - the accumulator, a local, the order - is a CARRIED position,
\ numbered the way the exit stub hands them to the join, and this is the one
\ place that turns such a number into an argument of the header.
: CARRY-ARG ( n -- n )
   {: i:n :}
   i P-IDX @ < if i exit then
   i 2 + ;

\ A header with more operations than the coverage table holds is DECLINED and not
\ refused: a capacity here must cost a routine its fold, never its compilation.
: COV-INIT? ( IR-ID:ir-block-id -- bool )
   OP-COUNT {: n:n :}
   n COV-MAX > if false exit then
   n 0 ?do  0 i cells P-COV + !  loop
   true ;

: COV! ( n -- )
   {: k:n :}
   k 0 < k COV-MAX >= or if E-NLOOP-CAP throw then
   1 k cells P-COV + ! ;

: COVERED? ( n -- bool )
   cells P-COV + @ 0<> ;

\ A block that hands control on and does nothing else: exactly one operation, its
\ terminator, and no arguments of its own. Both stubs of a counted loop are one,
\ and a stub that had grown an operation would be work this pass is about to
\ delete.
: PURE-EDGE? ( IR-ID:ir-block-id -- bool )
   {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT 1 <> if false exit then
   bk ARG-COUNT 0= ;

: BR-TARGET ( IR-ID:ir-block-id -- n bool )
   {: bk:IR-ID:ir-block-id :}
   bk TERM-AT {: t:IR-ID:ir-op-id :}
   t O-BR OP-IS? 0= if 0 false exit then
   t SUCCS-OF 1 <> if 0 false exit then
   t 0 SUCC-ORD true ;

\ ---- the loop's five blocks --------------------------------------------------
\ Read off the header's own terminator and the edges into it, and every one of
\ them checked rather than taken from the shape the elaborator usually builds.
: PLAN-STUBS? ( IR-ID:ir-fun-id n -- bool )
   {: f:IR-ID:ir-fun-id h:n :}
   f h BLOCK-AT TERM-AT {: t:IR-ID:ir-op-id :}
   t O-BRZ OP-IS? 0= if false exit then
   t SUCCS-OF 2 <> if false exit then
   t 0 SUCC-ORD {: xt:n :}
   t 1 SUCC-ORD {: la:n :}
   xt la = if false exit then
   xt h = la h = or if false exit then
   f xt BLOCK-AT PURE-EDGE? 0= if false exit then
   f la BLOCK-AT PURE-EDGE? 0= if false exit then
   f la BLOCK-AT BR-TARGET {: lt:n lok:bool :}
   lok 0= if false exit then
   lt h <> if false exit then
   f xt BLOCK-AT BR-TARGET {: jn:n jok:bool :}
   jok 0= if false exit then
   jn h = jn xt = or jn la = or if false exit then
   f xt EDGES-INTO 1 <> if false exit then
   f la EDGES-INTO 1 <> if false exit then
   h P-H !  xt P-XT !  la P-LA !  jn P-JN !
   true ;

\ The pre-header is the other edge into the header, and the guard is the only
\ edge into the pre-header. Requiring exactly two edges into the header is what
\ says the loop is entered from one place and gone round from one place.
: PLAN-ENTRY? ( IR-ID:ir-fun-id -- bool )
   {: f:IR-ID:ir-fun-id :}
   P-H @ {: h:n :}
   f h EDGES-INTO 2 <> if false exit then
   -1
   f BLOCK-COUNT 0 ?do
      i P-LA @ <> if
         f i BLOCK-AT TERM-AT {: t:IR-ID:ir-op-id :}
         t SUCCS-OF 0 ?do
            t i SUCC-ORD h = if drop j then
         loop
      then
   loop {: pr:n :}
   pr 0 < if false exit then
   f pr BLOCK-AT BR-TARGET {: pt:n pok:bool :}
   pok 0= if false exit then
   pt h <> if false exit then
   f pr SOLE-PRED {: g:n :}
   g 0 < if false exit then
   pr P-PR !  g P-G !
   true ;

\ The guard, which is the whole reason the trip count can be written down: its
\ branch tests `limit - start` and takes its SECOND successor when that is not
\ zero, so control reaches the pre-header exactly when the two differ.
: PLAN-GUARD? ( IR-ID:ir-fun-id -- bool )
   {: f:IR-ID:ir-fun-id :}
   f P-G @ BLOCK-AT TERM-AT {: t:IR-ID:ir-op-id :}
   t O-BRZ OP-IS? 0= if false exit then
   t SUCCS-OF 2 <> if false exit then
   t 1 SUCC-ORD P-PR @ <> if false exit then
   t 0 SUCC-ORD P-PR @ = if false exit then
   f P-G @ BLOCK-AT {: gb:IR-ID:ir-block-id :}
   gb  t 0 OPERAND-AT  DEF-INDEX {: d:n :}
   d 0 < if false exit then
   gb d OP-AT {: sb:IR-ID:ir-op-id :}
   sb O-SUB OP-IS? 0= if false exit then
   sb OPERANDS-OF 2 <> if false exit then
   f P-PR @ BLOCK-AT TERM-AT {: pt:IR-ID:ir-op-id :}
   sb 0 OPERAND-AT  pt P-IDX @ 1+ OPERAND-AT  SAME-VALUE? 0= if false exit then
   sb 1 OPERAND-AT  pt P-IDX @ OPERAND-AT     SAME-VALUE? 0= if false exit then
   true ;

\ ---- the header's own shape --------------------------------------------------
\ How wide the live vector is, which of its positions the loop changes, and that
\ the index really is counted the way `loop` counts it. Everything this word
\ writes is read by the recurrence walk below, and everything it checks is a
\ sentence the closed form depends on.
: PLAN-SHAPE? ( IR-ID:ir-fun-id -- bool )
   {: f:IR-ID:ir-fun-id :}
   f P-H @ BLOCK-AT {: hb:IR-ID:ir-block-id :}
   hb ARG-COUNT {: n:n :}
   n 3 < if false exit then
   n 2 - P-A !
   f P-LA @ BLOCK-AT TERM-AT {: lat:IR-ID:ir-op-id :}
   f P-XT @ BLOCK-AT TERM-AT {: xtt:IR-ID:ir-op-id :}
   f P-PR @ BLOCK-AT TERM-AT {: pt:IR-ID:ir-op-id :}
   lat OPERANDS-OF n <> if false exit then
   pt OPERANDS-OF n <> if false exit then
   xtt OPERANDS-OF P-A @ <> if false exit then
   hb COV-INIT? 0= if false exit then
   hb OP-COUNT 1- COV!
   true ;

\ The two counters, found by their USE and then located in the header's argument
\ list. The index is the value the loop's own addition adds one to and the limit
\ is what the comparison holds the sum against, so both are read off the
\ operations that count rather than taken from where the list usually puts them.
\ WHERE they sit is then recorded, and CARRY-ARG numbers everything else around
\ them.
\
\ THEY ARE ADJACENT, INDEX FIRST, because one edge hands the pair over together
\ (src/compiler/native/elaborate.f LOOP-ARG+ adds them in that order and nothing
\ else writes that list), and the latch hands the incremented index back into the
\ first of the two and the limit unchanged into the second. A header whose
\ counters sit anywhere else is not a shape this pass has a plan for.
: PLAN-COUNTERS? ( IR-ID:ir-fun-id IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id -- bool )
   {: f:IR-ID:ir-fun-id idx:IR-ID:ir-value-id lim:IR-ID:ir-value-id nx:IR-ID:ir-value-id :}
   f P-H @ BLOCK-AT  idx ARG-INDEX {: a:n :}
   a 0 < if false exit then
   f P-H @ BLOCK-AT  lim ARG-INDEX  a 1+ <> if false exit then
   f P-LA @ BLOCK-AT TERM-AT {: lat:IR-ID:ir-op-id :}
   lat a OPERAND-AT     nx  SAME-VALUE? 0= if false exit then
   lat a 1+ OPERAND-AT  lim SAME-VALUE? 0= if false exit then
   a P-IDX !
   true ;

\ The comparison the loop leaves on, the addition it counts with, and the one
\ that addition adds. Each is found from the operand of the operation above it,
\ so what is checked is that these four operations really are one counted step
\ and not four operations that happen to be in the block.
: PLAN-STEP? ( IR-ID:ir-fun-id -- bool )
   {: f:IR-ID:ir-fun-id :}
   f P-H @ BLOCK-AT {: hb:IR-ID:ir-block-id :}
   hb TERM-AT {: ht:IR-ID:ir-op-id :}
   hb  ht 0 OPERAND-AT  DEF-INDEX {: fi:n :}
   fi 0 < if false exit then
   hb fi OP-AT {: fop:IR-ID:ir-op-id :}
   fop O-LT OP-IS? 0= if false exit then
   fop OPERANDS-OF 2 <> if false exit then
   hb  fop 0 OPERAND-AT  DEF-INDEX {: ni:n :}
   ni 0 < if false exit then
   hb ni OP-AT {: nop:IR-ID:ir-op-id :}
   nop O-ADD OP-IS? 0= if false exit then
   nop OPERANDS-OF 2 <> if false exit then
   hb  nop 1 OPERAND-AT  DEF-INDEX {: oi:n :}
   oi 0 < if false exit then
   hb oi OP-AT CONST-VALUE {: one:n ok:bool :}
   ok 0= if false exit then
   one 1 <> if false exit then
   f  nop 0 OPERAND-AT  fop 1 OPERAND-AT  nop 0 RESULT-AT
   PLAN-COUNTERS? 0= if false exit then
   fi COV!  ni COV!  oi COV!
   true ;

\ Which position of the live vector the loop changes - exactly one, or this is
\ not a single-accumulator loop - and that the exit stub hands the join the same
\ list the latch hands the header. That second half is what lets the closed form
\ be handed to the join in the latch's own positions.
\
\ NONE-YET AND TOO-MANY ARE TWO ANSWERS AND NOT ONE NEGATIVE NUMBER, because the
\ scan asks about the one it already has rather than about its sign. Whether that
\ distinction is load-bearing was MEASURED rather than assumed: writing the test
\ as "is one already set" instead is a plan that names the last changed position
\ when three change, and the suite stays green under it - the second half of this
\ word catches that shape anyway, because a position the loop changed cannot
\ match the argument the header took. So the two names are for the reader, the
\ second half is the guard, and test/compiler/native-loop.f's three-accumulator
\ row is what holds both.
-1 constant ACC-NONE
-2 constant ACC-MANY

: ACC-SEEN ( n n -- n )
   {: sofar:n at:n :}
   sofar ACC-NONE <> if ACC-MANY exit then
   at ;

: PLAN-ACC? ( IR-ID:ir-fun-id -- bool )
   {: f:IR-ID:ir-fun-id :}
   f P-H @ BLOCK-AT {: hb:IR-ID:ir-block-id :}
   f P-LA @ BLOCK-AT TERM-AT {: lat:IR-ID:ir-op-id :}
   f P-XT @ BLOCK-AT TERM-AT {: xtt:IR-ID:ir-op-id :}
   ACC-NONE
   P-A @ 0 ?do
      lat i CARRY-ARG OPERAND-AT  hb i CARRY-ARG ARG-AT  SAME-VALUE? 0= if
         i ACC-SEEN
      then
   loop {: k:n :}
   k 0 < if false exit then
   k P-K !
   true
   P-A @ 0 ?do
      xtt i OPERAND-AT
      i k = if lat k CARRY-ARG OPERAND-AT else hb i CARRY-ARG ARG-AT then
      SAME-VALUE? 0= if drop false leave then
   loop ;

\ ---- what one turn adds ------------------------------------------------------
\ The walk back from the accumulator's new value to the accumulator itself. Each
\ step must be an addition whose LEFT operand carries the chain on, which is what
\ `acc x +` builds; an addition written the other way round is declined rather
\ than searched for, because a search would have to guess which operand is the
\ accumulator when both are additions of this block.
0 constant CH-WALK
1 constant CH-DONE
-1 constant CH-NO
variable CH-STATE

: CHAIN-INV+ ( IR-ID:ir-value-id -- )
   {: v:IR-ID:ir-value-id :}
   P-INV-N @ {: n:n :}
   n INV-MAX >= if CH-NO CH-STATE ! exit then
   v n P-INV !
   n 1+ P-INV-N ! ;

\ One addend classified: the loop index, a number this block builds, or a value
\ from outside the loop. A header argument that is not the index is declined -
\ it would be loop-invariant in effect, but reading one would mean carrying the
\ header's argument list into the arms, and no measured row needs it.
: CHAIN-ADDEND ( IR-ID:ir-block-id IR-ID:ir-value-id -- )
   {: hb:IR-ID:ir-block-id v:IR-ID:ir-value-id :}
   v  hb P-IDX @ ARG-AT  SAME-VALUE? if P-M @ 1+ P-M ! exit then
   hb v ARG-INDEX 0 >= if CH-NO CH-STATE ! exit then
   hb v DEF-INDEX {: d:n :}
   d 0 < if v CHAIN-INV+ exit then
   hb d OP-AT CONST-VALUE {: val:n ok:bool :}
   ok 0= if CH-NO CH-STATE ! exit then
   P-KCONST @ val + P-KCONST !
   d COV! ;

: CHAIN-STEP ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f P-H @ BLOCK-AT {: hb:IR-ID:ir-block-id :}
   0 CH-V @  hb P-K @ CARRY-ARG ARG-AT  SAME-VALUE? if CH-DONE CH-STATE ! exit then
   hb 0 CH-V @ DEF-INDEX {: d:n :}
   d 0 < if CH-NO CH-STATE ! exit then
   hb d OP-AT {: id:IR-ID:ir-op-id :}
   id O-ADD OP-IS? 0= if CH-NO CH-STATE ! exit then
   id OPERANDS-OF 2 <> if CH-NO CH-STATE ! exit then
   id RESULTS-OF 1 <> if CH-NO CH-STATE ! exit then
   d COV!
   hb  id 1 OPERAND-AT  CHAIN-ADDEND
   id 0 OPERAND-AT 0 CH-V ! ;

: PLAN-CHAIN? ( IR-ID:ir-fun-id -- bool )
   {: f:IR-ID:ir-fun-id :}
   f P-H @ BLOCK-AT {: hb:IR-ID:ir-block-id :}
   f P-LA @ BLOCK-AT TERM-AT  P-K @ CARRY-ARG OPERAND-AT  0 CH-V !
   0 P-M !  0 P-KCONST !  0 P-INV-N !
   CH-WALK CH-STATE !
   hb OP-COUNT 1+ 0 ?do
      CH-STATE @ CH-WALK = if f CHAIN-STEP then
   loop
   CH-STATE @ CH-DONE = ;

\ Every operation of the header accounted for. This is the coverage check the
\ soundness argument rests on: a store, a load, a call, a trap, a second
\ accumulator or an operation whose result nothing reads is an operation no rule
\ here claimed, and the loop is declined rather than folded around it.
: PLAN-COVER? ( IR-ID:ir-fun-id -- bool )
   {: f:IR-ID:ir-fun-id :}
   f P-H @ BLOCK-AT {: hb:IR-ID:ir-block-id :}
   true
   hb OP-COUNT 0 ?do
      i COVERED? 0= if drop false leave then
   loop ;

\ ---- the start, which has to be a number this pass can read ------------------
\ The largest representable integer is the one start the trip-count table has no
\ row for: there `index + 1` wraps below the limit and the loop runs round nearly
\ the whole range.
$7FFFFFFFFFFFFFFF constant MAX-START

: PLAN-START? ( IR-ID:ir-fun-id -- bool )
   {: f:IR-ID:ir-fun-id :}
   f P-PR @ BLOCK-AT TERM-AT {: pt:IR-ID:ir-op-id :}
   f  pt P-IDX @ OPERAND-AT  FUN-CONST {: v:n ok:bool :}
   ok 0= if false exit then
   v MAX-START = if false exit then
   v P-START !
   true ;

\ ---- what the rewriter's tables can hold -------------------------------------
\ Asked as part of recognising a loop rather than raised as a refusal during the
\ rewrite, so a function too big for the value map is compiled exactly as it was
\ instead of failing to compile at all.
: OPERANDS-FIT? ( IR-ID:ir-op-id -- bool )
   {: id:IR-ID:ir-op-id :}
   true
   id OPERANDS-OF 0 ?do
      id i OPERAND-AT IR-ID:VALUE-LOCAL VMAX >= if drop false leave then
   loop ;

: RESULTS-FIT? ( IR-ID:ir-op-id -- bool )
   {: id:IR-ID:ir-op-id :}
   true
   id RESULTS-OF 0 ?do
      id i RESULT-AT IR-ID:VALUE-LOCAL VMAX >= if drop false leave then
   loop ;

: VALUES-FIT-OP? ( IR-ID:ir-op-id -- bool )
   {: id:IR-ID:ir-op-id :}
   id OPERANDS-FIT? 0= if false exit then
   id RESULTS-FIT? ;

: ARGS-FIT? ( IR-ID:ir-block-id -- bool )
   {: bk:IR-ID:ir-block-id :}
   true
   bk ARG-COUNT 0 ?do
      bk i ARG-AT IR-ID:VALUE-LOCAL VMAX >= if drop false leave then
   loop ;

: OPS-FIT? ( IR-ID:ir-block-id -- bool )
   {: bk:IR-ID:ir-block-id :}
   true
   bk OP-COUNT 0 ?do
      bk i OP-AT VALUES-FIT-OP? 0= if drop false leave then
   loop ;

: VALUES-FIT-BLOCK? ( IR-ID:ir-block-id -- bool )
   {: bk:IR-ID:ir-block-id :}
   bk ARGS-FIT? 0= if false exit then
   bk OPS-FIT? ;

: VALUES-FIT? ( IR-ID:ir-fun-id -- bool )
   {: f:IR-ID:ir-fun-id :}
   true
   f BLOCK-COUNT 0 ?do
      f i BLOCK-AT VALUES-FIT-BLOCK? 0= if drop false leave then
   loop ;

\ ---- one candidate header, tried whole ---------------------------------------
: PLAN-TRY ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id h:n :}
   PLAN-RESET
   f h PLAN-STUBS? 0= if exit then
   f PLAN-ENTRY? 0= if exit then
   f PLAN-SHAPE? 0= if exit then
   f PLAN-STEP? 0= if exit then
   f PLAN-ACC? 0= if exit then
   f PLAN-CHAIN? 0= if exit then
   f PLAN-COVER? 0= if exit then
   f PLAN-GUARD? 0= if exit then
   f PLAN-START? 0= if exit then
   f VALUES-FIT? 0= if exit then
   1 P-OK ! ;

\ The new block ordinals, written once the plan is settled: the three blocks of
\ the loop lose theirs, and the two arms take the two that follow the pre-header.
: PLAN-ORDS ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   0
   f BLOCK-COUNT 0 ?do
      i DROPPED? if
         -1 i cells P-NEW + !
      else
         dup i cells P-NEW + !
         1+
         i P-PR @ = if
            dup P-ONE !  1+
            dup P-MANY ! 1+
         then
      then
   loop drop ;

: PLAN-FUN ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   PLAN-RESET
   f BLOCK-COUNT BMAX > if exit then
   f 0 BLOCK-ORD 0<> if exit then
   f BLOCK-COUNT 0 ?do
      P-OK @ 0= if f i PLAN-TRY then
   loop
   P-OK @ 0= if exit then
   f PLAN-ORDS ;

\ ---- staging one operation in the new module ---------------------------------
: OPEN ( IR-ID:ir-op-id HIR:opcode -- )
   {: id:IR-ID:ir-op-id o:HIR:opcode :}
   CTX BLD  CTX BLD o HIR:OPCODE  IR-BUILD:BEGIN-OP
   CTX BLD  id OP-SPAN  IR-BUILD:SET-OP-SPAN ;

: OPERAND+ ( IR-ID:ir-value-id -- )
   CTX BLD rot IR-BUILD:ADD-OPERAND ;

: CELL-RESULT+ ( -- )
   CTX BLD  CTX BLD HIR:CELL-TYPE  IR-BUILD:ADD-RESULT ;

: CLOSE ( -- IR-ID:ir-op-id )
   CTX BLD IR-BUILD:END-OP ;

: RESULT@ ( IR-ID:ir-op-id n -- IR-ID:ir-value-id )
   {: id:IR-ID:ir-op-id i:n :}
   CTX BLD id i IR-BUILD:OP-RESULT@ ;

\ A successor named by a NEW ordinal, which the two arms are: they exist only in
\ the module being written, so there is no old ordinal to translate.
: NSUCC+ ( n -- )
   {: b:n :}
   CTX BLD
   BLD IR-BUILD:MODULE-KEY  b  IR-ID:PACK-BLOCK
   IR-BUILD:ADD-SUCCESSOR ;

\ And one named by an OLD ordinal, translated through the plan's table.
: SUCC+ ( n -- )
   NEW-ORD NSUCC+ ;

\ The two shapes every operation this pass INVENTS has: a binary operation over
\ cells, and a number. Each takes the old operation whose span it stands at, so
\ a diagnostic about the closed form points at the loop it replaced.
: MK2 ( IR-ID:ir-op-id HIR:opcode IR-ID:ir-value-id IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: sp:IR-ID:ir-op-id o:HIR:opcode a:IR-ID:ir-value-id b:IR-ID:ir-value-id :}
   sp o OPEN
   a OPERAND+
   b OPERAND+
   CELL-RESULT+
   CLOSE 0 RESULT@ ;

\ A constant this pass mints for itself - a loop bound, a stride, an index. Every
\ one of them is an ORDINARY number: this pass rewrites control flow and never
\ invents an address, so the kind is stated here as a fact about what the pass
\ does rather than passed in by callers who would all pass the same thing.
: MKC ( IR-ID:ir-op-id n -- IR-ID:ir-value-id )
   {: sp:IR-ID:ir-op-id v:n :}
   sp HIR-OPCODE:CONST OPEN
   CELL-RESULT+
   CTX BLD  CTX BLD HIR:KEY-VALUE  CTX BLD v IR-BUILD:INTERN-INT-ATTR
   IR-BUILD:ADD-ATTR
   CTX BLD  CTX BLD HIR:KEY-ADDR  CTX BLD HIR:ADDR-NONE HIR:ADDR-ATTR
   IR-BUILD:ADD-ATTR
   CLOSE 0 RESULT@ ;

\ ---- copying one operation of an untouched block -----------------------------
: COPY-ATTRS ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id ATTRS-OF {: n:n :}
   n 0 ?do
      id i ATTR-KEY-AT KEY-SLOT-OF {: k:n :}
      id i ATTR-INT-AT {: v:n :}
      k K-VALUE = if
         CTX BLD  CTX BLD HIR:KEY-VALUE  CTX BLD v IR-BUILD:INTERN-INT-ATTR
         IR-BUILD:ADD-ATTR
      then
      k K-ADDR = if
         CTX BLD  CTX BLD HIR:KEY-ADDR  CTX BLD v HIR:ADDR-ATTR
         IR-BUILD:ADD-ATTR
      then
      k K-ENTRY = if
         CTX BLD  CTX BLD HIR:KEY-ENTRY  CTX BLD v IR-BUILD:INTERN-INT-ATTR
         IR-BUILD:ADD-ATTR
      then
      k K-IN = if
         CTX BLD  CTX BLD HIR:KEY-IN  CTX BLD v IR-BUILD:INTERN-INT-ATTR
         IR-BUILD:ADD-ATTR
      then
      k K-OUT = if
         CTX BLD  CTX BLD HIR:KEY-OUT  CTX BLD v IR-BUILD:INTERN-INT-ATTR
         IR-BUILD:ADD-ATTR
      then
   loop ;

\ A successor is carried across by its NEW ordinal, because this pass renumbers
\ blocks. Every one goes through the plan's table, so a branch to a block the
\ rewrite deleted is a refusal and never a branch to whatever now stands there.
: COPY-SUCCS ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id SUCCS-OF {: n:n :}
   n 0 ?do
      id i SUCC-ORD SUCC+
   loop ;

: COPY-OPERANDS ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id OPERANDS-OF {: n:n :}
   n 0 ?do
      id i OPERAND-AT VOF OPERAND+
   loop ;

: COPY-RESULTS ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id RESULTS-OF {: n:n :}
   n 0 ?do
      CTX BLD  id i RESULT-AT TYPE-OF  IR-BUILD:ADD-RESULT
   loop ;

: BIND-RESULTS ( IR-ID:ir-op-id IR-ID:ir-op-id -- )
   {: old:IR-ID:ir-op-id new:IR-ID:ir-op-id :}
   old RESULTS-OF {: n:n :}
   n 0 ?do
      old i RESULT-AT  new i RESULT@  VBIND
   loop ;

: COPY-OP ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id OP-SLOT SLOT-OPCODE {: o:HIR:opcode :}
   id o OPEN
   id COPY-OPERANDS
   id COPY-RESULTS
   id COPY-SUCCS
   id COPY-ATTRS
   id  CLOSE  BIND-RESULTS ;

: OPEN-BLOCK ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   CTX BLD IR-BUILD:BEGIN-BLOCK
   CTX BLD bk BLOCK-SPAN IR-BUILD:SET-BLOCK-SPAN
   bk ARG-COUNT {: n:n :}
   n 0 ?do
      bk i ARG-AT {: a:IR-ID:ir-value-id :}
      a
      CTX BLD  a TYPE-OF  IR-BUILD:ADD-BLOCK-ARG
      VBIND
   loop ;

: CLOSE-BLOCK ( -- )
   CTX BLD IR-BUILD:END-BLOCK drop ;

: COPY-BLOCK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT {: n:n :}
   n 1 < if E-NLOOP-SHAPE throw then
   bk OPEN-BLOCK
   n 0 ?do
      bk i OP-AT COPY-OP
   loop
   CLOSE-BLOCK ;

\ ---- the closed form ---------------------------------------------------------
1 TYPED-BUFFER W-K IR-ID:ir-value-id     \ what one turn adds that is not the index
variable W-K?
1 TYPED-BUFFER W-ACC IR-ID:ir-value-id   \ the arm's answer, built up one term at a time

: K-ADD ( IR-ID:ir-op-id IR-ID:ir-value-id -- )
   {: sp:IR-ID:ir-op-id v:IR-ID:ir-value-id :}
   W-K? @ 0= if v 0 W-K !  1 W-K? !  exit then
   sp HIR-OPCODE:ADD  0 W-K @  v  MK2  0 W-K ! ;

\ The sum of everything one turn adds apart from the index, staged in the
\ pre-header because both arms read it. The constants the loop built inside its
\ own header are added up here at compile time instead of being copied out: four
\ additions of one become one number, which is what makes the smallest row of the
\ three smaller than the loop it replaces.
: EMIT-K ( IR-ID:ir-op-id -- )
   {: sp:IR-ID:ir-op-id :}
   0 W-K? !
   P-INV-N @ 0 ?do
      sp  i P-INV @ VOF  K-ADD
   loop
   P-KCONST @ 0<> if
      sp  sp P-KCONST @ MKC  K-ADD
   then ;

: ACC-ADD ( IR-ID:ir-op-id IR-ID:ir-value-id -- )
   {: sp:IR-ID:ir-op-id v:IR-ID:ir-value-id :}
   sp HIR-OPCODE:ADD  0 W-ACC @  v  MK2  0 W-ACC ! ;

\ T*(T-1)/2 in sixty-four bits, exactly. One of T and T-1 is even, so the halving
\ happens before the multiply and the product that would have overflowed never
\ exists: `(T >> 1) * (T - 1 + (T & 1))`.
: EMIT-HALF ( IR-ID:ir-op-id IR-ID:ir-value-id IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: sp:IR-ID:ir-op-id t:IR-ID:ir-value-id one:IR-ID:ir-value-id :}
   sp HIR-OPCODE:SUB    t one MK2 {: tm1:IR-ID:ir-value-id :}
   sp HIR-OPCODE:RSHIFT t one MK2 {: hlf:IR-ID:ir-value-id :}
   sp HIR-OPCODE:AND    t one MK2 {: par:IR-ID:ir-value-id :}
   sp HIR-OPCODE:ADD    tm1 par MK2 {: p:IR-ID:ir-value-id :}
   sp HIR-OPCODE:MUL    hlf p MK2 ;

\ The index's whole contribution over T turns: start*T + T*(T-1)/2, taken m
\ times. The two multiplications that a compile-time zero or one would make
\ pointless are not staged at all - the start is a constant this pass has read
\ and m is a count it made itself, so neither is a guess.
1 TYPED-BUFFER W-IX IR-ID:ir-value-id    \ the index term while it is being built

: EMIT-INDEX ( IR-ID:ir-op-id IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: sp:IR-ID:ir-op-id t:IR-ID:ir-value-id one:IR-ID:ir-value-id st:IR-ID:ir-value-id :}
   sp t one EMIT-HALF 0 W-IX !
   P-START @ 0<> if
      sp HIR-OPCODE:MUL st t MK2 {: p0:IR-ID:ir-value-id :}
      sp HIR-OPCODE:ADD p0  0 W-IX @  MK2 0 W-IX !
   then
   P-M @ 1 > if
      sp P-M @ MKC {: mc:IR-ID:ir-value-id :}
      sp HIR-OPCODE:MUL mc  0 W-IX @  MK2 0 W-IX !
   then
   0 W-IX @ ;

\ ---- the two arms ------------------------------------------------------------
\ Which value the join is handed at one position: the arm's own answer at the
\ accumulator's position, and at every other position the value the pre-header
\ was handing the header - which is the same value, because the recogniser
\ established that the latch hands every other position back unchanged.
: ARM-OPERAND ( IR-ID:ir-op-id n -- IR-ID:ir-value-id )
   {: pt:IR-ID:ir-op-id i:n :}
   i P-K @ = if 0 W-ACC @ exit then
   pt i CARRY-ARG OPERAND-AT VOF ;

: ARM-TERM ( IR-ID:ir-fun-id IR-ID:ir-op-id -- )
   {: f:IR-ID:ir-fun-id sp:IR-ID:ir-op-id :}
   f P-PR @ BLOCK-AT TERM-AT {: pt:IR-ID:ir-op-id :}
   sp HIR-OPCODE:BR OPEN
   P-A @ 0 ?do
      pt i ARM-OPERAND OPERAND+
   loop
   P-JN @ SUCC+
   CLOSE drop ;

\ The arm the loop takes when it runs one turn, which is every ordering where the
\ start is not below the limit. The formula at T = 1 is acc0 + K + m*start, and
\ both products are gone: K is already a value and m*start is a number this pass
\ multiplies out itself, because m is a count it made and the start is a constant
\ it read.
: EMIT-ONE ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f P-H @ BLOCK-AT TERM-AT {: sp:IR-ID:ir-op-id :}
   f P-PR @ BLOCK-AT TERM-AT {: pt:IR-ID:ir-op-id :}
   CTX BLD IR-BUILD:BEGIN-BLOCK
   CTX BLD  f P-H @ BLOCK-AT BLOCK-SPAN  IR-BUILD:SET-BLOCK-SPAN
   pt P-K @ CARRY-ARG OPERAND-AT VOF 0 W-ACC !
   W-K? @ 0<> if sp  0 W-K @  ACC-ADD then
   P-M @ P-START @ * {: ms:n :}
   ms 0<> if sp  sp ms MKC  ACC-ADD then
   f sp ARM-TERM
   CLOSE-BLOCK ;

\ The arm the loop takes when the start is below the limit, where the trip count
\ is the guard's own subtraction. The subtraction is REUSED rather than recomputed:
\ it is defined in the guard, which dominates this arm, and it is exactly the
\ number of turns.
: EMIT-MANY ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f P-H @ BLOCK-AT TERM-AT {: sp:IR-ID:ir-op-id :}
   f P-PR @ BLOCK-AT TERM-AT {: pt:IR-ID:ir-op-id :}
   f P-G @ BLOCK-AT TERM-AT 0 OPERAND-AT VOF {: t:IR-ID:ir-value-id :}
   CTX BLD IR-BUILD:BEGIN-BLOCK
   CTX BLD  f P-H @ BLOCK-AT BLOCK-SPAN  IR-BUILD:SET-BLOCK-SPAN
   pt P-K @ CARRY-ARG OPERAND-AT VOF 0 W-ACC !
   W-K? @ 0<> if sp HIR-OPCODE:MUL  0 W-K @  t MK2 {: kt:IR-ID:ir-value-id :}
      sp kt ACC-ADD
   then
   P-M @ 0 > if
      sp 1 MKC {: one:IR-ID:ir-value-id :}
      sp t one  pt P-IDX @ OPERAND-AT VOF  EMIT-INDEX {: ix:IR-ID:ir-value-id :}
      sp ix ACC-ADD
   then
   f sp ARM-TERM
   CLOSE-BLOCK ;

\ ---- the pre-header, which now decides which form to run ---------------------
\ It keeps its own arguments and gains the invariant sum and one comparison. The
\ comparison and the branch that reads it are the pair src/compiler/native/select.f
\ already fuses into one compare-and-branch, so the test costs no register.
: EMIT-PRE ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f P-PR @ BLOCK-AT {: pb:IR-ID:ir-block-id :}
   pb TERM-AT {: pt:IR-ID:ir-op-id :}
   pb OPEN-BLOCK
   pt EMIT-K
   pt HIR-OPCODE:LT
   pt P-IDX @ OPERAND-AT VOF
   pt P-IDX @ 1+ OPERAND-AT VOF
   MK2 {: fl:IR-ID:ir-value-id :}
   pt HIR-OPCODE:BRZ OPEN
   fl OPERAND+
   P-ONE @ NSUCC+
   P-MANY @ NSUCC+
   CLOSE drop
   CLOSE-BLOCK
   1 N-FOLDED +! ;

\ ---- the function ------------------------------------------------------------
: FUN-NAME ( IR-ID:ir-fun-id -- IR-ID:ir-symbol-id )
   {: f:IR-ID:ir-fun-id :}
   V-SYMP VW V-SYMR VW  V-FUNR VW MKEY f IR-FUN:FSYMBOL@  NAMEBUF NAME-CAP
   IR-SYM:FCOPY {: u:n :}
   CTX BLD NAMEBUF u IR-BUILD:INTERN-SYMBOL ;

\ The routine's signature, restated in the new module: one cell per input and one
\ per output, exactly as the old module has them.
: FUN-SIG ( IR-ID:ir-fun-id -- IR-ID:ir-type-id )
   {: f:IR-ID:ir-fun-id :}
   V-TYPR VW  V-FUNR VW MKEY f IR-FUN:FSIGNATURE@  IR-TYPE:FARITY@
   {: in:n out:n :}
   CTX BLD HIR:CELL-TYPE {: t:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN
   in 0 ?do t IR-TYPE:FN-PARAM loop
   out 0 ?do t IR-TYPE:FN-RESULT loop
   CTX BLD IR-BUILD:INTERN-CODE-REF ;

: WALK-BLOCK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   b DROPPED? if exit then
   b P-PR @ = if
      f EMIT-PRE
      f EMIT-ONE
      f EMIT-MANY
      exit
   then
   f b COPY-BLOCK ;

: WALK-FUN ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f PLAN-FUN
   P-OK @ 0= if E-NLOOP-PLAN throw then
   CTX BLD f FUN-NAME IR-BUILD:BEGIN-FUN
   CTX BLD f FUN-SIG IR-BUILD:SET-SIGNATURE
   CTX BLD  V-FUNR VW f IR-FUN:FLINKAGE@  IR-BUILD:SET-LINKAGE
   CTX BLD  V-FUNR VW f IR-FUN:FVISIBILITY@  IR-BUILD:SET-VISIBILITY
   CTX BLD  V-FUNR VW f IR-FUN:FCONVENTION@  IR-BUILD:SET-CONVENTION
   CTX BLD f FUN-SPAN IR-BUILD:SET-FUN-SPAN
   VCLEAR
   f BLOCK-COUNT 0 ?do f i WALK-BLOCK loop
   CTX BLD IR-BUILD:END-FUN drop ;

\ ---- what one rewrite is told ------------------------------------------------
: SOURCE! ( IR-CTX:ctx IR-BUILD:builder ptr u8 n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p u:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   V-SRC VW IR-SOURCE:FSOURCES 1 <> if E-NLOOP-SHAPE throw then
   V-SRC VW  MKEY 0 IR-ID:PACK-SOURCE  IR-SOURCE:FDIGEST@
   p u CDIGEST:COMPUTE
   CDIGEST-DIGEST:EQ 0= if E-NLOOP-SOURCE throw then
   c b p u IR-BUILD:ADD-SOURCE 0 S-SID ! ;

\ The binding is taken whatever the outcome, so neither a rewrite without a
\ binding nor a refused rewrite can leave one behind for the next caller.
: BND-TAKE ( -- )
   BND-MODE @ {: have:n :}
   BOUND-NO BND-MODE !
   have BOUND-YES <> if E-NLOOP-BIND throw then ;

: BND-MODULE-CK ( IR-BUILD:module -- )
   IR-BUILD:FMODULE  0 BND-MOD @  IR-ID:MODULE-SAME?
   0= if E-NLOOP-BIND throw then ;

: BIND1 ( IR-CTX:ctx IR-BUILD:builder HIR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder o:HIR:opcode :}
   c b o HIR:OPCODE  o SLOT-OF BND-OP ! ;

: DIALECT-CK ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b  c b IR-BUILD:DIALECT@  HIR:NAME IR-BUILD:SYMBOL-IS?
   0= if E-NLOOP-BIND throw then
   c b IR-BUILD:SCHEMA-MAJOR@ HIR:MAJOR <> if E-NLOOP-BIND throw then
   c b IR-BUILD:SCHEMA-MINOR@ HIR:MINOR <> if E-NLOOP-BIND throw then ;

: BIND-INT ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b HIR-OPCODE:CONST    BIND1
   c b HIR-OPCODE:ADD      BIND1
   c b HIR-OPCODE:SUB      BIND1
   c b HIR-OPCODE:MUL      BIND1
   c b HIR-OPCODE:DIV      BIND1
   c b HIR-OPCODE:LT       BIND1
   c b HIR-OPCODE:LE       BIND1
   c b HIR-OPCODE:GT       BIND1
   c b HIR-OPCODE:GE       BIND1
   c b HIR-OPCODE:EQUAL    BIND1
   c b HIR-OPCODE:NE       BIND1
   c b HIR-OPCODE:AND      BIND1
   c b HIR-OPCODE:OR       BIND1
   c b HIR-OPCODE:XOR      BIND1
   c b HIR-OPCODE:LSHIFT   BIND1
   c b HIR-OPCODE:RSHIFT   BIND1
   c b HIR-OPCODE:INVERT   BIND1 ;

: BIND-MEMORY ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b HIR-OPCODE:MEM      BIND1
   c b HIR-OPCODE:LOAD     BIND1
   c b HIR-OPCODE:STORE    BIND1
   c b HIR-OPCODE:BLOAD    BIND1
   c b HIR-OPCODE:BSTORE   BIND1
   c b HIR-OPCODE:BR       BIND1
   c b HIR-OPCODE:BRZ      BIND1
   c b HIR-OPCODE:CALL     BIND1
   c b HIR-OPCODE:WORDCALL BIND1
   c b HIR-OPCODE:RETURN   BIND1
   c b HIR-OPCODE:TRAP     BIND1 ;

: BIND-REAL ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b HIR-OPCODE:FCONST   BIND1
   c b HIR-OPCODE:FADD     BIND1
   c b HIR-OPCODE:FSUB     BIND1
   c b HIR-OPCODE:FMUL     BIND1
   c b HIR-OPCODE:FDIV     BIND1
   c b HIR-OPCODE:FNEG     BIND1
   c b HIR-OPCODE:FABS     BIND1
   c b HIR-OPCODE:FSQRT    BIND1
   c b HIR-OPCODE:FLT      BIND1
   c b HIR-OPCODE:FGT      BIND1
   c b HIR-OPCODE:FEQ      BIND1
   c b HIR-OPCODE:FLTZ     BIND1
   c b HIR-OPCODE:FEQZ     BIND1
   c b HIR-OPCODE:INTREAL  BIND1
   c b HIR-OPCODE:REALINT  BIND1
   c b HIR-OPCODE:BITSREAL BIND1
   c b HIR-OPCODE:REALBITS BIND1 ;

public

\ ---- binding the dialect -----------------------------------------------------
\ Learn the operation, key and type identities of the module that is about to be
\ read, while it is still being built - the only moment a module can be asked
\ them, because its symbols and types are its own ordinals. The binding is spent
\ by the next REWRITE, or given back by RELEASE when the scan finds nothing.
: BIND-DIALECT ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   BND-MODE @ BOUND-YES = if E-NLOOP-BIND throw then
   c b DIALECT-CK
   b IR-BUILD:MODULE@ 0 BND-MOD !
   c b BIND-INT
   c b BIND-MEMORY
   c b BIND-REAL
   c b HIR:KEY-VALUE K-VALUE BND-KEY !
   c b HIR:KEY-ENTRY K-ENTRY BND-KEY !
   c b HIR:KEY-IN    K-IN    BND-KEY !
   c b HIR:KEY-OUT   K-OUT   BND-KEY !
   c b HIR:KEY-ADDR  K-ADDR  BND-KEY !
   c b HIR:CELL-TYPE 0 BND-CELL !
   c b HIR:MEM-TYPE  0 BND-MEM !
   c b HIR:REAL-TYPE 0 BND-REAL !
   BOUND-YES BND-MODE ! ;

: BOUND? ( -- bool )
   BND-MODE @ BOUND-YES = ;

\ Give up a binding without rewriting against it: what a caller does when the
\ scan below recognises no loop, and what one does when a later stage refuses.
: RELEASE ( -- )
   BND-TAKE ;

\ ---- what the module holds ---------------------------------------------------
\ How many loops this module would close, asked before anything is built. A
\ caller that gets zero keeps the module it has, which is what keeps every
\ routine WITHOUT a foldable loop byte-for-byte what it was: no second module is
\ built, no value is renumbered, and every stage downstream sees exactly what it
\ saw before. It reads through the bound module's own cursor, so it is asked
\ between the binding and the rewrite.
\
\ MORE THAN ONE FUNCTION IN A MODULE IS DECLINED RATHER THAN WALKED. This chain
\ compiles one definition into one module, and a block's ordinal within its
\ function is its ordinal within the module only while that holds; a second
\ function would make the plan's renumbering table name someone else's block.
: FOLDS ( IR-BUILD:module -- n )
   {: m:IR-BUILD:module :}
   BOUND? 0= if E-NLOOP-BIND throw then
   m BND-MODULE-CK
   m VIEWS!
   FUN-COUNT 1 <> if 0 exit then
   MKEY 0 IR-ID:PACK-FUN PLAN-FUN
   P-OK @ 0= if 0 exit then
   1 ;

\ ---- the pass ----------------------------------------------------------------
\ Build the module in which that loop is the arithmetic it would have computed,
\ and answer it frozen. The builder is a fresh one from HIR:NEW-BUILDER - this
\ pass registers the source operation family into it - and the bytes are the
\ source text the old module was compiled from, proved by digest before any span
\ is carried across.
: REWRITE ( IR-CTX:ctx IR-BUILD:module IR-BUILD:builder ptr u8 n -- IR-BUILD:module )
   {: c:IR-CTX:ctx m:IR-BUILD:module b:IR-BUILD:builder p u:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   BND-TAKE
   m BND-MODULE-CK
   0 N-FOLDED !
   c b HIR:REGISTER
   c 0 S-CTX !
   b 0 S-BLD !
   m VIEWS!
   c b p u SOURCE!
   FUN-COUNT 1 <> if E-NLOOP-PLAN throw then
   MKEY 0 IR-ID:PACK-FUN WALK-FUN
   c b IR-BUILD:FREEZE ;

\ How many loops the last rewrite really closed. A caller compares it with what
\ the scan promised, so a walk that quietly folded a different number than the
\ scan counted is a refusal at the caller rather than a module nobody checked.
: FOLDED ( -- n )
   N-FOLDED @ ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;using
;package
