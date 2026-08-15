\ loop.f - the module in which a counted loop that only adds is the arithmetic
\ that loop would have computed. One concern: recognising those loops and
\ writing the module that holds the closed form instead.
\
\ Trip count, measured on this engine rather than derived: start = limit skips
\ the loop at the guard; start < limit runs limit - start turns; start > limit
\ runs ONE turn. A start of the largest integer wraps below the limit, so the
\ start must be a compile-time constant and must not be that maximum.
\
\ With T turns, entry accumulator acc0, K added every turn and the index added m
\ times a turn, the loop leaves acc0 + K*T + m*(start*T + T*(T-1)/2), in
\ wrapping 64-bit arithmetic, which is what the loop itself does.
\
\ T*(T-1) overflows long before T*(T-1)/2 does, so the halving happens BEFORE
\ the multiply: T*(T-1)/2 = (T >> 1) * (T - 1 + (T & 1)), exact for both parities.
\
\ Deleting the header, the exit stub and the latch is sound only because the
\ recogniser proves nothing else reaches them, nothing outside reads their
\ values, and the body holds no store, no call and nothing that may trap.
\
\ One loop per function, and it is the first one recognised.

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
\ One slot per member of the source operation family, so a member added to
\ HIR:opcode fails to compile here until it has a slot and a rebuild rule.
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

\ This pass writes one key of its own and COPIES every one the elaborator built;
\ a field copied under the wrong key would be a call reaching the wrong routine.
5 constant KEYS-N
0 constant K-VALUE
1 constant K-ENTRY
2 constant K-IN
3 constant K-OUT
4 constant K-ADDR

0 constant BOUND-NO
1 constant BOUND-YES

\ A name is copied out of the old module's interner and interned into the new
\ one, because the two modules number their symbols separately.
128 constant NAME-CAP

\ Addends is this pass's own: how many loop-invariant values one turn may add.
NFROZEN:VMAX constant VMAX
NFROZEN:BMAX constant BMAX
32 constant INV-MAX

\ The header's own size, so it is bounded by what fits in a block.
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
\ Every row is written by the scan and read by the rewrite; by the time one
\ operation is copied the questions are all answered.
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
variable P-MOV-N                     \ how many operations the pre-header takes off the body

INV-MAX TYPED-BUFFER P-INV IR-ID:ir-value-id
create P-NEW BMAX cells allot        \ old block ordinal -> new ordinal, or -1 when dropped
create P-COV COV-MAX cells allot     \ scratch: which operations of the header are accounted for
create P-FIX COV-MAX cells allot     \ scratch: which of them cannot change with the turn
create P-MOV COV-MAX cells allot     \ scratch: which of THOSE the pre-header really takes
create P-THRU COV-MAX cells allot    \ scratch: which carried positions leave as a moved answer

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

\ A frozen module carries no attribute under a key its schema did not declare,
\ so this refusal is fail-closed rather than reachable.
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
\ A value defined inside the deleted loop binds NOTHING here, so a reader the
\ recogniser failed to account for reaches an unset slot and the rewrite fails.
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

\ The two modules number their types separately, so a value's class is carried
\ across by identity and not by ordinal.
: TYPE-OF ( IR-ID:ir-value-id -- IR-ID:ir-type-id )
   {: id:IR-ID:ir-value-id :}
   id VALUE-TYPE-AT {: t:IR-ID:ir-type-id :}
   t 0 BND-CELL @ SAME-TYPE? if CTX BLD HIR:CELL-TYPE exit then
   t 0 BND-REAL @ SAME-TYPE? if CTX BLD HIR:REAL-TYPE exit then
   t 0 BND-MEM @ SAME-TYPE? if CTX BLD HIR:MEM-TYPE exit then
   E-NLOOP-SHAPE throw ;

\ ---- the block ordinals of one function --------------------------------------
\ A successor names a block by its ordinal in the MODULE and this pass renumbers
\ blocks, so every successor it writes goes through the plan's table.
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

\ Every reader of a literal here goes through this, so "is this a compile-time
\ number" is one question asked in one place.
: CONST-VALUE ( IR-ID:ir-op-id -- n bool )
   {: id:IR-ID:ir-op-id :}
   id O-CONST OP-IS? 0= if 0 false exit then
   id RESULTS-OF 1 <> if 0 false exit then
   id K-VALUE ATTR-BY-KEY ;

\ ANY of an operation's results answers: a load leaves the cell AND the memory
\ order, and reading "not of this block" for one would hide what the block computes.
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

\ The whole function, so a question about a value defined OUTSIDE the loop can be
\ asked. A block argument answers "no operation", which is a real answer.
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
\ Every terminator asked for its successors, which is the only authority on what
\ reaches what - and what makes deleting the loop's blocks sound.
: EDGES-INTO ( IR-ID:ir-fun-id n -- n )
   {: f:IR-ID:ir-fun-id b:n :}
   0
   f BLOCK-COUNT 0 ?do
      f i BLOCK-AT TERM-AT {: t:IR-ID:ir-op-id :}
      t SUCCS-OF 0 ?do
         t i SUCC-ORD b = if 1+ then
      loop
   loop ;

\ A block named twice by the SAME terminator counts twice, so a two-way branch
\ to one block answers -1 rather than naming it.
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

\ The elaborator opens a header with the live vector, then the counters, then
\ crossing locals, then the memory order, so the counters are last only for a
\ body that binds no local and touches no memory. This numbers everything else.
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

\ ---- the work one turn repeats -----------------------------------------------
\ A tree of constants is one number spread over operations, so a turn that adds
\ one is DECLINED rather than moved: moving it would put arithmetic above the
\ loop that one literal expresses. Folding such a tree is the missing capability.
0 constant FIX-NO
1 constant FIX-NUMBER
2 constant FIX-OUTSIDE

: FIX@ ( n -- n )         cells P-FIX + @ ;
: FIXED? ( n -- bool )    FIX@ FIX-NO <> ;
: WORK? ( n -- bool )     FIX@ FIX-OUTSIDE = ;
: MOVED? ( n -- bool )    cells P-MOV + @ 0<> ;
: THRU? ( n -- bool )     cells P-THRU + @ 0<> ;

: SLOT-CK ( n -- n )
   {: k:n :}
   k 0 < k COV-MAX >= or if E-NLOOP-CAP throw then
   k ;

: FIX! ( n n -- )
   {: k:n c:n :}
   c  k SLOT-CK cells P-FIX +  ! ;

: THRU! ( n -- )    1 swap SLOT-CK cells P-THRU + ! ;

: MOV! ( n -- )
   {: k:n :}
   k MOVED? if exit then
   1 k SLOT-CK cells P-MOV + !
   P-MOV-N @ 1+ P-MOV-N ! ;

\ A block bigger than the three tables hold is DECLINED, for COV-INIT?'s reason.
: FIX-INIT? ( IR-ID:ir-block-id -- bool )
   {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT {: n:n :}
   bk ARG-COUNT {: an:n :}
   n COV-MAX > an COV-MAX > or if false exit then
   0 P-MOV-N !
   n 0 ?do  i FIX-NO FIX!  0 i cells P-MOV + !  loop
   an 0 ?do  0 i cells P-THRU + !  loop
   true ;

\ ---- what the schema says an operation does ----------------------------------
\ The effect class and whether an operation may trap are fields of the schema
\ the module carries, so this is READ rather than kept as a list of opcodes.
: EFFECT-QUIET? ( IR-SCHEMA:effect -- bool )
   MATCH IR-SCHEMA:effect
      pure       OF true  ENDOF
      read       OF true  ENDOF
      write      OF false ENDOF
      read-write OF false ENDOF
   ;MATCH ;

: EFFECT-MEM? ( IR-SCHEMA:effect -- bool )
   MATCH IR-SCHEMA:effect
      pure       OF false ENDOF
      read       OF true  ENDOF
      write      OF true  ENDOF
      read-write OF true  ENDOF
   ;MATCH ;

: OP-QUIET? ( IR-ID:ir-op-id -- bool )
   OPCODE-AT {: sym:IR-ID:ir-symbol-id :}
   V-SCHR VW sym IR-SCHEMA:FTRAPS? if false exit then
   V-SCHR VW sym IR-SCHEMA:FEFFECT@ EFFECT-QUIET? ;

: OP-MEMORY? ( IR-ID:ir-op-id -- bool )
   OPCODE-AT {: sym:IR-ID:ir-symbol-id :}
   V-SCHR VW sym IR-SCHEMA:FEFFECT@ EFFECT-MEM? ;

\ Told apart from the numbers a program computes by its TYPE, which is how the
\ elaborator tells them apart too.
: MEM-VALUE? ( IR-ID:ir-value-id -- bool )
   VALUE-TYPE-AT  0 BND-MEM @  SAME-TYPE? ;

\ ---- which operations cannot change with the turn ----------------------------
: OP-READS? ( IR-ID:ir-op-id IR-ID:ir-value-id -- bool )
   {: id:IR-ID:ir-op-id v:IR-ID:ir-value-id :}
   false
   id OPERANDS-OF 0 ?do
      id i OPERAND-AT v SAME-VALUE? if drop true leave then
   loop ;

: STAYING-READS? ( IR-ID:ir-block-id IR-ID:ir-value-id -- bool )
   {: hb:IR-ID:ir-block-id v:IR-ID:ir-value-id :}
   false
   hb OP-COUNT 0 ?do
      i FIXED? 0= if
         hb i OP-AT v OP-READS? if drop true leave then
      then
   loop ;

: MOVED-READS? ( IR-ID:ir-block-id IR-ID:ir-value-id -- bool )
   {: hb:IR-ID:ir-block-id v:IR-ID:ir-value-id :}
   false
   hb OP-COUNT 0 ?do
      i MOVED? if
         hb i OP-AT v OP-READS? if drop true leave then
      then
   loop ;

\ It holds the value the pre-header handed over on every turn.
: ARG-KEPT? ( IR-ID:ir-fun-id n -- bool )
   {: f:IR-ID:ir-fun-id a:n :}
   f P-LA @ BLOCK-AT TERM-AT a OPERAND-AT
   f P-H @ BLOCK-AT a ARG-AT
   SAME-VALUE? ;

\ A value from OUTSIDE the header, and an argument the loop hands back
\ untouched, are both values the pre-header holds, and reading either is work.
: VAL-FIX ( IR-ID:ir-fun-id IR-ID:ir-block-id IR-ID:ir-value-id -- n )
   {: f:IR-ID:ir-fun-id hb:IR-ID:ir-block-id v:IR-ID:ir-value-id :}
   hb v ARG-INDEX {: a:n :}
   a 0 >= if f a ARG-KEPT? if FIX-OUTSIDE else FIX-NO then exit then
   hb v DEF-INDEX {: d:n :}
   d 0 < if FIX-OUTSIDE exit then
   d FIX@ ;

\ The ORDER is left out on purpose: a read's order operand is the previous
\ read's answer, so requiring it unchanging is a circle that never starts.
: OP-FIX ( IR-ID:ir-fun-id IR-ID:ir-block-id IR-ID:ir-op-id -- n )
   {: f:IR-ID:ir-fun-id hb:IR-ID:ir-block-id id:IR-ID:ir-op-id :}
   id OP-QUIET? 0= if FIX-NO exit then
   FIX-NUMBER
   id OPERANDS-OF 0 ?do
      id i OPERAND-AT MEM-VALUE? 0= if
         f hb  id i OPERAND-AT  VAL-FIX {: c:n :}
         c FIX-NO = if drop FIX-NO leave then
         c FIX-OUTSIDE = if drop FIX-OUTSIDE then
      then
   loop ;

: FIX-ROUND ( IR-ID:ir-fun-id -- bool )
   {: f:IR-ID:ir-fun-id :}
   f P-H @ BLOCK-AT {: hb:IR-ID:ir-block-id :}
   false
   hb OP-COUNT 1- 0 ?do
      i FIXED? 0= if
         f hb  hb i OP-AT  OP-FIX {: c:n :}
         c FIX-NO <> if i c FIX! drop true then
      then
   loop ;

\ Licensed by nothing in the loop WRITING; then a read answers the same bytes
\ every turn. Leaving one behind would leave the order threaded through the body.
: MEM-GROUP? ( IR-ID:ir-block-id -- bool )
   {: hb:IR-ID:ir-block-id :}
   true
   hb OP-COUNT 0 ?do
      hb i OP-AT OP-MEMORY? if
         i FIXED? 0= if drop false leave then
      then
   loop ;

: FIX-CLEAR ( IR-ID:ir-block-id -- )
   OP-COUNT 0 ?do  i FIX-NO FIX!  loop ;

\ Asked again until nothing more answers yes; joining an operation fixes its
\ results for the next round. Nothing is refused here - an empty set is fine.
: PLAN-FIX ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f P-H @ BLOCK-AT {: hb:IR-ID:ir-block-id :}
   hb OP-COUNT 0 ?do
      f FIX-ROUND 0= if leave then
   loop
   hb MEM-GROUP? 0= if hb FIX-CLEAR then ;

\ Exactly one operation, its terminator, and no arguments of its own.
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
\ Read off the header's own terminator and the edges into it, every one checked
\ rather than taken from the shape the elaborator usually builds.
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

\ Exactly two edges into the header is what says the loop is entered from one
\ place and gone round from one place.
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

\ `brz` takes its SECOND successor when the tested value is not zero, so control
\ reaches the pre-header exactly when limit and start differ.
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
\ Everything this writes is read by the recurrence walk, and everything it
\ checks is a sentence the closed form depends on.
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
   hb FIX-INIT? 0= if false exit then
   hb OP-COUNT 1- COV!
   true ;

\ The index is the value the loop's addition adds one to and the limit is what
\ the comparison holds the sum against, so both are read off the operations that
\ count. They are adjacent, index first, because one edge hands the pair over.
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

\ Each is found from the operand of the operation above it, so what is checked
\ is that the four really are one counted step.
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

\ Exactly one position of the live vector may change, and the exit stub must
\ hand the join the same list the latch hands the header.
-1 constant ACC-NONE
-2 constant ACC-MANY

: ACC-SEEN ( n n -- n )
   {: sofar:n at:n :}
   sofar ACC-NONE <> if ACC-MANY exit then
   at ;

\ A position the loop changes that is NOT an accumulator: the latch hands on the
\ answer of moved work and nothing left in the body reads its argument.
: POS-THRU? ( IR-ID:ir-fun-id n -- bool )
   {: f:IR-ID:ir-fun-id a:n :}
   f P-H @ BLOCK-AT {: hb:IR-ID:ir-block-id :}
   hb  f P-LA @ BLOCK-AT TERM-AT a OPERAND-AT  DEF-INDEX {: d:n :}
   d 0 < if false exit then
   d FIXED? 0= if false exit then
   hb  hb a ARG-AT  STAYING-READS? 0= ;

\ The accumulator's new value where the loop accumulates, the moved answer where
\ it hands one through, and the header's own argument everywhere else.
: ACC-EXPECT ( IR-ID:ir-op-id IR-ID:ir-block-id n n -- IR-ID:ir-value-id )
   {: lat:IR-ID:ir-op-id hb:IR-ID:ir-block-id k:n i:n :}
   i k =  i THRU?  or if lat i CARRY-ARG OPERAND-AT exit then
   hb i CARRY-ARG ARG-AT ;

: PLAN-ACC? ( IR-ID:ir-fun-id -- bool )
   {: f:IR-ID:ir-fun-id :}
   f P-H @ BLOCK-AT {: hb:IR-ID:ir-block-id :}
   f P-LA @ BLOCK-AT TERM-AT {: lat:IR-ID:ir-op-id :}
   f P-XT @ BLOCK-AT TERM-AT {: xtt:IR-ID:ir-op-id :}
   ACC-NONE
   P-A @ 0 ?do
      lat i CARRY-ARG OPERAND-AT  hb i CARRY-ARG ARG-AT  SAME-VALUE? 0= if
         f i CARRY-ARG POS-THRU? if i THRU! else i ACC-SEEN then
      then
   loop {: k:n :}
   k 0 < if false exit then
   k P-K !
   true
   P-A @ 0 ?do
      xtt i OPERAND-AT  lat hb k i ACC-EXPECT  SAME-VALUE? 0= if drop false leave then
   loop ;

\ ---- what one turn adds ------------------------------------------------------
\ Each step must be an addition whose LEFT operand carries the chain on, which
\ is what `acc x +` builds; the other way round is declined rather than searched.
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

\ A number the block builds is FOLDED at compile time and not moved: four
\ additions of one become one number, where moving them would leave the arms
\ four values to add. That is why every loop that already folded folds the same.
: CHAIN-ADDEND ( IR-ID:ir-block-id IR-ID:ir-value-id -- )
   {: hb:IR-ID:ir-block-id v:IR-ID:ir-value-id :}
   v  hb P-IDX @ ARG-AT  SAME-VALUE? if P-M @ 1+ P-M ! exit then
   hb v ARG-INDEX 0 >= if CH-NO CH-STATE ! exit then
   hb v DEF-INDEX {: d:n :}
   d 0 < if v CHAIN-INV+ exit then
   hb d OP-AT CONST-VALUE {: val:n ok:bool :}
   ok if
      P-KCONST @ val + P-KCONST !
      d COV!
      exit
   then
   d WORK? 0= if CH-NO CH-STATE ! exit then
   v CHAIN-INV+ ;

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

\ ---- which of the unchanging operations the pre-header really takes ----------
\ has not been asked for yet, it joins the set and the answer says the set grew.
\ An operation nothing asks for is not moved: it would then be an operation no
\ rule accounted for, which PLAN-COVER? declines.
: MOVE-ASK ( IR-ID:ir-block-id IR-ID:ir-value-id -- bool )
   {: hb:IR-ID:ir-block-id v:IR-ID:ir-value-id :}
   hb v DEF-INDEX {: d:n :}
   d 0 < if false exit then
   d FIXED? 0= if false exit then
   d MOVED? if false exit then
   d MOV!
   true ;

: MOVE-SEEDS ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f P-H @ BLOCK-AT {: hb:IR-ID:ir-block-id :}
   f P-LA @ BLOCK-AT TERM-AT {: lat:IR-ID:ir-op-id :}
   P-INV-N @ 0 ?do  hb  i P-INV @  MOVE-ASK drop  loop
   P-A @ 0 ?do
      i THRU? if  hb  lat i CARRY-ARG OPERAND-AT  MOVE-ASK drop  then
   loop ;

: MOVE-OP ( IR-ID:ir-block-id IR-ID:ir-op-id -- bool )
   {: hb:IR-ID:ir-block-id id:IR-ID:ir-op-id :}
   false
   id OPERANDS-OF 0 ?do
      hb  id i OPERAND-AT  MOVE-ASK if drop true then
   loop ;

: MOVE-ROUND ( IR-ID:ir-block-id -- bool )
   {: hb:IR-ID:ir-block-id :}
   false
   hb OP-COUNT 0 ?do
      i MOVED? if
         hb  hb i OP-AT  MOVE-OP if drop true then
      then
   loop ;

\ Asked again because the EMISSION rests on it: an operand of any other kind
\ would be copied into the pre-header naming a value that block cannot see.
: MOVE-OPERAND-OK? ( IR-ID:ir-fun-id IR-ID:ir-block-id IR-ID:ir-value-id -- bool )
   {: f:IR-ID:ir-fun-id hb:IR-ID:ir-block-id v:IR-ID:ir-value-id :}
   hb v ARG-INDEX {: a:n :}
   a 0 >= if  f a ARG-KEPT?  f a POS-THRU?  or exit then
   hb v DEF-INDEX {: d:n :}
   d 0 < if true exit then
   d MOVED? ;

: MOVE-CLOSED-OP? ( IR-ID:ir-fun-id IR-ID:ir-block-id IR-ID:ir-op-id -- bool )
   {: f:IR-ID:ir-fun-id hb:IR-ID:ir-block-id id:IR-ID:ir-op-id :}
   true
   id OPERANDS-OF 0 ?do
      f hb  id i OPERAND-AT  MOVE-OPERAND-OK? 0= if drop false leave then
   loop ;

: PLAN-MOVE? ( IR-ID:ir-fun-id -- bool )
   {: f:IR-ID:ir-fun-id :}
   f P-H @ BLOCK-AT {: hb:IR-ID:ir-block-id :}
   f MOVE-SEEDS
   hb OP-COUNT 0 ?do
      hb MOVE-ROUND 0= if leave then
   loop
   true
   hb OP-COUNT 0 ?do
      i MOVED? if
         f hb  hb i OP-AT  MOVE-CLOSED-OP? 0= if drop false leave then
      then
   loop ;

\ The coverage check the soundness argument rests on: a store, a load the
\ addressing keeps inside, a call, a trap or a second accumulator is declined.
: PLAN-COVER? ( IR-ID:ir-fun-id -- bool )
   {: f:IR-ID:ir-fun-id :}
   f P-H @ BLOCK-AT {: hb:IR-ID:ir-block-id :}
   true
   hb OP-COUNT 0 ?do
      i COVERED?  i MOVED?  or 0= if drop false leave then
   loop ;

\ ---- the start, which has to be a number this pass can read ------------------
\ The one start the trip-count table has no row for: there index + 1 wraps below
\ the limit and the loop runs round nearly the whole range.
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
\ Asked while recognising rather than raised during the rewrite, so a function
\ too big for the value map is compiled exactly as it was.
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
   f PLAN-FIX
   f PLAN-ACC? 0= if exit then
   f PLAN-CHAIN? 0= if exit then
   f PLAN-MOVE? 0= if exit then
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

\ Each takes the old operation whose span it stands at, so a diagnostic about
\ the closed form points at the loop it replaced.
: MK2 ( IR-ID:ir-op-id HIR:opcode IR-ID:ir-value-id IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: sp:IR-ID:ir-op-id o:HIR:opcode a:IR-ID:ir-value-id b:IR-ID:ir-value-id :}
   sp o OPEN
   a OPERAND+
   b OPERAND+
   CELL-RESULT+
   CLOSE 0 RESULT@ ;

\ Every constant this pass mints is an ORDINARY number: it rewrites control flow
\ and never invents an address.
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

\ Carried across by its NEW ordinal, so a branch to a deleted block is a refusal
\ and never a branch to whatever now stands there.
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

\ ---- the work the pre-header takes off the body ------------------------------
\ What such an argument holds on every turn is the value the pre-header was
\ handing the header, so it is bound to the pre-header's operand.
: BIND-MOVED-ARGS ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f P-H @ BLOCK-AT {: hb:IR-ID:ir-block-id :}
   f P-PR @ BLOCK-AT TERM-AT {: pt:IR-ID:ir-op-id :}
   hb ARG-COUNT 0 ?do
      hb  hb i ARG-AT  MOVED-READS? if
         hb i ARG-AT  pt i OPERAND-AT VOF  VBIND
      then
   loop ;

\ The body's own order is legal here because the set is closed under what each
\ operation reads.
: EMIT-MOVED ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f BIND-MOVED-ARGS
   f P-H @ BLOCK-AT {: hb:IR-ID:ir-block-id :}
   hb OP-COUNT 0 ?do
      i MOVED? if hb i OP-AT COPY-OP then
   loop ;

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

\ Staged in the pre-header because both arms read it. Constants the loop built
\ in its own header are added up here at compile time rather than copied out.
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

\ One of T and T-1 is even, so the halving happens before the multiply and the
\ product that would have overflowed never exists.
: EMIT-HALF ( IR-ID:ir-op-id IR-ID:ir-value-id IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: sp:IR-ID:ir-op-id t:IR-ID:ir-value-id one:IR-ID:ir-value-id :}
   sp HIR-OPCODE:SUB    t one MK2 {: tm1:IR-ID:ir-value-id :}
   sp HIR-OPCODE:RSHIFT t one MK2 {: hlf:IR-ID:ir-value-id :}
   sp HIR-OPCODE:AND    t one MK2 {: par:IR-ID:ir-value-id :}
   sp HIR-OPCODE:ADD    tm1 par MK2 {: p:IR-ID:ir-value-id :}
   sp HIR-OPCODE:MUL    hlf p MK2 ;

\ The two multiplications a compile-time zero or one would make pointless are
\ not staged at all: the start is a constant read and m a count made here.
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
\ The arm's own answer at the accumulator's position, and the value the
\ pre-header was handing the header at every other.
: ARM-OPERAND ( IR-ID:ir-op-id IR-ID:ir-op-id n -- IR-ID:ir-value-id )
   {: pt:IR-ID:ir-op-id lat:IR-ID:ir-op-id i:n :}
   i P-K @ = if 0 W-ACC @ exit then
   i THRU? if lat i CARRY-ARG OPERAND-AT VOF exit then
   pt i CARRY-ARG OPERAND-AT VOF ;

: ARM-TERM ( IR-ID:ir-fun-id IR-ID:ir-op-id -- )
   {: f:IR-ID:ir-fun-id sp:IR-ID:ir-op-id :}
   f P-PR @ BLOCK-AT TERM-AT {: pt:IR-ID:ir-op-id :}
   f P-LA @ BLOCK-AT TERM-AT {: lat:IR-ID:ir-op-id :}
   sp HIR-OPCODE:BR OPEN
   P-A @ 0 ?do
      pt lat i ARM-OPERAND OPERAND+
   loop
   P-JN @ SUCC+
   CLOSE drop ;

\ The formula at T = 1, where both products are gone.
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

\ The trip count is the guard's own subtraction, REUSED rather than recomputed:
\ the guard dominates this arm.
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
\ The comparison and the branch that reads it are the pair select.f fuses into
\ one compare-and-branch, so the test costs no register.
: EMIT-PRE ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f P-PR @ BLOCK-AT {: pb:IR-ID:ir-block-id :}
   pb TERM-AT {: pt:IR-ID:ir-op-id :}
   pb OPEN-BLOCK
   f EMIT-MOVED
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
\ The only moment a module can be asked its operation, key and type identities,
\ because its symbols and types are its own ordinals.
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
\ Asked before anything is built. A caller that gets zero keeps the module it
\ has, which is what keeps every routine without a foldable loop byte-for-byte.
\ More than one function is DECLINED: a block's ordinal within its function is
\ its ordinal within the module only while there is one.
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
\ The bytes are the source text the old module was compiled from, proved by
\ digest before any span is carried across.
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

\ A caller compares it with what the scan promised, so a walk that folded a
\ different number is a refusal rather than a module nobody checked.
: FOLDED ( -- n )
   N-FOLDED @ ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;using
;package
