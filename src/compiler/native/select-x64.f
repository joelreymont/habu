\ select-x64.f - instruction selection for x86-64: read a frozen HIR module and
\ build the frozen X64IR module its operations select to. It is to
\ src/compiler/native/x64ir.f what src/compiler/native/select.f is to a64ir.f,
\ and it is a SEPARATE pass and not a parameterisation of that one, for the
\ reason docs/x86-64.md gives for the emitter: what the two passes share is the
\ walk, and what they do not share is every rule in it.
\
\ WHAT THIS PASS LOWERS, AND WHAT IT LEAVES TO THE NEXT SLICE. The straight-line
\ and branching core: literals, the two-address arithmetic and its immediate
\ forms, the shifts whose count is a literal, the addressed loads and stores,
\ the comparisons, the two branches, the data-stack boundary of a routine, a
\ call, a tail call, the frame and the return. It is CORRECT AND UNFUSED: a
\ comparison that feeds a branch selects to `x64.cmpset` and then `x64.brz`,
\ never to `x64.cmpbr`, and no selection becomes a `x64.cmpsel`. Fusing and
\ if-conversion are a rewrite of what this pass builds and not a condition of
\ its correctness, so they are the next slice; the ARM64 pass does them inline
\ only because it grew that way.
\
\ THE THREE THINGS THIS MACHINE MAKES THE SELECTOR DO DIFFERENTLY.
\
\ ONE. IT NEVER INSERTS A COPY FOR A TWO-ADDRESS FORM. `add rd, rs` destroys rd,
\ and the dialect says so with a schema TIE. A tie the allocator cannot satisfy
\ is a copy the ALLOCATOR inserts, so a selector that inserted one here would be
\ making the decision twice.
\
\ TWO. A LITERAL IN THE SECOND OPERAND IS PART OF THE INSTRUCTION. x86-64 ALU
\ and compare forms carry a signed imm32, so `8 +` is one instruction and not
\ two. The literal's own `x64.movi` is left out only when EVERY use of it is one
\ of these, which is counted over the whole function and not guessed at.
\
\ THREE. TWO FORMS NAME A REGISTER AND THIS PASS CANNOT YET ASK FOR IT. `shl
\ r64, cl` reads its count from rcx and `idiv` divides rdx:rax; the routine
\ contract has no way to fix an operand to a register, so a variable shift count
\ and a division are REFUSED by name (E-X64SEL-FIXED) rather than lowered into a
\ form whose register the allocator would hand out to something else. A shift by
\ a literal has no such constraint and is selected. The follow-up is the
\ register-constraint dot on the allocator; until it lands, a Habu program that
\ divides or shifts by a computed count has no x86-64 lowering and says so.
\
\ AND ONE THE DIALECT MAKES: THERE IS NO FLOATING FORM AT ALL. x64ir declares no
\ SSE operation, so every floating source operation - including the four
\ conversions - is refused with E-X64SEL-FLOAT instead of being lowered wrongly.
\
\ WHERE THE POINTER STANDS. The data-stack pointer is a register and stands at
\ ONE place for the whole body. This pass does not survey the body for the
\ cheapest place the way the ARM64 one does: it stands at the routine's entry
\ base, which costs one adjustment at each end and none in between, except in a
\ routine that leaves through its callee, where the tail branch carries no
\ adjustment at all and the pointer therefore has to stand where it was entered.
\ A cell already pushed is then at a NEGATIVE displacement, which this machine
\ addresses as readily as a positive one (x64ir.f DSLOT).

require lib/prelude.f
require lib/errors.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/symbol.f
require src/compiler/ir/type.f
require src/compiler/ir/source.f
require src/compiler/ir/schema.f
require src/compiler/ir/fun.f
require src/compiler/ir/build.f
require src/compiler/native-effect.f
require src/compiler/native/machine.f
require src/compiler/native/hir.f
require src/compiler/native/x64ir.f
require src/compiler/native/frozen.f
require src/compiler/native/dict.f
require src/compiler/native/trap.f
require src/arch/x86-64/machine.f

package X64SEL
using NFROZEN
private

\ ---- the scratch this pass retains -------------------------------------------
variable SCRATCH-VALUES
variable SCRATCH-BLOCKS
: VMAX ( -- n ) SCRATCH-VALUES @ ;
: BMAX ( -- n ) SCRATCH-BLOCKS @ ;

: SCRATCH-SIZES! ( -- )
   NFROZEN:VALUE-COUNT 1 max SCRATCH-VALUES !
   NFROZEN:TOTAL-BLOCKS 1 max SCRATCH-BLOCKS ! ;

DYNAMIC-BUFFER VMAP IR-ID:ir-value-id
DYNAMIC-BUFFER VSET-BUF n
: VSET ( -- ptr n ) 0 VSET-BUF ;
DYNAMIC-BUFFER D-ORDER IR-ID:ir-value-id
DYNAMIC-BUFFER D-ORDER-SET-BUF n
: D-ORDER-SET ( -- ptr n ) 0 D-ORDER-SET-BUF ;

: RESERVE-SCRATCH ( -- )
   SCRATCH-SIZES!
   VMAX VMAP-RESERVE
   VMAX VSET-BUF-RESERVE
   BMAX D-ORDER-RESERVE
   BMAX D-ORDER-SET-BUF-RESERVE ;

\ ---- the bound source dialect ------------------------------------------------
HIR-OPCODE:CONST    HIR:ORD constant O-CONST
HIR-OPCODE:RETURN   HIR:ORD constant O-RETURN
HIR-OPCODE:ADD      HIR:ORD constant O-ADD
HIR-OPCODE:SUB      HIR:ORD constant O-SUB
HIR-OPCODE:AND      HIR:ORD constant O-AND
HIR-OPCODE:OR       HIR:ORD constant O-OR
HIR-OPCODE:XOR      HIR:ORD constant O-XOR
HIR-OPCODE:LSHIFT   HIR:ORD constant O-LSHIFT
HIR-OPCODE:RSHIFT   HIR:ORD constant O-RSHIFT
HIR-OPCODE:LT       HIR:ORD constant O-LT
HIR-OPCODE:LE       HIR:ORD constant O-LE
HIR-OPCODE:GT       HIR:ORD constant O-GT
HIR-OPCODE:GE       HIR:ORD constant O-GE
HIR-OPCODE:EQUAL    HIR:ORD constant O-EQUAL
HIR-OPCODE:NE       HIR:ORD constant O-NE
HIR-OPCODE:BRZ      HIR:ORD constant O-BRZ
HIR-OPCODE:CALL     HIR:ORD constant O-CALL
HIR-OPCODE:WORDCALL HIR:ORD constant O-WORDCALL
HIR-OPCODE:TRAP     HIR:ORD constant O-TRAP

0 constant BOUND-NO
1 constant BOUND-YES

128 constant NAME-CAP
3 constant TRAP-CELLS                \ the address, the length and the exit code
64 constant EDGE-MAX                 \ operands one edge may carry

here CELL 1- and CELL swap - CELL 1- and allot
variable BND-MODE
BOUND-NO BND-MODE !

1 TYPED-BUFFER BND-MOD IR-ID:ir-module-id
HIR:OPCODES TYPED-BUFFER BND-OP IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-VAL IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-ADDR IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-FUN IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-ENTRY IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-IN IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-OUT IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-MEM IR-ID:ir-type-id

\ ---- what one selection run is working on ------------------------------------
1 TYPED-BUFFER S-CTX IR-CTX:ctx
1 TYPED-BUFFER S-BLD IR-BUILD:builder
1 TYPED-BUFFER S-SID IR-ID:ir-source-id
1 TYPED-BUFFER S-ACC IR-ID:ir-value-id
1 TYPED-BUFFER S-TOK IR-ID:ir-value-id
1 TYPED-BUFFER S-ARGS NEFF:placeseq
1 TYPED-BUFFER S-OUTS NEFF:placeseq
1 TYPED-BUFFER S-TRT NEFF:traits
1 TYPED-BUFFER S-FTOK IR-ID:ir-value-id
1 TYPED-BUFFER S-FUN IR-ID:ir-fun-id
1 TYPED-BUFFER S-BLK IR-ID:ir-block-id
EDGE-MAX TYPED-BUFFER EDGE-V IR-ID:ir-value-id
variable S-FRAME                     \ the frame the contract declares, in bytes
variable S-DECL-IN                   \ how many values the contract declares the emission takes
variable S-DECL-OUT                  \ and how many it declares it leaves
variable S-TAIL                      \ whether control leaves through a callee
variable S-DSTACK                    \ whether the data-stack convention is declared
variable N-CALLS
variable N-TAILS
variable TAIL-SITE                   \ the operation this function leaves through, or -1
variable D-POS                       \ where the body's data-stack pointer stands
variable V-BASE                      \ first value owned by the function being selected
variable V-LIMIT                     \ one past its last value
variable R-BASE                      \ where this function's blocks start in the module
variable R-NEWBASE                   \ and where they start in the module being built

here CELL 1- and CELL swap - CELL 1- and allot
create NAMEBUF NAME-CAP allot

\ ---- the slots, read back ----------------------------------------------------
: CTX ( -- IR-CTX:ctx )              0 S-CTX @ ;
: BLD ( -- IR-BUILD:builder )        0 S-BLD @ ;
: SID ( -- IR-ID:ir-source-id )      0 S-SID @ ;
: ACC ( -- IR-ID:ir-value-id )       0 S-ACC @ ;
: ACC! ( IR-ID:ir-value-id -- )      0 S-ACC ! ;
: TOK ( -- IR-ID:ir-value-id )       0 S-TOK @ ;
: TOK! ( IR-ID:ir-value-id -- )      0 S-TOK ! ;
: ARGS ( -- NEFF:placeseq )          0 S-ARGS @ ;
: OUTS ( -- NEFF:placeseq )          0 S-OUTS @ ;
: TRAITS ( -- NEFF:traits )          0 S-TRT @ ;
: TAIL? ( -- bool )                  S-TAIL @ 0<> ;
: FRAME ( -- n )                     S-FRAME @ ;
: FTOK ( -- IR-ID:ir-value-id )      0 S-FTOK @ ;
: FTOK! ( IR-ID:ir-value-id -- )     0 S-FTOK ! ;
: FUN ( -- IR-ID:ir-fun-id )         0 S-FUN @ ;
: BLK ( -- IR-ID:ir-block-id )       0 S-BLK @ ;

\ ---- the source dialect's opcode family --------------------------------------
\ A symbol naming no member is an operation this pass has no rule for.
: OPCODE-SLOT ( IR-ID:ir-symbol-id -- n )
   {: sym:IR-ID:ir-symbol-id :}
   -1
   HIR:OPCODES 0 ?do
      sym i BND-OP @ SAME-SYM? if drop i leave then
   loop
   dup 0 < if E-X64SEL-OPCODE throw then ;

: OP-SLOT ( IR-ID:ir-op-id -- n )
   OPCODE-AT OPCODE-SLOT ;

\ ---- the value map -----------------------------------------------------------
\ Which value of the NEW module a value of the source module selected to.
: V-RANGE1 ( IR-ID:ir-value-id -- )
   IR-ID:VALUE-LOCAL {: k:n :}
   k V-BASE @ min V-BASE !
   k 1+ V-LIMIT @ max V-LIMIT ! ;

: V-RANGE-OP ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id RESULTS-OF 0 ?do id i RESULT-AT V-RANGE1 loop ;

: V-RANGE-BLOCK ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   bk ARG-COUNT 0 ?do bk i ARG-AT V-RANGE1 loop
   bk OP-COUNT 0 ?do bk i OP-AT V-RANGE-OP loop ;

: V-RANGE! ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   $7FFFFFFF V-BASE !
   -1 V-LIMIT !
   f BLOCK-COUNT 0 ?do f i BLOCK-AT V-RANGE-BLOCK loop
   V-LIMIT @ 0< if 0 V-BASE ! 0 V-LIMIT ! exit then
   V-LIMIT @ V-BASE @ - VMAX > if E-X64SEL-CAP throw then ;

: VCLEAR ( -- )
   VMAX 0 ?do 0 i cells VSET + ! loop ;

: VSLOT ( IR-ID:ir-value-id -- n )
   IR-ID:VALUE-LOCAL V-BASE @ -
   dup 0 < over VMAX >= or if E-X64SEL-CAP throw then ;

: VBIND ( IR-ID:ir-value-id IR-ID:ir-value-id -- )
   {: src:IR-ID:ir-value-id new:IR-ID:ir-value-id :}
   src VSLOT {: k:n :}
   new k VMAP !
   1 k cells VSET + ! ;

: VOF ( IR-ID:ir-value-id -- IR-ID:ir-value-id )
   VSLOT {: k:n :}
   k cells VSET + @ 0= if E-X64SEL-SHAPE throw then
   k VMAP @ ;

\ ---- reading the frozen module -----------------------------------------------
: SRC-CK ( IR-ID:ir-source-id -- )
   IR-ID:SOURCE-LOCAL 0<> if E-X64SEL-SHAPE throw then ;

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

: OPERAND ( IR-ID:ir-op-id n -- IR-ID:ir-value-id )
   OPERAND-AT VOF ;

\ The TYPE the source module gives it, held against the identity this pass
\ interned - never a position or an opcode.
: TOKEN? ( IR-ID:ir-value-id -- bool )
   VALUE-TYPE-AT  0 BND-MEM @  SAME-TYPE? ;

\ ---- staging one machine operation -------------------------------------------
\ Every machine operation carries the span of the source operation it selects
\ from, so a diagnostic points at the source and not at the machine.
: OPEN ( IR-ID:ir-op-id X64IR:opcode -- )
   {: id:IR-ID:ir-op-id o:X64IR:opcode :}
   CTX BLD  CTX BLD o X64IR:ENSURE-OP  IR-BUILD:BEGIN-OP
   CTX BLD  id OP-SPAN  IR-BUILD:SET-OP-SPAN ;

: RESULT+ ( -- )
   CTX BLD  CTX BLD X64IR:GPR-TYPE  IR-BUILD:ADD-RESULT ;

: TOKEN+ ( -- )
   CTX BLD  CTX BLD X64IR:MEM-TYPE  IR-BUILD:ADD-RESULT ;

: OPERAND+ ( IR-ID:ir-value-id -- )
   CTX BLD rot IR-BUILD:ADD-OPERAND ;

: CLOSE-VALUE ( -- )
   CTX BLD IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CTX BLD id 0 IR-BUILD:OP-RESULT@ ACC! ;

\ ---- the attributes an operation of the source carries ------------------------
: ATTR-SLOT-OF ( IR-ID:ir-op-id IR-ID:ir-symbol-id -- n )
   {: id:IR-ID:ir-op-id want:IR-ID:ir-symbol-id :}
   -1
   id ATTRS-OF 0 ?do
      id i ATTR-KEY-AT want SAME-SYM? if drop i leave then
   loop
   dup 0 < if E-X64SEL-ATTR throw then ;

: ATTR-INT-OF ( IR-ID:ir-op-id IR-ID:ir-symbol-id -- n )
   {: id:IR-ID:ir-op-id want:IR-ID:ir-symbol-id :}
   id  id want ATTR-SLOT-OF  ATTR-INT-AT ;

: CONST-VALUE ( IR-ID:ir-op-id -- n )
   0 BND-VAL @ ATTR-INT-OF ;

: CONST-ADDR ( IR-ID:ir-op-id -- n )
   0 BND-ADDR @ ATTR-INT-OF ;

: WORD-ENTRY ( IR-ID:ir-op-id -- n )
   0 BND-ENTRY @ ATTR-INT-OF ;

\ ---- the routine's convention, read once -------------------------------------
: SLOT-POSITIONS ( NEFF:placeseq -- n )
   {: s:NEFF:placeseq :}
   s NEFF:SEQ-SLOTS {: sl:n :}
   sl 0= if 0 exit then
   sl s NEFF:SEQ-LEN <> if E-X64SEL-PLACE throw then
   sl ;

\ THE CONTRACT SAYS SO and this pass does not work it out.
: DSTACK? ( -- bool )
   S-DSTACK @ 0<> ;

: CALLS? ( -- bool )
   TRAITS NEFF:T-CALL NEFF:TRAITS-HAS? ;

\ A frame is the ALLOCATOR's spill room on this machine and nothing else: the
\ return address is on the machine stack, so a call costs this routine no slot.
: FRAMED? ( -- bool )
   FRAME 0<> ;

\ A call site hands the callee its arguments through the caller's data stack, so
\ a contract that declares a call has to declare that convention.
: CONTRACT-CK ( -- )
   CALLS? 0= if exit then
   DSTACK? 0= if E-X64SEL-CALL throw then ;

\ ---- each function's own boundary --------------------------------------------
\ The CONTRACT describes the published word and the MODULE describes every
\ function, so only function zero is held against the contract.
: FUN-SLOTS ( n -- NEFF:placeseq )
   NEFF:SEQ-DSTACK ;

: DECL-CK ( n n -- )
   {: in:n out:n :}
   in S-DECL-IN @ <> if E-X64SEL-PLACE throw then
   out S-DECL-OUT @ <> if E-X64SEL-PLACE throw then ;

: FUN-PLACES! ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id ord:n :}
   DSTACK? 0= if exit then
   f NFROZEN:FUN-ARITY {: in:n out:n :}
   ord 0= if in out DECL-CK then
   in FUN-SLOTS 0 S-ARGS !
   out FUN-SLOTS 0 S-OUTS ! ;

: CALLED-CK ( -- )
   CALLS? if
      N-CALLS @ 0= if E-X64SEL-CALL throw then exit
   then
   N-CALLS @ 0<> if E-X64SEL-CALL throw then ;

: TAILED-CK ( -- )
   TAIL? if
      N-TAILS @ 1 <> if E-X64SEL-TAIL throw then exit
   then
   N-TAILS @ 0<> if E-X64SEL-TAIL throw then ;

\ ---- the four data-stack operations ------------------------------------------
\ Every offset is written in the place the body's pointer stands, which is where
\ the machine addresses it from.
: DPLACED ( n -- n )
   D-POS @ - ;

: DSLOT-ATTR+ ( n -- )
   {: off:n :}
   CTX BLD  CTX BLD X64IR:KEY-DSLOT  CTX BLD off DPLACED X64IR:DSLOT-ATTR
   IR-BUILD:ADD-ATTR ;

\ The mover's own number, already placed by the caller: a move is a distance and
\ not an address.
: DBYTES-AT+ ( n -- )
   {: d:n :}
   CTX BLD  CTX BLD X64IR:KEY-DBYTES  CTX BLD d X64IR:DBYTES-ATTR
   IR-BUILD:ADD-ATTR ;

: DBACK-AT+ ( n -- )
   {: d:n :}
   CTX BLD  CTX BLD X64IR:KEY-DBACK  CTX BLD d X64IR:DBACK-ATTR
   IR-BUILD:ADD-ATTR ;

\ Where the routine's data-stack order is minted. It moves the pointer down over
\ the arguments the caller left, except in a routine that leaves through its
\ callee, where it moves nothing and only mints the order.
: EMIT-DTAKE ( IR-ID:ir-op-id n -- )
   {: at:IR-ID:ir-op-id d:n :}
   at X64IR-OPCODE:DTAKE OPEN
   TOKEN+
   d DBYTES-AT+
   CTX BLD IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CTX BLD id 0 IR-BUILD:OP-RESULT@ TOK! ;

: EMIT-DLOAD ( IR-ID:ir-op-id n -- IR-ID:ir-value-id )
   {: at:IR-ID:ir-op-id off:n :}
   at X64IR-OPCODE:DLOAD OPEN
   TOK OPERAND+
   RESULT+
   TOKEN+
   off DSLOT-ATTR+
   CTX BLD IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CTX BLD id 1 IR-BUILD:OP-RESULT@ TOK!
   CTX BLD id 0 IR-BUILD:OP-RESULT@ ;

: EMIT-DSTORE ( IR-ID:ir-op-id IR-ID:ir-value-id n -- )
   {: at:IR-ID:ir-op-id v:IR-ID:ir-value-id off:n :}
   at X64IR-OPCODE:DSTORE OPEN
   v OPERAND+
   TOK OPERAND+
   TOKEN+
   off DSLOT-ATTR+
   CTX BLD IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CTX BLD id 0 IR-BUILD:OP-RESULT@ TOK! ;

\ The pointer is left one past the results, which is the moment they become the
\ caller's. x86-64 has no write-back addressing, so this is its own instruction
\ where ARM64 rides it on the last store.
: EMIT-DPUBLISH ( IR-ID:ir-op-id n -- )
   {: at:IR-ID:ir-op-id d:n :}
   at X64IR-OPCODE:DPUBLISH OPEN
   TOK OPERAND+
   d DBYTES-AT+
   CTX BLD IR-BUILD:END-OP drop ;

\ ---- the routine's frame -----------------------------------------------------
\ There is no link save and no link load: `call` pushed the return address on
\ the machine stack and `ret` pops it, so the prologue is the reserve alone.
: FRAME-ATTR+ ( n -- )
   {: size:n :}
   CTX BLD  CTX BLD X64IR:KEY-FRAME  CTX BLD size X64IR:FRAME-ATTR
   IR-BUILD:ADD-ATTR ;

: EMIT-RESERVE ( IR-ID:ir-op-id -- )
   {: at:IR-ID:ir-op-id :}
   at X64IR-OPCODE:RESERVE OPEN
   TOKEN+
   FRAME FRAME-ATTR+
   CTX BLD IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CTX BLD id 0 IR-BUILD:OP-RESULT@ FTOK! ;

: EMIT-RELEASE ( IR-ID:ir-op-id -- )
   {: at:IR-ID:ir-op-id :}
   at X64IR-OPCODE:RELEASE OPEN
   FTOK OPERAND+
   FRAME FRAME-ATTR+
   CTX BLD IR-BUILD:END-OP drop ;

: PROLOGUE ( IR-ID:ir-op-id -- )
   {: at:IR-ID:ir-op-id :}
   FRAMED? 0= if exit then
   at EMIT-RESERVE ;

: EPILOGUE ( IR-ID:ir-op-id -- )
   {: at:IR-ID:ir-op-id :}
   FRAMED? 0= if exit then
   at EMIT-RELEASE ;

\ ---- which operation this function leaves through ----------------------------
: TAIL-OP? ( IR-ID:ir-op-id -- bool )
   {: id:IR-ID:ir-op-id :}
   TAIL-SITE @ 0 < if false exit then
   id IR-ID:OP-LOCAL TAIL-SITE @ = ;

: TAIL-HERE? ( -- bool )
   TAIL-SITE @ 0 < if false exit then
   BLK OP-COUNT {: n:n :}
   n 2 < if false exit then
   BLK n 2 - OP-AT TAIL-OP? ;

\ ---- the shape of a call site ------------------------------------------------
\ How many values are live ACROSS the call, held against both lists: the operands
\ are the order, the live values and then the arguments, and the results are the
\ order, the same live values and then the answers.
: CALL-LIVE ( IR-ID:ir-op-id n n -- n )
   {: id:IR-ID:ir-op-id a:n r:n :}
   id OPERANDS-OF 1- a - {: k:n :}
   k 0 < if E-X64SEL-CALL throw then
   id RESULTS-OF 1- r - k <> if E-X64SEL-CALL throw then
   k ;

\ A self-call is RECURSE, which names the DEFINITION and not the body the token
\ stands in, so its shape is the CONTRACT's declaration.
: SELF-SHAPE ( IR-ID:ir-op-id -- n n n n )
   {: id:IR-ID:ir-op-id :}
   S-DECL-IN @ {: a:n :}
   S-DECL-OUT @ {: r:n :}
   id a r CALL-LIVE {: k:n :}
   a r k 0 ;

: WORD-SHAPE ( IR-ID:ir-op-id -- n n n n )
   {: id:IR-ID:ir-op-id :}
   id 0 BND-IN @ ATTR-INT-OF {: a:n :}
   id 0 BND-OUT @ ATTR-INT-OF {: r:n :}
   id a r CALL-LIVE {: k:n :}
   a r k 0 ;

\ The shape of the SITE is not the shape of the operation at the one operation a
\ routine leaves through: nothing is live across a branch it never returns from.
: SITE-SHAPE ( IR-ID:ir-op-id -- n n n n )
   {: id:IR-ID:ir-op-id :}
   id WORD-SHAPE {: a:n r:n kk:n m:n :}
   id TAIL-OP? 0= if a r kk m exit then
   a r 0  kk m + ;

\ Which value goes into which cell is written down once and three readers stand
\ on it: the live values first, then the arguments.
: DSAVE-VAL ( IR-ID:ir-op-id n n n -- IR-ID:ir-value-id )
   {: id:IR-ID:ir-op-id kk:n m:n i:n :}
   i kk < if id i 1+ OPERAND-AT exit then
   id m i + 1+ OPERAND-AT ;

: DBACK-VAL ( IR-ID:ir-op-id n n n -- IR-ID:ir-value-id )
   {: id:IR-ID:ir-op-id kk:n m:n i:n :}
   i kk < if id i 1+ RESULT-AT exit then
   id m i + 1+ RESULT-AT ;

\ ---- selecting a call --------------------------------------------------------
: EMIT-CALL-OP ( IR-ID:ir-op-id n n -- )
   {: at:IR-ID:ir-op-id give:n back:n :}
   at X64IR-OPCODE:CALL OPEN
   TOK OPERAND+
   TOKEN+
   give DBYTES-AT+
   back DBACK-AT+
   CTX BLD IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CTX BLD id 0 IR-BUILD:OP-RESULT@ TOK! ;

: EMIT-WORDCALL-OP ( IR-ID:ir-op-id n n n -- )
   {: at:IR-ID:ir-op-id give:n back:n entry:n :}
   at X64IR-OPCODE:WORDCALL OPEN
   TOK OPERAND+
   TOKEN+
   give DBYTES-AT+
   back DBACK-AT+
   CTX BLD  CTX BLD X64IR:KEY-ENTRY  CTX BLD entry X64IR:ENTRY-ATTR
   IR-BUILD:ADD-ATTR
   CTX BLD IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CTX BLD id 0 IR-BUILD:OP-RESULT@ TOK! ;

\ Every live value and every argument is written to its cell. There is no
\ residency analysis in this slice, so a cell that already held the value is
\ written again; leaving one out is a rewrite over this shape and not a
\ different lowering.
: CALL-SAVE ( IR-ID:ir-op-id n n n -- )
   {: id:IR-ID:ir-op-id kk:n m:n a:n :}
   kk a + {: n:n :}
   n 0 ?do
      id  id kk m i DSAVE-VAL VOF  i X64IR:SLOT-WIDTH *  EMIT-DSTORE
   loop ;

: CALL-RESTORE ( IR-ID:ir-op-id n n n -- )
   {: id:IR-ID:ir-op-id kk:n m:n r:n :}
   kk r + {: n:n :}
   n 0 ?do
      id kk m i DBACK-VAL
      id  i X64IR:SLOT-WIDTH *  EMIT-DLOAD
      VBIND
   loop
   id 0 RESULT-AT  TOK  VBIND
   N-CALLS @ 1+ N-CALLS ! ;

: EMIT-CALL ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   DSTACK? 0= if E-X64SEL-CALL throw then
   id SELF-SHAPE {: a:n r:n kk:n m:n :}
   id 0 OPERAND TOK!
   id kk m a CALL-SAVE
   id  kk a + X64IR:SLOT-WIDTH * DPLACED
       kk r + X64IR:SLOT-WIDTH * DPLACED  EMIT-CALL-OP
   id kk m r CALL-RESTORE ;

: EMIT-WORD-CALL ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   DSTACK? 0= if E-X64SEL-CALL throw then
   id SITE-SHAPE {: a:n r:n kk:n m:n :}
   id 0 OPERAND TOK!
   id kk m a CALL-SAVE
   id  kk a + X64IR:SLOT-WIDTH * DPLACED
       kk r + X64IR:SLOT-WIDTH * DPLACED
   id WORD-ENTRY EMIT-WORDCALL-OP
   id kk m r CALL-RESTORE ;

\ ---- leaving through the callee ----------------------------------------------
\ A tail call is not a call site with the end cut off: the pointer must already
\ stand at the callee's entry base, and the branch carries no adjustment.
: TAIL-CK ( n n -- )
   {: a:n r:n :}
   a ARGS SLOT-POSITIONS <> if E-X64SEL-TAIL throw then
   r OUTS SLOT-POSITIONS <> if E-X64SEL-TAIL throw then
   a X64IR:SLOT-WIDTH * DPLACED 0<> if E-X64SEL-TAIL throw then
   r X64IR:SLOT-WIDTH * DPLACED 0<> if E-X64SEL-TAIL throw then ;

: EMIT-TAIL-BR ( IR-ID:ir-op-id n -- )
   {: at:IR-ID:ir-op-id entry:n :}
   at X64IR-OPCODE:TAILCALL OPEN
   TOK OPERAND+
   CTX BLD  CTX BLD X64IR:KEY-ENTRY  CTX BLD entry X64IR:ENTRY-ATTR
   IR-BUILD:ADD-ATTR
   CTX BLD IR-BUILD:END-OP drop ;

\ The epilogue stands in FRONT of the branch and not after it.
: EMIT-TAIL-CALL ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   DSTACK? 0= if E-X64SEL-CALL throw then
   id SITE-SHAPE {: a:n r:n kk:n m:n :}
   a r TAIL-CK
   id 0 OPERAND TOK!
   id kk m a CALL-SAVE
   id EPILOGUE
   id  id WORD-ENTRY  EMIT-TAIL-BR
   N-TAILS @ 1+ N-TAILS ! ;

: EMIT-CALL-OR-TAIL ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id TAIL-OP? if id EMIT-TAIL-CALL exit then
   id EMIT-WORD-CALL ;

\ ---- leaving through the routine that ends the process -----------------------
\ The trap registry resolves diagnostics at compile time; only die is needed in
\ the target dictionary.
: TRAP-ENTRY ( -- n )
   NTRAP:ROUTINE$ NDICT:CALL-TARGET {: e:n :}
   e 0= if E-X64SEL-TRAP throw then
   e ;

: EMIT-TRAP-BR ( IR-ID:ir-op-id n n -- )
   {: at:IR-ID:ir-op-id give:n entry:n :}
   at X64IR-OPCODE:TRAP OPEN
   TOK OPERAND+
   CTX BLD  CTX BLD X64IR:KEY-TRAP-ENTRY  CTX BLD entry X64IR:ENTRY-ATTR
   IR-BUILD:ADD-ATTR
   give DBYTES-AT+
   CTX BLD IR-BUILD:END-OP drop ;

\ A call site with nothing to take back: the address, the length and the exit
\ code go into the cells die reads them out of.
: EMIT-TRAP ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   DSTACK? 0= if E-X64SEL-TRAP throw then
   TRAP-CELLS 0 ?do
      id  id i OPERAND  i X64IR:SLOT-WIDTH *  EMIT-DSTORE
   loop
   id  TRAP-CELLS X64IR:SLOT-WIDTH * DPLACED  TRAP-ENTRY  EMIT-TRAP-BR ;

\ ---- selecting a constant ----------------------------------------------------
\ One instruction whatever the cell holds: `mov r64, imm64`, with `x64.addr`
\ saying whether a relocation pass has to find it again.
: EMIT-CONST ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id X64IR-OPCODE:MOVI OPEN
   RESULT+
   CTX BLD  CTX BLD X64IR:KEY-IMM
   CTX BLD  id CONST-VALUE  X64IR:IMM-ATTR  IR-BUILD:ADD-ATTR
   CTX BLD  CTX BLD X64IR:KEY-ADDR
   CTX BLD  id CONST-ADDR  X64IR:ADDR-ATTR  IR-BUILD:ADD-ATTR
   CLOSE-VALUE
   id 0 RESULT-AT  ACC  VBIND ;

\ ---- the literal an instruction carries instead of a register -----------------
\ Only a plain number: an address literal opens a relocation site the emitter
\ has to find, and a site folded into an ALU immediate is a site with no
\ instruction of its own.
: LIT-OP? ( IR-ID:ir-value-id -- bool )
   {: v:IR-ID:ir-value-id :}
   v VALUE-FROM-OP? 0= if false exit then
   v DEF-OP OP-SLOT O-CONST <> if false exit then
   v DEF-OP CONST-ADDR HIR:ADDR-NONE = ;

: LIT-VALUE ( IR-ID:ir-value-id -- n )
   DEF-OP CONST-VALUE ;

: IMM-FOLD? ( IR-ID:ir-value-id -- bool )
   {: v:IR-ID:ir-value-id :}
   v LIT-OP? 0= if false exit then
   v LIT-VALUE {: k:n :}
   k X64IR:IMM-LIMIT negate <  k X64IR:IMM-LIMIT 1- >  or 0= ;

: SHIFT-FOLD? ( IR-ID:ir-value-id -- bool )
   {: v:IR-ID:ir-value-id :}
   v LIT-OP? 0= if false exit then
   v LIT-VALUE {: k:n :}
   k 0 >=  k X64IR:SHIFT-LIMIT <  and ;

\ ---- selecting the arithmetic ------------------------------------------------
\ Operand 0 is the one the instruction overwrites and the schema ties the result
\ to it. No copy is inserted here: a tie the allocator cannot satisfy is a copy
\ IT inserts, and inserting one here would decide that twice.
: EMIT-BINARY ( IR-ID:ir-op-id X64IR:opcode -- )
   {: id:IR-ID:ir-op-id o:X64IR:opcode :}
   id o OPEN
   CTX BLD  id 0 OPERAND  IR-BUILD:ADD-OPERAND
   CTX BLD  id 1 OPERAND  IR-BUILD:ADD-OPERAND
   RESULT+
   CLOSE-VALUE
   id 0 RESULT-AT  ACC  VBIND ;

: EMIT-BINARY-IMM ( IR-ID:ir-op-id X64IR:opcode n -- )
   {: id:IR-ID:ir-op-id o:X64IR:opcode imm:n :}
   id o OPEN
   CTX BLD  id 0 OPERAND  IR-BUILD:ADD-OPERAND
   RESULT+
   CTX BLD  CTX BLD X64IR:KEY-IMM
   CTX BLD  imm X64IR:IMM32-ATTR  IR-BUILD:ADD-ATTR
   CLOSE-VALUE
   id 0 RESULT-AT  ACC  VBIND ;

: EMIT-SHIFT-IMM ( IR-ID:ir-op-id X64IR:opcode n -- )
   {: id:IR-ID:ir-op-id o:X64IR:opcode count:n :}
   id o OPEN
   CTX BLD  id 0 OPERAND  IR-BUILD:ADD-OPERAND
   RESULT+
   CTX BLD  CTX BLD X64IR:KEY-SHIFT
   CTX BLD  count X64IR:SHIFT-ATTR  IR-BUILD:ADD-ATTR
   CLOSE-VALUE
   id 0 RESULT-AT  ACC  VBIND ;

: EMIT-UNARY ( IR-ID:ir-op-id X64IR:opcode -- )
   {: id:IR-ID:ir-op-id o:X64IR:opcode :}
   id o OPEN
   CTX BLD  id 0 OPERAND  IR-BUILD:ADD-OPERAND
   RESULT+
   CLOSE-VALUE
   id 0 RESULT-AT  ACC  VBIND ;

\ The register form and the immediate form are one rule, because which one the
\ machine has is decided by what the second operand IS.
: BINARY-RULE ( IR-ID:ir-op-id X64IR:opcode X64IR:opcode -- )
   {: id:IR-ID:ir-op-id o:X64IR:opcode oi:X64IR:opcode :}
   id 1 OPERAND-AT IMM-FOLD? if
      id oi  id 1 OPERAND-AT LIT-VALUE  EMIT-BINARY-IMM exit
   then
   id o EMIT-BINARY ;

\ `shl r64, cl` is the only other form and it names rcx, which no contract can
\ yet demand, so a count that is not a literal is refused rather than lowered.
: SHIFT-RULE ( IR-ID:ir-op-id X64IR:opcode -- )
   {: id:IR-ID:ir-op-id o:X64IR:opcode :}
   id 1 OPERAND-AT SHIFT-FOLD? 0= if E-X64SEL-FIXED throw then
   id o  id 1 OPERAND-AT LIT-VALUE  EMIT-SHIFT-IMM ;

\ ---- selecting a comparison --------------------------------------------------
\ One source relation is one machine condition, and the comparison of two Habu
\ cells is SIGNED, so every one of them is an L/G condition.
: COMPARE-COND ( IR-ID:ir-op-id -- X64IR:cond )
   OP-SLOT
   case
      O-LT    of X64IR-COND:LT    endof
      O-LE    of X64IR-COND:LE    endof
      O-GT    of X64IR-COND:GT    endof
      O-GE    of X64IR-COND:GE    endof
      O-EQUAL of X64IR-COND:EQUAL endof
      O-NE    of X64IR-COND:NE    endof
      E-X64SEL-OPCODE throw
   endcase ;

: COND-ATTR+ ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   CTX BLD  CTX BLD X64IR:KEY-COND
   CTX BLD  id COMPARE-COND  X64IR:COND-ATTR  IR-BUILD:ADD-ATTR ;

\ Compare and set a boolean: ONE operation and three instructions, because the
\ flags between them are a single architectural resource no value may stand for.
\ It is not fused into the branch below it in this slice.
: EMIT-CMPSET ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id X64IR-OPCODE:CMPSET OPEN
   CTX BLD  id 0 OPERAND  IR-BUILD:ADD-OPERAND
   CTX BLD  id 1 OPERAND  IR-BUILD:ADD-OPERAND
   RESULT+
   id COND-ATTR+
   CLOSE-VALUE
   id 0 RESULT-AT  ACC  VBIND ;

\ The operand is the LEFT-hand side and the immediate the right, which is what
\ makes `0=` and `0<` one instruction against the literal zero.
: EMIT-CMPSETI ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id imm:n :}
   id X64IR-OPCODE:CMPSETI OPEN
   CTX BLD  id 0 OPERAND  IR-BUILD:ADD-OPERAND
   RESULT+
   id COND-ATTR+
   CTX BLD  CTX BLD X64IR:KEY-IMM
   CTX BLD  imm X64IR:IMM32-ATTR  IR-BUILD:ADD-ATTR
   CLOSE-VALUE
   id 0 RESULT-AT  ACC  VBIND ;

: COMPARE-RULE ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id 1 OPERAND-AT IMM-FOLD? if
      id  id 1 OPERAND-AT LIT-VALUE  EMIT-CMPSETI exit
   then
   id EMIT-CMPSET ;

\ ---- selecting the memory operations -----------------------------------------
\ The source order and the machine order are ONE order.
: EMIT-MEM ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   DSTACK? 0= if E-X64SEL-MEM throw then
   id 0 RESULT-AT  TOK  VBIND ;

: EMIT-ALOAD ( IR-ID:ir-op-id X64IR:opcode -- )
   {: id:IR-ID:ir-op-id o:X64IR:opcode :}
   id o OPEN
   CTX BLD  id 0 OPERAND  IR-BUILD:ADD-OPERAND
   CTX BLD  id 1 OPERAND  IR-BUILD:ADD-OPERAND
   RESULT+
   TOKEN+
   CTX BLD IR-BUILD:END-OP {: nid:IR-ID:ir-op-id :}
   CTX BLD nid 1 IR-BUILD:OP-RESULT@ {: tk:IR-ID:ir-value-id :}
   tk TOK!
   id 1 RESULT-AT tk VBIND
   id 0 RESULT-AT  CTX BLD nid 0 IR-BUILD:OP-RESULT@  VBIND ;

: EMIT-ASTORE ( IR-ID:ir-op-id X64IR:opcode -- )
   {: id:IR-ID:ir-op-id o:X64IR:opcode :}
   id o OPEN
   CTX BLD  id 0 OPERAND  IR-BUILD:ADD-OPERAND
   CTX BLD  id 1 OPERAND  IR-BUILD:ADD-OPERAND
   CTX BLD  id 2 OPERAND  IR-BUILD:ADD-OPERAND
   TOKEN+
   CTX BLD IR-BUILD:END-OP {: nid:IR-ID:ir-op-id :}
   CTX BLD nid 0 IR-BUILD:OP-RESULT@ {: tk:IR-ID:ir-value-id :}
   tk TOK!
   id 0 RESULT-AT tk VBIND ;

\ ---- selecting the address of another function of this emission ---------------
: QUOT-FUN ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id ATTRS-OF 1 <> if E-X64SEL-ATTR throw then
   id 0 ATTR-KEY-AT  0 BND-FUN @  SAME-SYM?
   0= if E-X64SEL-ATTR throw then
   id 0 ATTR-INT-AT ;

: EMIT-QUOT ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id X64IR-OPCODE:CODEADDR OPEN
   RESULT+
   CTX BLD  CTX BLD X64IR:KEY-FUN
   CTX BLD  id QUOT-FUN  X64IR:FUN-ATTR  IR-BUILD:ADD-ATTR
   CLOSE-VALUE
   id 0 RESULT-AT  ACC  VBIND ;

\ ---- selecting the return ----------------------------------------------------
\ Under the data-stack convention every result is stored to its cell and the
\ pointer published; under a register convention the values still live where
\ control leaves become the return's own operands.
: EMIT-EXIT ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   OUTS SLOT-POSITIONS {: r:n :}
   id OPERANDS-OF r <> if E-X64SEL-PLACE throw then
   r 0 ?do
      id  id i OPERAND  OUTS i NEFF:SEQ-SLOT@ X64IR:SLOT-WIDTH *  EMIT-DSTORE
   loop
   id  r X64IR:SLOT-WIDTH * DPLACED  EMIT-DPUBLISH ;

: EMIT-RETURN ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   DSTACK? if id EMIT-EXIT then
   id EPILOGUE
   id X64IR-OPCODE:RET OPEN
   DSTACK? 0= if
      id OPERANDS-OF {: k:n :}
      k 0 ?do
         CTX BLD  id i OPERAND  IR-BUILD:ADD-OPERAND
      loop
   then
   CTX BLD IR-BUILD:END-OP drop ;

: EMIT-RETURN-OR-TAILED ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   TAIL-HERE? if exit then
   id EMIT-RETURN ;

\ ---- the order a block is entered with ---------------------------------------
\ Every data-stack access threads one order, so a block takes the order its
\ edges stated. The machine two-way branch carries no operands, so an order two
\ paths differ on cannot be handed over as an argument here.
: BLOCK-ORD-CK ( n -- n )
   dup 0 < over BMAX >= or if E-X64SEL-CAP throw then ;

: ORDER-ARG? ( n -- bool )
   {: b:n :}
   false
   FUN b BLOCK-AT ARG-COUNT 0 ?do
      FUN b BLOCK-AT i ARG-AT TOKEN? if drop true leave then
   loop ;

: ORDER-SET? ( n -- bool )
   BLOCK-ORD-CK cells D-ORDER-SET + @ 0<> ;

: ORDER@ ( n -- IR-ID:ir-value-id )
   BLOCK-ORD-CK D-ORDER @ ;

: ORDER! ( n -- )
   {: b:n :}
   TOK b BLOCK-ORD-CK D-ORDER !
   1 b BLOCK-ORD-CK cells D-ORDER-SET + ! ;

: ORDER-SAME? ( n -- bool )
   ORDER@ IR-ID:VALUE-LOCAL  TOK IR-ID:VALUE-LOCAL  = ;

: ORDER-CLEAR ( -- )
   BMAX 0 ?do 0 i cells D-ORDER-SET + ! loop ;

: ORDER-EDGE! ( n -- )
   {: b:n :}
   DSTACK? 0= if exit then
   b ORDER-ARG? if exit then
   b ORDER-SET? 0= if b ORDER! exit then
   b ORDER-SAME? 0= if E-X64SEL-ORDER throw then ;

\ A block no edge reached is one control cannot arrive at.
: ORDER-ENTER ( n -- )
   {: b:n :}
   DSTACK? 0= if exit then
   b ORDER-ARG? if exit then
   b ORDER-SET? 0= if E-X64SEL-ORDER throw then
   b ORDER@ TOK! ;

\ ---- selecting the branches --------------------------------------------------
\ The blocks of the source function map one for one, so a successor's ordinal
\ rides across and only the module base changes.
: SUCC-IDX ( IR-ID:ir-op-id n -- n )
   SUCC-AT IR-ID:BLOCK-LOCAL  R-BASE @ -  BLOCK-ORD-CK ;

: SUCCESSOR-ORD+ ( n -- )
   {: b:n :}
   b ORDER-EDGE!
   CTX BLD
   BLD IR-BUILD:MODULE-KEY  R-NEWBASE @ b +  IR-ID:PACK-BLOCK
   IR-BUILD:ADD-SUCCESSOR ;

: SUCCESSOR+ ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id i:n :}
   id i SUCC-IDX SUCCESSOR-ORD+ ;

\ ---- splitting the edges that carry values -----------------------------------
\ Edge destinations must share the successor arguments' registers. Snapshot all
\ sources before those destination copies: a backedge can permute live header
\ values, and writing one destination must not destroy a later source. The copy
\ is `x64.mov`, which ties nothing and is what an allocator coalesces away.
: EMIT-COPY ( IR-ID:ir-op-id IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: at:IR-ID:ir-op-id v:IR-ID:ir-value-id :}
   at X64IR-OPCODE:MOV OPEN
   CTX BLD v IR-BUILD:ADD-OPERAND
   RESULT+
   CLOSE-VALUE
   ACC ;

\ An unchanged header argument already occupies its destination. Copying it
\ would create another value tied to that register while the original can still
\ be live on the loop's exit path.
: EDGE-STAYS? ( IR-ID:ir-op-id n -- bool )
   {: id:IR-ID:ir-op-id i:n :}
   id i OPERAND-AT  id 0 SUCC-AT i ARG-AT  SAME-VALUE? ;

: EDGE-VALUE ( IR-ID:ir-op-id n -- IR-ID:ir-value-id )
   {: id:IR-ID:ir-op-id i:n :}
   id i OPERAND-AT TOKEN?  id i EDGE-STAYS? or if id i OPERAND exit then
   id  id i OPERAND  EMIT-COPY ;

: EDGE-DESTINATION ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id i:n :}
   id i OPERAND-AT TOKEN?  id i EDGE-STAYS? or if exit then
   id  i EDGE-V @  EMIT-COPY  i EDGE-V ! ;

: EMIT-BR ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id OPERANDS-OF {: k:n :}
   k EDGE-MAX > if E-X64SEL-CAP throw then
   k 0 ?do  id i EDGE-VALUE  i EDGE-V !  loop
   k 0 ?do  id i EDGE-DESTINATION  loop
   id X64IR-OPCODE:BR OPEN
   k 0 ?do
      CTX BLD  i EDGE-V @  IR-BUILD:ADD-OPERAND
   loop
   id 0 SUCCESSOR+
   CTX BLD IR-BUILD:END-OP drop ;

\ `test rv, rv` and a jump on the zero flag, with the successors in the source's
\ own order: hir.brz goes to its FIRST successor on zero and so does this form.
: EMIT-BRZ ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id X64IR-OPCODE:BRZ OPEN
   CTX BLD  id 0 OPERAND  IR-BUILD:ADD-OPERAND
   id 0 SUCCESSOR+
   id 1 SUCCESSOR+
   CTX BLD IR-BUILD:END-OP drop ;

\ ---- the selection table -----------------------------------------------------
\ The selector may not lower a may-trap source operation to a form that does not
\ reproduce the trap. x86-64's add, sub and imul wrap exactly as ARM64's do, so
\ a trapping compilation unit is refused here too.
: TRAP-PRESERVED? ( HIR:opcode -- bool )
   MATCH HIR:opcode
      const  OF false ENDOF
      add    OF false ENDOF
      sub    OF false ENDOF
      mul    OF false ENDOF
      div    OF true  ENDOF
      lt     OF false ENDOF
      le     OF false ENDOF
      gt     OF false ENDOF
      ge     OF false ENDOF
      equal  OF false ENDOF
      ne     OF false ENDOF
      and    OF false ENDOF
      or     OF false ENDOF
      xor    OF false ENDOF
      lshift OF false ENDOF
      rshift OF false ENDOF
      invert OF false ENDOF
      mem    OF false ENDOF
      load   OF false ENDOF
      store  OF false ENDOF
      bload  OF false ENDOF
      bstore OF false ENDOF
      br     OF false ENDOF
      brz    OF false ENDOF
      call   OF true  ENDOF
      wordcall OF true ENDOF
      return OF false ENDOF
      trap   OF true  ENDOF
      fconst   OF false ENDOF
      fadd     OF false ENDOF
      fsub     OF false ENDOF
      fmul     OF false ENDOF
      fdiv     OF false ENDOF
      fneg     OF false ENDOF
      fabs     OF false ENDOF
      fsqrt    OF false ENDOF
      intreal  OF false ENDOF
      realint  OF false ENDOF
      bitsreal OF false ENDOF
      realbits OF false ENDOF
      flt      OF false ENDOF
      fgt      OF false ENDOF
      feq      OF false ENDOF
      fltz     OF false ENDOF
      feqz     OF false ENDOF
      quot     OF false ENDOF
   ;MATCH ;

: TRAP-CK ( HIR:opcode IR-ID:ir-symbol-id -- HIR:opcode )
   {: o:HIR:opcode sym:IR-ID:ir-symbol-id :}
   V-SCHR VW sym IR-SCHEMA:FTRAPS? if
      o TRAP-PRESERVED? 0= if E-X64SEL-TRAP throw then
   then
   o ;

\ ---- one source operation, lowered -------------------------------------------
: RULE ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id OPCODE-AT {: sym:IR-ID:ir-symbol-id :}
   sym OPCODE-SLOT HIR:NTH  sym TRAP-CK
   MATCH HIR:opcode
      const  OF id EMIT-CONST ENDOF
      add    OF id X64IR-OPCODE:ADD X64IR-OPCODE:ADDI BINARY-RULE ENDOF
      sub    OF id X64IR-OPCODE:SUB X64IR-OPCODE:SUBI BINARY-RULE ENDOF
      mul    OF id X64IR-OPCODE:IMUL EMIT-BINARY ENDOF
      div    OF E-X64SEL-FIXED throw ENDOF
      lt     OF id COMPARE-RULE ENDOF
      le     OF id COMPARE-RULE ENDOF
      gt     OF id COMPARE-RULE ENDOF
      ge     OF id COMPARE-RULE ENDOF
      equal  OF id COMPARE-RULE ENDOF
      ne     OF id COMPARE-RULE ENDOF
      and    OF id X64IR-OPCODE:AND X64IR-OPCODE:ANDI BINARY-RULE ENDOF
      or     OF id X64IR-OPCODE:OR X64IR-OPCODE:ORI BINARY-RULE ENDOF
      xor    OF id X64IR-OPCODE:XOR X64IR-OPCODE:XORI BINARY-RULE ENDOF
      lshift OF id X64IR-OPCODE:SHLI SHIFT-RULE ENDOF
      rshift OF id X64IR-OPCODE:SHRI SHIFT-RULE ENDOF
      invert OF id X64IR-OPCODE:NOT EMIT-UNARY ENDOF
      mem    OF id EMIT-MEM ENDOF
      load   OF id X64IR-OPCODE:ALOAD EMIT-ALOAD ENDOF
      store  OF id X64IR-OPCODE:ASTORE EMIT-ASTORE ENDOF
      bload  OF id X64IR-OPCODE:ABLOAD EMIT-ALOAD ENDOF
      bstore OF id X64IR-OPCODE:ABSTORE EMIT-ASTORE ENDOF
      br     OF id EMIT-BR ENDOF
      brz    OF id EMIT-BRZ ENDOF
      call   OF id EMIT-CALL ENDOF
      wordcall OF id EMIT-CALL-OR-TAIL ENDOF
      return OF id EMIT-RETURN-OR-TAILED ENDOF
      trap   OF id EMIT-TRAP ENDOF
      fconst   OF E-X64SEL-FLOAT throw ENDOF
      fadd     OF E-X64SEL-FLOAT throw ENDOF
      fsub     OF E-X64SEL-FLOAT throw ENDOF
      fmul     OF E-X64SEL-FLOAT throw ENDOF
      fdiv     OF E-X64SEL-FLOAT throw ENDOF
      fneg     OF E-X64SEL-FLOAT throw ENDOF
      fabs     OF E-X64SEL-FLOAT throw ENDOF
      fsqrt    OF E-X64SEL-FLOAT throw ENDOF
      flt      OF E-X64SEL-FLOAT throw ENDOF
      fgt      OF E-X64SEL-FLOAT throw ENDOF
      feq      OF E-X64SEL-FLOAT throw ENDOF
      fltz     OF E-X64SEL-FLOAT throw ENDOF
      feqz     OF E-X64SEL-FLOAT throw ENDOF
      intreal  OF E-X64SEL-FLOAT throw ENDOF
      realint  OF E-X64SEL-FLOAT throw ENDOF
      bitsreal OF E-X64SEL-FLOAT throw ENDOF
      realbits OF E-X64SEL-FLOAT throw ENDOF
      quot     OF id EMIT-QUOT ENDOF
   ;MATCH ;

\ ---- the literal that never becomes an instruction ---------------------------
\ A literal folded into every instruction that reads it needs no `x64.movi` of
\ its own. Whether that is so is COUNTED over the whole function: one use that
\ is not a fold - an operand the form has no immediate for, a branch argument, a
\ second reader - and the literal is materialised exactly as it always was.
: FOLDS-OPERAND1? ( IR-ID:ir-op-id -- bool )
   {: id:IR-ID:ir-op-id :}
   id OPERANDS-OF 2 < if false exit then
   id OP-SLOT {: s:n :}
   s O-LSHIFT =  s O-RSHIFT = or if id 1 OPERAND-AT SHIFT-FOLD? exit then
   s O-ADD = s O-SUB = or s O-AND = or s O-OR = or s O-XOR = or
   s O-LT = or s O-LE = or s O-GT = or s O-GE = or
   s O-EQUAL = or s O-NE = or
   0= if false exit then
   id 1 OPERAND-AT IMM-FOLD? ;

: OP-USES ( IR-ID:ir-value-id IR-ID:ir-op-id -- n )
   {: v:IR-ID:ir-value-id id:IR-ID:ir-op-id :}
   0
   id OPERANDS-OF 0 ?do
      id i OPERAND-AT v SAME-VALUE? if 1+ then
   loop ;

: OP-FOLDS ( IR-ID:ir-value-id IR-ID:ir-op-id -- n )
   {: v:IR-ID:ir-value-id id:IR-ID:ir-op-id :}
   id FOLDS-OPERAND1? 0= if 0 exit then
   id 1 OPERAND-AT v SAME-VALUE? if 1 else 0 then ;

: BLOCK-USES ( IR-ID:ir-value-id IR-ID:ir-block-id -- n )
   {: v:IR-ID:ir-value-id bk:IR-ID:ir-block-id :}
   0
   bk OP-COUNT 0 ?do  v bk i OP-AT OP-USES  +  loop ;

: BLOCK-FOLDS ( IR-ID:ir-value-id IR-ID:ir-block-id -- n )
   {: v:IR-ID:ir-value-id bk:IR-ID:ir-block-id :}
   0
   bk OP-COUNT 0 ?do  v bk i OP-AT OP-FOLDS  +  loop ;

: VALUE-USES ( IR-ID:ir-value-id -- n )
   {: v:IR-ID:ir-value-id :}
   0
   FUN BLOCK-COUNT 0 ?do  v FUN i BLOCK-AT BLOCK-USES  +  loop ;

: VALUE-FOLDS ( IR-ID:ir-value-id -- n )
   {: v:IR-ID:ir-value-id :}
   0
   FUN BLOCK-COUNT 0 ?do  v FUN i BLOCK-AT BLOCK-FOLDS  +  loop ;

: LIT-FOLDED-AWAY? ( IR-ID:ir-op-id -- bool )
   {: id:IR-ID:ir-op-id :}
   id OP-SLOT O-CONST <> if false exit then
   id RESULTS-OF 1 <> if false exit then
   id 0 RESULT-AT {: v:IR-ID:ir-value-id :}
   v VALUE-USES {: u:n :}
   u 0= if false exit then
   v VALUE-FOLDS u = ;

\ ---- opening the selected function -------------------------------------------
\ The two modules number their symbols separately, so the name is copied out of
\ the source module's interner and interned into the new one.
: FUN-NAME ( IR-ID:ir-fun-id -- IR-ID:ir-symbol-id )
   {: f:IR-ID:ir-fun-id :}
   V-SYMP VW V-SYMR VW  V-FUNR VW MKEY f IR-FUN:FSYMBOL@  NAMEBUF NAME-CAP
   IR-SYM:FCOPY {: u:n :}
   CTX BLD NAMEBUF u IR-BUILD:INTERN-SYMBOL ;

: FUN-SIG ( IR-ID:ir-fun-id -- IR-ID:ir-type-id )
   {: f:IR-ID:ir-fun-id :}
   V-TYPR VW  V-FUNR VW MKEY f IR-FUN:FSIGNATURE@  IR-TYPE:FARITY@
   {: in:n out:n :}
   CTX BLD X64IR:GPR-TYPE {: t:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN
   in 0 ?do t IR-TYPE:FN-PARAM loop
   out 0 ?do t IR-TYPE:FN-RESULT loop
   CTX BLD IR-BUILD:INTERN-CODE-REF ;

: OPEN-FUN ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   CTX BLD f FUN-NAME IR-BUILD:BEGIN-FUN
   CTX BLD f FUN-SIG IR-BUILD:SET-SIGNATURE
   CTX BLD  V-FUNR VW f IR-FUN:FLINKAGE@  IR-BUILD:SET-LINKAGE
   CTX BLD  V-FUNR VW f IR-FUN:FVISIBILITY@  IR-BUILD:SET-VISIBILITY
   CTX BLD  V-FUNR VW f IR-FUN:FCONVENTION@  IR-BUILD:SET-CONVENTION
   CTX BLD f FUN-SPAN IR-BUILD:SET-FUN-SPAN ;

\ At the class the source argument's TYPE says. A block argument of the
\ memory-order type is the ORDER arriving.
: OPEN-ARG1 ( IR-ID:ir-value-id -- )
   {: a:IR-ID:ir-value-id :}
   a TOKEN? if
      a  CTX BLD  CTX BLD X64IR:MEM-TYPE  IR-BUILD:ADD-BLOCK-ARG
      dup TOK!  VBIND
      exit
   then
   a  CTX BLD  CTX BLD X64IR:GPR-TYPE  IR-BUILD:ADD-BLOCK-ARG  VBIND ;

: OPEN-ARGS ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   bk ARG-COUNT 0 ?do bk i ARG-AT OPEN-ARG1 loop ;

\ Under the data-stack convention the entry block takes no argument at all,
\ because nothing arrives in a register: the pointer is taken and every
\ argument the contract lists is loaded out of its cell.
: OPEN-DARGS ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   ARGS SLOT-POSITIONS {: a:n :}
   bk ARG-COUNT a <> if E-X64SEL-PLACE throw then
   bk 0 OP-AT {: at:IR-ID:ir-op-id :}
   at PROLOGUE
   at  a X64IR:SLOT-WIDTH * DPLACED  EMIT-DTAKE
   0 ORDER-EDGE!
   a 0 ?do
      bk i ARG-AT
      at  ARGS i NEFF:SEQ-SLOT@ X64IR:SLOT-WIDTH *  EMIT-DLOAD
      VBIND
   loop ;

\ Only the entry block carries the routine's interface.
: OPEN-BLOCK ( IR-ID:ir-block-id n -- )
   {: bk:IR-ID:ir-block-id ord:n :}
   CTX BLD IR-BUILD:BEGIN-BLOCK
   CTX BLD bk BLOCK-SPAN IR-BUILD:SET-BLOCK-SPAN
   DSTACK? ord 0= and if bk OPEN-DARGS exit then
   bk OPEN-ARGS
   ord ORDER-ENTER ;

\ ---- which operation this function leaves through, decided once --------------
\ The operation immediately in front of the terminator of a block whose
\ terminator is the return.
: TAIL-AT? ( IR-ID:ir-block-id n -- bool )
   {: bk:IR-ID:ir-block-id at:n :}
   TAIL? 0= if false exit then
   bk OP-COUNT {: n:n :}
   n 2 < if false exit then
   at n 2 - <> if false exit then
   bk at OP-AT OP-SLOT O-WORDCALL <> if false exit then
   bk n 1- OP-AT OP-SLOT O-RETURN = ;

\ Nothing the site would carry is read again, which is the one thing a tail
\ branch needs that the source operation does not say.
: TAIL-DEAD-CK ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id k:n :}
   k 0 ?do
      id i 1+ RESULT-AT VALUE-USES 0<> if E-X64SEL-TAIL throw then
   loop ;

: TAIL-POS ( IR-ID:ir-block-id -- n )
   {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT {: n:n :}
   n 2 < if -1 exit then
   bk n 2 - TAIL-AT? 0= if -1 exit then
   n 2 - ;

: SITE-CROSSED ( IR-ID:ir-op-id -- n )
   WORD-SHAPE {: kk:n m:n :}
   2drop
   kk m + ;

\ A SECOND site in one function is refused rather than one of them chosen.
: TAIL-SITE-TRY ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   bk TAIL-POS {: at:n :}
   at 0 < if exit then
   TAIL-SITE @ 0 >= if E-X64SEL-TAIL throw then
   bk at OP-AT {: id:IR-ID:ir-op-id :}
   id IR-ID:OP-LOCAL TAIL-SITE !
   id  id SITE-CROSSED  TAIL-DEAD-CK ;

: TAIL-SITE! ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id ord:n :}
   -1 TAIL-SITE !
   TAIL? 0= if exit then
   ord 0<> if exit then
   f BLOCK-COUNT 0 ?do  f i BLOCK-AT TAIL-SITE-TRY  loop ;

\ ---- where this function's blocks and its pointer stand ----------------------
: R-BASE! ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f 0 BLOCK-AT IR-ID:BLOCK-LOCAL R-BASE !
   f BLOCK-COUNT 0 ?do
      f i BLOCK-AT IR-ID:BLOCK-LOCAL  R-BASE @ -  i <>
      if E-X64SEL-SHAPE throw then
   loop ;

\ The entry base, except in a routine that leaves through its callee: there the
\ tail branch carries no adjustment, so the pointer never moves at all.
: DPLACE ( -- )
   0 D-POS !
   DSTACK? 0= if exit then
   TAIL? 0= if exit then
   ARGS SLOT-POSITIONS X64IR:SLOT-WIDTH * D-POS ! ;

\ ---- the walk ----------------------------------------------------------------
\ The value map is NOT cleared between blocks: a value defined in one block and
\ read in another is ordinary.
: WALK-BLOCK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id ord:n :}
   f ord BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk ord OPEN-BLOCK
   bk 0 S-BLK !
   bk OP-COUNT {: n:n :}
   n 0 ?do
      bk i OP-AT {: id:IR-ID:ir-op-id :}
      id LIT-FOLDED-AWAY? 0= if id RULE then
   loop
   CTX BLD IR-BUILD:END-BLOCK drop ;

\ Block by block in the order the module records them, which is the order every
\ later pass reads too.
: WALK-FUN ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id ord:n :}
   f BLOCK-COUNT {: n:n :}
   n 1 < if E-X64SEL-SHAPE throw then
   n BMAX > if E-X64SEL-CAP throw then
   f 0 S-FUN !
   f ord FUN-PLACES!
   f ord TAIL-SITE!
   f OPEN-FUN
   f V-RANGE!
   VCLEAR
   ORDER-CLEAR
   f R-BASE!
   DPLACE
   n 0 ?do
      f i WALK-BLOCK
   loop
   R-NEWBASE @ n + R-NEWBASE !
   CTX BLD IR-BUILD:END-FUN drop ;

\ ---- what one selection run is told ------------------------------------------
\ The new module gets the same source the old one has, carried row and all.
: SOURCE! ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   V-SRC VW IR-SOURCE:FSOURCES 1 <> if E-X64SEL-SHAPE throw then
   c b  V-SRC VW  MKEY 0 IR-ID:PACK-SOURCE  IR-BUILD:CARRY-SOURCE 0 S-SID ! ;

: BND-TAKE ( -- )
   BND-MODE @ {: have:n :}
   BOUND-NO BND-MODE !
   have BOUND-YES <> if E-X64SEL-BIND throw then ;

: BND-MODULE-CK ( IR-BUILD:module -- )
   IR-BUILD:FMODULE  0 BND-MOD @  IR-ID:MODULE-SAME?
   0= if E-X64SEL-SOURCE throw then ;

: HIR-CK ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b  c b IR-BUILD:DIALECT@  HIR:NAME IR-BUILD:SYMBOL-IS?
   0= if E-X64SEL-SOURCE throw then
   c b IR-BUILD:SCHEMA-MAJOR@ HIR:MAJOR <> if E-X64SEL-SOURCE throw then
   c b IR-BUILD:SCHEMA-MINOR@ HIR:MINOR <> if E-X64SEL-SOURCE throw then ;

\ The machine a contract is written against is the contract's own field, and a
\ backend that lowered for another machine's would be reading numbers that mean
\ something else.
: MACHINE-CK ( NMACH:mach -- )
   X64M:MACHINE NMACH-MACH:EQ 0= if E-X64SEL-MACHINE throw then ;

public

\ ---- binding the source dialect ----------------------------------------------
\ The only moment a module can be asked its opcode identities, because its
\ symbols are its own ordinals.
: BIND-SOURCE ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   BND-MODE @ BOUND-YES = if E-X64SEL-BIND throw then
   c b HIR-CK
   b IR-BUILD:MODULE@ 0 BND-MOD !
   HIR:OPCODES 0 ?do
      c b i HIR:BIND i BND-OP !
   loop
   c b HIR:KEY-VALUE 0 BND-VAL !
   c b HIR:KEY-ADDR  0 BND-ADDR !
   c b HIR:KEY-FUN   0 BND-FUN !
   c b HIR:KEY-ENTRY 0 BND-ENTRY !
   c b HIR:KEY-IN    0 BND-IN !
   c b HIR:KEY-OUT   0 BND-OUT !
   c b HIR:MEM-TYPE 0 BND-MEM !
   BOUND-YES BND-MODE ! ;

\ Each pass answers for itself, because a caller cleaning up after a refusal
\ cannot know how far the run got.
: BOUND? ( -- bool )
   BND-MODE @ BOUND-YES = ;

: RELEASE ( -- )
   BND-TAKE ;

\ ---- the pass ----------------------------------------------------------------
: SELECT ( IR-CTX:ctx IR-BUILD:module IR-BUILD:builder NEFF:routine -- IR-BUILD:module )
   NEFF:VALIDATE NEFF-ROUTINE:UNMAKE
   {: cv:NEFF:conv gi:NEFF:placeseq gr:NEFF:placeseq gc:NEFF:gprs
      fi:NEFF:fprs fr:NEFF:fprs fc:NEFF:fprs
      z:NEFF:nzcv l:NEFF:link ct:NEFF:control
      t:NEFF:traits size:n delta:n mch:NMACH:mach :}
   {: c:IR-CTX:ctx m:IR-BUILD:module b:IR-BUILD:builder :}
   c X64IR:CHECK-TARGET
   mch MACHINE-CK
   BND-TAKE
   m BND-MODULE-CK
   gi 0 S-ARGS !
   gr 0 S-OUTS !
   gi NEFF:SEQ-LEN S-DECL-IN !
   gr NEFF:SEQ-LEN S-DECL-OUT !
   t 0 S-TRT !
   size S-FRAME !
   ct NEFF-CONTROL:TAIL-CALL NEFF-CONTROL:EQ if 1 else 0 then S-TAIL !
   cv NEFF-CONV:DSTACK NEFF-CONV:EQ if 1 else 0 then S-DSTACK !
   0 N-CALLS !
   0 N-TAILS !
   -1 TAIL-SITE !
   0 R-NEWBASE !
   CONTRACT-CK
   c 0 S-CTX !
   b 0 S-BLD !
   m VIEWS!
   RESERVE-SCRATCH
   c b SOURCE!
   FUN-COUNT {: n:n :}
   n 0 ?do
      MKEY i IR-ID:PACK-FUN i WALK-FUN
   loop
   CALLED-CK
   TAILED-CK
   c b IR-BUILD:FREEZE ;

: RESET-SCRATCH ( -- )
   0 SCRATCH-VALUES ! 0 SCRATCH-BLOCKS ! ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;using
;package
