\ regalloc.f - give every virtual register of one straight-line machine block a
\ real ARM64 general register, by linear scan.
\
\ docs/compiler-ir-design.md section 7.9 ("start with linear scan") over the
\ dialect src/compiler/native/a64ir.f defines and src/compiler/native/select.f
\ produces. Everything before this pass names values; everything after it names
\ registers and bytes. This file owns exactly one step of that: which physical
\ register holds which value. It rewrites no module, chooses no block order and
\ encodes nothing.
\
\ WHY THE INTERVALS ARE TRIVIAL, AND WHY THERE IS NO INTERVAL MACHINERY. The
\ input is one block with no branches, so the operations have one order and a
\ position is just an index into it. A value is written once - it is SSA - and
\ every use is an operation that comes after the write. A value's live range is
\ therefore the single stretch from its definition to its last use, with no
\ holes, no pieces and nothing to merge. Two values interfere exactly when those
\ two stretches overlap. A general allocator needs interval lists because a value
\ can be live down one arm of a branch and dead down the other; none of that
\ exists here, so none of it is built here. When control flow arrives, the
\ interval representation arrives with it.
\
\ THE READ-THEN-WRITE BOUNDARY. One operation reads its operands and then writes
\ its results, so a value whose last use is operation i and a value defined by
\ operation i are never live at the same instant: the second may take the first's
\ register. That is not a peephole nicety - it is what lets a chain of moves and
\ arithmetic run in one register, and it is the same rule the validator applies
\ when it decides whether two live ranges overlap.
\
\ TIED OPERANDS ARE READ, NOT RECOGNISED. Some instruction forms name one
\ register field twice - the move-wide overwrite writes one sixteen-bit half and
\ keeps the other three, so its source and its destination are one field. In SSA
\ those are two values, and this pass has to put them in the same physical
\ register or the instruction means something else. Which forms do that is not
\ knowledge this file holds: a form declares its ties in its own operation schema
\ (src/compiler/ir/schema.f), and the walk below reads that declaration for every
\ operation it allocates. A tied result therefore takes the register of the
\ operand it shares a field with, and a program in which the kept value is still
\ needed afterwards is refused by name, because one register field cannot hold a
\ value that must survive and the value that replaces it. A dialect that gives a
\ new form a tie is honoured here without a line changing.
\
\ WHAT THIS PASS HAS TO KNOW ABOUT A FORM, AND WHERE IT COMES FROM. Exactly two
\ things, and both are declarations it reads rather than facts it remembers: the
\ register class of each value, which is its type, and the ties, which are the
\ schema's. Nothing else about a FORM constrains a register: no form of this
\ dialect names a fixed register or clobbers one it does not name. A form
\ constraint invented later belongs in the schema beside the tie and must be read
\ here; a constraint kept somewhere this pass does not look would be allocated
\ around silently, which is the reason the tie moved out of this file in the
\ first place (dot habu-make-an-unread-33f525e8).
\
\ THE THIRD CONSTRAINT IS THE ROUTINE'S, NOT THE FORM'S. `add` never needs x0.
\ SQUARE's argument, on the other hand, has to arrive somewhere its caller and it
\ have agreed on, and its result has to leave somewhere the caller will look.
\ That is a property of one routine's interface, so it is declared where the rest
\ of that interface is - on the routine contract this pass is already handed
\ (src/compiler/a64-effect.f), as an ordered register list per side - and it
\ would be wrong in the operation schema, which describes forms and knows nothing
\ about which routine they are in. Reading it here is reading it from the same
\ value the independent validator is handed, so neither is repeating the other.
\
\ WHAT PRE-COLOURING DOES, AND WHEN A MOVE IS UNAVOIDABLE. A block argument whose
\ position the contract names is given exactly that register before the scan
\ starts, and the scan then cannot hand it out while the argument is live,
\ because it is held like any other assignment. A returned value is different:
\ what the contract says about it is where it has to be when control LEAVES, so
\ the walk gives it the declared register at its definition when that register is
\ free - which costs nothing and is why an ordinary routine emits no extra
\ instruction - and when it is not free, or when the value is an argument already
\ pinned somewhere else, or when its register is a tied field, the value is
\ placed like any other and the walk plans a register-to-register move in front
\ of the return. A move is a decision the same way a spill is: this pass
\ publishes it and src/compiler/native/spill.f builds the module in which it is
\ an operation.
\
\ WHICH REGISTERS MAY BE USED, AND WHY THERE IS NO LIST OF THEM HERE. The
\ routine's own contract says which general registers it may write - the ones it
\ destroys together with the ones it returns a value in - and a value living in a
\ register is exactly the routine writing it. The allocatable pool is therefore
\ that declared set and nothing else. There is no literal list of
\ register numbers in this file: x18, x30 and register 31 are excluded because
\ src/compiler/a64-effect.f refuses them in any general-register set at all, so
\ no contract that names one can be built, and a forged contract is rejected when
\ this pass revalidates it. A reserved register is out of reach here by
\ construction rather than by a check that could be forgotten.
\
\ RUNNING OUT OF REGISTERS IS A DECISION, NOT A REFUSAL. A straight-line block
\ can hold more values at once than any register file has - a long chain of
\ literals proves it - so the bound cannot be proved away and spilling is the
\ answer. A spill is a store into a slot of the routine's own frame and a load
\ back out of it, and the A64IR dialect has both, so this pass decides where they
\ go instead of refusing the program. Two refusals are left, and neither is
\ register pressure: E-A64RA-PRESSURE is now only the routine's declared frame
\ running out of slots, and E-A64RA-POOL is the one shape no spill can serve - an
\ operation that needs more registers at a single instant than the routine may
\ destroy, with every register already holding a value that same operation reads.
\ A routine that may destroy nothing is the smallest example.
\
\ THE COST RULE, AND WHY IT IS STATED RATHER THAN TUNED. When a register has to
\ be taken from a value, the value taken is the one whose next read is furthest
\ away: a store bought now then buys the most operations before a reload is
\ needed, which is the classic furthest-next-use rule. Two values whose next
\ reads are equally far - two values never read again, for instance - are
\ separated by the lower register number, so one program always allocates one
\ way and a fixture can assert the exact registers. A value is stored once, when
\ its register is taken, and reloaded once before each operation that reads it
\ afterwards; a use before that point still reads the register, because the
\ register still holds the value there.
\
\ THE PASS DECIDES SPILLS; IT STILL REWRITES NOTHING. A spill decision is a pair
\ of instructions, and instructions live in a module, and a frozen module cannot
\ gain one. So this pass publishes the decisions the same way it publishes
\ registers - as claims about the module it read - and src/compiler/native/
\ spill.f is what builds the module in which those decisions are real operations.
\ The register claims of a walk that decided any spill are NOT an assignment for
\ the module it read, and they are not meant to be: two values will share a
\ register there, because one of them is in a slot for part of its life and the
\ module does not say so. The validator refuses exactly that, which is what makes
\ "allocate, lower the spills, allocate the result" the only way to reach an
\ accepted answer rather than a convention. The second walk decides no spill: it
\ reads a module whose operations already are the ones the first walk assumed.
\
\ TWO VALUE CLASSES. A value of the machine dialect is a general register or the
\ memory token the frame forms thread, and this pass reads which by type, from
\ the two identities the dialect answered at binding time. A token lives in no
\ register, so it is measured like every other value - it has a definition and a
\ last use, and the walk covers it - and given none. A value of any third type is
\ refused: this pass has no class for it.
\
\ WHICH REGISTERS THE POOL IS, NOW THAT SOME OF THEM ARE RESULTS. The pool is
\ every register the routine may WRITE, which the contract answers as
\ A64EFF:GPR-WRITABLE: the ones it destroys plus the ones it returns a value in.
\ The destroyed set alone would be wrong, because a register holding a result is
\ deliberately not in it - one register cannot be in both roles - and a routine
\ that could not write the register its result leaves in could not compute the
\ result at all.
\
\ A PLACE THAT IS NOT A REGISTER IS NOT THIS PASS'S BUSINESS. A convention names
\ a PLACE per position, and a place is a register or a slot of the caller's data
\ stack (design section 7.6). A register place is a constraint on the allocation
\ and is pre-coloured here. A data-stack place is not: the selector turned it
\ into a load at the top of the block or a store in front of the return, so by
\ the time this pass runs the value it names is an ordinary value of the module
\ with no fixed register at all. So the two lists are read for their REGISTER
\ positions, and a side declared in slots contributes none. Two things are
\ refused rather than allocated around: a side that mixes the kinds, which no
\ pass of this chain has a lowering for, and a side declared in slots on a module
\ that still carries its interface as block arguments or terminator operands -
\ which means the lowering never happened, and allocating it would leave the
\ arguments sitting in registers nobody put them in.
\
\ WHAT THE ALLOCATION IS, AND WHO MAY READ IT. The product is not a new module:
\ nothing about the operations changes, only which register each value sits in.
\ It is a side table keyed by the value's own module-local ordinal, sealed when
\ the walk finishes, and bound to the module it was computed from. Nothing here
\ answers "which register holds this value" to a consumer that wants to emit
\ code: this package only ever publishes a CLAIM. The claim becomes an answer
\ when src/compiler/native/regalloc-verify.f has independently checked it against
\ the module, and that file is the only door to the checked answer. An allocator
\ that certified its own output would be checking its belief against itself.
\
\ ONE ALLOCATION AT A TIME. The tables are fixed package-owned slots rather than
\ heap objects, so this pass allocates one block at a time - the single-task
\ compilation discipline the rest of the native chain keeps. Each walk raises a
\ generation counter, so an acceptance of an earlier walk cannot be read as an
\ acceptance of this one.

require lib/prelude.f
require lib/errors.f
require src/compiler/target.f
require src/compiler/binding.f
require src/compiler/a64-effect.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/schema.f
require src/compiler/ir/op.f
require src/compiler/ir/fun.f
require src/compiler/ir/build.f
require src/compiler/native/a64ir.f
require src/compiler/native/frozen.f

package A64RA
using NFROZEN
private

\ ---- the bound dialect -------------------------------------------------------
\ A module's types are its own ordinals, so "is this value a general register"
\ cannot be answered from outside without either the dialect's own authority or a
\ restatement of its spellings. Restating them would be a second authority that
\ drifts, so this pass asks A64IR itself while the module is still being built,
\ and keeps the identity it answers.
0 constant BOUND-NO
1 constant BOUND-YES

\ ---- how much of one block this pass holds -----------------------------------
\ Spill decisions in one block. Each one is an operation the lowering pass will
\ insert: one store per value that loses its register, one load before each
\ operation that reads it afterwards. A block of VMAX values has fewer than VMAX
\ reads per value, but a ceiling that says so exactly would be a product of two
\ ceilings; this is the flat one both this pass and the lowering pass carry.
1024 constant PLMAX

\ The three kinds of decision.
0 constant P-STORE
1 constant P-RELOAD
2 constant P-MOVE                    \ a returned value put where it has to leave

\ The two value classes this dialect has.
0 constant C-GPR
1 constant C-TOKEN

\ This value is in no slot.
-1 constant NOSLOT

\ The register file, taken from the schema that owns the machine facts.
A64EFF:FILE-SIZE constant REGS-N

\ The position of a block argument: before every operation of the block.
-1 constant ENTRY

\ Nothing holds this register.
-1 constant NOBODY

\ This result shares its register field with no operand.
-1 constant UNTIED

\ Positions one side of a calling convention can name, which is the contract's
\ own bound rather than a second one.
A64EFF:SEQ-LIMIT constant FIXED-MAX

\ ---- allocation state --------------------------------------------------------
0 constant ST-EMPTY
1 constant ST-SEALED

here CELL 1- and CELL swap - CELL 1- and allot
variable BND-MODE
BOUND-NO BND-MODE !
variable ST
ST-EMPTY ST !
variable GEN-N
0 GEN-N !
variable N-VALS
0 N-VALS !
variable N-OPS
0 N-OPS !
variable N-PLAN
0 N-PLAN !
variable N-SLOTS
0 N-SLOTS !
variable FRAME-N
0 FRAME-N !
variable RL-N
0 RL-N !
variable ARGS-N
0 ARGS-N !
variable OUTS-N
0 OUTS-N !

1 TYPED-BUFFER BND-MOD IR-ID:ir-module-id
1 TYPED-BUFFER BND-TYP IR-ID:ir-type-id
1 TYPED-BUFFER BND-MEM IR-ID:ir-type-id
1 TYPED-BUFFER BND-SLOT IR-ID:ir-symbol-id

1 TYPED-BUFFER S-MOD IR-ID:ir-module-id
1 TYPED-BUFFER S-BLK IR-ID:ir-block-id
1 TYPED-BUFFER S-POOL A64EFF:gprs

create V-DEF VMAX cells allot
create V-LAST VMAX cells allot
create V-REG VMAX cells allot
create V-SET VMAX cells allot
create V-CLS VMAX cells allot
create V-SLOT VMAX cells allot
create V-WANT VMAX cells allot
create A-REG FIXED-MAX cells allot
create O-REG FIXED-MAX cells allot
create R-HOLD REGS-N cells allot
create R-PIN REGS-N cells allot
create R-RL REGS-N cells allot
create PL-POS PLMAX cells allot
create PL-KIND PLMAX cells allot
create PL-VAL PLMAX cells allot

\ ---- the slots, read back ----------------------------------------------------
: BLK ( -- IR-ID:ir-block-id )       0 S-BLK @ ;
: POOL-BITS ( -- n )                 0 S-POOL @ A64EFF:GPRS-N ;

\ ---- the per-value tables ----------------------------------------------------
\ Every table is keyed by the value's own module-local ordinal, so a value of
\ another module cannot index one: the ordinal is checked against the count this
\ module records before it is used.
: SLOT ( IR-ID:ir-value-id -- n )
   IR-ID:VALUE-LOCAL
   dup 0 < over VMAX >= or if E-A64RA-CAP throw then ;

: DEF-AT ( n -- n )                  cells V-DEF + @ ;
: LAST-AT ( n -- n )                 cells V-LAST + @ ;
: REG-AT ( n -- n )                  cells V-REG + @ ;
: SET-AT ( n -- n )                  cells V-SET + @ ;
: CLS-AT ( n -- n )                  cells V-CLS + @ ;
: SLOT-AT ( n -- n )                 cells V-SLOT + @ ;
: WANT-AT ( n -- n )                 cells V-WANT + @ ;

: DEF! ( n n -- )                    {: v:n k:n :} v k cells V-DEF + ! ;
: LAST! ( n n -- )                   {: v:n k:n :} v k cells V-LAST + ! ;
: REG! ( n n -- )                    {: v:n k:n :} v k cells V-REG + ! ;
: SET! ( n n -- )                    {: v:n k:n :} v k cells V-SET + ! ;
: CLS! ( n n -- )                    {: v:n k:n :} v k cells V-CLS + ! ;
: SLOT! ( n n -- )                   {: v:n k:n :} v k cells V-SLOT + ! ;
: WANT! ( n n -- )                   {: v:n k:n :} v k cells V-WANT + ! ;

: HOLD-AT ( n -- n )                 cells R-HOLD + @ ;
: HOLD! ( n n -- )                   {: v:n r:n :} v r cells R-HOLD + ! ;

\ A register a reload or a result of the operation being allocated has just been
\ given cannot be taken away again by the same operation: that operation needs
\ what is in it now. The pins are cleared before each operation.
: PINNED? ( n -- bool )              cells R-PIN + @ 0<> ;
: PIN! ( n -- )                      1 swap cells R-PIN + ! ;

: PINS-CLEAR ( -- )
   REGS-N 0 ?do 0 i cells R-PIN + ! loop
   0 RL-N ! ;

: TABLES-CLEAR ( -- )
   VMAX 0 ?do
      0 i SET!
      ENTRY i DEF!
      ENTRY i LAST!
      NOBODY i REG!
      C-GPR i CLS!
      NOSLOT i SLOT!
      NOBODY i WANT!
   loop
   REGS-N 0 ?do NOBODY i HOLD! loop
   PINS-CLEAR
   0 N-PLAN !
   0 N-SLOTS ! ;

\ ---- the spill plan ----------------------------------------------------------
\ One row per operation the lowering pass has to insert, in the order the walk
\ decided them, each anchored to the operation it goes in front of.
: PLAN+ ( n n n -- )
   {: kind:n pos:n k:n :}
   N-PLAN @ {: j:n :}
   j PLMAX >= if E-A64RA-CAP throw then
   pos j cells PL-POS + !
   kind j cells PL-KIND + !
   k j cells PL-VAL + !
   j 1+ N-PLAN ! ;

\ Is this value already being reloaded in front of this operation? One reload
\ serves every read of one value by one operation, so an operation that reads a
\ spilled value twice takes one register for it and not two.
: RELOADED? ( n n -- bool )
   {: k:n pos:n :}
   false
   N-PLAN @ 0 ?do
      i cells PL-KIND + @ P-RELOAD =
      i cells PL-POS + @ pos = and
      i cells PL-VAL + @ k = and
      if drop true leave then
   loop ;

\ ---- reading the frozen module -----------------------------------------------
\ The operation control leaves the block through. Its operands are the values the
\ routine returns, so it is where the contract's result declaration is decided.
\ It is read off the block's own row rather than taken as the last operation:
\ which operation terminates a block is the block's recorded fact.
: TERM-OF ( IR-ID:ir-block-id -- IR-ID:ir-op-id )
   {: bk:IR-ID:ir-block-id :}
   V-BLKR VW V-OPR VW MKEY bk IR-FUN:FTERMINATOR@ ;

\ ---- the register constraints this operation's form declares -----------------
\ The schema table of the module being allocated is the authority on the shape of
\ every form in it, ties included, so these three readers are the whole of what
\ this pass knows about which registers an operation may be given.
: TIES-AT ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   V-SCHR VW id OPCODE-AT IR-SCHEMA:FTIES ;

: TIE-RESULT-AT ( IR-ID:ir-op-id n -- n )
   {: id:IR-ID:ir-op-id i:n :}
   V-SCHP VW V-SCHR VW id OPCODE-AT i IR-SCHEMA:FTIE-RESULT@ ;

: TIE-OPERAND-AT ( IR-ID:ir-op-id n -- n )
   {: id:IR-ID:ir-op-id i:n :}
   V-SCHP VW V-SCHR VW id OPCODE-AT i IR-SCHEMA:FTIE-OPERAND@ ;

\ Which operand this result shares a register field with, or UNTIED when the form
\ ties it to none.
: TIED-TO ( IR-ID:ir-op-id n -- n )
   {: id:IR-ID:ir-op-id rs:n :}
   UNTIED
   id TIES-AT 0 ?do
      id i TIE-RESULT-AT rs = if
         drop id i TIE-OPERAND-AT leave
      then
   loop ;

\ ---- the two value classes this dialect has ----------------------------------
\ A general register, or the memory token the frame forms thread. Both identities
\ came from the dialect itself at binding time, so nothing here compares
\ spellings or knows which opcode produced the value. A value of any third type
\ has no class here and is refused rather than given a register.
: CLASS-OF ( IR-ID:ir-value-id -- n )
   {: id:IR-ID:ir-value-id :}
   id VALUE-TYPE-AT {: t:IR-ID:ir-type-id :}
   t 0 BND-TYP @ SAME-TYPE? if C-GPR exit then
   t 0 BND-MEM @ SAME-TYPE? if C-TOKEN exit then
   E-A64RA-CLASS throw ;

\ ---- pass one: where each value is written, and where it is last read ---------
\ A definition is recorded once - a second one means the walk is not reading an
\ SSA module - and a use is recorded as the position of the operation that makes
\ it, which is monotonic because the walk runs forwards.
: DEFINE ( IR-ID:ir-value-id n -- )
   {: id:IR-ID:ir-value-id pos:n :}
   id CLASS-OF {: cls:n :}
   id SLOT {: k:n :}
   k SET-AT 0<> if E-A64RA-SHAPE throw then
   1 k SET!
   cls k CLS!
   pos k DEF!
   pos k LAST! ;

: USE ( IR-ID:ir-value-id n -- )
   {: id:IR-ID:ir-value-id pos:n :}
   id SLOT {: k:n :}
   k SET-AT 0= if E-A64RA-SHAPE throw then
   pos k LAST! ;

: DEFS-OF-OP ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id pos:n :}
   id RESULTS-OF {: n:n :}
   n 0 ?do id i RESULT-AT pos DEFINE loop ;

: USES-OF-OP ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id pos:n :}
   id OPERANDS-OF {: n:n :}
   n 0 ?do id i OPERAND-AT pos USE loop ;

: SCAN-ARGS ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   bk ARG-COUNT {: n:n :}
   n 0 ?do
      bk i ARG-AT ENTRY DEFINE
   loop ;

: SCAN-LIVE ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   bk SCAN-ARGS
   bk OP-COUNT {: n:n :}
   n N-OPS !
   n 0 ?do
      bk i OP-AT {: id:IR-ID:ir-op-id :}
      id i USES-OF-OP
      id i DEFS-OF-OP
   loop ;

\ Every value the module holds has to be a value of this one block, or the walk
\ has read only part of the program it is allocating for.
: COVER-CK ( -- )
   V-VALR VW IR-OP:FVALUES {: n:n :}
   n VMAX > if E-A64RA-CAP throw then
   n 0 ?do i SET-AT 0= if E-A64RA-SHAPE throw then loop
   n N-VALS ! ;

\ ---- pass two: the scan ------------------------------------------------------
\ Free every register whose value is dead before this position. Called with the
\ position just after the operation being allocated, so a value read by that
\ operation releases its register to the value the same operation writes.
: EXPIRE ( n -- )
   {: limit:n :}
   REGS-N 0 ?do
      i HOLD-AT {: v:n :}
      v NOBODY <> if
         v LAST-AT limit < if NOBODY i HOLD! then
      then
   loop ;

: POOL-HAS? ( n -- bool )
   {: r:n :}
   POOL-BITS 1 r lshift and 0<> ;

\ The lowest-numbered register of the pool that holds nothing, or -1 when every
\ one of them is taken. Lowest rather than next-around, so the same block always
\ allocates the same way.
: FREE-REG ( -- n )
   -1
   REGS-N 0 ?do
      i POOL-HAS? i HOLD-AT NOBODY = and if drop i leave then
   loop ;

\ Nothing below hands out a register that is not the routine's: FREE-REG only
\ answers one of the pool, and a register taken from a value was one of the pool
\ when that value got it. The check is here because a register outside the
\ contract would be the routine destroying something it promised to keep, and
\ that must fail closed rather than be argued about.
: TAKE ( n n -- )
   {: k:n r:n :}
   r POOL-HAS? 0= if E-A64RA-POOL throw then
   r k REG!
   k r HOLD! ;

\ ---- the routine's own fixed registers ---------------------------------------
\ The contract's two ordered lists, read once into tables this walk can index by
\ position. Nothing here decides anything about them: the contract already
\ refused a list that could not be a convention - a place no caller could use, or
\ one place at two positions - so what is left to judge is whether THIS allocation
\ can honour them.

\ How many positions of one side are register places, which is how many of them
\ constrain this allocation. A side declared entirely in data-stack slots
\ constrains nothing: the selector already turned every one of those places into
\ a load or a store, so the values are ordinary values by the time they reach
\ here. A side that mixes the two kinds is refused, because pairing position i of
\ a mixed list with argument i of a module some of whose arguments are no longer
\ arguments is a rule nothing in this chain has.
: REG-POSITIONS ( A64EFF:placeseq -- n )
   {: s:A64EFF:placeseq :}
   s A64EFF:SEQ-LEN {: len:n :}
   s A64EFF:SEQ-SLOTS {: sl:n :}
   sl 0= if len exit then
   sl len <> if E-A64RA-PLACE throw then
   0 ;

: FIXED! ( A64EFF:placeseq A64EFF:placeseq -- )
   {: args:A64EFF:placeseq outs:A64EFF:placeseq :}
   args REG-POSITIONS ARGS-N !
   outs REG-POSITIONS OUTS-N !
   ARGS-N @ 0 ?do args i A64EFF:SEQ-REG@  i cells A-REG + ! loop
   OUTS-N @ 0 ?do outs i A64EFF:SEQ-REG@  i cells O-REG + ! loop ;

\ A declared register the routine may not write is a contract that contradicts
\ itself for this allocation: the argument could not be held and the result could
\ not be computed. It is refused before a single value is placed. The two halves
\ are not equally reachable and say so: an argument register outside the pool is
\ an ordinary declaration mistake, while ALLOCATE derives the pool from the same
\ contract and always puts the result registers in it, so the second loop can
\ only answer for a caller that drives WALK with a pool of its own.
: FIXED-POOL-CK ( -- )
   ARGS-N @ 0 ?do
      i cells A-REG + @ POOL-HAS? 0= if E-A64RA-FIXED throw then
   loop
   OUTS-N @ 0 ?do
      i cells O-REG + @ POOL-HAS? 0= if E-A64RA-FIXED throw then
   loop ;

\ A convention that names more positions than the routine has arguments, or more
\ than it returns values, is not this routine's convention.
: FIXED-ARITY-CK ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   bk ARG-COUNT ARGS-N @ < if E-A64RA-FIXED throw then
   bk TERM-OF OPERANDS-OF OUTS-N @ < if E-A64RA-FIXED throw then ;

\ A side declared in data-stack slots is a side the module no longer carries in
\ registers at all: the selector turned each place into a load at the top of the
\ block or a store in front of the return, so the block has no argument and the
\ terminator no operand for it. A module that still carries them has not been
\ through that step, and allocating it would hand the arguments registers no
\ caller ever wrote to.
: LOWERED-CK ( IR-ID:ir-block-id A64EFF:placeseq A64EFF:placeseq -- )
   {: bk:IR-ID:ir-block-id args:A64EFF:placeseq outs:A64EFF:placeseq :}
   args A64EFF:SEQ-SLOTS 0<> bk ARG-COUNT 0<> and
   if E-A64RA-PLACE throw then
   outs A64EFF:SEQ-SLOTS 0<> bk TERM-OF OPERANDS-OF 0<> and
   if E-A64RA-PLACE throw then ;

\ Which register each returned value has to be in where control leaves. A value
\ returned at two declared positions would have to be in two registers at once,
\ and copying it into the second is a lowering this pass does not have (dot
\ habu-lower-parallel-copies-cdf9720e), so it is refused rather than half-served.
: WANTS! ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   bk TERM-OF {: id:IR-ID:ir-op-id :}
   OUTS-N @ 0 ?do
      id i OPERAND-AT SLOT {: k:n :}
      k WANT-AT NOBODY <> if E-A64RA-FIXED throw then
      k CLS-AT C-TOKEN = if E-A64RA-FIXED throw then
      i cells O-REG + @  k WANT!
   loop ;

\ ---- the cost rule -----------------------------------------------------------
\ Does this operation read this value?
: READS? ( IR-ID:ir-op-id n -- bool )
   {: id:IR-ID:ir-op-id k:n :}
   false
   id OPERANDS-OF 0 ?do
      id i OPERAND-AT SLOT k = if drop true leave then
   loop ;

\ The position of the first operation at or after `from` that reads this value. A
\ value nothing reads again answers the operation count, which is past every
\ position, so "furthest next use" puts the values nobody wants last without a
\ second rule for them.
: NEXT-USE ( n n -- n )
   {: k:n from:n :}
   N-OPS @ {: n:n :}
   n
   n from 0 max ?do
      BLK i OP-AT k READS? if drop i leave then
   loop ;

\ May this register be taken away? Not if it holds nothing, not if the operation
\ being allocated has already been given it, and not if the operation named by
\ `spare` is about to read it - taking it then would destroy the value before the
\ instruction that needs it runs. ENTRY spares no operation.
: EVICTABLE? ( n n -- bool )
   {: r:n spare:n :}
   r HOLD-AT {: v:n :}
   v NOBODY = if false exit then
   r PINNED? if false exit then
   spare ENTRY = if true exit then
   BLK spare OP-AT v READS? 0= ;

: FURTHEST ( n n -- n )
   {: spare:n pos:n :}
   -1
   REGS-N 0 ?do
      i spare EVICTABLE? if
         i HOLD-AT pos 1+ 0 max NEXT-USE {: c:n :}
         c over > if drop c then
      then
   loop ;

\ The register the next value takes. The lowest free one if there is one;
\ otherwise the one whose value is read furthest away, with the lowest register
\ number breaking a tie. A shape where nothing can be taken is the one register
\ pressure no spill can serve.
: VICTIM ( n n -- n )
   {: spare:n pos:n :}
   spare pos FURTHEST {: want:n :}
   want 0 < if E-A64RA-POOL throw then
   -1
   REGS-N 0 ?do
      i spare EVICTABLE? if
         i HOLD-AT pos 1+ 0 max NEXT-USE want = if drop i leave then
      then
   loop
   dup 0 < if E-A64RA-POOL throw then ;

\ ---- taking a register away --------------------------------------------------
\ The next slot of the routine's frame. Slots are handed out in order and never
\ handed out twice, so no two values ever share one; a frame with no room for the
\ next slot is what is left of register pressure as a refusal.
: NEW-SLOT ( -- n )
   N-SLOTS @ A64IR:SLOT-WIDTH * {: off:n :}
   off A64IR:SLOT-WIDTH + FRAME-N @ > if E-A64RA-PRESSURE throw then
   N-SLOTS @ 1+ N-SLOTS !
   off ;

\ Put the value in this register away, in front of the operation at `pos`. The
\ store reads the register, so the value is still there for an operation at `pos`
\ that reads it; from `pos` on, the register is free and every later read of the
\ value is a reload.
: EVICT ( n n -- )
   {: r:n pos:n :}
   r HOLD-AT {: k:n :}
   NEW-SLOT k SLOT!
   P-STORE pos 0 max k PLAN+
   NOBODY r HOLD! ;

: GRAB ( n n -- n )
   {: spare:n pos:n :}
   FREE-REG {: r:n :}
   r 0 >= if r exit then
   spare pos VICTIM {: v:n :}
   v pos EVICT
   v ;

\ Where a value goes when it is written. A value the contract says leaves in a
\ named register takes that register when nothing holds it, which is what makes
\ an ordinary return cost no instruction at all; when something does hold it, the
\ value is placed like any other and the walk plans a move at the return.
\
\ The one shape refused here is a register held by ANOTHER value the same return
\ has to deliver. Moving that one out of the way first is a parallel copy, and
\ two moves that each need the other's register need a temporary or an exchange -
\ neither of which this pass has (dot habu-lower-parallel-copies-cdf9720e). It
\ cannot arise while a convention names one returned value, because then only one
\ value wants anything.
: PLACE ( n n -- n )
   {: k:n pos:n :}
   k WANT-AT {: want:n :}
   want NOBODY <> if
      want HOLD-AT {: held:n :}
      held NOBODY = if want exit then
      held WANT-AT NOBODY <> if E-A64RA-FIXED throw then
   then
   ENTRY pos GRAB ;

: ASSIGN ( IR-ID:ir-value-id n -- )
   {: id:IR-ID:ir-value-id pos:n :}
   id SLOT {: k:n :}
   k CLS-AT C-TOKEN = if exit then
   k pos PLACE {: r:n :}
   k r TAKE
   pos ENTRY <> if r PIN! then ;

\ A block argument the contract gives a register arrives in exactly that one. It
\ is taken before the scan hands anything out, so nothing can be holding it: the
\ arguments are the first values placed and one register is one position, so the
\ second refusal below is fail-closed rather than reachable. The first is not:
\ the memory token the frame forms thread lives in no register, and a module that
\ made one a block argument would be declaring a convention for something a
\ caller cannot put anywhere.
: PIN-ARG ( IR-ID:ir-value-id n -- )
   {: id:IR-ID:ir-value-id r:n :}
   id SLOT {: k:n :}
   k CLS-AT C-TOKEN = if E-A64RA-FIXED throw then
   r HOLD-AT NOBODY <> if E-A64RA-FIXED throw then
   k r TAKE ;

\ A tied result lands in the register field its operand already occupies, so that
\ field has to be free the moment this operation writes. Everything that dies
\ here has just been released, so a field that is still held means one of exactly
\ two things, and neither can be given registers: the kept value is read again
\ after the operation that overwrites it, or another tie of this same operation
\ has already taken the field, which is what happens when a form is handed one
\ value as two of its tied operands. Both are refused by the same name, because
\ both say one register field would have to hold two values at once.
\
\ An operand that was spilled lends the register its reload landed in, because
\ that is the register the instruction really names: the reload is a value of its
\ own in the module this walk is planning, it dies at this operation, and the
\ tied result takes its place. So a tie over a value that has to survive is
\ legal once that value is in a slot, and it is refused only when the value is
\ still in the register.
\
\ An operand holding no register cannot lend one. SSA puts every definition
\ before its uses, so the walk has already given it one and this cannot happen;
\ reading the holder table at a negative index is not how we would find out.
: TIE ( IR-ID:ir-op-id n n -- )
   {: id:IR-ID:ir-op-id rs:n op:n :}
   id op OPERAND-AT SLOT REG-AT {: r:n :}
   r 0 < r REGS-N >= or if E-A64RA-TIE throw then
   r HOLD-AT NOBODY <> if E-A64RA-TIE throw then
   id rs RESULT-AT SLOT  r  TAKE
   r PIN! ;

: ASSIGN-RESULT ( IR-ID:ir-op-id n n -- )
   {: id:IR-ID:ir-op-id rs:n pos:n :}
   id rs TIED-TO {: op:n :}
   op UNTIED = if
      id rs RESULT-AT pos ASSIGN exit
   then
   id rs op TIE ;

\ ---- the reloads one operation needs -----------------------------------------
\ A value that lost its register is read out of its slot again, once for this
\ operation however many of its operands name it. The register the reload lands
\ in has to survive until the operation reads it, so nothing this operation reads
\ may be taken to make room - that is what `pos` as the spared operation says -
\ and it is given back the moment the operation has read it.
: RELOAD-REG ( n -- )
   {: r:n :}
   RL-N @ {: j:n :}
   j REGS-N >= if E-A64RA-CAP throw then
   r j cells R-RL + !
   j 1+ RL-N ! ;

: RELOADS-FREE ( -- )
   RL-N @ 0 ?do
      NOBODY i cells R-RL + @ HOLD!
   loop
   0 RL-N ! ;

: OP-RELOADS ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id pos:n :}
   id OPERANDS-OF {: n:n :}
   n 0 ?do
      id i OPERAND-AT SLOT {: k:n :}
      k SLOT-AT NOSLOT <> k pos RELOADED? 0= and if
         pos pos GRAB {: r:n :}
         P-RELOAD pos k PLAN+
         k r HOLD!
         r k REG!
         r PIN!
         r RELOAD-REG
      then
   loop ;

\ One operation, in the order the machine runs it: the values it reads are put
\ where it can read them, then everything that dies here gives its register back,
\ then the values it writes are given registers - which is what lets a result
\ land in a register its own operand has just vacated.
: ASSIGN-OP ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id pos:n :}
   PINS-CLEAR
   id pos OP-RELOADS
   RELOADS-FREE
   pos 1+ EXPIRE
   id RESULTS-OF {: n:n :}
   n 0 ?do id i pos ASSIGN-RESULT loop ;

\ Every returned value the contract named a register for is in it where control
\ leaves, or the walk plans the move that puts it there. This is decided after
\ the whole scan, because it is a statement about where the values ARE at the
\ return - a value that was spilled and read back is in the register its reload
\ landed in, not the one it was computed in, and only the finished scan knows
\ which that is.
: RETURN-CK ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   bk TERM-OF {: id:IR-ID:ir-op-id :}
   OUTS-N @ 0 ?do
      id i OPERAND-AT SLOT {: k:n :}
      k REG-AT  i cells O-REG + @  <> if
         P-MOVE  N-OPS @ 1-  k  PLAN+
      then
   loop ;

: SCAN-ASSIGN ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   PINS-CLEAR
   bk ARG-COUNT {: n:n :}
   n 0 ?do
      bk i ARG-AT {: a:IR-ID:ir-value-id :}
      i ARGS-N @ < if
         a  i cells A-REG + @  PIN-ARG
      else
         a ENTRY ASSIGN
      then
   loop
   bk OP-COUNT {: k:n :}
   k 0 ?do bk i OP-AT i ASSIGN-OP loop
   bk RETURN-CK ;

\ ---- what one allocation run is told -----------------------------------------
\ The straight-line subset is one function of one block; any other shape means
\ control flow, and control flow has no allocation rule here yet.
: BLOCK-OF ( -- IR-ID:ir-block-id )
   FUN-COUNT 1 <> if E-A64RA-SHAPE throw then
   MKEY 0 IR-ID:PACK-FUN {: f:IR-ID:ir-fun-id :}
   f BLOCK-COUNT 1 <> if E-A64RA-SHAPE throw then
   f 0 BLOCK-AT ;

\ ---- the contract, read once -------------------------------------------------
\ A contract is a twelve-field value and a value of more than one cell cannot be
\ bound to a local, so a word that needs two of its fields takes them apart once
\ and a word that needs the whole contract again builds it back. Both readers
\ below revalidate what they are handed, which is what makes rebuilding it safe:
\ a forged contract cannot survive the round trip.
\ Every slot this walk handed out, measured against the routine that has to
\ address it. A64EFF owns that rule - a width its forms carry, an offset its
\ scale division will not round, an offset inside the declared frame and inside
\ the reach of the offset field - so the decision is made there and not here.
: SLOTS-CK ( A64EFF:routine -- )
   A64EFF:VALIDATE A64EFF-ROUTINE:UNMAKE
   {: gi:A64EFF:placeseq gr:A64EFF:placeseq gc:A64EFF:gprs
      fi:A64EFF:fprs fr:A64EFF:fprs fc:A64EFF:fprs
      z:A64EFF:nzcv l:A64EFF:link ct:A64EFF:control t:A64EFF:traits
      size:n delta:n :}
   N-SLOTS @ 0 ?do
      i A64IR:SLOT-WIDTH *  A64IR:SLOT-WIDTH
      gi gr gc fi fr fc z l ct t size delta A64EFF-ROUTINE:MAKE
      A64EFF:CHECK-SLOT
   loop ;

\ These registers belong to one architecture. A context bound to another target
\ describes a machine that has none of them.
: TARGET-CK ( IR-CTX:ctx -- )
   IR-CTX:BINDING@ CBIND:VALIDATE CBIND:TARGET@ CTARGET:ARCH@
   CTARGET-ARCH:AARCH64 CTARGET-ARCH:EQ
   0= if E-A64RA-TARGET throw then ;

\ The binding is taken whatever the outcome, so neither an allocation without a
\ binding nor a refused allocation can leave one behind for the next caller.
: BND-TAKE ( -- )
   BND-MODE @ {: have:n :}
   BOUND-NO BND-MODE !
   have BOUND-YES <> if E-A64RA-BIND throw then ;

: BND-MODULE-CK ( IR-BUILD:module -- )
   IR-BUILD:FMODULE  0 BND-MOD @  IR-ID:MODULE-SAME?
   0= if E-A64RA-MODULE throw then ;

\ A module whose schema table was created for another dialect, or for another
\ version of this one, holds operations whose register constraints this pass does
\ not know even if some of them happen to be spelled the same.
: DIALECT-CK ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b  c b IR-BUILD:DIALECT@  A64IR:NAME IR-BUILD:SYMBOL-IS?
   0= if E-A64RA-MODULE throw then
   c b IR-BUILD:SCHEMA-MAJOR@ A64IR:MAJOR <> if E-A64RA-MODULE throw then
   c b IR-BUILD:SCHEMA-MINOR@ A64IR:MINOR <> if E-A64RA-MODULE throw then ;

\ A reader answers only about the walk that is sealed now.
: SEAL-CK ( -- )
   ST @ ST-SEALED <> if E-A64RA-STATE throw then ;

: ORD-CK ( n -- n )
   dup 0 < over N-VALS @ >= or if E-A64RA-CAP throw then ;

public

\ ---- binding the dialect -----------------------------------------------------
\ Learn the identity of the module that is about to be allocated and the type of
\ its general register, while it is still being built. A module's types are its
\ own ordinals, so this is the only moment the dialect can be asked which type
\ its general register is; the answer stays valid after the module freezes
\ because freezing keeps the module's identity. The binding is spent by the next
\ ALLOCATE.
: BIND-DIALECT ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   BND-MODE @ BOUND-YES = if E-A64RA-BIND throw then
   c b DIALECT-CK
   b IR-BUILD:MODULE@ 0 BND-MOD !
   c b A64IR:GPR-TYPE 0 BND-TYP !
   c b A64IR:MEM-TYPE 0 BND-MEM !
   c b A64IR:KEY-SLOT 0 BND-SLOT !
   BOUND-YES BND-MODE ! ;

\ Give up a binding without allocating against it.
: RELEASE ( -- )
   BND-TAKE ;

\ ---- the pass ----------------------------------------------------------------
\ Allocate the whole of one frozen machine module against the contract of the
\ routine it is being emitted as. The contract's destroyed set is the pool, so a
\ routine that may destroy nothing allocates nothing; the walk seals a claim per
\ value, which src/compiler/native/regalloc-verify.f then has to accept before
\ anything may act on it.
: WALK ( IR-CTX:ctx IR-BUILD:module A64EFF:gprs A64EFF:placeseq A64EFF:placeseq n -- )
   {: c:IR-CTX:ctx m:IR-BUILD:module pool:A64EFF:gprs
      args:A64EFF:placeseq outs:A64EFF:placeseq frame:n :}
   BND-TAKE
   ST-EMPTY ST !
   m BND-MODULE-CK
   c TARGET-CK
   pool 0 S-POOL !
   frame FRAME-N !
   m VIEWS!
   m IR-BUILD:FMODULE 0 S-MOD !
   TABLES-CLEAR
   args outs FIXED!
   FIXED-POOL-CK
   BLOCK-OF {: bk:IR-ID:ir-block-id :}
   bk 0 S-BLK !
   bk FIXED-ARITY-CK
   bk args outs LOWERED-CK
   bk SCAN-LIVE
   COVER-CK
   bk WANTS!
   bk SCAN-ASSIGN ;

: ALLOCATE ( IR-CTX:ctx IR-BUILD:module A64EFF:routine -- )
   A64EFF:VALIDATE A64EFF-ROUTINE:UNMAKE
   {: gi:A64EFF:placeseq gr:A64EFF:placeseq gc:A64EFF:gprs
      fi:A64EFF:fprs fr:A64EFF:fprs fc:A64EFF:fprs
      z:A64EFF:nzcv l:A64EFF:link ct:A64EFF:control
      t:A64EFF:traits size:n delta:n :}
   gi gr gc fi fr fc z l ct t size delta A64EFF-ROUTINE:MAKE
   A64EFF:GPR-WRITABLE {: pool:A64EFF:gprs :}
   pool gi gr size WALK
   gi gr gc fi fr fc z l ct t size delta A64EFF-ROUTINE:MAKE SLOTS-CK
   GEN-N @ 1+ GEN-N !
   ST-SEALED ST ! ;

\ ---- the sealed allocation ---------------------------------------------------
\ Everything below answers about the walk that is sealed now, and nothing below
\ is the checked answer to "which register holds this value": these are the
\ allocator's claims, and regalloc-verify.f is what turns an accepted claim into
\ an answer.
: SEALED? ( -- bool )
   ST @ ST-SEALED = ;

\ Which walk this is. An acceptance records it, so an acceptance of an earlier
\ walk cannot be read as an acceptance of this one.
: GEN ( -- n )
   SEAL-CK GEN-N @ ;

: MODULE@ ( -- IR-ID:ir-module-id )
   SEAL-CK 0 S-MOD @ ;

: POOL ( -- A64EFF:gprs )
   SEAL-CK 0 S-POOL @ ;

\ The frame this walk allocated under, and how much of it the spills used. The
\ first is the contract's declaration and the second is what the program proved
\ it needs; nothing yet turns the second into the first, so a routine whose
\ author declared too small a frame is refused rather than given the frame it
\ needs (dot habu-derive-a-routine-84ed36b6).
: FRAME ( -- n )
   SEAL-CK FRAME-N @ ;

: FRAME-USED ( -- n )
   SEAL-CK N-SLOTS @ A64IR:SLOT-WIDTH * ;

: VALUES ( -- n )
   SEAL-CK N-VALS @ ;

: CLAIM@ ( n -- n )
   SEAL-CK ORD-CK REG-AT ;

: DEF@ ( n -- n )
   SEAL-CK ORD-CK DEF-AT ;

: LAST@ ( n -- n )
   SEAL-CK ORD-CK LAST-AT ;

\ ---- the spill decisions -----------------------------------------------------
\ How many values lost their register, and where each one went. A walk that
\ answers zero here decided no spill, and its register claims describe the module
\ it read; a walk that answers more decided a program the module does not yet
\ contain, and src/compiler/native/spill.f is what builds the one it does.
: SPILLS ( -- n )
   SEAL-CK N-SLOTS @ ;

: SLOT@ ( n -- n )
   SEAL-CK ORD-CK SLOT-AT ;

\ The decisions in the order they were made. Each one is an operation to insert
\ in front of the operation at its position: a store of the value out of the
\ register it is losing, or a load of it back before the operation that reads it.
: PLAN-N ( -- n )
   SEAL-CK N-PLAN @ ;

: PLAN-ORD-CK ( n -- n )
   dup 0 < over N-PLAN @ >= or if E-A64RA-CAP throw then ;

: PLAN-POS@ ( n -- n )
   SEAL-CK PLAN-ORD-CK cells PL-POS + @ ;

: PLAN-VALUE@ ( n -- n )
   SEAL-CK PLAN-ORD-CK cells PL-VAL + @ ;

: PLAN-STORE? ( n -- bool )
   SEAL-CK PLAN-ORD-CK cells PL-KIND + @ P-STORE = ;

\ A decision of the third kind: the value at this row has to be put into the
\ register the contract says it leaves in, by a register-to-register move in
\ front of the return. The register itself is not carried here - the lowered
\ module's own allocation reads it back off the same contract - so this row says
\ only which value has to be moved and where the move goes.
: PLAN-MOVE? ( n -- bool )
   SEAL-CK PLAN-ORD-CK cells PL-KIND + @ P-MOVE = ;

\ How many moves this walk decided. A walk that answers zero and spills nothing
\ decided a module that already is the one it read.
: MOVES ( -- n )
   SEAL-CK
   0
   N-PLAN @ 0 ?do
      i cells PL-KIND + @ P-MOVE = if 1+ then
   loop ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;using
;package
