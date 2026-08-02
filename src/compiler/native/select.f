\ select.f - instruction selection: read a frozen straight-line HIR module and
\ build the frozen A64IR module its operations select to.
\
\ docs/compiler-ir-design.md section 7.2's stage chain. src/compiler/native/
\ elaborate.f is the step that turns source into HIR operations; this is the step
\ that turns HIR operations into machine operations. Everything before it says
\ what the program computes, everything after it says which registers and bytes
\ compute it, and this file is the only place that turns one into the other.
\
\ MODULE IN, MODULE OUT. The input is a frozen module - so it has already been
\ through the whole structural verifier - and it is read only through the frozen
\ readers of the tables that own it. The output is built through the ordinary
\ staged builder and frozen, so it goes through that same verifier before this
\ word answers. Nothing here writes a cell of either module by any other route,
\ and nothing here re-checks a fact IR-OP, IR-FUN or IR-VERIFY already checks.
\
\ WHAT SELECTION IS, IN ONE SENTENCE. Each operation of the source module becomes
\ the machine operations that compute the same value, and each value of the source
\ module becomes the value the last of those operations defines. There is one
\ table below, the MATCH in RULE, and it is the whole selection rule:
\   hir.const  -> a64.movz, then one a64.movk per further non-zero half
\   hir.add    -> a64.add
\   hir.sub    -> a64.sub
\   hir.mul    -> a64.mul
\   hir.div    -> a64.sdiv
\   hir.lt     -> a64.flag, under the signed less-than condition
\   hir.le     -> a64.flag, under the signed less-than-or-equal condition
\   hir.gt     -> a64.flag, under the signed greater-than condition
\   hir.ge     -> a64.flag, under the signed greater-than-or-equal condition
\   hir.eq     -> a64.flag, under the equal condition
\   hir.ne     -> a64.flag, under the not-equal condition
\   hir.and    -> a64.and
\   hir.or     -> a64.orr
\   hir.xor    -> a64.eor
\   hir.lshift -> a64.lslv, the shift-by-register form, because Habu's shift
\                 takes its count off the stack
\   hir.rshift -> a64.lsrv, the logical one, which is what Habu's `rshift` is
\   hir.invert -> a64.mvn
\   hir.mem    -> no instruction: the order binds to the one a64.dtake minted
\   hir.load   -> a64.aldr
\   hir.store  -> a64.astr
\   hir.bload  -> a64.aldrb
\   hir.bstore -> a64.astrb
\   hir.call   -> one a64.dstore per value crossing the call, then a64.call,
\                 then one a64.dload per value coming back
\   hir.wordcall -> the same three runs with a64.wordcall in the middle, and the
\                 counts read off the callee's declared arity instead of this
\                 routine's own convention
\   hir.return -> a64.ret
\ An operand is not "the same position in the new operation"; it is the value the
\ source operand's own definition selected to, looked up in the value map. That is
\ what makes a wrongly wired operand a wrong VALUE rather than a wrong index, and
\ it is why the fixtures assert operand identity and not operand count.
\
\ ONE PAIR OF SOURCE OPERATIONS BECOMES ONE MACHINE OPERATION, AND IT IS THE ONLY
\ ONE. A comparison standing immediately above the hir.brz that tests it, whose
\ value the rest of the function never reads again, selects together with that
\ branch to a64.cmpbr - a compare and a conditional branch, three instructions
\ and no register, where the two rules above would give five instructions and
\ one. Every other comparison keeps a64.flag and every other two-way branch keeps
\ a64.cbz. The fusion section below states what it requires and why it lives in
\ this pass rather than in one of its own.
\
\ WHY A CONSTANT IS A CHAIN. ARM64 has no instruction that puts an arbitrary
\ 64-bit number into a register. It writes one sixteen-bit half at a time: movz
\ clears the register and writes a half, movk overwrites a half and keeps the
\ rest. A literal therefore selects to a movz for its lowest half followed by one
\ movk for each further half that is not already zero - always correct, always the
\ same instructions for the same number, and never more than four. Choosing which
\ half to start from to save an instruction is an optimisation, and an optimiser
\ is not this leaf.
\
\ WHY A TRAPPING OPERATION IS REFUSED UNLESS ITS RULE KEEPS THE TRAP. The source
\ dialect records in each schema's may-trap flag whether an operation can raise,
\ and a lowering that drops the raise is a wrong program rather than a faster
\ one. So this pass reads that flag and asks one more question of the opcode -
\ does the rule below reproduce the trap? Division does: a64.sdiv is the guard
\ and the divide together, exactly the three instructions the engine's own `/`
\ is, so a compiled division ends the process on a zero divisor where an
\ interpreted one does. Trapping arithmetic does not: ARM64's Add, Sub and Mul
\ wrap, and a trapping addition needs a flag-setting form, a conditional branch
\ and a trap target that the A64IR dialect has none of yet, so it is refused
\ rather than selected to a plain a64.add. That missing lowering is tracked as
\ its own capability; until it lands, a trapping unit does not select at all
\ rather than selecting wrongly.
\
\ HOW IT KNOWS WHICH OPCODE IS WHICH. An operation names its opcode with a symbol
\ of its own module, and a module's symbols are its own ordinals, so "is this
\ hir.add" cannot be answered from outside without either the source dialect's
\ authority or a restatement of its spellings. Restating them would be a second
\ authority that drifts, so this pass asks HIR itself: BIND-SOURCE takes the HIR
\ module while it is still being built, asks HIR:OPCODE for each member of HIR's
\ own opcode family, and keeps every identity it answers. Every spelling
\ stays HIR's; the pairing of an opcode to its machine operations is this file's,
\ and no caller can get it wrong because no caller supplies it. The binding
\ records which module it learned from, and SELECT refuses a frozen module that is
\ not that one, so "bind the module you are about to select" is a check rather
\ than a usage rule.
\
\ THE SOURCE TEXT IS PROVED, NOT TRUSTED. Every operation carries the span of the
\ source it came from, and a span names a source of its own module - so the new
\ module needs the same source registered in it. IR-SOURCE records a source as the
\ digest of its bytes, so the text presented to SELECT is the text the HIR module
\ was compiled from exactly when the two digests agree, and that is the check made
\ before a single span is rebuilt.
\
\ THE ROUTINE'S CALLING CONVENTION IS SELECTED HERE, AND WHY IT IS HERE. A
\ convention names a PLACE per argument and per returned value, and a place is a
\ register or a slot of the caller's data stack (src/compiler/a64-effect.f,
\ design section 7.6). A register place needs no instruction: the value arrives
\ in a register, and which register is the allocator's constraint, so it stays a
\ block argument and a terminator operand exactly as before. A data-stack place
\ needs three - the pointer moved down over the caller's operands, one load per
\ argument, one store per result and the pointer moved back up - and those are
\ INSTRUCTIONS, so they have to be in a module.
\
\ THE ONE-AUTHORITY RULE PUTS THEM IN THIS PASS AND NOT IN THE EMITTER. An
\ instruction the emitter materialised out of a contract would be an instruction
\ no module contains, so the independent register-allocation validator would have
\ nothing to re-derive it from and would end up checking the emitter's belief
\ against the emitter's belief - the same argument that made spill lowering build
\ a second module rather than let the emitter invent stores. It does NOT need a
\ second module here, which is the difference from spilling: a spill plan is the
\ ALLOCATOR's output and only exists after the module is frozen, while a routine's
\ convention is known before a single operation is selected. So this pass, which
\ is already the one that decides which machine operations a program becomes,
\ takes the contract and builds the entry and the exit as part of the module it
\ was going to build anyway. Everything downstream then reads one module that
\ contains its own interface.
\
\ AND A ROUTINE THAT CALLS GETS TWO MORE PIECES OF INTERFACE, FOR THE SAME
\ REASON. Its caller's return address is destroyed by the first call it makes, so
\ it reserves its own frame and puts x30 in it before the body and takes it back
\ before it returns; and every value the caller still holds at a call site is
\ written into the caller's data stack and read back out of it, because no
\ register survives a call to a routine whose contract destroys the whole pool.
\ Both are instructions, both are decided by the contract, and both therefore
\ belong in the module this pass builds rather than in the emitter.
\
\ THE ENTRY AND EXIT PAIR IS EMITTED WHOLE, EVEN WHEN A COUNT IS ZERO. A routine
\ whose convention uses the data stack always gets both the take and the publish,
\ so the chain of memory tokens has one beginning and one end and the validator
\ has one layout to re-derive. A word with arguments and no results therefore
\ ends with an addition of zero, and one with results and no arguments starts
\ with a subtraction of zero. Dropping those is a peephole, and a peephole is an
\ optimisation this leaf deliberately does not do - the same reason a constant is
\ always materialised from its lowest half up.
\
\ ONE SELECTION AT A TIME. The value map is a fixed package-owned slot rather than
\ a heap object, and the source module is read through the one cursor
\ src/compiler/native/frozen.f owns, so this pass selects one module at a time -
\ the single-task compilation discipline the rest of the compiler already keeps.
\ The whole walk is one call, so nothing a refused call left behind can be read by
\ the next one; the binding is separately taken at entry, so a refused selection
\ also leaves no binding for a later caller to select against by accident.

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
require src/compiler/a64-effect.f
require src/compiler/native/hir.f
require src/compiler/native/a64ir.f
require src/compiler/native/frame.f
require src/compiler/native/frozen.f

package A64SEL
using NFROZEN
private

\ ---- the bound source dialect ------------------------------------------------
\ One slot per member of the source dialect's opcode family, plus the attribute
\ key its constant carries and the module all six were learned from.
27 constant OPCODES-N
0 constant O-CONST
1 constant O-ADD
2 constant O-SUB
3 constant O-MUL
4 constant O-RETURN
5 constant O-LT
6 constant O-LE
7 constant O-BR
8 constant O-BRZ
9 constant O-MEM
10 constant O-LOAD
11 constant O-STORE
12 constant O-DIV
13 constant O-BLOAD
14 constant O-BSTORE
15 constant O-EQ
16 constant O-CALL
17 constant O-WORDCALL
18 constant O-GT
19 constant O-GE
20 constant O-NE
21 constant O-AND
22 constant O-OR
23 constant O-XOR
24 constant O-LSHIFT
25 constant O-RSHIFT
26 constant O-INVERT

0 constant BOUND-NO
1 constant BOUND-YES

\ The longest function name this pass can carry across. A name is copied out of
\ the source module's interner and interned into the new one, because the two
\ modules number their symbols separately.
128 constant NAME-CAP

here CELL 1- and CELL swap - CELL 1- and allot
variable BND-MODE
BOUND-NO BND-MODE !

1 TYPED-BUFFER BND-MOD IR-ID:ir-module-id
OPCODES-N TYPED-BUFFER BND-OP IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-VAL IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-ENTRY IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-IN IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-OUT IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-MEM IR-ID:ir-type-id

1 TYPED-BUFFER S-CTX IR-CTX:ctx
1 TYPED-BUFFER S-BLD IR-BUILD:builder
1 TYPED-BUFFER S-SID IR-ID:ir-source-id
1 TYPED-BUFFER S-ACC IR-ID:ir-value-id
1 TYPED-BUFFER S-TOK IR-ID:ir-value-id
1 TYPED-BUFFER S-ARGS A64EFF:placeseq
1 TYPED-BUFFER S-OUTS A64EFF:placeseq
1 TYPED-BUFFER S-TRT A64EFF:traits
1 TYPED-BUFFER S-FTOK IR-ID:ir-value-id
1 TYPED-BUFFER S-FUN IR-ID:ir-fun-id
1 TYPED-BUFFER S-BLK IR-ID:ir-block-id
variable S-FRAME                     \ the frame the contract declares, in bytes
variable N-CALLS                     \ calls this selection built
variable FUSE-AT                     \ where in this block the fused comparison is, or -1
VMAX TYPED-BUFFER VMAP IR-ID:ir-value-id
create VSET VMAX cells allot
create NAMEBUF NAME-CAP allot

\ ---- the slots, read back ----------------------------------------------------
: CTX ( -- IR-CTX:ctx )              0 S-CTX @ ;
: BLD ( -- IR-BUILD:builder )        0 S-BLD @ ;
: SID ( -- IR-ID:ir-source-id )      0 S-SID @ ;
: ACC ( -- IR-ID:ir-value-id )       0 S-ACC @ ;
: ACC! ( IR-ID:ir-value-id -- )      0 S-ACC ! ;
: TOK ( -- IR-ID:ir-value-id )       0 S-TOK @ ;
: TOK! ( IR-ID:ir-value-id -- )      0 S-TOK ! ;
: ARGS ( -- A64EFF:placeseq )        0 S-ARGS @ ;
: OUTS ( -- A64EFF:placeseq )        0 S-OUTS @ ;
: TRAITS ( -- A64EFF:traits )        0 S-TRT @ ;
: FRAME ( -- n )                     S-FRAME @ ;
: FTOK ( -- IR-ID:ir-value-id )      0 S-FTOK @ ;
: FTOK! ( IR-ID:ir-value-id -- )     0 S-FTOK ! ;
: FUN ( -- IR-ID:ir-fun-id )         0 S-FUN @ ;
: BLK ( -- IR-ID:ir-block-id )       0 S-BLK @ ;

\ ---- the source dialect's opcode family --------------------------------------
\ One injective slot per member, so the family stays exhaustive: a member added
\ to HIR:opcode makes both of these fail to compile until it has a slot and a
\ selection rule.
: SLOT-OF ( HIR:opcode -- n )
   MATCH HIR:opcode
      const  OF O-CONST  ENDOF
      add    OF O-ADD    ENDOF
      sub    OF O-SUB    ENDOF
      mul    OF O-MUL    ENDOF
      div    OF O-DIV    ENDOF
      lt     OF O-LT     ENDOF
      le     OF O-LE     ENDOF
      gt     OF O-GT     ENDOF
      ge     OF O-GE     ENDOF
      equal  OF O-EQ     ENDOF
      ne     OF O-NE     ENDOF
      and    OF O-AND    ENDOF
      or     OF O-OR     ENDOF
      xor    OF O-XOR    ENDOF
      lshift OF O-LSHIFT ENDOF
      rshift OF O-RSHIFT ENDOF
      invert OF O-INVERT ENDOF
      br     OF O-BR     ENDOF
      brz    OF O-BRZ    ENDOF
      mem    OF O-MEM    ENDOF
      load   OF O-LOAD   ENDOF
      store  OF O-STORE  ENDOF
      bload  OF O-BLOAD  ENDOF
      bstore OF O-BSTORE ENDOF
      call   OF O-CALL   ENDOF
      wordcall OF O-WORDCALL ENDOF
      return OF O-RETURN ENDOF
   ;MATCH ;

: SLOT-OPCODE ( n -- HIR:opcode )
   case
      O-CONST  of HIR-OPCODE:CONST  endof
      O-ADD    of HIR-OPCODE:ADD    endof
      O-SUB    of HIR-OPCODE:SUB    endof
      O-MUL    of HIR-OPCODE:MUL    endof
      O-DIV    of HIR-OPCODE:DIV    endof
      O-LT     of HIR-OPCODE:LT     endof
      O-LE     of HIR-OPCODE:LE     endof
      O-GT     of HIR-OPCODE:GT     endof
      O-GE     of HIR-OPCODE:GE     endof
      O-EQ     of HIR-OPCODE:EQUAL endof
      O-NE     of HIR-OPCODE:NE     endof
      O-AND    of HIR-OPCODE:AND    endof
      O-OR     of HIR-OPCODE:OR     endof
      O-XOR    of HIR-OPCODE:XOR    endof
      O-LSHIFT of HIR-OPCODE:LSHIFT endof
      O-RSHIFT of HIR-OPCODE:RSHIFT endof
      O-INVERT of HIR-OPCODE:INVERT endof
      O-BR     of HIR-OPCODE:BR     endof
      O-BRZ    of HIR-OPCODE:BRZ    endof
      O-MEM    of HIR-OPCODE:MEM    endof
      O-LOAD   of HIR-OPCODE:LOAD   endof
      O-STORE  of HIR-OPCODE:STORE  endof
      O-BLOAD  of HIR-OPCODE:BLOAD  endof
      O-BSTORE of HIR-OPCODE:BSTORE endof
      O-CALL   of HIR-OPCODE:CALL   endof
      O-WORDCALL of HIR-OPCODE:WORDCALL endof
      O-RETURN of HIR-OPCODE:RETURN endof
      E-A64SEL-OPCODE throw
   endcase ;

\ Which member of the source family this symbol names. A symbol that names none
\ of them is an operation this pass has no rule for, and it is refused rather
\ than skipped.
: OPCODE-SLOT ( IR-ID:ir-symbol-id -- n )
   {: sym:IR-ID:ir-symbol-id :}
   -1
   OPCODES-N 0 ?do
      sym i BND-OP @ SAME-SYM? if drop i leave then
   loop
   dup 0 < if E-A64SEL-OPCODE throw then ;

\ ---- the value map -----------------------------------------------------------
\ Which value of the new module a value of the source module selected to. It is
\ keyed by the source value's module-local ordinal, and a lookup of a value no
\ operation has defined yet refuses: a verified module defines every value before
\ it is used, so reaching one means the walk is not reading what it thinks.
: VCLEAR ( -- )
   VMAX 0 ?do
      0 i cells VSET + !
   loop ;

: VSLOT ( IR-ID:ir-value-id -- n )
   IR-ID:VALUE-LOCAL
   dup 0 < over VMAX >= or if E-A64SEL-CAP throw then ;

: VBIND ( IR-ID:ir-value-id IR-ID:ir-value-id -- )
   {: src:IR-ID:ir-value-id new:IR-ID:ir-value-id :}
   src VSLOT {: k:n :}
   new k VMAP !
   1 k cells VSET + ! ;

: VOF ( IR-ID:ir-value-id -- IR-ID:ir-value-id )
   VSLOT {: k:n :}
   k cells VSET + @ 0= if E-A64SEL-SHAPE throw then
   k VMAP @ ;

\ ---- reading the frozen module -----------------------------------------------
\ A span of the source module names a source of the source module; the new
\ module has exactly one registered source, proved to be the same bytes, so the
\ only ordinal a carried span may name is that one.
: SRC-CK ( IR-ID:ir-source-id -- )
   IR-ID:SOURCE-LOCAL 0<> if E-A64SEL-SHAPE throw then ;

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

\ The value one operand of a source operation selected to.
: OPERAND ( IR-ID:ir-op-id n -- IR-ID:ir-value-id )
   OPERAND-AT VOF ;

\ Is this value of the source module the memory order rather than a number? The
\ answer is the TYPE the source module gives it, held against the identity the
\ source dialect answered at binding time, so this pass never asks which opcode
\ defined a value or which position it sits in.
: TOKEN? ( IR-ID:ir-value-id -- bool )
   VALUE-TYPE-AT  0 BND-MEM @  SAME-TYPE? ;

\ ---- staging one machine operation -------------------------------------------
\ Every machine operation carries the span of the source operation it selects
\ from, so a diagnostic about a register still points at the source the
\ programmer wrote.
: OPEN ( IR-ID:ir-op-id A64IR:opcode -- )
   {: id:IR-ID:ir-op-id o:A64IR:opcode :}
   CTX BLD  CTX BLD o A64IR:OPCODE  IR-BUILD:BEGIN-OP
   CTX BLD  id OP-SPAN  IR-BUILD:SET-OP-SPAN ;

: RESULT+ ( -- )
   CTX BLD  CTX BLD A64IR:GPR-TYPE  IR-BUILD:ADD-RESULT ;

: TOKEN+ ( -- )
   CTX BLD  CTX BLD A64IR:MEM-TYPE  IR-BUILD:ADD-RESULT ;

: OPERAND+ ( IR-ID:ir-value-id -- )
   CTX BLD rot IR-BUILD:ADD-OPERAND ;

\ Close the operation and keep the one value it defined as the running value.
: CLOSE-VALUE ( -- )
   CTX BLD IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CTX BLD id 0 IR-BUILD:OP-RESULT@ ACC! ;

\ ---- the routine's convention, read once -------------------------------------
\ How many positions of one side are data-stack slots. A side that mixes register
\ places with data-stack places is refused: there is no entry sequence here for a
\ convention that puts argument one in a register and argument two on the stack,
\ and half-serving it would leave one of them nowhere.
: SLOT-POSITIONS ( A64EFF:placeseq -- n )
   {: s:A64EFF:placeseq :}
   s A64EFF:SEQ-SLOTS {: sl:n :}
   sl 0= if 0 exit then
   sl s A64EFF:SEQ-LEN <> if E-A64SEL-PLACE throw then
   sl ;

\ Does this routine take its arguments and leave its results on the caller's data
\ stack at all? One side saying so commits the other: a word whose arguments
\ arrive on the stack and whose result leaves in a register is not a convention
\ this pass builds, so a register place on either side is refused once either
\ side names a slot.
: DSTACK? ( -- bool )
   ARGS SLOT-POSITIONS OUTS SLOT-POSITIONS or 0<> ;

: DSTACK-CK ( -- )
   DSTACK? 0= if exit then
   ARGS SLOT-POSITIONS ARGS A64EFF:SEQ-LEN <> if E-A64SEL-PLACE throw then
   OUTS SLOT-POSITIONS OUTS A64EFF:SEQ-LEN <> if E-A64SEL-PLACE throw then ;

\ Does the contract say this routine calls? That declaration is what decides
\ whether the frame and the link save are built, and SELECT holds it against the
\ module: a contract that declares a call for a body containing none would
\ reserve a frame for nothing, and a body containing one under a contract that
\ declares none would destroy the caller's return address.
: CALLS? ( -- bool )
   TRAITS A64EFF:T-CALL A64EFF:TRAITS-HAS? ;

\ What a contract that declares a call has to say for this pass to build one.
\ A call reaches the callee through the caller's data stack, so a routine whose
\ convention names no data-stack place has no way to hand an argument over; and
\ the return address goes into slot zero of the routine's own frame, so a frame
\ too small to hold one cell has nowhere to put it. Both are the contract's
\ declaration and both are decided before a single operation is selected.
: CONTRACT-CK ( -- )
   CALLS? 0= if exit then
   DSTACK? 0= if E-A64SEL-CALL throw then
   FRAME A64IR:SLOT-WIDTH < if E-A64SEL-CALL throw then ;

\ The contract and the module have to agree about whether this routine calls.
\ A contract that declares a call for a body containing none reserves a frame and
\ saves a return address for nothing; a body containing one is refused at the
\ call by CONTRACT-CK's other half. Two derivations of one fact, held together.
: CALLED-CK ( -- )
   CALLS? if
      N-CALLS @ 0= if E-A64SEL-CALL throw then exit
   then
   N-CALLS @ 0<> if E-A64SEL-CALL throw then ;

\ ---- the four data-stack operations ------------------------------------------
\ Each carries the span of the source operation it is anchored to, so a
\ diagnostic about an entry load still points at the word the programmer wrote.
: DSLOT-ATTR+ ( n -- )
   {: off:n :}
   CTX BLD  CTX BLD A64IR:KEY-DSLOT  CTX BLD off A64IR:DSLOT-ATTR
   IR-BUILD:ADD-ATTR ;

: DBYTES-ATTR+ ( n -- )
   {: size:n :}
   CTX BLD  CTX BLD A64IR:KEY-DBYTES  CTX BLD size A64IR:DBYTES-ATTR
   IR-BUILD:ADD-ATTR ;

\ The pointer moves down over the caller's operands, and the order of every
\ data-stack access starts here.
: EMIT-DTAKE ( IR-ID:ir-op-id n -- )
   {: at:IR-ID:ir-op-id bytes:n :}
   at A64IR-OPCODE:DTAKE OPEN
   TOKEN+
   bytes DBYTES-ATTR+
   CTX BLD IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CTX BLD id 0 IR-BUILD:OP-RESULT@ TOK! ;

\ One argument, read out of its slot. The value it defines is what every use of
\ that argument in the selected module reads.
: EMIT-DLOAD ( IR-ID:ir-op-id n -- IR-ID:ir-value-id )
   {: at:IR-ID:ir-op-id off:n :}
   at A64IR-OPCODE:DLOAD OPEN
   TOK OPERAND+
   RESULT+
   TOKEN+
   off DSLOT-ATTR+
   CTX BLD IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CTX BLD id 1 IR-BUILD:OP-RESULT@ TOK!
   CTX BLD id 0 IR-BUILD:OP-RESULT@ ;

\ One result, written into its slot.
: EMIT-DSTORE ( IR-ID:ir-op-id IR-ID:ir-value-id n -- )
   {: at:IR-ID:ir-op-id v:IR-ID:ir-value-id off:n :}
   at A64IR-OPCODE:DSTORE OPEN
   v OPERAND+
   TOK OPERAND+
   TOKEN+
   off DSLOT-ATTR+
   CTX BLD IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CTX BLD id 0 IR-BUILD:OP-RESULT@ TOK! ;

\ The pointer moves up over the results, which is the moment they become the
\ caller's, and the order of the data-stack accesses ends.
: EMIT-DPUBLISH ( IR-ID:ir-op-id n -- )
   {: at:IR-ID:ir-op-id bytes:n :}
   at A64IR-OPCODE:DPUBLISH OPEN
   TOK OPERAND+
   bytes DBYTES-ATTR+
   CTX BLD IR-BUILD:END-OP drop ;

\ ---- the routine's frame, and the return address in it -----------------------
\ A routine that calls is a routine whose own return address is destroyed by the
\ first call it makes, so it puts it away before the body and takes it back
\ before it returns. WHERE it puts it is not this pass's decision:
\ src/compiler/native/frame.f owns the layout of a routine's frame and answers
\ the link slot from the contract's own trait, so the offset this pass writes and
\ the offset the register allocator places its own slots above are one statement.
\ The frame itself is the one the contract declares - the same declaration the
\ register allocator's validator measures every frame access against.
\
\ WHY THE PAIR IS BUILT HERE AND NOT AT EMISSION. It is the same argument that
\ put the data-stack entry and exit in this pass: an instruction the emitter
\ materialised out of a contract is an instruction no module contains, so the
\ independent validator would have nothing to re-derive it from. A routine's
\ interface is known before a single operation is selected, and saving the link
\ register is part of that interface - it is what makes the contract's
\ `link preserved` true of the routine rather than merely declared.
\
\ THE FRAME ORDER IS ITS OWN CHAIN AND CROSSES BLOCKS BY DOMINANCE. The reserve
\ mints it in the entry block, the save takes it, the restore reads what the save
\ left and the release ends it. The entry block dominates every block of the
\ routine, so the value the save answers is readable in the block control leaves
\ through without being handed across an edge - which is ordinary SSA and is what
\ the freeze verifier proves. The data stack keeps a separate chain for the
\ reason the dialect gives: no operation of this dialect can compute an address
\ inside the frame, so the two orders are independent and stating one would be
\ claiming something nothing proved.
: FRAME-ATTR+ ( n -- )
   {: size:n :}
   CTX BLD  CTX BLD A64IR:KEY-FRAME  CTX BLD size A64IR:FRAME-ATTR
   IR-BUILD:ADD-ATTR ;

: SLOT-ATTR+ ( n -- )
   {: off:n :}
   CTX BLD  CTX BLD A64IR:KEY-SLOT  CTX BLD off A64IR:SLOT-ATTR
   IR-BUILD:ADD-ATTR ;

: EMIT-RESERVE ( IR-ID:ir-op-id -- )
   {: at:IR-ID:ir-op-id :}
   at A64IR-OPCODE:RESERVE OPEN
   TOKEN+
   FRAME FRAME-ATTR+
   CTX BLD IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CTX BLD id 0 IR-BUILD:OP-RESULT@ FTOK! ;

: EMIT-RELEASE ( IR-ID:ir-op-id -- )
   {: at:IR-ID:ir-op-id :}
   at A64IR-OPCODE:RELEASE OPEN
   FTOK OPERAND+
   FRAME FRAME-ATTR+
   CTX BLD IR-BUILD:END-OP drop ;

\ The two halves of the link save, which differ only in which way the register
\ moves. Neither takes the register as an operand: it is x30, named by the form.
: EMIT-LINK ( IR-ID:ir-op-id A64IR:opcode -- )
   {: at:IR-ID:ir-op-id o:A64IR:opcode :}
   at o OPEN
   FTOK OPERAND+
   TOKEN+
   A64FRAME:LINK-SLOT SLOT-ATTR+
   CTX BLD IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CTX BLD id 0 IR-BUILD:OP-RESULT@ FTOK! ;

\ Whether the frame and the link save are built at all is CALLS? above.
: PROLOGUE ( IR-ID:ir-op-id -- )
   {: at:IR-ID:ir-op-id :}
   CALLS? 0= if exit then
   at EMIT-RESERVE
   at A64IR-OPCODE:LINKSAVE EMIT-LINK ;

: EPILOGUE ( IR-ID:ir-op-id -- )
   {: at:IR-ID:ir-op-id :}
   CALLS? 0= if exit then
   at A64IR-OPCODE:LINKLOAD EMIT-LINK
   at EMIT-RELEASE ;

\ ---- selecting a call --------------------------------------------------------
\ THE CALL-SITE INVARIANT, WHICH IS WHAT THIS WORD BUILDS.
\
\ The engine keeps the data stack full-ascending with its pointer one cell past
\ the caller's top live value. A routine of arity (a -> r) is entered with the
\ pointer one past its a arguments; it moves the pointer down over them, reads
\ argument i at 8i, writes result j at 8j and moves the pointer up over the r
\ results. That is the routine's own entry and exit, and it is already what this
\ pass builds.
\
\ A call site is the mirror of it. Let the pointer stand at ds0 and let the
\ caller hold k live values besides the a arguments. The site writes each live
\ value into slot i and each argument into slot k+i, through the same a64.dstore
\ the exit uses; moves the pointer up over all k+a of them, branches with link to
\ the routine's own entry, and moves it back down over k+r; then reads each live
\ value back out of slot i and each result out of slot k+j, through the same
\ a64.dload the entry uses. The callee's own -8a and +8r land exactly on the
\ argument slots, so the pointer ends at ds0 again and the caller's live values
\ are back in registers, read out of the very slots they were written to.
\
\ WHY THE CALLER SAVES AT ALL. The callee is this same routine: its contract's
\ destroyed set is exactly the register pool the allocator hands out, so every
\ register the caller holds a value in is one the recursive instance writes.
\ Nothing in a Habu word's convention is callee-saved - src/compiler/a64-effect.f
\ has no role for a register that is written and put back - so the caller saves,
\ and the one place it can save to that the callee cannot reach is its own data
\ stack below the callee's argument base.
\
\ AND WHY THE COUNTS ARE DERIVED TWICE. How many values are live across the call
\ is read off the operand list, and again off the result list, against the
\ routine's own declared arity; the two have to agree. A source operation whose
\ two lists tell different stories is refused by name rather than lowered into a
\ store run and a load run of different lengths.
: DBACK-ATTR+ ( n -- )
   {: size:n :}
   CTX BLD  CTX BLD A64IR:KEY-DBACK  CTX BLD size A64IR:DBACK-ATTR
   IR-BUILD:ADD-ATTR ;

: CALL-LIVE ( IR-ID:ir-op-id n n -- n )
   {: id:IR-ID:ir-op-id a:n r:n :}
   id OPERANDS-OF 1- a - {: k:n :}
   k 0 < if E-A64SEL-CALL throw then
   id RESULTS-OF 1- r - k <> if E-A64SEL-CALL throw then
   k ;

: EMIT-BL ( IR-ID:ir-op-id n n -- )
   {: at:IR-ID:ir-op-id give:n back:n :}
   at A64IR-OPCODE:CALL OPEN
   TOK OPERAND+
   TOKEN+
   give DBYTES-ATTR+
   back DBACK-ATTR+
   CTX BLD IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CTX BLD id 0 IR-BUILD:OP-RESULT@ TOK! ;

\ The same branch to a callee named by its address. The three adjustments are the
\ same three fields the self-call carries under the same two keys, so every
\ consumer that finds a call site by its keys finds this one too; the entry is
\ the third, and it is the only thing about the two forms that differs.
: EMIT-WBL ( IR-ID:ir-op-id n n n -- )
   {: at:IR-ID:ir-op-id give:n back:n entry:n :}
   at A64IR-OPCODE:WORDCALL OPEN
   TOK OPERAND+
   TOKEN+
   give DBYTES-ATTR+
   back DBACK-ATTR+
   CTX BLD  CTX BLD A64IR:KEY-ENTRY  CTX BLD entry A64IR:ENTRY-ATTR
   IR-BUILD:ADD-ATTR
   CTX BLD IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CTX BLD id 0 IR-BUILD:OP-RESULT@ TOK! ;

\ Nothing here re-asks whether this routine may call at all. CONTRACT-CK decided
\ it before the first operation was selected - a contract that declares a call
\ needs the data-stack convention and a frame for the return address - and
\ CALLED-CK decides the other half after the last one, by holding the calls this
\ pass really built against the contract's declaration. A third copy of the
\ question here would be a check no mutation can reach.
\ The store run in front of the branch, the load run behind it, and the two
\ pointer moves the branch itself carries. Both call forms are this sequence and
\ they differ only in the operation in the middle, so the sequence is written
\ once: a second copy would be a second statement of the caller-save discipline
\ the whole design turns on.
: CALL-SAVE ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id n:n :}
   n 0 ?do
      id  id i 1+ OPERAND  i A64IR:SLOT-WIDTH *  EMIT-DSTORE
   loop ;

: CALL-RESTORE ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id n:n :}
   n 0 ?do
      id i 1+ RESULT-AT
      id  i A64IR:SLOT-WIDTH *  EMIT-DLOAD
      VBIND
   loop
   id 0 RESULT-AT  TOK  VBIND
   N-CALLS @ 1+ N-CALLS ! ;

: EMIT-CALL ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   ARGS SLOT-POSITIONS {: a:n :}
   OUTS SLOT-POSITIONS {: r:n :}
   id a r CALL-LIVE {: k:n :}
   id 0 OPERAND TOK!
   id k a + CALL-SAVE
   id  k a + A64IR:SLOT-WIDTH *  k r + A64IR:SLOT-WIDTH *  EMIT-BL
   id k r + CALL-RESTORE ;

\ ---- selecting a call to another word ----------------------------------------
\ THE ONE THING THAT DIFFERS FROM A SELF-CALL IS WHOSE ARITY IS READ. A self-call
\ enters this same routine, so how many values the site publishes and takes back
\ is the ROUTINE's own convention - which is exactly what makes EMIT-CALL above
\ read ARGS and OUTS. A call to another word enters a routine with a convention
\ of its own, and the operation carries it: the source dialect put the callee's
\ declared effect on the operation because neither list can be counted for it,
\ both being variadic. Everything else - the store run, the branch, the load run,
\ the two byte counts and the slots they name - is the same sequence, and it is
\ the same words below that build it.
\
\ AND THE CALLER'S SAVE DISCIPLINE IS UNCHANGED, WHICH IS THE POINT. The site
\ writes every live value into a slot of the caller's own stack BELOW the
\ callee's argument base and reads it back out of that slot afterwards, so it
\ assumes nothing at all about which registers the callee destroys. That is what
\ makes it correct against a callee this compiler did not produce: a word the
\ engine's own emitter compiled keeps the same convention - it takes its
\ arguments out of the caller's slots, leaves its results in them, and never
\ writes below the base it was entered at - and no register of the caller is
\ live across the branch for it to clobber.
\ Which of an operation's attributes stands under this key. The search answers a
\ POSITION and the value is read from it, so the "no such attribute" answer is an
\ index no attribute has rather than a number some attribute could carry. The
\ freeze verifier already proves an operation carries exactly one attribute under
\ each key its schema declares, so the refusal is fail-closed.
: ATTR-SLOT-OF ( IR-ID:ir-op-id IR-ID:ir-symbol-id -- n )
   {: id:IR-ID:ir-op-id want:IR-ID:ir-symbol-id :}
   -1
   id ATTRS-OF 0 ?do
      id i ATTR-KEY-AT want SAME-SYM? if drop i leave then
   loop
   dup 0 < if E-A64SEL-ATTR throw then ;

: ATTR-INT-OF ( IR-ID:ir-op-id IR-ID:ir-symbol-id -- n )
   {: id:IR-ID:ir-op-id want:IR-ID:ir-symbol-id :}
   id  id want ATTR-SLOT-OF  ATTR-INT-AT ;

: EMIT-WORD-CALL ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id 0 BND-IN @ ATTR-INT-OF {: a:n :}
   id 0 BND-OUT @ ATTR-INT-OF {: r:n :}
   id a r CALL-LIVE {: k:n :}
   id 0 OPERAND TOK!
   id k a + CALL-SAVE
   id  k a + A64IR:SLOT-WIDTH *  k r + A64IR:SLOT-WIDTH *
   id 0 BND-ENTRY @ ATTR-INT-OF  EMIT-WBL
   id k r + CALL-RESTORE ;

\ ---- selecting a constant ----------------------------------------------------
\ The literal is the whole content of a source constant, and it rides as the
\ attribute the source opcode's schema requires. The key is compared against the
\ one this pass was told, so a constant carrying some other attribute is refused
\ instead of read as if it were the value.
: CONST-VALUE ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id ATTRS-OF 1 <> if E-A64SEL-ATTR throw then
   id 0 ATTR-KEY-AT  0 BND-VAL @  SAME-SYM?
   0= if E-A64SEL-ATTR throw then
   id 0 ATTR-INT-AT ;

\ One move-wide operation. `keep` is whether the halves already in place survive:
\ movz clears them and movk keeps them, which is exactly the difference between
\ taking the running value as an operand and taking none.
: MOVE-WIDE ( IR-ID:ir-op-id A64IR:opcode n n bool -- )
   {: id:IR-ID:ir-op-id o:A64IR:opcode imm:n sh:n keep:bool :}
   id o OPEN
   keep if CTX BLD ACC IR-BUILD:ADD-OPERAND then
   RESULT+
   CTX BLD  CTX BLD A64IR:KEY-IMM    CTX BLD imm A64IR:IMM-ATTR    IR-BUILD:ADD-ATTR
   CTX BLD  CTX BLD A64IR:KEY-SHIFT  CTX BLD sh A64IR:SHIFT-ATTR   IR-BUILD:ADD-ATTR
   CLOSE-VALUE ;

\ The move-wide chain that materialises one 64-bit value: the lowest half always,
\ then every further half that is not already zero.
: MATERIALISE ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id v:n :}
   id A64IR-OPCODE:MOVZ  v 0 A64IR:HALF-OF  0 A64IR:HALF-SHIFT  false MOVE-WIDE
   A64IR:HALVES 1 ?do
      v i A64IR:HALF-OF 0<> if
         id A64IR-OPCODE:MOVK  v i A64IR:HALF-OF  i A64IR:HALF-SHIFT  true MOVE-WIDE
      then
   loop ;

: EMIT-CONST ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id  id CONST-VALUE  MATERIALISE
   id 0 RESULT-AT  ACC  VBIND ;

\ ---- selecting the arithmetic ------------------------------------------------
\ Two values in, one out. The operands are the values the source operands
\ selected to, in the source order, so a subtraction keeps subtracting the same
\ side - and so does a shift, whose first operand is the value moved and whose
\ second is the count.
: EMIT-BINARY ( IR-ID:ir-op-id A64IR:opcode -- )
   {: id:IR-ID:ir-op-id o:A64IR:opcode :}
   id o OPEN
   CTX BLD  id 0 OPERAND  IR-BUILD:ADD-OPERAND
   CTX BLD  id 1 OPERAND  IR-BUILD:ADD-OPERAND
   RESULT+
   CLOSE-VALUE
   id 0 RESULT-AT  ACC  VBIND ;

\ One value in, one out, which is what `invert` is. It is the shape above with
\ one operand rather than a second rule: the machine form takes one register and
\ writes one, and the source operation declares exactly that.
: EMIT-UNARY ( IR-ID:ir-op-id A64IR:opcode -- )
   {: id:IR-ID:ir-op-id o:A64IR:opcode :}
   id o OPEN
   CTX BLD  id 0 OPERAND  IR-BUILD:ADD-OPERAND
   RESULT+
   CLOSE-VALUE
   id 0 RESULT-AT  ACC  VBIND ;

\ ---- selecting the memory operations -----------------------------------------
\ THE SOURCE ORDER AND THE MACHINE ORDER ARE ONE ORDER. The source dialect starts
\ a definition's memory order with hir.mem, and this machine dialect starts a
\ routine's generic memory order with a64.dtake - the moment the routine takes
\ the caller's operands off the data stack. They are the same order: the data
\ stack is generic memory, an address a program computed may name a slot of it,
\ and a module that kept two chains over one space would be stating an
\ independence nothing proved. So hir.mem selects to NO instruction at all. What
\ it selects to is a VALUE: the token a64.dtake already minted, bound to the
\ source order so that every access below it threads the one chain the whole
\ module has.
\
\ WHICH IS WHY A ROUTINE WITHOUT A DATA-STACK CONVENTION IS REFUSED. A routine
\ that takes nothing and publishes nothing off the caller's stack has no
\ a64.dtake, so there is no token for the source order to bind to, and this pass
\ has no other way to mint one. It is refused by name rather than lowered to
\ something with an order nobody stated; dot habu-begin-a-mem-4d2399cf is where
\ that gap is answered.
: EMIT-MEM ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   DSTACK? 0= if E-A64SEL-MEM throw then
   id 0 RESULT-AT  TOK  VBIND ;

\ One read through an address the program computed. The address is the source
\ load's first operand and the order its second, and both are the values those
\ operands selected to, so a load reading the wrong operand is a wrong VALUE
\ rather than a wrong index. The token the machine load answers becomes the
\ running order, so the routine's own exit stores are ordered after it.
\
\ The WIDTH is the machine opcode this word is handed, because it is a property
\ of the form on both sides: hir.load selects to a64.aldr and hir.bload to
\ a64.aldrb, and the two source forms are otherwise the same shape. Handing the
\ opcode in rather than deciding it here is what keeps the pairing in one place -
\ the selection table below - so a byte load lowered at cell width is a wrong
\ line of that table rather than a missing branch inside this word.
: EMIT-ALOAD ( IR-ID:ir-op-id A64IR:opcode -- )
   {: id:IR-ID:ir-op-id o:A64IR:opcode :}
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

\ One write through an address the program computed: the value, the address, the
\ order - the same three the source store carries, in the same order, so the two
\ cannot be swapped without swapping them in the source too. The width is the
\ machine opcode handed in, for the reason the load above gives.
: EMIT-ASTORE ( IR-ID:ir-op-id A64IR:opcode -- )
   {: id:IR-ID:ir-op-id o:A64IR:opcode :}
   id o OPEN
   CTX BLD  id 0 OPERAND  IR-BUILD:ADD-OPERAND
   CTX BLD  id 1 OPERAND  IR-BUILD:ADD-OPERAND
   CTX BLD  id 2 OPERAND  IR-BUILD:ADD-OPERAND
   TOKEN+
   CTX BLD IR-BUILD:END-OP {: nid:IR-ID:ir-op-id :}
   CTX BLD nid 0 IR-BUILD:OP-RESULT@ {: tk:IR-ID:ir-value-id :}
   tk TOK!
   id 0 RESULT-AT tk VBIND ;

\ ---- selecting the return ----------------------------------------------------
\ Under a register convention the values still live where control leaves become
\ the terminator's operands, in the order the source return has them. Under the
\ data-stack convention each of them is written into the slot its declared place
\ names first, the pointer is moved up over them, and the return carries nothing:
\ the results are already published where the caller will look.
: EMIT-EXIT ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   OUTS SLOT-POSITIONS {: r:n :}
   id OPERANDS-OF r <> if E-A64SEL-PLACE throw then
   r 0 ?do
      id  id i OPERAND  OUTS i A64EFF:SEQ-SLOT@ A64IR:SLOT-WIDTH *  EMIT-DSTORE
   loop
   id  r A64IR:SLOT-WIDTH *  EMIT-DPUBLISH ;

: EMIT-RETURN ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   DSTACK? if id EMIT-EXIT then
   id EPILOGUE
   id A64IR-OPCODE:RET OPEN
   DSTACK? 0= if
      id OPERANDS-OF {: k:n :}
      k 0 ?do
         CTX BLD  id i OPERAND  IR-BUILD:ADD-OPERAND
      loop
   then
   CTX BLD IR-BUILD:END-OP drop ;

\ ---- selecting a comparison --------------------------------------------------
\ Which condition one source comparison is made under, read off the operation's
\ own opcode. Both the comparison that answers a Habu flag and the fused
\ compare-and-branch below ask here, so the pairing of a source relation to a
\ machine condition is written once: a comparison lowered under the wrong
\ condition is one wrong line of this table rather than two lines that can
\ disagree with each other. An operation that is not one of the six is refused
\ by name, because a caller asking this of anything else has already gone wrong.
: COMPARE-COND ( IR-ID:ir-op-id -- A64IR:cond )
   OPCODE-AT OPCODE-SLOT
   case
      O-LT of A64IR-COND:LT    endof
      O-LE of A64IR-COND:LE    endof
      O-GT of A64IR-COND:GT    endof
      O-GE of A64IR-COND:GE    endof
      O-EQ of A64IR-COND:EQUAL endof
      O-NE of A64IR-COND:NE    endof
      E-A64SEL-OPCODE throw
   endcase ;

: COMPARE-SLOT? ( n -- bool )
   {: k:n :}
   k O-LT = k O-LE = or k O-GT = or k O-GE = or k O-EQ = or k O-NE = or ;

\ One source comparison is one machine comparison, under the condition the source
\ opcode names. The machine form is three instructions and one operation, because
\ the condition flags the three pass between them are a single architectural
\ resource with no value of the machine dialect to stand for it; the dialect says
\ so, and this pass only has to name the condition.
: EMIT-FLAG ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id A64IR-OPCODE:FLAG OPEN
   CTX BLD  id 0 OPERAND  IR-BUILD:ADD-OPERAND
   CTX BLD  id 1 OPERAND  IR-BUILD:ADD-OPERAND
   RESULT+
   CTX BLD  CTX BLD A64IR:KEY-COND
   CTX BLD  id COMPARE-COND  A64IR:COND-ATTR  IR-BUILD:ADD-ATTR
   CLOSE-VALUE
   id 0 RESULT-AT  ACC  VBIND ;

\ ---- selecting the branches --------------------------------------------------
\ A successor is a block of the source module, and this pass rebuilds the blocks
\ one for one and in order, so block b of the source is block b of the machine
\ module and the edge is carried across by its ordinal. Nothing is renumbered:
\ the source module's block order is the order the whole chain agrees on.
: SUCCESSOR+ ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id i:n :}
   CTX BLD
   BLD IR-BUILD:MODULE-KEY  id i SUCC-AT IR-ID:BLOCK-LOCAL  IR-ID:PACK-BLOCK
   IR-BUILD:ADD-SUCCESSOR ;

\ ---- splitting the edges that carry values -----------------------------------
\ A block argument and every value handed to it across an edge have to end up in
\ ONE physical register, because the branch itself moves nothing: that is what
\ makes them one class to the register allocator. Handing the argument a value
\ the program is still using would force that class to contain two values that
\ are live at the same time, and no allocation exists for it. `MAX2` is the
\ smallest example - its two arms hand the join (a, b) and (b, a), so the two
\ arguments and the two values would collapse into one class holding both a and
\ b at once.
\
\ So every value crossing an argument-carrying edge is copied into a value of its
\ own first. A copy's result is defined just before the branch and dies at it, so
\ two copies on different edges can never be live together and neither can be
\ live with the argument they feed: the class is interference-free by
\ construction rather than by luck. This is ordinary critical-edge splitting,
\ done in values rather than in blocks - the elaborator already gave every such
\ edge a block of its own, and a block split alone does not help, because the
\ VALUES crossing it are still the long-lived ones.
\
\ THE PRICE, AND WHAT PAYS IT BACK. One instruction and one live register per
\ argument per edge. Most of that is given back further down the chain: the
\ register allocator prefers one register for a copy's two ends wherever they do
\ not interfere (src/compiler/native/regalloc.f, step five), and the emitter
\ writes no instruction for a copy from a register into itself
\ (src/compiler/native/emit.f, SELF-MOV?). So a copy that did not have to be
\ there costs nothing at run time, and the copies that remain are the ones whose
\ ends really are live at the same instant - which is exactly the case this
\ splitting exists for.
64 constant EDGE-MAX
EDGE-MAX TYPED-BUFFER EDGE-V IR-ID:ir-value-id

: EMIT-COPY ( IR-ID:ir-op-id IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: at:IR-ID:ir-op-id v:IR-ID:ir-value-id :}
   at A64IR-OPCODE:MOV OPEN
   CTX BLD v IR-BUILD:ADD-OPERAND
   RESULT+
   CLOSE-VALUE
   ACC ;

\ Going on to one block. The operands are the values the destination takes as its
\ block arguments, each one copied into a value of its own first - except the
\ memory order, which crosses uncopied.
\
\ WHY THE ORDER IS EXEMPT, AND WHY THAT IS NOT A SPECIAL CASE. The copy exists
\ for one reason: an argument and every value handed to it are one physical
\ register, so handing over a value the program still holds would put two live
\ values in one register. A memory order holds NO register - the allocator gives
\ its class none and the validator refuses one that was given one - so there is
\ no register for two values to collide in and nothing for a copy to break apart.
\ There is also nothing to copy it WITH: a64.mov moves a general register, and a
\ form that moved an ordering would be an instruction that moves nothing. So the
\ exemption is a consequence of the class rather than an exception to the rule,
\ and the union-find the allocator builds over these edges still puts the order
\ and the argument it feeds in one class - a class that is given no register.
: EDGE-VALUE ( IR-ID:ir-op-id n -- IR-ID:ir-value-id )
   {: id:IR-ID:ir-op-id i:n :}
   id i OPERAND-AT TOKEN? if id i OPERAND exit then
   id  id i OPERAND  EMIT-COPY ;

: EMIT-BR ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id OPERANDS-OF {: k:n :}
   k EDGE-MAX > if E-A64SEL-CAP throw then
   k 0 ?do
      id i EDGE-VALUE  i EDGE-V !
   loop
   id A64IR-OPCODE:BR OPEN
   k 0 ?do
      CTX BLD  i EDGE-V @  IR-BUILD:ADD-OPERAND
   loop
   id 0 SUCCESSOR+
   CTX BLD IR-BUILD:END-OP drop ;

\ Going on to one of two blocks on whether a value is zero. It hands neither of
\ them anything, which is what the source form declares too, so the two
\ successors carry across and nothing else does.
: EMIT-BRZ ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id A64IR-OPCODE:BRZ OPEN
   CTX BLD  id 0 OPERAND  IR-BUILD:ADD-OPERAND
   id 0 SUCCESSOR+
   id 1 SUCCESSOR+
   CTX BLD IR-BUILD:END-OP drop ;

\ ---- the selection table -----------------------------------------------------
\ The whole rule. Every arm names the machine operations one source operation
\ becomes; nothing else in this file decides which opcode a source operation
\ selects to.
\ Which source operations this pass lowers WITHOUT losing the trap its schema
\ declares. It is one exhaustive answer per member of the source family rather
\ than a flag read off the operation, because whether a trap survives is a
\ property of the rule below and of nothing else - a member added to
\ HIR:opcode has to answer it before it can be selected at all.
\
\ Division is the only one that survives today, and it survives because a64.sdiv
\ IS the guard and the divide: the machine form branches over a `brk` when the
\ divisor is not zero, which is what the engine's own `/` does, so a compiled
\ division traps exactly where an interpreted one traps. A trapping ADDITION has
\ no such form - it needs a flag-setting add, a conditional branch and a trap
\ target, none of which is in the machine dialect - so selecting it as a plain
\ a64.add would drop the check the unit's numeric policy asked for, and it is
\ refused instead. Dot habu-lower-trapping-arithmetic-5f514ffe carries it.
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
   ;MATCH ;

: TRAP-CK ( HIR:opcode IR-ID:ir-symbol-id -- HIR:opcode )
   {: o:HIR:opcode sym:IR-ID:ir-symbol-id :}
   V-SCHR VW sym IR-SCHEMA:FTRAPS? if
      o TRAP-PRESERVED? 0= if E-A64SEL-TRAP throw then
   then
   o ;

\ ---- fusing a comparison into the branch that tests it -----------------------
\ A source comparison whose answer is nothing but the question the branch below
\ it asks does not need the number a Habu flag is. The machine has the answer in
\ its condition flags one instruction after the compare, and a conditional
\ branch reads it there - so the two source operations become ONE machine
\ operation, a64.cmpbr, which is three instructions and no register where
\ a64.flag followed by a64.cbz is five and one.
\
\ WHY THE FUSION IS HERE AND NOT IN A PASS OF ITS OWN. Two reasons, and the
\ second is the one that decides it.
\
\ ONE AUTHORITY. This pass is already the only place that says which machine
\ operations a source operation becomes - the table below is that rule whole -
\ so a second pass choosing a64.cmpbr would be a second authority over the same
\ question, and the two could disagree about what a comparison lowers to. It
\ would also have nowhere to live: there is no hir.cmpbr to rewrite the source
\ module into, and inventing one would put a machine form into the source
\ dialect; rewriting the MACHINE module instead means building the flag and the
\ two-way branch and then deleting them again, which is the shape
\ src/compiler/native/spill.f has - and spill.f has it because a spill plan is
\ the ALLOCATOR's output and cannot exist until the module is frozen. A fusion
\ decision needs nothing that does not already exist before the first operation
\ is selected.
\
\ AND BECAUSE THE SINGLE-USE FACT IS ONLY CHEAP HERE. What makes the fusion
\ legal is that the comparison's value has exactly one use in the whole
\ function. That is one walk of the FROZEN source module's operand lists - the
\ module this pass reads from first to last, whose every value is already
\ defined and whose every use is already written down. In the module being BUILT
\ the same question has no answer yet: a use that has not been selected is not
\ there to count, so a pass asking it of the machine module would be asking
\ about half a module. The fact is therefore derived where it is complete, off
\ the frozen module, and never guessed at from the shape of the source.
\
\ WHAT THE FUSION REQUIRES, ALL THREE STRUCTURAL.
\   - The block's last operation is hir.brz, and the operation before it is one
\     of the three comparisons. Adjacency is the whole of the scheduling
\     question: the two operations become one where they already stand, so
\     nothing is moved, no live range is stretched, and no operation ends up
\     between the compare and the branch that reads its flags.
\   - The value the branch tests is the value that comparison defines. Held as
\     an identity, so a branch testing something else is not fused into a
\     comparison that happens to sit above it.
\   - That value has exactly one use in the function. A comparison whose answer
\     is also returned, stored or handed across an edge still needs the number,
\     so it keeps a64.flag and the branch keeps a64.cbz.
\ The comparison also goes through the same trap gate every selected operation
\ goes through, so a source dialect that ever made a comparison trapping cannot
\ have that trap dropped by being fused away.
: USES-IN-OP ( IR-ID:ir-value-id IR-ID:ir-op-id -- n )
   {: v:IR-ID:ir-value-id id:IR-ID:ir-op-id :}
   0
   id OPERANDS-OF 0 ?do
      id i OPERAND-AT v SAME-VALUE? if 1+ then
   loop ;

: USES-IN-BLOCK ( IR-ID:ir-value-id IR-ID:ir-block-id -- n )
   {: v:IR-ID:ir-value-id bk:IR-ID:ir-block-id :}
   0
   bk OP-COUNT 0 ?do
      v  bk i OP-AT  USES-IN-OP  +
   loop ;

\ How many operands of the whole function name this value. An operand is the
\ only place a value can be used - a successor is a block, an attribute is a
\ number - so this count is every use there is.
: USES-OF ( IR-ID:ir-value-id -- n )
   {: v:IR-ID:ir-value-id :}
   0
   FUN BLOCK-COUNT 0 ?do
      v  FUN i BLOCK-AT  USES-IN-BLOCK  +
   loop ;

\ Where this block's fused comparison is, or -1 when it has none. It is read
\ once per block, before a single operation of the block is selected, so the
\ walk and the terminator both read one answer.
: FUSE-SCAN ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   -1 FUSE-AT !
   bk OP-COUNT {: n:n :}
   n 2 < if exit then
   bk n 1- OP-AT OPCODE-AT OPCODE-SLOT O-BRZ <> if exit then
   bk n 2 - OP-AT {: d:IR-ID:ir-op-id :}
   d OPCODE-AT OPCODE-SLOT COMPARE-SLOT? 0= if exit then
   d RESULTS-OF 1 <> if exit then
   d 0 RESULT-AT  bk n 1- OP-AT 0 OPERAND-AT  SAME-VALUE? 0= if exit then
   d 0 RESULT-AT USES-OF 1 <> if exit then
   d OPCODE-AT {: sym:IR-ID:ir-symbol-id :}
   sym OPCODE-SLOT SLOT-OPCODE  sym TRAP-CK  drop
   n 2 - FUSE-AT ! ;

\ THE POLARITY, WHICH IS THE ONE THING THIS FUSION HAS TO GET RIGHT. A Habu flag
\ is all bits set when the source relation HOLDS, and hir.brz goes to its FIRST
\ successor when the value it tests is ZERO - so the source branch's first
\ successor is the arm the relation did NOT choose and its second is the arm it
\ did. a64.cmpbr goes to ITS first successor when the condition HOLDS. The two
\ therefore line up by swapping: the fused branch keeps the comparison's own
\ condition, unnegated, and takes the source branch's SECOND successor as its
\ first.
\
\   source `<` answers a flag; hir.brz succ 0 is `not (a < b)`, succ 1 is `a < b`
\   a64.cmpbr under `lt`: succ 0 is `a < b`, succ 1 is `not (a < b)`
\   so cmpbr succ 0 := brz succ 1, cmpbr succ 1 := brz succ 0, condition `lt`
\   and the same for `<=` under `le` and `=` under `equal`
\
\ THE OTHER WAY ROUND WOULD ALSO BE CORRECT AND IS MEASURABLY SLOWER. Negating
\ the condition and keeping the source successor order computes exactly the same
\ program. It puts the arm the relation did not choose behind the TAKEN
\ conditional branch, which on the corpus's loops is the arm taken on every turn
\ but the last - so the hot path becomes a taken conditional jumping over the
\ unconditional branch beside it. Measured over the eleven-row table against the
\ four byte-identical rows as a control, that costs SUM-TO 3.7%, COUNT-DOWN
\ 3.9%, BYTE-SUM 4.6%, BYTE-FIND 4.6% and FACT 6.1%, while this wiring is inside
\ +/-1% on every row. It is also the wiring that leaves the unconditional branch
\ pointing at the block laid out next, which is what a later elision pass can
\ delete (dot habu-elide-a-branch-74966a02).
\
\ The operands are the comparison's, in the comparison's order: a64.cmpbr
\ compares its first operand against its second exactly as a64.flag does, so
\ `a b <` fuses to a compare of a against b under `lt` and a swapped pair would
\ be a wrong program rather than a different spelling.
\
\ The span is the BRANCH's. The operation is the block's terminator - what it
\ is, is a two-way branch - and the span every reader of a terminator expects is
\ the control word the programmer wrote.
: EMIT-CMPBR ( IR-ID:ir-op-id IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id cm:IR-ID:ir-op-id :}
   id A64IR-OPCODE:CMPBR OPEN
   CTX BLD  cm 0 OPERAND  IR-BUILD:ADD-OPERAND
   CTX BLD  cm 1 OPERAND  IR-BUILD:ADD-OPERAND
   CTX BLD  CTX BLD A64IR:KEY-COND
   CTX BLD  cm COMPARE-COND  A64IR:COND-ATTR  IR-BUILD:ADD-ATTR
   id 1 SUCCESSOR+
   id 0 SUCCESSOR+
   CTX BLD IR-BUILD:END-OP drop ;

\ The two-way branch, fused or not. A block whose scan found no comparison to
\ fuse gets the same a64.cbz it always got, over the value the source branch
\ tests - which is why a comparison that is used a second time costs nothing but
\ the fusion.
: EMIT-BRANCH ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   FUSE-AT @ 0 < if id EMIT-BRZ exit then
   id  BLK FUSE-AT @ OP-AT  EMIT-CMPBR ;

: RULE ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id OPCODE-AT {: sym:IR-ID:ir-symbol-id :}
   sym OPCODE-SLOT SLOT-OPCODE  sym TRAP-CK
   MATCH HIR:opcode
      const  OF id EMIT-CONST ENDOF
      add    OF id A64IR-OPCODE:ADD EMIT-BINARY ENDOF
      sub    OF id A64IR-OPCODE:SUB EMIT-BINARY ENDOF
      mul    OF id A64IR-OPCODE:MUL EMIT-BINARY ENDOF
      div    OF id A64IR-OPCODE:SDIV EMIT-BINARY ENDOF
      lt     OF id EMIT-FLAG ENDOF
      le     OF id EMIT-FLAG ENDOF
      gt     OF id EMIT-FLAG ENDOF
      ge     OF id EMIT-FLAG ENDOF
      equal  OF id EMIT-FLAG ENDOF
      ne     OF id EMIT-FLAG ENDOF
      and    OF id A64IR-OPCODE:AND EMIT-BINARY ENDOF
      or     OF id A64IR-OPCODE:ORR EMIT-BINARY ENDOF
      xor    OF id A64IR-OPCODE:EOR EMIT-BINARY ENDOF
      lshift OF id A64IR-OPCODE:LSLV EMIT-BINARY ENDOF
      rshift OF id A64IR-OPCODE:LSRV EMIT-BINARY ENDOF
      invert OF id A64IR-OPCODE:MVN EMIT-UNARY ENDOF
      mem    OF id EMIT-MEM ENDOF
      load   OF id A64IR-OPCODE:ALOAD EMIT-ALOAD ENDOF
      store  OF id A64IR-OPCODE:ASTORE EMIT-ASTORE ENDOF
      bload  OF id A64IR-OPCODE:ABLOAD EMIT-ALOAD ENDOF
      bstore OF id A64IR-OPCODE:ABSTORE EMIT-ASTORE ENDOF
      br     OF id EMIT-BR ENDOF
      brz    OF id EMIT-BRANCH ENDOF
      call   OF id EMIT-CALL ENDOF
      wordcall OF id EMIT-WORD-CALL ENDOF
      return OF id EMIT-RETURN ENDOF
   ;MATCH ;

\ ---- opening the selected function -------------------------------------------
\ The two modules number their symbols separately, so the name is copied out of
\ the source interner and interned into the new one. Interning deduplicates, so
\ the new module gains one symbol per distinct name and no more.
: FUN-NAME ( IR-ID:ir-fun-id -- IR-ID:ir-symbol-id )
   {: f:IR-ID:ir-fun-id :}
   V-SYMP VW V-SYMR VW  V-FUNR VW MKEY f IR-FUN:FSYMBOL@  NAMEBUF NAME-CAP
   IR-SYM:FCOPY {: u:n :}
   CTX BLD NAMEBUF u IR-BUILD:INTERN-SYMBOL ;

\ The word's declared effect, restated in this dialect's type: one virtual
\ register per input and one per output. The counts are the source signature's,
\ read off the source module rather than counted off its body.
: FUN-SIG ( IR-ID:ir-fun-id -- IR-ID:ir-type-id )
   {: f:IR-ID:ir-fun-id :}
   V-TYPR VW  V-FUNR VW MKEY f IR-FUN:FSIGNATURE@  IR-TYPE:FARITY@
   {: in:n out:n :}
   CTX BLD A64IR:GPR-TYPE {: t:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN
   in 0 ?do t IR-TYPE:FN-PARAM loop
   out 0 ?do t IR-TYPE:FN-RESULT loop
   CTX BLD IR-BUILD:INTERN-CODE-REF ;

\ How the function is linked, seen and called is a property of the function and
\ not of the stage it is in, so all three are carried across rather than decided
\ again here.
: OPEN-FUN ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   CTX BLD f FUN-NAME IR-BUILD:BEGIN-FUN
   CTX BLD f FUN-SIG IR-BUILD:SET-SIGNATURE
   CTX BLD  V-FUNR VW f IR-FUN:FLINKAGE@  IR-BUILD:SET-LINKAGE
   CTX BLD  V-FUNR VW f IR-FUN:FVISIBILITY@  IR-BUILD:SET-VISIBILITY
   CTX BLD  V-FUNR VW f IR-FUN:FCONVENTION@  IR-BUILD:SET-CONVENTION
   CTX BLD f FUN-SPAN IR-BUILD:SET-FUN-SPAN ;

\ The word's inputs under a register convention: one block argument each, one
\ virtual register each, and each one is the value the matching source argument
\ selects to.
: OPEN-ARG1 ( IR-ID:ir-value-id -- )
   {: a:IR-ID:ir-value-id :}
   a TOKEN? if
      a  CTX BLD  CTX BLD A64IR:MEM-TYPE  IR-BUILD:ADD-BLOCK-ARG
      dup TOK!  VBIND
      exit
   then
   a  CTX BLD  CTX BLD A64IR:GPR-TYPE  IR-BUILD:ADD-BLOCK-ARG  VBIND ;

\ A block argument of the memory-order type is the ORDER arriving, and it is the
\ running order from the top of this block: the accesses of the block thread it
\ on from there, and the routine's exit stores - which are this pass's own and
\ are not in the source module at all - take whatever the block they are built
\ into ends with. Reading it out of the argument rather than carrying the
\ previous block's value forward is what makes a loop work: the order the second
\ turn reads is the one the first turn left, and no value of one turn is read in
\ another.
: OPEN-ARGS ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   bk ARG-COUNT {: n:n :}
   n 0 ?do
      bk i ARG-AT OPEN-ARG1
   loop ;

\ The same inputs under the data-stack convention: the block takes no argument at
\ all, because nothing arrives in a register. The pointer is moved down over the
\ caller's operands once, and each argument is then the value its own load
\ defines. The entry operations are anchored to the block's first source
\ operation, so they carry a span of the word they belong to.
: OPEN-DARGS ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   ARGS SLOT-POSITIONS {: a:n :}
   bk ARG-COUNT a <> if E-A64SEL-PLACE throw then
   bk 0 OP-AT {: at:IR-ID:ir-op-id :}
   at PROLOGUE
   at  a A64IR:SLOT-WIDTH *  EMIT-DTAKE
   a 0 ?do
      bk i ARG-AT
      at  ARGS i A64EFF:SEQ-SLOT@ A64IR:SLOT-WIDTH *  EMIT-DLOAD
      VBIND
   loop ;

\ Every block keeps its arguments, and only the entry block carries the routine's
\ interface: the convention says where the CALLER left the arguments, and the
\ caller reaches the routine at its entry. A later block's arguments are values
\ the routine handed itself, so they are ordinary block arguments whichever
\ convention the routine has.
: OPEN-BLOCK ( IR-ID:ir-block-id n -- )
   {: bk:IR-ID:ir-block-id ord:n :}
   CTX BLD IR-BUILD:BEGIN-BLOCK
   CTX BLD bk BLOCK-SPAN IR-BUILD:SET-BLOCK-SPAN
   DSTACK? ord 0= and if bk OPEN-DARGS exit then
   bk OPEN-ARGS ;

\ One block of the source function, selected whole. The value map is NOT cleared
\ between blocks: a value defined in one block and read in another is ordinary
\ SSA under dominance - the freeze verifier proved it of the source module and
\ will prove it again of this one - so the map has to answer across the whole
\ function.
\
\ A comparison the scan chose to fuse selects to NOTHING here. It is not skipped
\ so much as moved into the terminator: the branch below it builds the one
\ machine operation that stands for both, and the value the comparison defined
\ is never bound into the value map because nothing left in the function reads
\ it. That is what the single-use fact bought, and it is held rather than
\ assumed - a second reader would ask the map for a value nothing bound and be
\ refused by name.
: WALK-BLOCK ( IR-ID:ir-block-id n -- )
   {: bk:IR-ID:ir-block-id ord:n :}
   bk ord OPEN-BLOCK
   bk 0 S-BLK !
   bk FUSE-SCAN
   bk OP-COUNT {: n:n :}
   n 0 ?do
      i FUSE-AT @ <> if bk i OP-AT RULE then
   loop
   CTX BLD IR-BUILD:END-BLOCK drop ;

\ One function of the source module, block by block in the order the module
\ records them. That order is the one every later pass reads too, so a successor
\ ordinal means the same block on both sides of this pass.
: WALK-FUN ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f BLOCK-COUNT {: n:n :}
   n 1 < if E-A64SEL-SHAPE throw then
   n NFROZEN:BMAX > if E-A64SEL-CAP throw then
   f 0 S-FUN !
   f OPEN-FUN
   VCLEAR
   n 0 ?do
      f i BLOCK-AT i WALK-BLOCK
   loop
   CTX BLD IR-BUILD:END-FUN drop ;

\ ---- what one selection run is told ------------------------------------------
\ The new module gets the same source the old one has, proved the same rather
\ than assumed: IR-SOURCE records a source as the digest of its bytes, so the
\ text presented here is the text the source module was compiled from exactly
\ when the two digests agree.
: SOURCE! ( IR-CTX:ctx IR-BUILD:builder ptr u8 n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p u:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   V-SRC VW IR-SOURCE:FSOURCES 1 <> if E-A64SEL-SHAPE throw then
   V-SRC VW  MKEY 0 IR-ID:PACK-SOURCE  IR-SOURCE:FDIGEST@
   p u CDIGEST:COMPUTE
   CDIGEST-DIGEST:EQ 0= if E-A64SEL-SOURCE throw then
   c b p u IR-BUILD:ADD-SOURCE 0 S-SID ! ;

\ The binding is taken whatever the outcome, so neither a selection without a
\ binding nor a refused selection can leave one behind for the next caller.
: BND-TAKE ( -- )
   BND-MODE @ {: have:n :}
   BOUND-NO BND-MODE !
   have BOUND-YES <> if E-A64SEL-BIND throw then ;

: BND-MODULE-CK ( IR-BUILD:module -- )
   IR-BUILD:FMODULE  0 BND-MOD @  IR-ID:MODULE-SAME?
   0= if E-A64SEL-SOURCE throw then ;

\ One member of the source dialect's opcode family, learned from the module that
\ is going to hold it. The spelling is HIR's; the slot it goes into is this
\ file's, and nothing between the two is a caller's decision.
: BIND1 ( IR-CTX:ctx IR-BUILD:builder HIR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder o:HIR:opcode :}
   c b o HIR:OPCODE  o SLOT-OF BND-OP ! ;

\ A module whose schema table was created for another dialect, or for another
\ version of this one, holds operations this pass has no rules for even if some
\ of them happen to be spelled the same.
: HIR-CK ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b  c b IR-BUILD:DIALECT@  HIR:NAME IR-BUILD:SYMBOL-IS?
   0= if E-A64SEL-SOURCE throw then
   c b IR-BUILD:SCHEMA-MAJOR@ HIR:MAJOR <> if E-A64SEL-SOURCE throw then
   c b IR-BUILD:SCHEMA-MINOR@ HIR:MINOR <> if E-A64SEL-SOURCE throw then ;

public

\ ---- binding the source dialect ----------------------------------------------
\ Learn the opcode identities of the module that is about to be selected, while
\ it is still being built. A module's symbols are its own ordinals, so this is
\ the only moment the source dialect can be asked which symbol each of its
\ opcodes is; the answers stay valid after the module freezes because freezing
\ keeps the module's identity. The binding is spent by the next SELECT.
: BIND-SOURCE ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   BND-MODE @ BOUND-YES = if E-A64SEL-BIND throw then
   c b HIR-CK
   b IR-BUILD:MODULE@ 0 BND-MOD !
   c b HIR-OPCODE:CONST  BIND1
   c b HIR-OPCODE:ADD    BIND1
   c b HIR-OPCODE:SUB    BIND1
   c b HIR-OPCODE:MUL    BIND1
   c b HIR-OPCODE:DIV    BIND1
   c b HIR-OPCODE:LT     BIND1
   c b HIR-OPCODE:LE     BIND1
   c b HIR-OPCODE:GT     BIND1
   c b HIR-OPCODE:GE     BIND1
   c b HIR-OPCODE:EQUAL  BIND1
   c b HIR-OPCODE:NE     BIND1
   c b HIR-OPCODE:AND    BIND1
   c b HIR-OPCODE:OR     BIND1
   c b HIR-OPCODE:XOR    BIND1
   c b HIR-OPCODE:LSHIFT BIND1
   c b HIR-OPCODE:RSHIFT BIND1
   c b HIR-OPCODE:INVERT BIND1
   c b HIR-OPCODE:BR     BIND1
   c b HIR-OPCODE:BRZ    BIND1
   c b HIR-OPCODE:MEM    BIND1
   c b HIR-OPCODE:LOAD   BIND1
   c b HIR-OPCODE:STORE  BIND1
   c b HIR-OPCODE:BLOAD  BIND1
   c b HIR-OPCODE:BSTORE BIND1
   c b HIR-OPCODE:CALL   BIND1
   c b HIR-OPCODE:WORDCALL BIND1
   c b HIR-OPCODE:RETURN BIND1
   c b HIR:KEY-VALUE 0 BND-VAL !
   c b HIR:KEY-ENTRY 0 BND-ENTRY !
   c b HIR:KEY-IN    0 BND-IN !
   c b HIR:KEY-OUT   0 BND-OUT !
   c b HIR:MEM-TYPE 0 BND-MEM !
   BOUND-YES BND-MODE ! ;

\ Whether a binding is live. A caller that has to clean up after a refused run
\ needs to know which of the chain's passes still hold one, and the only honest
\ answer is each pass's own - a caller tracking it would be a second copy of this
\ state that a refusal in an unexpected place could put out of step.
: BOUND? ( -- bool )
   BND-MODE @ BOUND-YES = ;

\ Give up a binding without selecting against it.
: RELEASE ( -- )
   BND-TAKE ;

\ ---- the pass ----------------------------------------------------------------
\ Select the whole of one frozen source module into a new module of the machine
\ dialect, and answer that module frozen. The builder is a fresh one from
\ A64IR:NEW-BUILDER - this pass registers the machine operation family into it,
\ so a builder that already holds them, or one of another dialect, is refused by
\ A64IR. The bytes are the source text the frozen module was compiled from, and
\ they are proved to be by digest before any span is carried across.
\
\ The routine contract is the LAST argument because a contract is twelve cells
\ and a value of more than one cell cannot be bound to a typed local: it is taken
\ apart at the top, its two place lists are kept, and the ten fields this pass
\ has no use for are dropped. It is the whole contract rather than the two lists
\ alone so that a caller cannot hand this pass one routine's convention and the
\ allocator another's.
: SELECT ( IR-CTX:ctx IR-BUILD:module IR-BUILD:builder ptr u8 n A64EFF:routine -- IR-BUILD:module )
   A64EFF:VALIDATE A64EFF-ROUTINE:UNMAKE
   {: gi:A64EFF:placeseq gr:A64EFF:placeseq gc:A64EFF:gprs
      fi:A64EFF:fprs fr:A64EFF:fprs fc:A64EFF:fprs
      z:A64EFF:nzcv l:A64EFF:link ct:A64EFF:control
      t:A64EFF:traits size:n delta:n :}
   {: c:IR-CTX:ctx m:IR-BUILD:module b:IR-BUILD:builder p u:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   BND-TAKE
   m BND-MODULE-CK
   gi 0 S-ARGS !
   gr 0 S-OUTS !
   t 0 S-TRT !
   size S-FRAME !
   0 N-CALLS !
   DSTACK-CK
   CONTRACT-CK
   c b A64IR:REGISTER
   c 0 S-CTX !
   b 0 S-BLD !
   m VIEWS!
   c b p u SOURCE!
   FUN-COUNT {: n:n :}
   n 0 ?do
      MKEY i IR-ID:PACK-FUN WALK-FUN
   loop
   CALLED-CK
   c b IR-BUILD:FREEZE ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;using
;package
