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
\   hir.mem    -> no instruction: the order binds to the one a64.dtake minted
\   hir.load   -> a64.aldr
\   hir.store  -> a64.astr
\   hir.return -> a64.ret
\ An operand is not "the same position in the new operation"; it is the value the
\ source operand's own definition selected to, looked up in the value map. That is
\ what makes a wrongly wired operand a wrong VALUE rather than a wrong index, and
\ it is why the fixtures assert operand identity and not operand count.
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
\ own opcode family, and keeps the five identities it answers. Every spelling
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
require src/compiler/native/frozen.f

package A64SEL
using NFROZEN
private

\ ---- the bound source dialect ------------------------------------------------
\ One slot per member of the source dialect's opcode family, plus the attribute
\ key its constant carries and the module all six were learned from.
13 constant OPCODES-N
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

1 TYPED-BUFFER S-CTX IR-CTX:ctx
1 TYPED-BUFFER S-BLD IR-BUILD:builder
1 TYPED-BUFFER S-SID IR-ID:ir-source-id
1 TYPED-BUFFER S-ACC IR-ID:ir-value-id
1 TYPED-BUFFER S-TOK IR-ID:ir-value-id
1 TYPED-BUFFER S-ARGS A64EFF:placeseq
1 TYPED-BUFFER S-OUTS A64EFF:placeseq
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
      br     OF O-BR     ENDOF
      brz    OF O-BRZ    ENDOF
      mem    OF O-MEM    ENDOF
      load   OF O-LOAD   ENDOF
      store  OF O-STORE  ENDOF
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
      O-BR     of HIR-OPCODE:BR     endof
      O-BRZ    of HIR-OPCODE:BRZ    endof
      O-MEM    of HIR-OPCODE:MEM    endof
      O-LOAD   of HIR-OPCODE:LOAD   endof
      O-STORE  of HIR-OPCODE:STORE  endof
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
\ side.
: EMIT-BINARY ( IR-ID:ir-op-id A64IR:opcode -- )
   {: id:IR-ID:ir-op-id o:A64IR:opcode :}
   id o OPEN
   CTX BLD  id 0 OPERAND  IR-BUILD:ADD-OPERAND
   CTX BLD  id 1 OPERAND  IR-BUILD:ADD-OPERAND
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

\ One cell read through an address the program computed. The address is the
\ source load's first operand and the order its second, and both are the values
\ those operands selected to, so a load reading the wrong operand is a wrong
\ VALUE rather than a wrong index. The token the machine load answers becomes the
\ running order, so the routine's own exit stores are ordered after it.
: EMIT-ALOAD ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id A64IR-OPCODE:ALOAD OPEN
   CTX BLD  id 0 OPERAND  IR-BUILD:ADD-OPERAND
   CTX BLD  id 1 OPERAND  IR-BUILD:ADD-OPERAND
   RESULT+
   TOKEN+
   CTX BLD IR-BUILD:END-OP {: nid:IR-ID:ir-op-id :}
   CTX BLD nid 1 IR-BUILD:OP-RESULT@ {: tk:IR-ID:ir-value-id :}
   tk TOK!
   id 1 RESULT-AT tk VBIND
   id 0 RESULT-AT  CTX BLD nid 0 IR-BUILD:OP-RESULT@  VBIND ;

\ One cell written through an address the program computed: the value, the
\ address, the order - the same three the source store carries, in the same
\ order, so the two cannot be swapped without swapping them in the source too.
: EMIT-ASTORE ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id A64IR-OPCODE:ASTORE OPEN
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
   id A64IR-OPCODE:RET OPEN
   DSTACK? 0= if
      id OPERANDS-OF {: k:n :}
      k 0 ?do
         CTX BLD  id i OPERAND  IR-BUILD:ADD-OPERAND
      loop
   then
   CTX BLD IR-BUILD:END-OP drop ;

\ ---- selecting a comparison --------------------------------------------------
\ One source comparison is one machine comparison, under the condition the source
\ opcode names. The machine form is three instructions and one operation, because
\ the condition flags the three pass between them are a single architectural
\ resource with no value of the machine dialect to stand for it; the dialect says
\ so, and this pass only has to name the condition.
: EMIT-FLAG ( IR-ID:ir-op-id A64IR:cond -- )
   {: id:IR-ID:ir-op-id k:A64IR:cond :}
   id A64IR-OPCODE:FLAG OPEN
   CTX BLD  id 0 OPERAND  IR-BUILD:ADD-OPERAND
   CTX BLD  id 1 OPERAND  IR-BUILD:ADD-OPERAND
   RESULT+
   CTX BLD  CTX BLD A64IR:KEY-COND  CTX BLD k A64IR:COND-ATTR  IR-BUILD:ADD-ATTR
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
\ THE PRICE, STATED. One instruction and one live register per argument per
\ edge, and a copy whose two ends coalesce into one register comes out as a
\ register copied into itself - a no-op instruction that is still emitted,
\ because eliding it is a peephole and this leaf does not do peepholes.
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
\ block arguments, each one copied into a value of its own first.
: EMIT-BR ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id OPERANDS-OF {: k:n :}
   k EDGE-MAX > if E-A64SEL-CAP throw then
   k 0 ?do
      id  id i OPERAND  EMIT-COPY  i EDGE-V !
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
      mem    OF false ENDOF
      load   OF false ENDOF
      store  OF false ENDOF
      br     OF false ENDOF
      brz    OF false ENDOF
      return OF false ENDOF
   ;MATCH ;

: TRAP-CK ( HIR:opcode IR-ID:ir-symbol-id -- HIR:opcode )
   {: o:HIR:opcode sym:IR-ID:ir-symbol-id :}
   V-SCHR VW sym IR-SCHEMA:FTRAPS? if
      o TRAP-PRESERVED? 0= if E-A64SEL-TRAP throw then
   then
   o ;

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
      lt     OF id A64IR-COND:LT EMIT-FLAG ENDOF
      le     OF id A64IR-COND:LE EMIT-FLAG ENDOF
      mem    OF id EMIT-MEM ENDOF
      load   OF id EMIT-ALOAD ENDOF
      store  OF id EMIT-ASTORE ENDOF
      br     OF id EMIT-BR ENDOF
      brz    OF id EMIT-BRZ ENDOF
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
: OPEN-ARGS ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   bk ARG-COUNT {: n:n :}
   n 0 ?do
      bk i ARG-AT
      CTX BLD  CTX BLD A64IR:GPR-TYPE  IR-BUILD:ADD-BLOCK-ARG
      VBIND
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
: WALK-BLOCK ( IR-ID:ir-block-id n -- )
   {: bk:IR-ID:ir-block-id ord:n :}
   bk ord OPEN-BLOCK
   bk OP-COUNT {: n:n :}
   n 0 ?do
      bk i OP-AT RULE
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
   c b HIR-OPCODE:BR     BIND1
   c b HIR-OPCODE:BRZ    BIND1
   c b HIR-OPCODE:MEM    BIND1
   c b HIR-OPCODE:LOAD   BIND1
   c b HIR-OPCODE:STORE  BIND1
   c b HIR-OPCODE:RETURN BIND1
   c b HIR:KEY-VALUE 0 BND-VAL !
   BOUND-YES BND-MODE ! ;

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
   DSTACK-CK
   c b A64IR:REGISTER
   c 0 S-CTX !
   b 0 S-BLD !
   m VIEWS!
   c b p u SOURCE!
   FUN-COUNT {: n:n :}
   n 0 ?do
      MKEY i IR-ID:PACK-FUN WALK-FUN
   loop
   c b IR-BUILD:FREEZE ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;using
;package
