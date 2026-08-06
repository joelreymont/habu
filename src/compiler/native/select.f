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
\ own opcode family, and keeps every identity it answers. Every spelling stays
\ HIR's; the pairing of an opcode to its machine operations is this file's,
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
require src/compiler/native/clobber.f
require src/compiler/native/frame.f
require src/compiler/native/frozen.f

package A64SEL
using NFROZEN
public

\ WHICH SHAPE OF MACHINE COMPARISON A SOURCE OPERATION BECOMES. It is one table
\ and not three because the three questions the passes below ask - is this
\ fusable, which machine form does it become, and how many operands does it
\ have - are one question about the operation, and three tables over eleven
\ opcodes are three tables that can come to disagree. `none` is the answer for
\ everything that is not a comparison, which is what makes the fusion's own test
\ a reading of this table rather than a second list of opcode names. COMPARE-KIND
\ below is the table; this is its vocabulary, and it is an ENUM so that a member
\ added here has to be answered for everywhere it is read.
\   none   not a comparison at all
\   gpr    two general registers compared
\   freg   two floating registers compared
\   fzero  one floating register compared against the instruction's own zero
ENUM cmpkind DERIVE eq
   none
   gpr
   freg
   fzero
;ENUM

private

\ ---- the bound source dialect ------------------------------------------------
\ One slot per member of the source dialect's opcode family, plus the attribute
\ key its constant carries and the module they were all learned from.
44 constant OPCODES-N
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
27 constant O-FCONST
28 constant O-FADD
29 constant O-FSUB
30 constant O-FMUL
31 constant O-FDIV
32 constant O-FNEG
33 constant O-FABS
34 constant O-FSQRT
35 constant O-INTREAL
36 constant O-REALINT
37 constant O-BITSREAL
38 constant O-REALBITS
39 constant O-FLT
40 constant O-FGT
41 constant O-FEQ
42 constant O-FLTZ
43 constant O-FEQZ

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
1 TYPED-BUFFER BND-REAL IR-ID:ir-type-id

1 TYPED-BUFFER S-CTX IR-CTX:ctx
1 TYPED-BUFFER S-BLD IR-BUILD:builder
1 TYPED-BUFFER S-SID IR-ID:ir-source-id
1 TYPED-BUFFER S-ACC IR-ID:ir-value-id
1 TYPED-BUFFER S-TOK IR-ID:ir-value-id
1 TYPED-BUFFER S-ARGS A64EFF:placeseq
1 TYPED-BUFFER S-OUTS A64EFF:placeseq
1 TYPED-BUFFER S-TRT A64EFF:traits
1 TYPED-BUFFER S-POOL A64EFF:gprs
1 TYPED-BUFFER S-FPOOL A64EFF:fprs
1 TYPED-BUFFER S-FTOK IR-ID:ir-value-id
1 TYPED-BUFFER S-FUN IR-ID:ir-fun-id
1 TYPED-BUFFER S-BLK IR-ID:ir-block-id
variable S-FRAME                     \ the frame the contract declares, in bytes
variable N-CALLS                     \ calls this selection built
variable N-TAILS                     \ tail branches this selection built
variable S-TAIL                      \ whether the contract says control leaves through a callee
variable FUSE-AT                     \ where in this block the fused comparison is, or -1
VMAX TYPED-BUFFER VMAP IR-ID:ir-value-id
create VSET VMAX cells allot
create NAMEBUF NAME-CAP allot

\ ---- what the if-conversion below is working on ------------------------------
\ One row per block of the function being selected. They are package storage
\ rather than a structure because this pass selects one module at a time, which
\ is the same discipline the value map above keeps.
\
\ THE THREE BOUNDS ARE WHAT KEEPS THE CONVERSION TO A SMALL SELECTION, and they
\ are the whole of the cost side of the rule. Every operation of an arm is run
\ whichever arm the program would have taken, so a converted region pays its
\ own operations on every path and is paid back one branch that no predictor
\ can get wrong. That trade is a win while the arms are a few instructions and
\ a loss when they are many, which is why the width, the block count and the
\ speculated value count are all held down rather than left open: the
\ measurement this leaf exists for (docs/codegen-placement.md) is over bodies
\ whose arms are one or two operations, and these bounds admit that shape and
\ nothing much larger.
\
\ THE VALUE COUNT IS ALSO WHAT KEEPS THE ROUTINE ALLOCATABLE, and that half of
\ it was measured rather than argued. Every value an arm computes is live from
\ where it is computed to the select that reads it, so a converted region holds
\ all of them at once; the eight-deep early-exit ladder of
\ tools/codegen-compare-corpus4.f is the shape that showed it, running an
\ eight-register routine out of registers when the bound was eight and
\ allocating cleanly at four. Four is also exactly what the range fold this work
\ was raised for needs: three values across its two arms, and one of headroom.
\ Correctness never rests on the number - a region refused here is a region that
\ keeps its branch - and the allocator remains the authority on what really
\ fits, which is what R-PRESSURE-OK? below holds the count against.
16 constant SEL-WIDTH-MAX            \ values a converted selection may hand its join
16 constant SEL-BLOCK-MAX            \ blocks a converted region may absorb
4 constant SEL-DEFS-MAX              \ values it may compute on a path not taken

here CELL 1- and CELL swap - CELL 1- and allot
create R-PRED NFROZEN:BMAX cells allot     \ predecessors this block has
create R-FROM NFROZEN:BMAX cells allot     \ and the last one seen, which is the only
                                           \ one when the count is one
create R-ABSORB NFROZEN:BMAX cells allot   \ this block is inside a converted region
create R-OWNER NFROZEN:BMAX cells allot    \ and this is the head that absorbed it
create R-HEAD NFROZEN:BMAX cells allot     \ this block heads a converted region
create R-EXIT NFROZEN:BMAX cells allot     \ and the region leaves through this block
create R-ORD NFROZEN:BMAX cells allot      \ the machine block this source block became
create R-MARK NFROZEN:BMAX cells allot     \ membership while one region is being tried
create R-QB NFROZEN:BMAX cells allot       \ blocks still to classify
create R-QP NFROZEN:BMAX cells allot       \ and the block each was reached from
create R-LIST NFROZEN:BMAX cells allot     \ the members the current try has taken
variable R-QN
variable R-QI
variable R-LIST-N
variable R-SPEC                      \ operations the current try would speculate
variable R-SPEC-D                    \ how many of them define a double
variable R-JOIN                      \ the exit the current try has found, or -1
variable R-WIDTH                     \ how many values the region hands its exit
variable R-WIDTH-D                   \ how many of those values are doubles
variable R-EXIT-BK                   \ the exit of the region being emitted
variable R-NEXT                      \ the next machine block ordinal to hand out
variable R-S0                        \ the successors of the branch being selected
variable R-S1
variable R-BASE                      \ where this function's blocks start in the module
variable R-NEWBASE                   \ and where they start in the module being built
NFROZEN:BMAX SEL-WIDTH-MAX * TYPED-BUFFER RSEL IR-ID:ir-value-id
1 TYPED-BUFFER R-JB IR-ID:ir-block-id

\ ---- what the data-stack residency pass answers ------------------------------
\ THE FACT, IN ONE SENTENCE. A slot of the caller's data stack holds a value of
\ the source module, and the pass below says which one at every point of the
\ routine. A store that would write a value into the slot that already holds it
\ is no instruction at all, and a value nothing needs in a register never has to
\ be lifted out of its slot in the first place - so the two answers this section
\ keeps are WHICH VALUE EACH SLOT HOLDS and WHICH VALUES REACH A REGISTER.
\
\ WHY THE TWO ARE COMPUTED IN THAT ORDER AND NOT TOGETHER. Dropping a store that
\ writes what is already there leaves the memory exactly as it was, and so does
\ dropping a load, and so does dropping a block argument. None of the three
\ changes what any slot holds. So the residency fact does not depend on any of
\ them, it is computed first from the source module alone, and the register need
\ is then read off it. A single fixpoint over both would be stating a dependence
\ that is not there.
\
\ THE SLOT WINDOW IS A PRECISION BOUND AND NOT A LIMIT ON WHAT COMPILES, which
\ is the same shape as the region bounds above. A slot outside the window is
\ simply never resident, so every access to it is emitted exactly as it was
\ before this pass existed; the window is wider than any convention the chain
\ builds, and a routine that reached past it would lose an optimisation rather
\ than be refused.
64 constant DSLOT-MAX                \ slots one routine's residency is tracked over
-1 constant DNONE                    \ this slot holds nothing this pass can name
-2 constant DANY                     \ nothing has been said about this slot yet
63 constant DELIDE-MAX               \ store positions one run's elision mask holds

here CELL 1- and CELL swap - CELL 1- and allot
create D-IN NFROZEN:BMAX DSLOT-MAX * cells allot   \ what each slot holds at a block's head
create D-OUT NFROZEN:BMAX DSLOT-MAX * cells allot  \ and at its end
create D-CUR DSLOT-MAX cells allot                 \ the running answer inside one block
create D-NEED VMAX cells allot                     \ this value reaches a register
variable D-MOVED                                   \ a fixpoint round changed something

\ ---- where the routine's data-stack pointer stands ---------------------------
\ THE FACT, IN ONE SENTENCE. The pointer is a register, so it stands at ONE place
\ while the body runs, and every access of the caller's stack is written as the
\ distance from that place to the cell it names. Which place it is, is this
\ pass's choice; what a choice costs is one instruction at each point that
\ REQUIRES some other place, and nothing at each point that requires this one.
\
\ WHAT ACTUALLY REQUIRES A PLACE, WHICH IS THE WHOLE OF THE PROBLEM. Three
\ things, and they are all interface: the caller leaves the pointer one past the
\ arguments, so the routine is ENTERED at 8*in; a branch-with-link has to hand
\ the callee its own base, so a call site REQUIRES the pointer at the callee's
\ argument base and gets it back at the callee's result base; and the caller
\ reads the results from where the convention says, so the return REQUIRES the
\ pointer at 8*out. Nothing else requires anything: an access reaches its cell
\ from wherever the pointer is, because the offset is signed.
\
\ SO THE CHOICE IS AMONG THOSE REQUIRED PLACES AND NOWHERE ELSE. A place that is
\ required by nothing zeroes no adjustment and cannot beat a place that is, so
\ the candidates are exactly the required places, plus the base itself - which is
\ where this pass used to stand unconditionally and is therefore the incumbent
\ every other candidate has to beat. Ties go to the smallest place, so the answer
\ does not depend on the order the requirements were met in.
\
\ AND WHY THE POSITION IS BOUNDED RATHER THAN THE ACCESSES FILTERED. An access
\ under the pointer is encoded in the unscaled signed field, which reaches
\ A64EFF:SLOT-BACK bytes down; an access over it is encoded in the scaled field,
\ which reaches A64EFF:SLOT-REACH up. Every slot of the caller's window is at or
\ above the base, so a position kept inside [0, SLOT-BACK] puts EVERY access of
\ the routine inside one of the two fields, whichever cells it turns out to name.
\ Bounding the position is therefore the same statement as checking every access,
\ made once and before any access exists - which is what lets this run before the
\ operations are lowered.
\
\ AND THE SURVEY'S OWN SIZE IS A PRECISION BOUND AND NOT A LIMIT ON WHAT
\ COMPILES, which is the same shape as the residency window above. A routine with
\ more required places than the survey holds keeps the BASE - which is where this
\ pass stood before there was a choice, is always inside the bound, and puts
\ every access at a non-negative offset - so what such a routine loses is an
\ optimisation and never a compilation. The number is a hundred and twenty-seven
\ call sites, which no routine this chain compiles comes near.
256 constant DREQ-MAX                \ places one routine's survey holds

here CELL 1- and CELL swap - CELL 1- and allot
create D-REQ DREQ-MAX cells allot                  \ the places, with repeats
variable D-REQ-N
variable D-REQ-OVER                                \ the survey ran past its size
variable D-POS                                     \ where the body's pointer stands
variable D-COST                                    \ what standing there costs
variable D-RETS                                    \ returns seen while surveying

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
: TAIL? ( -- bool )                  S-TAIL @ 0<> ;
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
      fconst   OF O-FCONST   ENDOF
      fadd     OF O-FADD     ENDOF
      fsub     OF O-FSUB     ENDOF
      fmul     OF O-FMUL     ENDOF
      fdiv     OF O-FDIV     ENDOF
      fneg     OF O-FNEG     ENDOF
      fabs     OF O-FABS     ENDOF
      fsqrt    OF O-FSQRT    ENDOF
      intreal  OF O-INTREAL  ENDOF
      realint  OF O-REALINT  ENDOF
      bitsreal OF O-BITSREAL ENDOF
      realbits OF O-REALBITS ENDOF
      flt      OF O-FLT      ENDOF
      fgt      OF O-FGT      ENDOF
      feq      OF O-FEQ      ENDOF
      fltz     OF O-FLTZ     ENDOF
      feqz     OF O-FEQZ     ENDOF
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
      O-FCONST   of HIR-OPCODE:FCONST   endof
      O-FADD     of HIR-OPCODE:FADD     endof
      O-FSUB     of HIR-OPCODE:FSUB     endof
      O-FMUL     of HIR-OPCODE:FMUL     endof
      O-FDIV     of HIR-OPCODE:FDIV     endof
      O-FNEG     of HIR-OPCODE:FNEG     endof
      O-FABS     of HIR-OPCODE:FABS     endof
      O-FSQRT    of HIR-OPCODE:FSQRT    endof
      O-INTREAL  of HIR-OPCODE:INTREAL  endof
      O-REALINT  of HIR-OPCODE:REALINT  endof
      O-BITSREAL of HIR-OPCODE:BITSREAL endof
      O-REALBITS of HIR-OPCODE:REALBITS endof
      O-FLT      of HIR-OPCODE:FLT      endof
      O-FGT      of HIR-OPCODE:FGT      endof
      O-FEQ      of HIR-OPCODE:FEQ      endof
      O-FLTZ     of HIR-OPCODE:FLTZ     endof
      O-FEQZ     of HIR-OPCODE:FEQZ     endof
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

\ Is this value of the source module a double rather than a cell? Asked exactly
\ as TOKEN? is - the TYPE the source module gives it, against the identity the
\ source dialect answered at binding time - so this pass never reads a class off
\ an opcode's name. What the answer decides is which register file the machine
\ value it selects to belongs in.
: REAL? ( IR-ID:ir-value-id -- bool )
   VALUE-TYPE-AT  0 BND-REAL @  SAME-TYPE? ;

\ ---- the residency answers, read back ----------------------------------------
\ The map holds a source value by its module-local ordinal, so a slot's content
\ is compared with a value by the one ordinal the value map is keyed on too. The
\ readers are here rather than beside the pass that fills them because the
\ lowering words below consult them and the pass that fills them needs the whole
\ of the block and successor machinery, which is further down.
: DSLOT-CK ( n -- n )
   dup 0 < if E-A64SEL-CAP throw then ;

\ Does the window reach this slot at all? A slot beyond it is never resident,
\ which is what makes the window a precision bound rather than a refusal.
: DIN-WINDOW? ( n -- bool )
   DSLOT-CK DSLOT-MAX < ;

: DRES@ ( n -- n )
   dup DIN-WINDOW? 0= if drop DNONE exit then
   cells D-CUR + @ ;

: DRES! ( IR-ID:ir-value-id n -- )
   {: v:IR-ID:ir-value-id s:n :}
   s DIN-WINDOW? 0= if exit then
   v VSLOT s cells D-CUR + ! ;

\ Publish one value into one slot: the answer is whether the cell already held
\ it, which is the whole of what makes a store droppable. Every store this pass
\ lowers goes through here, so the map and the emission cannot come to disagree
\ about what a slot holds.
: DPUT? ( IR-ID:ir-value-id n -- bool )
   {: v:IR-ID:ir-value-id s:n :}
   s DRES@  v VSLOT =  {: had:bool :}
   v s DRES!
   had ;

: DKILL ( -- )
   DSLOT-MAX 0 ?do DNONE i cells D-CUR + ! loop ;

\ Does anything this pass emits read this value out of a register? A memory
\ order holds no register and is never dropped, so it answers yes without being
\ asked: the order has to cross every edge it crosses today.
: DNEED? ( IR-ID:ir-value-id -- bool )
   dup TOKEN? if drop true exit then
   VSLOT cells D-NEED + @ 0<> ;

: DNEED+ ( IR-ID:ir-value-id -- )
   dup DNEED? if drop exit then
   VSLOT cells D-NEED + 1 swap !
   1 D-MOVED ! ;

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

\ A result in the floating file. It is a second word and not a flag on the first
\ because which file a machine operation writes is a fact about that operation,
\ fixed by its own schema - and the schema is what refuses the pair if a caller
\ states the wrong one.
: FRESULT+ ( -- )
   CTX BLD  CTX BLD A64IR:FPR-TYPE  IR-BUILD:ADD-RESULT ;

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

\ And the same holding for the way control leaves. The contract declares it
\ before a single operation is selected - it is what decides whether the frame
\ and the link save are built at all - and this holds it against what the pass
\ really built: a contract declaring a tail call over a routine that built none
\ describes a routine whose caller is returned to twice, and a tail branch under
\ a contract that declares a return is a routine that never comes back from a
\ frame it still reserved.
: TAILED-CK ( -- )
   TAIL? if
      N-TAILS @ 1 <> if E-A64SEL-TAIL throw then exit
   then
   N-TAILS @ 0<> if E-A64SEL-TAIL throw then ;

\ ---- the four data-stack operations ------------------------------------------
\ Each carries the span of the source operation it is anchored to, so a
\ diagnostic about an entry load still points at the word the programmer wrote.
\
\ AND EVERY ONE OF THEM IS WRITTEN AGAINST WHERE THE POINTER STANDS. A caller
\ below names the CELL it means - the slot the convention gives an argument, the
\ base a callee is entered at, the place the routine returns with - and the four
\ builders turn that into the distance from the body's own position, which is
\ what the instruction encodes. The subtraction is here and in one word rather
\ than at each caller so that a place and an offset can never be swapped: a
\ caller of these builders never handles an offset at all.
: DPLACED ( n -- n )
   D-POS @ - ;

: DSLOT-ATTR+ ( n -- )
   {: off:n :}
   CTX BLD  CTX BLD A64IR:KEY-DSLOT  CTX BLD off DPLACED A64IR:DSLOT-ATTR
   IR-BUILD:ADD-ATTR ;

: DBYTES-ATTR+ ( n -- )
   {: at:n :}
   CTX BLD  CTX BLD A64IR:KEY-DBYTES  CTX BLD at DPLACED A64IR:DBYTES-ATTR
   IR-BUILD:ADD-ATTR ;

\ The pointer is placed where the body addresses from, and the order of every
\ data-stack access starts here. What it moves over is not the caller's operands
\ any more: it is whatever distance separates the place the caller left it - one
\ past the arguments - from the place the placement above chose, and that
\ distance is nothing whenever the two coincide.
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

\ The pointer is left where the caller expects it - one past the results, which
\ is the moment they become the caller's - and the order of the data-stack
\ accesses ends. It is the same distance-from-the-position the entry is, so a
\ routine whose body already stands there publishes with no instruction at all.
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
   {: at:n :}
   CTX BLD  CTX BLD A64IR:KEY-DBACK  CTX BLD at DPLACED A64IR:DBACK-ATTR
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
\ ---- how many live values may stay where they are ----------------------------
\ THE NARROWING, IN ONE SENTENCE. A call site has to put a live value somewhere
\ the callee cannot reach, and a register the callee never writes is such a
\ place. Which registers those are is the callee's own answer, recorded against
\ its entry address when it was published (src/compiler/native/clobber.f), so a
\ site calling a routine the chain compiled saves only what that answer covers
\ and a site calling anything else saves everything, exactly as before.
\
\ WHAT DECIDES THE NUMBER. Every value kept in a register across the branch needs
\ a register of its own file that the callee does not destroy, and it needs it
\ for the whole stretch it is live - so the most that can be kept is how many
\ such registers this routine has.
\
\ AND NOTHING RESTS ON THIS NUMBER BEING RIGHT, which is worth saying plainly. It
\ is a request, not a promise: the allocator is what really places a value, and
\ it will not put one that crosses a call in a register the callee writes
\ (src/compiler/native/regalloc.f, SB-FORBID and MB-FORBID), while the validator
\ refuses an assignment that does anyway (E-A64RAV-CLOBBER). A count that asked
\ for too much would cost a REFUSAL - the allocator running out of registers it
\ may use - and never wrong code. What the count is for is not asking.
\
\ AND THE KEPT VALUES ARE A SUFFIX OF THE LIVE LIST, which is not a preference
\ about which value is worth keeping - it is what makes the saved ones name slots
\ zero upwards with nothing missing, which is the shape the validator measures a
\ store run as. The suffix is taken from the TOP of the caller's stack downwards,
\ so the values nearest the work being done are the ones that stay in registers.
: BITS-N ( n -- n )
   {: v:n :}
   0
   A64EFF:FILE-SIZE 0 ?do
      v 1 i lshift and 0<> if 1+ then
   loop ;

: GPR-ROOM ( n -- n )
   {: e:n :}
   0 S-POOL @ A64EFF:GPRS-N {: p:n :}
   e 0 S-POOL @ NCLOB:GPR-CLOB A64EFF:GPRS-N {: c:n :}
   p c invert and BITS-N ;

: FPR-ROOM ( n -- n )
   {: e:n :}
   0 S-FPOOL @ A64EFF:FPRS-N {: p:n :}
   e 0 S-FPOOL @ NCLOB:FPR-CLOB A64EFF:FPRS-N {: c:n :}
   p c invert and BITS-N ;

\ How many of the kept suffix are of each file, as the walk grows it. They are
\ package state rather than two more numbers on the stack because the walk below
\ carries its answer there and a loop cannot hold three.
variable KEPT-G
variable KEPT-F

: KEEP-N ( IR-ID:ir-op-id n n -- n )
   {: id:IR-ID:ir-op-id e:n k:n :}
   e GPR-ROOM {: groom:n :}
   e FPR-ROOM {: froom:n :}
   0 KEPT-G !
   0 KEPT-F !
   0
   k 0 ?do
      id k i - OPERAND-AT REAL? if
         KEPT-F @ 1+ froom > if leave then
         KEPT-F @ 1+ KEPT-F !
      else
         KEPT-G @ 1+ groom > if leave then
         KEPT-G @ 1+ KEPT-G !
      then
      1+
   loop ;

\ ---- what a call site publishes, and what it takes back ----------------------
\ WHICH VALUE GOES INTO WHICH SLOT IS WRITTEN DOWN ONCE. Three readers stand
\ between a call operation and the two runs around it: the value store `i`
\ publishes, the value slot `i` holds when the branch comes back, and nothing
\ else. The lowering below reads them, and so does the residency pass further
\ down that decides which of those stores are already true - so the two cannot
\ come to disagree about which operand names which slot, which is exactly the
\ kind of disagreement that would publish one value where a callee reads another.
\
\ `kk` live values go out to slots zero upwards, then the `a` arguments; `m` are
\ kept, and a kept value has no operation here at all - it is in a register the
\ callee does not write, and the restore below says so by binding the source
\ value that comes back out of the call to the very value that went in.
: DSAVE-VAL ( IR-ID:ir-op-id n n n -- IR-ID:ir-value-id )
   {: id:IR-ID:ir-op-id kk:n m:n i:n :}
   i kk < if id i 1+ OPERAND-AT exit then
   id m i + 1+ OPERAND-AT ;

\ Slot `i` after the branch: below `kk` it is the live value the site saved,
\ coming back out of the very cell it went into, and at or above it the callee's
\ own result. Both are results of the source operation, which is what lets the
\ pass name a slot's content whether or not a load for it is built.
: DBACK-VAL ( IR-ID:ir-op-id n n n -- IR-ID:ir-value-id )
   {: id:IR-ID:ir-op-id kk:n m:n i:n :}
   i kk < if id i 1+ RESULT-AT exit then
   id m i + 1+ RESULT-AT ;

\ ---- the residency of one call site ------------------------------------------
\ The store run, applied to the map: the answer is one bit per position, set
\ where the cell already held the value that position publishes. A run longer
\ than the mask carries reports no position at all, so a site past the bound
\ emits every store exactly as it did before this pass existed.
: DBIT? ( n n -- bool )
   {: mask:n i:n :}
   i DELIDE-MAX >= if false exit then
   mask 1 i lshift and 0<> ;

: DSAVE-XFER ( IR-ID:ir-op-id n n n -- n )
   {: id:IR-ID:ir-op-id kk:n m:n a:n :}
   0
   kk a + 0 ?do
      id kk m i DSAVE-VAL  i  DPUT? if
         i DELIDE-MAX < if 1 i lshift or then
      then
   loop ;

\ AND WHAT THE BRANCH ITSELF DOES TO THE MAP. Every slot the callee could have
\ written stops holding anything this routine can name, which is every slot at or
\ above the callee's own base - and the callee's base is the caller's slot `kk`,
\ so the saved values below it are exactly the ones that survive. The run then
\ names all of `kk + r` again from the operation's own results, which covers the
\ survivors and the callee's answers in one statement.
: DBACK-XFER ( IR-ID:ir-op-id n n n -- )
   {: id:IR-ID:ir-op-id kk:n m:n r:n :}
   DKILL
   kk r + 0 ?do
      id kk m i DBACK-VAL  i  DRES!
   loop ;

: CALL-SAVE ( IR-ID:ir-op-id n n n -- )
   {: id:IR-ID:ir-op-id kk:n m:n a:n :}
   id kk m a DSAVE-XFER {: mask:n :}
   kk a + 0 ?do
      mask i DBIT? 0= if
         id  id kk m i DSAVE-VAL VOF  i A64IR:SLOT-WIDTH *  EMIT-DSTORE
      then
   loop ;

: CALL-RESTORE ( IR-ID:ir-op-id n n n -- )
   {: id:IR-ID:ir-op-id kk:n m:n r:n :}
   id kk m r DBACK-XFER
   kk r + 0 ?do
      id kk m i DBACK-VAL {: v:IR-ID:ir-value-id :}
      v DNEED? if
         v  id  i A64IR:SLOT-WIDTH *  EMIT-DLOAD  VBIND
      then
   loop
   m 0 ?do
      id kk i + 1+ RESULT-AT   id kk i + 1+ OPERAND   VBIND
   loop
   id 0 RESULT-AT  TOK  VBIND
   N-CALLS @ 1+ N-CALLS ! ;

\ A call to THIS routine keeps nothing. The callee is this same routine and its
\ contract destroys exactly the registers the allocator hands out, so there is no
\ register of the pool a value could survive in - and there is no recorded answer
\ to consult either, because the routine being compiled has not been published.
\ The four numbers are answered by one word because the residency pass asks for
\ them too, and a second reading of the convention could give another answer.
: SELF-SHAPE ( IR-ID:ir-op-id -- n n n n )
   {: id:IR-ID:ir-op-id :}
   ARGS SLOT-POSITIONS {: a:n :}
   OUTS SLOT-POSITIONS {: r:n :}
   id a r CALL-LIVE {: k:n :}
   a r k 0 ;

: EMIT-CALL ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id SELF-SHAPE {: a:n r:n kk:n m:n :}
   id 0 OPERAND TOK!
   id kk m a CALL-SAVE
   id  kk a + A64IR:SLOT-WIDTH *  kk r + A64IR:SLOT-WIDTH *  EMIT-BL
   id kk m r CALL-RESTORE ;

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

: WORD-ENTRY ( IR-ID:ir-op-id -- n )
   0 BND-ENTRY @ ATTR-INT-OF ;

\ The same four numbers a self-call answers, read off the operation's own record
\ of the callee's convention. One word for the same reason SELF-SHAPE is one: the
\ residency pass asks the same question and has to get the same answer.
: WORD-SHAPE ( IR-ID:ir-op-id -- n n n n )
   {: id:IR-ID:ir-op-id :}
   id 0 BND-IN @ ATTR-INT-OF {: a:n :}
   id 0 BND-OUT @ ATTR-INT-OF {: r:n :}
   id a r CALL-LIVE {: k:n :}
   id  id WORD-ENTRY  k KEEP-N {: m:n :}
   a r  k m -  m ;

: EMIT-WORD-CALL ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id WORD-SHAPE {: a:n r:n kk:n m:n :}
   id WORD-ENTRY {: e:n :}
   id 0 OPERAND TOK!
   id kk m a CALL-SAVE
   id  kk a + A64IR:SLOT-WIDTH *  kk r + A64IR:SLOT-WIDTH *
   e EMIT-WBL
   id kk m r CALL-RESTORE ;

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

\ A double literal, which is the same materialisation and one instruction more.
\
\ WHY IT IS THE MOVE-WIDE CHAIN AND A MOVE ACROSS, AND NOT THE FLOATING
\ IMMEDIATE. AArch64 has an FMOV with an eight-bit immediate, and it reaches
\ exactly the doubles whose exponent fits three bits and whose significand fits
\ four - 256 values in all. Every other double, which is every literal a real
\ program is likely to write, needs the bit pattern built somewhere else first.
\ So a compiler that used the immediate form would need BOTH paths and a rule
\ that decides between them, and would still reach the general path for most
\ literals. This chain builds the pattern in a general register with the move-wide
\ chain it already has and moves it across in one FMOV - which is the same route
\ the ENGINE takes at exactly this point: src/habu/habu1.f gets a data-stack cell
\ into a floating register with `0 A FMOVXD,` and never uses the immediate form
\ at all. Matching it means a compiled literal and an interpreted literal are the
\ same bits by construction and not by a second argument.
\
\ WHAT IT COSTS AND WHAT IT WOULD TAKE TO BEAT IT. One to four move-wide
\ instructions plus one FMOV, against one FMOV for the 256 immediate-reachable
\ doubles and a literal-pool load - one ADR and one LDR, plus a constant pool the
\ emission does not have - for the rest. Adding either is a measurable change to
\ make against the committed table rather than a guess, and dot
\ habu-materialise-a-double-4cf2b9a3 carries it. The pinned outputs are bit-exact
\ either way; what would move is the byte count and the cost.
: EMIT-FCONST ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id  id CONST-VALUE  MATERIALISE
   id A64IR-OPCODE:FMOVXD OPEN
   CTX BLD ACC IR-BUILD:ADD-OPERAND
   FRESULT+
   CLOSE-VALUE
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

\ The same two-in one-out shape in the floating file. It is a second word rather
\ than an argument to the first because the two differ in the file their result
\ belongs to, and that is what a machine operation's schema declares - so the
\ pair is checked when the operation is closed rather than assumed here.
: EMIT-FBINARY ( IR-ID:ir-op-id A64IR:opcode -- )
   {: id:IR-ID:ir-op-id o:A64IR:opcode :}
   id o OPEN
   CTX BLD  id 0 OPERAND  IR-BUILD:ADD-OPERAND
   CTX BLD  id 1 OPERAND  IR-BUILD:ADD-OPERAND
   FRESULT+
   CLOSE-VALUE
   id 0 RESULT-AT  ACC  VBIND ;

\ One value in, one out, answering in the floating file. Six source operations
\ take this shape and they are not all the same kind of thing, which is the
\ point: the three unary float words answer a double from a double, `s>f` rounds
\ a cell into one, and the reinterpretation moves a cell's bits across without
\ rounding. What they share is where the ANSWER goes, and that is all this word
\ decides; which instruction runs is the caller's opcode.
: EMIT-FUNARY ( IR-ID:ir-op-id A64IR:opcode -- )
   {: id:IR-ID:ir-op-id o:A64IR:opcode :}
   id o OPEN
   CTX BLD  id 0 OPERAND  IR-BUILD:ADD-OPERAND
   FRESULT+
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
\ The exit run, applied to the map, answering which of the routine's results are
\ already in the cells the convention publishes them from. A result the caller
\ handed in and the routine only passed along is such a value, which is the whole
\ of what this pass is for.
\ THE ARITY IS HELD HERE AND NOT AT THE LOWERING, because this is where the
\ return is first read: the residency pass walks the operations before a single
\ one is lowered, so a return whose operand list disagrees with the convention
\ has to be refused by name here or it would be read past instead.
: DEXIT-XFER ( IR-ID:ir-op-id n -- n )
   {: id:IR-ID:ir-op-id r:n :}
   id OPERANDS-OF r <> if E-A64SEL-PLACE throw then
   0
   r 0 ?do
      id i OPERAND-AT  OUTS i A64EFF:SEQ-SLOT@  DPUT? if
         i DELIDE-MAX < if 1 i lshift or then
      then
   loop ;

: EMIT-EXIT ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   OUTS SLOT-POSITIONS {: r:n :}
   id r DEXIT-XFER {: mask:n :}
   r 0 ?do
      mask i DBIT? 0= if
         id  id i OPERAND  OUTS i A64EFF:SEQ-SLOT@ A64IR:SLOT-WIDTH *  EMIT-DSTORE
      then
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
\ own opcode. Every form below asks here - the three that answer a Habu flag, the
\ three fused compare-and-branches and the six fused compare-and-selects - so the
\ pairing of a source relation to a machine condition is written once: a
\ comparison lowered under the wrong condition is one wrong line of this table
\ rather than several lines that can disagree with each other. An operation that
\ is not one of the eleven is refused by name, because a caller asking this of
\ anything else has already gone wrong.
\
\ THE FIVE FLOAT ROWS ARE NOT THE INTEGER ROWS REPEATED, and this is the one
\ table in the chain where reading a condition off a relation's name would be
\ wrong. What follows is the derivation, because three separate lowerings now
\ rest on it and each of them reads the flags an Fcmp left.
\
\ (a) WHAT THE FLAGS ARE. An Fcmp of x against y sets NZCV from x-y, except that
\ when either operand is a NaN it raises the UNORDERED condition instead, which
\ is N=0 Z=0 C=1 V=1. The four cases are the whole input to this table:
\
\     x < y        N=1 Z=0 C=0 V=0
\     x > y        N=0 Z=0 C=1 V=0
\     x = y        N=0 Z=1 C=1 V=0
\     unordered    N=0 Z=0 C=1 V=1
\
\ (b) WHAT THE ENGINE ANSWERS, which is the contract a compiled body has to
\ match. src/habu/habu1.f (FCMP) is `Fcmp d0,d1; Cset a,cc; Sub a,xzr,a` under
\ cc = MI for `f<`, GT for `f>` and EQ for `f=`, and (FCMP0) is the same three
\ against the immediate zero under MI for `f0<` and EQ for `f0=`. So the Habu
\ flag is all bits set exactly when cc holds, and the measured consequence -
\ every float comparison answers FALSE for a NaN - is survey (4) at the head of
\ tools/codegen-compare-corpus3.f.
\
\ (c) THE CONDITIONS, READ AGAINST (a). Each row is the architecture's own test
\ evaluated on each of the four flag states:
\
\     condition code  the test         x<y    x>y    x=y    unordered
\     mi         4    N = 1            TRUE   false  false  false
\     gt        12    Z=0 and N = V    false  TRUE   false  false
\     equal      0    Z = 1            false  false  TRUE   false
\     lt        11    N != V           TRUE   false  false  TRUE
\     le        13    Z=1 or N != V    TRUE   false  TRUE   TRUE
\     ne         1    Z = 0            TRUE   TRUE   false  TRUE
\     ge        10    N = V            false  TRUE   TRUE   false
\     hs         2    C = 1            false  TRUE   TRUE   TRUE
\
\ `mi`, `gt` and `equal` are the three whose column is TRUE in exactly one of the
\ four states and false in the unordered one, so they are the three that mean
\ what the engine's `f<`, `f>` and `f=` mean on every input. That is why this
\ table gives the float rows those three and not the ones their names suggest.
\
\ (d) THE ROW THAT WOULD HAVE BEEN WRONG, by name. `f<` reads as less-than, and
\ the machine condition called less-than is `lt` - which agrees with `mi` on all
\ three ORDERED states and disagrees on the fourth. A table that took `lt` would
\ therefore pass every test that did not hand it a NaN and then take the wrong
\ arm on one, and the same holds for `f0<`. `ne` and `hs` are unordered-TRUE for
\ the same reason. test/compiler/native-select.f asserts the two `lt`-shaped rows
\ answer `mi`, which is the negative control for this whole derivation.
\
\ (e) WHY THIS TABLE DECIDES THE ARM AND NOT ONLY THE FLAG. A fused branch and a
\ fused select both read the flags directly, so nothing downstream of them can
\ repair a wrong condition: the branch goes to its condition-holds successor and
\ the select writes its first source, and both of those were wired to the arm the
\ source's `if` takes when the relation HOLDS. Under `mi` a NaN takes neither, so
\ control and value both go the way the interpreted word goes; under `lt` both
\ would go the other way. The wiring is identical either way, which is why the
\ NaN rule lives here and nowhere else.
\
\ There is no float row for `<=`, `>=` or `<>` because the engine has no such
\ word - the three float relations it has are these three, twice over, once
\ against another double and once against zero. src/compiler/native/a64ir.f
\ carries the flag table and the condition codes this derivation rests on.
: COMPARE-COND ( IR-ID:ir-op-id -- A64IR:cond )
   OPCODE-AT OPCODE-SLOT
   case
      O-LT of A64IR-COND:LT    endof
      O-LE of A64IR-COND:LE    endof
      O-GT of A64IR-COND:GT    endof
      O-GE of A64IR-COND:GE    endof
      O-EQ of A64IR-COND:EQUAL endof
      O-NE of A64IR-COND:NE    endof
      O-FLT  of A64IR-COND:MI    endof
      O-FGT  of A64IR-COND:GT    endof
      O-FEQ  of A64IR-COND:EQUAL endof
      O-FLTZ of A64IR-COND:MI    endof
      O-FEQZ of A64IR-COND:EQUAL endof
      E-A64SEL-OPCODE throw
   endcase ;

\ The kind table itself, answered for every member of the source family, over the
\ vocabulary declared at the head of this file.
: COMPARE-KIND ( HIR:opcode -- A64SEL:cmpkind )
   MATCH HIR:opcode
      const  OF A64SEL-CMPKIND:NONE ENDOF
      add    OF A64SEL-CMPKIND:NONE ENDOF
      sub    OF A64SEL-CMPKIND:NONE ENDOF
      mul    OF A64SEL-CMPKIND:NONE ENDOF
      div    OF A64SEL-CMPKIND:NONE ENDOF
      lt     OF A64SEL-CMPKIND:GPR  ENDOF
      le     OF A64SEL-CMPKIND:GPR  ENDOF
      gt     OF A64SEL-CMPKIND:GPR  ENDOF
      ge     OF A64SEL-CMPKIND:GPR  ENDOF
      equal  OF A64SEL-CMPKIND:GPR  ENDOF
      ne     OF A64SEL-CMPKIND:GPR  ENDOF
      and    OF A64SEL-CMPKIND:NONE ENDOF
      or     OF A64SEL-CMPKIND:NONE ENDOF
      xor    OF A64SEL-CMPKIND:NONE ENDOF
      lshift OF A64SEL-CMPKIND:NONE ENDOF
      rshift OF A64SEL-CMPKIND:NONE ENDOF
      invert OF A64SEL-CMPKIND:NONE ENDOF
      mem    OF A64SEL-CMPKIND:NONE ENDOF
      load   OF A64SEL-CMPKIND:NONE ENDOF
      store  OF A64SEL-CMPKIND:NONE ENDOF
      bload  OF A64SEL-CMPKIND:NONE ENDOF
      bstore OF A64SEL-CMPKIND:NONE ENDOF
      br     OF A64SEL-CMPKIND:NONE ENDOF
      brz    OF A64SEL-CMPKIND:NONE ENDOF
      call   OF A64SEL-CMPKIND:NONE ENDOF
      wordcall OF A64SEL-CMPKIND:NONE ENDOF
      return OF A64SEL-CMPKIND:NONE ENDOF
      fconst   OF A64SEL-CMPKIND:NONE ENDOF
      fadd     OF A64SEL-CMPKIND:NONE ENDOF
      fsub     OF A64SEL-CMPKIND:NONE ENDOF
      fmul     OF A64SEL-CMPKIND:NONE ENDOF
      fdiv     OF A64SEL-CMPKIND:NONE ENDOF
      fneg     OF A64SEL-CMPKIND:NONE ENDOF
      fabs     OF A64SEL-CMPKIND:NONE ENDOF
      fsqrt    OF A64SEL-CMPKIND:NONE ENDOF
      flt      OF A64SEL-CMPKIND:FREG  ENDOF
      fgt      OF A64SEL-CMPKIND:FREG  ENDOF
      feq      OF A64SEL-CMPKIND:FREG  ENDOF
      fltz     OF A64SEL-CMPKIND:FZERO ENDOF
      feqz     OF A64SEL-CMPKIND:FZERO ENDOF
      intreal  OF A64SEL-CMPKIND:NONE ENDOF
      realint  OF A64SEL-CMPKIND:NONE ENDOF
      bitsreal OF A64SEL-CMPKIND:NONE ENDOF
      realbits OF A64SEL-CMPKIND:NONE ENDOF
   ;MATCH ;

: SLOT-KIND ( n -- A64SEL:cmpkind )
   SLOT-OPCODE COMPARE-KIND ;

: COMPARE-SLOT? ( n -- bool )
   SLOT-KIND A64SEL-CMPKIND:NONE A64SEL-CMPKIND:EQ 0= ;

\ How many values a comparison of this shape reads. The zero comparisons read
\ one, because the instruction compares against an immediate the form carries and
\ not against a register anything computed. It is read off the kind rather than
\ off the schema so that the one authority over what a shape IS stays the table
\ above; the schema still refuses an operation staged with the wrong count.
: KIND-OPERANDS ( A64SEL:cmpkind -- n )
   MATCH A64SEL:cmpkind
      none  OF E-A64SEL-OPCODE throw ENDOF
      gpr   OF 2 ENDOF
      freg  OF 2 ENDOF
      fzero OF 1 ENDOF
   ;MATCH ;

: KIND-FLAG-OPCODE ( A64SEL:cmpkind -- A64IR:opcode )
   MATCH A64SEL:cmpkind
      none  OF E-A64SEL-OPCODE throw ENDOF
      gpr   OF A64IR-OPCODE:FLAG   ENDOF
      freg  OF A64IR-OPCODE:FFLAG  ENDOF
      fzero OF A64IR-OPCODE:FFLAGZ ENDOF
   ;MATCH ;

: KIND-FUSED-OPCODE ( A64SEL:cmpkind -- A64IR:opcode )
   MATCH A64SEL:cmpkind
      none  OF E-A64SEL-OPCODE throw ENDOF
      gpr   OF A64IR-OPCODE:CMPBR   ENDOF
      freg  OF A64IR-OPCODE:FCMPBR  ENDOF
      fzero OF A64IR-OPCODE:FCMPBRZ ENDOF
   ;MATCH ;

: OP-KIND ( IR-ID:ir-op-id -- A64SEL:cmpkind )
   OPCODE-AT OPCODE-SLOT SLOT-KIND ;

\ One source comparison is one machine comparison, under the condition the source
\ opcode names. The machine form is three instructions and one operation, because
\ the condition flags the three pass between them are a single architectural
\ resource with no value of the machine dialect to stand for it; the dialect says
\ so, and this pass only has to name the condition.
\
\ ONE WORD SERVES ALL ELEVEN, and the two things that differ between them - which
\ instruction compares, and how many registers it reads - both come off the kind.
\ The RESULT is the same in every case: a Habu flag is a number, so it goes in a
\ general register whichever file the values compared came out of, and a
\ comparison that answered into the floating file would be a flag no branch of
\ this machine could test.
: EMIT-FLAG ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id OP-KIND {: k:A64SEL:cmpkind :}
   id k KIND-FLAG-OPCODE OPEN
   k KIND-OPERANDS 0 ?do
      CTX BLD  id i OPERAND  IR-BUILD:ADD-OPERAND
   loop
   RESULT+
   CTX BLD  CTX BLD A64IR:KEY-COND
   CTX BLD  id COMPARE-COND  A64IR:COND-ATTR  IR-BUILD:ADD-ATTR
   CLOSE-VALUE
   id 0 RESULT-AT  ACC  VBIND ;

\ ---- selecting the branches --------------------------------------------------
\ A successor is a block of the source module, and this pass rebuilds the source
\ blocks in order, so the edge is carried across by its ordinal. The ordinals
\ are not always the same two numbers, because a block the if-conversion below
\ absorbed into an earlier one is not rebuilt at all - so every edge goes
\ through the one table that says which machine block a source block became.
\ Without a region to convert that table is the identity and this is the same
\ statement it always was; with one it is the only place the two numberings
\ meet, which is what stops a branch from naming a block that is no longer
\ there. A branch INTO an absorbed block is refused by name rather than
\ renumbered to something: an absorbed block's only predecessor is inside its
\ own region, and that edge is what the conversion replaced.
: BLOCK-ORD-CK ( n -- n )
   dup 0 < over NFROZEN:BMAX >= or if E-A64SEL-CAP throw then ;

: R-ORD-OF ( n -- n )
   BLOCK-ORD-CK cells R-ORD + @
   dup 0 < if E-A64SEL-SHAPE throw then ;

\ ---- which block arguments are built -----------------------------------------
\ The map at a block's head, read by the block's own ordinal. The rows are here
\ rather than beside the pass that fills them because the branch below and the
\ block opening further down both ask which of a destination's arguments are
\ built, and the answer has to be one answer.
: DIN-AT ( n n -- n )
   {: b:n s:n :}
   s DIN-WINDOW? 0= if DNONE exit then
   b BLOCK-ORD-CK DSLOT-MAX * s + cells D-IN + @ ;

: DIN-AT! ( n n n -- )
   {: v:n b:n s:n :}
   s DIN-WINDOW? 0= if exit then
   v  b BLOCK-ORD-CK DSLOT-MAX * s + cells D-IN + ! ;

: DIN-HOLDS? ( n IR-ID:ir-value-id -- bool )
   {: b:n v:IR-ID:ir-value-id :}
   v VSLOT {: k:n :}
   false
   DSLOT-MAX 0 ?do
      b i DIN-AT k = if drop true leave then
   loop ;

\ A block argument is not built when the value it carries lives in a data-stack
\ slot however control arrived AND nothing reads it out of a register. The values
\ every edge hands it are then not built either, which is what removes the copy an
\ edge would otherwise make.
\
\ WHY BOTH HALVES, WHEN ONE WOULD BE SOUND. Nothing needs a register is the whole
\ of the safety argument: an argument nothing reads can be left out and the module
\ still says what it said. But an argument that no slot holds and nothing reads is
\ a DEAD value, which is a different fact with a different owner - the source
\ module should not be carrying it at all - and removing it here would be this
\ pass quietly doing dead-code elimination under the name of residency. Dot
\ habu-drop-a-dead-1c0ff5a2 carries that half.
: DDROP? ( n n -- bool )
   {: b:n i:n :}
   FUN b BLOCK-AT i ARG-AT {: v:IR-ID:ir-value-id :}
   v DNEED? if false exit then
   b v DIN-HOLDS? ;

\ A block names itself twice over: by its ordinal in the module, which is what a
\ successor carries, and by its ordinal in its own function, which is what the
\ readers above index by and what the plan's rows are keyed on. The two differ
\ by where the function's blocks start, and they differ AGAIN on the new side
\ because the functions in front of this one may have emitted fewer blocks than
\ they were selected from. Both bases are held here so a successor crosses
\ through one subtraction and one addition and no caller does the arithmetic.
: SUCC-IDX ( IR-ID:ir-op-id n -- n )
   SUCC-AT IR-ID:BLOCK-LOCAL  R-BASE @ -  BLOCK-ORD-CK ;

: SUCCESSOR-ORD+ ( n -- )
   {: b:n :}
   CTX BLD
   BLD IR-BUILD:MODULE-KEY  R-NEWBASE @ b R-ORD-OF +  IR-ID:PACK-BLOCK
   IR-BUILD:ADD-SUCCESSOR ;

: SUCCESSOR+ ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id i:n :}
   id i SUCC-IDX SUCCESSOR-ORD+ ;

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

\ THE COPY IS MADE IN THE FILE THE VALUE LIVES IN, AND THAT IS THE WHOLE
\ DIFFERENCE BETWEEN THE TWO ARMS. A cell is copied with a64.mov and a double
\ with a64.fmovdd, which is the same instruction in the other register file: FMOV
\ Dd, Dn moves eight bytes from one D register to another and reads none of them
\ as a number. Copying a double with the general move would be eight bytes taken
\ out of a register that does not hold them, so the file is not a preference here
\ - it is what makes the copy the same value at the other end.
\
\ WHICH VALUE IS ASKED, AND WHY IT IS NOT THE ONE BEING COPIED. The value handed
\ over here is a value of the NEW module - what the source operand selected to -
\ and REAL? reads the type the SOURCE module gives a value, so asking it about
\ this one would be presenting one module's identity to another module's table.
\ The class comes from the source operand instead, which is where the question
\ belongs: the source dialect is what says whether a value is a double.
: EMIT-COPY ( IR-ID:ir-op-id IR-ID:ir-value-id bool -- IR-ID:ir-value-id )
   {: at:IR-ID:ir-op-id v:IR-ID:ir-value-id real:bool :}
   real if
      at A64IR-OPCODE:FMOVDD OPEN
      CTX BLD v IR-BUILD:ADD-OPERAND
      FRESULT+
      CLOSE-VALUE
      ACC exit
   then
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
   id  id i OPERAND  id i OPERAND-AT REAL?  EMIT-COPY ;

\ The block this branch goes on to. Which of its arguments are built is what
\ decides which of this branch's operands cross, and the two questions are asked
\ of the one block so the positions cannot slide against each other.
: BR-TARGET ( IR-ID:ir-op-id -- n )
   0 SUCC-IDX ;

: EMIT-BR ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id OPERANDS-OF {: k:n :}
   k EDGE-MAX > if E-A64SEL-CAP throw then
   id BR-TARGET {: tb:n :}
   0
   k 0 ?do
      tb i DDROP? 0= if
         id i EDGE-VALUE  over EDGE-V !
         1+
      then
   loop
   {: n:n :}
   id A64IR-OPCODE:BR OPEN
   n 0 ?do
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
\ EVERY FLOAT OPERATION ANSWERS FALSE AND NONE OF THEM NEEDS TO ANSWER
\ OTHERWISE: the source dialect declares them all total, because IEEE754
\ arithmetic answers an infinity or the default NaN where integer arithmetic
\ would trap, so there is no trap for the lowering to lose. The two sides agree
\ rather than one of them being relaxed - if HIR ever declared a float operation
\ trapping, TRAP-CK would refuse it here until this table said how it survives.
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
\     of the eleven comparisons - six over cells and five over doubles, which the
\     kind table above is the one authority for. Adjacency is the whole of the scheduling
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
: FUSE-INDEX ( IR-ID:ir-block-id -- n )
   {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT {: n:n :}
   n 2 < if -1 exit then
   bk n 1- OP-AT OPCODE-AT OPCODE-SLOT O-BRZ <> if -1 exit then
   bk n 2 - OP-AT {: d:IR-ID:ir-op-id :}
   d OPCODE-AT OPCODE-SLOT COMPARE-SLOT? 0= if -1 exit then
   d RESULTS-OF 1 <> if -1 exit then
   d 0 RESULT-AT  bk n 1- OP-AT 0 OPERAND-AT  SAME-VALUE? 0= if -1 exit then
   d 0 RESULT-AT USES-OF 1 <> if -1 exit then
   d OPCODE-AT {: sym:IR-ID:ir-symbol-id :}
   sym OPCODE-SLOT SLOT-OPCODE  sym TRAP-CK  drop
   n 2 - ;

: FUSE-SCAN ( IR-ID:ir-block-id -- )
   FUSE-INDEX FUSE-AT ! ;


\ ---- which selections become a select instead of a branch --------------------
\ THE SHAPE, IN ONE SENTENCE. A two-way branch whose two arms compute a few
\ values and meet again at one block does not need the branch at all: the
\ machine can run both arms and choose the answer with a Csel, which is one
\ instruction and nothing for a predictor to get wrong. This is the pass that
\ decides which two-way branches are that shape, and the emission below is what
\ replaces them.
\
\ WHY IT IS HERE AND NOT IN A PASS OF ITS OWN, which is the same argument the
\ comparison fusion above makes and it decides this the same way. This pass is
\ already the only place that says which machine operations a source operation
\ becomes; a pass that rewrote the source module into a select-shaped source
\ module would need a source form for a select, and a select is a machine form.
\ Rewriting the MACHINE module afterwards would mean building the branches and
\ the blocks and then deleting them, which is the shape src/compiler/native/
\ spill.f has and has for a reason this transform does not share: a spill plan
\ is the ALLOCATOR's output and cannot exist before the module is frozen, while
\ every fact this decision needs is in the frozen SOURCE module before a single
\ operation is selected.
\
\ THE ADMISSION RULE, DERIVED RATHER THAN CHOSEN. Four things have to be true,
\ and each of them is the answer to a question the transform cannot avoid.
\
\   1. WHAT MAY RUN THAT WOULD NOT HAVE RUN. The conversion runs both arms, so
\      every operation of an arm has to be one the program can run on a path it
\      would not have taken. That is exactly "cannot raise, and touches no
\      memory", and both are read off the SOURCE dialect's own schema - the
\      may-trap flag and the memory effect - rather than off a list of opcode
\      names kept here. A division stays branched because its schema says it
\      traps; a load stays branched because its schema says it reads; a call
\      stays branched because its schema says both.
\
\   2. WHERE THE ARMS MEET. Every path out of the branch has to reach ONE block,
\      because the values the arms compute are handed to that block and a select
\      is what chooses between them. The region is grown from the branch's two
\      successors: a block every path to which comes from inside the region is
\      part of it, and the first block that is reached from anywhere else is
\      where the region leaves. Two different such blocks mean the arms do not
\      meet and there is nothing to select.
\
\   3. WHY EVERY BLOCK OF THE REGION HAS EXACTLY ONE PREDECESSOR IN IT. Because
\      this pass emits each block's operations ONCE. A block reached on two
\      paths inside the region would arrive with two different sets of block
\      arguments, so its operations would compute two different things and one
\      copy of them cannot stand for both. Converting that shape needs a
\      predicate per block and a select for every block argument, which is a
\      different and larger transform; dot habu-if-convert-a-774efe46
\      carries it. Here the region is a TREE hanging off the branch, every
\      block's arguments have one source, and the value a path hands the exit is
\      read off the tree from the leaves up.
\
\   4. WHY THE ORDINALS ONLY GO FORWARD. A region that could reach a block with
\      a lower ordinal than the one branching to it would be a region containing
\      a loop, and running a loop speculatively is not running a few extra
\      instructions. The elaborator numbers a loop's latch above its header, so
\      "every edge inside the region goes to a higher ordinal" is exactly "the
\      region is acyclic" - and it is also what makes plain ordinal order a
\      topological order, which is what lets the emission below walk the members
\      once, forwards for the operations and backwards for the values.
\
\ AND WHAT IT DOES NOT ADMIT, by name and with the reason. The three bounds at
\ the head of this file: a region wider, deeper or busier than a small selection
\ is refused because the trade it makes stops paying. And a region whose selects
\ would not FIT: the two pool questions below, asked once per register file.
\
\ THE TYPE OF A CHOSEN VALUE IS NOT ONE OF THE REASONS, AND THAT IS THE POINT OF
\ THE SPLIT. A join that carries a double used to be refused here because the
\ only select this dialect had wrote a general register; it now carries one that
\ writes a D register, so a double is admitted on exactly the same terms a cell
\ is and the only thing its type decides is which pool its arms are counted
\ against. What that costs is one more question rather than one more rule: every
\ count below is taken per file, because a register number names a register of
\ one file and a sum across the two would be held against a pool neither half
\ comes out of.
: TERM-OP ( IR-ID:ir-block-id -- IR-ID:ir-op-id )
   {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT {: n:n :}
   n 1 < if E-A64SEL-SHAPE throw then
   bk n 1- OP-AT ;

: OP-SLOT ( IR-ID:ir-op-id -- n )
   OPCODE-AT OPCODE-SLOT ;

: BRZ-TERM? ( IR-ID:ir-block-id -- bool )
   TERM-OP OP-SLOT O-BRZ = ;

: BR-TERM? ( IR-ID:ir-block-id -- bool )
   TERM-OP OP-SLOT O-BR = ;

\ May this operation be run on a path the program would not have taken? The two
\ halves of the answer are the source dialect's own declarations and nothing
\ else: an operation whose schema may trap could raise where the program never
\ would, and one whose schema names any memory effect could read or write where
\ the program never would.
: SPECULABLE? ( IR-ID:ir-op-id -- bool )
   OPCODE-AT {: sym:IR-ID:ir-symbol-id :}
   V-SCHR VW sym IR-SCHEMA:FTRAPS? if false exit then
   V-SCHR VW sym IR-SCHEMA:FEFFECT@
   IR--SCHEMA-EFFECT:PURE IR--SCHEMA-EFFECT:EQ ;

\ Everything but the terminator has to be speculable, and the terminator has to
\ be a branch this walk can follow.
: MEMBER-OK? ( IR-ID:ir-block-id -- bool )
   {: bk:IR-ID:ir-block-id :}
   bk BR-TERM? bk BRZ-TERM? or 0= if false exit then
   bk OP-COUNT 1- {: n:n :}
   true
   n 0 ?do
      bk i OP-AT SPECULABLE? 0= if drop false leave then
   loop ;

\ WHICH COMPARISON A BLOCK'S SELECT FUSES WITH IS THE SAME QUESTION AS WHICH ONE
\ ITS BRANCH FUSES WITH, and it is FUSE-INDEX above for both. It used to be a
\ second and narrower question: a select had to read the flags a Cmp of two
\ GENERAL registers left, because those were the only conditional-select forms
\ the dialect had, so a float comparison materialised its Habu flag with
\ a64.fflag and the select tested that number against zero. The dialect now has
\ the other row of the square - a64.fcmpsel, a64.fcmpselz, a64.fcmpseld and
\ a64.fcmpselzd, whose first instruction is the Fcmp - so every comparison that
\ fuses into a branch fuses into a select as well and there is no filter left to
\ write. What a float comparison changes is not WHETHER it fuses but which POOL
\ its compared registers come out of, which is the next question below.
\
\ ---- whether the routine has registers for the form at all -------------------
\ A select reads every one of its sources AT ONE INSTANT, which is what makes
\ this a question about the routine's POOL rather than about pressure anywhere.
\ A routine whose pool is smaller than that cannot hold the instruction whatever
\ it puts away: a spill frees a register by moving a value that is NOT wanted at
\ that instant, and every one of a select's sources is. So the read count is a
\ floor no allocation can get under, and a region that would need more than the
\ routine has stays branched - refusing the conversion is always correct, where
\ refusing the ROUTINE would turn an optimisation into a compilation failure.
\
\ THE FLOOR IS TWO NUMBERS AND NOT ONE, BECAUSE A REGISTER NUMBER NAMES A FILE.
\ d3 and x3 are two registers, so a count that added a double to a cell would be
\ holding a sum against a pool neither half comes out of. The read count
\ therefore splits exactly the way the instruction does:
\
\   the COMPARE half is one file or the other, and WHICH is the kind of the
\     comparison the select fuses with. A select that fuses with nothing tests
\     the cell the source branch tests, which is one GENERAL register. A select
\     that fuses with a comparison of two cells reads two general ones. A select
\     that fuses with a FLOAT comparison reads no general register at all and
\     reads the D registers the Fcmp names instead - two of them, or one for the
\     form that compares against the instruction's own zero. That count is not
\     written out again here: KIND-OPERANDS above already says how many
\     registers each kind of comparison reads, and the kind already says which
\     file they are registers of, so both floors come off the one table.
\
\   the CHOSEN half is two registers of the file the position lives in, and a
\     region may have positions in both files at once, so both counts get their
\     two as soon as the region has one position of that file. They are not
\     summed over the positions: the selects of one block run at one instant
\     each and a register a select has finished with is free for the next, so
\     the floor is the widest single instruction and not the whole row. What
\     the whole row costs is pressure, which is the next question below.
\
\ SO THE SIX FLOORS ARE, per shape, general first and floating second: an
\ unfused select choosing cells 1+2 and 0; unfused choosing doubles 1 and 2;
\ cell-compare fused choosing cells 2+2 and 0; cell-compare fused choosing
\ doubles 2 and 2; float-compare fused choosing cells 2 and 2 (or 1 for the zero
\ form); float-compare fused choosing doubles 0 and 2+2 (or 1+2). A region with
\ positions in both files takes the sum in each, which is exactly what the two
\ words below add up.
\
\ THE ANSWER IS NOT COUNTED, and that is a statement about the allocator rather
\ than a rounding down. A select's result may take the register of a source that
\ dies at it, which is the ordinary case and is why the two-argument bodies this
\ leaf exists for fit in four registers with room to spare. A routine sitting
\ exactly on the floor may still be refused, and that refusal is the allocator's
\ own answer about the whole routine; this pass is not in a position to predict
\ it and does not try.
1 constant SELZ-CMP-REGS             \ the cell a zero-test select reads
2 constant SEL-ARM-REGS              \ the two any of them chooses between

: GPR-POOL-N ( -- n )
   0 S-POOL @ A64EFF:GPRS-N BITS-N ;

: FPR-POOL-N ( -- n )
   0 S-FPOOL @ A64EFF:FPRS-N BITS-N ;

\ Whether the comparison a block's select fuses with compares two general
\ registers. It is asked twice below, once per file, so the two floors cannot
\ disagree about which file a compare's operands come out of.
: FUSED-GPR? ( IR-ID:ir-block-id n -- bool )
   {: bk:IR-ID:ir-block-id fz:n :}
   bk fz OP-AT OP-KIND  A64SEL-CMPKIND:GPR A64SEL-CMPKIND:EQ ;

\ How many GENERAL registers the compare half of this block's select reads.
: SEL-CMP-G ( IR-ID:ir-block-id -- n )
   {: bk:IR-ID:ir-block-id :}
   bk FUSE-INDEX {: fz:n :}
   fz 0 < if SELZ-CMP-REGS exit then
   bk fz FUSED-GPR? 0= if 0 exit then
   bk fz OP-AT OP-KIND KIND-OPERANDS ;

\ And how many FLOATING ones, which is the same question the other way round: a
\ float comparison's operands are the two D registers it compares, or the one it
\ compares against the instruction's own zero.
: SEL-CMP-F ( IR-ID:ir-block-id -- n )
   {: bk:IR-ID:ir-block-id :}
   bk FUSE-INDEX {: fz:n :}
   fz 0 < if 0 exit then
   bk fz FUSED-GPR? if 0 exit then
   bk fz OP-AT OP-KIND KIND-OPERANDS ;

\ What one block's selects read out of the general file: the compare's general
\ registers, plus the arms of any position that is not a double. A block that
\ does not end in a two-way branch emits no select and reads nothing.
: SEL-NEED-G ( IR-ID:ir-block-id -- n )
   {: bk:IR-ID:ir-block-id :}
   bk BRZ-TERM? 0= if 0 exit then
   bk SEL-CMP-G
   R-WIDTH @ R-WIDTH-D @ > if SEL-ARM-REGS + then ;

\ And out of the floating file: the compare's D registers when an Fcmp is what
\ writes the flags, plus the arms of any position that is a double.
: SEL-NEED-F ( IR-ID:ir-block-id -- n )
   {: bk:IR-ID:ir-block-id :}
   bk BRZ-TERM? 0= if 0 exit then
   bk SEL-CMP-F
   R-WIDTH-D @ 0 > if SEL-ARM-REGS + then ;

\ How many VALUES one block of the region would compute on a path the program
\ would not have taken. It is the block's operations less its terminator, which
\ is not selected at all, and less the comparison its branch fuses with, which
\ selects to no register either - the select stands for both. Every one of the
\ rest defines a value that is live from where it is computed to the select that
\ reads it, which is why this is the number the pool is held against.
: SPEC-DEFS ( IR-ID:ir-block-id -- n )
   {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT 1-
   bk FUSE-INDEX 0 >= if 1- then ;

\ Does this operation define a double? An operation that defines nothing at all
\ answers no, which is the right answer for the floating count and leaves it in
\ the general one - where the total above already counts it, and where counting
\ a value that needs no register is the conservative direction.
: REAL-DEF? ( IR-ID:ir-op-id -- bool )
   {: id:IR-ID:ir-op-id :}
   id RESULTS-OF 1 < if false exit then
   id 0 RESULT-AT REAL? ;

\ How many of that block's speculated values are doubles. The two counts are
\ taken over exactly the same operations - the same terminator and the same
\ fused comparison are left out - so the general count is the difference and
\ neither file's pressure can be counted twice or missed.
: SPEC-DEFS-D ( IR-ID:ir-block-id -- n )
   {: bk:IR-ID:ir-block-id :}
   bk FUSE-INDEX {: fz:n :}
   bk OP-COUNT 1- {: k:n :}
   0
   k 0 ?do
      i fz <> if
         bk i OP-AT REAL-DEF? if 1+ then
      then
   loop ;

\ ---- the per-block rows, read and written ------------------------------------
: R-PRED@ ( n -- n )     BLOCK-ORD-CK cells R-PRED + @ ;
: R-FROM@ ( n -- n )     BLOCK-ORD-CK cells R-FROM + @ ;
: R-ABSORB? ( n -- bool ) BLOCK-ORD-CK cells R-ABSORB + @ 0<> ;
: R-OWNER@ ( n -- n )    BLOCK-ORD-CK cells R-OWNER + @ ;
: R-HEAD? ( n -- bool )  BLOCK-ORD-CK cells R-HEAD + @ 0<> ;
: R-EXIT@ ( n -- n )     BLOCK-ORD-CK cells R-EXIT + @ ;
: R-MARK? ( n -- bool )  BLOCK-ORD-CK cells R-MARK + @ 0<> ;

: R-RESET1 ( n -- )
   {: b:n :}
   0 b cells R-PRED + !
   -1 b cells R-FROM + !
   0 b cells R-ABSORB + !
   -1 b cells R-OWNER + !
   0 b cells R-HEAD + !
   -1 b cells R-EXIT + !
   -1 b cells R-ORD + !
   0 b cells R-MARK + ! ;

: R-RESET ( -- )
   NFROZEN:BMAX 0 ?do i R-RESET1 loop ;

\ ---- counting the predecessors -----------------------------------------------
\ How many edges reach each block, taken off every terminator's successor list.
\ It is the one fact the region growth turns on: a block every path to which
\ comes from inside the region has exactly one predecessor and belongs to it,
\ and the first block that is reached from anywhere else is where the region
\ leaves.
: PRED-NOTE ( n n -- )
   {: p:n t:n :}
   t BLOCK-ORD-CK cells R-PRED + dup @ 1+ swap !
   p t BLOCK-ORD-CK cells R-FROM + ! ;

: PRED-NOTE-OP ( n IR-ID:ir-op-id -- )
   {: home:n t:IR-ID:ir-op-id :}
   t SUCCS-OF 0 ?do
      home  t i SUCC-IDX  PRED-NOTE
   loop ;

: PRED-SCAN ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f BLOCK-COUNT 0 ?do
      i  f i BLOCK-AT TERM-OP  PRED-NOTE-OP
   loop ;

\ ---- growing one region ------------------------------------------------------
: R-TRY-RESET ( -- )
   0 R-QN !
   0 R-QI !
   0 R-LIST-N !
   0 R-SPEC !
   0 R-SPEC-D !
   -1 R-JOIN !
   0 R-WIDTH-D !
   NFROZEN:BMAX 0 ?do 0 i cells R-MARK + ! loop ;

: R-PUSH ( n n -- )
   {: p:n b:n :}
   R-QN @ NFROZEN:BMAX >= if E-A64SEL-CAP throw then
   b R-QB R-QN @ cells + !
   p R-QP R-QN @ cells + !
   R-QN @ 1+ R-QN ! ;

: R-TAKE ( n -- )
   {: b:n :}
   R-LIST-N @ NFROZEN:BMAX >= if E-A64SEL-CAP throw then
   1 b BLOCK-ORD-CK cells R-MARK + !
   b R-LIST R-LIST-N @ cells + !
   R-LIST-N @ 1+ R-LIST-N ! ;

\ The first block reached from outside the region is where it leaves; a second
\ such block that is not the first one means the two arms never meet.
: R-JOIN-OK? ( n -- bool )
   {: b:n :}
   R-JOIN @ 0 < if b R-JOIN ! true exit then
   R-JOIN @ b = ;

\ One popped candidate, classified. It is either where the region leaves - a
\ block something outside reaches too - or one more block of the region, and a
\ block of the region has to be admissible and pushes its own successors on.
: R-CLASSIFY ( IR-ID:ir-fun-id n n -- bool )
   {: f:IR-ID:ir-fun-id p:n b:n :}
   b p <= if false exit then
   b R-MARK? if false exit then
   b R-PRED@ 1 <> if b R-JOIN-OK? exit then
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk MEMBER-OK? 0= if false exit then
   b R-TAKE
   R-SPEC @  bk SPEC-DEFS +  R-SPEC !
   R-SPEC-D @  bk SPEC-DEFS-D +  R-SPEC-D !
   bk TERM-OP {: t:IR-ID:ir-op-id :}
   t SUCCS-OF 0 ?do
      b  t i SUCC-IDX  R-PUSH
   loop
   true ;

: R-GROW ( IR-ID:ir-fun-id -- bool )
   {: f:IR-ID:ir-fun-id :}
   true
   begin
      dup  R-QI @ R-QN @ <  and
   while
      drop
      R-QI @ {: k:n :}
      k 1+ R-QI !
      f  k cells R-QP + @  k cells R-QB + @  R-CLASSIFY
   repeat ;

\ ---- what a grown region still has to satisfy --------------------------------
\ The exit's arguments are the values the arms hand over, so their number is the
\ width of the selection and their types say which FILE each chosen value lives
\ in. Both are recorded here and nothing is refused for its type: a double is
\ chosen between by an Fcsel exactly as a cell is chosen between by a Csel, so
\ what a double changes is which pool the arms are held against and not whether
\ the region converts. The pool question below is where that lands.
\
\ A POSITION IS ONE OF THREE THINGS AND THE COUNT SAYS SO. It is a double, and
\ then it costs the floating file; it is the memory order, which holds no
\ register at all; or it is a cell. The order is counted with the cells rather
\ than given a third number, because counting a value that needs no register can
\ only refuse a region that would have fitted, and the alternative - a third
\ count - would be a bound the emission has no instruction to spend.
: R-WIDTH-OK? ( IR-ID:ir-fun-id -- bool )
   {: f:IR-ID:ir-fun-id :}
   f R-JOIN @ BLOCK-AT {: jb:IR-ID:ir-block-id :}
   jb ARG-COUNT {: w:n :}
   w SEL-WIDTH-MAX > if false exit then
   w R-WIDTH !
   0
   w 0 ?do
      jb i ARG-AT REAL? if 1+ then
   loop
   R-WIDTH-D !
   true ;

\ Every two-way branch left inside the region has to land on blocks the region
\ owns, because a two-way branch hands nothing over and the exit takes the
\ arms' values as arguments. It is vacuous for a region whose exit takes none,
\ and that is the only shape where a branch may reach the exit directly.
: R-EDGE-OK? ( IR-ID:ir-fun-id n -- bool )
   {: f:IR-ID:ir-fun-id b:n :}
   R-WIDTH @ 0= if true exit then
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk BRZ-TERM? 0= if true exit then
   bk TERM-OP {: t:IR-ID:ir-op-id :}
   true
   t SUCCS-OF 0 ?do
      t i SUCC-IDX R-MARK? 0= if drop false leave then
   loop ;

: R-EDGES-OK? ( IR-ID:ir-fun-id n -- bool )
   {: f:IR-ID:ir-fun-id h:n :}
   f h R-EDGE-OK? 0= if false exit then
   true
   R-LIST-N @ 0 ?do
      f  i cells R-LIST + @  R-EDGE-OK? 0= if drop false leave then
   loop ;

\ The widest select the region would emit, held against the pool - once per
\ file, because the two floors are counts of two different sets of registers. A
\ region that hands its exit nothing selects nothing, so it needs neither.
: R-NEED-G ( IR-ID:ir-fun-id n -- n )
   {: f:IR-ID:ir-fun-id h:n :}
   f h BLOCK-AT SEL-NEED-G
   R-LIST-N @ 0 ?do
      f  i cells R-LIST + @  BLOCK-AT SEL-NEED-G  max
   loop ;

: R-NEED-F ( IR-ID:ir-fun-id n -- n )
   {: f:IR-ID:ir-fun-id h:n :}
   f h BLOCK-AT SEL-NEED-F
   R-LIST-N @ 0 ?do
      f  i cells R-LIST + @  BLOCK-AT SEL-NEED-F  max
   loop ;

\ THE OTHER HALF OF THE POOL QUESTION IS PRESSURE, AND IT IS THE ONE THAT
\ DECIDES REAL BODIES. A converted region computes every arm's values whether
\ that arm would have run or not, and each of those values is live from where it
\ is computed to the select that reads it - so they are all live at once, on top
\ of the values the join is handed. A region that makes more of them than the
\ routine has registers does not become slower code: it becomes a routine the
\ allocator refuses, which would turn this optimisation into a compilation
\ failure. So the count is held against the pool, and a region that does not fit
\ stays branched.
\
\ WHAT THE COUNT DOES NOT INCLUDE, said plainly. Whatever else the routine has
\ live where the branch stood is not counted here, because this pass has no
\ liveness of its own and building one would be a second derivation of what the
\ allocator already computes. The bound is therefore a floor rather than a
\ proof, and it is the floor that was measured: with it the four comparison
\ corpora, the maki suite and the chain's own suites all allocate, and without
\ it the eight-deep early-exit ladder of tools/codegen-compare-corpus4.f does
\ not. A routine that still runs out is refused by the allocator by name, which
\ is the same refusal any too-tight pool has always given.
\
\ AND IT IS TWO PRESSURES FOR THE REASON IT IS TWO FLOORS: a speculated double
\ and a speculated cell do not compete for the same register. The two counts
\ partition the same set of values - every speculated operation is counted in
\ exactly one of them, and every position of the join in exactly one of them -
\ so a region that fits both pools has each file's live set inside that file.
: R-PRESSURE-OK? ( -- bool )
   GPR-POOL-N
   R-SPEC @ R-SPEC-D @ -  R-WIDTH @ R-WIDTH-D @ -  +  >= 0= if false exit then
   FPR-POOL-N  R-SPEC-D @ R-WIDTH-D @ +  >= ;

: R-POOL-OK? ( IR-ID:ir-fun-id n -- bool )
   {: f:IR-ID:ir-fun-id h:n :}
   R-WIDTH @ 0= if true exit then
   GPR-POOL-N  f h R-NEED-G  < if false exit then
   FPR-POOL-N  f h R-NEED-F  < if false exit then
   R-PRESSURE-OK? ;

: R-BOUNDS-OK? ( -- bool )
   R-LIST-N @ 0= if false exit then
   R-LIST-N @ SEL-BLOCK-MAX > if false exit then
   R-SPEC @ SEL-DEFS-MAX > if false exit then
   R-JOIN @ 0 >= ;

: R-COMMIT ( n -- )
   {: h:n :}
   R-LIST-N @ 0 ?do
      i cells R-LIST + @ {: b:n :}
      1 b BLOCK-ORD-CK cells R-ABSORB + !
      h b BLOCK-ORD-CK cells R-OWNER + !
   loop
   1 h BLOCK-ORD-CK cells R-HEAD + !
   R-JOIN @ h BLOCK-ORD-CK cells R-EXIT + ! ;

\ One candidate head, tried whole. Nothing is written outside this pass's own
\ scratch rows until every question has been answered, so a region that fails
\ half way leaves the plan exactly as it found it.
: R-TRY ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id h:n :}
   f h BLOCK-AT {: hb:IR-ID:ir-block-id :}
   hb BRZ-TERM? 0= if exit then
   hb TERM-OP {: t:IR-ID:ir-op-id :}
   t 0 SUCC-IDX {: s0:n :}
   t 1 SUCC-IDX {: s1:n :}
   s0 s1 = if exit then
   R-TRY-RESET
   h s0 R-PUSH
   h s1 R-PUSH
   f R-GROW 0= if exit then
   R-BOUNDS-OK? 0= if exit then
   f R-WIDTH-OK? 0= if exit then
   f h R-EDGES-OK? 0= if exit then
   f h R-POOL-OK? 0= if exit then
   h R-COMMIT ;

\ ---- the plan for one function -----------------------------------------------
\ Every block in order gets one try, and a block already inside a region is
\ never a head of its own: the region that took it will emit it. The machine
\ ordinals are handed out afterwards, in source order, skipping what was
\ absorbed - so the block order the rest of the chain reads is still the source
\ module's order with the absorbed blocks taken out of it.
: R-NUMBER ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   0 R-NEXT !
   f BLOCK-COUNT 0 ?do
      i R-ABSORB? 0= if
         R-NEXT @ i BLOCK-ORD-CK cells R-ORD + !
         R-NEXT @ 1+ R-NEXT !
      then
   loop ;

\ Where this function's blocks start in the module, and that they run on from
\ there without a gap. The plan's rows and every reader above are indexed by a
\ block's ordinal WITHIN its function, and a successor carries its ordinal
\ within the MODULE, so the two are one subtraction apart exactly while the
\ function's blocks are contiguous. That is how IR-BUILD mints them, and it is
\ proved here rather than assumed because everything downstream of the
\ subtraction would be reading someone else's block if it were not so.
: R-BASE! ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f 0 BLOCK-AT IR-ID:BLOCK-LOCAL R-BASE !
   f BLOCK-COUNT 0 ?do
      f i BLOCK-AT IR-ID:BLOCK-LOCAL  R-BASE @ -  i <>
      if E-A64SEL-SHAPE throw then
   loop ;

: PLAN-REGIONS ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   R-RESET
   f R-BASE!
   f PRED-SCAN
   f BLOCK-COUNT 0 ?do
      i R-ABSORB? 0= if f i R-TRY then
   loop
   f R-NUMBER ;

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
\ AND THE SAME WIRING CARRIES THE NaN RULE THROUGH A FUSED FLOAT BRANCH, which is
\ the one thing this leaf had to get right. Work it through for `f<`, whose
\ condition is `mi`:
\
\   x y f<   answers 0 when x is a NaN (measured; survey (4))
\   so the INTERPRETED `if` takes hir.brz succ 0 - the arm the relation did not
\      choose, which is the source's `else`
\   the FUSED branch is Fcmp x,y under `mi`. A NaN raises the unordered
\      condition, N is clear, `mi` does not hold
\   so control goes to a64.fcmpbr succ 1, which this wiring set to brz succ 0
\   - the same arm the interpreted word takes
\
\ The step that does the work is the second: `mi` is false on unordered. It holds
\ identically for `f>` under `gt` and `f=` under `equal`, and for the two zero
\ comparisons under the same two conditions, because those three conditions are
\ exactly the ones that are false when the unordered flag is set - which is why
\ the engine chose them and why src/compiler/native/a64ir.f names them. Under
\ `lt`, which is what a table that read the condition off the relation's NAME
\ would have chosen for `f<`, the unordered flag makes the condition HOLD, and
\ the fused branch would take the arm the interpreted word does not. Nothing
\ about the successor wiring changes between the two, which is why the wiring is
\ not where the NaN rule lives.
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
\ The operands are the comparison's, in the comparison's order: every fused form
\ compares its first operand against its second exactly as the matching flag form
\ does, so `a b <` fuses to a compare of a against b under `lt`, `a b f<` to a
\ compare of a against b under `mi`, and a swapped pair would be a wrong program
\ rather than a different spelling. That matters most for the pair whose relation
\ is not symmetric under a swap: turning the operands of `f<` round computes `f>`
\ and answers the other arm for every pair of distinct numbers.
\
\ WHICH MACHINE FORM IT IS comes off the comparison's kind, exactly as the flag
\ form's does, and so does how many operands to carry over: the two comparisons
\ against zero carry one, because the instruction's second operand is the
\ immediate zero the form itself holds.
\
\ The span is the BRANCH's. The operation is the block's terminator - what it
\ is, is a two-way branch - and the span every reader of a terminator expects is
\ the control word the programmer wrote.
: EMIT-CMPBR ( IR-ID:ir-op-id IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id cm:IR-ID:ir-op-id :}
   cm OP-KIND {: k:A64SEL:cmpkind :}
   id k KIND-FUSED-OPCODE OPEN
   k KIND-OPERANDS 0 ?do
      CTX BLD  cm i OPERAND  IR-BUILD:ADD-OPERAND
   loop
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

\ WHICH OPERATION THE ROUTINE LEAVES THROUGH, read off the module. It is the
\ operation immediately in front of the terminator of a block whose terminator is
\ the return - which, for a body the elaborator called a tail call, is the only
\ block there is. Asked of a position, because both the placement below and the
\ lowering above have one; and asked of the CONTRACT first, so a routine that was
\ not declared a tail caller never grows one.
: TAIL-AT? ( IR-ID:ir-block-id n -- bool )
   {: bk:IR-ID:ir-block-id at:n :}
   TAIL? 0= if false exit then
   bk OP-COUNT {: n:n :}
   n 2 < if false exit then
   at n 2 - <> if false exit then
   bk at OP-AT OP-SLOT O-WORDCALL <> if false exit then
   bk n 1- OP-AT OP-SLOT O-RETURN = ;

\ Does the block being lowered leave through a callee? The walk hands each
\ operation down on its own, so the position is found by asking the block.
: TAIL-HERE? ( -- bool )
   TAIL? 0= if false exit then
   BLK OP-COUNT {: n:n :}
   n 2 < if false exit then
   BLK n 2 - TAIL-AT? ;

: TAIL-OP? ( IR-ID:ir-op-id -- bool )
   {: id:IR-ID:ir-op-id :}
   TAIL-HERE? 0= if false exit then
   BLK  BLK OP-COUNT 2 -  OP-AT IR-ID:OP-LOCAL  id IR-ID:OP-LOCAL = ;

\ ---- leaving through the callee ----------------------------------------------
\ WHAT A TAIL CALL IS, AND WHY IT IS NOT A CALL SITE WITH THE END CUT OFF. A call
\ site is the routine's exit sequence, the branch, and the routine's entry
\ sequence, arranged so that the pointer is where it started when control comes
\ back. Control does not come back here. So the site is the store run and the
\ branch, and there is nothing to take back, nothing to load, no publication and
\ no return - the CALLEE's own exit publishes the results, into the very cells
\ this routine's caller will read them out of, and the callee's own return goes
\ to the address x30 holds, which is this routine's caller's.
\
\ THE FOUR THINGS THAT MAKE THAT TRUE ARE HELD HERE, NOT ASSUMED. The elaborator
\ decided this routine leaves through its last call and the contract says so;
\ this is the second derivation, made from the module, and it refuses by name
\ rather than lowering a branch that would hand the caller a stack it does not
\ expect:
\   nothing is live across the branch, because nothing follows it;
\   the callee takes exactly what this routine takes and leaves exactly what it
\   leaves, so the cells are the same cells at both ends;
\   and the pointer already stands at the callee's entry base, so the site has no
\   adjustment - which is what DPLACE below makes true rather than hopes for.
: TAIL-CK ( n n n n -- )
   {: a:n r:n kk:n m:n :}
   kk 0<> if E-A64SEL-TAIL throw then
   m 0<> if E-A64SEL-TAIL throw then
   a ARGS SLOT-POSITIONS <> if E-A64SEL-TAIL throw then
   r OUTS SLOT-POSITIONS <> if E-A64SEL-TAIL throw then
   a A64IR:SLOT-WIDTH * DPLACED 0<> if E-A64SEL-TAIL throw then
   r A64IR:SLOT-WIDTH * DPLACED 0<> if E-A64SEL-TAIL throw then ;

: EMIT-TAIL-BR ( IR-ID:ir-op-id n -- )
   {: at:IR-ID:ir-op-id entry:n :}
   at A64IR-OPCODE:TAILCALL OPEN
   TOK OPERAND+
   CTX BLD  CTX BLD A64IR:KEY-ENTRY  CTX BLD entry A64IR:ENTRY-ATTR
   IR-BUILD:ADD-ATTR
   CTX BLD IR-BUILD:END-OP drop ;

\ The epilogue stands in FRONT of the branch and not after it, which is the whole
\ of what a routine that also calls has to do differently: the frame and the
\ saved return address are given back while this routine is still running, and
\ then it leaves. A routine whose only call is this one has no epilogue at all -
\ CALLS? is false, so PROLOGUE and EPILOGUE build nothing - and that is the win.
: EMIT-TAIL-CALL ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id WORD-SHAPE {: a:n r:n kk:n m:n :}
   a r kk m TAIL-CK
   id 0 OPERAND TOK!
   id kk m a CALL-SAVE
   id EPILOGUE
   id  id WORD-ENTRY  EMIT-TAIL-BR
   N-TAILS @ 1+ N-TAILS ! ;


\ The two arms the tail decision changes, and it changes nothing else: a word
\ call that is the operation the routine leaves through becomes the branch, and
\ the return it stood in front of becomes nothing at all, because the branch is
\ already the block's terminator.
: EMIT-CALL-OR-TAIL ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id TAIL-OP? if id EMIT-TAIL-CALL exit then
   id EMIT-WORD-CALL ;

: EMIT-RETURN-OR-TAILED ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   TAIL-HERE? if exit then
   id EMIT-RETURN ;

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
      wordcall OF id EMIT-CALL-OR-TAIL ENDOF
      return OF id EMIT-RETURN-OR-TAILED ENDOF
      fconst   OF id EMIT-FCONST ENDOF
      fadd     OF id A64IR-OPCODE:FADD EMIT-FBINARY ENDOF
      fsub     OF id A64IR-OPCODE:FSUB EMIT-FBINARY ENDOF
      fmul     OF id A64IR-OPCODE:FMUL EMIT-FBINARY ENDOF
      fdiv     OF id A64IR-OPCODE:FDIV EMIT-FBINARY ENDOF
      fneg     OF id A64IR-OPCODE:FNEG EMIT-FUNARY ENDOF
      fabs     OF id A64IR-OPCODE:FABS EMIT-FUNARY ENDOF
      fsqrt    OF id A64IR-OPCODE:FSQRT EMIT-FUNARY ENDOF
      flt      OF id EMIT-FLAG ENDOF
      fgt      OF id EMIT-FLAG ENDOF
      feq      OF id EMIT-FLAG ENDOF
      fltz     OF id EMIT-FLAG ENDOF
      feqz     OF id EMIT-FLAG ENDOF
      intreal  OF id A64IR-OPCODE:SCVTF EMIT-FUNARY ENDOF
      realint  OF id A64IR-OPCODE:FCVTZS EMIT-UNARY ENDOF
      bitsreal OF id A64IR-OPCODE:FMOVXD EMIT-FUNARY ENDOF
      realbits OF id A64IR-OPCODE:FMOVDX EMIT-UNARY ENDOF
   ;MATCH ;

\ ---- which slot holds which value, over the whole routine ---------------------
\ THE FACT. A slot of the caller's data stack holds a value of the source module,
\ and this pass says which one at the head and at the end of every block. It is a
\ MUST fact: a slot holds a value only when it holds that same value however
\ control arrived, so the answer starts at "nothing has been said" everywhere but
\ the entry and descends until it stops moving.
\
\ THE ONE STEP THAT MAKES A LOOP WORK IS THE TRANSLATION ACROSS AN EDGE. A
\ predecessor's map names the predecessor's own values, and the block it goes on
\ to names its own arguments; so a value the branch hands to argument `i` is READ
\ as that argument on the way in. Without it the walker's loop - whose value
\ comes out of the entry on one edge and out of the call on the other - would
\ meet two different names for one cell and conclude that the cell holds nothing.
\ With it both edges say "argument zero", which is the true statement: whichever
\ way control arrived, slot zero holds what argument zero is.
\
\ AND IT IS EXACT RATHER THAN MERELY SOUND WHERE TWO ARMS DISAGREE. If one arm
\ hands over `x` and the other `y`, both translate to the argument, and the
\ argument IS whichever of the two the run took - which is the same thing the
\ cell holds. Nothing is claimed that is not true on every path.
\
\ WHAT A BRANCH TO A ROUTINE DESTROYS. Every slot at or above the callee's own
\ base, which is the caller's slot `kk`: the callee takes its arguments out of
\ the caller's cells, leaves its results in them, and saves whatever it holds
\ across its OWN calls below its own argument base - which is at or above `kk`
\ and never under it. That is the same statement the caller-save discipline
\ above already rests on, read the other way round. The run then names all of
\ `kk + r` from the operation's results, so the survivors and the answers are one
\ statement.
\
\ AND WHAT AN ADDRESSED STORE DESTROYS: everything. A value the program computed
\ may be the address of a data-stack cell - which is exactly what the dialect
\ declares when it puts a64.astr in the same space and the same chain as the
\ data-stack forms - so a store through one says nothing about which cell it
\ reached and every slot stops holding anything this pass can name.
: DOUT-AT ( n n -- n )
   {: b:n s:n :}
   s DIN-WINDOW? 0= if DNONE exit then
   b BLOCK-ORD-CK DSLOT-MAX * s + cells D-OUT + @ ;

: DOUT-AT! ( n n n -- )
   {: v:n b:n s:n :}
   s DIN-WINDOW? 0= if exit then
   v  b BLOCK-ORD-CK DSLOT-MAX * s + cells D-OUT + ! ;

: DCUR<IN ( n -- )
   {: b:n :}
   DSLOT-MAX 0 ?do  b i DIN-AT  i cells D-CUR + !  loop ;

: DOUT<CUR ( n -- )
   {: b:n :}
   DSLOT-MAX 0 ?do  i cells D-CUR + @  b i DOUT-AT!  loop ;

\ ---- the effect of one source operation on the map ---------------------------
\ Every walk over a block's operations goes through this one word - the fixpoint
\ that computes the map, the pass that reads the register need off it, and the
\ lowering itself, which calls the same two halves either side of the branch it
\ builds. So there is one statement of what an operation does to the caller's
\ stack, and the answer a lowering acts on is the answer the fixpoint reached.
: DCALL-XFER ( IR-ID:ir-op-id n n n n -- n )
   {: id:IR-ID:ir-op-id a:n r:n kk:n m:n :}
   id kk m a DSAVE-XFER {: mask:n :}
   id kk m r DBACK-XFER
   mask ;

: DOP-XFER ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id OP-SLOT {: s:n :}
   s O-CALL = if id  id SELF-SHAPE  DCALL-XFER exit then
   s O-WORDCALL = if id  id WORD-SHAPE  DCALL-XFER exit then
   s O-RETURN = if
      DSTACK? 0= if 0 exit then
      id  OUTS SLOT-POSITIONS  DEXIT-XFER exit
   then
   s O-STORE = if DKILL 0 exit then
   s O-BSTORE = if DKILL 0 exit then
   0 ;

: DXFER-BLOCK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   b DCUR<IN
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT 0 ?do  bk i OP-AT DOP-XFER drop  loop
   b DOUT<CUR ;

\ ---- the meet, and the translation that makes it exact -----------------------
here CELL 1- and CELL swap - CELL 1- and allot
create D-MEET DSLOT-MAX cells allot

: DMEET1 ( n n -- n )
   {: a:n b:n :}
   a DANY = if b exit then
   b DANY = if a exit then
   a b = if a exit then
   DNONE ;

\ WHY A NAME MAY NOT CROSS A BACKWARD EDGE UNCHANGED, which is the one step of
\ this pass that is not obvious and the one a wrong answer comes out of. A value
\ is written once, but a value written INSIDE a loop is written once per turn,
\ and "slot s holds v" carried round the back edge would be a claim about the
\ PREVIOUS turn's v read as a claim about this turn's. `begin dup 0 > while 1-
\ repeat` is the smallest example: the cell holds the loop's argument at the
\ bottom of the body, and the branch hands the DECREMENT to that same argument -
\ so carrying the name unchanged says the cell holds the argument when it holds
\ the number the argument had one turn ago, and the routine answers what it was
\ given instead of zero.
\
\ SO ONLY A TRANSLATED NAME CROSSES A BACKWARD EDGE. Translation is exactly the
\ statement that survives: the cell held the value the branch hands to argument
\ `i`, and argument `i` IS that value at the other end, this turn and every turn.
\ Every cycle in any block order contains at least one edge whose destination
\ does not come after its source, so killing the untranslated names there is
\ enough to stop a name from ever going round a loop; on an edge that goes
\ forward nothing runs between the two ends, so a name crosses unchanged.
: DXLATE ( IR-ID:ir-op-id IR-ID:ir-block-id bool n -- n )
   {: t:IR-ID:ir-op-id tb:IR-ID:ir-block-id back:bool v:n :}
   v 0 < if v exit then
   tb ARG-COUNT {: k:n :}
   DANY
   t OP-SLOT O-BR = if
      t OPERANDS-OF k = if
         k 0 ?do
            t i OPERAND-AT VSLOT v = if
               drop  tb i ARG-AT VSLOT  leave
            then
         loop
      then
   then
   dup DANY <> if exit then
   drop
   back if DNONE exit then
   v ;

: DMEET-EDGE ( IR-ID:ir-op-id IR-ID:ir-block-id n n -- )
   {: t:IR-ID:ir-op-id tb:IR-ID:ir-block-id p:n b:n :}
   b p <= {: back:bool :}
   DSLOT-MAX 0 ?do
      i cells D-MEET + @
      t tb  back  p i DOUT-AT  DXLATE
      DMEET1
      i cells D-MEET + !
   loop ;

: DEDGE? ( IR-ID:ir-op-id n -- bool )
   {: t:IR-ID:ir-op-id b:n :}
   false
   t SUCCS-OF 0 ?do
      t i SUCC-IDX b = if drop true leave then
   loop ;

: DMEET-FROM ( IR-ID:ir-fun-id n n -- )
   {: f:IR-ID:ir-fun-id p:n b:n :}
   f p BLOCK-AT TERM-OP {: t:IR-ID:ir-op-id :}
   t b DEDGE? 0= if exit then
   t  f b BLOCK-AT  p b DMEET-EDGE ;

: DIN-SET? ( n n n -- bool )
   {: v:n b:n s:n :}
   b s DIN-AT v = if false exit then
   v b s DIN-AT!
   true ;

: DMEET-BLOCK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   DSLOT-MAX 0 ?do DANY i cells D-MEET + ! loop
   f BLOCK-COUNT 0 ?do  f i b DMEET-FROM  loop
   DSLOT-MAX 0 ?do
      i cells D-MEET + @  b i DIN-SET? if 1 D-MOVED ! then
   loop ;

\ ---- the entry map, and the fixpoint over the rest ---------------------------
\ The routine's own arguments are the seed, and they are the only thing in the
\ map that no operation of the module put there: the CALLER wrote those cells,
\ the pointer move at the head of the entry block is what makes them slots zero
\ upwards, and the convention says which slot each argument arrived in.
: DIN-ANY ( n -- )
   {: b:n :}
   DSLOT-MAX 0 ?do DANY b i DIN-AT! loop ;

: DENTRY-IN ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   DSLOT-MAX 0 ?do DNONE 0 i DIN-AT! loop
   DSTACK? 0= if exit then
   f 0 BLOCK-AT {: bk:IR-ID:ir-block-id :}
   ARGS SLOT-POSITIONS {: a:n :}
   bk ARG-COUNT a <> if E-A64SEL-PLACE throw then
   a 0 ?do
      bk i ARG-AT VSLOT  0  ARGS i A64EFF:SEQ-SLOT@  DIN-AT!
   loop ;

: DIN-INIT ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f BLOCK-COUNT 1 ?do i DIN-ANY loop
   f DENTRY-IN ;

: DRES-ROUND ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f BLOCK-COUNT 1 ?do  f i DMEET-BLOCK  loop
   f BLOCK-COUNT 0 ?do  f i DXFER-BLOCK  loop ;

\ How many rounds the descent can still be moving after. Every cell of the map
\ starts at "nothing said", may name one value, and may then fall to "nothing
\ this pass can name" - two steps, and a round that moves nothing ends the walk -
\ so a round past twice the map's own size is a round that cannot be reached.
\ Reaching it is a fixpoint that is not descending, which is a bug in a transfer
\ rather than a routine this pass should compile.
NFROZEN:BMAX DSLOT-MAX * 2 * 2 + constant DRES-ROUNDS

: DRES-FIX ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f DIN-INIT
   f BLOCK-COUNT 0 ?do f i DXFER-BLOCK loop
   0
   begin
      1 D-MOVED !
      dup DRES-ROUNDS >= if E-A64SEL-CAP throw then
      0 D-MOVED !
      f DRES-ROUND
      1+
      D-MOVED @ 0=
   until
   drop ;

\ ---- which values reach a register -------------------------------------------
\ A value needs a register when some operation this pass really emits reads it
\ out of one. Three positions do not: a store the map says is already true, an
\ argument of a block that carries nothing to a register, and the memory order,
\ which holds no register at all and is therefore always carried. The first two
\ are what makes the answer a fixpoint - whether an edge's operand is read
\ depends on whether the argument it feeds is built, which depends on what reads
\ THAT - and the third is why a token answers yes without being asked.
\
\ NOTHING HERE DECIDES SAFETY, WHICH IS WORTH SAYING PLAINLY. A value this pass
\ wrongly calls unneeded is a value nothing binds in the value map, so the
\ operation that reads it is refused by name at the moment it is selected. The
\ answer is an optimisation and its failure mode is a refusal, never a register
\ holding something else.
: DNEED-CLEAR ( -- )
   VMAX 0 ?do 0 i cells D-NEED + ! loop ;

\ The routine's own interface is not this pass's to change. Under a register
\ convention the entry block's arguments ARE the declared argument registers and
\ every one of them is built whether the body reads it or not; under the
\ data-stack convention they are cells the caller wrote, and the entry load is
\ exactly what this pass may drop.
: DNEED-ENTRY ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   DSTACK? if exit then
   f 0 BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk ARG-COUNT 0 ?do bk i ARG-AT DNEED+ loop ;

: DNEED-OPERANDS ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id OPERANDS-OF 0 ?do id i OPERAND-AT DNEED+ loop ;

: DNEED-CALL ( IR-ID:ir-op-id n n n n n -- )
   {: id:IR-ID:ir-op-id mask:n a:n r:n kk:n m:n :}
   id 0 OPERAND-AT DNEED+
   kk a + 0 ?do
      mask i DBIT? 0= if id kk m i DSAVE-VAL DNEED+ then
   loop
   m 0 ?do id kk i + 1+ OPERAND-AT DNEED+ loop ;

: DNEED-EXIT ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id mask:n :}
   DSTACK? 0= if id DNEED-OPERANDS exit then
   OUTS SLOT-POSITIONS 0 ?do
      mask i DBIT? 0= if id i OPERAND-AT DNEED+ then
   loop ;

: DNEED-OP ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id DOP-XFER {: mask:n :}
   id OP-SLOT {: s:n :}
   s O-BR = if exit then
   s O-CALL = if id mask  id SELF-SHAPE  DNEED-CALL exit then
   s O-WORDCALL = if id mask  id WORD-SHAPE  DNEED-CALL exit then
   s O-RETURN = if id mask DNEED-EXIT exit then
   id DNEED-OPERANDS ;

: DNEED-BLOCK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   b DCUR<IN
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT 0 ?do  bk i OP-AT DNEED-OP  loop ;

: DNEED-EDGE-OF ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT TERM-OP {: t:IR-ID:ir-op-id :}
   t OP-SLOT O-BR <> if exit then
   t BR-TARGET {: tb:n :}
   FUN tb BLOCK-AT ARG-COUNT {: k:n :}
   t OPERANDS-OF k <> if exit then
   k 0 ?do
      tb i DDROP? 0= if t i OPERAND-AT DNEED+ then
   loop ;

: DNEED-EDGES ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f BLOCK-COUNT 0 ?do f i DNEED-EDGE-OF loop ;

\ The closure only ever ADDS to the answer, and there are as many values as the
\ map holds, so it stops.
: DNEED-FIX ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   DNEED-CLEAR
   f DNEED-ENTRY
   f BLOCK-COUNT 0 ?do f i DNEED-BLOCK loop
   begin
      0 D-MOVED !
      f DNEED-EDGES
      D-MOVED @ 0=
   until ;

: DRESIDENCY ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f DRES-FIX
   f DNEED-FIX ;

\ ---- choosing where the pointer stands ---------------------------------------
\ The survey and the choice the section at the head of this file describes. It
\ runs over the SOURCE function, because every place the body requires is read
\ off the routine's contract and off the shape of each call operation - which
\ values a site hands over and takes back - and none of that depends on which
\ accesses the residency above turned out to drop. So the position is settled
\ before a single operation is lowered, which is what lets every builder write
\ its offset against it.
: DREQ+ ( n -- )
   {: at:n :}
   D-REQ-N @ DREQ-MAX >= if 1 D-REQ-OVER ! exit then
   at  D-REQ-N @ cells D-REQ + !
   D-REQ-N @ 1+ D-REQ-N ! ;

: DREQ-AT ( n -- n )
   cells D-REQ + @ ;

\ A call requires two places and they are the callee's, not this routine's: the
\ base it is entered at, which is one past everything the site hands it, and the
\ base it leaves at, which is one past everything it hands back. How many of the
\ live values the site keeps in registers is the last of the four numbers a shape
\ answers and is nothing to do with where the pointer stands, so it is dropped
\ rather than bound.
: DPLACE-CALL ( n n n n -- )
   drop
   {: a:n r:n kk:n :}
   kk a + A64IR:SLOT-WIDTH * DREQ+
   kk r + A64IR:SLOT-WIDTH * DREQ+ ;

\ A routine with two returns would publish twice and this survey would count one
\ place per publication, while every reader that measures the routine afterwards
\ - the allocation validator included - re-derives ONE block that control leaves
\ through. Refused here rather than surveyed, so the two cannot count differently.
: DPLACE-RETURN ( -- )
   D-RETS @ 1+ D-RETS !
   D-RETS @ 1 > if E-A64SEL-PLACE throw then
   OUTS SLOT-POSITIONS A64IR:SLOT-WIDTH * DREQ+ ;

: DPLACE-OP ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id OP-SLOT {: s:n :}
   s O-CALL = if id SELF-SHAPE DPLACE-CALL exit then
   s O-WORDCALL = if id WORD-SHAPE DPLACE-CALL exit then
   s O-RETURN = if DPLACE-RETURN then ;

: DPLACE-BLOCK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT 0 ?do  bk i OP-AT DPLACE-OP  loop ;

\ What standing at one place costs: one instruction for every place the routine
\ requires that is not this one. A place required twice is counted twice, because
\ two points each pay their own adjustment.
: DPLACE-COST ( n -- n )
   {: c:n :}
   0
   D-REQ-N @ 0 ?do
      i DREQ-AT c <> if 1+ then
   loop ;

\ Whether the body may stand here at all - the bound the section head derives,
\ and the reason no access has to be consulted.
: DPLACE-OK? ( n -- bool )
   {: c:n :}
   c 0 >=  c A64EFF:SLOT-BACK <=  and ;

\ A ROUTINE THAT LEAVES THROUGH A CALLEE HAS NO CHOICE, which is why the survey
\ above is not consulted for one. A tail branch is the whole of its site: there
\ is no instruction in front of it to move the pointer with, so the pointer has
\ to be standing at the callee's entry base already - and the rule that makes a
\ tail call possible at all is that the callee takes what this routine takes, so
\ that base is this routine's own argument base. Standing anywhere else would
\ make the branch impossible rather than expensive, so the place is settled
\ rather than chosen, and EMIT-TAIL-CALL holds every adjustment the site would
\ have carried against zero.
\
\ Better means fewer adjustments, and on a tie the lower place. The base is the
\ incumbent: it is where this pass stood before there was a choice, it is always
\ inside the bound, and it is the lowest place there is, so a tie with it keeps
\ it and the answer does not depend on the order the survey ran in.
: DPLACE-BETTER? ( n n -- bool )
   {: c:n k:n :}
   k D-COST @ < if true exit then
   k D-COST @ =  c D-POS @ <  and ;

: DPLACE-TRY ( n -- )
   {: c:n :}
   c DPLACE-OK? 0= if exit then
   c DPLACE-COST {: k:n :}
   c k DPLACE-BETTER? 0= if exit then
   c D-POS !
   k D-COST ! ;

: DPLACE-CHOOSE ( -- )
   0 D-POS !
   0 DPLACE-COST D-COST !
   D-REQ-N @ 0 ?do  i DREQ-AT DPLACE-TRY  loop ;

: DPLACE ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   0 D-POS !
   0 D-REQ-N !
   0 D-REQ-OVER !
   0 D-RETS !
   DSTACK? 0= if exit then
   ARGS SLOT-POSITIONS A64IR:SLOT-WIDTH * DREQ+
   f BLOCK-COUNT 0 ?do  f i DPLACE-BLOCK  loop
   TAIL? if ARGS SLOT-POSITIONS A64IR:SLOT-WIDTH * D-POS ! exit then
   D-REQ-OVER @ 0<> if exit then
   DPLACE-CHOOSE ;

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

\ One block argument, at the class the source argument's TYPE says: the memory
\ order holds no register, a double holds one of the D file, and everything else
\ holds one of the X file. Nothing here reads which block the argument belongs to
\ or which opcode handed a value to it - a class is a property of the value, and
\ the source module is the one authority on it, which is the same door TOKEN? and
\ REAL? are asked through everywhere else in this pass.
: OPEN-ARG1 ( IR-ID:ir-value-id -- )
   {: a:IR-ID:ir-value-id :}
   a TOKEN? if
      a  CTX BLD  CTX BLD A64IR:MEM-TYPE  IR-BUILD:ADD-BLOCK-ARG
      dup TOK!  VBIND
      exit
   then
   a REAL? if
      a  CTX BLD  CTX BLD A64IR:FPR-TYPE  IR-BUILD:ADD-BLOCK-ARG  VBIND
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
\ A block argument nothing reads out of a register is not built at all: the
\ values every edge hands it are not built either, so the copy each edge would
\ have made is gone with it. Nothing binds it in the value map, so an operation
\ that did read it would be refused by name rather than given some other value -
\ which is what makes the register-need answer above fail closed.
: OPEN-ARGS ( IR-ID:ir-block-id n -- )
   {: bk:IR-ID:ir-block-id ord:n :}
   bk ARG-COUNT {: n:n :}
   n 0 ?do
      ord i DDROP? 0= if bk i ARG-AT OPEN-ARG1 then
   loop ;

\ The same inputs under the data-stack convention: the block takes no argument at
\ all, because nothing arrives in a register. The pointer is moved down over the
\ caller's operands once, and each argument is then the value its own load
\ defines. The entry operations are anchored to the block's first source
\ operation, so they carry a span of the word they belong to.
\ An argument no operation reads out of a register is left in its slot: the
\ caller put it there, nothing here moves it, and the routine's own exit or the
\ call it is handed to finds it exactly where it already is. The load is the only
\ thing that goes; the pointer move is not this pass's to touch.
: OPEN-DARGS ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   ARGS SLOT-POSITIONS {: a:n :}
   bk ARG-COUNT a <> if E-A64SEL-PLACE throw then
   bk 0 OP-AT {: at:IR-ID:ir-op-id :}
   at PROLOGUE
   at  a A64IR:SLOT-WIDTH *  EMIT-DTAKE
   a 0 ?do
      bk i ARG-AT {: v:IR-ID:ir-value-id :}
      v DNEED? if
         v
         at  ARGS i A64EFF:SEQ-SLOT@ A64IR:SLOT-WIDTH *  EMIT-DLOAD
         VBIND
      then
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
   bk ord OPEN-ARGS ;


\ ---- emitting a selection as a select ----------------------------------------
\ The plan above says which blocks a region absorbed and where it leaves; this
\ is what the head block becomes. Three passes over the region, and the order
\ of them is the whole of the construction:
\
\   the OPERATIONS, head first and then every member in ordinal order. A
\     member's block arguments are not arguments any more - the edge that
\     carried them is gone - so they are bound to the very values its one
\     predecessor's branch was handing over, and every operation of the member
\     is then selected exactly as it would have been in its own block. Ordinal
\     order is a topological order because the plan admitted no edge that goes
\     backwards, so every value is defined before it is read.
\
\   the VALUES, in the opposite order. What a path hands the exit is read off
\     the region from the leaves up: a block that branches to the exit hands
\     over its own branch's operands, a block that branches to another member
\     hands over whatever that member hands over, and a block that still ends
\     in a two-way branch hands over the SELECT of its two successors' answers.
\     Reverse ordinal order is what makes each of those readable when it is
\     needed.
\
\   the BRANCH, once, from the head to the exit, carrying the head's answer.
\     Its successor is the exit's machine ordinal, and the exit is almost always
\     the block laid out next - so the emitter's fall-through rule usually
\     leaves no instruction for it at all.
\
\ WHAT IS NOT SELECTED, AND WHY THAT IS NOT AN EXCEPTION. A position where both
\ arms hand over the SAME value needs no instruction: there is nothing to
\ choose. The memory order is always such a position, because nothing in a
\ converted region touches memory - which is the admission rule doing the work
\ rather than a case written here - and an order that somehow differed would be
\ an order this pass has no way to choose between, so it is refused by name.
: RSEL-SLOT ( n n -- n )
   {: b:n i:n :}
   i 0 < i SEL-WIDTH-MAX >= or if E-A64SEL-CAP throw then
   b BLOCK-ORD-CK SEL-WIDTH-MAX * i + ;

: RSEL@ ( n n -- IR-ID:ir-value-id )
   RSEL-SLOT RSEL @ ;

: RSEL! ( IR-ID:ir-value-id n n -- )
   RSEL-SLOT RSEL ! ;

\ The two machine select SHAPES, in the files they may answer in and under the
\ instructions that may have written their flags. Each takes the answer the
\ source branch reaches when the tested value is NOT zero first, because that is
\ the arm a Habu `if` takes and because both Csel and Fcsel write their first
\ source when the condition holds.
\
\ THE FILE AND THE FLAGS-WRITER ARE PARAMETERS AND NOT SIX MORE WORDS, because
\ the forms of one shape differ in exactly one thing: which opcode is staged.
\ The operands, their order, the condition and the polarity are the same
\ statement, so writing them out per form would be six places for the polarity to
\ be got right in.
\
\ THE ZERO-TEST SHAPE HAS ONLY THE TWO FORMS IT ALWAYS HAD, and that is not an
\ omission. It is what a block whose branch fuses with NOTHING becomes, so what
\ it tests is a cell the program computed and what wrote the flags is the Cmp
\ against the immediate zero this form itself carries. A float comparison
\ against zero is not this shape at all: it is a comparison, so it goes through
\ the fused shape below under a64.fcmpselz or a64.fcmpselzd.
\
\ AND THE POLARITY CARRIES THE NaN RULE THROUGH A FUSED FLOAT SELECT, which is
\ the one thing this leaf has to get right and is the same argument the fused
\ float BRANCH makes with the successors instead of the sources. Work it through
\ for `x y f< if y else x then`, whose condition the table above gives as `mi`:
\
\   x y f<   answers 0 when either operand is a NaN (measured; survey (4))
\   so the INTERPRETED `if` takes hir.brz succ 0 - the source's `else`, which is
\      `x` - and that is the value R-S0 holds, which REGION-PICK passes as fv
\   the FUSED select is Fcmp x,y then a select under `mi`. A NaN raises the
\      unordered condition, N is clear, `mi` does not hold
\   a Csel and an Fcsel both write their SECOND source when the condition does
\      not hold, which is fv - the same value the interpreted word answers
\
\ The step that does the work is the third: `mi` is false on unordered.
\ It holds identically for `f>` under `gt`, `f=` under `equal` and the two zero
\ comparisons under `mi` and `equal`, because those three conditions are exactly
\ the ones that are false when the unordered flag is set. Under `lt`, which is
\ what a table that read the condition off the relation's NAME would give `f<`,
\ the unordered flag makes the condition HOLD and the select would write tv - the
\ arm the interpreted word does not take. Nothing about the source order changes
\ between the two, which is why the NaN rule lives in the condition table and not
\ here.
: SELZ-OPCODE ( bool -- A64IR:opcode )
   if A64IR-OPCODE:SELZD else A64IR-OPCODE:SELZ then ;

\ One row of the fused shape each: which file the answer lives in, under the
\ instruction that wrote the flags.
: GPR-SEL-OPCODE ( bool -- A64IR:opcode )
   if A64IR-OPCODE:CMPSELD else A64IR-OPCODE:CMPSEL then ;

: FREG-SEL-OPCODE ( bool -- A64IR:opcode )
   if A64IR-OPCODE:FCMPSELD else A64IR-OPCODE:FCMPSEL then ;

: FZERO-SEL-OPCODE ( bool -- A64IR:opcode )
   if A64IR-OPCODE:FCMPSELZD else A64IR-OPCODE:FCMPSELZ then ;

\ Which fused select one comparison becomes: its kind says which instruction
\ writes the flags and how many registers that instruction reads, and the join
\ argument's type says which file the answer lives in. Six of the eight forms
\ are here because the other two are the zero-test shape above.
: CMPSEL-OPCODE ( A64SEL:cmpkind bool -- A64IR:opcode )
   {: k:A64SEL:cmpkind d:bool :}
   k
   MATCH A64SEL:cmpkind
      none  OF E-A64SEL-OPCODE throw ENDOF
      gpr   OF d GPR-SEL-OPCODE ENDOF
      freg  OF d FREG-SEL-OPCODE ENDOF
      fzero OF d FZERO-SEL-OPCODE ENDOF
   ;MATCH ;

: SEL-RESULT+ ( bool -- )
   if FRESULT+ else RESULT+ then ;

: EMIT-SELZ ( IR-ID:ir-op-id IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id bool -- IR-ID:ir-value-id )
   {: at:IR-ID:ir-op-id v:IR-ID:ir-value-id
      tv:IR-ID:ir-value-id fv:IR-ID:ir-value-id d:bool :}
   at d SELZ-OPCODE OPEN
   CTX BLD v IR-BUILD:ADD-OPERAND
   CTX BLD tv IR-BUILD:ADD-OPERAND
   CTX BLD fv IR-BUILD:ADD-OPERAND
   d SEL-RESULT+
   CLOSE-VALUE
   ACC ;

\ HOW MANY OF THE COMPARISON'S OPERANDS THE SELECT CARRIES comes off its kind,
\ exactly as it does for the fused BRANCH: the two comparisons against zero carry
\ one, because the instruction's second operand is the immediate zero the form
\ itself holds. The chosen-between pair follows them, so a form's operand list is
\ the compared registers and then the two answers, in that order, whichever
\ corner of the square it is.
: EMIT-CMPSEL ( IR-ID:ir-op-id IR-ID:ir-op-id IR-ID:ir-value-id IR-ID:ir-value-id bool -- IR-ID:ir-value-id )
   {: at:IR-ID:ir-op-id cm:IR-ID:ir-op-id
      tv:IR-ID:ir-value-id fv:IR-ID:ir-value-id d:bool :}
   cm OP-KIND {: k:A64SEL:cmpkind :}
   at k d CMPSEL-OPCODE OPEN
   k KIND-OPERANDS 0 ?do
      CTX BLD  cm i OPERAND  IR-BUILD:ADD-OPERAND
   loop
   CTX BLD tv IR-BUILD:ADD-OPERAND
   CTX BLD fv IR-BUILD:ADD-OPERAND
   d SEL-RESULT+
   CTX BLD  CTX BLD A64IR:KEY-COND
   CTX BLD  cm COMPARE-COND  A64IR:COND-ATTR  IR-BUILD:ADD-ATTR
   CLOSE-VALUE
   ACC ;

\ One position of the join, as the block ending in this two-way branch leaves
\ it. The comparison, when the branch fuses with one, is read out of the block
\ this walk is standing in - the same place the fused branch reads it.
\
\ WHICH FILE THE SELECT ANSWERS IN IS THE JOIN ARGUMENT'S OWN TYPE, asked
\ through the same door every other class question in this pass goes through.
\ Not the type of the value being chosen: the two are the same type - the freeze
\ verifier matched the branch operands against the destination's arguments - and
\ the argument is the one this pass is about to build a machine argument for.
: REGION-PICK ( IR-ID:ir-op-id n n -- IR-ID:ir-value-id )
   {: t:IR-ID:ir-op-id fz:n i:n :}
   R-S1 @ i RSEL@ {: tv:IR-ID:ir-value-id :}
   R-S0 @ i RSEL@ {: fv:IR-ID:ir-value-id :}
   tv fv SAME-VALUE? if tv exit then
   0 R-JB @ i ARG-AT TOKEN? if E-A64SEL-SHAPE throw then
   0 R-JB @ i ARG-AT REAL? {: d:bool :}
   fz 0 < if  t  t 0 OPERAND  tv fv  d  EMIT-SELZ exit then
   t  BLK fz OP-AT  tv fv  d  EMIT-CMPSEL ;

\ ---- one block of the region ------------------------------------------------
\ A member's arguments, bound to what its one predecessor handed over. A
\ predecessor that ends in a two-way branch hands nothing over, which is that
\ form's own rule, so a block reached that way has to take no arguments.
: REGION-BIND-ARGS ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk ARG-COUNT {: k:n :}
   f b R-FROM@ BLOCK-AT TERM-OP {: t:IR-ID:ir-op-id :}
   t OP-SLOT O-BR <> if
      k 0<> if E-A64SEL-SHAPE throw then
      exit
   then
   t OPERANDS-OF k <> if E-A64SEL-SHAPE throw then
   k 0 ?do
      b i DDROP? 0= if
         bk i ARG-AT  t i OPERAND  VBIND
      then
   loop ;

\ Every operation of one block of the region but its terminator, and but the
\ comparison the select will make: the terminator is what the conversion
\ replaced, and a fused comparison selects to nothing here for exactly the
\ reason it selects to nothing under the fused branch - the select below stands
\ for both.
: REGION-OPS ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   bk FUSE-INDEX {: fz:n :}
   bk OP-COUNT 1- {: k:n :}
   k 0 ?do
      i fz <> if bk i OP-AT RULE then
   loop ;

: REGION-BLOCK-OPS ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk 0 S-BLK !
   bk REGION-OPS ;

: REGION-MEMBER? ( n n -- bool )
   {: h:n b:n :}
   b R-ABSORB? 0= if false exit then
   b R-OWNER@ h = ;

: REGION-MEMBERS-OPS ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id h:n :}
   f BLOCK-COUNT h 1+ ?do
      h i REGION-MEMBER? if
         f i REGION-BIND-ARGS
         f i REGION-BLOCK-OPS
      then
   loop ;

\ ---- what each block of the region hands the exit ---------------------------
\ A join position whose argument is not built carries nothing anywhere: no value
\ is read for it, no select is made for it and no copy crosses the one branch the
\ region keeps. Every walk over the join's positions asks this first, so the
\ region's three passes and its branch drop the same positions.
: RJOIN-DROP? ( n -- bool )
   R-EXIT-BK @ swap DDROP? ;

: REGION-VALUES-BR ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT TERM-OP {: t:IR-ID:ir-op-id :}
   t 0 SUCC-IDX {: sc:n :}
   sc R-EXIT-BK @ = if
      t OPERANDS-OF R-WIDTH @ <> if E-A64SEL-SHAPE throw then
      R-WIDTH @ 0 ?do
         i RJOIN-DROP? 0= if  t i OPERAND  b i RSEL!  then
      loop
      exit
   then
   R-WIDTH @ 0 ?do
      i RJOIN-DROP? 0= if  sc i RSEL@  b i RSEL!  then
   loop ;

: REGION-VALUES-BRZ ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk TERM-OP {: t:IR-ID:ir-op-id :}
   bk FUSE-INDEX {: fz:n :}
   t 0 SUCC-IDX R-S0 !
   t 1 SUCC-IDX R-S1 !
   R-WIDTH @ 0 ?do
      i RJOIN-DROP? 0= if  t fz i REGION-PICK  b i RSEL!  then
   loop ;

: REGION-BLOCK-VALUES ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk 0 S-BLK !
   bk BR-TERM? if f b REGION-VALUES-BR exit then
   f b REGION-VALUES-BRZ ;

: REGION-MEMBERS-VALUES ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id h:n :}
   f BLOCK-COUNT {: k:n :}
   k h 1+ ?do
      k h + i - {: b:n :}
      h b REGION-MEMBER? if f b REGION-BLOCK-VALUES then
   loop ;

\ ---- the one branch the region keeps ----------------------------------------
\ Each value the exit takes as an argument crosses the same way every other
\ argument-carrying edge in this pass crosses: copied into a value of its own,
\ so the argument's register class holds one live value. The memory order is
\ exempt for the reason EDGE-VALUE gives - it holds no register - and there is
\ nothing to copy an ordering with.
\
\ WHICH COPY, is the argument's own type, exactly as it is in EDGE-VALUE. A
\ double is copied with an Fmov of the D file and a cell with the general move,
\ and a copy made with the wrong one would move eight bytes out of a register
\ that does not hold them.
: REGION-BR ( IR-ID:ir-op-id n -- )
   {: t:IR-ID:ir-op-id h:n :}
   0
   R-WIDTH @ 0 ?do
      i RJOIN-DROP? 0= if
         0 R-JB @ i ARG-AT TOKEN? if
            h i RSEL@
         else
            t  h i RSEL@  0 R-JB @ i ARG-AT REAL?  EMIT-COPY
         then
         over EDGE-V !
         1+
      then
   loop
   {: n:n :}
   t A64IR-OPCODE:BR OPEN
   n 0 ?do
      CTX BLD  i EDGE-V @  IR-BUILD:ADD-OPERAND
   loop
   R-EXIT-BK @ SUCCESSOR-ORD+
   CTX BLD IR-BUILD:END-OP drop ;

: REGION-WIDTH! ( -- )
   0 R-JB @ ARG-COUNT {: w:n :}
   w SEL-WIDTH-MAX > if E-A64SEL-CAP throw then
   w R-WIDTH ! ;

: REGION-EMIT ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id h:n :}
   h R-EXIT@ R-EXIT-BK !
   f R-EXIT-BK @ BLOCK-AT 0 R-JB !
   REGION-WIDTH!
   f h REGION-BLOCK-OPS
   f h REGION-MEMBERS-OPS
   f h REGION-MEMBERS-VALUES
   f h REGION-BLOCK-VALUES
   f h BLOCK-AT TERM-OP  h  REGION-BR ;

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
: WALK-BLOCK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id ord:n :}
   ord R-ABSORB? if exit then
   ord DCUR<IN
   f ord BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk ord OPEN-BLOCK
   bk 0 S-BLK !
   ord R-HEAD? if
      f ord REGION-EMIT
      CTX BLD IR-BUILD:END-BLOCK drop
      exit
   then
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
   f PLAN-REGIONS
   f DRESIDENCY
   f DPLACE
   n 0 ?do
      f i WALK-BLOCK
   loop
   R-NEWBASE @ R-NEXT @ + R-NEWBASE !
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
   c b HIR-OPCODE:FCONST   BIND1
   c b HIR-OPCODE:FADD     BIND1
   c b HIR-OPCODE:FSUB     BIND1
   c b HIR-OPCODE:FMUL     BIND1
   c b HIR-OPCODE:FDIV     BIND1
   c b HIR-OPCODE:FNEG     BIND1
   c b HIR-OPCODE:FABS     BIND1
   c b HIR-OPCODE:FSQRT    BIND1
   c b HIR-OPCODE:INTREAL  BIND1
   c b HIR-OPCODE:REALINT  BIND1
   c b HIR-OPCODE:BITSREAL BIND1
   c b HIR-OPCODE:REALBITS BIND1
   c b HIR-OPCODE:FLT      BIND1
   c b HIR-OPCODE:FGT      BIND1
   c b HIR-OPCODE:FEQ      BIND1
   c b HIR-OPCODE:FLTZ     BIND1
   c b HIR-OPCODE:FEQZ     BIND1
   c b HIR:KEY-VALUE 0 BND-VAL !
   c b HIR:KEY-ENTRY 0 BND-ENTRY !
   c b HIR:KEY-IN    0 BND-IN !
   c b HIR:KEY-OUT   0 BND-OUT !
   c b HIR:MEM-TYPE 0 BND-MEM !
   c b HIR:REAL-TYPE 0 BND-REAL !
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
   gi gr gc fi fr fc z l ct t size delta A64EFF-ROUTINE:MAKE
   A64EFF:GPR-WRITABLE 0 S-POOL !
   gi gr gc fi fr fc z l ct t size delta A64EFF-ROUTINE:MAKE
   A64EFF:FPR-WRITABLE 0 S-FPOOL !
   t 0 S-TRT !
   size S-FRAME !
   ct A64EFF-CONTROL:TAIL-CALL A64EFF-CONTROL:EQ if 1 else 0 then S-TAIL !
   0 N-CALLS !
   0 N-TAILS !
   0 R-NEWBASE !
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
   TAILED-CK
   c b IR-BUILD:FREEZE ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;using
;package
