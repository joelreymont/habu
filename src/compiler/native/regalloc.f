\ regalloc.f - give every virtual register of one machine routine a real ARM64
\ general register, by linear scan over the routine's blocks.
\
\ docs/compiler-ir-design.md section 7.9 ("start with linear scan") over the
\ dialect src/compiler/native/a64ir.f defines and src/compiler/native/select.f
\ produces. Everything before this pass names values; everything after it names
\ registers and bytes. This file owns exactly one step of that: which physical
\ register holds which value. It rewrites no module, chooses no block order and
\ encodes nothing.
\
\ ONE PATH, AND WHY IT USED TO BE TWO. A routine of ONE block has one operation
\ order, so a position could be an index into it and two values could be said to
\ interfere exactly when their definition-to-last-use stretches overlap. That was
\ once a second allocator in this file, kept because the general rule could not
\ yet anchor a spill decision to a block. It can: a plan row carries its block,
\ so the general rule below covers a routine of one block as the case N=1 and the
\ separate scan is gone. What is left is one statement of the linear order, one
\ liveness, one interval rule, one class rule, one victim rule and one plan - so
\ a routine that branches and a routine that does not are allocated by the same
\ words, and a fixture about either measures the same code.
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
\ WHAT PRE-COLOURING DOES, AND WHEN A MOVE IS UNAVOIDABLE. A block argument of
\ the entry block whose position the contract names is given exactly that
\ register, before any register is handed out at that position, and the scan then
\ cannot hand it out while the argument is live because it is held like any other
\ assignment. A returned value is different: what the contract says about it is
\ where it has to be when control LEAVES, so the walk gives it the declared
\ register at its definition when that register is free - which costs nothing and
\ is why an ordinary routine emits no extra instruction - and when it is not
\ free, or when the value is an argument already pinned somewhere else, or when
\ it spent part of its life in a frame slot, the value is placed like any other
\ and the walk plans a register-to-register move in front of the return. A move
\ is a decision the same way a spill is: this pass publishes it and
\ src/compiler/native/spill.f builds the module in which it is an operation.
\
\ AND BOTH ARE STATED ABOUT A CLASS, NOT A VALUE. What holds a register here is a
\ class - the values an edge or a schema tie says are one register - so a
\ declared register is a constraint on the class its value belongs to, and the
\ move is planned when the class did not end up in the declared register. For a
\ routine of one block every class is usually one value and the two readings
\ coincide; for a routine that branches, giving one member the register and not
\ the others would not be an answer at all.
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
\ RUNNING OUT OF REGISTERS IS A DECISION, NOT A REFUSAL. A block can hold more
\ values at once than any register file has - a long chain of literals proves it
\ - so the bound cannot be proved away and spilling is the answer. A spill is a
\ store into a slot of the routine's own frame and a load
\ back out of it, and the A64IR dialect has both, so this pass decides where they
\ go instead of refusing the program. Three refusals are left, and none of them
\ is register pressure by itself: E-A64RA-PRESSURE is the routine's declared
\ frame running out of slots, E-A64RA-POOL is the one shape no spill can serve -
\ an operation that needs more registers at a single instant than the routine may
\ destroy, with every register already holding a value that same operation reads
\ - and E-A64RA-SPILL is a position where every class holding a register is one
\ this pass may not put in the frame. A routine that may destroy nothing is the
\ smallest example of the second.
\
\ THE COST RULE, AND WHY IT IS STATED RATHER THAN TUNED. When a register has to
\ be taken, the class taken is the one whose next read is furthest away: a store
\ bought now then buys the most operations before a reload is needed, which is
\ the classic furthest-next-use rule. Two classes whose next reads are equally
\ far - two values never read again, for instance - are separated by the lower
\ register number, so one program always allocates one way and a fixture can
\ assert the exact registers. A class that loses its register loses it for the
\ whole of its life: its definition is followed by a store and every read of it
\ is preceded by a load. That is what a class IS - every member is one register,
\ so it is also one slot - and it is why a slot is written exactly once, which is
\ how src/compiler/native/regalloc-verify.f decides what a reload reads.
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
\ AN ANCHOR IS A BLOCK AND A POSITION INSIDE IT. A routine of one block would
\ need only the position, because there is one operation order and an index into
\ it names one operation. With more than one block a position names one operation
\ per block, so every row carries the block as well or the lowering pass puts the
\ store in whichever block its cursor happened to reach.
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
\ heap objects, so this pass allocates one routine at a time - the single-task
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
require src/compiler/native/clobber.f
require src/compiler/native/frame.f
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

\ ---- how much of one routine this pass holds ---------------------------------
\ Spill decisions in one routine. Each one is an operation the lowering pass will
\ insert: one store per value that loses its register, one load before each
\ operation that reads it afterwards. A routine of VMAX values has fewer than
\ VMAX reads per value, but a ceiling that says so exactly would be a product of
\ two ceilings; this is the flat one both this pass and the lowering pass carry.
1024 constant PLMAX

\ The three kinds of decision.
0 constant P-STORE
1 constant P-RELOAD
2 constant P-MOVE                    \ a returned value put where it has to leave

\ The three value classes this dialect has: a general register, a floating
\ register, and the memory token the frame forms thread. Two of them are held in
\ registers and the third is held nowhere.
0 constant C-GPR
1 constant C-TOKEN
2 constant C-FPR

\ The two register FILES, which is what the two register-holding classes index.
\ A register number names a register of ONE file - d0 and x0 are two registers
\ and both are number zero - so every table below that is keyed by a register is
\ keyed by the file and the number together, and a class that lives in no file
\ has no row at all rather than a row nothing writes.
\
\ THE FILE IS WHAT EVERY POOL AND EVERY HOLDER QUESTION BELOW IS ASKED OF, never
\ the class. The two agree today, one class per file, and they stop agreeing the
\ moment one file holds two classes - this machine's vector registers are the
\ floating ones, v3 and d3 being one register - at which point a class-keyed pool
\ hands out registers another class is already holding. So the map is read once,
\ here, and everything downstream carries the file it answered. A class the map
\ does not name is refused rather than defaulted onto the general file, which is
\ what makes adding a class a loud failure instead of a quiet one.
2 constant FILES-N
0 constant F-GPR
1 constant F-FPR

\ The class is held in no register file at all. It is an answer and not a
\ refusal, because "which file is this value's" is asked of every value the
\ routine has, and the memory token's honest answer is "none" - while a class
\ this map has never heard of is still a refusal.
-1 constant NOFILE

: FILE-OF ( n -- n )
   {: cls:n :}
   cls C-GPR = if F-GPR exit then
   cls C-FPR = if F-FPR exit then
   cls C-TOKEN = if NOFILE exit then
   E-A64RA-CLASS throw ;

\ This value is in no slot.
-1 constant NOSLOT

\ The register file, taken from the schema that owns the machine facts.
A64EFF:FILE-SIZE constant REGS-N

\ No position at all: what the tables hold for a value the walk has not measured
\ yet. Every position a measured value carries is one of the linear order below,
\ which starts at zero.
-1 constant NOPOS

\ The three attribute keys that say an operation reaches the CALLER's data stack:
\ a slot of it, how far the pointer moves, and how far a call takes it back. An
\ operation carrying one of them belongs to a fixed sequence - the routine's own
\ entry or exit, or a call site - and nothing may be placed inside one.
3 constant DKEYS-N
0 constant DK-SLOT
1 constant DK-BYTES
2 constant DK-BACK

\ Nothing holds this register.
-1 constant NOBODY

\ The operation carries no attribute under this key.
-1 constant NOATTR

\ Positions one side of a calling convention can name, which is the contract's
\ own bound rather than a second one.
A64EFF:SEQ-LIMIT constant FIXED-MAX

\ The two ways a routine's contract can name a register for one of its values,
\ which is one table of two planes because everything that reads one reads the
\ other the same way: D-FIX is where an argument place says the caller has PUT
\ the value, D-WANT is where a result place says it has to BE when control
\ leaves.
2 constant DECLS-N
0 constant D-FIX
1 constant D-WANT

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
variable N-PLAN
0 N-PLAN !
variable N-SLOTS
0 N-SLOTS !
variable BASE-N                      \ the first frame byte this walk may use
0 BASE-N !
variable ARGS-N
0 ARGS-N !
variable OUTS-N
0 OUTS-N !

1 TYPED-BUFFER BND-MOD IR-ID:ir-module-id
1 TYPED-BUFFER BND-TYP IR-ID:ir-type-id
1 TYPED-BUFFER BND-MEM IR-ID:ir-type-id
1 TYPED-BUFFER BND-FPR IR-ID:ir-type-id
1 TYPED-BUFFER BND-SLOT IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-MOV IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-ENTRY IR-ID:ir-symbol-id
DKEYS-N TYPED-BUFFER BND-DKEY IR-ID:ir-symbol-id

1 TYPED-BUFFER S-MOD IR-ID:ir-module-id
1 TYPED-BUFFER S-POOL A64EFF:gprs
1 TYPED-BUFFER S-FPOOL A64EFF:fprs

create V-DEF VMAX cells allot
create V-LAST VMAX cells allot
create V-REG VMAX cells allot
create V-SET VMAX cells allot
create V-CLS VMAX cells allot
create V-SLOT VMAX cells allot
create V-DECL DECLS-N VMAX * cells allot
create A-REG FIXED-MAX cells allot
create O-REG FIXED-MAX cells allot
create R-HOLD FILES-N REGS-N * cells allot
create PL-BLK PLMAX cells allot
create PL-POS PLMAX cells allot
create PL-KIND PLMAX cells allot
create PL-VAL PLMAX cells allot

\ ---- the slots, read back ----------------------------------------------------
\ The registers of one file this routine may hand out. Two pools, asked the same
\ way of two contract fields, because a shortage in one file is not relieved by a
\ free register in the other. Each file is named and there is no default arm: a
\ file this word does not know would otherwise be handed the general registers
\ and hand them out twice.
: POOL-BITS ( n -- n )
   {: fl:n :}
   fl F-FPR = if 0 S-FPOOL @ A64EFF:FPRS-N exit then
   fl F-GPR = if 0 S-POOL @ A64EFF:GPRS-N exit then
   E-A64RA-CLASS throw ;

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
: FILE-AT ( n -- n )                 CLS-AT FILE-OF ;
: SLOT-AT ( n -- n )                 cells V-SLOT + @ ;

: DECL-IX ( n n -- n )               {: d:n k:n :} d VMAX * k + ;
: DECL-AT ( n n -- n )               DECL-IX cells V-DECL + @ ;
: DECL! ( n n n -- )                 {: v:n d:n k:n :} v d k DECL-IX cells V-DECL + ! ;

: DEF! ( n n -- )                    {: v:n k:n :} v k cells V-DEF + ! ;
: LAST! ( n n -- )                   {: v:n k:n :} v k cells V-LAST + ! ;
: REG! ( n n -- )                    {: v:n k:n :} v k cells V-REG + ! ;
: SET! ( n n -- )                    {: v:n k:n :} v k cells V-SET + ! ;
: CLS! ( n n -- )                    {: v:n k:n :} v k cells V-CLS + ! ;
: SLOT! ( n n -- )                   {: v:n k:n :} v k cells V-SLOT + ! ;

\ The holder table's key is a register, and a register is a file and a number.
\ The file is checked against the table's own shape here rather than trusted from
\ the caller: this is the one row arithmetic in the pass, NOFILE is a real answer
\ FILE-OF gives, and a row index off either end would be a quiet write into
\ whatever is next in the dictionary.
: RIX ( n n -- n )
   {: fl:n r:n :}
   fl 0 < fl FILES-N >= or if E-A64RA-CLASS throw then
   r 0 < r REGS-N >= or if E-A64RA-CLASS throw then
   fl REGS-N * r + ;

: HOLD-AT ( n n -- n )               RIX cells R-HOLD + @ ;
: HOLD! ( n n n -- )                 {: v:n fl:n r:n :} v fl r RIX cells R-HOLD + ! ;

\ Every register of every file holds nothing. Written once and used by both the
\ walk's own reset and the start of each scan, and it loops over the FILES rather
\ than naming them, so a file added to the map above is cleared without this word
\ being touched.
: HOLDERS-CLEAR ( -- )
   FILES-N 0 ?do
      REGS-N 0 ?do NOBODY j i HOLD! loop
   loop ;

: TABLES-CLEAR ( -- )
   VMAX 0 ?do
      0 i SET!
      NOPOS i DEF!
      NOPOS i LAST!
      NOBODY i REG!
      C-GPR i CLS!
      NOSLOT i SLOT!
      DECLS-N 0 ?do NOBODY i j DECL! loop
   loop
   HOLDERS-CLEAR
   0 N-PLAN !
   0 N-SLOTS ! ;

\ ---- the spill plan ----------------------------------------------------------
\ One row per operation the lowering pass has to insert, in the order the walk
\ decided them, each anchored to the block it belongs in and the index inside
\ that block of the operation it goes in front of. See the header for why the
\ block is carried and not only the index.
: PLAN+ ( n n n n -- )
   {: blk:n kind:n pos:n k:n :}
   N-PLAN @ {: j:n :}
   j PLMAX >= if E-A64RA-CAP throw then
   blk j cells PL-BLK + !
   pos j cells PL-POS + !
   kind j cells PL-KIND + !
   k j cells PL-VAL + !
   j 1+ N-PLAN ! ;

\ Is this value already being reloaded in front of this operation? One reload
\ serves every read of one value by one operation, so an operation that reads a
\ spilled value twice takes one register for it and not two.
: RELOADED? ( n n n -- bool )
   {: blk:n k:n pos:n :}
   false
   N-PLAN @ 0 ?do
      i cells PL-KIND + @ P-RELOAD =
      i cells PL-BLK + @ blk = and
      i cells PL-POS + @ pos = and
      i cells PL-VAL + @ k = and
      if drop true leave then
   loop ;

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

\ ---- what an operation says about the caller's data stack --------------------
\ A routine reaches the caller's stack in fixed sequences - the entry run that
\ reads its arguments, the exit run that publishes its results, and the store and
\ load runs a call site is made of - and every one of those is checked as a
\ contiguous shape by src/compiler/native/regalloc-verify.f. So nothing may be
\ placed INSIDE one, and this walk has to know which operations they are. It asks
\ the same way that validator does: by the attribute keys the dialect declares
\ for that region, never by an opcode name.
: DSTACK-TOUCH? ( IR-ID:ir-op-id -- bool )
   {: id:IR-ID:ir-op-id :}
   false
   id ATTRS-OF 0 ?do
      id i ATTR-KEY-AT {: key:IR-ID:ir-symbol-id :}
      DKEYS-N 0 ?do
         key i BND-DKEY @ SAME-SYM? if drop true leave then
      loop
   loop ;

\ ---- what a call site destroys -----------------------------------------------
\ A call site is the one place in a routine where a register can lose its value
\ without any instruction of this module naming it. Which registers those are is
\ not a guess: a callee the native chain published recorded what it destroys
\ against the address its code starts at (src/compiler/native/clobber.f), and the
\ call operation carries that address. So a value whose live range crosses a call
\ may be given any register the callee does not write, and must not be given one
\ it does.
\
\ THE TWO ANSWERS FOR A CALL WITH NO RECORD, and both are the worst case rather
\ than a guess. A call to ANOTHER word whose address this process has no row for
\ is a word the engine's own emitter compiled: nothing is known about it, so
\ every register of this routine's pool is assumed destroyed and no value may
\ cross it in a register - which is exactly the discipline that was in force
\ before any of this existed. A call to THIS SAME routine is not unknown at all
\ and is still the whole pool: the callee is this routine and its contract
\ destroys precisely the registers this walk is handing out.
: ATTR-INT-OF ( IR-ID:ir-op-id IR-ID:ir-symbol-id -- n )
   {: id:IR-ID:ir-op-id want:IR-ID:ir-symbol-id :}
   NOATTR
   id ATTRS-OF 0 ?do
      id i ATTR-KEY-AT want SAME-SYM? if
         drop
         id i ATTR-INT-AT
         leave
      then
   loop ;

\ Is this operation the branch of a call site? Asked by the attribute a call
\ carries and no other operation does - how far the branch takes the data-stack
\ pointer back - so a form added to the dialect is judged by what it says about
\ itself, exactly as DSTACK-TOUCH? above is.
: CALL-AT? ( IR-ID:ir-op-id -- bool )
   DK-BACK BND-DKEY @ ATTR-INT-OF NOATTR <> ;

\ And is it the branch a routine LEAVES through? It names an address, as a call
\ to another word does, and carries no take-back count, because control does not
\ come back for anything to be taken back. Both halves are attributes, so this is
\ the same reading CALL-AT? is made by and no opcode is spelled here either.
: TAILBR-AT? ( IR-ID:ir-op-id -- bool )
   {: id:IR-ID:ir-op-id :}
   id 0 BND-ENTRY @ ATTR-INT-OF NOATTR = if false exit then
   id CALL-AT? 0= ;

\ What a call leaves alone in ONE file. Named per file with no default arm, for
\ POOL-BITS' reason: a file this word does not know would be held against the
\ record of what the callee does to the GENERAL registers, and a value of it
\ would be left across the call looking safe.
: CALL-BITS ( IR-ID:ir-op-id n -- n )
   {: id:IR-ID:ir-op-id fl:n :}
   id 0 BND-ENTRY @ ATTR-INT-OF {: e:n :}
   e NOATTR = if fl POOL-BITS exit then
   fl F-FPR = if
      e 0 S-FPOOL @ NCLOB:FPR-CLOB A64EFF:FPRS-N exit
   then
   fl F-GPR = 0= if E-A64RA-CLASS throw then
   e 0 S-POOL @ NCLOB:GPR-CLOB A64EFF:GPRS-N ;

: FORBIDDEN? ( n n -- bool )
   {: forbid:n r:n :}
   1 r lshift forbid and 0<> ;

\ ---- the two value classes this dialect has ----------------------------------
\ A general register, or the memory token the frame forms thread. Both identities
\ came from the dialect itself at binding time, so nothing here compares
\ spellings or knows which opcode produced the value. A value of any third type
\ has no class here and is refused rather than given a register.
: CLASS-OF ( IR-ID:ir-value-id -- n )
   {: id:IR-ID:ir-value-id :}
   id VALUE-TYPE-AT {: t:IR-ID:ir-type-id :}
   t 0 BND-TYP @ SAME-TYPE? if C-GPR exit then
   t 0 BND-FPR @ SAME-TYPE? if C-FPR exit then
   t 0 BND-MEM @ SAME-TYPE? if C-TOKEN exit then
   E-A64RA-CLASS throw ;

\ Every value the module holds has to be a value the walk measured, or it has
\ read only part of the program it is allocating for.
: COVER-CK ( -- )
   V-VALR VW IR-OP:FVALUES {: n:n :}
   n VMAX > if E-A64RA-CAP throw then
   n 0 ?do i SET-AT 0= if E-A64RA-SHAPE throw then loop
   n N-VALS ! ;

: POOL-HAS? ( n n -- bool )
   {: fl:n r:n :}
   fl POOL-BITS 1 r lshift and 0<> ;

\ The lowest-numbered register of one file's pool that holds nothing and that
\ the value being placed may have, or -1 when every one of them is taken or
\ barred. Lowest rather than next-around, so the same block always allocates the
\ same way. `forbid` is the registers a call inside the value's own live range
\ destroys - no bits at all for a value that crosses no call, which is every
\ value of a routine that calls nothing.
: FREE-REG ( n n -- n )
   {: fl:n forbid:n :}
   -1
   REGS-N 0 ?do
      fl i POOL-HAS?
      forbid i FORBIDDEN? 0= and
      fl i HOLD-AT NOBODY = and if drop i leave then
   loop ;

\ Nothing below hands out a register that is not the routine's: FREE-REG only
\ answers one of the pool, and a register taken from a value was one of the pool
\ when that value got it. The check is here because a register outside the
\ contract would be the routine destroying something it promised to keep, and
\ that must fail closed rather than be argued about.
: TAKE ( n n -- )
   {: k:n r:n :}
   k FILE-AT {: fl:n :}
   fl r POOL-HAS? 0= if E-A64RA-POOL throw then
   r k REG!
   k fl r HOLD! ;

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
      F-GPR i cells A-REG + @ POOL-HAS? 0= if E-A64RA-FIXED throw then
   loop
   OUTS-N @ 0 ?do
      F-GPR i cells O-REG + @ POOL-HAS? 0= if E-A64RA-FIXED throw then
   loop ;

\ A convention that names more positions than the routine has arguments, or more
\ than it returns values, is not this routine's convention.
: FIXED-ARITY-CK ( IR-ID:ir-block-id IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id rb:IR-ID:ir-block-id :}
   bk ARG-COUNT ARGS-N @ < if E-A64RA-FIXED throw then
   rb TERM-AT OPERANDS-OF OUTS-N @ < if E-A64RA-FIXED throw then ;

\ A side declared in data-stack slots is a side the module no longer carries in
\ registers at all: the selector turned each place into a load at the top of the
\ block or a store in front of the return, so the block has no argument and the
\ terminator no operand for it. A module that still carries them has not been
\ through that step, and allocating it would hand the arguments registers no
\ caller ever wrote to.
\ The two sides are asked of two different blocks, because they are about two
\ different instants: the arguments arrive where the caller enters, which is the
\ entry block, and the results leave where control returns, which is the block
\ whose terminator names no successor. In a routine of one block they are the
\ same block; with control flow they are not, and asking the entry block about
\ the results would read a branch's block arguments as if they were the routine's.
\ IT IS THE DECLARED CONVENTION THAT DECIDES, not the place lists. Asking whether
\ a list names a slot gives no answer for a routine that passes nothing, and a
\ pass that reads the lists twice can come to two views of one contract.
: LOWERED-CK ( IR-ID:ir-block-id IR-ID:ir-block-id A64EFF:conv -- )
   {: bk:IR-ID:ir-block-id rb:IR-ID:ir-block-id cv:A64EFF:conv :}
   cv A64EFF-CONV:DSTACK A64EFF-CONV:EQ 0= if exit then
   bk ARG-COUNT 0<> if E-A64RA-PLACE throw then
   rb TERM-AT TAILBR-AT? if exit then
   rb TERM-AT OPERANDS-OF 0<> if E-A64RA-PLACE throw then ;

\ ---- taking a register away --------------------------------------------------
\ The next slot of the routine's frame. Slots are handed out in order and never
\ handed out twice, so no two values ever share one.
\
\ THIS WALK IS NOT HELD TO THE FRAME ITS CONTRACT DECLARED, and that is the whole
\ of what habu-derive-a-routine-84ed36b6 changed. A routine's frame is declared
\ before its body is allocated, so a declaration can only ever be a guess at how
\ many values will not fit their registers - and it was the caller making it.
\ Refusing the walk when the guess was too small meant a program the chain can
\ compile perfectly well was rejected for a number nobody was in a position to
\ get right. So the walk hands out what the program needs and RECORDS it, and
\ FRAME below answers the frame the routine must therefore declare. The
\ declaration is this pass's OUTPUT now, not its input.
\
\ WHAT IS STILL A REFUSAL IS A DEMAND NO FRAME CAN MEET. That is a fact about the
\ machine and not about anybody's guess, so it keeps the pressure name.
\
\ THEY START ABOVE WHAT THE PROLOGUE OWNS. A routine that calls keeps its
\ caller's return address in the bottom slot of its own frame, and
\ src/compiler/native/frame.f is the one place that says so; this walk asks it
\ where its own slots may begin rather than assuming the frame is empty. That is
\ what makes a routine that both calls and spills one frame with one layout
\ instead of two passes agreeing by luck.
\
\ THE CEILING IS THE TIGHTER OF THE TWO THE CHAIN HAS, taken rather than chosen
\ so that it stays true if either moves: the architecture cannot describe a frame
\ past A64EFF:FRAME-MAX, and no pass of this chain can name more than NFROZEN:VMAX
\ slots - src/compiler/native/regalloc-verify.f refuses a slot ordinal at that
\ ceiling, so a walk that handed one out would only be refused later by name.
: FRAME-CEIL ( -- n )
   NFROZEN:VMAX A64IR:SLOT-WIDTH *  A64EFF:FRAME-MAX min ;

: NEW-SLOT ( -- n )
   BASE-N @  N-SLOTS @ A64IR:SLOT-WIDTH *  + {: off:n :}
   off A64IR:SLOT-WIDTH + FRAME-CEIL > if E-A64RA-PRESSURE throw then
   N-SLOTS @ 1+ N-SLOTS !
   off ;

\ How deep this walk reached, and the frame that implies. The first is what the
\ prologue owns plus every slot handed out; the second is that rounded to what
\ the stack pointer may be moved by, which is what a routine has to declare to
\ hold it. A64EFF owns the rounding so that this and the declaration
\ src/compiler/native/abi.f builds are the same number rather than two that agree
\ by luck - the validator refuses the difference between them.
: DEPTH-WANT ( -- n )
   BASE-N @  N-SLOTS @ A64IR:SLOT-WIDTH *  + ;

: FRAME-WANT ( -- n )
   DEPTH-WANT A64EFF:FRAME-ROUND ;

\ ---- the linear order, and everything decided over it ------------------------
\ Everything above this line reads one declaration or one operation. What follows
\ is the allocation itself, and it is six steps.
\
\ ONE. A LINEAR ORDER AND GLOBAL POSITIONS. Blocks are numbered in the order the
\ module records them - the order the selector built them in - so every pass that
\ reasons about the module numbers the same instruction the same way. Each block
\ gets one position for its arguments and one per operation, so block b holds
\ positions B-ST[b] (its arguments) through B-EN[b] (its last operation), and the
\ next block starts one past that.
\
\ THE EMITTER WRITES THE BLOCKS OUT IN AN ORDER OF ITS OWN, and that changes
\ nothing here. src/compiler/native/emit.f chooses which block's instructions
\ follow which, because that is what decides whether a terminator's trailing
\ branch can be left out; it makes that choice AFTER this allocation has been
\ accepted, and it reads the assignment rather than being read by it. So every
\ range, every hull and every interference below is a fact about the module and
\ the register budget alone. It also means the conservatism named in step three
\ is a fact about the RECORDED order: a value live across a block the module
\ records inside a loop is held over that block whether or not the emitter writes
\ it there, and no reordering downstream can shorten a hull.
\
\ TWO. LIVENESS BY BACKWARD DATAFLOW. use(b) is every value an operation of b
\ reads that b did not already define; def(b) is b's block arguments together
\ with every result of its operations. Then live-out(b) is the union of live-in
\ over b's successors and live-in(b) is use(b) plus live-out(b) minus def(b),
\ iterated until nothing changes. The block arguments are what keeps this honest
\ across an edge: the values a terminator hands over are USES in its own block
\ and the arguments they land in are DEFS of the destination, so nothing flows
\ across an edge by accident and a loop-carried value is not confused with the
\ value it replaces.
\
\ THREE. HULL INTERVALS. A value's range is its definition, every use, and the
\ whole of every block it is live in or out of. That is one contiguous stretch -
\ no holes and no interval lists - because the shapes a structured Forth control
\ word builds are reducible: a value live across a back edge is live over the
\ whole loop, and the loop's blocks are contiguous in the linear order. It is
\ conservative where a value is live in only part of a block it is live-out of,
\ and that conservatism costs registers rather than correctness. Splitting the
\ range there is an optimisation and it is not this leaf.
\
\ FOUR. BLOCK-ARGUMENT CLASSES. The branch itself moves nothing, so a block
\ argument and every value handed to it must be one physical register. They are
\ therefore one class - a union-find over the edges - and the class, not the
\ value, is what gets a register, over the hull of its members' ranges. Two
\ members that are live at the same time would need one register to hold two
\ values, and that is E-A64RA-EDGE rather than an allocation that is quietly
\ wrong. It cannot happen for what this chain builds, because
\ src/compiler/native/select.f copies every value crossing an argument-carrying
\ edge into a value of its own first, and those copies die at the branch.
\
\ A SCHEMA TIE IS THE SAME KIND OF CONSTRAINT AND IS UNIONED THE SAME WAY. A form
\ that names one register field for a result and an operand says those two values
\ ARE one register, which is a statement about class membership and nothing else,
\ so it is a union like an edge is. See MB-TIES below for why leaving it to the
\ scan looked like it worked and did not.
\
\ FIVE. COALESCING BY PREFERENCE. Step four is the classes that MUST be one
\ register. This step is the classes that MAY be. An a64.mov copies one value
\ into another, so if its two ends are given one register it moves that register
\ into itself and src/compiler/native/emit.f writes no instruction for it at
\ all. The copies worth removing are exactly the ones step four's edge splitting
\ put there - one per value crossing an argument-carrying edge, which on a loop
\ latch is a value copied back to where it came from.
\
\ A COPY'S TWO ENDS ARE MERGED WHEN THE MERGE KEEPS THE CLASS INVARIANT, AND NOT
\ OTHERWISE. The invariant is step four's own: no two members of one class are
\ live at the same instant. So the question asked of a candidate is the question
\ MB-MEMBER-CK asks of the answer - is any member of the source's class live at
\ the same instant as any member of the destination's class - and the merge is
\ made only when the answer is no. A merge that would put two live values in one
\ register is simply not made, the copy stays a real instruction, and the
\ allocation is the one this pass would have produced without this step. There is
\ no wrong allocation here that something later repairs.
\
\ WHY THAT IS THE RIGHT QUESTION AND NOT MERELY A SAFE ONE. What the merge buys
\ is that the copy can be dropped, and dropping it is sound only if the register
\ already holds the source's value where the copy stood and still holds it at
\ every read of the destination. Both follow from non-interference alone. A
\ member of the class defined anywhere between the source's definition and the
\ copy would be live where the source is live; one defined between the copy and a
\ read of the destination would be live where the destination is live. Neither
\ can exist in a class whose members never overlap, so the register holds exactly
\ what the elided copy would have put in it. Non-interference is measured over
\ the hull intervals of step three, which over-approximate real liveness, so a
\ pair this test calls disjoint is disjoint at run time too.
\
\ THE ORDER MERGES ARE MADE IN, AND WHY IT DOES NOT DECIDE CORRECTNESS. Merging
\ grows classes, so one merge can stop a later one from being made: the question
\ is asked against the classes as they stand at the time. Candidates are
\ therefore taken in the order the module records them - blocks in module order,
\ operations in block order, the order every pass in this chain already agrees on
\ - so one module always gets one answer. What the order cannot do is admit an
\ unsound merge: the test is asked against current membership, membership only
\ grows, so the test only ever gets harder as the walk goes on. That is also why
\ src/compiler/native/regalloc-verify.f does not need to know the order. It
\ checks the RESULT - no two values live at the same instant were given one
\ register - which is a property of the assignment and not of the walk that
\ reached it.
\
\ THE PRICE, STATED. A merged class is held over the hull of both its parts, so
\ coalescing across a gap keeps a register busy where the two classes apart would
\ have let something else use it. That can turn a routine that fitted into one
\ that does not, and this path refuses rather than spills, so coalescing is paid
\ for in E-A64RA-SPILL and never in wrong code. Choosing between candidates that
\ compete for the same class, instead of taking them in module order, is dot
\ habu-choose-between-competing-ecc61e5c.
\
\ SIX. SPILLING A WHOLE CLASS. A class is what holds a register here, so a class
\ is what loses one: when the pool runs short at a position, one class goes into
\ a frame slot for the whole of its life, its definition is followed by a store
\ and every read of it is preceded by a load. Putting the whole class away rather
\ than splitting one member's range at the point its register is taken is what a
\ class is for - every member of one class is one register, so it is also one
\ slot, and a slot written once is what makes a reload's value decidable from the
\ module alone.
\
\ WHICH CLASS, AND WHAT THIS PASS STILL REFUSES. The class taken is the one whose
\ next read is furthest away in the linear order, among the classes holding a
\ register that the failing position does not itself touch. Five kinds of class
\ are not candidates at all and each has its own reason, written where
\ MB-SPILLABLE? and MB-KEEP-BLOCK decide them: a class of more than one value
\ would write one slot more than once, a block argument would mean rewriting a
\ block's interface, a memory token lives in no register, a value a data-stack
\ operation reads would need a load inside a run the validator measures as a
\ shape, and a value defined or read outside the entry and exit blocks would put
\ a frame access where the memory order cannot be stated. A position where
\ nothing can be taken is E-A64RA-SPILL; a position where every register holds
\ something that position itself needs is E-A64RA-POOL; a frame with no room for
\ the next slot is E-A64RA-PRESSURE.

\ ---- the sets ----------------------------------------------------------------
\ Four bitsets per block over the module's values, in one array so the accessors
\ are three words rather than four copies of them.
64 constant SET-BITS
VMAX SET-BITS / constant SETC
0 constant P-IN
1 constant P-OUT
2 constant P-USE
3 constant P-DEF
4 constant PLANES

\ A value's range starts no earlier than this, so the first member of a class
\ always lowers it.
$3FFFFFFF constant POS-INF

here CELL 1- and CELL swap - CELL 1- and allot
variable N-BLKS
0 N-BLKS !
variable MB-AT
0 MB-AT !
variable CHANGED
0 CHANGED !
variable SHORT-AT                    \ the position the scan ran short at, or -1
-1 SHORT-AT !
variable SHORT-FILE                  \ and which register file it ran short of
0 SHORT-FILE !
variable RET-B                       \ the block control leaves the routine through
0 RET-B !

create B-ST BMAX cells allot
create B-EN BMAX cells allot
create L-SETS PLANES BMAX * SETC * cells allot
create TMPSET SETC cells allot
create UF VMAX cells allot
create CL-LO VMAX cells allot
create CL-HI VMAX cells allot
create CL-SLOT VMAX cells allot      \ the frame slot a spilled class went into
create CL-DEF VMAX cells allot       \ where a spilled class is written
create CL-ANCH VMAX cells allot      \ where the store that puts it away stands
create CL-SIZE VMAX cells allot      \ how many values one class holds
create CL-KEEP VMAX cells allot      \ whether this class must stay in a register
create CL-FIX VMAX cells allot       \ the register the contract pins this class to
create CL-WANT VMAX cells allot      \ the register the contract wants it to leave in

: BIT-CELL ( n -- n )    SET-BITS / ;
: BIT-MASK ( n -- n )    SET-BITS mod 1 swap lshift ;

: LS-IX ( n n n -- n )
   {: pl:n b:n w:n :}
   pl BMAX * b + SETC * w + ;

: LS@ ( n n n -- n )     LS-IX cells L-SETS + @ ;

: LS! ( n n n n -- )
   {: val:n pl:n b:n w:n :}
   val  pl b w LS-IX cells L-SETS + ! ;

: LS-HAS? ( n n n -- bool )
   {: pl:n b:n v:n :}
   pl b v BIT-CELL LS@  v BIT-MASK and 0<> ;

: LS-SET ( n n n -- )
   {: pl:n b:n v:n :}
   pl b v BIT-CELL LS@  v BIT-MASK or  pl b v BIT-CELL LS! ;

: TMP-CLEAR ( -- )
   SETC 0 ?do 0 i cells TMPSET + ! loop ;

: TMP-HAS? ( n -- bool )
   {: v:n :}
   v BIT-CELL cells TMPSET + @  v BIT-MASK and 0<> ;

: TMP-SET ( n -- )
   {: v:n :}
   v BIT-CELL cells TMPSET + @  v BIT-MASK or
   v BIT-CELL cells TMPSET + ! ;

: SETS-CLEAR ( -- )
   PLANES BMAX * SETC * 0 ?do 0 i cells L-SETS + ! loop ;

\ ---- step one: the linear order ----------------------------------------------
: MB-LAY1 ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   MB-AT @ b cells B-ST + !
   MB-AT @  f b BLOCK-AT OP-COUNT  + {: e:n :}
   e b cells B-EN + !
   e 1+ MB-AT ! ;

: MB-LAYOUT ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f BLOCK-COUNT {: n:n :}
   n BMAX > if E-A64RA-CAP throw then
   n N-BLKS !
   0 MB-AT !
   n 0 ?do f i MB-LAY1 loop ;

: OP-POS ( n n -- n )
   {: b:n i:n :}
   b cells B-ST + @ 1+ i + ;

\ ---- step two: liveness ------------------------------------------------------
: MB-USE1 ( n IR-ID:ir-value-id -- )
   {: b:n id:IR-ID:ir-value-id :}
   id SLOT {: v:n :}
   v TMP-HAS? if exit then
   P-USE b v LS-SET ;

: MB-DEF1 ( n IR-ID:ir-value-id -- )
   {: b:n id:IR-ID:ir-value-id :}
   id SLOT {: v:n :}
   P-DEF b v LS-SET
   v TMP-SET ;

: MB-OP-UD ( n IR-ID:ir-op-id -- )
   {: b:n id:IR-ID:ir-op-id :}
   id OPERANDS-OF 0 ?do b  id i OPERAND-AT  MB-USE1 loop
   id RESULTS-OF 0 ?do  b  id i RESULT-AT   MB-DEF1 loop ;

: MB-BLOCK-UD ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   TMP-CLEAR
   bk ARG-COUNT 0 ?do b  bk i ARG-AT  MB-DEF1 loop
   bk OP-COUNT 0 ?do  b  bk i OP-AT   MB-OP-UD loop ;

: SUCC-ORD ( IR-ID:ir-op-id n -- n )
   SUCC-AT IR-ID:BLOCK-LOCAL
   dup 0 < over N-BLKS @ >= or if E-A64RA-SHAPE throw then ;

: MB-OUT-ADD ( n n -- )
   {: b:n s:n :}
   SETC 0 ?do
      P-OUT b i LS@  P-IN s i LS@ or  P-OUT b i LS!
   loop ;

: MB-OUT ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   SETC 0 ?do 0 P-OUT b i LS! loop
   f b BLOCK-AT TERM-AT {: t:IR-ID:ir-op-id :}
   t SUCCS-OF 0 ?do
      b  t i SUCC-ORD  MB-OUT-ADD
   loop ;

: MB-IN1 ( n n -- bool )
   {: b:n w:n :}
   P-USE b w LS@   P-OUT b w LS@  P-DEF b w LS@ invert and   or {: nv:n :}
   nv  P-IN b w LS@ = if false exit then
   nv P-IN b w LS!
   true ;

: MB-IN ( n -- bool )
   {: b:n :}
   false
   SETC 0 ?do b i MB-IN1 or loop ;

: MB-PASS1 ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b MB-OUT
   b MB-IN if 1 CHANGED ! then ;

\ The sets only grow, and there are finitely many values and blocks, so the
\ iteration terminates. Blocks are visited backwards because that is the order
\ the answers propagate in and it is what keeps the number of rounds small.
: MB-LIVENESS ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   SETS-CLEAR
   N-BLKS @ 0 ?do f i MB-BLOCK-UD loop
   begin
      0 CHANGED !
      N-BLKS @ 0 ?do
         f  N-BLKS @ 1- i -  MB-PASS1
      loop
      CHANGED @ 0=
   until ;

\ ---- step three: the hull intervals ------------------------------------------
: MB-DEFINE ( IR-ID:ir-value-id n -- )
   {: id:IR-ID:ir-value-id pos:n :}
   id CLASS-OF {: cls:n :}
   id SLOT {: k:n :}
   k SET-AT 0<> if E-A64RA-SHAPE throw then
   1 k SET!
   cls k CLS!
   pos k DEF!
   pos k LAST! ;

: MB-USE ( IR-ID:ir-value-id n -- )
   {: id:IR-ID:ir-value-id pos:n :}
   id SLOT {: k:n :}
   k SET-AT 0= if E-A64RA-SHAPE throw then
   pos k LAST-AT max k LAST! ;

: MB-OP-RANGE ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id pos:n :}
   id OPERANDS-OF 0 ?do id i OPERAND-AT pos MB-USE loop
   id RESULTS-OF 0 ?do  id i RESULT-AT  pos MB-DEFINE loop ;

: MB-BLOCK-RANGE ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk ARG-COUNT 0 ?do
      bk i ARG-AT  b cells B-ST + @  MB-DEFINE
   loop
   bk OP-COUNT 0 ?do
      bk i OP-AT  b i OP-POS  MB-OP-RANGE
   loop ;

\ Live at a block's entry means the range reaches back to that entry; live at its
\ exit means the range reaches its last operation. Live-IN alone does NOT reach
\ the end of the block: a value live-in to a block and not live-out of it dies at
\ its last use inside, which the use scan already recorded. Extending it to the
\ end anyway is what would make a loop-carried argument and the copy that hands
\ it back round the loop look like two values live at once - they are one value
\ copied into itself, and that is the whole reason they are one class.
: MB-EXTEND1 ( n n -- )
   {: b:n k:n :}
   P-IN b k LS-HAS? if
      b cells B-ST + @  k DEF-AT min  k DEF!
   then
   P-OUT b k LS-HAS? if
      b cells B-EN + @  k LAST-AT max  k LAST!
   then ;

: MB-EXTEND-V ( n -- )
   {: k:n :}
   N-BLKS @ 0 ?do i k MB-EXTEND1 loop ;

: MB-RANGES ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   N-BLKS @ 0 ?do f i MB-BLOCK-RANGE loop
   COVER-CK
   N-VALS @ 0 ?do i MB-EXTEND-V loop ;

\ ---- step four: the block-argument classes -----------------------------------
: UF-INIT ( -- )
   VMAX 0 ?do
      i i cells UF + !
      POS-INF i cells CL-LO + !
      -1 i cells CL-HI + !
   loop ;

: UF-FIND ( n -- n )
   begin dup cells UF + @ over <> while
      cells UF + @
   repeat ;

\ Two values joined into one class have to be able to share one register, and
\ this asks their CLASSES and not their files, which is the one register question
\ in the pass that is not a file question. Two files are the plainest way to
\ fail - the same eight bytes in a general register and in a floating one are not
\ the same place, and no instruction reads both fields - but two classes of ONE
\ file need not be one place either: a file's registers can be read at more than
\ one width, and one value's register is then a part of another's rather than the
\ same one. Sharing needs the same place, so the same class is the question, and
\ a union across classes is refused by name rather than made and then
\ discovered - which is what would happen otherwise, the class being given one
\ register and half its members read out of something else.
: UF-UNION ( n n -- )
   {: a:n b:n :}
   a CLS-AT b CLS-AT <> if E-A64RA-FILE throw then
   a UF-FIND {: ra:n :}
   b UF-FIND {: rb:n :}
   ra rb = if exit then
   ra rb min  ra rb max cells UF + ! ;

: MB-EDGES-OF ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT TERM-AT {: t:IR-ID:ir-op-id :}
   t SUCCS-OF 1 <> if exit then
   f  t 0 SUCC-ORD  BLOCK-AT {: sb:IR-ID:ir-block-id :}
   t OPERANDS-OF sb ARG-COUNT <> if E-A64RA-EDGE throw then
   t OPERANDS-OF 0 ?do
      t i OPERAND-AT SLOT  sb i ARG-AT SLOT  UF-UNION
   loop ;

\ Two values that would have to share one register, and are live at the same
\ time. The rule is the read-then-write boundary, stated once for the whole
\ routine: one
\ operation reads its operands and then writes its results, so a value read for
\ the last time where another is written is not live at the same instant.
: OVERLAP? ( n n -- bool )
   {: a:n b:n :}
   a DEF-AT b DEF-AT = if true exit then
   a DEF-AT b DEF-AT < if
      a LAST-AT b DEF-AT > exit
   then
   b LAST-AT a DEF-AT > ;

\ ---- the registers the routine's own contract names --------------------------
\ A declared place is a fact about one VALUE - the argument the caller puts in a
\ register, the value the return has to leave in one - and it is recorded that
\ way, before a single class exists. What holds a register here is a class, so
\ the class reading is derived from the members below; recording it on the class
\ instead would mean recording it after the unions and losing which member said
\ it. Both lists refuse a memory token: the frame's ordering value lives in no
\ register, so a convention naming one is naming a place no caller can fill.
: MB-FIX! ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   ARGS-N @ 0 ?do
      bk i ARG-AT SLOT {: k:n :}
      k CLS-AT C-TOKEN = if E-A64RA-FIXED throw then
      i cells A-REG + @  D-FIX k DECL!
   loop ;

: MB-WANT! ( IR-ID:ir-block-id -- )
   {: rb:IR-ID:ir-block-id :}
   rb TERM-AT {: id:IR-ID:ir-op-id :}
   OUTS-N @ 0 ?do
      id i OPERAND-AT SLOT {: k:n :}
      k CLS-AT C-TOKEN = if E-A64RA-FIXED throw then
      i cells O-REG + @  D-WANT k DECL!
   loop ;

\ The one register a class is declared into on one of the two planes, read off
\ its members. Two members declared into two different registers cannot be one
\ register at all, and that is refused rather than resolved: a class joined by an
\ edge or a schema tie IS one register, and copying it into a second place is the
\ parallel copy this pass does not have (dot
\ habu-lower-parallel-copies-cdf9720e). It is also the statement the old "one
\ value returned at two declared positions" was making, said where registers are
\ actually decided.
: MB-ONE-DECL ( n n -- n )
   {: so-far:n x:n :}
   x NOBODY = if so-far exit then
   so-far NOBODY <> so-far x <> and if E-A64RA-FIXED throw then
   x ;

: MB-DECL-KIND ( n n -- n )
   {: r:n d:n :}
   NOBODY
   N-VALS @ 0 ?do
      i UF-FIND r = if d i DECL-AT MB-ONE-DECL then
   loop ;

\ The register the contract declares this class into: where an argument place
\ pins it if one does, and otherwise where a result place says it leaves. The pin
\ comes first because the caller has already put the value there - the walk can
\ only obey it - while the result place is a preference the return can pay a copy
\ for.
: MB-DECL-OF ( n -- n )
   {: r:n :}
   r D-FIX MB-DECL-KIND {: f:n :}
   f NOBODY <> if f exit then
   r D-WANT MB-DECL-KIND ;

: MB-DECLS! ( -- )
   N-VALS @ 0 ?do
      i UF-FIND i = if
         i D-FIX MB-DECL-KIND   i cells CL-FIX + !
         i D-WANT MB-DECL-KIND  i cells CL-WANT + !
      then
   loop ;

\ ---- the same question, asked of two whole classes ---------------------------
\ Is this value live at the same instant as any member of that class? The
\ per-value question is OVERLAP? above, unchanged - this only asks it of every
\ member.
: MB-MEETS? ( n n -- bool )
   {: a:n r:n :}
   false
   N-VALS @ 0 ?do
      i UF-FIND r = if a i OVERLAP? or then
   loop ;

\ Would merging these two classes break the invariant that no two members of one
\ class are live at once? Asked member against member, which is the same
\ comparison MB-MEMBER-CK makes over the finished classes: a union is allowed
\ exactly when the merged class would still pass that check. Both callers below -
\ the ties, which must merge, and step five, which prefers to - ask it here, so
\ there is one statement of the question rather than two.
\
\ It is asked pairwise rather than between the two class HULLS because the hulls
\ answer a slightly different question. A hull runs from the earliest definition
\ in the class to the last use, so two hulls that only touch can still hold a
\ pair that overlaps - a value defined at the touching position and never read
\ shares its definition point with a member of the other class - and the pair is
\ what the invariant is about. The hull is what the scan below holds a register
\ over; it is not the interference question.
: MB-CLASH? ( n n -- bool )
   {: ra:n rb:n :}
   false
   N-VALS @ 0 ?do
      i UF-FIND ra = if i rb MB-MEETS? or then
   loop ;

\ ---- the ties, which are must-share constraints too --------------------------
\ A form that names one register field for a result and an operand - the
\ move-wide overwrite is the one this dialect has - says those two values ARE one
\ physical register, exactly as an argument-carrying edge does. So a tie is a
\ union of the same kind and belongs in the same structure: the class is what
\ gets a register, and a tie decides membership of a class.
\
\ WHY IT IS WRITTEN DOWN HERE RATHER THAN LEFT TO THE SCAN. It used to be left to
\ the scan, and the scan does not enforce it. It came out right only because the
\ operand of an overwrite dies at the overwrite, so its register is free again
\ one position later, and FREE-REG hands out the lowest free register - so the
\ result usually got the register the operand had just given up. Usually is not a
\ rule. Step five below fixes the register of one end of a copy on purpose, which
\ is enough to make the two ends of a tie differ, and the validator then refuses
\ the whole routine with E-A64RAV-TIE. Stating the tie as a union makes it hold
\ by construction: the two ends are one class and a class is one register.
\
\ ENDS THAT ARE LIVE AT THE SAME INSTANT CANNOT BE ONE REGISTER AT ALL, and that
\ is E-A64RA-TIE - the register a tie needs is holding something else, which is
\ what that name has always said. It cannot
\ happen for what this chain builds: the overwrite is the only tied form and its
\ operand is the half-built constant, whose only reader is the overwrite itself.
: MB-TIE1 ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id i:n :}
   id  id i TIE-OPERAND-AT  OPERAND-AT SLOT {: s:n :}
   id  id i TIE-RESULT-AT   RESULT-AT  SLOT {: d:n :}
   s UF-FIND {: ra:n :}
   d UF-FIND {: rb:n :}
   ra rb = if exit then
   ra rb MB-CLASH? if E-A64RA-TIE throw then
   s d UF-UNION ;

: MB-TIES-OP ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id TIES-AT 0 ?do id i MB-TIE1 loop ;

: MB-TIES-BLOCK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT 0 ?do bk i OP-AT MB-TIES-OP loop ;

: MB-TIES ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   N-BLKS @ 0 ?do f i MB-TIES-BLOCK loop ;

\ ---- step five: coalescing the copies ----------------------------------------
\ Which operations are candidates. A copy is the one form whose whole effect is
\ to put one register's contents in another, so it is the one form that becomes
\ nothing when both ends are the same register. The name is the dialect's own,
\ learned at BIND-DIALECT, so this pass never spells an opcode itself.
: MB-COPY? ( IR-ID:ir-op-id -- bool )
   OPCODE-AT 0 BND-MOV @ SAME-SYM? ;

\ One candidate. Ends already in one class need nothing; ends whose classes hold
\ an interfering pair keep their copy; and ends the contract declares into two
\ different registers keep it too.
\
\ THE DECLARATION CLAUSE IS NOT CAUTION, IT IS WHAT THE COPY IS FOR. Merging is a
\ preference, and merging two ends the contract sends to two different registers
\ would throw the preference away and cost the very copy this step exists to
\ remove: the merged class can sit in ONE register, so one of the two
\ declarations would have to be paid for at the return anyway. The commonest
\ shape is exactly the one the spill lowering builds - a copy from an argument
\ pinned where the caller put it into the value the return has to leave elsewhere
\ - and merging it would put the result back in the argument's register and earn
\ a second copy for the same value.
: MB-COALESCE1 ( n n -- )
   {: s:n d:n :}
   s UF-FIND {: ra:n :}
   d UF-FIND {: rb:n :}
   ra rb = if exit then
   ra MB-DECL-OF {: da:n :}
   rb MB-DECL-OF {: db:n :}
   da NOBODY <> db NOBODY <> and  da db <> and if exit then
   ra rb MB-CLASH? if exit then
   s d UF-UNION ;

\ A copy of this dialect carries one operand and one result, which is what its
\ schema declares and what the freeze verifier has already held it to, so the
\ source is operand zero and the destination result zero.
: MB-COALESCE-OP ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id MB-COPY? 0= if exit then
   id 0 OPERAND-AT SLOT  id 0 RESULT-AT SLOT  MB-COALESCE1 ;

: MB-COALESCE-BLOCK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT 0 ?do bk i OP-AT MB-COALESCE-OP loop ;

\ The whole walk, in the module's own order. See step five in the header for why
\ the order is written down and why it cannot decide correctness.
: MB-COALESCE ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   N-BLKS @ 0 ?do f i MB-COALESCE-BLOCK loop ;

\ The hull of each class, which is what the scan below holds a register over. It
\ is computed after every union - the ones the edges force and the ones step five
\ prefers - so a merged class is held over the whole of both its parts.
: MB-CLASS1 ( n -- )
   {: k:n :}
   k UF-FIND {: r:n :}
   k DEF-AT   r cells CL-LO + @ min  r cells CL-LO + !
   k LAST-AT  r cells CL-HI + @ max  r cells CL-HI + ! ;

\ The class invariant, stated over the finished classes: no two members of one
\ class are live at the same instant. It is the reason the edge splitting exists
\ and it is the condition step five asks before every merge, so a bug in either
\ dies here rather than in an allocation that puts two live values in one
\ register.
: MB-MEMBER-CK ( n n -- )
   {: a:n b:n :}
   a UF-FIND b UF-FIND <> if exit then
   a b OVERLAP? if E-A64RA-EDGE throw then ;

: MB-CLASSES ( -- )
   N-VALS @ 0 ?do i MB-CLASS1 loop
   N-VALS @ 0 ?do
      N-VALS @ i 1+ ?do
         j i MB-MEMBER-CK
      loop
   loop ;

\ ---- which class may be put in the frame --------------------------------------
\ Everything below decides ONE thing: when the pool runs short, which class goes
\ into a frame slot. Five things disqualify a class, and each of them is a rule
\ this pass would otherwise have to break.
\
\ A CLASS OF MORE THAN ONE VALUE would write one slot more than once - every
\ member's own definition would have to store into it - and "a slot is written
\ once" is how src/compiler/native/regalloc-verify.f decides what a reload reads.
\ Generalising that rule across a routine that branches is dot
\ habu-spill-a-class-f712088d; until it lands, one value per slot.
\
\ A BLOCK ARGUMENT is handed over by a branch, so taking it out of a register
\ would mean changing the interface of a block the lowering pass copies as it
\ stands - and the values feeding it across every edge with it.
\
\ A MEMORY TOKEN lives in no register, so there is nothing to take away.
\
\ A VALUE A DATA-STACK OPERATION READS. Those operations stand in contiguous runs
\ - the routine's entry and exit sequences and the two halves of a call site -
\ and a load in front of one would be an operation inside a run the validator
\ measures as a shape. The value a routine publishes and every value live across
\ a call are read that way, so they stay in registers; a value a data-stack
\ operation DEFINES is fine, because its store is anchored after the run rather
\ than inside it.
\
\ A VALUE DEFINED OR READ OUTSIDE TWO BLOCKS. The frame forms thread a memory
\ order, and that order has to be read exactly once on every run: two blocks that
\ both reach the frame, where one can be reached from the other, are two readers
\ of one order on one path. The pair of blocks that never has that problem is the
\ one the prologue already uses - the block the caller enters and the block
\ control leaves through. The first dominates the second, and every run passes
\ through both in that order, so their frame accesses read in that order are the
\ order every run makes them in. A store or a load anywhere else is refused
\ rather than placed where the order cannot be stated (dot
\ habu-spill-from-a-4145325c).
: KEEP! ( n -- )
   UF-FIND {: r:n :}
   1 r cells CL-KEEP + ! ;

: KEEP? ( n -- bool )
   cells CL-KEEP + @ 0<> ;

: MB-KIND-CLEAR ( -- )
   VMAX 0 ?do
      NOSLOT i cells CL-SLOT + !
      NOPOS i cells CL-DEF + !
      NOPOS i cells CL-ANCH + !
      0 i cells CL-SIZE + !
      0 i cells CL-KEEP + !
      NOBODY i cells CL-FIX + !
      NOBODY i cells CL-WANT + !
   loop ;

: MB-SIZES ( -- )
   N-VALS @ 0 ?do
      i UF-FIND {: r:n :}
      r cells CL-SIZE + @ 1+  r cells CL-SIZE + !
   loop ;

: MB-KEEP-OP ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id OPERANDS-OF 0 ?do id i OPERAND-AT SLOT KEEP! loop
   id RESULTS-OF 0 ?do  id i RESULT-AT  SLOT KEEP! loop ;

: MB-KEEP-READS ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id OPERANDS-OF 0 ?do id i OPERAND-AT SLOT KEEP! loop ;

: MB-KEEP-BLOCK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk ARG-COUNT 0 ?do bk i ARG-AT SLOT KEEP! loop
   b 0= b RET-B @ = or 0= if
      bk OP-COUNT 0 ?do bk i OP-AT MB-KEEP-OP loop
      exit
   then
   bk OP-COUNT 0 ?do
      bk i OP-AT {: id:IR-ID:ir-op-id :}
      id DSTACK-TOUCH? if id MB-KEEP-READS then
   loop ;

: MB-SPILLABLE? ( n -- bool )
   {: r:n :}
   r cells CL-SLOT + @ NOSLOT <> if false exit then
   r cells CL-SIZE + @ 1 <> if false exit then
   r KEEP? if false exit then
   r CLS-AT C-TOKEN = if false exit then
   true ;

\ ---- reading the linear order backwards --------------------------------------
\ The layout gave every block one position for its arguments and one per
\ operation, so a global position names a block and, unless it is the block's
\ own first position, one operation of it.
: POS-BLOCK ( n -- n )
   {: p:n :}
   -1
   N-BLKS @ 0 ?do
      p i cells B-ST + @ >=  p i cells B-EN + @ <=  and if drop i leave then
   loop
   dup 0 < if E-A64RA-SHAPE throw then ;

: POS-OP? ( n -- bool )
   {: p:n :}
   p POS-BLOCK cells B-ST + @ p <> ;

: POS-OP ( IR-ID:ir-fun-id n -- IR-ID:ir-op-id )
   {: f:IR-ID:ir-fun-id p:n :}
   p POS-BLOCK {: b:n :}
   f b BLOCK-AT  p  b cells B-ST + @ -  1-  OP-AT ;

: MB-READS? ( IR-ID:ir-fun-id n n -- bool )
   {: f:IR-ID:ir-fun-id r:n p:n :}
   p POS-OP? 0= if false exit then
   f p POS-OP {: id:IR-ID:ir-op-id :}
   false
   id OPERANDS-OF 0 ?do
      id i OPERAND-AT SLOT UF-FIND r = if drop true leave then
   loop ;

: MB-DEFS? ( IR-ID:ir-fun-id n n -- bool )
   {: f:IR-ID:ir-fun-id r:n p:n :}
   p POS-OP? 0= if false exit then
   f p POS-OP {: id:IR-ID:ir-op-id :}
   false
   id RESULTS-OF 0 ?do
      id i RESULT-AT SLOT UF-FIND r = if drop true leave then
   loop ;

: MB-TOUCHES? ( IR-ID:ir-fun-id n n -- bool )
   {: f:IR-ID:ir-fun-id r:n p:n :}
   f r p MB-READS? if true exit then
   f r p MB-DEFS? ;

\ The position of the first operation at or after `from` that reads a member of
\ this class. A class nothing reads again answers the position past the last one,
\ so furthest-next-use puts the classes nobody wants last without a second rule
\ for them.
: MB-NEXT-USE ( IR-ID:ir-fun-id n n -- n )
   {: f:IR-ID:ir-fun-id r:n from:n :}
   MB-AT @ {: n:n :}
   n
   n from 0 max ?do
      f r i MB-READS? if drop i leave then
   loop ;

\ The operation a store for a value written here is anchored to, as a position of
\ the linear order. It is the first operation after the definition that is not
\ part of a data-stack run - see the plan section below for why - and it is
\ computed here as well because the scan has to know how long the register the
\ definition wrote is still needed.
: MB-ANCHOR ( IR-ID:ir-block-id n -- n )
   {: bk:IR-ID:ir-block-id at:n :}
   bk OP-COUNT {: n:n :}
   n
   n at 1+ ?do
      bk i OP-AT DSTACK-TOUCH? 0= if drop i leave then
   loop ;

: MB-DEF-POS ( IR-ID:ir-fun-id n -- n )
   {: f:IR-ID:ir-fun-id r:n :}
   -1
   MB-AT @ 0 ?do
      f r i MB-DEFS? if drop i leave then
   loop ;

: MB-ANCH-POS ( IR-ID:ir-fun-id n -- n )
   {: f:IR-ID:ir-fun-id p:n :}
   p POS-BLOCK {: b:n :}
   f b BLOCK-AT  p  b cells B-ST + @ -  1-  MB-ANCHOR {: k:n :}
   b k OP-POS ;

\ ---- the scan ----------------------------------------------------------------
: MB-EXPIRE1 ( n n n -- )
   {: fl:n r:n limit:n :}
   fl r HOLD-AT {: v:n :}
   v NOBODY = if exit then
   v cells CL-HI + @ limit < if NOBODY fl r HOLD! then ;

: MB-EXPIRE ( n -- )
   {: limit:n :}
   FILES-N 0 ?do
      REGS-N 0 ?do j i limit MB-EXPIRE1 loop
   loop ;

\ How many registers of ONE file are free here. It is per file because a class
\ that wants a floating register is not served by a free general one - and
\ because two classes of ONE file are served by the same free registers, so the
\ count belongs to the file that holds them and not to either class.
: MB-FREE-N ( n -- n )
   {: fl:n :}
   0
   REGS-N 0 ?do
      fl i POOL-HAS? fl i HOLD-AT NOBODY = and if 1+ then
   loop ;

\ ---- what a class already in the frame still costs in registers ---------------
\ A class in a frame slot has left the scan's holder table, and it still needs
\ real registers at some positions: one for the load in front of every operation
\ that reads it, and one for the stretch between the operation that WRITES it and
\ the store that takes it away. Counting them is what makes this scan a statement
\ about the module the lowering pass will build rather than about the one it is
\ reading.
\
\ ONE POSITION IS TWO INSTANTS, AND THEY ARE COUNTED APART. Everything at one
\ position is emitted in one order: the stores the operations before it earned,
\ then the loads this operation needs, then the operation itself, which reads and
\ then writes. So a load standing in front of an operation has been read and its
\ register given back before that operation's own results are written, and a load
\ and a definition at one position may therefore be ONE register. Adding the two
\ together would refuse a routine that fits - a chain that reloads a value into
\ the register its own result is about to take is the commonest shape there is -
\ so the two instants are counted separately and each is held against the
\ registers free at that instant.
\
\ THE STRETCH STOPS SHORT OF THE ANCHOR ITSELF, and starts at the definition. At
\ the anchor the pending stores have already given their registers back before a
\ load or a result asks for one. At the definition the register is written, so it
\ belongs to the writing instant; strictly between the two it is held across the
\ whole position and belongs to both.
: MB-ACROSS? ( n n -- bool )
   {: r:n p:n :}
   p  r cells CL-DEF + @  >   p  r cells CL-ANCH + @  <  and ;

: MB-WRITTEN? ( n n -- bool )
   {: r:n p:n :}
   p  r cells CL-DEF + @  = ;

\ Is this class one of THIS file's that the walk already put in a frame slot? The
\ demand a position makes on a file is the demand of every class that file holds,
\ so the question is asked of the file: two classes sharing one file compete for
\ the same free registers and their loads have to be counted together.
: MB-FRAMED? ( n n -- bool )
   {: r:n fl:n :}
   r UF-FIND r =  r FILE-AT fl =  and
   r cells CL-SLOT + @ NOSLOT <>  and ;

\ What the frame's classes need while this position READS: every load in front of
\ it, plus every class held across it.
: MB-LOAD-N ( IR-ID:ir-fun-id n n -- n )
   {: f:IR-ID:ir-fun-id p:n fl:n :}
   0
   N-VALS @ 0 ?do
      i fl MB-FRAMED? if
         i p MB-ACROSS?  f i p MB-READS? or if 1+ then
      then
   loop ;

\ And what they need while it WRITES: every class this position puts in a
\ register, plus the same held-across ones.
: MB-STORE-N ( n n -- n )
   {: p:n fl:n :}
   0
   N-VALS @ 0 ?do
      i fl MB-FRAMED? if
         i p MB-ACROSS?  i p MB-WRITTEN? or if 1+ then
      then
   loop ;

\ Where the pool ran short, and in WHICH file. The file is recorded beside the
\ position because the class that has to go in the frame has to be one of that
\ file: putting a double away frees a floating register and does nothing at all
\ for a routine that ran out of general ones.
: MB-SHORT! ( n n -- )
   {: p:n fl:n :}
   SHORT-AT @ 0 < if p SHORT-AT !  fl SHORT-FILE ! then ;

\ ---- which registers one class may not have ----------------------------------
\ The registers destroyed by the calls this class's live range crosses. A value
\ defined at the call itself, or last read by it, does not cross it: the store
\ run in front of a call and the load run behind it are ordinary operations of
\ this module and the values they move are dead, or not yet alive, at the branch.
\ So the range is open at both ends. A class live from before a loop to after it
\ crosses every call inside that loop, which is exactly the shape a local read
\ after a loop of calls has, and a routine that calls nothing answers no bits for
\ any class at all.
\
\ AND IT IS ASKED OF THE MEMBERS AND NOT OF THE CLASS'S OWN HULL. A class holds
\ one register for the whole stretch from its first definition to its last read,
\ and that stretch is wider than any one member's: the values a call site saves
\ and the values it reads back afterwards are joined into one class by the edges
\ around the call, so the CLASS spans every call while no MEMBER of it does. A
\ register barred by the class's hull would be barred for a value that is in a
\ data-stack slot at the branch, which would refuse programs that fit before any
\ of this existed. What really loses a register at a call is a member whose own
\ definition is before the branch and whose own last read is after it, so that is
\ the question, asked of each member.
: MB-CROSSES? ( n n -- bool )
   {: r:n p:n :}
   false
   N-VALS @ 0 ?do
      i UF-FIND r = if
         i DEF-AT p <  i LAST-AT p >  and or
      then
   loop ;

: MB-FORBID ( IR-ID:ir-fun-id n -- n )
   {: f:IR-ID:ir-fun-id r:n :}
   r FILE-AT {: fl:n :}
   0
   MB-AT @ 0 ?do
      i POS-OP? if
         f i POS-OP CALL-AT? if
            r i MB-CROSSES? if
               f i POS-OP fl CALL-BITS or
            then
         then
      then
   loop ;

\ Is this class one the scan still has to place here? Its hull has to start at
\ this position - a class is given one register once, over the whole hull - it has
\ to be one that lives in a register at all, and it has to be one the walk has not
\ already put in a frame slot.
: MB-DUE? ( n n -- bool )
   {: r:n pos:n :}
   r cells CL-LO + @ pos <> if false exit then
   r CLS-AT C-TOKEN = if false exit then
   r cells CL-SLOT + @ NOSLOT = ;

\ A class the contract pins to one register arrives in exactly that one, and
\ three things make that impossible rather than merely awkward: a register the
\ routine may not write, one a call inside the class's own range destroys, and one
\ already held. The first is unreachable here because FIXED-POOL-CK refused it
\ before a value was placed, and it is still asked because TAKE would answer it
\ under E-A64RA-POOL, which is a different statement. The third is fail-closed:
\ every pinned class is an argument of the entry block, they are placed before
\ anything else at that position, and one register is one convention position.
: MB-PIN ( IR-ID:ir-fun-id n n -- )
   {: f:IR-ID:ir-fun-id r:n want:n :}
   r FILE-AT {: fl:n :}
   fl want POOL-HAS? 0= if E-A64RA-FIXED throw then
   f r MB-FORBID want FORBIDDEN? if E-A64RA-FIXED throw then
   fl want HOLD-AT NOBODY <> if E-A64RA-FIXED throw then
   r want TAKE ;

\ The register the contract says a returned class leaves in, when the walk can
\ give it that one for nothing: it has to be free where the class is written and
\ not destroyed by a call the class crosses. That is what makes an ordinary
\ declared return emit no instruction at all. When it is not free the class is
\ placed like any other and MB-RETURN-CK plans the copy - unless what holds it is
\ ANOTHER class the same return has to deliver, which is a parallel copy this pass
\ does not have (dot habu-lower-parallel-copies-cdf9720e) and is refused.
: MB-WANTED ( IR-ID:ir-fun-id n n -- n )
   {: f:IR-ID:ir-fun-id r:n forbid:n :}
   r cells CL-WANT + @ {: want:n :}
   want NOBODY = if -1 exit then
   forbid want FORBIDDEN? if -1 exit then
   r FILE-AT want HOLD-AT {: held:n :}
   held NOBODY = if want exit then
   held cells CL-WANT + @ NOBODY <> if E-A64RA-FIXED throw then
   -1 ;

: MB-PLACE1 ( IR-ID:ir-fun-id n n -- )
   {: f:IR-ID:ir-fun-id r:n pos:n :}
   r pos MB-DUE? 0= if exit then
   r cells CL-FIX + @ {: fix:n :}
   fix NOBODY <> if f r fix MB-PIN exit then
   f r MB-FORBID {: forbid:n :}
   f r forbid MB-WANTED {: w:n :}
   w 0 >= if r w TAKE exit then
   r FILE-AT forbid FREE-REG {: g:n :}
   g 0 < if pos r FILE-AT MB-SHORT! exit then
   r g TAKE ;

: MB-READ-PRESSURE ( IR-ID:ir-fun-id n n -- )
   {: f:IR-ID:ir-fun-id pos:n fl:n :}
   f pos fl MB-LOAD-N  fl MB-FREE-N > if pos fl MB-SHORT! then ;

: MB-WRITE-PRESSURE ( n n -- )
   {: pos:n fl:n :}
   pos fl MB-STORE-N  fl MB-FREE-N > if pos fl MB-SHORT! then ;

\ Both instants are measured of every file, and the walk stops at the first one
\ that ran short - the scan restarts after a spill, so measuring the rest would
\ record nothing MB-SHORT! keeps. Written as a loop over the files rather than a
\ line per file, so a file added to the map is measured without this being
\ touched.
: MB-READ-PRESSURE-ALL ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id pos:n :}
   FILES-N 0 ?do
      SHORT-AT @ 0 >= if leave then
      f pos i MB-READ-PRESSURE
   loop ;

: MB-WRITE-PRESSURE-ALL ( n -- )
   {: pos:n :}
   FILES-N 0 ?do
      SHORT-AT @ 0 >= if leave then
      pos i MB-WRITE-PRESSURE
   loop ;

\ The classes due here, pinned ones first. The order matters for exactly one
\ shape and it is the commonest one there is: the entry block's arguments are all
\ written at the same position, so a class with no constraint would otherwise be
\ handed the lowest free register and a pinned class named that same register
\ would find it taken. Placing the pinned ones first is the statement "the caller
\ decided these" rather than a numbering that happens to work.
: MB-PLACE-PINNED ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id pos:n :}
   N-VALS @ 0 ?do
      i UF-FIND i =  i cells CL-FIX + @ NOBODY <> and if f i pos MB-PLACE1 then
   loop ;

: MB-PLACE-REST ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id pos:n :}
   N-VALS @ 0 ?do
      i UF-FIND i =  i cells CL-FIX + @ NOBODY = and if f i pos MB-PLACE1 then
   loop ;

\ One position, in the order the machine runs it. A class whose last read is HERE
\ still holds its register while this operation reads, so the reading instant is
\ measured before it is expired and the writing instant after - which is the same
\ read-then-write boundary that lets a result land in a register its own operand
\ has just vacated.
: MB-STEP ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id pos:n :}
   pos MB-EXPIRE
   f pos MB-READ-PRESSURE-ALL
   SHORT-AT @ 0 >= if exit then
   pos 1+ MB-EXPIRE
   f pos MB-PLACE-PINNED
   f pos MB-PLACE-REST
   SHORT-AT @ 0 >= if exit then
   pos MB-WRITE-PRESSURE-ALL ;

: MB-SCAN ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   -1 SHORT-AT !
   F-GPR SHORT-FILE !
   HOLDERS-CLEAR
   MB-AT @ 0 ?do
      SHORT-AT @ 0 < if f i MB-STEP then
   loop ;

\ ---- taking a class out of the registers -------------------------------------
: MB-HELD? ( n -- bool )
   {: r:n :}
   r FILE-AT {: fl:n :}
   false
   REGS-N 0 ?do fl i HOLD-AT r = if drop true leave then loop ;

\ A class the scan could take a register from here. It has to hold one, it has to
\ be one this pass may put in the frame, and this position must not touch it: a
\ class the operation here reads would need a load at once, which puts the same
\ demand back.
: MB-CANDIDATE? ( IR-ID:ir-fun-id n n -- bool )
   {: f:IR-ID:ir-fun-id r:n p:n :}
   r MB-HELD? 0= if false exit then
   f r p MB-TOUCHES? if false exit then
   r MB-SPILLABLE? ;

\ How many classes hold a register here that this position does not touch. A
\ position with none of them is the one register pressure no spill can serve:
\ every register holds something the operation itself needs.
: MB-SPARE-N ( IR-ID:ir-fun-id n n -- n )
   {: f:IR-ID:ir-fun-id p:n fl:n :}
   0
   REGS-N 0 ?do
      fl i HOLD-AT {: r:n :}
      r NOBODY <> if
         f r p MB-TOUCHES? 0= if 1+ then
      then
   loop ;

: MB-FURTHEST ( IR-ID:ir-fun-id n n -- n )
   {: f:IR-ID:ir-fun-id p:n fl:n :}
   -1
   REGS-N 0 ?do
      fl i HOLD-AT {: r:n :}
      r NOBODY <> if
         f r p MB-CANDIDATE? if
            f r p 1+ MB-NEXT-USE {: c:n :}
            c over > if drop c then
         then
      then
   loop ;

\ The class that loses its registers: the one whose next read is furthest away,
\ with the lowest register number breaking a tie, so one program always spills
\ the same way and a fixture can assert which value went into the frame. When
\ nothing can be taken the refusal says which of the two walls was hit - every
\ register needed by this one operation, or nothing here that may go in a frame.
: MB-VICTIM ( IR-ID:ir-fun-id n n -- n )
   {: f:IR-ID:ir-fun-id p:n fl:n :}
   f p fl MB-FURTHEST {: want:n :}
   want 0 < if
      f p fl MB-SPARE-N 0= if E-A64RA-POOL throw then
      E-A64RA-SPILL throw
   then
   -1
   REGS-N 0 ?do
      fl i HOLD-AT {: r:n :}
      r NOBODY <> if
         f r p MB-CANDIDATE? if
            f r p 1+ MB-NEXT-USE want = if drop r leave then
         then
      then
   loop
   dup 0 < if E-A64RA-SPILL throw then ;

: MB-EVICT ( IR-ID:ir-fun-id n n -- )
   {: f:IR-ID:ir-fun-id p:n fl:n :}
   f p fl MB-VICTIM {: r:n :}
   NEW-SLOT r cells CL-SLOT + !
   f r MB-DEF-POS {: d:n :}
   d r cells CL-DEF + !
   f d MB-ANCH-POS  r cells CL-ANCH + ! ;

\ Scan, and when the pool ran short somewhere, put one class in the frame and
\ scan again. Putting a class away only ever frees registers, so a later scan
\ runs short no earlier than the one before it, and every round takes one more
\ class - so this stops either when the whole routine fits or when there is no
\ class left to take and the refusal above says why.
: MB-FIT ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   begin
      f MB-SCAN
      SHORT-AT @ 0 <
      dup 0= if drop f SHORT-AT @ SHORT-FILE @ MB-EVICT false then
   until ;

\ Every value takes the register its class was given, or the slot its class was
\ given. A memory token is in no class that holds either and takes neither.
: MB-FINISH ( -- )
   N-VALS @ 0 ?do
      i CLS-AT C-TOKEN = if
         NOBODY i REG!
      else
         i UF-FIND {: r:n :}
         r cells CL-SLOT + @ {: s:n :}
         s NOSLOT = if
            r REG-AT i REG!
         else
            NOBODY i REG!
            s i SLOT!
         then
      then
   loop ;

\ ---- the decisions, anchored to their blocks ---------------------------------
\ A store goes in front of the first operation after the one that defines the
\ value that is not part of a data-stack run. As early as that is right on every
\ run - SSA puts a definition before every read of it, so a store there has
\ happened before any load of that slot can be reached - and no earlier, because
\ the entry sequence, the exit sequence and a call site are contiguous shapes the
\ validator measures and an operation inside one breaks the shape. A load goes in
\ front of each operation that reads the value, one per operation however many of
\ its operands name it; the values a data-stack operation reads are not spilled
\ at all, which is why a load never has to go inside a run. MB-ANCHOR above is
\ the one statement of where a store goes; the scan reads it too, to know how
\ long the register the definition wrote is still needed.
: MB-PLAN-STORES ( IR-ID:ir-block-id n n n -- )
   {: bk:IR-ID:ir-block-id b:n at:n d:n :}
   bk d OP-AT {: id:IR-ID:ir-op-id :}
   id RESULTS-OF 0 ?do
      id i RESULT-AT SLOT {: k:n :}
      k SLOT-AT NOSLOT <> if b P-STORE at k PLAN+ then
   loop ;

: MB-PLAN-LOADS ( IR-ID:ir-block-id n n -- )
   {: bk:IR-ID:ir-block-id b:n at:n :}
   bk at OP-AT {: id:IR-ID:ir-op-id :}
   id OPERANDS-OF 0 ?do
      id i OPERAND-AT SLOT {: k:n :}
      k SLOT-AT NOSLOT <>  b k at RELOADED? 0=  and if
         b P-RELOAD at k PLAN+
      then
   loop ;

\ A value defined by an operation with no anchor behind it inside its own block -
\ the terminator, or the last of a data-stack run that reaches the terminator -
\ has nowhere for its store. No terminator of this dialect answers a value and no
\ data-stack run reaches one, so this is fail-closed rather than reachable.
: MB-PLAN-TAIL-CK ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT {: n:n :}
   n 0 ?do
      bk i MB-ANCHOR n = if
         bk i OP-AT {: id:IR-ID:ir-op-id :}
         id RESULTS-OF 0 ?do
            id i RESULT-AT SLOT SLOT-AT NOSLOT <> if E-A64RA-SPILL throw then
         loop
      then
   loop ;

\ Every returned value the contract named a register for is in it where control
\ leaves, or the walk plans the move that puts it there. It is decided after the
\ whole scan, because it is a statement about where the values ARE at the return:
\ a class that spent its life in a frame slot is in the register its load landed
\ in, not one it was ever computed in, and only the finished scan knows which
\ classes those are.
\
\ THE ROWS GO IN LAST AND THAT IS WHERE THEY BELONG. Everything at one anchor is
\ emitted in the order the plan records it, so a copy planned after the loads of
\ the same operation reads the value as the loads left it - which is the whole
\ reason a spilled returned value composes with a declared register at all.
: MB-PLAN-MOVES ( IR-ID:ir-block-id -- )
   {: rb:IR-ID:ir-block-id :}
   rb TERM-AT {: id:IR-ID:ir-op-id :}
   rb OP-COUNT 1- {: at:n :}
   OUTS-N @ 0 ?do
      id i OPERAND-AT SLOT {: k:n :}
      k REG-AT  i cells O-REG + @  <> if
         RET-B @ P-MOVE at k PLAN+
      then
   loop ;

: MB-PLAN-BLOCK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT 0 ?do
      i {: at:n :}
      at 0 ?do
         bk i MB-ANCHOR at = if bk b at i MB-PLAN-STORES then
      loop
      bk b at MB-PLAN-LOADS
   loop
   bk MB-PLAN-TAIL-CK
   b RET-B @ = if bk MB-PLAN-MOVES then ;

\ The rows in the order the lowering pass reads them: blocks in the module's own
\ order, operations in the block's, and at one operation the store the operation
\ before it earned in front of the loads this one needs - so the register the
\ store gives back is free for the load that follows it.
: MB-PLAN ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   N-BLKS @ 0 ?do f i MB-PLAN-BLOCK loop ;

\ The ordinal of the block control leaves the routine through: the one whose
\ terminator names no successor. A routine with none never returns and one with
\ two returns twice, and neither is a shape this pass can decide a convention
\ against. It is also where the frame accesses may stand beside the block the
\ caller enters.
: MB-RET-ORD ( IR-ID:ir-fun-id -- n )
   {: f:IR-ID:ir-fun-id :}
   -1
   f BLOCK-COUNT 0 ?do
      f i BLOCK-AT TERM-AT SUCCS-OF 0= if
         dup 0 < 0= if E-A64RA-SHAPE throw then
         drop i
      then
   loop
   dup 0 < if E-A64RA-SHAPE throw then ;

: MB-RUN ( IR-ID:ir-fun-id IR-ID:ir-block-id IR-ID:ir-block-id -- )
   {: f:IR-ID:ir-fun-id bk:IR-ID:ir-block-id rb:IR-ID:ir-block-id :}
   f MB-LAYOUT
   f MB-LIVENESS
   UF-INIT
   f MB-RANGES
   bk MB-FIX!
   rb MB-WANT!
   N-BLKS @ 0 ?do f i MB-EDGES-OF loop
   f MB-TIES
   f MB-COALESCE
   MB-CLASSES
   MB-KIND-CLEAR
   MB-SIZES
   MB-DECLS!
   N-BLKS @ 0 ?do f i MB-KEEP-BLOCK loop
   f MB-FIT
   MB-FINISH
   f MB-PLAN ;

\ ---- what one allocation run is told -----------------------------------------
\ The one function this pass allocates. A module with any other shape is not a
\ routine at all.
: FUN-OF ( -- IR-ID:ir-fun-id )
   FUN-COUNT 1 <> if E-A64RA-SHAPE throw then
   MKEY 0 IR-ID:PACK-FUN ;

\ ---- the contract, read once -------------------------------------------------
\ A contract is a twelve-field value and a value of more than one cell cannot be
\ bound to a local, so a word that needs two of its fields takes them apart once
\ and a word that needs the whole contract again builds it back. Both readers
\ below revalidate what they are handed, which is what makes rebuilding it safe:
\ a forged contract cannot survive the round trip.
\ Every slot this walk handed out, measured against the routine that has to
\ address it. A64EFF owns that rule - a width its forms carry, an offset its
\ scale division will not round, an offset inside the frame and inside the reach
\ of the offset field - so the decision is made there and not here.
\
\ THE FRAME IT IS MEASURED AGAINST IS THE ONE THIS WALK DERIVED and not the one
\ the contract arrived with, because the contract's is a caller's guess this pass
\ no longer honours (see NEW-SLOT). What survives the substitution is every rule
\ that is about the machine rather than about the guess: an offset the scale
\ division will not round and one past the reach of the offset field are still
\ refused here, under A64EFF's name, before anything acts on the assignment. That
\ the offsets lie inside the derived frame is true by construction, and it is
\ checked again where it is not - src/compiler/native/regalloc-verify.f measures
\ them against the contract the routine is actually emitted under.
: SLOTS-CK ( A64EFF:routine -- )
   A64EFF:VALIDATE A64EFF-ROUTINE:UNMAKE
   {: cv:A64EFF:conv gi:A64EFF:placeseq gr:A64EFF:placeseq gc:A64EFF:gprs
      fi:A64EFF:fprs fr:A64EFF:fprs fc:A64EFF:fprs
      z:A64EFF:nzcv l:A64EFF:link ct:A64EFF:control t:A64EFF:traits
      size:n delta:n :}
   FRAME-WANT {: want:n :}
   N-SLOTS @ 0 ?do
      BASE-N @  i A64IR:SLOT-WIDTH *  +  A64IR:SLOT-WIDTH
      cv gi gr gc fi fr fc z l ct t want delta A64EFF-ROUTINE:MAKE
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
\ Learn the identity of the module that is about to be allocated, the type of its
\ general register, and the name of its register-to-register copy, while it is
\ still being built. A module's types and symbols are its own ordinals, so this
\ is the only moment the dialect can be asked any of them; the answers stay valid
\ after the module freezes because freezing keeps the module's identity. The copy
\ is asked for because coalescing has to recognise one - see step five below -
\ and asking A64IR rather than spelling "a64.mov" again here is what keeps the
\ dialect the only authority on its own names. The binding is spent by the next
\ ALLOCATE.
: BIND-DIALECT ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   BND-MODE @ BOUND-YES = if E-A64RA-BIND throw then
   c b DIALECT-CK
   b IR-BUILD:MODULE@ 0 BND-MOD !
   c b A64IR:GPR-TYPE 0 BND-TYP !
   c b A64IR:FPR-TYPE 0 BND-FPR !
   c b A64IR:MEM-TYPE 0 BND-MEM !
   c b A64IR:KEY-SLOT 0 BND-SLOT !
   c b A64IR:KEY-DSLOT  DK-SLOT BND-DKEY !
   c b A64IR:KEY-DBYTES DK-BYTES BND-DKEY !
   c b A64IR:KEY-DBACK  DK-BACK BND-DKEY !
   c b A64IR:KEY-ENTRY  0 BND-ENTRY !
   c b A64IR-OPCODE:MOV A64IR:OPCODE 0 BND-MOV !
   BOUND-YES BND-MODE ! ;

\ Whether a binding is live, for a caller cleaning up after a refused run. See
\ src/compiler/native/select.f BOUND? for why each pass answers for itself.
: BOUND? ( -- bool )
   BND-MODE @ BOUND-YES = ;

\ Give up a binding without allocating against it.
: RELEASE ( -- )
   BND-TAKE ;

\ ---- the pass ----------------------------------------------------------------
\ Allocate the whole of one frozen machine module against the contract of the
\ routine it is being emitted as. The contract's destroyed set is the pool, so a
\ routine that may destroy nothing allocates nothing; the walk seals a claim per
\ value, which src/compiler/native/regalloc-verify.f then has to accept before
\ anything may act on it.
\ The frame the contract declares is NOT among what this takes, and that is the
\ change habu-derive-a-routine-84ed36b6 made: the walk decides how much frame the
\ routine needs, so a declaration handed in could only be a guess to be held to.
\ FRAME below answers what the walk decided, and the caller declares that.
: WALK ( IR-CTX:ctx IR-BUILD:module A64EFF:gprs A64EFF:fprs A64EFF:conv A64EFF:placeseq A64EFF:placeseq A64EFF:traits -- )
   {: c:IR-CTX:ctx m:IR-BUILD:module pool:A64EFF:gprs fpool:A64EFF:fprs
      cv:A64EFF:conv args:A64EFF:placeseq outs:A64EFF:placeseq
      traits:A64EFF:traits :}
   BND-TAKE
   ST-EMPTY ST !
   m BND-MODULE-CK
   c TARGET-CK
   pool 0 S-POOL !
   fpool 0 S-FPOOL !
   traits A64FRAME:SPILL-BASE BASE-N !
   m VIEWS!
   m IR-BUILD:FMODULE 0 S-MOD !
   TABLES-CLEAR
   args outs FIXED!
   FIXED-POOL-CK
   FUN-OF {: f:IR-ID:ir-fun-id :}
   f MB-RET-ORD RET-B !
   f 0 BLOCK-AT {: bk:IR-ID:ir-block-id :}
   f RET-B @ BLOCK-AT {: rb:IR-ID:ir-block-id :}
   bk rb FIXED-ARITY-CK
   bk rb cv LOWERED-CK
   f bk rb MB-RUN ;

: ALLOCATE ( IR-CTX:ctx IR-BUILD:module A64EFF:routine -- )
   A64EFF:VALIDATE A64EFF-ROUTINE:UNMAKE
   {: cv:A64EFF:conv gi:A64EFF:placeseq gr:A64EFF:placeseq gc:A64EFF:gprs
      fi:A64EFF:fprs fr:A64EFF:fprs fc:A64EFF:fprs
      z:A64EFF:nzcv l:A64EFF:link ct:A64EFF:control
      t:A64EFF:traits size:n delta:n :}
   cv gi gr gc fi fr fc z l ct t size delta A64EFF-ROUTINE:MAKE
   A64EFF:GPR-WRITABLE {: pool:A64EFF:gprs :}
   cv gi gr gc fi fr fc z l ct t size delta A64EFF-ROUTINE:MAKE
   A64EFF:FPR-WRITABLE {: fpool:A64EFF:fprs :}
   pool fpool cv gi gr t WALK
   cv gi gr gc fi fr fc z l ct t size delta A64EFF-ROUTINE:MAKE SLOTS-CK
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

\ The other file's pool, answered separately for the same reason it is held
\ separately: the validator compares each one against the contract field it came
\ from, and one answer covering both could not.
: FPOOL ( -- A64EFF:fprs )
   SEAL-CK 0 S-FPOOL @ ;

\ The frame this walk proved its routine needs, and how deep into it the walk
\ reached. The second is what the prologue owns plus every slot handed out; the
\ first is that rounded to what the stack pointer may be moved by, which is what
\ the routine has to DECLARE to hold it.
\
\ THE FIRST IS THIS PASS'S ANSWER AND NOT A RESTATEMENT OF ITS QUESTION. It used
\ to be the frame the contract arrived with - a caller's guess the walk was held
\ to, so a routine whose author guessed low was refused rather than given the
\ frame its program needs. It is now derived from what the walk decided, which is
\ what habu-derive-a-routine-84ed36b6 asked for: a caller allocates, reads this,
\ and declares exactly it. src/compiler/native/regalloc-verify.f then holds the
\ emitted routine to that declaration, so the number below is checked against the
\ program rather than believed.
: FRAME ( -- n )
   SEAL-CK FRAME-WANT ;

: FRAME-USED ( -- n )
   SEAL-CK DEPTH-WANT ;

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

\ The block the row's operation is in, and the operation's index inside it. Two
\ readers rather than one number, because the lowering pass rebuilds one block at
\ a time and asks both questions in different places.
: PLAN-BLOCK@ ( n -- n )
   SEAL-CK PLAN-ORD-CK cells PL-BLK + @ ;

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
