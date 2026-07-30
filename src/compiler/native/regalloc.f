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
\ THE MOVE-WIDE OVERWRITE IS A TIED OPERAND. Movk writes one sixteen-bit half of
\ a register and keeps the other three, so in the encoding its source and its
\ destination are one register field. In SSA those are two values, and the
\ allocator has to put them in the same physical register or the instruction
\ means something else. It is handled here as what it is - a tie - and not as a
\ coincidence: the result takes the operand's register, and a program in which
\ the kept value is still needed after the overwrite is refused by name, because
\ one register field cannot hold a value that must survive and a value that
\ replaces it. That the tie belongs to movk is a fact of the dialect; today this
\ pass learns which opcode movk is from the dialect itself (see the binding
\ below), and the destination is a tie declared in the operation schema, so an
\ allocator can read a constraint instead of knowing a name.
\
\ WHICH REGISTERS MAY BE USED, AND WHY THERE IS NO LIST OF THEM HERE. The
\ routine's own contract says which general registers it may destroy, and a value
\ living in a register is exactly the routine destroying it. The allocatable pool
\ is therefore that declared set and nothing else. There is no literal list of
\ register numbers in this file: x18, x30 and register 31 are excluded because
\ src/compiler/a64-effect.f refuses them in any general-register set at all, so
\ no contract that names one can be built, and a forged contract is rejected when
\ this pass revalidates it. A reserved register is out of reach here by
\ construction rather than by a check that could be forgotten.
\
\ WHY RUNNING OUT OF REGISTERS IS A REFUSAL AND NOT A SPILL. A straight-line
\ block can hold more values at once than any register file has - a long chain of
\ literals proves it - so the bound cannot be proved away and spilling is the
\ real answer. The real answer cannot be built yet: spilling means writing a
\ value to a frame slot and reading it back, and the A64IR dialect has no store,
\ no load and no frame-slot operand, so an allocation that said "this value lives
\ in a slot" would name an instruction nothing can emit. This pass therefore
\ refuses the program by name, exactly as the selector refuses trapping
\ arithmetic that has no machine lowering. The missing capability is dot
\ habu-lower-spills-and-ef14a0dd; when the dialect can express a spill and a
\ reload, the refusal becomes a decision.
\
\ NO FIXED CONSTRAINTS, AND WHY THERE CANNOT BE ANY YET. Design section 7.9 also
\ asks for pre-coloured intervals, and there are none here because nothing can
\ yet say that a value must be in a named register: a Habu word takes its inputs
\ and publishes its outputs through data-stack slots, and this dialect has no
\ load, no store and no way to name the data-stack pointer. A block argument is
\ therefore a value that has to be somewhere, and it is given the next free
\ register of the routine's own set. The seam is dot habu-bind-arm64-arg-f76afa3a.
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
require src/compiler/ir/arena.f
require src/compiler/ir/op.f
require src/compiler/ir/fun.f
require src/compiler/ir/build.f
require src/compiler/native/a64ir.f

package A64RA
private

\ ---- the bound dialect -------------------------------------------------------
\ A module's symbols and types are its own ordinals, so "is this operation a
\ move-wide overwrite" and "is this value a general register" cannot be answered
\ from outside without either the dialect's own authority or a restatement of its
\ spellings. Restating them would be a second authority that drifts, so this pass
\ asks A64IR itself while the module is still being built, and keeps the
\ identities it answers. One slot per member of the operation family keeps the
\ family exhaustive: a member added to A64IR:opcode makes this fail to compile
\ until it has a slot.
6 constant OPCODES-N
0 constant O-MOVZ
1 constant O-MOVK
2 constant O-ADD
3 constant O-SUB
4 constant O-MUL
5 constant O-RET

0 constant BOUND-NO
1 constant BOUND-YES

\ ---- how much of one block this pass holds -----------------------------------
\ Values in one block. The selector carries the same ceiling, so a module it
\ produced always fits; a block that wants more is a capability to raise in both,
\ not a ceiling to widen silently.
256 constant VMAX

\ The register file, taken from the schema that owns the machine facts.
A64EFF:FILE-SIZE constant REGS-N

\ The position of a block argument: before every operation of the block.
-1 constant ENTRY

\ Nothing holds this register.
-1 constant NOBODY

\ ---- the frozen tables of the module being read ------------------------------
5 constant VIEWS-N
0 constant V-OPP                     \ operation pool
1 constant V-OPR                     \ operation rows
2 constant V-VALR                    \ value rows
3 constant V-FUNR                    \ function rows
4 constant V-BLKR                    \ block rows

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

1 TYPED-BUFFER BND-MOD IR-ID:ir-module-id
OPCODES-N TYPED-BUFFER BND-OP IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-TYP IR-ID:ir-type-id

1 TYPED-BUFFER S-MOD IR-ID:ir-module-id
1 TYPED-BUFFER S-KEY IR-ID:ir-module-key
1 TYPED-BUFFER S-POOL A64EFF:gprs
VIEWS-N TYPED-BUFFER S-VIEW IR-ARENA:view

create V-DEF VMAX cells allot
create V-LAST VMAX cells allot
create V-REG VMAX cells allot
create V-SET VMAX cells allot
create R-HOLD REGS-N cells allot

\ ---- the slots, read back ----------------------------------------------------
: KEY ( -- IR-ID:ir-module-key )     0 S-KEY @ ;
: VW ( n -- IR-ARENA:view )          S-VIEW @ ;
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

: DEF! ( n n -- )                    {: v:n k:n :} v k cells V-DEF + ! ;
: LAST! ( n n -- )                   {: v:n k:n :} v k cells V-LAST + ! ;
: REG! ( n n -- )                    {: v:n k:n :} v k cells V-REG + ! ;
: SET! ( n n -- )                    {: v:n k:n :} v k cells V-SET + ! ;

: HOLD-AT ( n -- n )                 cells R-HOLD + @ ;
: HOLD! ( n n -- )                   {: v:n r:n :} v r cells R-HOLD + ! ;

: TABLES-CLEAR ( -- )
   VMAX 0 ?do
      0 i SET!
      ENTRY i DEF!
      ENTRY i LAST!
      NOBODY i REG!
   loop
   REGS-N 0 ?do NOBODY i HOLD! loop ;

\ ---- identity ----------------------------------------------------------------
\ Two symbols, or two types, are the same when they are the same ordinal of the
\ same module. Nothing here compares spellings.
: SAME-SYM? ( IR-ID:ir-symbol-id IR-ID:ir-symbol-id -- bool )
   {: x:IR-ID:ir-symbol-id y:IR-ID:ir-symbol-id :}
   x IR-ID:SYMBOL-LOCAL y IR-ID:SYMBOL-LOCAL <> if false exit then
   x IR-ID:SYMBOL-OWNER y IR-ID:SYMBOL-OWNER IR-ID:MODULE-SAME? ;

: SAME-TYPE? ( IR-ID:ir-type-id IR-ID:ir-type-id -- bool )
   {: x:IR-ID:ir-type-id y:IR-ID:ir-type-id :}
   x IR-ID:TYPE-LOCAL y IR-ID:TYPE-LOCAL <> if false exit then
   x IR-ID:TYPE-OWNER y IR-ID:TYPE-OWNER IR-ID:MODULE-SAME? ;

\ ---- the dialect's operation family ------------------------------------------
: SLOT-OF ( A64IR:opcode -- n )
   MATCH A64IR:opcode
      movz OF O-MOVZ ENDOF
      movk OF O-MOVK ENDOF
      add  OF O-ADD  ENDOF
      sub  OF O-SUB  ENDOF
      mul  OF O-MUL  ENDOF
      ret  OF O-RET  ENDOF
   ;MATCH ;

\ Which member of the family this symbol names. An operation of a form outside
\ the family is refused rather than allocated blind: an unmodelled form may tie
\ its operands the way the move-wide overwrite does, and guessing that it does
\ not is how a wrong register reaches the encoder.
: OPCODE-SLOT ( IR-ID:ir-symbol-id -- n )
   {: sym:IR-ID:ir-symbol-id :}
   -1
   OPCODES-N 0 ?do
      sym i BND-OP @ SAME-SYM? if drop i leave then
   loop
   dup 0 < if E-A64RA-OPCODE throw then ;

\ ---- reading the frozen module -----------------------------------------------
: OP-AT ( IR-ID:ir-block-id n -- IR-ID:ir-op-id )
   {: bk:IR-ID:ir-block-id i:n :}
   V-BLKR VW V-OPR VW KEY bk i IR-FUN:FOP@ ;

: OPCODE-AT ( IR-ID:ir-op-id -- n )
   V-OPR VW KEY rot IR-OP:FOPCODE@ OPCODE-SLOT ;

: OPERAND-AT ( IR-ID:ir-op-id n -- IR-ID:ir-value-id )
   {: id:IR-ID:ir-op-id i:n :}
   V-OPP VW V-OPR VW KEY id i IR-OP:FOPERAND@ ;

: RESULT-AT ( IR-ID:ir-op-id n -- IR-ID:ir-value-id )
   {: id:IR-ID:ir-op-id i:n :}
   V-OPP VW V-OPR VW KEY id i IR-OP:FRESULT@ ;

\ ---- the one register class this dialect has ---------------------------------
\ Every value of the machine dialect is a 64-bit general register. A value of any
\ other type has no place in the pool, and it is refused rather than given one.
: CLASS-CK ( IR-ID:ir-value-id -- )
   V-VALR VW KEY rot IR-OP:FVALUE-TYPE@  0 BND-TYP @  SAME-TYPE?
   0= if E-A64RA-CLASS throw then ;

\ ---- pass one: where each value is written, and where it is last read ---------
\ A definition is recorded once - a second one means the walk is not reading an
\ SSA module - and a use is recorded as the position of the operation that makes
\ it, which is monotonic because the walk runs forwards.
: DEFINE ( IR-ID:ir-value-id n -- )
   {: id:IR-ID:ir-value-id pos:n :}
   id CLASS-CK
   id SLOT {: k:n :}
   k SET-AT 0<> if E-A64RA-SHAPE throw then
   1 k SET!
   pos k DEF!
   pos k LAST! ;

: USE ( IR-ID:ir-value-id n -- )
   {: id:IR-ID:ir-value-id pos:n :}
   id SLOT {: k:n :}
   k SET-AT 0= if E-A64RA-SHAPE throw then
   pos k LAST! ;

: DEFS-OF-OP ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id pos:n :}
   V-OPR VW id IR-OP:FRESULTS {: n:n :}
   n 0 ?do id i RESULT-AT pos DEFINE loop ;

: USES-OF-OP ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id pos:n :}
   V-OPR VW id IR-OP:FOPERANDS {: n:n :}
   n 0 ?do id i OPERAND-AT pos USE loop ;

: SCAN-ARGS ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   V-BLKR VW bk IR-FUN:FARG-COUNT {: n:n :}
   n 0 ?do
      V-BLKR VW V-VALR VW KEY bk i IR-FUN:FARG@ ENTRY DEFINE
   loop ;

: SCAN-LIVE ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   bk SCAN-ARGS
   V-BLKR VW bk IR-FUN:FOP-COUNT {: n:n :}
   n 0 ?do
      bk i OP-AT {: id:IR-ID:ir-op-id :}
      id OPCODE-AT drop
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

\ The lowest-numbered register of the pool that holds nothing. Lowest rather than
\ next-around, so the same block always allocates the same way.
: FREE-REG ( -- n )
   -1
   REGS-N 0 ?do
      i POOL-HAS? i HOLD-AT NOBODY = and if drop i leave then
   loop
   dup 0 < if E-A64RA-PRESSURE throw then ;

: TAKE ( n n -- )
   {: k:n r:n :}
   r POOL-HAS? 0= if E-A64RA-PRESSURE throw then
   r k REG!
   k r HOLD! ;

: ASSIGN ( IR-ID:ir-value-id -- )
   SLOT FREE-REG TAKE ;

\ The move-wide overwrite: the result has to land in the register the kept value
\ is already in, so the kept value must die here. If it is read again later, no
\ single register field can serve both and the program is refused.
: TIE ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id pos:n :}
   id 0 OPERAND-AT SLOT {: keep:n :}
   keep LAST-AT pos <> if E-A64RA-TIE throw then
   id 0 RESULT-AT SLOT  keep REG-AT  TAKE ;

: ASSIGN-RESULTS ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   V-OPR VW id IR-OP:FRESULTS {: n:n :}
   n 0 ?do id i RESULT-AT ASSIGN loop ;

: ASSIGN-OP ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id pos:n :}
   pos 1+ EXPIRE
   id OPCODE-AT O-MOVK = if
      id pos TIE
   else
      id ASSIGN-RESULTS
   then ;

: SCAN-ASSIGN ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   V-BLKR VW bk IR-FUN:FARG-COUNT {: n:n :}
   n 0 ?do
      V-BLKR VW V-VALR VW KEY bk i IR-FUN:FARG@ ASSIGN
   loop
   V-BLKR VW bk IR-FUN:FOP-COUNT {: k:n :}
   k 0 ?do bk i OP-AT i ASSIGN-OP loop ;

\ ---- what one allocation run is told -----------------------------------------
: VIEWS! ( IR-BUILD:module -- )
   {: m:IR-BUILD:module :}
   m IR-BUILD:FKEY 0 S-KEY !
   m IR-BUILD:FMODULE 0 S-MOD !
   m IR-BUILD:FOP-POOL    V-OPP  S-VIEW !
   m IR-BUILD:FOP-ROWS    V-OPR  S-VIEW !
   m IR-BUILD:FVALUE-ROWS V-VALR S-VIEW !
   m IR-BUILD:FFUN-ROWS   V-FUNR S-VIEW !
   m IR-BUILD:FBLOCK-ROWS V-BLKR S-VIEW ! ;

\ The straight-line subset is one function of one block; any other shape means
\ control flow, and control flow has no allocation rule here yet.
: BLOCK-OF ( -- IR-ID:ir-block-id )
   V-FUNR VW IR-FUN:FFUNS 1 <> if E-A64RA-SHAPE throw then
   KEY 0 IR-ID:PACK-FUN {: f:IR-ID:ir-fun-id :}
   V-FUNR VW f IR-FUN:FBLOCK-COUNT 1 <> if E-A64RA-SHAPE throw then
   V-FUNR VW V-BLKR VW KEY f 0 IR-FUN:FBLOCK@ ;

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

: BIND1 ( IR-CTX:ctx IR-BUILD:builder A64IR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder o:A64IR:opcode :}
   c b o A64IR:OPCODE  o SLOT-OF BND-OP ! ;

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
\ Learn the operation and type identities of the module that is about to be
\ allocated, while it is still being built. A module's symbols and types are its
\ own ordinals, so this is the only moment the dialect can be asked which symbol
\ each of its opcodes is and which type its general register is; the answers stay
\ valid after the module freezes because freezing keeps the module's identity.
\ The binding is spent by the next ALLOCATE.
: BIND-DIALECT ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   BND-MODE @ BOUND-YES = if E-A64RA-BIND throw then
   c b DIALECT-CK
   b IR-BUILD:MODULE@ 0 BND-MOD !
   c b A64IR-OPCODE:MOVZ BIND1
   c b A64IR-OPCODE:MOVK BIND1
   c b A64IR-OPCODE:ADD  BIND1
   c b A64IR-OPCODE:SUB  BIND1
   c b A64IR-OPCODE:MUL  BIND1
   c b A64IR-OPCODE:RET  BIND1
   c b A64IR:GPR-TYPE 0 BND-TYP !
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
: ALLOCATE ( IR-CTX:ctx IR-BUILD:module A64EFF:routine -- )
   A64EFF:VALIDATE A64EFF:GPR-CLOBBER@ {: pool:A64EFF:gprs :}
   {: c:IR-CTX:ctx m:IR-BUILD:module :}
   BND-TAKE
   ST-EMPTY ST !
   m BND-MODULE-CK
   c TARGET-CK
   pool 0 S-POOL !
   m VIEWS!
   TABLES-CLEAR
   BLOCK-OF {: bk:IR-ID:ir-block-id :}
   bk SCAN-LIVE
   COVER-CK
   bk SCAN-ASSIGN
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

: MOVK-SYM ( -- IR-ID:ir-symbol-id )
   SEAL-CK O-MOVK BND-OP @ ;

: GPR-TYPE@ ( -- IR-ID:ir-type-id )
   SEAL-CK 0 BND-TYP @ ;

: VALUES ( -- n )
   SEAL-CK N-VALS @ ;

: CLAIM@ ( n -- n )
   SEAL-CK ORD-CK REG-AT ;

: DEF@ ( n -- n )
   SEAL-CK ORD-CK DEF-AT ;

: LAST@ ( n -- n )
   SEAL-CK ORD-CK LAST-AT ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
