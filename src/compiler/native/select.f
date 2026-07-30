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
\ WHY A TRAPPING OPERATION IS REFUSED RATHER THAN SELECTED. Whether integer
\ overflow traps is the compilation unit's numeric policy, and the source dialect
\ records the answer in each arithmetic schema's may-trap flag. ARM64's Add, Sub
\ and Mul wrap; lowering a trapping addition needs a flag-setting form, a
\ conditional branch and a trap target, and the A64IR dialect has none of the
\ three yet. Selecting a trapping addition to a plain a64.add would silently drop
\ the check the policy asked for, so this pass reads the source schema's own flag
\ and refuses. The missing lowering is tracked as its own capability; until it
\ lands, a trapping unit does not select at all rather than selecting wrongly.
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
\ ONE SELECTION AT A TIME. The value map and the frozen module's views are fixed
\ package-owned slots rather than heap objects, so this pass selects one module at
\ a time - the single-task compilation discipline the rest of the compiler already
\ keeps. The whole walk is one call, so nothing a refused call left behind can be
\ read by the next one; the binding is separately taken at entry, so a refused
\ selection also leaves no binding for a later caller to select against by
\ accident.

require lib/prelude.f
require lib/errors.f
require src/compiler/digest.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/arena.f
require src/compiler/ir/symbol.f
require src/compiler/ir/type.f
require src/compiler/ir/attr.f
require src/compiler/ir/source.f
require src/compiler/ir/schema.f
require src/compiler/ir/op.f
require src/compiler/ir/fun.f
require src/compiler/ir/build.f
require src/compiler/native/hir.f
require src/compiler/native/a64ir.f

package A64SEL
private

\ ---- the bound source dialect ------------------------------------------------
\ One slot per member of the source dialect's opcode family, plus the attribute
\ key its constant carries and the module all six were learned from.
5 constant OPCODES-N
0 constant O-CONST
1 constant O-ADD
2 constant O-SUB
3 constant O-MUL
4 constant O-RETURN

0 constant BOUND-NO
1 constant BOUND-YES

\ ---- how much of one function this pass holds --------------------------------
\ Values in one function. Two hundred and fifty-six is far past anything a
\ straight-line body reaches; a function that wants more is a capability to raise
\ here, not a ceiling to widen silently.
256 constant VMAX

\ The longest function name this pass can carry across. A name is copied out of
\ the source module's interner and interned into the new one, because the two
\ modules number their symbols separately.
128 constant NAME-CAP

\ The frozen tables of the module being read. One indexed slot per view keeps
\ every helper below to a signature a reader can hold in their head.
11 constant VIEWS-N
0 constant V-SYMP                    \ symbol pool
1 constant V-SYMR                    \ symbol rows
2 constant V-TYPR                    \ type rows
3 constant V-ATTR                    \ attribute rows
4 constant V-SRC                     \ source registry
5 constant V-SCHR                    \ schema rows
6 constant V-OPP                     \ operation pool
7 constant V-OPR                     \ operation rows
8 constant V-VALR                    \ value rows
9 constant V-FUNR                    \ function rows
10 constant V-BLKR                   \ block rows

here CELL 1- and CELL swap - CELL 1- and allot
variable BND-MODE
BOUND-NO BND-MODE !

1 TYPED-BUFFER BND-MOD IR-ID:ir-module-id
OPCODES-N TYPED-BUFFER BND-OP IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-VAL IR-ID:ir-symbol-id

1 TYPED-BUFFER S-CTX IR-CTX:ctx
1 TYPED-BUFFER S-BLD IR-BUILD:builder
1 TYPED-BUFFER S-KEY IR-ID:ir-module-key
1 TYPED-BUFFER S-SID IR-ID:ir-source-id
1 TYPED-BUFFER S-ACC IR-ID:ir-value-id
VIEWS-N TYPED-BUFFER S-VIEW IR-ARENA:view
VMAX TYPED-BUFFER VMAP IR-ID:ir-value-id
create VSET VMAX cells allot
create NAMEBUF NAME-CAP allot

\ ---- the slots, read back ----------------------------------------------------
: CTX ( -- IR-CTX:ctx )              0 S-CTX @ ;
: BLD ( -- IR-BUILD:builder )        0 S-BLD @ ;
: KEY ( -- IR-ID:ir-module-key )     0 S-KEY @ ;
: SID ( -- IR-ID:ir-source-id )      0 S-SID @ ;
: ACC ( -- IR-ID:ir-value-id )       0 S-ACC @ ;
: ACC! ( IR-ID:ir-value-id -- )      0 S-ACC ! ;
: VW ( n -- IR-ARENA:view )          S-VIEW @ ;

\ ---- symbol identity ---------------------------------------------------------
\ Two symbols are the same when they are the same ordinal of the same module.
\ Nothing here compares spellings: the source dialect's spellings are the source
\ dialect's, and this pass holds identities it was given rather than bytes it
\ decided on.
: SAME-SYM? ( IR-ID:ir-symbol-id IR-ID:ir-symbol-id -- bool )
   {: x:IR-ID:ir-symbol-id y:IR-ID:ir-symbol-id :}
   x IR-ID:SYMBOL-LOCAL y IR-ID:SYMBOL-LOCAL <> if false exit then
   x IR-ID:SYMBOL-OWNER y IR-ID:SYMBOL-OWNER IR-ID:MODULE-SAME? ;

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
      return OF O-RETURN ENDOF
   ;MATCH ;

: SLOT-OPCODE ( n -- HIR:opcode )
   case
      O-CONST  of HIR-OPCODE:CONST  endof
      O-ADD    of HIR-OPCODE:ADD    endof
      O-SUB    of HIR-OPCODE:SUB    endof
      O-MUL    of HIR-OPCODE:MUL    endof
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
   V-OPR VW KEY id IR-OP:FSPAN@ IR--SOURCE-SPAN:UNMAKE
   {: src:IR-ID:ir-source-id st:n ln:n :}
   src SRC-CK
   BLD SID st ln IR-BUILD:ADD-SPAN ;

: FUN-SPAN ( IR-ID:ir-fun-id -- IR-SOURCE:span )
   {: f:IR-ID:ir-fun-id :}
   V-FUNR VW KEY f IR-FUN:FSPAN@ IR--SOURCE-SPAN:UNMAKE
   {: src:IR-ID:ir-source-id st:n ln:n :}
   src SRC-CK
   BLD SID st ln IR-BUILD:ADD-SPAN ;

: BLOCK-SPAN ( IR-ID:ir-block-id -- IR-SOURCE:span )
   {: bk:IR-ID:ir-block-id :}
   V-BLKR VW KEY bk IR-FUN:FBLOCK-SPAN@ IR--SOURCE-SPAN:UNMAKE
   {: src:IR-ID:ir-source-id st:n ln:n :}
   src SRC-CK
   BLD SID st ln IR-BUILD:ADD-SPAN ;

\ The value one operand of a source operation selected to.
: OPERAND ( IR-ID:ir-op-id n -- IR-ID:ir-value-id )
   {: id:IR-ID:ir-op-id i:n :}
   V-OPP VW V-OPR VW KEY id i IR-OP:FOPERAND@ VOF ;

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

\ Close the operation and keep the one value it defined as the running value.
: CLOSE-VALUE ( -- )
   CTX BLD IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CTX BLD id 0 IR-BUILD:OP-RESULT@ ACC! ;

\ ---- selecting a constant ----------------------------------------------------
\ The literal is the whole content of a source constant, and it rides as the
\ attribute the source opcode's schema requires. The key is compared against the
\ one this pass was told, so a constant carrying some other attribute is refused
\ instead of read as if it were the value.
: CONST-VALUE ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   V-OPR VW id IR-OP:FATTRS 1 <> if E-A64SEL-ATTR throw then
   V-OPP VW V-OPR VW KEY id 0 IR-OP:FATTR-KEY@  0 BND-VAL @  SAME-SYM?
   0= if E-A64SEL-ATTR throw then
   V-ATTR VW  V-OPP VW V-OPR VW KEY id 0 IR-OP:FATTR@  IR-ATTR:FINT@ ;

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
   V-OPP VW V-OPR VW KEY id 0 IR-OP:FRESULT@  ACC  VBIND ;

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
   V-OPP VW V-OPR VW KEY id 0 IR-OP:FRESULT@  ACC  VBIND ;

\ ---- selecting the return ----------------------------------------------------
\ The values still live where control leaves become the terminator's operands,
\ in the order the source return has them.
: EMIT-RETURN ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id A64IR-OPCODE:RET OPEN
   V-OPR VW id IR-OP:FOPERANDS {: k:n :}
   k 0 ?do
      CTX BLD  id i OPERAND  IR-BUILD:ADD-OPERAND
   loop
   CTX BLD IR-BUILD:END-OP drop ;

\ ---- the selection table -----------------------------------------------------
\ The whole rule. Every arm names the machine operations one source operation
\ becomes; nothing else in this file decides which opcode a source operation
\ selects to.
: RULE ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   V-OPR VW KEY id IR-OP:FOPCODE@ {: sym:IR-ID:ir-symbol-id :}
   V-SCHR VW sym IR-SCHEMA:FTRAPS? if E-A64SEL-TRAP throw then
   sym OPCODE-SLOT SLOT-OPCODE
   MATCH HIR:opcode
      const  OF id EMIT-CONST ENDOF
      add    OF id A64IR-OPCODE:ADD EMIT-BINARY ENDOF
      sub    OF id A64IR-OPCODE:SUB EMIT-BINARY ENDOF
      mul    OF id A64IR-OPCODE:MUL EMIT-BINARY ENDOF
      return OF id EMIT-RETURN ENDOF
   ;MATCH ;

\ ---- opening the selected function -------------------------------------------
\ The two modules number their symbols separately, so the name is copied out of
\ the source interner and interned into the new one. Interning deduplicates, so
\ the new module gains one symbol per distinct name and no more.
: FUN-NAME ( IR-ID:ir-fun-id -- IR-ID:ir-symbol-id )
   {: f:IR-ID:ir-fun-id :}
   V-SYMP VW V-SYMR VW  V-FUNR VW KEY f IR-FUN:FSYMBOL@  NAMEBUF NAME-CAP
   IR-SYM:FCOPY {: u:n :}
   CTX BLD NAMEBUF u IR-BUILD:INTERN-SYMBOL ;

\ The word's declared effect, restated in this dialect's type: one virtual
\ register per input and one per output. The counts are the source signature's,
\ read off the source module rather than counted off its body.
: FUN-SIG ( IR-ID:ir-fun-id -- IR-ID:ir-type-id )
   {: f:IR-ID:ir-fun-id :}
   V-TYPR VW  V-FUNR VW KEY f IR-FUN:FSIGNATURE@  IR-TYPE:FARITY@
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

\ The entry block's arguments are the word's inputs, one virtual register each,
\ and each one is the value the matching source argument selects to.
: OPEN-BLOCK ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   CTX BLD IR-BUILD:BEGIN-BLOCK
   CTX BLD bk BLOCK-SPAN IR-BUILD:SET-BLOCK-SPAN
   VCLEAR
   V-BLKR VW bk IR-FUN:FARG-COUNT {: n:n :}
   n 0 ?do
      V-BLKR VW V-VALR VW KEY bk i IR-FUN:FARG@
      CTX BLD  CTX BLD A64IR:GPR-TYPE  IR-BUILD:ADD-BLOCK-ARG
      VBIND
   loop ;

\ One function of the source module. The straight-line subset is one block, and a
\ function with any other shape is refused here rather than selected in part: a
\ second block means control flow, and control flow has no selection rule yet.
: WALK-FUN ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   V-FUNR VW f IR-FUN:FBLOCK-COUNT 1 <> if E-A64SEL-SHAPE throw then
   V-FUNR VW V-BLKR VW KEY f 0 IR-FUN:FBLOCK@ {: bk:IR-ID:ir-block-id :}
   f OPEN-FUN
   bk OPEN-BLOCK
   V-BLKR VW bk IR-FUN:FOP-COUNT {: n:n :}
   n 0 ?do
      V-BLKR VW V-OPR VW KEY bk i IR-FUN:FOP@ RULE
   loop
   CTX BLD IR-BUILD:END-BLOCK drop
   CTX BLD IR-BUILD:END-FUN drop ;

\ ---- what one selection run is told ------------------------------------------
: VIEWS! ( IR-BUILD:module -- )
   {: m:IR-BUILD:module :}
   m IR-BUILD:FKEY 0 S-KEY !
   m IR-BUILD:FSYM-POOL    V-SYMP S-VIEW !
   m IR-BUILD:FSYM-ROWS    V-SYMR S-VIEW !
   m IR-BUILD:FTYPE-ROWS   V-TYPR S-VIEW !
   m IR-BUILD:FATTR-ROWS   V-ATTR S-VIEW !
   m IR-BUILD:FSOURCES     V-SRC  S-VIEW !
   m IR-BUILD:FSCHEMA-ROWS V-SCHR S-VIEW !
   m IR-BUILD:FOP-POOL     V-OPP  S-VIEW !
   m IR-BUILD:FOP-ROWS     V-OPR  S-VIEW !
   m IR-BUILD:FVALUE-ROWS  V-VALR S-VIEW !
   m IR-BUILD:FFUN-ROWS    V-FUNR S-VIEW !
   m IR-BUILD:FBLOCK-ROWS  V-BLKR S-VIEW ! ;

\ The new module gets the same source the old one has, proved the same rather
\ than assumed: IR-SOURCE records a source as the digest of its bytes, so the
\ text presented here is the text the source module was compiled from exactly
\ when the two digests agree.
: SOURCE! ( IR-CTX:ctx IR-BUILD:builder ptr u8 n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p u:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   V-SRC VW IR-SOURCE:FSOURCES 1 <> if E-A64SEL-SHAPE throw then
   V-SRC VW  KEY 0 IR-ID:PACK-SOURCE  IR-SOURCE:FDIGEST@
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
: SELECT ( IR-CTX:ctx IR-BUILD:module IR-BUILD:builder ptr u8 n -- IR-BUILD:module )
   {: c:IR-CTX:ctx m:IR-BUILD:module b:IR-BUILD:builder p u:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   BND-TAKE
   m BND-MODULE-CK
   c b A64IR:REGISTER
   c 0 S-CTX !
   b 0 S-BLD !
   m VIEWS!
   c b p u SOURCE!
   V-FUNR VW IR-FUN:FFUNS {: n:n :}
   n 0 ?do
      KEY i IR-ID:PACK-FUN WALK-FUN
   loop
   c b IR-BUILD:FREEZE ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
