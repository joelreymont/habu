\ dialect.f - what a machine dialect tells the register allocator (VOCAB) and
\ the spill rewriter (LOWERING) about itself, so regalloc.f, regalloc-verify.f
\ and spill.f name no machine package. Two records, not one: their union passes
\ MAKE's 32-entry staged-list ceiling (src/compiler/ir/type.f,
\ E-IR-TYPE-ARITY). Every name is one module's symbol ordinal, so a value is
\ built at the binding for that module and never outlives it.

require src/lib/prelude.f
require src/lib/errors.f
require src/compiler/target.f
require src/compiler/ir/id.f

package NDIALECT
public

\ ---- a name a dialect may not have -------------------------------------------
\ An absent key or form has no symbol, not a special number. It covers opcodes
\ too: a dialect without a floating file declares no float slot form.
SUMTYPE optsym
   VARIANT absent ;VARIANT
   VARIANT present IR-ID:ir-symbol-id ;VARIANT
;SUMTYPE

\ ---- where a dialect's selector stands the data-stack pointer ----------------
\ The pointer stands at one place for a whole body; which place is the
\ selector's policy, and the validator checks against it rather than
\ re-deriving it. `survey` (select.f): where the fewest boundary transfers need
\ an adjustment. `entry-base` (select-x64.f): the base the entry transfer
\ leaves, with one adjustment at each end and none between.
SUMTYPE dstand
   VARIANT survey ;VARIANT
   VARIANT entry-base ;VARIANT
;SUMTYPE

\ ---- one dialect's vocabulary ------------------------------------------------
\ name/major/minor and arch are checked against the module and the context's
\ contract; dwb is absent on x86-64, which has no write-back addressing; lanes
\ is how many instructions an address literal takes (four contiguous move-wides
\ on ARM64, one on x86-64).
STRUCTURE vocab
   FIELD name IR-ID:ir-symbol-id
   FIELD major n
   FIELD minor n
   FIELD arch CTARGET:arch
   FIELD gpr IR-ID:ir-type-id
   FIELD fpr IR-ID:ir-type-id
   FIELD mem IR-ID:ir-type-id
   FIELD slot IR-ID:ir-symbol-id
   FIELD frame IR-ID:ir-symbol-id
   FIELD dslot IR-ID:ir-symbol-id
   FIELD dbytes IR-ID:ir-symbol-id
   FIELD dback IR-ID:ir-symbol-id
   FIELD tail-in IR-ID:ir-symbol-id
   FIELD tail-out IR-ID:ir-symbol-id
   FIELD dwb optsym
   FIELD entry IR-ID:ir-symbol-id
   FIELD trap-entry IR-ID:ir-symbol-id
   FIELD addr IR-ID:ir-symbol-id
   FIELD shift IR-ID:ir-symbol-id
   FIELD copy IR-ID:ir-symbol-id
   FIELD remat IR-ID:ir-symbol-id
   FIELD remat-mark IR-ID:ir-symbol-id
   FIELD lanes n
   FIELD slot-width n
   FIELD stand dstand
;STRUCTURE

\ ---- what the spill rewriter is told -----------------------------------------
\ src/compiler/native/spill.f asks every question of an operation by name. Only
\ this record holds the frame forms. fstore/fload are absent on x86-64, so a
\ float slot there is refused by name; linksave/linkload are absent on x86-64
\ because its call pushes the return address.
STRUCTURE lowering
   FIELD name IR-ID:ir-symbol-id
   FIELD major n
   FIELD minor n
   FIELD gpr IR-ID:ir-type-id
   FIELD fpr IR-ID:ir-type-id
   FIELD mem IR-ID:ir-type-id
   FIELD slot IR-ID:ir-symbol-id
   FIELD frame IR-ID:ir-symbol-id
   FIELD copy IR-ID:ir-symbol-id
   FIELD remat IR-ID:ir-symbol-id
   FIELD remat-mark IR-ID:ir-symbol-id
   FIELD reserve IR-ID:ir-symbol-id
   FIELD release IR-ID:ir-symbol-id
   FIELD store IR-ID:ir-symbol-id
   FIELD load IR-ID:ir-symbol-id
   FIELD fstore optsym
   FIELD fload optsym
   FIELD trap IR-ID:ir-symbol-id
   FIELD linksave optsym
   FIELD linkload optsym
;STRUCTURE

\ ---- reading a field that may be missing --------------------------------------
: HAS? ( NDIALECT:optsym -- bool )
   MATCH optsym
      absent  OF false ENDOF
      present OF drop true ENDOF
   ;MATCH ;

\ A dialect that declared the name absent is refused rather than answered with
\ some other dialect's symbol or a number that would read as one.
: SYM ( NDIALECT:optsym -- IR-ID:ir-symbol-id )
   MATCH optsym
      absent  OF E-NDIALECT throw ENDOF
      present OF ENDOF
   ;MATCH ;

\ ---- what a pass that WRITES a module asks ------------------------------------
\ A rebuilding pass needs only the written module's types: every name it writes
\ crosses from the module it copies.
: TYPES ( NDIALECT:lowering -- IR-ID:ir-type-id IR-ID:ir-type-id IR-ID:ir-type-id )
   NDIALECT-LOWERING:UNMAKE
   {: nm:IR-ID:ir-symbol-id mj:n mi:n
      gpr:IR-ID:ir-type-id fpr:IR-ID:ir-type-id mem:IR-ID:ir-type-id
      slot:IR-ID:ir-symbol-id frame:IR-ID:ir-symbol-id
      copy:IR-ID:ir-symbol-id remat:IR-ID:ir-symbol-id
      mark:IR-ID:ir-symbol-id
      reserve:IR-ID:ir-symbol-id release:IR-ID:ir-symbol-id
      store:IR-ID:ir-symbol-id load:IR-ID:ir-symbol-id
      fstore:NDIALECT:optsym fload:NDIALECT:optsym
      trapop:IR-ID:ir-symbol-id
      linksave:NDIALECT:optsym linkload:NDIALECT:optsym :}
   gpr fpr mem ;

private

get-current prot-wid-add

public
get-current prot-wid-add

;package
