\ dialect.f - what a machine dialect has to TELL the register allocator and the
\ spill rewriter about itself, as checked values the dialect builds and the
\ passes read.
\
\ WHY IT EXISTS. src/compiler/native/regalloc.f, regalloc-verify.f and spill.f
\ are about live ranges, register pressure and frame slots, and none of that is
\ one machine's. What WAS one machine's is the spelling: the files named A64IR
\ to ask which type a general register has, which key an operation carries its
\ frame slot under, which opcode is a copy, which form puts a value away and
\ brings it back, and how many instructions an address literal takes. A second
\ dialect with every one of those answers could not be allocated or lowered for,
\ because the questions were asked of a package by name.
\
\ ONE VALUE PER PASS, AND WHY NOT ONE FOR ALL OF THEM. The allocator's
\ vocabulary and the rewriter's lowering hold the same identity and some of the
\ same names, and one record holding the union of them would be the obvious
\ shape. It is not a shape this language has: a record's MAKE stages one value
\ per cell and src/compiler/ir/type.f commits a 32-entry ceiling on a staged
\ list (E-IR-TYPE-ARITY), which the union - thirty-one fields, five of them a
\ two-cell `optsym` - is past. Each pass therefore takes the record of what IT
\ is told, and the dialect builds both out of the same private answers, so there
\ is still one place per dialect where a name is decided.
\
\ WHAT THE VOCABULARY HOLDS. Exactly the names the allocator and its validator
\ would otherwise spell, and nothing a pass can derive for itself:
\   name/major/minor - who built the module, checked against what the module
\                      says about itself, so a dialect's vocabulary can never be
\                      bound to another dialect's module.
\   arch             - the machine the dialect lowers for, checked against the
\                      contract the compilation context is bound to.
\   gpr/fpr/mem      - the three value types the allocator classifies by: a
\                      general register, a floating register, and the memory
\                      token that is no register at all.
\   the keys         - the attributes an operation carries its frame slot, its
\                      frame size, its data-stack slot, its data-stack byte
\                      move, its take-back, its call or branch target, its trap
\                      target, its address and its address lane under.
\   dwb              - the data-stack key a dialect may not HAVE: an access that
\                      moves the pointer as part of its own encoding. x86-64 has
\                      no write-back addressing, so its vocabulary says `absent`
\                      rather than naming a symbol it never interned.
\   copy/remat       - the opcode a register-to-register move is, and the one
\                      the allocator may re-emit instead of reloading.
\   lanes            - how many instructions an address literal is. ARM64 builds
\                      one out of four move-wides that must stay contiguous
\                      through spill insertion; x86-64 writes one instruction,
\                      and a run of one has no lane arithmetic at all.
\   slot-width       - the bytes one frame access moves.
\   stand            - where the dialect's SELECTOR stands the data-stack
\                      pointer over a body, which the validator checks a module
\                      against instead of re-deriving one selector's policy.
\
\ WHY A VALUE AND NOT A TABLE. Every name in it is one MODULE's ordinal - a
\ symbol id carries the module it was interned in - so a vocabulary is only
\ meaningful for the module it was built from and outlives it by nothing. A row
\ in a table would have to be recycled or grown per compiled module; a value
\ built at the binding and consumed by it cannot go stale, be forged, or be read
\ by a pass that was handed another module.

require lib/prelude.f
require lib/errors.f
require src/compiler/target.f
require src/compiler/ir/id.f

package NDIALECT
public

\ ---- a name a dialect may not have -------------------------------------------
\ An absent key or form is not one with a special number: there is no symbol,
\ because the dialect never declared what would carry it. Saying so in the type
\ is what keeps a reader from asking. It holds keys AND opcodes: a dialect with
\ no floating file declares no float slot form for the same reason one with no
\ write-back addressing declares no write-back key.
SUMTYPE optsym 0
   VARIANT absent ;VARIANT
   VARIANT present IR-ID:ir-symbol-id ;VARIANT
;SUMTYPE

\ ---- where a dialect's selector stands the data-stack pointer ----------------
\ The pointer is a register and stands at ONE place for a whole body, but WHICH
\ place is the SELECTOR's policy rather than anything a later pass can derive
\ from the module: two selectors that both lower correctly stand in different
\ places, and a validator that re-derives one of them refuses the other's
\ modules.
\
\ `survey` is src/compiler/native/select.f's: the pointer stands where the
\ fewest boundary transfers need an adjustment, which is a place the body's own
\ call sites decide and a reader has to re-derive.
\
\ `entry` is src/compiler/native/select-x64.f's: the entry transfer takes every
\ argument's bytes at once, so the body stands at the base that transfer leaves
\ it at and the exit publishes every result's - one adjustment at each end and
\ none in between.
SUMTYPE dstand 0
   VARIANT survey ;VARIANT
   VARIANT entry ;VARIANT
;SUMTYPE

\ ---- one dialect's vocabulary ------------------------------------------------
\ The order is the order a binding reads it in: who you are, what machine, the
\ types, the keys, the opcodes, the shape, and the policy the selector states.
STRUCTURE vocab 0
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
   FIELD dwb optsym
   FIELD entry IR-ID:ir-symbol-id
   FIELD trap-entry IR-ID:ir-symbol-id
   FIELD addr IR-ID:ir-symbol-id
   FIELD shift IR-ID:ir-symbol-id
   FIELD copy IR-ID:ir-symbol-id
   FIELD remat IR-ID:ir-symbol-id
   FIELD lanes n
   FIELD slot-width n
   FIELD stand dstand
;STRUCTURE

\ ---- what the spill rewriter is told -----------------------------------------
\ src/compiler/native/spill.f reads a frozen module and writes a new one, and
\ every question it asks of an operation is about a NAME: is this the copy
\ coalescing left standing, the trap a block ends without returning in, the
\ reserve a selector's prologue opens with; is this attribute the frame slot or
\ the frame size. The identity and the three types are here for the reasons they
\ are in the vocabulary - a record of one dialect cannot be bound to another's
\ module, and a value minted in the module being written has to be of that
\ module's own types. What is here and in no other record is the FRAME FORMS:
\   reserve/release  - open and close one invocation's frame.
\   store/load       - put a general value away in a slot and bring it back.
\   fstore/fload     - the same for a value out of the floating file, which a
\                      dialect without one does not have. x86-64 declares
\                      neither, so a float slot there is refused by name rather
\                      than put away in a form it cannot come back into.
\   trap             - the terminator that leaves a block without returning, so
\                      the block a routine RETURNS from can be told from it.
\   linksave/linkload - the save and restore of the return address, which a
\                      dialect that keeps it on the machine stack does not have.
\                      ARM64 holds it in a register and names both; x86-64's
\                      call pushes it, so both are absent and the frame shape
\                      counts none.
STRUCTURE lowering 0
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
\ The two readers a pass needs and the only ones: whether the dialect has the
\ name, and - under that answer - which symbol it is.
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
\ The three types a value it mints can have. A pass that rebuilds a module holds
\ two vocabularies - the read module's and the written module's - and of the
\ second one it needs only these: every NAME it writes crosses from the module
\ it is copying, while a type has to be the written module's own identity. A
\ record is unmade whole, so the question is answered once here rather than by a
\ list of every field at each pass that asks it.
: TYPES ( NDIALECT:lowering -- IR-ID:ir-type-id IR-ID:ir-type-id IR-ID:ir-type-id )
   NDIALECT-LOWERING:UNMAKE
   {: nm:IR-ID:ir-symbol-id mj:n mi:n
      gpr:IR-ID:ir-type-id fpr:IR-ID:ir-type-id mem:IR-ID:ir-type-id
      slot:IR-ID:ir-symbol-id frame:IR-ID:ir-symbol-id
      copy:IR-ID:ir-symbol-id remat:IR-ID:ir-symbol-id
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
