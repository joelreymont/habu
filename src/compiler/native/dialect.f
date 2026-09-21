\ dialect.f - what a machine dialect has to TELL the register allocator about
\ itself, as one checked value the dialect builds and the allocator reads.
\
\ WHY IT EXISTS. src/compiler/native/regalloc.f and regalloc-verify.f are about
\ live ranges, register pressure and frame slots, and none of that is one
\ machine's. What WAS one machine's is the spelling: both files named A64IR to
\ ask which type a general register has, which key an operation carries its
\ frame slot under, which opcode is a copy, and how many instructions an address
\ literal takes. A second dialect with every one of those answers could not be
\ allocated for, because the questions were asked of a package by name.
\
\ WHAT IT HOLDS. Exactly the names those two passes would otherwise spell, and
\ nothing a pass can derive for itself:
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

\ ---- a key a dialect may not have --------------------------------------------
\ An absent key is not a key with a special number: there is no symbol, because
\ the dialect never declared the form that would carry it. Saying so in the type
\ is what keeps a reader from asking.
SUMTYPE optkey 0
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
   FIELD dwb optkey
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

\ ---- reading the one field that may be missing --------------------------------
\ The two readers a pass needs and the only ones: whether the dialect has the
\ key, and - under that answer - which symbol it is.
: HAS-KEY? ( NDIALECT:optkey -- bool )
   MATCH optkey
      absent  OF false ENDOF
      present OF drop true ENDOF
   ;MATCH ;

\ A dialect that declared the key absent is refused rather than answered with
\ some other dialect's symbol or a number that would read as one.
: KEY ( NDIALECT:optkey -- IR-ID:ir-symbol-id )
   MATCH optkey
      absent  OF E-NDIALECT throw ENDOF
      present OF ENDOF
   ;MATCH ;

private

get-current prot-wid-add

public
get-current prot-wid-add

;package
