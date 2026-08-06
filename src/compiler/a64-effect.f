\ a64-effect.f - the typed machine-state contract of one emitted ARM64 routine.
\
\ A Forth stack effect describes the host emitter's stack: what the word that
\ WRITES instructions consumes and leaves. It says nothing about the machine
\ state the written instructions read and destroy, so a callable label emitted by
\ src/habu/habu2.f or bootstrap/cg/forth.fs has, today, no first-class statement
\ of what it does to the registers, the condition flags, the link register and the
\ stack pointer. tools/lint/clobber-lint.f fills that hole with hand-written name
\ and opcode tables - a second authority that drifts from the emitters it
\ describes. This file is the first-class statement those tables should be
\ answering questions from, and the record the A64IR dialect composes with its
\ own indexed operand records.
\
\ WHAT A CONTRACT SAYS. For each of the two register files the shipped assembler
\ can name: which registers the routine reads before it writes them, which hold
\ its results, and which it destroys. Then what it does to the condition flags and
\ to the link register, the stack frame it reserves and the net stack-pointer
\ change where control leaves, whether it calls, calls indirectly or enters the
\ kernel, and how control leaves at all.
\
\ THE INTERFACE IS ORDERED, BECAUSE A CONVENTION IS. A set can say that a routine
\ reads x0 and x1; it cannot say that argument two arrives in x1, and that is the
\ only thing a caller and a callee have to agree about. The two general interface
\ fields are therefore ordered lists rather than sets: `gpr-arg` says where each
\ argument arrives, position by position, and `gpr-out` where each returned value
\ leaves. The register sets are still answerable - GPR-IN@ and GPR-RESULT@ derive
\ them - so no reader lost anything, and there is no second place a convention
\ could be written down and disagree from. This is what the register allocator
\ pre-colours from and what its validator checks the finished assignment against,
\ and both are handed this one value, so neither can be answering about another
\ routine's interface. WHICH places a particular ABI uses is the caller's
\ declaration, not a constant here: this file owns what a convention IS, not
\ which one is in force.
\
\ A POSITION NAMES A PLACE, AND A REGISTER IS ONE KIND OF PLACE. Design section
\ 7.6 gives an externally callable Habu word its inputs and outputs in canonical
\ data-stack slots, which are not registers at all, so "argument two arrives in
\ x1" and "argument two arrives in data-stack slot two" have to be sayable by one
\ declaration. A position therefore holds a PLACE: a general register, or a
\ data-stack slot. There are exactly two kinds and no third is anticipated - a
\ machine-stack argument slot, a floating register and an immediate would each be
\ a further kind with a further owner, and none of them exists in the chain.
\ The floating-register interface is still a pair of sets, deliberately: the
\ machine dialect has no floating value class yet, so an ordered floating list
\ would be a promise rather than a declaration anything could honour, and a
\ convention that interleaves integer and floating arguments needs the position
\ of an argument to carry its class as well (dot
\ habu-bind-floating-and-d2a16dbd).
\
\ THE VOCABULARY IS THE ASSEMBLER'S, NOT A SECOND ONE. Every bound here is read
\ off the instruction vocabulary that formal/Common/Insn.v models and
\ src/arch/arm64/asm.f emits, and test/compiler/a64-effect.f pins each one against
\ that source instead of restating it:
\   - a register operand is a five-bit field, so a file holds 32 registers;
\   - x18 is Darwin platform-reserved and `XREG?` refuses it in EVERY X-register
\     operand slot, so no emitted routine can hold state there;
\   - x19 holds the running engine's data-stack pointer. src/arch/arm64/mnem.f
\     names it (`19 constant XDS`), src/habu/rt.f's push and pop are stores and
\     loads through it, and src/habu/habu2.f measures the interpreter's stack
\     depth as (XDS - S0) / 8 - so every word the engine calls finds its operands
\     through this one register and leaves its results through it. It is not a
\     register a routine may be given: a routine that wrote to it would move the
\     caller's stack under the caller. It is excluded from the general-register
\     mask for the same reason x18 is, which is what makes "the allocator may
\     never hand out the data-stack pointer" a fact about what a contract can BE
\     rather than a check some pass has to remember;
\   - the D-register file has no reserved member, so all 32 are nameable;
\   - a frame slot is reached by an unsigned-offset load or store, whose offset
\     field is twelve bits scaled by the access width, and whose scale division
\     `SCALE/` refuses an offset it would round - which is exactly natural
\     alignment. That fixes both how far a slot can sit from the stack pointer
\     and the largest frame this contract can describe.
\ There is no halfword slot width because the modelled vocabulary has no halfword
\ load or store: its memory forms are Ldr/Str (eight bytes), Ldrw/Strw (four) and
\ Ldrb/Strb (one).
\
\ WHY THE DESTROYED SET IS COMPLETE AND THE PRESERVED SET IS DERIVED. A contract
\ that stored both could say a register is preserved and destroyed at once, and
\ two readers of one contract would then disagree. The destroyed set is therefore
\ total - it names every register the routine may write and not restore - and what
\ the routine preserves is computed from it. For the same reason the link register
\ is NOT a member of the general-register sets even though x30 is a general
\ register: whether the caller's return address survives is a different question
\ from whether some scratch register does, and one fact must have one owner.
\ Register 31 is not a member either - in the encoding it is the zero register or
\ the stack pointer depending on the form, and neither holds routine state. The
\ stack pointer's effect is the frame and delta fields.
\
\ WHAT THE VALIDATOR OWNS. Only whether the declared facts can all be true of one
\ routine. A routine whose control comes back must still hold the caller's return
\ address; a routine that never returns cannot deliver a result; a stack pointer
\ that ends above the caller's has discarded the caller's frame. Whether a
\ contract is TRUE of the instructions actually emitted under its label is a
\ different question with a different owner - the emitted control-flow verifier -
\ and collapsing the two would make a contract undeclarable before the code it
\ describes exists.
\
\ IDENTITY. A64EFF:DIGEST is SHA-256 over the canonical preimage: the
\ domain-separation tag, the schema version, and one eight-byte slot per field in
\ declaration order. The per-family codes below are stable wire codes: a variant
\ may be added to a family, but an existing variant's code may never be
\ renumbered without bumping SCHEMA.
\
\ FORGERY. `routine` is a public family, so its generated MAKE can assemble twelve
\ field values that never passed the checked constructor, and `placeseq` likewise
\ can assemble a cell that is not a list at all. Every word here whose result
\ carries identity or a decision - VALIDATE, SAME?, ENCODE, DIGEST, the derived
\ interface, preserved and writable sets, RETURNS?, CHECK-SLOT, and each of the
\ list readers - revalidates its input first. The plain field readers do not:
\ they only project a value the caller holds.
\
\ WHY THE RECORD IS FLAT. Grouping the three roles of a register file into a
\ nested record would read better, but a multi-cell value cannot be bound to a
\ typed local today (the layout-polymorphic parameter capability is still open),
\ so every reader of a middle field would have to dispose of a nested value it
\ cannot name. Twelve single-cell fields keep every word in this file checkable
\ with the types the checker has now. The field NAMES carry the grouping.
\
\ NOT MODELLED YET, deliberately, each with a named owner. The kernel-entry
\ register convention of a syscall - which registers the Svc form reads and
\ returns - is ABI-specific and belongs with the target contract, so this file
\ records only THAT a routine enters the kernel. Signal and crash-handler entry
\ state is the asynchronous-ABI schema's subject. What LIVES in a frame slot is an
\ indexed record of the A64IR dialect; this file owns the frame region those slots
\ must lie inside and the rule that decides whether a slot is addressable at all.

require lib/prelude.f
require lib/errors.f
require src/compiler/digest.f

package A64EFF
public

\ ---- register sets -----------------------------------------------------------
\ Two nominal one-field records over a bit mask, one per register file, so a set
\ of general registers cannot be passed where a set of floating registers is
\ wanted and neither can be confused with a bare integer. Bit i names register i.

STRUCTURE gprs 0 DERIVE eq
   FIELD bits n
;STRUCTURE

STRUCTURE fprs 0 DERIVE eq
   FIELD bits n
;STRUCTURE

\ ---- what one position of a convention names ---------------------------------
\ A place is where one argument arrives or one returned value leaves. There are
\ two kinds and the closed family says so, which is what makes every reader of a
\ position answer for both: a general register, or a slot of the caller's data
\ stack. The two codes are stable wire codes - they are the kind bit of the
\ packing below, so they ride in the digest - and neither may be renumbered
\ without bumping SCHEMA.
ENUM pkind DERIVE eq
   gpr
   dslot
;ENUM

\ ---- an ordered place list ---------------------------------------------------
\ What a set cannot say: where argument two arrives. A nominal one-field record
\ again, but over a packed ORDERED list - element i is the place at position i
\ and the length rides above them - so it cannot be confused with a set, with the
\ other file's set, or with a bare integer.
\
\ WHY IT IS PACKED INTO ONE CELL. A contract field has to be one cell: a value of
\ more than one cell cannot be bound to a typed local today, which is the same
\ reason the record below is flat. It fits without squeezing because a place is a
\ kind bit over a five-bit payload - five bits being the register operand field,
\ the fact that makes a file 32 registers - so one cell holds ten positions and
\ their count with bits to spare.
\
\ WHY THE KIND RIDES IN THE ELEMENT AND NOT IN A SECOND LIST. A second parallel
\ list would be a second field, and a contract with a place list of four and a
\ kind list of three would be two statements about one convention that could
\ disagree - exactly what making the interface ordered was for. Packing the kind
\ into the element makes a place one value with one spelling: the packing stays
\ canonical because every bit past the last position is zero, so one list has
\ exactly one cell and A64EFF:DIGEST, which stores that cell, agrees with SAME?
\ rather than approximating it. The cost is two positions: ten instead of the
\ twelve a five-bit element held.
STRUCTURE placeseq 0 DERIVE eq
   FIELD bits n
;STRUCTURE

\ ---- the condition flags -----------------------------------------------------
\ NZCV is a one-register file whose legal role combinations are few enough to
\ name, which is better than three independent flags that could spell a state no
\ routine has.
\   untouched      - neither read nor written
\   clobbered      - written, and what is left behind means nothing to the caller
\   delivered      - written, and the caller may branch on what is left
\   read-preserved - read at entry and left as it was found
\   read-clobbered - read at entry and then destroyed
\
\ `delivered` and not `result`: a variant name becomes a constructor word, so the
\ declaration gate (src/core/generated-declaration.f, throw 7110) refuses a
\ variant that collides with a visible type family, and `result` is the family
\ lib/adt/result.f declares. The old spelling made this whole file - and every
\ codegen suite downstream of it - fail to load the moment anything on the same
\ command line required result<T,E>.
ENUM nzcv DERIVE eq
   untouched
   clobbered
   delivered
   read-preserved
   read-clobbered
;ENUM

\ ---- the link register -------------------------------------------------------
\ Does x30 still hold the caller's return address where control leaves? A call
\ writes it, so a routine that calls and then comes back has to save and restore.
ENUM link DERIVE eq
   preserved
   clobbered
;ENUM

\ ---- how control leaves ------------------------------------------------------
\   returns   - back to the caller, through the Ret form
\   tail-call - by branching to another routine, which returns to OUR caller
\   no-return - control never comes back: a trap, an exit, or a handler return
ENUM control DERIVE eq
   returns
   tail-call
   no-return
;ENUM

\ ---- what the routine does besides compute -----------------------------------
\ A set rather than a choice, because a routine can do several. Each bit is named
\ for the instruction form in the modelled vocabulary that creates it.
STRUCTURE traits 0 DERIVE eq
   FIELD bits n
;STRUCTURE

\ ---- the routine contract ----------------------------------------------------
\ Fields in declaration order, deepest stack field first. It does not DERIVE eq -
\ a structure field is not a derivable role - so SAME? below is the hand-written
\ field-by-field identity, and it is the equality the digest is proved to agree
\ with. `gpr-arg` is the register each argument arrives in and `gpr-out` the
\ register each returned value leaves in, both position by position; the sets a
\ reader used to find in their place are derived from them below. `frame` is how
\ far below the entry stack pointer the routine's own frame reaches, in bytes;
\ `sp-delta` is the net stack-pointer change where control leaves, which is zero
\ or negative.
STRUCTURE routine 0
   FIELD gpr-arg placeseq
   FIELD gpr-out placeseq
   FIELD gpr-clobber gprs
   FIELD fpr-live-in fprs
   FIELD fpr-result fprs
   FIELD fpr-clobber fprs
   FIELD nzcv nzcv
   FIELD link link
   FIELD control control
   FIELD traits traits
   FIELD frame n
   FIELD sp-delta n
;STRUCTURE

private

\ ---- the machine facts this schema is bounded by -----------------------------
\ Each one is the assembler's. test/compiler/a64-effect.f reads the assembler's
\ own constant and asserts it against the public reader below, so a bound that
\ moved there reddens this schema instead of silently disagreeing with it.

5 constant REG-BITS       \ a register operand is a five-bit field
1 REG-BITS lshift constant FILE-N        \ registers per file, which is that field's reach
18 constant RESERVED-N    \ x18, refused by XREG? in every X-register operand slot
30 constant LINK-N        \ x30, the link register, which has its own contract field
31 constant ZERO-N        \ operand 31: the zero register, or the stack pointer

\ The registers this TARGET gives another owner: the platform register and the
\ two operand slots that are not general state. Everything the ENGINE occupies is
\ not this file's to know - src/habu/layout.f declares its own register
\ assignments and derives ENGINE-GPR:MASK from them, and that mask is the one
\ authority. This file used to name x19 here as a fifth constant, which is
\ exactly the second copy that let x20, x26, x27 and x28 through: the engine
\ claimed four more registers and nothing propagated (CG-13).
1 RESERVED-N lshift
   1 LINK-N lshift or
   1 ZERO-N lshift or
constant TARGET-RESERVED-MASK

\ Every register a routine may not hold state in, from both owners, in one place.
TARGET-RESERVED-MASK ENGINE-GPR:MASK or constant RESERVED-MASK

\ The general registers a routine CAN hold state in: the whole file less that.
1 FILE-N lshift 1 -  RESERVED-MASK invert and  constant GPR-MASK

1 FILE-N lshift 1 - constant FPR-MASK

\ ---- how an ordered place list is packed --------------------------------------
\ Positions from the bottom of the cell, six bits each - one kind bit over a
\ five-bit payload - and the length in the bits left over at the top. How many
\ positions there are is therefore not a number chosen here: it is how many of
\ that element one cell holds once the length has its own room, and the length
\ field is wide enough for it.
CELL 8 * constant SEQ-BITS               \ bits in the one cell a list occupies
4 constant SEQ-LEN-BITS                  \ the length, above the last position
REG-BITS 1+ constant PLACE-BITS          \ a payload and the bit that says what it is
SEQ-BITS SEQ-LEN-BITS - constant SEQ-LEN-SHIFT
SEQ-LEN-SHIFT PLACE-BITS / constant SEQ-MAX-N   \ positions one cell holds
1 REG-BITS lshift 1 - constant PAY-MASK  \ the payload: a register number, or a data-stack slot index
1 REG-BITS lshift constant KIND-BIT      \ set on a data-stack slot, clear on a register
1 PLACE-BITS lshift 1 - constant PLACE-MASK
1 SEQ-LEN-BITS lshift 1 - constant SEQ-LEN-MASK

\ The unsigned-offset load and store field: twelve bits, scaled by the access
\ width. The widest access moves eight bytes, so the deepest byte a slot can sit
\ at is (2^12 - 1) * 8, and the largest describable frame is that rounded down to
\ the stack alignment.
1 12 lshift 1 - constant OFF-MAX      \ largest scaled offset the field holds
8 constant WIDEST                     \ bytes moved by the Ldr and Str forms
16 constant SP-ALIGN-N                \ the stack pointer is 16-byte aligned

\ The unscaled load and store field: nine bits read as a signed number of BYTES,
\ so an access can name a byte below the register it is taken from as well as
\ above it. It is a second field on the same instruction group rather than a
\ second group, and it is what makes an access below a base expressible at all.
\ The number below is the DEEPEST byte under the base such a field can name.
1 8 lshift constant BACK-MAX-N

OFF-MAX WIDEST * dup SP-ALIGN-N mod - constant FRAME-MAX-N

\ ---- trait bits --------------------------------------------------------------
$1 constant BIT-CALL       \ contains a direct call: the Bl form
$2 constant BIT-INDIRECT   \ contains an indirect call: the Blr form
$4 constant BIT-SYSCALL    \ enters the kernel: the Svc form

BIT-CALL BIT-INDIRECT or BIT-SYSCALL or constant BIT-ALL

: MK-G ( n -- A64EFF:gprs )       A64EFF-GPRS:MAKE ;
: G-BITS ( A64EFF:gprs -- n )     A64EFF-GPRS:UNMAKE ;
: MK-F ( n -- A64EFF:fprs )       A64EFF-FPRS:MAKE ;
: F-BITS ( A64EFF:fprs -- n )     A64EFF-FPRS:UNMAKE ;
: MK-T ( n -- A64EFF:traits )     A64EFF-TRAITS:MAKE ;
: T-BITS ( A64EFF:traits -- n )   A64EFF-TRAITS:UNMAKE ;
: MK-S ( n -- A64EFF:placeseq )     A64EFF-PLACESEQ:MAKE ;
: S-BITS ( A64EFF:placeseq -- n )   A64EFF-PLACESEQ:UNMAKE ;

\ ---- stable wire codes -------------------------------------------------------
\ One injective code per closed family. These fix the digest; see the header.
: NZCV-CODE ( A64EFF:nzcv -- n )
   MATCH nzcv
      untouched      OF 0 ENDOF
      clobbered      OF 1 ENDOF
      delivered      OF 2 ENDOF
      read-preserved OF 3 ENDOF
      read-clobbered OF 4 ENDOF
   ;MATCH ;

: LINK-CODE ( A64EFF:link -- n )
   MATCH link
      preserved OF 0 ENDOF
      clobbered OF 1 ENDOF
   ;MATCH ;

: CONTROL-CODE ( A64EFF:control -- n )
   MATCH control
      returns   OF 0 ENDOF
      tail-call OF 1 ENDOF
      no-return OF 2 ENDOF
   ;MATCH ;

\ ---- per-field rules ---------------------------------------------------------

: GPR-CK ( n -- n )
   dup GPR-MASK invert and 0<> if E-A64EFF-GPR throw then ;

: FPR-CK ( n -- n )
   dup FPR-MASK invert and 0<> if E-A64EFF-FPR throw then ;

: TRAIT-CK ( n -- n )
   dup BIT-ALL invert and 0<> if E-A64EFF-TRAIT throw then ;

\ A register number for the single-register constructors. Outside the file is the
\ same refusal as a bit past the file.
: REG-CK ( n -- n )
   dup 0 < over FILE-N >= or if E-A64EFF-GPR throw then ;

: FREG-CK ( n -- n )
   dup 0 < over FILE-N >= or if E-A64EFF-FPR throw then ;

\ ---- the rules of an ordered place list --------------------------------------
\ Reading one packed list. Nothing below is public: a caller reaches a position
\ through the readers further down, which validate the whole list first, so there
\ is no route to an element of a list that was never checked.
: SEQ-LEN-OF ( n -- n )
   SEQ-LEN-SHIFT rshift SEQ-LEN-MASK and ;

\ The whole element at one position - kind bit and payload together - which is
\ the value two positions are compared as, so one register and one data-stack
\ slot with the same number are two different places.
: SEQ-AT ( n n -- n )
   {: w:n p:n :}
   w p PLACE-BITS * rshift PLACE-MASK and ;

: PLACE-SLOT? ( n -- bool )
   KIND-BIT and 0<> ;

: PLACE-PAY ( n -- n )
   PAY-MASK and ;

\ A register a routine can hold state in, decided by the set rule rather than by
\ a second list of what is forbidden: x18, x19, x30 and 31 fail here because no
\ general-register set may name them either.
: SEQ-REG-CK ( n -- )
   REG-CK 1 swap lshift GPR-CK drop ;

\ A data-stack slot index the payload field holds. How deep a caller's stack may
\ be is the caller's business; what this schema owns is that a position's payload
\ is five bits wide, so an index past that has nowhere to be written down.
: SEQ-SLOT-CK ( n -- )
   dup 0 < over PAY-MASK > or if E-A64EFF-SEQ throw then drop ;

: SEQ-PLACE-CK ( n -- )
   dup PLACE-SLOT? if PLACE-PAY SEQ-SLOT-CK exit then
   PLACE-PAY SEQ-REG-CK ;

\ Does the place at this position already appear before it? A caller cannot put
\ two different values in one place, so one place is one position.
: SEQ-REPEATS? ( n n -- bool )
   {: w:n p:n :}
   false
   p 0 ?do
      w i SEQ-AT  w p SEQ-AT  = if drop true leave then
   loop ;

\ A packed list that can be a convention: a length the cell holds, every element
\ a place a caller could really use, no place twice, and nothing left standing
\ past the last position - which is what makes the packing canonical, so two
\ lists are the same value exactly when their cells are equal.
: SEQ-CK ( n -- n )
   dup {: w:n :}
   w SEQ-LEN-OF {: len:n :}
   len SEQ-MAX-N > if E-A64EFF-SEQ throw then
   len 0 ?do
      w i SEQ-AT SEQ-PLACE-CK
      w i SEQ-REPEATS? if E-A64EFF-SEQ throw then
   loop
   SEQ-MAX-N len ?do
      w i SEQ-AT 0<> if E-A64EFF-SEQ throw then
   loop ;

\ The set of registers a list names, which is how the two derived reader sets
\ below are answered. A data-stack slot is not a register and contributes
\ nothing: a routine whose whole interface is data-stack slots reads, returns and
\ destroys no register on account of its convention.
: SEQ-MASK ( n -- n )
   dup SEQ-LEN-OF {: w:n len:n :}
   0
   len 0 ?do
      w i SEQ-AT dup PLACE-SLOT? if drop else PLACE-PAY 1 swap lshift or then
   loop ;

\ How many positions of a list are data-stack slots. Zero says every position is
\ a register, the length says every position is a slot, and anything between says
\ the list mixes the two kinds - which is a convention this file can describe and
\ no pass of the chain has a rule for yet.
: SEQ-SLOTS-OF ( n -- n )
   dup SEQ-LEN-OF {: w:n len:n :}
   0
   len 0 ?do
      w i SEQ-AT PLACE-SLOT? if 1+ then
   loop ;

\ A result is a register the caller reads; a destroyed register holds nothing the
\ caller may read. One register cannot be both.
: ROLE-CK ( n n -- )
   {: res:n clob:n :}
   res clob and 0<> if E-A64EFF-ROLE throw then ;

: ALIGNED? ( n -- bool )
   SP-ALIGN-N mod 0= ;

: FRAME-CK ( n -- )
   {: size:n :}
   size 0 < size FRAME-MAX-N > or if E-A64EFF-FRAME throw then
   size ALIGNED? 0= if E-A64EFF-FRAME throw then ;

: DELTA-CK ( n n -- )
   {: size:n delta:n :}
   delta 0 > if E-A64EFF-SP throw then
   delta ALIGNED? 0= if E-A64EFF-SP throw then
   delta size negate < if E-A64EFF-SP throw then ;

\ ---- whole-contract rules ----------------------------------------------------
\ Only the facts that need more than one field to decide.

: RETURNING? ( A64EFF:control -- bool )
   MATCH control
      returns   OF true ENDOF
      tail-call OF true ENDOF
      no-return OF false ENDOF
   ;MATCH ;

\ A routine control comes back from - directly, or through a tail callee that
\ returns to our caller - leaves the stack pointer where it found it.
: BALANCE-CK ( A64EFF:control n -- )
   {: c:control delta:n :}
   c RETURNING? 0= if exit then
   delta 0<> if E-A64EFF-SP throw then ;

\ Both a return and a tail call end by jumping to the address in x30: the tail
\ callee's own return does. Either way a destroyed link register has nowhere to
\ go back to.
: LINK-CK ( A64EFF:control A64EFF:link -- )
   {: c:control l:link :}
   c RETURNING? 0= if exit then
   l A64EFF-LINK:CLOBBERED A64EFF-LINK:EQ if E-A64EFF-LINK throw then ;

\ Control that never comes back delivers nothing, so a declared result of any
\ kind contradicts it.
: RESULT-CK ( n n A64EFF:nzcv A64EFF:control -- )
   {: gres:n fres:n z:nzcv c:control :}
   c RETURNING? if exit then
   gres 0<> fres 0<> or
   z A64EFF-NZCV:DELIVERED A64EFF-NZCV:EQ or
   if E-A64EFF-CONTROL throw then ;

\ ---- canonical preimage ------------------------------------------------------
\ Version 3: the two interface slots hold ordered lists of PLACES rather than of
\ registers, so the same cell means something different than it did - a position
\ is now six bits and one of them says whether the payload is a register or a
\ data-stack slot - and the version says so instead of two schemas sharing one
\ digest. Version 2 was the step before, where those slots became ordered lists
\ rather than sets.
3 constant SCHEMA
14 constant SLOTS
0 constant SLOT-TAG
1 constant SLOT-SCHEMA
2 constant SLOT-GPR-IN
3 constant SLOT-GPR-RES
4 constant SLOT-GPR-CLOB
5 constant SLOT-FPR-IN
6 constant SLOT-FPR-RES
7 constant SLOT-FPR-CLOB
8 constant SLOT-NZCV
9 constant SLOT-LINK
10 constant SLOT-CONTROL
11 constant SLOT-TRAITS
12 constant SLOT-FRAME
13 constant SLOT-DELTA

SLOTS CDIGEST:SLOT-BYTES * constant PRE-BYTES
create PRE PRE-BYTES allot

\ The widths the modelled memory forms move.
: WIDTH-OK? ( n -- bool )
   {: w:n :}
   w 1 = w 4 = or w WIDEST = or ;

public

\ ---- the machine facts, for a consumer that has to agree with them ------------
: FILE-SIZE ( -- n )      FILE-N ;
: RESERVED-GPR ( -- n )   RESERVED-N ;
: LINK-GPR ( -- n )       LINK-N ;
: ZERO-GPR ( -- n )       ZERO-N ;

\ The register the running engine keeps its data-stack pointer in. A pass that
\ emits an access to the caller's stack asks for it here rather than writing 19,
\ so the one place that says why no routine may hold state there is also the one
\ place that says where it does appear. It is excluded from GPR-MASK above, so
\ no general-register set and no place list can name it and no contract that
\ hands it out can be built at all. The number itself comes from the engine's own
\ declaration (src/arch/arm64/mnem.f XDS, which src/habu/layout.f folds into
\ ENGINE-GPR:MASK): this file reports the engine's register, it does not decide it.
: DSTACK-GPR ( -- n )     ENGINE-GPR:DSTACK ;

\ Every register no routine may hold state in, from both owners at once - the
\ target's platform/link/zero operands and everything the engine occupies. A
\ consumer that wants to ask "may a routine use this register" asks here; there
\ is no second list to consult and none to keep in step.
: RESERVED-GPRS ( -- n )  RESERVED-MASK ;
: ENGINE-GPRS ( -- n )    ENGINE-GPR:MASK ;

\ The same operand number, named for what it means in the forms that reach a
\ frame slot: there operand 31 is the stack pointer rather than the zero
\ register. A pass emitting a frame access asks for it here instead of writing
\ the number, so the one place that knows why 31 holds no routine state is also
\ the one place that says where it does appear.
: SP-GPR ( -- n )         ZERO-N ;
: SP-ALIGN ( -- n )       SP-ALIGN-N ;
: FRAME-MAX ( -- n )      FRAME-MAX-N ;

\ The frame a routine needing `want` bytes below its entry declares: the request
\ rounded up to the stack alignment, and no frame at all for a routine that needs
\ nothing. It is here rather than in either caller because two of them need the
\ same number now - src/compiler/native/abi.f turns a spill count into a
\ declaration, and src/compiler/native/regalloc.f measures the frame a walk
\ proved its routine needs - and the two have to agree exactly or the validator
\ refuses the difference. One authority rather than two that drift.
: FRAME-ROUND ( n -- n )
   {: want:n :}
   want 0= if 0 exit then
   want SP-ALIGN-N 1- +  SP-ALIGN-N /  SP-ALIGN-N * ;

\ The deepest byte an access of this width can name through the unsigned-offset
\ field. A consumer placing slots asks this rather than repeating the arithmetic.
\ A width no load or store form moves has no reach, so it is refused here.
: SLOT-REACH ( n -- n )
   dup WIDTH-OK? 0= if E-A64EFF-SLOT throw then
   OFF-MAX * ;

\ And the deepest byte UNDER the base that an access can name, through the
\ unscaled signed field. It takes no width because that field is not scaled: the
\ same nine bits mean the same bytes whatever the access moves. It is answered
\ as a positive magnitude, so a consumer writes `negate` where it means an
\ offset and reads it as a depth where it means a bound.
: SLOT-BACK ( -- n )
   BACK-MAX-N ;

\ ---- register sets -----------------------------------------------------------
: GPR-SET ( n -- A64EFF:gprs )    GPR-CK MK-G ;
: GPRS-N ( A64EFF:gprs -- n )     G-BITS ;
: FPR-SET ( n -- A64EFF:fprs )    FPR-CK MK-F ;
: FPRS-N ( A64EFF:fprs -- n )     F-BITS ;

: GPR-NONE ( -- A64EFF:gprs )     0 MK-G ;
: GPR-ALL ( -- A64EFF:gprs )      GPR-MASK MK-G ;
: FPR-NONE ( -- A64EFF:fprs )     0 MK-F ;
: FPR-ALL ( -- A64EFF:fprs )      FPR-MASK MK-F ;

\ The set holding exactly one register. A register this schema says no routine
\ can hold state in is refused here rather than silently dropped.
: GPR-REG ( n -- A64EFF:gprs )    REG-CK 1 swap lshift GPR-CK MK-G ;
: FPR-REG ( n -- A64EFF:fprs )    FREG-CK 1 swap lshift MK-F ;

: GPR-WITH ( A64EFF:gprs A64EFF:gprs -- A64EFF:gprs )
   G-BITS swap G-BITS or GPR-CK MK-G ;

: FPR-WITH ( A64EFF:fprs A64EFF:fprs -- A64EFF:fprs )
   F-BITS swap F-BITS or FPR-CK MK-F ;

: GPR-WITHOUT ( A64EFF:gprs A64EFF:gprs -- A64EFF:gprs )
   {: set:gprs less:gprs :}
   set G-BITS GPR-CK less G-BITS GPR-CK invert and MK-G ;

: FPR-WITHOUT ( A64EFF:fprs A64EFF:fprs -- A64EFF:fprs )
   {: set:fprs less:fprs :}
   set F-BITS FPR-CK less F-BITS FPR-CK invert and MK-F ;

\ Does the set hold every register of the probe set?
: GPR-HAS? ( A64EFF:gprs A64EFF:gprs -- bool )
   {: set:gprs probe:gprs :}
   probe G-BITS GPR-CK {: want:n :}
   set G-BITS GPR-CK want and want = ;

: FPR-HAS? ( A64EFF:fprs A64EFF:fprs -- bool )
   {: set:fprs probe:fprs :}
   probe F-BITS FPR-CK {: want:n :}
   set F-BITS FPR-CK want and want = ;

\ ---- ordered place lists ------------------------------------------------------
\ The convention that names nothing: a routine whose arguments arrive, or whose
\ results leave, in no place this contract has an opinion about. It is also what
\ a routine that takes or returns nothing declares, and those are the same
\ statement - there is no position to say anything about either way.
: SEQ-NONE ( -- A64EFF:placeseq )   0 MK-S ;

\ How many positions one list can hold at all. A consumer that walks positions
\ asks rather than assuming the packing.
: SEQ-LIMIT ( -- n )              SEQ-MAX-N ;

\ The largest data-stack slot index a position can name, which is the reach of a
\ position's payload field. A consumer placing slots asks rather than assuming.
: SEQ-SLOT-LIMIT ( -- n )         PAY-MASK ;

private

\ The list with one more place after its last position. A place the list already
\ names is refused here rather than appended: two positions in one place is a
\ convention no caller could satisfy - it would have to put two different values
\ in one place - and that is exactly the shape a mistyped declaration takes.
: SEQ-PUT ( A64EFF:placeseq n -- A64EFF:placeseq )
   {: s:placeseq e:n :}
   s S-BITS SEQ-CK {: w:n :}
   w SEQ-LEN-OF {: len:n :}
   len SEQ-MAX-N >= if E-A64EFF-SEQ throw then
   e SEQ-PLACE-CK
   w  e len PLACE-BITS * lshift or  1 SEQ-LEN-SHIFT lshift +
   SEQ-CK MK-S ;

public

\ The list with one more REGISTER place after its last position.
: SEQ-WITH ( A64EFF:placeseq n -- A64EFF:placeseq )
   REG-CK SEQ-PUT ;

\ The list with one more DATA-STACK SLOT place after its last position. This is
\ the whole of what design section 7.6's convention needs to be sayable: argument
\ i arrives in slot i, result j leaves in slot j.
: SEQ-WITH-SLOT ( A64EFF:placeseq n -- A64EFF:placeseq )
   dup 0 < over PAY-MASK > or if E-A64EFF-SEQ throw then
   KIND-BIT or SEQ-PUT ;

: SEQ-LEN ( A64EFF:placeseq -- n )
   S-BITS SEQ-CK SEQ-LEN-OF ;

private

: SEQ-ELEM ( A64EFF:placeseq n -- n )
   {: s:placeseq p:n :}
   s S-BITS SEQ-CK {: w:n :}
   p 0 < p w SEQ-LEN-OF >= or if E-A64EFF-SEQ throw then
   w p SEQ-AT ;

public

\ Which kind of place one position names. Every reader of a position goes through
\ this or is refused: there is no word that answers a payload without saying what
\ it is, so a pass that treated a slot index as a register number would have had
\ to ask for the register of a slot and be told no.
: SEQ-KIND@ ( A64EFF:placeseq n -- A64EFF:pkind )
   SEQ-ELEM PLACE-SLOT? if A64EFF-PKIND:DSLOT exit then A64EFF-PKIND:GPR ;

\ The register at one position. A position holding a data-stack slot is refused
\ rather than answered with an index that would read as a register number.
: SEQ-REG@ ( A64EFF:placeseq n -- n )
   SEQ-ELEM dup PLACE-SLOT? if E-A64EFF-KIND throw then PLACE-PAY ;

\ The data-stack slot index at one position, refused the same way in reverse.
: SEQ-SLOT@ ( A64EFF:placeseq n -- n )
   SEQ-ELEM dup PLACE-SLOT? 0= if E-A64EFF-KIND throw then PLACE-PAY ;

\ How many positions of the list are data-stack slots. Zero and the whole length
\ are the two homogeneous conventions; anything between mixes the kinds, which is
\ describable here and has no lowering rule anywhere in the chain yet.
: SEQ-SLOTS ( A64EFF:placeseq -- n )
   S-BITS SEQ-CK SEQ-SLOTS-OF ;

\ Which registers a list names, forgetting the order. Data-stack slots are not
\ registers and are not in it.
: SEQ-SET ( A64EFF:placeseq -- A64EFF:gprs )
   S-BITS SEQ-CK SEQ-MASK MK-G ;

\ ---- traits ------------------------------------------------------------------
: TRAIT-SET ( n -- A64EFF:traits )   TRAIT-CK MK-T ;
: TRAITS-N ( A64EFF:traits -- n )    T-BITS ;
: TRAITS-NONE ( -- A64EFF:traits )   0 MK-T ;
: T-CALL ( -- A64EFF:traits )        BIT-CALL MK-T ;
: T-INDIRECT ( -- A64EFF:traits )    BIT-INDIRECT MK-T ;
: T-SYSCALL ( -- A64EFF:traits )     BIT-SYSCALL MK-T ;

: TRAITS-WITH ( A64EFF:traits A64EFF:traits -- A64EFF:traits )
   T-BITS swap T-BITS or TRAIT-CK MK-T ;

: TRAITS-HAS? ( A64EFF:traits A64EFF:traits -- bool )
   {: set:traits probe:traits :}
   probe T-BITS TRAIT-CK {: want:n :}
   set T-BITS TRAIT-CK want and want = ;

\ ---- construction and validation ---------------------------------------------
\ The production entry point. A combination that cannot be true of one routine
\ throws a named error and no contract value is produced.
: ROUTINE ( A64EFF:placeseq A64EFF:placeseq A64EFF:gprs A64EFF:fprs A64EFF:fprs A64EFF:fprs A64EFF:nzcv A64EFF:link A64EFF:control A64EFF:traits n n -- A64EFF:routine )
   {: gi:placeseq gr:placeseq gc:gprs fi:fprs fr:fprs fc:fprs z:nzcv
      l:link c:control t:traits size:n delta:n :}
   gi S-BITS SEQ-CK drop
   gr S-BITS SEQ-CK SEQ-MASK gc G-BITS GPR-CK ROLE-CK
   fi F-BITS FPR-CK drop
   fr F-BITS FPR-CK fc F-BITS FPR-CK ROLE-CK
   t T-BITS TRAIT-CK drop
   size FRAME-CK
   size delta DELTA-CK
   c delta BALANCE-CK
   c l LINK-CK
   gr S-BITS SEQ-MASK fr F-BITS z c RESULT-CK
   gi gr gc fi fr fc z l c t size delta A64EFF-ROUTINE:MAKE ;

\ Recheck a contract that may have been assembled by the generated constructor.
: VALIDATE ( A64EFF:routine -- A64EFF:routine )
   A64EFF-ROUTINE:UNMAKE ROUTINE ;

\ ---- field readers -----------------------------------------------------------
\ A projection of a value the caller already holds; nothing here revalidates.
: ARGS@ ( A64EFF:routine -- A64EFF:placeseq )
   A64EFF-ROUTINE:UNMAKE drop drop drop drop drop drop drop drop drop drop drop ;

: RESULTS@ ( A64EFF:routine -- A64EFF:placeseq )
   A64EFF-ROUTINE:UNMAKE drop drop drop drop drop drop drop drop drop drop nip ;

: GPR-CLOBBER@ ( A64EFF:routine -- A64EFF:gprs )
   A64EFF-ROUTINE:UNMAKE drop drop drop drop drop drop drop drop drop nip nip ;

: FPR-IN@ ( A64EFF:routine -- A64EFF:fprs )
   A64EFF-ROUTINE:UNMAKE drop drop drop drop drop drop drop drop nip nip nip ;

: FPR-RESULT@ ( A64EFF:routine -- A64EFF:fprs )
   A64EFF-ROUTINE:UNMAKE drop drop drop drop drop drop drop nip nip nip nip ;

: FPR-CLOBBER@ ( A64EFF:routine -- A64EFF:fprs )
   A64EFF-ROUTINE:UNMAKE drop drop drop drop drop drop nip nip nip nip nip ;

: NZCV@ ( A64EFF:routine -- A64EFF:nzcv )
   A64EFF-ROUTINE:UNMAKE drop drop drop drop drop nip nip nip nip nip nip ;

: LINK@ ( A64EFF:routine -- A64EFF:link )
   A64EFF-ROUTINE:UNMAKE drop drop drop drop nip nip nip nip nip nip nip ;

: CONTROL@ ( A64EFF:routine -- A64EFF:control )
   A64EFF-ROUTINE:UNMAKE drop drop drop nip nip nip nip nip nip nip nip ;

: TRAITS@ ( A64EFF:routine -- A64EFF:traits )
   A64EFF-ROUTINE:UNMAKE drop drop nip nip nip nip nip nip nip nip nip ;

: FRAME@ ( A64EFF:routine -- n )
   A64EFF-ROUTINE:UNMAKE drop nip nip nip nip nip nip nip nip nip nip ;

: DELTA@ ( A64EFF:routine -- n )
   A64EFF-ROUTINE:UNMAKE nip nip nip nip nip nip nip nip nip nip nip ;

\ ---- derived facts -----------------------------------------------------------
\ Which registers the interface lists name, as sets. A caller that only wants to
\ know whether a register takes part - and every caller before the convention was
\ ordered wanted exactly that - asks here rather than walking positions, and the
\ answer cannot contradict the list because there is nothing else to read it out
\ of.
: GPR-IN@ ( A64EFF:routine -- A64EFF:gprs )
   VALIDATE ARGS@ SEQ-SET ;

: GPR-RESULT@ ( A64EFF:routine -- A64EFF:gprs )
   VALIDATE RESULTS@ SEQ-SET ;

\ What survives the routine: every register of the file it neither returns nor
\ destroys. Derived, never stored, so it cannot contradict the destroyed set.
: GPR-PRESERVED ( A64EFF:routine -- A64EFF:gprs )
   VALIDATE A64EFF-ROUTINE:UNMAKE
   drop drop drop drop drop drop   \ delta, frame, traits, control, link, nzcv
   drop drop drop                  \ the floating sets
   {: gi:placeseq gr:placeseq gc:gprs :}
   GPR-MASK gr S-BITS SEQ-MASK invert and gc G-BITS invert and MK-G ;

\ Every register the routine may WRITE: the ones it destroys, plus the ones it
\ returns a value in. They are two roles and one register cannot be both, which
\ is why they are two fields - but a register allocator does not care why a
\ register may be written, only that it may, so the set it may hand out is this
\ one and not the destroyed set alone. Derived for the same reason the preserved
\ set is: a stored copy could disagree with the two fields it is made of.
: GPR-WRITABLE ( A64EFF:routine -- A64EFF:gprs )
   VALIDATE A64EFF-ROUTINE:UNMAKE
   drop drop drop drop drop drop   \ delta, frame, traits, control, link, nzcv
   drop drop drop                  \ the floating sets
   {: gi:placeseq gr:placeseq gc:gprs :}
   gr S-BITS SEQ-MASK gc G-BITS or MK-G ;

: FPR-PRESERVED ( A64EFF:routine -- A64EFF:fprs )
   VALIDATE A64EFF-ROUTINE:UNMAKE
   drop drop drop drop drop drop   \ delta, frame, traits, control, link, nzcv
   {: fi:fprs fr:fprs fc:fprs :}
   drop drop drop                  \ the general interface lists and destroyed set
   FPR-MASK fr F-BITS invert and fc F-BITS invert and MK-F ;

\ The floating file's writable set, derived exactly as the general file's is:
\ what the routine destroys plus what it returns a value in. A register
\ allocator with a second file asks this question of the second file in the same
\ words it asks it of the first, so the two pools come from one rule rather than
\ from one rule and one special case.
: FPR-WRITABLE ( A64EFF:routine -- A64EFF:fprs )
   VALIDATE A64EFF-ROUTINE:UNMAKE
   drop drop drop drop drop drop   \ delta, frame, traits, control, link, nzcv
   {: fi:fprs fr:fprs fc:fprs :}
   drop drop drop                  \ the general interface lists and destroyed set
   fr F-BITS fc F-BITS or MK-F ;

: RETURNS? ( A64EFF:routine -- bool )
   VALIDATE CONTROL@ RETURNING? ;

\ ---- frame slots -------------------------------------------------------------
\ A slot the A64IR dialect wants to place: a byte offset from the deepest point
\ of the routine's own frame, and an access width. It is accepted only if a load
\ or store form can actually reach it - a width the vocabulary carries, an offset
\ the scale division will not round, an offset inside the declared frame, and an
\ offset inside the reach of that width's twelve-bit field. The routine is last
\ so the two numbers can be read into locals; a multi-cell value cannot be one.
: CHECK-SLOT ( n n A64EFF:routine -- )
   VALIDATE FRAME@ {: off:n width:n size:n :}
   width WIDTH-OK? 0= if E-A64EFF-SLOT throw then
   off 0 < if E-A64EFF-SLOT throw then
   off width mod 0<> if E-A64EFF-SLOT throw then
   off width + size > if E-A64EFF-SLOT throw then
   off width SLOT-REACH > if E-A64EFF-SLOT throw then ;

\ ---- identity ----------------------------------------------------------------
\ Field-by-field identity. Both inputs are revalidated first, so a forged
\ contract cannot be compared as if it were a declarable routine.
: SAME? ( A64EFF:routine A64EFF:routine -- bool )
   VALIDATE A64EFF-ROUTINE:UNMAKE
   {: ygi:placeseq ygr:placeseq ygc:gprs yfi:fprs yfr:fprs yfc:fprs yz:nzcv
      yl:link yc:control yt:traits ysize:n ydelta:n :}
   VALIDATE A64EFF-ROUTINE:UNMAKE
   {: xgi:placeseq xgr:placeseq xgc:gprs xfi:fprs xfr:fprs xfc:fprs xz:nzcv
      xl:link xc:control xt:traits xsize:n xdelta:n :}
   xgi ygi A64EFF-PLACESEQ:EQ
   xgr ygr A64EFF-PLACESEQ:EQ and
   xgc ygc A64EFF-GPRS:EQ and
   xfi yfi A64EFF-FPRS:EQ and
   xfr yfr A64EFF-FPRS:EQ and
   xfc yfc A64EFF-FPRS:EQ and
   xz yz A64EFF-NZCV:EQ and
   xl yl A64EFF-LINK:EQ and
   xc yc A64EFF-CONTROL:EQ and
   xt yt A64EFF-TRAITS:EQ and
   xsize ysize = and
   xdelta ydelta = and ;

\ The canonical preimage. The bytes live in this module and stay valid until the
\ next ENCODE call; DIGEST is the copy-free consumer.
: ENCODE ( A64EFF:routine -- ptr u8 n )
   VALIDATE A64EFF-ROUTINE:UNMAKE
   {: gi:placeseq gr:placeseq gc:gprs fi:fprs fr:fprs fc:fprs z:nzcv
      l:link c:control t:traits size:n delta:n :}
   CDIGEST:TAG-A64-ROUTINE PRE SLOT-TAG CDIGEST:SLOT!
   SCHEMA PRE SLOT-SCHEMA CDIGEST:SLOT!
   gi S-BITS PRE SLOT-GPR-IN CDIGEST:SLOT!
   gr S-BITS PRE SLOT-GPR-RES CDIGEST:SLOT!
   gc G-BITS PRE SLOT-GPR-CLOB CDIGEST:SLOT!
   fi F-BITS PRE SLOT-FPR-IN CDIGEST:SLOT!
   fr F-BITS PRE SLOT-FPR-RES CDIGEST:SLOT!
   fc F-BITS PRE SLOT-FPR-CLOB CDIGEST:SLOT!
   z NZCV-CODE PRE SLOT-NZCV CDIGEST:SLOT!
   l LINK-CODE PRE SLOT-LINK CDIGEST:SLOT!
   c CONTROL-CODE PRE SLOT-CONTROL CDIGEST:SLOT!
   t T-BITS PRE SLOT-TRAITS CDIGEST:SLOT!
   size PRE SLOT-FRAME CDIGEST:SLOT!
   delta PRE SLOT-DELTA CDIGEST:SLOT!
   PRE PRE-BYTES ;

: DIGEST ( A64EFF:routine -- CDIGEST:digest )
   ENCODE CDIGEST:COMPUTE ;

;package
