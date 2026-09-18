\ native-effect.f - the typed machine-state contract of one emitted ARM64 routine.
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
\ Habu convention passes floating values through canonical data-stack slots too,
\ so it has no ordered floating-register argument or result positions to state.
\
\ THE MACHINE IS A PARAMETER, NOT A CONSTANT. Every bound this schema used to
\ hold - a file of 32 registers, x30 is the link register, operand 31 is the
\ stack pointer, the stack is 16-byte aligned, a frame slot is reached by a
\ twelve-bit offset scaled by the access width, there is no halfword load - was
\ ARM64's, read off the instruction vocabulary that formal/Common/Insn.v models
\ and src/arch/arm64/asm.f emits. They are still exactly that, and they are now
\ declared where the machine that has them is: src/arch/arm64/machine.f, as one
\ description src/compiler/native/machine.f judges. A contract NAMES the machine
\ it is a contract over, in its `mach` field, so every rule below asks that
\ description instead of a constant, every reader of a contract has the machine
\ in its hand without being handed a second value that could be the wrong one,
\ and a second architecture supplies numbers rather than forking the 2,300 lines
\ of register allocator that read contracts. What stays here is what is true of
\ every machine: the roles, the ordered convention, the flags, the exits, the
\ rule that a routine control comes back from still holds its return address,
\ and the packing.
\
\ WHAT THE SCHEMA STILL FIXES FOR EVERY MACHINE is one number: a position of an
\ ordered place list carries a five-bit payload, so a list can name register 0
\ to 31 and data-stack slot 0 to 31 however the machine numbers its registers. A
\ machine with a wider file can hold contracts - it just cannot state a REGISTER
\ convention that names one of the registers above 31, and SEQ-WITH refuses that
\ rather than packing it away.
\
\ WHICH REGISTERS A CONTRACT MAY NAME IS THE MACHINE'S ANSWER, AND IT IS ASKED
\ AT THE CONTRACT. A set of registers is a value and not a statement about any
\ machine, so GPR-SET and SEQ-WITH judge only what this schema can represent. It
\ is ROUTINE that holds every named register to what the machine says a routine
\ may hold state in - the file less what the virtual machine, the link register
\ and the stack pointer took - so no pass ever sees a contract handing out a
\ register its machine reserved, which is the invariant those per-set refusals
\ were there for.
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
\ IDENTITY. NEFF:DIGEST is SHA-256 over the canonical preimage: the
\ domain-separation tag, the schema version, and one eight-byte slot per field in
\ declaration order. The per-family codes below are stable wire codes: a variant
\ may be added to a family, but an existing variant's code may never be
\ renumbered without bumping SCHEMA.
\
\ FORGERY. `routine` is a public family, so its generated MAKE can assemble
\ thirteen field values that never passed the checked constructor, and
\ `placeseq` likewise can assemble a cell that is not a list at all. Every word
\ here whose result carries identity or a decision - VALIDATE, SAME?, ENCODE,
\ DIGEST, the derived interface, preserved and writable sets, RETURNS?,
\ CHECK-SLOT, and each of the list readers - revalidates its input first. The
\ plain field readers do not: they only project a value the caller holds.
\
\ WHY THE RECORD IS FLAT. Grouping the three roles of a register file into a
\ nested record would read better, but a multi-cell value cannot be bound to a
\ typed local today (the layout-polymorphic parameter capability is still open),
\ so every reader of a middle field would have to dispose of a nested value it
\ cannot name. Thirteen single-cell fields keep every word in this file checkable
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
require src/compiler/native/machine.f

package NEFF
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
\ exactly one cell and NEFF:DIGEST, which stores that cell, agrees with SAME?
\ rather than approximating it. The cost is two positions: ten instead of the
\ twelve a five-bit element held.
STRUCTURE placeseq 0 DERIVE eq
   FIELD bits n
;STRUCTURE

\ ---- which convention the two place lists are stated in -----------------------
\ A DECLARATION AND NOT AN INFERENCE, WHICH IS THE WHOLE OF WHY IT EXISTS. An
\ empty place list says two different things - a routine that passes nothing, and
\ a routine whose placement this contract has no opinion about - and SEQ-NONE
\ above says so in as many words. So no reader can recover the convention by
\ looking at the lists: a routine of arity zero has an empty list under BOTH
\ conventions, and every pass that guessed from the lists guessed the same way
\ and got a ( -- ) routine wrong.
\
\   dstack   - every argument arrives in, and every result leaves in, a slot of
\              the caller's data stack. This is the convention a Habu word is
\              entered under, and the one src/compiler/native/abi.f declares.
\   register - every argument arrives in, and every result leaves in, a register.
\              A contract may still name no position at all, which is how a test
\              fixture says it has no opinion about an interface it is not about.
\
\ The two codes are stable wire codes and ride in the digest; neither may be
\ renumbered without bumping SCHEMA.
ENUM conv DERIVE eq
   dstack
   register
;ENUM

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
\ Does the link register still hold the caller's return address where control
\ leaves? A call writes it, so a routine that calls and then comes back has to
\ save and restore.
\   absent - this machine has no link register, so there is no return address in
\            one to preserve or destroy. It is a state of the same field rather
\            than a missing field, because "what does this routine do to the
\            link register" has one answer per contract and a machine without
\            one answers it too. A contract over a machine that HAS a link
\            register may not declare it, and a contract over a machine that has
\            none may declare nothing else.
ENUM link DERIVE eq
   preserved
   clobbered
   absent
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
\ with. `conv` comes first because it is what says how to READ the two lists
\ after it: a position means a register under one convention and a data-stack
\ slot under the other, and an empty list means nothing at all without it.
\ `gpr-arg` is the place each argument arrives in and `gpr-out` the place each
\ returned value leaves in, both position by position; the sets a
\ reader used to find in their place are derived from them below. `frame` is how
\ far below the entry stack pointer the routine's own frame reaches, in bytes;
\ `sp-delta` is the net stack-pointer change where control leaves, which is zero
\ or negative. `mach` is the machine every other field is stated about, and it
\ is last because that is the order the constructor takes its arguments in and
\ VALIDATE is exactly UNMAKE into it.
STRUCTURE routine 0
   FIELD conv conv
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
   FIELD mach NMACH:mach
;STRUCTURE

private

\ ---- what this schema can represent, whatever the machine ---------------------
\ One number, and it is about the packing below rather than about any register
\ file: a position of a place list carries a five-bit payload. The machines the
\ chain targets number 16 and 32 registers, so both fit; a wider file would fit
\ here too and only its registers above 31 would be unnameable in a convention.

5 constant REG-BITS       \ the payload of one position of a place list

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
\ A contiguous data-stack interface needs only its length. The reserved length
\ tag distinguishes that range from an explicitly ordered register/slot list.
SEQ-LEN-MASK SEQ-LEN-SHIFT lshift constant SEQ-RANGE-TAG
$FFFFFFFF constant SEQ-RANGE-MASK       \ u32 count; the remaining payload bits are reserved

\ ---- trait bits --------------------------------------------------------------
$1 constant BIT-CALL       \ contains a direct call: the Bl form
$2 constant BIT-INDIRECT   \ contains an indirect call: the Blr form
$4 constant BIT-SYSCALL    \ enters the kernel: the Svc form

BIT-CALL BIT-INDIRECT or BIT-SYSCALL or constant BIT-ALL

: MK-G ( n -- NEFF:gprs )       NEFF-GPRS:MAKE ;
: G-BITS ( NEFF:gprs -- n )     NEFF-GPRS:UNMAKE ;
: MK-F ( n -- NEFF:fprs )       NEFF-FPRS:MAKE ;
: F-BITS ( NEFF:fprs -- n )     NEFF-FPRS:UNMAKE ;
: MK-T ( n -- NEFF:traits )     NEFF-TRAITS:MAKE ;
: T-BITS ( NEFF:traits -- n )   NEFF-TRAITS:UNMAKE ;
: MK-S ( n -- NEFF:placeseq )     NEFF-PLACESEQ:MAKE ;
: S-BITS ( NEFF:placeseq -- n )   NEFF-PLACESEQ:UNMAKE ;

\ ---- stable wire codes -------------------------------------------------------
\ One injective code per closed family. These fix the digest; see the header.
: NZCV-CODE ( NEFF:nzcv -- n )
   MATCH nzcv
      untouched      OF 0 ENDOF
      clobbered      OF 1 ENDOF
      delivered      OF 2 ENDOF
      read-preserved OF 3 ENDOF
      read-clobbered OF 4 ENDOF
   ;MATCH ;

: LINK-CODE ( NEFF:link -- n )
   MATCH link
      preserved OF 0 ENDOF
      clobbered OF 1 ENDOF
      absent    OF 2 ENDOF
   ;MATCH ;

: CONTROL-CODE ( NEFF:control -- n )
   MATCH control
      returns   OF 0 ENDOF
      tail-call OF 1 ENDOF
      no-return OF 2 ENDOF
   ;MATCH ;

: CONV-CODE ( NEFF:conv -- n )
   MATCH conv
      dstack   OF 0 ENDOF
      register OF 1 ENDOF
   ;MATCH ;

\ ---- per-field rules ---------------------------------------------------------

\ A set of registers, as a value: bit i names register i, and the sign bit names
\ no register because a set is a collection of members and never a negative
\ number. WHICH registers a routine of some machine may name is that machine's
\ answer, and ROUTINE is where it is asked.
: GPR-CK ( n -- n )
   dup 0 < if E-NEFF-GPR throw then ;

: FPR-CK ( n -- n )
   dup 0 < if E-NEFF-FPR throw then ;

\ And that question, asked: every register of the set is one the machine says a
\ routine may hold state in. The machine's own answer, so a register the virtual
\ machine, the link register or the stack pointer took is refused here whichever
\ of them took it.
: GPR-FIT ( n n -- )
   {: bits:n alloc:n :}
   bits alloc invert and 0<> if E-NEFF-GPR throw then ;

: FPR-FIT ( n n -- )
   {: bits:n alloc:n :}
   bits alloc invert and 0<> if E-NEFF-FPR throw then ;

: TRAIT-CK ( n -- n )
   dup BIT-ALL invert and 0<> if E-NEFF-TRAIT throw then ;

\ A register number this schema can write down at all: a position of a place list
\ carries a five-bit payload, so a number outside it has nowhere to be recorded
\ however many registers the machine has.
: REG-CK ( n -- n )
   dup 0 < over PAY-MASK > or if E-NEFF-GPR throw then ;

: FREG-CK ( n -- n )
   dup 0 < over PAY-MASK > or if E-NEFF-FPR throw then ;

\ ---- the rules of an ordered place list --------------------------------------
\ Reading one packed list. Nothing below is public: a caller reaches a position
\ through the readers further down, which validate the whole list first, so there
\ is no route to an element of a list that was never checked.
: SEQ-RANGE? ( n -- bool )
   SEQ-RANGE-TAG and SEQ-RANGE-TAG = ;

: SEQ-LEN-OF ( n -- n )
   dup SEQ-RANGE? if SEQ-RANGE-MASK and exit then
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

\ A data-stack slot index the payload field holds. How deep a caller's stack may
\ be is the caller's business; what this schema owns is that a position's payload
\ is five bits wide, so an index past that has nowhere to be written down.
: SEQ-SLOT-CK ( n -- )
   dup 0 < over PAY-MASK > or if E-NEFF-SEQ throw then drop ;

: SEQ-PLACE-CK ( n -- )
   dup PLACE-SLOT? if PLACE-PAY SEQ-SLOT-CK exit then
   PLACE-PAY REG-CK drop ;

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
   w SEQ-RANGE? if
      w SEQ-RANGE-TAG invert and SEQ-RANGE-MASK > if E-NEFF-SEQ throw then
      len SEQ-MAX-N <= if E-NEFF-SEQ throw then
      exit
   then
   len SEQ-MAX-N > if E-NEFF-SEQ throw then
   len 0 ?do
      w i SEQ-AT SEQ-PLACE-CK
      w i SEQ-REPEATS? if E-NEFF-SEQ throw then
   loop
   SEQ-MAX-N len ?do
      w i SEQ-AT 0<> if E-NEFF-SEQ throw then
   loop ;

\ The set of registers a list names, which is how the two derived reader sets
\ below are answered. A data-stack slot is not a register and contributes
\ nothing: a routine whose whole interface is data-stack slots reads, returns and
\ destroys no register on account of its convention.
: SEQ-MASK ( n -- n )
   dup SEQ-RANGE? if drop 0 exit then
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
   dup SEQ-RANGE? if SEQ-LEN-OF exit then
   dup SEQ-LEN-OF {: w:n len:n :}
   0
   len 0 ?do
      w i SEQ-AT PLACE-SLOT? if 1+ then
   loop ;

\ A result is a register the caller reads; a destroyed register holds nothing the
\ caller may read. One register cannot be both.
: ROLE-CK ( n n -- )
   {: res:n clob:n :}
   res clob and 0<> if E-NEFF-ROLE throw then ;

: FRAME-CK ( n NMACH:mach -- )
   {: size:n m:NMACH:mach :}
   size 0 < size m NMACH:FRAME-MAX > or if E-NEFF-FRAME throw then
   size m NMACH:ALIGNED? 0= if E-NEFF-FRAME throw then ;

: DELTA-CK ( n n NMACH:mach -- )
   {: size:n delta:n m:NMACH:mach :}
   delta 0 > if E-NEFF-SP throw then
   delta m NMACH:ALIGNED? 0= if E-NEFF-SP throw then
   delta size negate < if E-NEFF-SP throw then ;

\ ---- whole-contract rules ----------------------------------------------------
\ Only the facts that need more than one field to decide.

: RETURNING? ( NEFF:control -- bool )
   MATCH control
      returns   OF true ENDOF
      tail-call OF true ENDOF
      no-return OF false ENDOF
   ;MATCH ;

\ A routine control comes back from - directly, or through a tail callee that
\ returns to our caller - leaves the stack pointer where it found it.
: BALANCE-CK ( NEFF:control n -- )
   {: c:control delta:n :}
   c RETURNING? 0= if exit then
   delta 0<> if E-NEFF-SP throw then ;

\ The convention a contract declares and the places it names have to be the same
\ statement. A data-stack convention whose list names a register, or a register
\ convention whose list names a slot, is a routine no entry sequence can be
\ written for: the two kinds are reached by different instructions and no pass of
\ this chain pairs them. It is refused HERE, at construction, so no later pass
\ ever holds such a contract - which is the point of declaring the convention at
\ all. A list naming NO position satisfies either declaration: a routine that
\ passes nothing passes nothing whichever way it would have.
: SIDE-CK ( NEFF:conv NEFF:placeseq -- )
   {: cv:conv s:placeseq :}
   s S-BITS SEQ-CK {: w:n :}
   w SEQ-SLOTS-OF {: sl:n :}
   cv NEFF-CONV:DSTACK NEFF-CONV:EQ if
      sl w SEQ-LEN-OF <> if E-NEFF-CONV throw then exit
   then
   sl 0<> if E-NEFF-CONV throw then ;

: CONV-CK ( NEFF:conv NEFF:placeseq NEFF:placeseq -- )
   {: cv:conv gi:placeseq gr:placeseq :}
   cv gi SIDE-CK
   cv gr SIDE-CK ;

\ Both a return and a tail call end by jumping to the link register: the tail
\ callee's own return does. Either way a destroyed link register has nowhere to
\ go back to.
\
\ On a machine that has no link register there is no such field to answer about,
\ and `absent` is the only answer its contracts may give - the return address of
\ one of ITS routines is in the frame, and that a returning routine leaves it
\ reachable is what BALANCE-CK says when it holds the stack-pointer delta of a
\ returning routine to zero.
: LINK-CK ( NEFF:control NEFF:link NMACH:mach -- )
   {: c:control l:link m:NMACH:mach :}
   l NEFF-LINK:ABSENT NEFF-LINK:EQ {: none:bool :}
   m NMACH:LINK? 0= if
      none 0= if E-NEFF-LINK throw then exit
   then
   none if E-NEFF-LINK throw then
   c RETURNING? 0= if exit then
   l NEFF-LINK:CLOBBERED NEFF-LINK:EQ if E-NEFF-LINK throw then ;

\ Control that never comes back delivers nothing, so a declared result of any
\ kind contradicts it.
: RESULT-CK ( n n NEFF:nzcv NEFF:control -- )
   {: gres:n fres:n z:nzcv c:control :}
   c RETURNING? if exit then
   gres 0<> fres 0<> or
   z NEFF-NZCV:DELIVERED NEFF-NZCV:EQ or
   if E-NEFF-CONTROL throw then ;

\ ---- canonical preimage ------------------------------------------------------
\ Version 3: the two interface slots hold ordered lists of PLACES rather than of
\ registers, so the same cell means something different than it did - a position
\ is now six bits and one of them says whether the payload is a register or a
\ data-stack slot - and the version says so instead of two schemas sharing one
\ digest. Version 2 was the step before, where those slots became ordered lists
\ rather than sets.
\ Version 4: the contract states which convention its two interface lists are
\ written in, so the preimage carries one more slot and it stands in front of
\ them - the field that says how the next two are read. Two contracts that
\ differed only in a convention nothing recorded used to digest the same.
\ Version 5: a contract names the machine it is about, so the preimage carries
\ one more slot holding that machine's mark - the fold over its declared facts,
\ not the ordinal it happens to have been declared under, which would make one
\ compiler's digests disagree with another's. Two contracts that differed only
\ in the machine they were stated about used to digest the same.
5 constant SCHEMA
16 constant SLOTS
0 constant SLOT-TAG
1 constant SLOT-SCHEMA
2 constant SLOT-CONV
3 constant SLOT-GPR-IN
4 constant SLOT-GPR-RES
5 constant SLOT-GPR-CLOB
6 constant SLOT-FPR-IN
7 constant SLOT-FPR-RES
8 constant SLOT-FPR-CLOB
9 constant SLOT-NZCV
10 constant SLOT-LINK
11 constant SLOT-CONTROL
12 constant SLOT-TRAITS
13 constant SLOT-FRAME
14 constant SLOT-DELTA
15 constant SLOT-MACH

SLOTS CDIGEST:SLOT-BYTES * constant PRE-BYTES
create PRE PRE-BYTES allot

public

\ ---- the machine a consumer has to agree with ---------------------------------
\ Every fact this schema used to report - the file size, the link, zero and
\ stack-pointer operands, the reserved and engine sets, the stack alignment, the
\ frame bound and the reach of a frame access - is a fact about one machine and
\ is read from its description: NMACH for a caller holding one, and
\ src/arch/arm64/machine.f or src/arch/x86-64/machine.f for a pass that is about
\ that machine in the first place. What is left here is the question only a
\ contract can answer, which machine it is a contract over, and it is answered
\ from the contract's own field rather than from anything ambient.

\ ---- register sets -----------------------------------------------------------
: GPR-SET ( n -- NEFF:gprs )    GPR-CK MK-G ;
: GPRS-N ( NEFF:gprs -- n )     G-BITS ;
: FPR-SET ( n -- NEFF:fprs )    FPR-CK MK-F ;
: FPRS-N ( NEFF:fprs -- n )     F-BITS ;

: GPR-NONE ( -- NEFF:gprs )     0 MK-G ;
: FPR-NONE ( -- NEFF:fprs )     0 MK-F ;

\ Every register a routine of THIS machine may hold state in, which is the only
\ set of the four whose members depend on which machine is asked: the file less
\ what the virtual machine, the link register and the stack pointer took.
: GPR-ALL ( NMACH:mach -- NEFF:gprs )
   NMACH:GPR-ALLOCATABLE MK-G ;

: FPR-ALL ( NMACH:mach -- NEFF:fprs )
   NMACH:FPR-ALLOCATABLE MK-F ;

\ The set holding exactly one register. A number no place list could write down
\ is refused here rather than silently shifted out of the cell.
: GPR-REG ( n -- NEFF:gprs )    REG-CK 1 swap lshift GPR-CK MK-G ;
: FPR-REG ( n -- NEFF:fprs )    FREG-CK 1 swap lshift MK-F ;

: GPR-WITH ( NEFF:gprs NEFF:gprs -- NEFF:gprs )
   G-BITS swap G-BITS or GPR-CK MK-G ;

: FPR-WITH ( NEFF:fprs NEFF:fprs -- NEFF:fprs )
   F-BITS swap F-BITS or FPR-CK MK-F ;

: GPR-WITHOUT ( NEFF:gprs NEFF:gprs -- NEFF:gprs )
   {: set:gprs less:gprs :}
   set G-BITS GPR-CK less G-BITS GPR-CK invert and MK-G ;

: FPR-WITHOUT ( NEFF:fprs NEFF:fprs -- NEFF:fprs )
   {: set:fprs less:fprs :}
   set F-BITS FPR-CK less F-BITS FPR-CK invert and MK-F ;

\ Does the set hold every register of the probe set?
: GPR-HAS? ( NEFF:gprs NEFF:gprs -- bool )
   {: set:gprs probe:gprs :}
   probe G-BITS GPR-CK {: want:n :}
   set G-BITS GPR-CK want and want = ;

: FPR-HAS? ( NEFF:fprs NEFF:fprs -- bool )
   {: set:fprs probe:fprs :}
   probe F-BITS FPR-CK {: want:n :}
   set F-BITS FPR-CK want and want = ;

\ ---- ordered place lists ------------------------------------------------------
\ The convention that names nothing: a routine whose arguments arrive, or whose
\ results leave, in no place this contract has an opinion about. It is also what
\ a routine that takes or returns nothing declares, and those are the same
\ statement - there is no position to say anything about either way.
: SEQ-NONE ( -- NEFF:placeseq )   0 MK-S ;

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
: SEQ-PUT ( NEFF:placeseq n -- NEFF:placeseq )
   {: s:placeseq e:n :}
   s S-BITS SEQ-CK {: w:n :}
   w SEQ-LEN-OF {: len:n :}
   len SEQ-MAX-N >= if E-NEFF-SEQ throw then
   e SEQ-PLACE-CK
   w  e len PLACE-BITS * lshift or  1 SEQ-LEN-SHIFT lshift +
   SEQ-CK MK-S ;

public

\ The list with one more REGISTER place after its last position.
: SEQ-WITH ( NEFF:placeseq n -- NEFF:placeseq )
   REG-CK SEQ-PUT ;

\ The list with one more DATA-STACK SLOT place after its last position. This is
\ the whole of what design section 7.6's convention needs to be sayable: argument
\ i arrives in slot i, result j leaves in slot j.
: SEQ-WITH-SLOT ( NEFF:placeseq n -- NEFF:placeseq )
   dup 0 < over PAY-MASK > or if E-NEFF-SEQ throw then
   KIND-BIT or SEQ-PUT ;

\ Positions 0..n-1 on the Forth data stack. Small sequences keep the existing
\ canonical packed representation; larger ones store the same range directly.
: SEQ-DSTACK ( n -- NEFF:placeseq ) {: n:n :}
   n 0 < n SEQ-RANGE-MASK > or if E-NEFF-SEQ throw then
   n SEQ-MAX-N > if n SEQ-RANGE-TAG or MK-S exit then
   SEQ-NONE
   n 0 ?do i SEQ-WITH-SLOT loop ;

: SEQ-LEN ( NEFF:placeseq -- n )
   S-BITS SEQ-CK SEQ-LEN-OF ;

private

: SEQ-POSITION ( NEFF:placeseq n -- n n )
   {: s:placeseq p:n :}
   s S-BITS SEQ-CK {: w:n :}
   p 0 < p w SEQ-LEN-OF >= or if E-NEFF-SEQ throw then
   w p ;

public

\ Which kind of place one position names. Every reader of a position goes through
\ this or is refused: there is no word that answers a payload without saying what
\ it is, so a pass that treated a slot index as a register number would have had
\ to ask for the register of a slot and be told no.
: SEQ-KIND@ ( NEFF:placeseq n -- NEFF:pkind )
   SEQ-POSITION {: w:n p:n :}
   w SEQ-RANGE? if NEFF-PKIND:DSLOT exit then
   w p SEQ-AT PLACE-SLOT? if NEFF-PKIND:DSLOT exit then NEFF-PKIND:GPR ;

\ The register at one position. A position holding a data-stack slot is refused
\ rather than answered with an index that would read as a register number.
: SEQ-REG@ ( NEFF:placeseq n -- n )
   SEQ-POSITION {: w:n p:n :}
   w SEQ-RANGE? if E-NEFF-KIND throw then
   w p SEQ-AT dup PLACE-SLOT? if E-NEFF-KIND throw then PLACE-PAY ;

\ The data-stack slot index at one position, refused the same way in reverse.
: SEQ-SLOT@ ( NEFF:placeseq n -- n )
   SEQ-POSITION {: w:n p:n :}
   w SEQ-RANGE? if p exit then
   w p SEQ-AT dup PLACE-SLOT? 0= if E-NEFF-KIND throw then PLACE-PAY ;

\ How many positions of the list are data-stack slots. Zero and the whole length
\ are the two homogeneous conventions; anything between mixes the kinds, which is
\ describable here and has no lowering rule anywhere in the chain yet.
: SEQ-SLOTS ( NEFF:placeseq -- n )
   S-BITS SEQ-CK SEQ-SLOTS-OF ;

\ Which registers a list names, forgetting the order. Data-stack slots are not
\ registers and are not in it.
: SEQ-SET ( NEFF:placeseq -- NEFF:gprs )
   S-BITS SEQ-CK SEQ-MASK MK-G ;

\ ---- traits ------------------------------------------------------------------
: TRAIT-SET ( n -- NEFF:traits )   TRAIT-CK MK-T ;
: TRAITS-N ( NEFF:traits -- n )    T-BITS ;
: TRAITS-NONE ( -- NEFF:traits )   0 MK-T ;
: T-CALL ( -- NEFF:traits )        BIT-CALL MK-T ;
: T-INDIRECT ( -- NEFF:traits )    BIT-INDIRECT MK-T ;
: T-SYSCALL ( -- NEFF:traits )     BIT-SYSCALL MK-T ;

: TRAITS-WITH ( NEFF:traits NEFF:traits -- NEFF:traits )
   T-BITS swap T-BITS or TRAIT-CK MK-T ;

: TRAITS-HAS? ( NEFF:traits NEFF:traits -- bool )
   {: set:traits probe:traits :}
   probe T-BITS TRAIT-CK {: want:n :}
   set T-BITS TRAIT-CK want and want = ;

\ ---- construction and validation ---------------------------------------------
\ The production entry point. A combination that cannot be true of one routine
\ throws a named error and no contract value is produced.
: ROUTINE ( NEFF:conv NEFF:placeseq NEFF:placeseq NEFF:gprs NEFF:fprs NEFF:fprs NEFF:fprs NEFF:nzcv NEFF:link NEFF:control NEFF:traits n n NMACH:mach -- NEFF:routine )
   {: cv:conv gi:placeseq gr:placeseq gc:gprs fi:fprs fr:fprs fc:fprs z:nzcv
      l:link c:control t:traits size:n delta:n m:NMACH:mach :}
   m NMACH:GPR-ALLOCATABLE {: ga:n :}
   m NMACH:FPR-ALLOCATABLE {: fa:n :}
   gi S-BITS SEQ-CK SEQ-MASK ga GPR-FIT
   gr S-BITS SEQ-CK SEQ-MASK {: res:n :}
   res ga GPR-FIT
   gc G-BITS GPR-CK {: clob:n :}
   clob ga GPR-FIT
   res clob ROLE-CK
   fi F-BITS FPR-CK fa FPR-FIT
   fr F-BITS FPR-CK {: fres:n :}
   fres fa FPR-FIT
   fc F-BITS FPR-CK {: fclob:n :}
   fclob fa FPR-FIT
   fres fclob ROLE-CK
   t T-BITS TRAIT-CK drop
   size m FRAME-CK
   size delta m DELTA-CK
   c delta BALANCE-CK
   c l m LINK-CK
   cv gi gr CONV-CK
   res fres z c RESULT-CK
   cv gi gr gc fi fr fc z l c t size delta m NEFF-ROUTINE:MAKE ;

\ Recheck a contract that may have been assembled by the generated constructor.
: VALIDATE ( NEFF:routine -- NEFF:routine )
   NEFF-ROUTINE:UNMAKE ROUTINE ;

\ ---- field readers -----------------------------------------------------------
\ A projection of a value the caller already holds; nothing here revalidates.
: CONV@ ( NEFF:routine -- NEFF:conv )
   NEFF-ROUTINE:UNMAKE
   drop drop drop drop drop drop drop drop drop drop drop drop drop ;

: ARGS@ ( NEFF:routine -- NEFF:placeseq )
   NEFF-ROUTINE:UNMAKE
   drop drop drop drop drop drop drop drop drop drop drop drop nip ;

: RESULTS@ ( NEFF:routine -- NEFF:placeseq )
   NEFF-ROUTINE:UNMAKE
   drop drop drop drop drop drop drop drop drop drop drop nip nip ;

: GPR-CLOBBER@ ( NEFF:routine -- NEFF:gprs )
   NEFF-ROUTINE:UNMAKE
   drop drop drop drop drop drop drop drop drop drop nip nip nip ;

: FPR-IN@ ( NEFF:routine -- NEFF:fprs )
   NEFF-ROUTINE:UNMAKE
   drop drop drop drop drop drop drop drop drop nip nip nip nip ;

: FPR-RESULT@ ( NEFF:routine -- NEFF:fprs )
   NEFF-ROUTINE:UNMAKE
   drop drop drop drop drop drop drop drop nip nip nip nip nip ;

: FPR-CLOBBER@ ( NEFF:routine -- NEFF:fprs )
   NEFF-ROUTINE:UNMAKE
   drop drop drop drop drop drop drop nip nip nip nip nip nip ;

: NZCV@ ( NEFF:routine -- NEFF:nzcv )
   NEFF-ROUTINE:UNMAKE
   drop drop drop drop drop drop nip nip nip nip nip nip nip ;

: LINK@ ( NEFF:routine -- NEFF:link )
   NEFF-ROUTINE:UNMAKE
   drop drop drop drop drop nip nip nip nip nip nip nip nip ;

: CONTROL@ ( NEFF:routine -- NEFF:control )
   NEFF-ROUTINE:UNMAKE
   drop drop drop drop nip nip nip nip nip nip nip nip nip ;

: TRAITS@ ( NEFF:routine -- NEFF:traits )
   NEFF-ROUTINE:UNMAKE
   drop drop drop nip nip nip nip nip nip nip nip nip nip ;

: FRAME@ ( NEFF:routine -- n )
   NEFF-ROUTINE:UNMAKE
   drop drop nip nip nip nip nip nip nip nip nip nip nip ;

: DELTA@ ( NEFF:routine -- n )
   NEFF-ROUTINE:UNMAKE
   drop nip nip nip nip nip nip nip nip nip nip nip nip ;

\ Which machine every field above is stated about. A pass holding a contract
\ needs no second value to read it, and cannot be holding the wrong one.
: MACH@ ( NEFF:routine -- NMACH:mach )
   NEFF-ROUTINE:UNMAKE
   nip nip nip nip nip nip nip nip nip nip nip nip nip ;

\ ---- derived facts -----------------------------------------------------------
\ Which registers the interface lists name, as sets. A caller that only wants to
\ know whether a register takes part - and every caller before the convention was
\ ordered wanted exactly that - asks here rather than walking positions, and the
\ answer cannot contradict the list because there is nothing else to read it out
\ of.
: GPR-IN@ ( NEFF:routine -- NEFF:gprs )
   VALIDATE ARGS@ SEQ-SET ;

: GPR-RESULT@ ( NEFF:routine -- NEFF:gprs )
   VALIDATE RESULTS@ SEQ-SET ;

\ What survives the routine: every register of the file it neither returns nor
\ destroys. Derived, never stored, so it cannot contradict the destroyed set.
: GPR-PRESERVED ( NEFF:routine -- NEFF:gprs )
   VALIDATE NEFF-ROUTINE:UNMAKE
   {: m:NMACH:mach :}              \ the machine, which says what there is to preserve
   drop drop drop drop drop drop   \ delta, frame, traits, control, link, nzcv
   drop drop drop                  \ the floating sets
   {: cv:conv gi:placeseq gr:placeseq gc:gprs :}
   m NMACH:GPR-ALLOCATABLE gr S-BITS SEQ-MASK invert and gc G-BITS invert and MK-G ;

\ Every register the routine may WRITE: the ones it destroys, plus the ones it
\ returns a value in. They are two roles and one register cannot be both, which
\ is why they are two fields - but a register allocator does not care why a
\ register may be written, only that it may, so the set it may hand out is this
\ one and not the destroyed set alone. Derived for the same reason the preserved
\ set is: a stored copy could disagree with the two fields it is made of.
: GPR-WRITABLE ( NEFF:routine -- NEFF:gprs )
   VALIDATE NEFF-ROUTINE:UNMAKE
   drop                            \ the machine: what a routine writes is its own
   drop drop drop drop drop drop   \ delta, frame, traits, control, link, nzcv
   drop drop drop                  \ the floating sets
   {: cv:conv gi:placeseq gr:placeseq gc:gprs :}
   gr S-BITS SEQ-MASK gc G-BITS or MK-G ;

: FPR-PRESERVED ( NEFF:routine -- NEFF:fprs )
   VALIDATE NEFF-ROUTINE:UNMAKE
   {: m:NMACH:mach :}              \ the machine, which says what there is to preserve
   drop drop drop drop drop drop   \ delta, frame, traits, control, link, nzcv
   {: fi:fprs fr:fprs fc:fprs :}
   drop drop drop drop             \ the general lists, the destroyed set, the convention
   m NMACH:FPR-ALLOCATABLE fr F-BITS invert and fc F-BITS invert and MK-F ;

\ The floating file's writable set, derived exactly as the general file's is:
\ what the routine destroys plus what it returns a value in. A register
\ allocator with a second file asks this question of the second file in the same
\ words it asks it of the first, so the two pools come from one rule rather than
\ from one rule and one special case.
: FPR-WRITABLE ( NEFF:routine -- NEFF:fprs )
   VALIDATE NEFF-ROUTINE:UNMAKE
   drop                            \ the machine: what a routine writes is its own
   drop drop drop drop drop drop   \ delta, frame, traits, control, link, nzcv
   {: fi:fprs fr:fprs fc:fprs :}
   drop drop drop drop             \ the general lists, the destroyed set, the convention
   fr F-BITS fc F-BITS or MK-F ;

: RETURNS? ( NEFF:routine -- bool )
   VALIDATE CONTROL@ RETURNING? ;

\ ---- frame slots -------------------------------------------------------------
\ A slot the A64IR dialect wants to place: a byte offset from the deepest point
\ of the routine's own frame, and an access width. It is accepted only if a load
\ or store form can actually reach it - a width the vocabulary carries, an offset
\ the scale division will not round, an offset inside the declared frame, and an
\ offset inside the reach of that width's twelve-bit field. The routine is last
\ so the two numbers can be read into locals; a multi-cell value cannot be one.
: CHECK-SLOT ( n n NEFF:routine -- )
   VALIDATE NEFF-ROUTINE:UNMAKE
   {: m:NMACH:mach :}
   drop {: size:n :}                  \ the delta, then the frame this slot is in
   drop drop drop drop drop drop drop drop drop drop drop
   {: off:n width:n :}
   width m NMACH:WIDTH-OK? 0= if E-NEFF-SLOT throw then
   off 0 < if E-NEFF-SLOT throw then
   off width mod 0<> if E-NEFF-SLOT throw then
   off width + size > if E-NEFF-SLOT throw then
   off  width m NMACH:SLOT-REACH  > if E-NEFF-SLOT throw then ;

\ ---- identity ----------------------------------------------------------------
\ Field-by-field identity. Both inputs are revalidated first, so a forged
\ contract cannot be compared as if it were a declarable routine.
: SAME? ( NEFF:routine NEFF:routine -- bool )
   VALIDATE NEFF-ROUTINE:UNMAKE
   {: ycv:conv ygi:placeseq ygr:placeseq ygc:gprs yfi:fprs yfr:fprs yfc:fprs
      yz:nzcv yl:link yc:control yt:traits ysize:n ydelta:n ym:NMACH:mach :}
   VALIDATE NEFF-ROUTINE:UNMAKE
   {: xcv:conv xgi:placeseq xgr:placeseq xgc:gprs xfi:fprs xfr:fprs xfc:fprs
      xz:nzcv xl:link xc:control xt:traits xsize:n xdelta:n xm:NMACH:mach :}
   xm ym NMACH-MACH:EQ
   xcv ycv NEFF-CONV:EQ and
   xgi ygi NEFF-PLACESEQ:EQ and
   xgr ygr NEFF-PLACESEQ:EQ and
   xgc ygc NEFF-GPRS:EQ and
   xfi yfi NEFF-FPRS:EQ and
   xfr yfr NEFF-FPRS:EQ and
   xfc yfc NEFF-FPRS:EQ and
   xz yz NEFF-NZCV:EQ and
   xl yl NEFF-LINK:EQ and
   xc yc NEFF-CONTROL:EQ and
   xt yt NEFF-TRAITS:EQ and
   xsize ysize = and
   xdelta ydelta = and ;

\ The canonical preimage. The bytes live in this module and stay valid until the
\ next ENCODE call; DIGEST is the copy-free consumer.
: ENCODE ( NEFF:routine -- ptr u8 n )
   VALIDATE NEFF-ROUTINE:UNMAKE
   {: cv:conv gi:placeseq gr:placeseq gc:gprs fi:fprs fr:fprs fc:fprs z:nzcv
      l:link c:control t:traits size:n delta:n m:NMACH:mach :}
   CDIGEST:TAG-A64-ROUTINE PRE SLOT-TAG CDIGEST:SLOT!
   SCHEMA PRE SLOT-SCHEMA CDIGEST:SLOT!
   cv CONV-CODE PRE SLOT-CONV CDIGEST:SLOT!
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
   m NMACH:MARK PRE SLOT-MACH CDIGEST:SLOT!
   PRE PRE-BYTES ;

: DIGEST ( NEFF:routine -- CDIGEST:digest )
   ENCODE CDIGEST:COMPUTE ;

;package
