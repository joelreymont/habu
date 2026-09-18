\ machine.f - the ARM64 machine, as the one description every ARM64 pass and
\ every ARM64 routine contract is written against.
\
\ WHERE THESE NUMBERS COME FROM. Every bound here is read off the instruction
\ vocabulary that formal/Common/Insn.v models and src/arch/arm64/asm.f emits, and
\ test/compiler/native-effect.f pins each one against that source instead of
\ restating it:
\   - a register operand is a five-bit field, so a file holds 32 registers;
\   - x18 is platform-reserved on Darwin and ordinary on Linux;
\   - x19 holds the running engine's data-stack pointer. src/arch/arm64/mnem.f
\     names it (`19 constant XDS`), src/habu/rt.f's push and pop are stores and
\     loads through it, and src/habu/habu2.f measures the interpreter's stack
\     depth as (XDS - S0) / 8 - so every word the engine calls finds its operands
\     through this one register and leaves its results through it. It is not a
\     register a routine may be given: a routine that wrote to it would move the
\     caller's stack under the caller. The engine's whole claim arrives here as
\     ENGINE-GPR:MASK, which src/habu/layout.f derives from its own register
\     assignments and is the one authority for it. This file used to name x19 as
\     a fifth constant of its own, which is exactly the second copy that let x20,
\     x26, x27 and x28 through: the engine claimed four more registers and
\     nothing propagated (CG-13);
\   - x30 is the link register. It is reserved, because whether the caller's
\     return address survives is the contract's `link` field and not a question
\     about a scratch register, and one fact has one owner;
\   - operand 31 is the zero register or the stack pointer depending on the form,
\     and neither holds routine state, so it is reserved too;
\   - the D-register file has no reserved member, so all 32 are nameable;
\   - a frame slot is reached by an unsigned-offset load or store, whose offset
\     field is twelve bits SCALED BY THE ACCESS WIDTH, and whose scale division
\     `SCALE/` refuses an offset it would round - which is exactly natural
\     alignment. That fixes both how far a slot can sit from the stack pointer
\     and the largest frame a contract over this machine can describe;
\   - the unscaled load and store field is nine bits read as a signed number of
\     BYTES, so an access can name a byte below the register it is taken from as
\     well as above it. It is a second field on the same instruction group rather
\     than a second group, and it is what makes an access below a base
\     expressible at all;
\   - the modelled memory forms are Ldr/Str (eight bytes), Ldrw/Strw (four) and
\     Ldrb/Strb (one). There is no halfword form, so there is no halfword slot.
\
\ WHY IT IS A FILE OF ITS OWN. These were constants inside the routine-contract
\ schema, which made the schema ARM64's and left the x86-64 backend with no way
\ to declare a contract at all. The schema is now target-neutral
\ (src/compiler/native-effect.f) and every contract names the machine it is
\ about; this is the ARM64 description, and src/arch/x86-64/machine.f is the
\ other one. A pass that used to ask the schema for a number asks here, so the
\ one place that says why a register is not a routine's to use is also the one
\ place that says where it does appear.
\
\ THE READERS AND THE DESCRIPTION ARE ONE AUTHORITY. The constants below are the
\ facts; MACHINE is those same facts handed to NMACH once at load, and each
\ reader answers from the constant it was built out of rather than through the
\ description, because a pass emitting one instruction asks several of them per
\ instruction. Nothing here can drift from what the description says, because
\ nothing here is stated twice.

require lib/prelude.f
require lib/errors.f
require src/compiler/native/regfile.f
require src/compiler/native/machine.f

package A64M
private

5 constant REG-BITS       \ a register operand is a five-bit field
1 REG-BITS lshift constant FILE-N        \ registers per file, which is that field's reach
18 constant DARWIN-RESERVED-N
30 constant LINK-N        \ x30, the link register, which has its own contract field
31 constant ZERO-N        \ operand 31: the zero register, or the stack pointer

: PLATFORM-RESERVED-MASK ( -- n )
   HB-TARGET-LINUX? if 0 exit then
   HB-TARGET-MACOS? if 1 DARWIN-RESERVED-N lshift exit then
   E-CTGT-ABI throw ;

\ The registers this TARGET gives another owner: its optional platform register
\ and the two operand slots that are not general state.
PLATFORM-RESERVED-MASK
   1 LINK-N lshift or
   1 ZERO-N lshift or
constant TARGET-RESERVED-MASK

\ Every register a routine may not hold state in, from both owners, in one place.
TARGET-RESERVED-MASK ENGINE-GPR:MASK or constant RESERVED-MASK

\ The general registers a routine CAN hold state in: the whole file less that.
1 FILE-N lshift 1 -  RESERVED-MASK invert and  constant GPR-MASK

1 FILE-N lshift 1 - constant FPR-MASK

16 constant SP-ALIGN-N                \ the stack pointer is 16-byte aligned
1 12 lshift 1 - constant OFF-MAX      \ largest scaled offset the unsigned field holds
8 constant WIDEST                     \ bytes moved by the Ldr and Str forms
1 8 lshift constant BACK-MAX-N        \ the deepest byte under a base the signed field names

\ The widths the modelled memory forms move, as a set: bit w is set when some
\ form moves w bytes. There is no halfword bit because there is no halfword form.
$1 1 lshift  $1 4 lshift or  $1 WIDEST lshift or  constant WIDTHS-N

\ The largest frame a contract over this machine can describe: the deepest byte
\ the widest access reaches, rounded down to the stack alignment.
OFF-MAX WIDEST * dup SP-ALIGN-N mod - constant FRAME-MAX-N

\ A spill slot holds one general register, and the widest access moves exactly
\ that. src/compiler/native/a64ir.f's frame forms read their slot width from
\ here rather than declaring a second eight.
WIDEST constant SLOT-WIDTH-N

\ Every allocatable register is declared call-destroyed because that is what the
\ Habu convention says: src/compiler/native/abi.f declares the whole pool
\ destroyed on every routine, so no value survives a call in a register and the
\ callee-saved set the description derives is empty. A convention that kept
\ registers across a call would say so here and the allocator would follow.
: REGFILE-OF ( -- NREGFILE:file )
   FILE-N  RESERVED-MASK NREGFILE:REGS-SET  GPR-MASK NREGFILE:REGS-SET
   FILE-N  NREGFILE:REGS-NONE              FPR-MASK NREGFILE:REGS-SET
   SLOT-WIDTH-N
   NREGFILE:FILE ;

\ The description, built once at load. A word that described the machine per call
\ would pay a table scan on every register a pass names, so what is kept is the
\ ordinal NMACH hands back and MACHINE below is the checked way back to it.
: DESCRIBE ( -- n )
   REGFILE-OF
   1 LINK-N lshift NREGFILE:REGS-SET
   ZERO-N SP-ALIGN-N FRAME-MAX-N OFF-MAX
   NMACH-OFFSCALE:BY-WIDTH WIDTHS-N BACK-MAX-N
   NMACH:MACHINE NMACH:ID ;

DESCRIBE constant ROW

public

\ ---- the description ---------------------------------------------------------
\ One value that says everything about this machine a routine contract and the
\ passes over it need. Every reader below is the same fact read without it.
: MACHINE ( -- NMACH:mach )   ROW NMACH:BY-ID ;

\ ---- the register files -------------------------------------------------------
: FILE-SIZE ( -- n )      FILE-N ;
: RESERVED-GPRS ( -- n )  RESERVED-MASK ;
: ENGINE-GPRS ( -- n )    ENGINE-GPR:MASK ;
: SLOT-WIDTH ( -- n )     SLOT-WIDTH-N ;

\ The register the running engine keeps its data-stack pointer in. A pass that
\ emits an access to the caller's stack asks for it here rather than writing 19.
\ The number comes from the engine's own declaration (src/arch/arm64/mnem.f XDS,
\ which src/habu/layout.f folds into ENGINE-GPR:MASK): this file reports the
\ engine's register, it does not decide it.
: DSTACK-GPR ( -- n )     ENGINE-GPR:DSTACK ;

\ ---- the registers that are not general state ---------------------------------
: LINK-GPR ( -- n )       LINK-N ;
: ZERO-GPR ( -- n )       ZERO-N ;

\ The same operand number, named for what it means in the forms that reach a
\ frame slot: there operand 31 is the stack pointer rather than the zero
\ register. A pass emitting a frame access asks for it here instead of writing
\ the number.
: SP-GPR ( -- n )         ZERO-N ;

\ ---- the frame ---------------------------------------------------------------
: SP-ALIGN ( -- n )       SP-ALIGN-N ;
: FRAME-MAX ( -- n )      FRAME-MAX-N ;
: OFF-LIMIT ( -- n )      OFF-MAX ;

: FRAME-ROUND ( n -- n )  MACHINE NMACH:FRAME-ROUND ;

: SLOT-REACH ( n -- n )   MACHINE NMACH:SLOT-REACH ;

: SLOT-BACK ( -- n )      BACK-MAX-N ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
