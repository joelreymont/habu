\ machine.f - A64M, the ARM64 machine description every ARM64 pass and routine
\ contract is written against; src/arch/x86-64/machine.f is the x86-64 half and
\ src/compiler/native-effect.f the target-neutral schema. Every bound is read off
\ the vocabulary src/arch/arm64/asm.f emits; the engine's registers come from
\ ENGINE-GPR (src/habu/layout.f). Readers answer from the constants MACHINE is
\ built from, so nothing is stated twice.

require src/lib/prelude.f
require src/lib/errors.f
require src/compiler/native/regfile.f
require src/compiler/native/machine.f

package A64M
private

5 constant REG-BITS       \ a register operand is a five-bit field
1 REG-BITS lshift constant FILE-N        \ registers per file, which is that field's reach
18 constant PLATFORM-N    \ x18, AAPCS64's platform register
30 constant LINK-N        \ x30, the link register, which has its own contract field
31 constant ZERO-N        \ operand 31: the zero register, or the stack pointer

\ AAPCS64 leaves x18 to the platform: Linux gives it to code, Darwin keeps it. An
\ x86-64 target still loads this file (the shared compiler requires a64ir.f, and
\ an x86-64 build window loads it on an ARM64 host), so it answers the reserving
\ default instead of refusing the whole closure.
: PLATFORM-RESERVED-MASK ( -- n )
   HB-TARGET-LINUX? if 0 exit then
   HB-TARGET-MACOS? if 1 PLATFORM-N lshift exit then
   HB-TARGET-LINUX-X86-64? if 1 PLATFORM-N lshift exit then
   E-CTGT-ABI throw ;

\ The registers this target gives another owner: the platform register and the
\ two operand slots that hold no general state.
PLATFORM-RESERVED-MASK
   1 LINK-N lshift or
   1 ZERO-N lshift or
constant TARGET-RESERVED-MASK

\ The engine's claim, x19's data stack among it, is ENGINE-GPR:A64-MASK, the one
\ authority: never name an engine register here. It is read by name, never as
\ ENGINE-GPR:MASK, because the x86-64 compiler loads this file too (abi.f and
\ frame.f require a64ir.f) and there MASK is the x86-64 claim.
TARGET-RESERVED-MASK ENGINE-GPR:A64-MASK or constant RESERVED-MASK

1 FILE-N lshift 1 -  RESERVED-MASK invert and  constant GPR-MASK

1 FILE-N lshift 1 - constant FPR-MASK

\ A frame slot is reached by an unsigned-offset access: twelve bits scaled by the
\ access width, and SCALE/ refuses an offset it would round, which is natural
\ alignment. The unscaled field is nine signed bits counted in bytes, the only
\ way to name a byte below a base.
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

\ A spill slot holds one general register; a64ir.f's frame forms read their slot
\ width here.
WIDEST constant SLOT-WIDTH-N

\ Every allocatable register is call-destroyed: src/compiler/native/abi.f
\ declares the whole pool destroyed on every routine, so no value survives a call
\ in a register and the callee-saved set is empty.
: REGFILE-OF ( -- NREGFILE:file )
   FILE-N  RESERVED-MASK NREGFILE:REGS-SET  GPR-MASK NREGFILE:REGS-SET
   FILE-N  NREGFILE:REGS-NONE              FPR-MASK NREGFILE:REGS-SET
   SLOT-WIDTH-N
   NREGFILE:FILE ;

\ Built once at load: describing per call would pay a table scan on every
\ register a pass names, so the ordinal NMACH hands back is kept.
: DESCRIBE ( -- n )
   REGFILE-OF
   1 LINK-N lshift NREGFILE:REGS-SET
   ZERO-N SP-ALIGN-N FRAME-MAX-N OFF-MAX
   NMACH-OFFSCALE:BY-WIDTH WIDTHS-N BACK-MAX-N
   NMACH:MACHINE NMACH:ID ;

DESCRIBE constant ROW

public

\ ---- the description ---------------------------------------------------------
: MACHINE ( -- NMACH:mach )   ROW NMACH:BY-ID ;

\ ---- the register files -------------------------------------------------------
: FILE-SIZE ( -- n )      FILE-N ;
: RESERVED-GPRS ( -- n )  RESERVED-MASK ;
: ENGINE-GPRS ( -- n )    ENGINE-GPR:A64-MASK ;
: SLOT-WIDTH ( -- n )     SLOT-WIDTH-N ;

\ Declared by src/habu/layout.f; RT:DSTACK-AGREE holds it equal to
\ src/arch/arm64/mnem.f XDS. Read by name for the reason RESERVED-MASK reads
\ A64-MASK.
: DSTACK-GPR ( -- n )     ENGINE-GPR:A64-DSTACK ;

\ ---- the registers that are not general state ---------------------------------
: LINK-GPR ( -- n )       LINK-N ;
: ZERO-GPR ( -- n )       ZERO-N ;

\ Operand 31 in the forms that reach a frame slot is the stack pointer, not the
\ zero register.
: SP-GPR ( -- n )         ZERO-N ;

\ ---- the frame ---------------------------------------------------------------
: SP-ALIGN ( -- n )       SP-ALIGN-N ;
: FRAME-MAX ( -- n )      FRAME-MAX-N ;

: SLOT-REACH ( n -- n )   MACHINE NMACH:SLOT-REACH ;

: SLOT-BACK ( -- n )      BACK-MAX-N ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
