\ machine.f - the x86-64 machine, as the one description a routine contract over
\ this backend is written against.
\
\ WHERE THESE NUMBERS COME FROM. Every fact here is read off
\ src/compiler/native/x64ir.f, which is the machine dialect this backend lowers
\ into, so this file states nothing twice:
\   - sixteen general registers (rax..r15) and sixteen floating (xmm0..xmm15).
\     Seven of the general file are the running engine's - rbx, rsp, rbp and
\     r12..r15 - and x64ir's own RESERVED-MASK is where that is decided;
\   - THERE IS NO LINK REGISTER. `call` pushes the return address onto the
\     machine stack, so the caller's return address is not in a register a
\     routine could preserve or clobber, and the contract's link field is
\     `absent` here. That is the one field an ARM64 contract cannot share;
\   - there is no zero register either, so nothing is reserved on that account;
\   - rsp is the machine stack pointer AND an ordinary operand number, which is
\     why it has to be reserved by name. ARM64 has no equivalent slip to make;
\   - a frame access is a mov through a disp32 displacement, counted in BYTES
\     and not scaled by the access width, so a byte slot and an eight-byte slot
\     reach the same distance from the stack pointer. That is the second reading
\     of an offset field NMACH:SLOT-REACH exists to keep apart;
\   - the displacement is signed, so an access names a byte below its base as
\     well as above it, with the same magnitude either way;
\   - a mov moves a byte, a word, a dword or a qword, so the modelled widths are
\     1, 2, 4 and 8. Unlike ARM64 there IS a halfword form;
\   - the SysV stack alignment this port keeps is sixteen bytes, and the largest
\     frame is the deepest displacement rounded down to it.
\
\ WHY IT IS A FILE OF ITS OWN. It is the x86-64 half of what
\ src/arch/arm64/machine.f is for AArch64: the facts the target-neutral routine
\ contract (src/compiler/native-effect.f) used to hold as ARM64 constants. It
\ requires the machine dialect and nothing under src/os/, so an engine that
\ carries no x86-64 backend carries no description of one either.

require lib/prelude.f
require src/compiler/native/machine.f
require src/compiler/native/x64ir.f

package X64M
private

\ The widths the modelled memory forms move, as a set: bit w is set when some
\ form moves w bytes. Every power of two up to the slot width has a mov.
$1 1 lshift
   $1 2 lshift or
   $1 4 lshift or
   $1 X64IR:SLOT-WIDTH lshift or
constant WIDTHS-N

\ The description, built once at load, for the reason the ARM64 one gives: a
\ word that described the machine per call would pay a table scan on every
\ register a pass named, so what is kept is the ordinal NMACH hands back.
: DESCRIBE ( -- n )
   X64IR:REGFILE
   NREGFILE:REGS-NONE                   \ no link register: the return address is pushed
   X64IR:SP-GPR X64IR:SP-ALIGN X64IR:FRAME-LIMIT X64IR:DISP-LIMIT
   NMACH-OFFSCALE:BY-BYTE WIDTHS-N X64IR:DISP-LIMIT
   NMACH:MACHINE NMACH:ID ;

DESCRIBE constant ROW

public

\ ---- the description ---------------------------------------------------------
\ One value that says everything about this machine a routine contract needs.
\ There are no fast readers beside it as there are for ARM64: no pass is written
\ against this machine yet, and a reader nothing asks would be a second copy of
\ a fact x64ir.f already owns.
: MACHINE ( -- NMACH:mach )   ROW NMACH:BY-ID ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
