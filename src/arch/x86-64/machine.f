\ machine.f - X64M, the x86-64 machine description routine contracts over this
\ backend are written against; the x86-64 half of src/arch/arm64/machine.f. Every
\ fact is read off src/compiler/native/x64ir.f, except the data-stack register,
\ read off src/habu/layout.f. It requires nothing under src/os/, so an engine
\ without an x86-64 backend carries no description of one.

require src/lib/prelude.f
require src/habu/layout.f
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

\ Built once at load: describing per call would pay a table scan on every
\ register a pass names, so the ordinal NMACH hands back is kept. A frame access
\ is a mov through a signed disp32 counted in bytes, not scaled by the access
\ width, so every width reaches the same distance above and below its base.
: DESCRIBE ( -- n )
   X64IR:REGFILE
   NREGFILE:REGS-NONE                   \ no link register: the return address is pushed
   X64IR:SP-GPR X64IR:SP-ALIGN X64IR:FRAME-LIMIT X64IR:DISP-LIMIT
   NMACH-OFFSCALE:BY-BYTE WIDTHS-N X64IR:DISP-LIMIT
   NMACH:MACHINE NMACH:ID ;

DESCRIBE constant ROW

public

\ ---- the description ---------------------------------------------------------
\ No fast readers as for ARM64: nothing asks, and one would copy a fact x64ir.f
\ owns.
: MACHINE ( -- NMACH:mach )   ROW NMACH:BY-ID ;

\ ---- the register the running engine keeps its data-stack pointer in ---------
\ The twin of A64M:DSTACK-GPR. It is ENGINE-GPR:X64-DSTACK by name, not
\ ENGINE-GPR:DSTACK: an ARM64 engine loads this file to cross-build x86-64, and
\ its DSTACK is x19. src/arch/x86-64/rt.f emits its data-stack moves through the
\ same declaration, so compiler and engine code share one stack.
: DSTACK-GPR ( -- n )   ENGINE-GPR:X64-DSTACK ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
