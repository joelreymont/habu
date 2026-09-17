\ backend.f - the ARM64 backend's row in the compiler's target registry.
\
\ WHAT IT IS FOR. src/compiler/target.f keeps no list of architectures that have
\ a backend; a backend puts itself in the registry when it loads. This file is
\ that act for ARM64, so the host backend is registered exactly the way a second
\ architecture will be and is not a special case the registry has to know about.
\
\ WHY IT IS ITS OWN FILE, AND WHY IT IS THIS SMALL. It is required by the ARM64
\ machine IR and by the ARM64 emitter, which is what makes the row present
\ whenever any of this backend's code is loaded and absent when none of it is.
\ It therefore has to load wherever they do, and it requires nothing but the
\ registry: not the machine IR (which requires it), not the assembler, and
\ nothing under src/os/. The ARM64 ASSEMBLER (asm.f, icode.f, mnem.f) is a
\ different layer with a different audience - the engine's own image is written
\ with it, long before any compiler source is read - so nothing here is reachable
\ from the engine prefix or from the recovery chain, and neither of those grows
\ a compiler dependency because this file exists.

require lib/prelude.f
require src/compiler/target.f

package A64BACK
private

\ THE MACHINES THIS BACKEND SERVES. AArch64, little-endian, 64-bit addresses -
\ the layout src/compiler/native/a64ir.f builds and src/compiler/native/emit.f
\ writes. The ABI is deliberately not part of it: both AAPCS64 variants are
\ served, and which one the host runs is src/compiler/native/abi.f's answer, not
\ this one. A big-endian AArch64 contract is a coherent machine this backend
\ does not serve, which is a different refusal from an architecture that has no
\ backend loaded at all.
\
\ The contract arrives already revalidated: CTARGET:LOWERS? and CTARGET:EMITS?
\ are its only callers and validate before they resolve the row.
: SERVES? ( CTARGET:contract -- bool )
   CTARGET-CONTRACT:UNMAKE drop
   {: a:CTARGET:arch b:CTARGET:abi e:CTARGET:endian p:CTARGET:ptr-width :}
   a CTARGET-ARCH:AARCH64 CTARGET-ARCH:EQ
   e CTARGET-ENDIAN:LITTLE CTARGET-ENDIAN:EQ and
   p CTARGET-PTR--WIDTH:BITS64 CTARGET-PTR--WIDTH:EQ and ;

public

\ Both stages answer with the same predicate because this backend lowers for
\ exactly the machines it emits for. They are still two rows: the machine-IR
\ constructor and the emitter ask separately, each with its own refusal, and a
\ backend whose lowering outruns its encodings would answer differently here.
: INSTALL ( -- )
   CTARGET-ARCH:AARCH64 [: SERVES? ;] [: SERVES? ;] CTARGET:REGISTER ;

;package

A64BACK:INSTALL
