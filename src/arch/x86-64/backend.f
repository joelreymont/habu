\ backend.f - the x86-64 backend's row in the compiler's target registry.
\
\ WHAT IT IS FOR. src/compiler/target.f keeps no list of architectures that have
\ a backend; a backend puts itself in the registry when it loads. This file is
\ that act for x86-64, the same act src/arch/arm64/backend.f is for AArch64, so
\ the arm64 engine carries no x86-64 row and an engine that loads this backend
\ carries one without the registry knowing either architecture by name.
\
\ WHY IT IS ITS OWN FILE, AND WHY IT IS THIS SMALL. It is required by the x86-64
\ machine dialect and by the x86-64 emitter, which is what makes the row present
\ whenever any of this backend's code is loaded and absent when none of it is.
\ It therefore requires nothing but the registry: not the machine dialect (which
\ requires it), not src/arch/x86-64/asm.f, and nothing under src/os/. The x86-64
\ ASSEMBLER is a different layer with a different audience - a cross-built image
\ is written with it before any compiler source is read - so nothing here is
\ reachable from an engine prefix, and no prefix grows a compiler dependency
\ because this file exists.

require lib/prelude.f
require src/compiler/target.f

package X64BACK
private

\ THE MACHINES THIS BACKEND SERVES. x86-64, little-endian, 64-bit addresses -
\ the layout src/compiler/native/x64ir.f builds and the x86-64 emitter writes.
\ The ABI is deliberately not part of it, for the reason the arm64 row gives:
\ which convention the host runs is src/compiler/native/abi.f's answer, not this
\ one. Byte order and pointer width are asked even though src/compiler/target.f
\ admits no other x86-64 contract today, because this predicate answers for the
\ machine this backend was written for and not for the table's current shape: a
\ contract the table widens to and this backend has not been taught is a
\ refusal, not an assumption.
\
\ The contract arrives already revalidated: CTARGET:LOWERS? and CTARGET:EMITS?
\ are its only callers and validate before they resolve the row.
: SERVES? ( CTARGET:contract -- bool )
   CTARGET-CONTRACT:UNMAKE drop
   {: a:CTARGET:arch b:CTARGET:abi e:CTARGET:endian p:CTARGET:ptr-width :}
   a CTARGET-ARCH:X86-64 CTARGET-ARCH:EQ
   e CTARGET-ENDIAN:LITTLE CTARGET-ENDIAN:EQ and
   p CTARGET-PTR--WIDTH:BITS64 CTARGET-PTR--WIDTH:EQ and ;

public

\ Both stages answer with the same predicate because this backend lowers for
\ exactly the machines it emits for. They are still two rows: the machine-IR
\ constructor and the emitter ask separately, each with its own refusal.
: INSTALL ( -- )
   CTARGET-ARCH:X86-64 [: SERVES? ;] [: SERVES? ;] CTARGET:REGISTER ;

;package

X64BACK:INSTALL
