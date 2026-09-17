\ target-registry.f - acceptance suite for the compiler's backend registry.
\
\ Covers the registry in src/compiler/target.f and the registration in
\ src/arch/arm64/backend.f through their public production words. Four
\ properties are owed:
\
\ 1. AN ARCHITECTURE WITH NO BACKEND REFUSES, AND SAYS SO AS ITSELF. Asking
\    either stage about a PTX, ARM32, Thumb-2 or C66x contract - every coherent
\    machine in the family that no loaded module serves - throws
\    E-CTGT-UNLOADED, and so does asking for its row. This is not the same
\    refusal as a backend declining a machine, and property 2 shows both.
\
\ 2. THE ARM64 BACKEND IS REGISTERED BY HAVING LOADED, AND ANSWERS FOR ITSELF.
\    The host contract passes both stages; a coherent BIG-ENDIAN AArch64
\    contract is resolved - the row is there - and then declined by the backend
\    itself, which is the distinction the registry exists to keep.
\
\ 3. THE ROW IS TYPED, AND CLAIMED ONCE. A registration whose stages are raw
\    cells, or quotations of the wrong effect, or whose arguments are swapped,
\    is refused by the CHECKER - proved with candidate definitions it declines
\    to certify - so no such row can exist at runtime. A second module claiming
\    an architecture that already has a row throws E-CTGT-REGISTERED rather than
\    replacing it.
\
\ 4. BACKENDS DO NOT COLLIDE, AND THE TABLE IS BOUNDED. A second backend
\    registering its own architecture leaves the first row answering exactly as
\    before, each contract reaching its own backend's predicate; claiming past
\    the last free row throws E-CTGT-ROW.
\
\ Property 4 registers backends of its own, so it runs last: the registry is
\ process-wide and a row is never released.

require lib/test.f
require lib/string.f
require test/checker-assert.f
require src/compiler/target.f
require src/arch/arm64/backend.f

package CTREG-TEST
private

\ ---- the machines this suite asks about --------------------------------------
\ Every one of them is coherent, so CTARGET:CONTRACT builds it and the only
\ question left is the backend's.
: MK ( CTARGET:arch CTARGET:abi CTARGET:endian CTARGET:ptr-width -- CTARGET:contract )
   CTARGET:F-BASE CTARGET:CONTRACT ;

: HOST ( -- CTARGET:contract )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-LINUX CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64 MK ;

: HOST-DARWIN ( -- CTARGET:contract )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64 MK ;

\ AArch64 big-endian is a real machine and a declarable contract; this backend
\ does not serve it.
: HOST-BIG ( -- CTARGET:contract )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-LINUX CTARGET-ENDIAN:BIG
   CTARGET-PTR--WIDTH:BITS64 MK ;

: GPU ( -- CTARGET:contract )
   CTARGET-ARCH:PTX CTARGET-ABI:PTX-KERNEL CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64 MK ;

: ARM32 ( -- CTARGET:contract )
   CTARGET-ARCH:A32 CTARGET-ABI:AAPCS32 CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS32 MK ;

: THUMB ( -- CTARGET:contract )
   CTARGET-ARCH:THUMB2 CTARGET-ABI:AAPCS32 CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS32 MK ;

: C66 ( -- CTARGET:contract )
   CTARGET-ARCH:C66X CTARGET-ABI:C6000-EABI CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS32 MK ;

\ ---- 1. no backend loaded ----------------------------------------------------
: NO-GPU-LOWER ( -- )   GPU CTARGET:LOWERS? drop ;
: NO-GPU-EMIT ( -- )    GPU CTARGET:EMITS? drop ;
: NO-GPU-ROW ( -- )     CTARGET-ARCH:PTX CTARGET:ROW drop ;
: NO-ARM32-LOWER ( -- ) ARM32 CTARGET:LOWERS? drop ;
: NO-THUMB-LOWER ( -- ) THUMB CTARGET:LOWERS? drop ;
: NO-C66-EMIT ( -- )    C66 CTARGET:EMITS? drop ;

: UNLOADED ( -- )
   s" an architecture with no backend loaded is refused by name" T-LABEL
   CTARGET-ARCH:PTX CTARGET:REGISTERED? 0= TTRUE
   CTARGET-ARCH:A32 CTARGET:REGISTERED? 0= TTRUE
   CTARGET-ARCH:THUMB2 CTARGET:REGISTERED? 0= TTRUE
   CTARGET-ARCH:C66X CTARGET:REGISTERED? 0= TTRUE
   [: NO-GPU-LOWER ;] E-CTGT-UNLOADED TTHROWSQ
   [: NO-GPU-EMIT ;] E-CTGT-UNLOADED TTHROWSQ
   [: NO-GPU-ROW ;] E-CTGT-UNLOADED TTHROWSQ
   [: NO-ARM32-LOWER ;] E-CTGT-UNLOADED TTHROWSQ
   [: NO-THUMB-LOWER ;] E-CTGT-UNLOADED TTHROWSQ
   [: NO-C66-EMIT ;] E-CTGT-UNLOADED TTHROWSQ ;

\ ---- 2. the host backend registered itself by loading ------------------------
: REGISTERED ( -- )
   s" the arm64 backend holds a row because src/arch/arm64/backend.f loaded" T-LABEL
   CTARGET-ARCH:AARCH64 CTARGET:REGISTERED? TTRUE
   CTARGET-ARCH:AARCH64 CTARGET:ROW 0 >= TTRUE
   CTARGET-ARCH:AARCH64 CTARGET:ROW CTARGET:BACKEND-ROWS < TTRUE
   s" both stages serve the host, on either AAPCS64 ABI" T-LABEL
   HOST CTARGET:LOWERS? TTRUE
   HOST CTARGET:EMITS? TTRUE
   HOST-DARWIN CTARGET:LOWERS? TTRUE
   HOST-DARWIN CTARGET:EMITS? TTRUE
   s" a resolved backend declining a machine is not an unloaded architecture" T-LABEL
   HOST-BIG CTARGET:LOWERS? 0= TTRUE
   HOST-BIG CTARGET:EMITS? 0= TTRUE ;

\ ---- 3. the row is typed, and claimed once -----------------------------------
: NEVER ( CTARGET:contract -- bool )  drop false ;
: ALWAYS ( CTARGET:contract -- bool ) drop true ;

: SECOND-CLAIM ( -- )
   CTARGET-ARCH:AARCH64 [: NEVER ;] [: NEVER ;] CTARGET:REGISTER ;

: TYPED-ROWS ( -- )
   s" a registration states both stage effects, or the checker declines it" T-LABEL
   s" CTR-OK ( CTARGET:arch [ CTARGET:contract -- bool ] [ CTARGET:contract -- bool ] -- ) CTARGET:REGISTER"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" CTR-CELL ( CTARGET:arch n n -- ) CTARGET:REGISTER"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" CTR-EFFECT ( CTARGET:arch [ n -- n ] [ CTARGET:contract -- bool ] -- ) CTARGET:REGISTER"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" CTR-RESULT ( CTARGET:arch [ CTARGET:contract -- n ] [ CTARGET:contract -- bool ] -- ) CTARGET:REGISTER"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" CTR-SWAP ( [ CTARGET:contract -- bool ] CTARGET:arch [ CTARGET:contract -- bool ] -- ) CTARGET:REGISTER"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" CTR-SHORT ( CTARGET:arch [ CTARGET:contract -- bool ] -- ) CTARGET:REGISTER"
      CHECK-QUIET-CANDIDATE! 0 T= ;

: CLAIMED-ONCE ( -- )
   s" a second module claiming a registered architecture is refused" T-LABEL
   [: SECOND-CLAIM ;] E-CTGT-REGISTERED TTHROWSQ
   s" and the row it tried to take still answers as the loaded backend does" T-LABEL
   HOST CTARGET:LOWERS? TTRUE
   HOST CTARGET:EMITS? TTRUE ;

\ ---- 4. a second backend, and the bound --------------------------------------
\ Registrations from here on are this suite's own and are never released, so
\ nothing above may run after them.
: PAST-LAST-ROW ( -- )
   CTARGET-ARCH:C66X [: ALWAYS ;] [: ALWAYS ;] CTARGET:REGISTER ;

: SECOND-BACKEND ( -- )
   s" a second backend takes its own row and leaves the first one alone" T-LABEL
   CTARGET-ARCH:PTX [: NEVER ;] [: ALWAYS ;] CTARGET:REGISTER
   CTARGET-ARCH:PTX CTARGET:REGISTERED? TTRUE
   CTARGET-ARCH:PTX CTARGET:ROW CTARGET-ARCH:AARCH64 CTARGET:ROW <> TTRUE
   s" and each contract is answered by its own architecture's backend" T-LABEL
   GPU CTARGET:LOWERS? 0= TTRUE
   GPU CTARGET:EMITS? TTRUE
   HOST CTARGET:LOWERS? TTRUE
   HOST CTARGET:EMITS? TTRUE ;

: TABLE-BOUND ( -- )
   s" the last free rows can be claimed, and one claim past them is refused" T-LABEL
   CTARGET:BACKEND-ROWS 4 T=
   CTARGET-ARCH:A32 [: ALWAYS ;] [: ALWAYS ;] CTARGET:REGISTER
   CTARGET-ARCH:THUMB2 [: ALWAYS ;] [: ALWAYS ;] CTARGET:REGISTER
   ARM32 CTARGET:LOWERS? TTRUE
   THUMB CTARGET:EMITS? TTRUE
   [: PAST-LAST-ROW ;] E-CTGT-ROW TTHROWSQ
   CTARGET-ARCH:C66X CTARGET:REGISTERED? 0= TTRUE ;

public

: RUN ( -- )
   T-RESET
   UNLOADED
   REGISTERED
   TYPED-ROWS
   CLAIMED-ONCE
   SECOND-BACKEND
   TABLE-BOUND
   T-REPORT ;

;package

CTREG-TEST:RUN
