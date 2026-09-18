\ regfile.f - acceptance suite for the register-file description.
\
\ Covers src/compiler/native/regfile.f through its public words, and the ARM64
\ description src/compiler/native/a64ir.f builds from it. Four things are owed:
\
\ 1. THE DERIVED SETS ARE DERIVED, AND FROM THE RIGHT HALF. Reserved is what a
\    backend declares and allocatable is what is left; clobbered is what a call
\    destroys and callee-saved is what survives. Each is checked against a set
\    this suite computes for itself out of the declaration, not against a
\    restatement of the module's own arithmetic, and on a description where the
\    two answers actually differ - a file with a non-empty callee-saved set -
\    because on ARM64 that set is empty and an implementation that returned zero
\    unconditionally would pass every arm64 case.
\
\ 2. A DESCRIPTION THAT DESCRIBES NO MACHINE IS REFUSED, one case per rule with
\    its exact code. These are the shapes a backend author gets wrong: a file of
\    no registers, a file wider than a set can name, a register reserved that the
\    file does not have, a file the virtual machine took all of, a call
\    destroying a register no routine could have held, and a slot width no scaled
\    access has.
\
\ 3. THE ARM64 DESCRIPTION IS DERIVED AND NOT WRITTEN AGAIN. Every field of
\    A64IR:REGFILE is asserted against the authority it was taken from -
\    src/compiler/native-effect.f for the file size and the reserved set, the
\    dialect's own frame access for the slot width - so a description that
\    started restating numbers reddens here. This is what keeps Darwin working:
\    x18 is reserved there and ordinary on Linux, the reserved set already knows
\    it, and a literal would not.
\
\ 4. THE x86_64 SHAPE IS EXPRESSIBLE AT ALL. Sixteen registers with six of them
\    the virtual machine's is the shape docs/x86-64.md fixes, and the point of
\    the description is that it can be stated before a backend exists. The
\    allocator's own suite takes it from here and allocates under it.

require lib/test.f
require src/compiler/native/regfile.f
require src/arch/arm64/machine.f
require src/compiler/native/a64ir.f

package REGFILE-TEST
private

\ A description is seven stack cells, so a case that only wants the throw has to
\ unmake what it built.
: DROP-FILE ( NREGFILE:file -- )
   NREGFILE-FILE:UNMAKE
   drop drop drop drop drop drop drop ;

\ `n` registers from `base`, built one register at a time so every member went
\ through REGS-REG - which is what refuses a register no file can name.
: RANGE ( n n -- NREGFILE:regs )
   {: base:n n:n :}
   NREGFILE:REGS-NONE
   n 0 ?do base i + NREGFILE:REGS-REG NREGFILE:REGS-WITH loop ;

\ ---- the shape the x86_64 backend will have ----------------------------------
\ Sixteen general registers, six of them the virtual machine's (rbp, rbx and
\ r12..r15 in docs/x86-64.md), so ten are allocatable; sixteen floating with none
\ reserved; eight-byte slots. Every allocatable number is one ARM64 leaves alone
\ too, so the allocator's suite can build NEFF pools over the same registers.
16 constant X64-N
10 constant X64-ALLOC-N
6 constant X64-RES-N

: X64-RESERVED ( -- NREGFILE:regs )   X64-ALLOC-N X64-RES-N RANGE ;

: X64-SHAPE ( -- NREGFILE:file )
   X64-N  X64-RESERVED  0 X64-ALLOC-N RANGE
   X64-N  NREGFILE:REGS-NONE  0 X64-N RANGE
   8 NREGFILE:FILE ;

\ The same machine under a convention that keeps two registers across a call, so
\ the callee-saved set is not empty and the derivation is measured where it says
\ something. Nothing on ARM64 exercises this.
2 constant X64-SAVED-N

: X64-SAVED ( -- NREGFILE:file )
   X64-N  X64-RESERVED  0 X64-ALLOC-N X64-SAVED-N - RANGE
   X64-N  NREGFILE:REGS-NONE  0 X64-N RANGE
   8 NREGFILE:FILE ;

\ ---- reading one field off a description --------------------------------------
: G-SIZE ( NREGFILE:file -- n )    NREGFILE:GPR-SIZE ;
: G-RES ( NREGFILE:file -- n )     NREGFILE:GPR-RESERVED NREGFILE:REGS-BITS ;
: G-ALC ( NREGFILE:file -- n )     NREGFILE:GPR-ALLOCATABLE NREGFILE:REGS-BITS ;
: G-CLB ( NREGFILE:file -- n )     NREGFILE:GPR-CLOBBERED NREGFILE:REGS-BITS ;
: G-SAV ( NREGFILE:file -- n )     NREGFILE:GPR-CALLEE-SAVED NREGFILE:REGS-BITS ;
: F-SIZE ( NREGFILE:file -- n )    NREGFILE:FPR-SIZE ;
: F-RES ( NREGFILE:file -- n )     NREGFILE:FPR-RESERVED NREGFILE:REGS-BITS ;
: F-ALC ( NREGFILE:file -- n )     NREGFILE:FPR-ALLOCATABLE NREGFILE:REGS-BITS ;
: F-SAV ( NREGFILE:file -- n )     NREGFILE:FPR-CALLEE-SAVED NREGFILE:REGS-BITS ;
: W ( NREGFILE:file -- n )         NREGFILE:SLOT-WIDTH ;

\ ---- what a case expects, computed here and not by the module ----------------
: MASK-N ( n -- n )   1 swap lshift 1- ;

: RANGE-BITS ( n n -- n )
   {: base:n n:n :}
   0
   n 0 ?do 1 base i + lshift or loop ;

public

\ ---- the representation's own bound ------------------------------------------
\ A set is a non-negative bit mask in one cell, so the sign bit names no register
\ and a file holds one less than a cell's bits. A case states the number as the
\ cell arithmetic rather than as 63, so a wider cell moves it here too.
: BOUNDS ( -- )
   NREGFILE:REG-MAX CELL 8 * 1- T= ;

\ ---- building sets -----------------------------------------------------------
: SETS ( -- )
   NREGFILE:REGS-NONE NREGFILE:REGS-BITS 0 T=
   0 NREGFILE:REGS-REG NREGFILE:REGS-BITS 1 T=
   5 NREGFILE:REGS-REG NREGFILE:REGS-BITS 1 5 lshift T=
   NREGFILE:REG-MAX 1- NREGFILE:REGS-REG NREGFILE:REGS-BITS
      1 NREGFILE:REG-MAX 1- lshift T=
   0 3 RANGE NREGFILE:REGS-BITS 7 T=
   10 6 RANGE NREGFILE:REGS-BITS 10 6 RANGE-BITS T=
   $FF NREGFILE:REGS-SET NREGFILE:REGS-BITS $FF T=
   \ A register the representation cannot name, either end.
   [: -1 NREGFILE:REGS-REG NREGFILE:REGS-BITS drop ;] E-NREGFILE TTHROWSQ
   [: NREGFILE:REG-MAX NREGFILE:REGS-REG NREGFILE:REGS-BITS drop ;]
      E-NREGFILE TTHROWSQ
   \ A mask with the sign bit set is not a set of registers.
   [: -1 NREGFILE:REGS-SET NREGFILE:REGS-BITS drop ;] E-NREGFILE TTHROWSQ ;

\ ---- the derived sets --------------------------------------------------------
: DERIVED ( -- )
   \ What the declaration says.
   X64-SHAPE G-SIZE X64-N T=
   X64-SHAPE G-RES X64-ALLOC-N X64-RES-N RANGE-BITS T=
   \ And what is derived from it: the file less the virtual machine's claim.
   X64-SHAPE G-ALC  X64-N MASK-N  X64-ALLOC-N X64-RES-N RANGE-BITS invert and T=
   X64-SHAPE G-ALC  0 X64-ALLOC-N RANGE-BITS T=
   \ Every allocatable register destroyed leaves nothing callee-saved.
   X64-SHAPE G-CLB  0 X64-ALLOC-N RANGE-BITS T=
   X64-SHAPE G-SAV  0 T=
   \ The floating file answers the same questions in the same words.
   X64-SHAPE F-SIZE X64-N T=
   X64-SHAPE F-RES  0 T=
   X64-SHAPE F-ALC  X64-N MASK-N T=
   X64-SHAPE F-SAV  0 T=
   X64-SHAPE W 8 T= ;

\ The derivation where it says something: two allocatable registers a call does
\ not destroy. The allocatable set is unchanged - what a call does to a register
\ is a different fact from whether a routine may have it at all.
: CALLEE-SAVED ( -- )
   X64-SAVED G-ALC  0 X64-ALLOC-N RANGE-BITS T=
   X64-SAVED G-CLB  0 X64-ALLOC-N X64-SAVED-N - RANGE-BITS T=
   X64-SAVED G-SAV
      X64-ALLOC-N X64-SAVED-N -  X64-SAVED-N  RANGE-BITS T= ;

\ ---- a description that describes no machine ---------------------------------
: REFUSALS ( -- )
   \ A file of no registers.
   [: 0 NREGFILE:REGS-NONE NREGFILE:REGS-NONE
      X64-N NREGFILE:REGS-NONE 0 X64-N RANGE  8 NREGFILE:FILE DROP-FILE ;]
      E-NREGFILE TTHROWSQ
   \ A file wider than a set can name.
   [: NREGFILE:REG-MAX 1+ NREGFILE:REGS-NONE NREGFILE:REGS-NONE
      X64-N NREGFILE:REGS-NONE 0 X64-N RANGE  8 NREGFILE:FILE DROP-FILE ;]
      E-NREGFILE TTHROWSQ
   \ A register reserved that this file does not have.
   [: X64-N  X64-N 1 RANGE  0 X64-ALLOC-N RANGE
      X64-N NREGFILE:REGS-NONE 0 X64-N RANGE  8 NREGFILE:FILE DROP-FILE ;]
      E-NREGFILE TTHROWSQ
   \ A file the virtual machine took every register of.
   [: X64-N  0 X64-N RANGE  NREGFILE:REGS-NONE
      X64-N NREGFILE:REGS-NONE 0 X64-N RANGE  8 NREGFILE:FILE DROP-FILE ;]
      E-NREGFILE TTHROWSQ
   \ A call destroying a register no routine could have been holding.
   [: X64-N  X64-RESERVED  0 X64-N RANGE
      X64-N NREGFILE:REGS-NONE 0 X64-N RANGE  8 NREGFILE:FILE DROP-FILE ;]
      E-NREGFILE TTHROWSQ
   \ The floating file is judged by the same rule and not a weaker one.
   [: X64-N  X64-RESERVED  0 X64-ALLOC-N RANGE
      X64-N  0 X64-N RANGE  NREGFILE:REGS-NONE  8 NREGFILE:FILE DROP-FILE ;]
      E-NREGFILE TTHROWSQ
   \ A slot width no scaled access has.
   [: X64-SHAPE DROP-FILE
      X64-N X64-RESERVED 0 X64-ALLOC-N RANGE
      X64-N NREGFILE:REGS-NONE 0 X64-N RANGE  0 NREGFILE:FILE DROP-FILE ;]
      E-NREGFILE TTHROWSQ
   [: X64-N X64-RESERVED 0 X64-ALLOC-N RANGE
      X64-N NREGFILE:REGS-NONE 0 X64-N RANGE  3 NREGFILE:FILE DROP-FILE ;]
      E-NREGFILE TTHROWSQ
   \ And the derivation exposed for a reader holding the numbers is held to the
   \ same bound as the constructor.
   [: 0 NREGFILE:REGS-NONE NREGFILE:ALLOCATABLE-MASK drop ;] E-NREGFILE TTHROWSQ
   [: NREGFILE:REG-MAX 1+ NREGFILE:REGS-NONE NREGFILE:ALLOCATABLE-MASK drop ;]
      E-NREGFILE TTHROWSQ ;

\ ---- the ARM64 description ---------------------------------------------------
\ Every field against the authority it was derived from. A64M:RESERVED-GPRS
\ already folds the platform register and everything the engine occupies
\ together, so asserting against it is what proves the description did not
\ restate a number - and what keeps Darwin, where x18 is reserved, correct.
: ARM64 ( -- )
   A64IR:REGFILE G-SIZE A64M:FILE-SIZE T=
   A64IR:REGFILE G-RES A64M:RESERVED-GPRS T=
   A64IR:REGFILE G-ALC A64M:MACHINE NEFF:GPR-ALL NEFF:GPRS-N T=
   A64IR:REGFILE G-ALC
      A64M:FILE-SIZE MASK-N A64M:RESERVED-GPRS invert and T=
   \ The Habu convention declares the whole pool destroyed on every routine, so
   \ a call leaves no general register alone.
   A64IR:REGFILE G-CLB A64M:MACHINE NEFF:GPR-ALL NEFF:GPRS-N T=
   A64IR:REGFILE G-SAV 0 T=
   A64IR:REGFILE F-SIZE A64M:FILE-SIZE T=
   A64IR:REGFILE F-RES 0 T=
   A64IR:REGFILE F-ALC A64M:MACHINE NEFF:FPR-ALL NEFF:FPRS-N T=
   A64IR:REGFILE F-SAV 0 T=
   A64IR:REGFILE W A64IR:SLOT-WIDTH T=
   \ The engine's own registers are reserved and not allocatable, which is the
   \ one thing a description getting this wrong would break silently.
   A64IR:REGFILE G-ALC  A64M:ENGINE-GPRS and  0 T=
   A64IR:REGFILE G-ALC  1 A64M:LINK-GPR lshift and  0 T=
   A64IR:REGFILE G-ALC  1 A64M:ZERO-GPR lshift and  0 T= ;

: RUN ( -- )
   T-RESET
   BOUNDS
   SETS
   DERIVED
   CALLEE-SAVED
   REFUSALS
   ARM64
   T-REPORT ;

;package

REGFILE-TEST:RUN
