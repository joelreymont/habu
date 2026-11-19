split-str
\ x86-64-seam.f - the x86_64 Linux seam from an aarch64 engine: the mov r64,
\ imm64 relocation site kind (RELOC) over hand-built fixtures, and the x86-64
\ row of src/compiler/target.f. HB-TARGET-LINUX-X86-64? is false on this host,
\ so the x86-64 arms of host selectors (src/compiler/native/abi.f,
\ src/habu/prof.f) are not reached here.

require src/lib/test.f
require src/lib/error.f
require src/arch/x86-64/asm.f
require src/habu/address-carrier.f
require src/compiler/target.f

package X64-SEAM-TEST

\ ---- the mov r64, imm64 relocation site --------------------------------------
\ A site is ten bytes: REX.W (with B for r8..r15), B8+rd, then the eight
\ immediate bytes the patch replaces.
create SITE 16 allot
create NEAR 16 allot

: SITE-PTR ( -- ^u8 )  SITE BYTE-VIEW ;
: NEAR-PTR ( -- ^u8 )  NEAR BYTE-VIEW ;

: BUILD-SITE ( n n n -- ) {: rex:n op:n val:n :}
   rex SITE-PTR c!
   op SITE-PTR 1+ c!
   8 0 ?do  val i 8 * rshift $FF and  SITE-PTR 2 + i + c!  loop ;

: NEAR! ( n n -- ) {: rex:n op:n :}
   rex NEAR-PTR c!
   op NEAR-PTR 1+ c!
   8 0 ?do  0 NEAR-PTR 2 + i + c!  loop ;

: MOVABS-CASES ( -- )
   s" the site kind is ten bytes patched at offset two, eight wide" T-LABEL
   ADDRESS-CARRIER:MOVABS-BYTES 10 T=
   ADDRESS-CARRIER:MOVABS-IMM-OFF 2 T=
   ADDRESS-CARRIER:MOVABS-IMM-BYTES 8 T=

   \ The site kind and the assembler each state where the immediate begins.
   \ src/habu/address-carrier.f cannot read X64ASM's constant: it is in every
   \ engine's payload and the encoder would follow it there, so the two numbers
   \ are one fact in two files and this case keeps them equal.
   s" the assembler names the same patch offset as the site kind" T-LABEL
   ADDRESS-CARRIER:MOVABS-IMM-OFF X64ASM:MOV-RI64-IMM-OFF T=

   s" mov rax, imm64 reads back the immediate it carries" T-LABEL
   $48 $B8 $123456789ABCDEF BUILD-SITE
   SITE-PTR ADDRESS-CARRIER:MOVABS-SITE? TTRUE
   SITE-PTR ADDRESS-CARRIER:MOVABSV $123456789ABCDEF T=

   s" mov r15, imm64 is a site too, and REX.B and the register survive a rewrite" T-LABEL
   $49 $BF 0 BUILD-SITE
   SITE-PTR ADDRESS-CARRIER:MOVABS-SITE? TTRUE
   SITE-PTR -1 ADDRESS-CARRIER:SET-MOVABS
   SITE-PTR ADDRESS-CARRIER:MOVABSV -1 T=
   SITE-PTR c@ $49 T=
   SITE-PTR 1+ c@ $BF T=

   s" a rewrite replaces the whole immediate, high bytes included" T-LABEL
   $48 $B9 -1 BUILD-SITE
   SITE-PTR $FF ADDRESS-CARRIER:SET-MOVABS
   SITE-PTR ADDRESS-CARRIER:MOVABSV $FF T=
   SITE-PTR 9 + c@ 0 T=

   s" a byte outside the REX.W wall is not a site" T-LABEL
   $40 $B8 NEAR! NEAR-PTR ADDRESS-CARRIER:MOVABS-SITE? TFALSE
   $4C $B8 NEAR! NEAR-PTR ADDRESS-CARRIER:MOVABS-SITE? TFALSE
   $B8 $B8 NEAR! NEAR-PTR ADDRESS-CARRIER:MOVABS-SITE? TFALSE

   s" a 64-bit move that is not the imm64 form is not a site" T-LABEL
   $48 $C7 NEAR! NEAR-PTR ADDRESS-CARRIER:MOVABS-SITE? TFALSE
   $48 $89 NEAR! NEAR-PTR ADDRESS-CARRIER:MOVABS-SITE? TFALSE
   $48 $B7 NEAR! NEAR-PTR ADDRESS-CARRIER:MOVABS-SITE? TFALSE
   $48 $C0 NEAR! NEAR-PTR ADDRESS-CARRIER:MOVABS-SITE? TFALSE ;

\ ---- the target contract -----------------------------------------------------
: X64-FEATURES ( -- CTARGET:features )
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH ;

: X64 ( -- CTARGET:contract )
   CTARGET-ARCH:X86-64 CTARGET-ABI:SYSV-AMD64 CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64 X64-FEATURES CTARGET:CONTRACT ;

: CONTRACT-CASES ( -- )
   s" the x86-64 contract resolves and is its own identity" T-LABEL
   X64 X64 CTARGET:SAME? TTRUE
   X64 CTARGET:VALIDATE CTARGET:ARCH@ CTARGET-ARCH:X86-64 CTARGET-ARCH:EQ TTRUE
   X64 CTARGET:PTR-BITS 64 T=

   s" its identity differs from the aarch64 Linux contract's" T-LABEL
   X64
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-LINUX CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64 X64-FEATURES CTARGET:CONTRACT
   CTARGET:SAME? TFALSE

   s" an AArch64 ABI on an x86-64 core is refused" T-LABEL
   [: CTARGET-ARCH:X86-64 CTARGET-ABI:AAPCS64-LINUX CTARGET-ENDIAN:LITTLE
      CTARGET-PTR--WIDTH:BITS64 X64-FEATURES CTARGET:CONTRACT drop ;]
   E-CTGT-ABI TTHROWSQ

   s" SysV AMD64 on an AArch64 core is refused" T-LABEL
   [: CTARGET-ARCH:AARCH64 CTARGET-ABI:SYSV-AMD64 CTARGET-ENDIAN:LITTLE
      CTARGET-PTR--WIDTH:BITS64 X64-FEATURES CTARGET:CONTRACT drop ;]
   E-CTGT-ABI TTHROWSQ

   s" big-endian x86-64 is refused" T-LABEL
   [: CTARGET-ARCH:X86-64 CTARGET-ABI:SYSV-AMD64 CTARGET-ENDIAN:BIG
      CTARGET-PTR--WIDTH:BITS64 X64-FEATURES CTARGET:CONTRACT drop ;]
   E-CTGT-ENDIAN TTHROWSQ

   s" 32-bit pointers under SysV AMD64 are refused" T-LABEL
   [: CTARGET-ARCH:X86-64 CTARGET-ABI:SYSV-AMD64 CTARGET-ENDIAN:LITTLE
      CTARGET-PTR--WIDTH:BITS32 X64-FEATURES CTARGET:CONTRACT drop ;]
   E-CTGT-PTR TTHROWSQ

   s" a feature no x86-64 core has is refused" T-LABEL
   [: CTARGET-ARCH:X86-64 CTARGET-ABI:SYSV-AMD64 CTARGET-ENDIAN:LITTLE
      CTARGET-PTR--WIDTH:BITS64
      X64-FEATURES CTARGET:F-TF32 CTARGET:WITH CTARGET:CONTRACT drop ;]
   E-CTGT-FEATURE TTHROWSQ ;

: RUN ( -- )
   T-RESET
   MOVABS-CASES
   CONTRACT-CASES
   T-REPORT ;

RUN
;package
