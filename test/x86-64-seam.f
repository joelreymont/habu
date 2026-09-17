\ x86-64-seam.f - the x86_64 Linux seam, exercised from an aarch64 engine.
\
\ Three things this can prove on this host, and one it cannot.
\
\ THE IMAGE WRITER. src/os/linux-x86-64/elf.f is ordinary Habu: it reads the
\ assembler's code buffer and writes bytes into the shared image cursor
\ (src/os/image-bytes.f), so an aarch64 engine can write an x86_64 executable
\ header as readily as its own. That is the cross-build step docs/x86-64.md
\ names, reduced to the part that exists: BUILD-ELF over an empty code window,
\ read back FIELD BY FIELD rather than compared against a recorded blob, because
\ a blob says "these bytes changed" where a field says which one and to what.
\ The seam's layout.f cannot be loaded beside the host's - both spell CODE-OFF
\ and the engine refuses a duplicate definition - so the writer is loaded on its
\ own, over the host's identical CODE-OFF and PROT-PAGE-MAX. The two values are
\ pinned here, so this test fails rather than drifts if either target moves one.
\
\ THE RELOCATION SITE KIND. The mov r64, imm64 site (package SNAP-RELOC) is
\ recognised, read and rewritten here over hand-built fixtures, including the
\ near misses that must NOT read as a site.
\
\ THE TARGET CONTRACT. The x86-64 row in src/compiler/target.f resolves,
\ digests, and refuses each incoherent combination with its own named error.
\
\ WHAT IT CANNOT PROVE. HB-TARGET-LINUX-X86-64? is a constant false on this
\ host, so no test here can reach the x86-64 arm of a host selector
\ (src/compiler/native/abi.f, src/habu/prof.f): those are inspection-only until
\ an x86_64 engine exists.

require lib/test.f
require lib/errors.f
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/os/image-bytes.f
require src/habu/aot-decl.f
require src/compiler/target.f

s" src/os/linux-x86-64/elf.f" required

package X64-SEAM-TEST

\ ---- reading the written image ----------------------------------------------
: U8@ ( n -- n )
   MBUF swap M-BYTE@ ;

: U16@ ( n -- n ) {: off:n :}
   off U8@  off 1+ U8@ 8 lshift or ;

: U32@ ( n -- n )
   M-OFF M-LE32@ ;

: U64@ ( n -- n ) {: off:n :}
   off U32@  off 4 + U32@ 32 lshift or ;

\ ---- the fields a loader reads ----------------------------------------------
$400000 constant WANT-VMBASE
$1000 constant WANT-CODE-OFF
62 constant WANT-EM-X86-64
2 constant WANT-ELFCLASS64
1 constant WANT-ELFDATA2LSB
64 constant PHOFF
56 constant PHENT

: PH ( n n -- n ) {: i:n field:n :}
   PHOFF i PHENT * + field + ;

: PH-TYPE@ ( n -- n )   0 PH U32@ ;
: PH-FLAGS@ ( n -- n )  4 PH U32@ ;
: PH-OFF@ ( n -- n )    8 PH U64@ ;
: PH-VADDR@ ( n -- n )  $10 PH U64@ ;
: PH-PADDR@ ( n -- n )  $18 PH U64@ ;
: PH-FILESZ@ ( n -- n ) $20 PH U64@ ;
: PH-MEMSZ@ ( n -- n )  $28 PH U64@ ;
: PH-ALIGN@ ( n -- n )  $30 PH U64@ ;

: CHECK-IDENT ( -- )
   s" e_ident names a 64-bit little-endian ELF" T-LABEL
   0 U8@ $7F T=  1 U8@ 69 T=  2 U8@ 76 T=  3 U8@ 70 T=
   4 U8@ WANT-ELFCLASS64 T=
   5 U8@ WANT-ELFDATA2LSB T=
   6 U8@ 1 T=
   7 U8@ 0 T=  8 U8@ 0 T=
   9 U8@ 0 T=  10 U8@ 0 T=  11 U8@ 0 T=  12 U8@ 0 T=
   13 U8@ 0 T=  14 U8@ 0 T=  15 U8@ 0 T= ;

: CHECK-HEADER ( -- )
   s" the header names an x86-64 executable entered at CODE-OFF" T-LABEL
   $10 U16@ 2 T=                          \ e_type = ET_EXEC
   $12 U16@ WANT-EM-X86-64 T=             \ e_machine = EM_X86_64
   $14 U32@ 1 T=                          \ e_version = EV_CURRENT
   $18 U64@ WANT-VMBASE WANT-CODE-OFF + T= \ e_entry
   $20 U64@ PHOFF T=                      \ e_phoff
   $28 U64@ 0 T=                          \ e_shoff
   $30 U32@ 0 T=                          \ e_flags
   $34 U16@ 64 T=                         \ e_ehsize
   $36 U16@ PHENT T=                      \ e_phentsize
   $38 U16@ 4 T=                          \ e_phnum
   $3A U16@ 0 T=  $3C U16@ 0 T=  $3E U16@ 0 T= ;

: CHECK-RX-PHDR ( n -- ) {: textsz:n :}
   s" program header 0 loads the text read-execute from file offset zero" T-LABEL
   0 PH-TYPE@ 1 T=                        \ PT_LOAD
   0 PH-FLAGS@ 5 T=                       \ PF_R | PF_X
   0 PH-OFF@ 0 T=
   0 PH-VADDR@ WANT-VMBASE T=
   0 PH-PADDR@ WANT-VMBASE T=
   0 PH-FILESZ@ textsz T=
   0 PH-MEMSZ@ textsz T=
   0 PH-ALIGN@ PROT-PAGE-MAX T= ;

: CHECK-RW-PHDR ( n -- ) {: textsz:n :}
   s" program header 1 loads the read-write tail on its own page" T-LABEL
   1 PH-TYPE@ 1 T=                        \ PT_LOAD
   1 PH-FLAGS@ 6 T=                       \ PF_R | PF_W
   1 PH-OFF@ textsz T=
   1 PH-VADDR@ WANT-VMBASE textsz + T=
   1 PH-FILESZ@ $C0 T=
   1 PH-MEMSZ@ $C0 T=
   1 PH-ALIGN@ PROT-PAGE-MAX T=
   s" the two load segments share no page" T-LABEL
   textsz PROT-PAGE-MAX mod 0 T= ;

: CHECK-INTERP-PHDR ( -- )
   s" program header 2 names the x86-64 dynamic loader" T-LABEL
   2 PH-TYPE@ 3 T=                        \ PT_INTERP
   2 PH-FLAGS@ 4 T=                       \ PF_R
   2 PH-OFF@ $120 T=
   2 PH-VADDR@ WANT-VMBASE $120 + T=
   2 PH-FILESZ@ 28 T=
   2 PH-ALIGN@ 1 T= ;

: CHECK-DYNAMIC-PHDR ( n -- ) {: textsz:n :}
   s" program header 3 is the dynamic table inside the read-write segment" T-LABEL
   3 PH-TYPE@ 2 T=                        \ PT_DYNAMIC
   3 PH-FLAGS@ 6 T=
   3 PH-OFF@ textsz T=
   3 PH-VADDR@ WANT-VMBASE textsz + T=
   3 PH-FILESZ@ $B0 T=
   3 PH-ALIGN@ 8 T= ;

: CHECK-INTERP-BYTES ( -- )
   s" the interpreter string is /lib64/ld-linux-x86-64.so.2 and is terminated" T-LABEL
   MBUF $120 M-BYTE+ 27 s" /lib64/ld-linux-x86-64.so.2" T$=
   $120 27 + U8@ 0 T= ;

\ Both GOT slots are bound by a GLOB_DAT relocation of the x86-64 vocabulary:
\ symbol index in the high half, type in the low.
: CHECK-RELA ( n -- ) {: textsz:n :}
   s" each GOT slot takes an R_X86_64_GLOB_DAT against its own symbol" T-LABEL
   $1B8 U64@ WANT-VMBASE textsz + $B0 + T=
   $1C0 U64@ 1 32 lshift 6 or T=
   $1C8 U64@ 0 T=
   $1D0 U64@ WANT-VMBASE textsz + $B8 + T=
   $1D8 U64@ 2 32 lshift 6 or T=
   $1E0 U64@ 0 T= ;

: CHECK-LENGTH ( n -- ) {: textsz:n :}
   s" the image is the padded text plus the read-write segment" T-LABEL
   MLEN@ textsz $C0 + T= ;

: ELF-CASES ( -- )
   s" the writer's layout agrees with the engine it was loaded into" T-LABEL
   CODE-OFF WANT-CODE-OFF T=
   VMBASE WANT-VMBASE T=
   BUILD-ELF
   TEXTSZ {: textsz:n :}
   CHECK-IDENT
   CHECK-HEADER
   textsz CHECK-RX-PHDR
   textsz CHECK-RW-PHDR
   CHECK-INTERP-PHDR
   textsz CHECK-DYNAMIC-PHDR
   CHECK-INTERP-BYTES
   textsz CHECK-RELA
   textsz CHECK-LENGTH ;

\ ---- the mov r64, imm64 relocation site --------------------------------------
\ A site is ten bytes: REX.W (with B for r8..r15), B8+rd, then the eight
\ immediate bytes the patch replaces.
create SITE 16 allot
create NEAR 16 allot

: SITE-PTR ( -- ptr u8 )  SITE BYTE-VIEW ;
: NEAR-PTR ( -- ptr u8 )  NEAR BYTE-VIEW ;

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
   SNAP-RELOC:MOVABS-BYTES 10 T=
   SNAP-RELOC:MOVABS-IMM-OFF 2 T=
   SNAP-RELOC:MOVABS-IMM-BYTES 8 T=

   s" mov rax, imm64 reads back the immediate it carries" T-LABEL
   $48 $B8 $123456789ABCDEF BUILD-SITE
   SITE-PTR SNAP-RELOC:MOVABS-SITE? TTRUE
   SITE-PTR SNAP-RELOC:MOVABSV $123456789ABCDEF T=

   s" mov r15, imm64 is a site too, and REX.B and the register survive a rewrite" T-LABEL
   $49 $BF 0 BUILD-SITE
   SITE-PTR SNAP-RELOC:MOVABS-SITE? TTRUE
   SITE-PTR -1 SNAP-RELOC:SET-MOVABS
   SITE-PTR SNAP-RELOC:MOVABSV -1 T=
   SITE-PTR c@ $49 T=
   SITE-PTR 1+ c@ $BF T=

   s" a rewrite replaces the whole immediate, high bytes included" T-LABEL
   $48 $B9 -1 BUILD-SITE
   SITE-PTR $FF SNAP-RELOC:SET-MOVABS
   SITE-PTR SNAP-RELOC:MOVABSV $FF T=
   SITE-PTR 9 + c@ 0 T=

   s" a byte outside the REX.W wall is not a site" T-LABEL
   $40 $B8 NEAR! NEAR-PTR SNAP-RELOC:MOVABS-SITE? TFALSE
   $4C $B8 NEAR! NEAR-PTR SNAP-RELOC:MOVABS-SITE? TFALSE
   $B8 $B8 NEAR! NEAR-PTR SNAP-RELOC:MOVABS-SITE? TFALSE

   s" a 64-bit move that is not the imm64 form is not a site" T-LABEL
   $48 $C7 NEAR! NEAR-PTR SNAP-RELOC:MOVABS-SITE? TFALSE
   $48 $89 NEAR! NEAR-PTR SNAP-RELOC:MOVABS-SITE? TFALSE
   $48 $B7 NEAR! NEAR-PTR SNAP-RELOC:MOVABS-SITE? TFALSE
   $48 $C0 NEAR! NEAR-PTR SNAP-RELOC:MOVABS-SITE? TFALSE ;

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

   s" its digest differs from the aarch64 Linux contract's" T-LABEL
   X64 CTARGET:DIGEST
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-LINUX CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64 X64-FEATURES CTARGET:CONTRACT CTARGET:DIGEST
   CDIGEST-DIGEST:EQ TFALSE

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
   ELF-CASES
   MOVABS-CASES
   CONTRACT-CASES
   T-REPORT ;

RUN
;package
