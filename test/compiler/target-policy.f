\ target-policy.f - acceptance suite for the compiler target/policy binding.
\
\ Covers src/compiler/digest.f, src/compiler/target.f,
\ src/compiler/numeric-policy.f and src/compiler/binding.f through their public
\ production words. Three properties are owed by the dot, and each is proved over
\ the whole declarable domain rather than by example:
\
\ 1. MISSING OR ILLEGAL COMBINATIONS REJECT. A missing or role-swapped field is a
\    CHECKER refusal - proved with candidate definitions that the checker declines
\    to certify - so it never reaches runtime at all. An incoherent combination of
\    present fields is a named throw, one case per rule, with the exact code.
\
\ 2. EQUAL RECORDS DIGEST IDENTICALLY. Records built twice by separate calls
\    compare equal and digest equal, at every level including the binding.
\
\ 3. EVERY SEMANTIC FIELD CHANGES IDENTITY. The suite enumerates the ENTIRE legal
\    domain - 516 target contracts, 60 numerical policies - and shows the digests
\    are pairwise distinct, and that digest equality holds exactly where the
\    structural comparison holds. Enumerating everything is what makes this a
\    proof over the domain rather than a sample: it covers every field, every
\    variant of every field, and every interaction between them at once.
\
\ The enumeration is driven by this suite's own index arithmetic and its own
\ legality rules, not by the modules under test, so a rule that silently widened
\ would show up as a changed legal-domain size (the counts are asserted).

require lib/test.f
require lib/string.f
require test/checker-assert.f
require src/compiler/binding.f

package CTPOL-TEST
private

\ ---- enumeration shape -------------------------------------------------------
\ Target contracts: architecture x ABI x byte order x pointer width x every
\ feature mask over the nine defined bits. A single flat index keeps the sweep to
\ one loop; the projections below take it apart.
6 constant ARCH#
6 constant ABI#
2 constant END#
2 constant PTR#
$200 constant RAW#
ARCH# ABI# * END# * PTR# * RAW# * constant COMBO#
\ 484 before the x86-64 row: SysV AMD64 admits one byte order and one pointer
\ width, and MASK-X86-64 has five optional bits above the baseline, so it adds
\ exactly 32 contracts.
516 constant LEGAL-CONTRACTS

\ Numerical policies: the full product of the five families.
2 constant OVF#
2 constant FLT#
2 constant CON#
3 constant FM#
3 constant CMP#
OVF# FLT# * CON# * FM# * CMP# * constant PCOMBO#
60 constant LEGAL-POLICIES

\ One digest store, reused by each sweep in turn.
COMBO# constant STORE#
create DGA STORE# 4 * cells allot
create IXA STORE# cells allot
variable N

: IX! ( n n -- )
   {: v:n idx:n :}
   v IXA idx cells + ! ;

: IX@ ( n -- n )
   cells IXA + @ ;

: DG! ( n n n n n -- )
   {: w0:n w1:n w2:n w3:n idx:n :}
   idx 4 * {: at:n :}
   w0 DGA at cells + !
   w1 DGA at 1+ cells + !
   w2 DGA at 2 + cells + !
   w3 DGA at 3 + cells + ! ;

: DG= ( n n -- bool )
   {: x:n y:n :}
   x 4 * {: px:n :}
   y 4 * {: py:n :}
   DGA px cells + @      DGA py cells + @ =
   DGA px 1+ cells + @   DGA py 1+ cells + @ = and
   DGA px 2 + cells + @  DGA py 2 + cells + @ = and
   DGA px 3 + cells + @  DGA py 3 + cells + @ = and ;

\ Assert two truth values agree. `=` is not defined on booleans, so the check is
\ written as a case split.
: TBOOL= ( bool bool -- )
   {: got:bool want:bool :}
   want if got TTRUE exit then
   got TFALSE ;

\ ---- consuming layout values -------------------------------------------------
\ A record is several stack cells, so a test that only wants the throw has to
\ unmake what it built.
: DROP-CONTRACT ( CTARGET:contract -- )
   CTARGET-CONTRACT:UNMAKE drop drop drop drop drop ;

: DROP-POLICY ( CNUM:numeric-policy -- )
   CNUM-NUMERIC--POLICY:UNMAKE drop drop drop drop drop ;

: DROP-BINDING ( CBIND:binding -- )
   CBIND-BINDING:UNMAKE DROP-POLICY DROP-CONTRACT ;

: TRY-CONTRACT ( CTARGET:arch CTARGET:abi CTARGET:endian CTARGET:ptr-width CTARGET:features -- )
   CTARGET:CONTRACT DROP-CONTRACT ;

: TRY-POLICY ( CNUM:overflow CNUM:float-model CNUM:contraction CNUM:fast-math CNUM:compare -- )
   CNUM:POLICY DROP-POLICY ;

: TRY-BIND ( CTARGET:contract CNUM:numeric-policy -- )
   CBIND:BIND DROP-BINDING ;

\ ---- index projections -------------------------------------------------------
\ Total by construction: the last alternative is the fall-through, and every
\ caller drives these from a loop bounded by the family size.
: N>ARCH ( n -- CTARGET:arch )
   dup 0= if drop CTARGET-ARCH:AARCH64 exit then
   dup 1 = if drop CTARGET-ARCH:PTX exit then
   dup 2 = if drop CTARGET-ARCH:A32 exit then
   dup 3 = if drop CTARGET-ARCH:THUMB2 exit then
   4 = if CTARGET-ARCH:C66X exit then
   CTARGET-ARCH:X86-64 ;

: N>ABI ( n -- CTARGET:abi )
   dup 0= if drop CTARGET-ABI:AAPCS64-DARWIN exit then
   dup 1 = if drop CTARGET-ABI:AAPCS64-LINUX exit then
   dup 2 = if drop CTARGET-ABI:PTX-KERNEL exit then
   dup 3 = if drop CTARGET-ABI:AAPCS32 exit then
   4 = if CTARGET-ABI:C6000-EABI exit then
   CTARGET-ABI:SYSV-AMD64 ;

: N>ENDIAN ( n -- CTARGET:endian )
   0= if CTARGET-ENDIAN:LITTLE exit then
   CTARGET-ENDIAN:BIG ;

: N>PTR ( n -- CTARGET:ptr-width )
   0= if CTARGET-PTR--WIDTH:BITS32 exit then
   CTARGET-PTR--WIDTH:BITS64 ;

: N>OVF ( n -- CNUM:overflow )
   0= if CNUM-OVERFLOW:WRAP exit then
   CNUM-OVERFLOW:TRAP ;

: N>FLT ( n -- CNUM:float-model )
   0= if CNUM-FLOAT--MODEL:IEEE754 exit then
   CNUM-FLOAT--MODEL:FLUSH-DENORMAL ;

: N>CON ( n -- CNUM:contraction )
   0= if CNUM-CONTRACTION:FORBIDDEN exit then
   CNUM-CONTRACTION:ALLOWED ;

: N>FM ( n -- CNUM:fast-math )
   dup 0= if drop CNUM-FAST--MATH:BIT-EXACT exit then
   1 = if CNUM-FAST--MATH:REASSOCIATE exit then
   CNUM-FAST--MATH:APPROXIMATE ;

: N>CMP ( n -- CNUM:compare )
   dup 0= if drop CNUM-COMPARE:IEEE754-UNORDERED exit then
   1 = if CNUM-COMPARE:TOTAL-ORDER exit then
   CNUM-COMPARE:ASSUME-ORDERED ;

: C-RAW ( n -- n )   RAW# mod ;
: C-PTR ( n -- n )   RAW# / PTR# mod ;
: C-END ( n -- n )   RAW# PTR# * / END# mod ;
: C-ABI ( n -- n )   RAW# PTR# * END# * / ABI# mod ;
: C-ARCH ( n -- n )  RAW# PTR# * END# * ABI# * / ;

: P-CMP ( n -- n )   CMP# mod ;
: P-FM ( n -- n )    CMP# / FM# mod ;
: P-CON ( n -- n )   CMP# FM# * / CON# mod ;
: P-FLT ( n -- n )   CMP# FM# * CON# * / FLT# mod ;
: P-OVF ( n -- n )   CMP# FM# * CON# * FLT# * / ;

\ ---- this suite's own legality rules -----------------------------------------
\ AAPCS32 covers two instruction states. Embedded ABIs require 32-bit
\ pointers and support either data byte order; PTX permits either width.
\ SysV AMD64 is one machine's one ABI: little-endian, 64-bit, x86-64 only.
: OK-ABI? ( n n -- bool )
   {: arch:n abi:n :}
   arch 0= if abi 2 < exit then
   arch 1 = if abi 2 = exit then
   arch 4 = if abi 4 = exit then
   arch 5 = if abi 5 = exit then
   abi 3 = ;

: OK-ENDIAN? ( n n -- bool )
   {: abi:n endian:n :}
   endian 0= if true exit then
   abi 5 = if false exit then
   abi 1 = abi 3 >= or ;

: OK-PTR? ( n n -- bool )
   {: abi:n ptr:n :}
   abi 5 = if ptr 1 = exit then
   abi 3 >= if ptr 0= exit then
   ptr 1 = if true exit then
   abi 2 = ;

\ The feature mask must carry the baseline bit and nothing the architecture has
\ not. The architecture's mask is read back from the module under test; the
\ hostile per-feature cases below pin the rule itself with exact error codes.
: OK-FEAT? ( n n -- bool )
   {: arch:n raw:n :}
   CTARGET:F-BASE CTARGET:FEATURES-N {: base:n :}
   raw base and 0= if false exit then
   arch N>ARCH CTARGET:ARCH-MASK CTARGET:FEATURES-N {: mask:n :}
   raw mask invert and 0= ;

: OK-COMBO? ( n -- bool )
   {: c:n :}
   c C-ARCH c C-ABI OK-ABI? 0= if false exit then
   c C-ABI c C-END OK-ENDIAN? 0= if false exit then
   c C-ABI c C-PTR OK-PTR? 0= if false exit then
   c C-ARCH c C-RAW OK-FEAT? ;

: OK-PCOMBO? ( n -- bool )
   {: p:n :}
   p P-CON 1 = p P-FM 0= and 0= ;

\ ---- building a record from an index ----------------------------------------
: COMBO>CONTRACT ( n -- CTARGET:contract )
   {: c:n :}
   c C-ARCH N>ARCH
   c C-ABI N>ABI
   c C-END N>ENDIAN
   c C-PTR N>PTR
   c C-RAW CTARGET:FEATURE-SET
   CTARGET:CONTRACT ;

: COMBO>POLICY ( n -- CNUM:numeric-policy )
   {: p:n :}
   p P-OVF N>OVF
   p P-FLT N>FLT
   p P-CON N>CON
   p P-FM N>FM
   p P-CMP N>CMP
   CNUM:POLICY ;

\ ---- reference records -------------------------------------------------------
: A64-FEATURES ( -- CTARGET:features )
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH ;

: A64 ( -- CTARGET:contract )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64 A64-FEATURES CTARGET:CONTRACT ;

: A64-NO-FP ( -- CTARGET:contract )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64 CTARGET:F-BASE CTARGET:CONTRACT ;

: GPU ( -- CTARGET:contract )
   CTARGET-ARCH:PTX CTARGET-ABI:PTX-KERNEL CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:F-MMA CTARGET:WITH
   CTARGET:CONTRACT ;

: STRICT ( -- CNUM:numeric-policy )
   CNUM-OVERFLOW:WRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY ;

: FUSED ( -- CNUM:numeric-policy )
   CNUM-OVERFLOW:TRAP CNUM-FLOAT--MODEL:FLUSH-DENORMAL CNUM-CONTRACTION:ALLOWED
   CNUM-FAST--MATH:REASSOCIATE CNUM-COMPARE:ASSUME-ORDERED CNUM:POLICY ;

\ ---- 1. construction and field readback --------------------------------------
: READBACK ( -- )
   A64 CTARGET:ARCH@ CTARGET-ARCH:AARCH64 CTARGET-ARCH:EQ TTRUE
   A64 CTARGET:ABI@ CTARGET-ABI:AAPCS64-DARWIN CTARGET-ABI:EQ TTRUE
   A64 CTARGET:ENDIAN@ CTARGET-ENDIAN:LITTLE CTARGET-ENDIAN:EQ TTRUE
   A64 CTARGET:PTR-WIDTH@ CTARGET-PTR--WIDTH:BITS64 CTARGET-PTR--WIDTH:EQ TTRUE
   A64 CTARGET:PTR-BITS 64 T=
   A64 CTARGET:FEATURES@ A64-FEATURES CTARGET-FEATURES:EQ TTRUE
   GPU CTARGET:PTR-BITS 64 T=
   FUSED CNUM:OVERFLOW@ CNUM-OVERFLOW:TRAP CNUM-OVERFLOW:EQ TTRUE
   FUSED CNUM:FLOAT@ CNUM-FLOAT--MODEL:FLUSH-DENORMAL CNUM-FLOAT--MODEL:EQ TTRUE
   FUSED CNUM:CONTRACTION@ CNUM-CONTRACTION:ALLOWED CNUM-CONTRACTION:EQ TTRUE
   FUSED CNUM:FAST-MATH@ CNUM-FAST--MATH:REASSOCIATE CNUM-FAST--MATH:EQ TTRUE
   FUSED CNUM:COMPARE@ CNUM-COMPARE:ASSUME-ORDERED CNUM-COMPARE:EQ TTRUE
   GPU FUSED CBIND:BIND CBIND:TARGET@ GPU CTARGET:SAME? TTRUE
   GPU FUSED CBIND:BIND CBIND:POLICY@ FUSED CNUM:SAME? TTRUE ;

: FEATURE-ALGEBRA ( -- )
   CTARGET:F-BASE CTARGET:FEATURES-N CTARGET:FEATURE-SET
      CTARGET:F-BASE CTARGET-FEATURES:EQ TTRUE
   A64-FEATURES CTARGET:F-FP CTARGET:HAS? TTRUE
   A64-FEATURES CTARGET:F-MMA CTARGET:HAS? TFALSE
   CTARGET:F-BASE CTARGET:F-FP CTARGET:HAS? TFALSE
   CTARGET-ARCH:AARCH64 CTARGET:ARCH-MASK CTARGET:F-MMA CTARGET:HAS? TFALSE
   CTARGET-ARCH:PTX CTARGET:ARCH-MASK CTARGET:F-MMA CTARGET:HAS? TTRUE
   CTARGET-ARCH:PTX CTARGET:ARCH-MASK CTARGET:F-SIMD CTARGET:HAS? TFALSE ;

\ ---- 2. illegal combinations reject ------------------------------------------
: BAD-ABI ( -- )
   [: CTARGET-ARCH:AARCH64 CTARGET-ABI:PTX-KERNEL CTARGET-ENDIAN:LITTLE
      CTARGET-PTR--WIDTH:BITS64 CTARGET:F-BASE TRY-CONTRACT ;]
      E-CTGT-ABI TTHROWSQ
   [: CTARGET-ARCH:PTX CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE
      CTARGET-PTR--WIDTH:BITS64 CTARGET:F-BASE TRY-CONTRACT ;]
      E-CTGT-ABI TTHROWSQ
   [: CTARGET-ARCH:PTX CTARGET-ABI:AAPCS64-LINUX CTARGET-ENDIAN:LITTLE
      CTARGET-PTR--WIDTH:BITS64 CTARGET:F-BASE TRY-CONTRACT ;]
      E-CTGT-ABI TTHROWSQ ;

: BAD-ENDIAN ( -- )
   [: CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:BIG
      CTARGET-PTR--WIDTH:BITS64 CTARGET:F-BASE TRY-CONTRACT ;]
      E-CTGT-ENDIAN TTHROWSQ
   [: CTARGET-ARCH:PTX CTARGET-ABI:PTX-KERNEL CTARGET-ENDIAN:BIG
      CTARGET-PTR--WIDTH:BITS64 CTARGET:F-BASE TRY-CONTRACT ;]
      E-CTGT-ENDIAN TTHROWSQ ;

: BAD-PTR ( -- )
   [: CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE
      CTARGET-PTR--WIDTH:BITS32 CTARGET:F-BASE TRY-CONTRACT ;]
      E-CTGT-PTR TTHROWSQ
   [: CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-LINUX CTARGET-ENDIAN:LITTLE
      CTARGET-PTR--WIDTH:BITS32 CTARGET:F-BASE TRY-CONTRACT ;]
      E-CTGT-PTR TTHROWSQ ;

: EMBEDDED ( -- )
   [: CTARGET-ARCH:A32 CTARGET-ABI:AAPCS32 CTARGET-ENDIAN:LITTLE
      CTARGET-PTR--WIDTH:BITS64 CTARGET:F-BASE TRY-CONTRACT ;]
      E-CTGT-PTR TTHROWSQ
   [: CTARGET-ARCH:C66X CTARGET-ABI:C6000-EABI CTARGET-ENDIAN:LITTLE
      CTARGET-PTR--WIDTH:BITS64 CTARGET:F-BASE TRY-CONTRACT ;]
      E-CTGT-PTR TTHROWSQ
   [: CTARGET-ARCH:C66X CTARGET-ABI:AAPCS32 CTARGET-ENDIAN:LITTLE
      CTARGET-PTR--WIDTH:BITS32 CTARGET:F-BASE TRY-CONTRACT ;]
      E-CTGT-ABI TTHROWSQ
   [: CTARGET-ARCH:C66X CTARGET-ABI:C6000-EABI CTARGET-ENDIAN:LITTLE
      CTARGET-PTR--WIDTH:BITS32 CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH
      TRY-CONTRACT ;] E-CTGT-FEATURE TTHROWSQ
   CTARGET-ARCH:A32 CTARGET-ABI:AAPCS32 CTARGET-ENDIAN:BIG
      CTARGET-PTR--WIDTH:BITS32 CTARGET:F-BASE CTARGET:CONTRACT
      CTARGET:ENCODE drop {: a:ptr :}
   a 2 CDIGEST:SLOT@ 2 T= a 3 CDIGEST:SLOT@ 3 T=
   a 4 CDIGEST:SLOT@ 1 T= a 5 CDIGEST:SLOT@ 0 T=
   CTARGET-ARCH:THUMB2 CTARGET-ABI:AAPCS32 CTARGET-ENDIAN:LITTLE
      CTARGET-PTR--WIDTH:BITS32 CTARGET:F-BASE CTARGET:CONTRACT
      CTARGET:ENCODE drop 2 CDIGEST:SLOT@ 3 T=
   CTARGET-ARCH:C66X CTARGET-ABI:C6000-EABI CTARGET-ENDIAN:LITTLE
      CTARGET-PTR--WIDTH:BITS32 CTARGET:F-BASE CTARGET:CONTRACT
      CTARGET:ENCODE drop {: c:ptr :}
   c 2 CDIGEST:SLOT@ 4 T= c 3 CDIGEST:SLOT@ 4 T= ;

: BAD-FEATURES ( -- )
   [: $200 CTARGET:FEATURE-SET CTARGET-FEATURES:UNMAKE drop ;]
      E-CTGT-FEATURE-BITS TTHROWSQ
   [: -1 CTARGET:FEATURE-SET CTARGET-FEATURES:UNMAKE drop ;]
      E-CTGT-FEATURE-BITS TTHROWSQ
   [: CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE
      CTARGET-PTR--WIDTH:BITS64 CTARGET:F-FP TRY-CONTRACT ;]
      E-CTGT-BASE TTHROWSQ
   [: CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE
      CTARGET-PTR--WIDTH:BITS64
      CTARGET:F-BASE CTARGET:F-MMA CTARGET:WITH TRY-CONTRACT ;]
      E-CTGT-FEATURE TTHROWSQ
   [: CTARGET-ARCH:PTX CTARGET-ABI:PTX-KERNEL CTARGET-ENDIAN:LITTLE
      CTARGET-PTR--WIDTH:BITS64
      CTARGET:F-BASE CTARGET:F-SIMD CTARGET:WITH TRY-CONTRACT ;]
      E-CTGT-FEATURE TTHROWSQ ;

: BAD-POLICY ( -- )
   [: CNUM-OVERFLOW:WRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:ALLOWED
      CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED TRY-POLICY ;]
      E-CNUM-CONTRACT TTHROWSQ
   [: CNUM-OVERFLOW:TRAP CNUM-FLOAT--MODEL:FLUSH-DENORMAL CNUM-CONTRACTION:ALLOWED
      CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:ASSUME-ORDERED TRY-POLICY ;]
      E-CNUM-CONTRACT TTHROWSQ ;

: BAD-BINDING ( -- )
   [: A64-NO-FP FUSED TRY-BIND ;] E-CBIND-CONTRACT TTHROWSQ
   [: A64-NO-FP STRICT CBIND:BIND DROP-BINDING ;] 0 TTHROWSQ ;

\ A forged record - one the generated constructor assembled without passing
\ through the checked entry point - must not be able to produce an identity.
: FORGED ( -- CTARGET:contract )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:PTX-KERNEL CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64 CTARGET:F-BASE CTARGET-CONTRACT:MAKE ;

: FORGED-POLICY ( -- CNUM:numeric-policy )
   CNUM-OVERFLOW:WRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:ALLOWED
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED
   CNUM-NUMERIC--POLICY:MAKE ;

: FORGERY-REJECTS ( -- )
   [: FORGED CTARGET:VALIDATE DROP-CONTRACT ;] E-CTGT-ABI TTHROWSQ
   [: FORGED CTARGET:DIGEST CDIGEST-DIGEST:UNMAKE drop drop drop drop ;]
      E-CTGT-ABI TTHROWSQ
   [: FORGED A64 CTARGET:SAME? drop ;] E-CTGT-ABI TTHROWSQ
   [: FORGED-POLICY CNUM:DIGEST CDIGEST-DIGEST:UNMAKE drop drop drop drop ;]
      E-CNUM-CONTRACT TTHROWSQ
   [: A64 FORGED-POLICY CBIND-BINDING:MAKE CBIND:DIGEST
      CDIGEST-DIGEST:UNMAKE drop drop drop drop ;]
      E-CNUM-CONTRACT TTHROWSQ ;

\ A missing or role-swapped field never reaches runtime: the checker refuses to
\ certify the definition. -1 is accepted, 0 refused.
: STATIC-REJECTS ( -- )
   s" CTP-OK ( CTARGET:arch CTARGET:abi CTARGET:endian CTARGET:ptr-width CTARGET:features -- CTARGET:contract ) CTARGET:CONTRACT"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" CTP-SHORT ( CTARGET:arch CTARGET:abi CTARGET:endian CTARGET:ptr-width -- CTARGET:contract ) CTARGET:CONTRACT"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" CTP-SWAP ( CTARGET:abi CTARGET:arch CTARGET:endian CTARGET:ptr-width CTARGET:features -- CTARGET:contract ) CTARGET:CONTRACT"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" CTP-BARE ( CTARGET:arch CTARGET:abi CTARGET:endian CTARGET:ptr-width n -- CTARGET:contract ) CTARGET:CONTRACT"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" CTP-POK ( CNUM:overflow CNUM:float-model CNUM:contraction CNUM:fast-math CNUM:compare -- CNUM:numeric-policy ) CNUM:POLICY"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" CTP-PSHORT ( CNUM:overflow CNUM:float-model CNUM:contraction CNUM:fast-math -- CNUM:numeric-policy ) CNUM:POLICY"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" CTP-PSWAP ( CNUM:overflow CNUM:float-model CNUM:fast-math CNUM:contraction CNUM:compare -- CNUM:numeric-policy ) CNUM:POLICY"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" CTP-BOK ( CTARGET:contract CNUM:numeric-policy -- CBIND:binding ) CBIND:BIND"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" CTP-BSWAP ( CNUM:numeric-policy CTARGET:contract -- CBIND:binding ) CBIND:BIND"
      CHECK-QUIET-CANDIDATE! 0 T= ;

\ ---- 3. canonical preimage ---------------------------------------------------
\ The stable wire codes are pinned here: renumbering one silently would change
\ every stored digest, so the codes are asserted rather than assumed.
: PREIMAGE ( -- )
   GPU CTARGET:ENCODE {: base:ptr len:n :}
   len 56 T=
   base 0 CDIGEST:SLOT@ CDIGEST:TAG-TARGET T=
   base 1 CDIGEST:SLOT@ 1 T=
   base 2 CDIGEST:SLOT@ 1 T=
   base 3 CDIGEST:SLOT@ 2 T=
   base 4 CDIGEST:SLOT@ 0 T=
   base 5 CDIGEST:SLOT@ 1 T=
   base 6 CDIGEST:SLOT@ GPU CTARGET:FEATURES@ CTARGET:FEATURES-N T=
   FUSED CNUM:ENCODE {: pbase:ptr plen:n :}
   plen 56 T=
   pbase 0 CDIGEST:SLOT@ CDIGEST:TAG-NUMERIC T=
   pbase 1 CDIGEST:SLOT@ 1 T=
   pbase 2 CDIGEST:SLOT@ 1 T=
   pbase 3 CDIGEST:SLOT@ 1 T=
   pbase 4 CDIGEST:SLOT@ 1 T=
   pbase 5 CDIGEST:SLOT@ 1 T=
   pbase 6 CDIGEST:SLOT@ 2 T=
   GPU FUSED CBIND:BIND CBIND:ENCODE {: bbase:ptr blen:n :}
   blen 128 T=
   bbase 0 CDIGEST:SLOT@ CDIGEST:TAG-BINDING T=
   bbase 1 CDIGEST:SLOT@ 1 T=
   bbase 2 CDIGEST:SLOT@ CDIGEST:TAG-TARGET T=
   bbase 9 CDIGEST:SLOT@ CDIGEST:TAG-NUMERIC T= ;

\ Every address residue includes the aligned native path and the byte fallback.
\ Inspect canonical bytes independently of SLOT@ and preserve neighboring bytes.
: SLOT-AT ( n n -- )
   {: value:n offset:n :}
   32 0 ?do $A5 DGA i + c! loop
   DGA offset + {: base:ptr :}
   value base 1 CDIGEST:SLOT!
   base 1 CDIGEST:SLOT@ value T=
   8 0 ?do
      base 8 + i + c@ value i 8 * rshift $FF and T=
   loop
   base 7 + c@ $A5 T=
   base 16 + c@ $A5 T= ;

: SLOT-ROUNDTRIP ( -- )
   CDIGEST:SLOT-BYTES 8 T=
   -1 DGA 0 CDIGEST:SLOT!
   DGA 0 CDIGEST:SLOT@ -1 T=
   $0123456789ABCDEF DGA 3 CDIGEST:SLOT!
   DGA 3 CDIGEST:SLOT@ $0123456789ABCDEF T=
   0 DGA 3 CDIGEST:SLOT!
   DGA 3 CDIGEST:SLOT@ 0 T=
   8 0 ?do
      0 i SLOT-AT
      -1 i SLOT-AT
      $0123456789ABCDEF i SLOT-AT
      $8000000000000000 i SLOT-AT
   loop ;

\ ---- 4. equal records digest identically -------------------------------------
: STABLE ( -- )
   A64 A64 CTARGET:SAME? TTRUE
   A64 CTARGET:DIGEST A64 CTARGET:DIGEST CDIGEST-DIGEST:EQ TTRUE
   GPU CTARGET:DIGEST GPU CTARGET:DIGEST CDIGEST-DIGEST:EQ TTRUE
   A64 CTARGET:DIGEST GPU CTARGET:DIGEST CDIGEST-DIGEST:EQ TFALSE
   STRICT STRICT CNUM:SAME? TTRUE
   STRICT CNUM:DIGEST STRICT CNUM:DIGEST CDIGEST-DIGEST:EQ TTRUE
   STRICT CNUM:DIGEST FUSED CNUM:DIGEST CDIGEST-DIGEST:EQ TFALSE
   GPU FUSED CBIND:BIND GPU FUSED CBIND:BIND CBIND:SAME? TTRUE
   GPU FUSED CBIND:BIND CBIND:DIGEST
   GPU FUSED CBIND:BIND CBIND:DIGEST CDIGEST-DIGEST:EQ TTRUE
   GPU FUSED CBIND:BIND CBIND:DIGEST
   GPU STRICT CBIND:BIND CBIND:DIGEST CDIGEST-DIGEST:EQ TFALSE
   GPU FUSED CBIND:BIND CBIND:DIGEST
   A64 FUSED CBIND:BIND CBIND:DIGEST CDIGEST-DIGEST:EQ TFALSE ;

\ A record kind can never share a digest with another kind: slot 0 separates the
\ domains before a single field is written.
: DOMAIN-SEPARATION ( -- )
   A64 CTARGET:DIGEST STRICT CNUM:DIGEST CDIGEST-DIGEST:EQ TFALSE
   A64 CTARGET:DIGEST A64 STRICT CBIND:BIND CBIND:DIGEST
      CDIGEST-DIGEST:EQ TFALSE
   STRICT CNUM:DIGEST A64 STRICT CBIND:BIND CBIND:DIGEST
      CDIGEST-DIGEST:EQ TFALSE ;

\ ---- 5. every field changes identity, over the whole legal domain -------------
: COLLECT-CONTRACTS ( -- )
   0 N !
   COMBO# 0 ?do
      i OK-COMBO? if
         i N @ IX!
         N @ 1+ N !
      then
   loop
   N @ LEGAL-CONTRACTS T= ;

: DIGEST-CONTRACTS ( -- )
   N @ 0 ?do
      i IX@ COMBO>CONTRACT CTARGET:DIGEST CDIGEST-DIGEST:UNMAKE i DG!
   loop ;

: PAIR-CONTRACTS ( -- )
   N @ dup * 0 ?do
      i N @ / {: x:n :}
      i N @ mod {: y:n :}
      x IX@ COMBO>CONTRACT y IX@ COMBO>CONTRACT CTARGET:SAME? {: same:bool :}
      x y DG= same TBOOL=
      same x y = TBOOL=
   loop ;

: COLLECT-POLICIES ( -- )
   0 N !
   PCOMBO# 0 ?do
      i OK-PCOMBO? if
         i N @ IX!
         N @ 1+ N !
      then
   loop
   N @ LEGAL-POLICIES T= ;

: DIGEST-POLICIES ( -- )
   N @ 0 ?do
      i IX@ COMBO>POLICY CNUM:DIGEST CDIGEST-DIGEST:UNMAKE i DG!
   loop ;

: PAIR-POLICIES ( -- )
   N @ dup * 0 ?do
      i N @ / {: x:n :}
      i N @ mod {: y:n :}
      x IX@ COMBO>POLICY y IX@ COMBO>POLICY CNUM:SAME? {: same:bool :}
      x y DG= same TBOOL=
      same x y = TBOOL=
   loop ;

\ Every rejected policy index is rejected for the one stated reason, over the
\ entire product of the five families.
\ A quotation carries no locals, so the swept index travels through a cell.
variable REJ-IX

: REJ-BODY ( -- )
   REJ-IX @ {: p:n :}
   p P-OVF N>OVF p P-FLT N>FLT p P-CON N>CON
   p P-FM N>FM p P-CMP N>CMP TRY-POLICY ;

: SWEEP-POLICY-REJECTS ( -- )
   PCOMBO# 0 ?do
      i OK-PCOMBO? 0= if
         i REJ-IX !
         [: REJ-BODY ;] E-CNUM-CONTRACT TTHROWSQ
      then
   loop ;

\ The binding digest separates every target when the policy is held fixed, and
\ every policy when the target is held fixed. With the preimage being the two
\ component preimages concatenated, those two sweeps cover both halves.
: BIND-OVER-TARGETS ( -- )
   COLLECT-CONTRACTS
   N @ 0 ?do
      i IX@ COMBO>CONTRACT STRICT CBIND:BIND CBIND:DIGEST
      CDIGEST-DIGEST:UNMAKE i DG!
   loop
   N @ dup * 0 ?do
      i N @ / {: x:n :}
      i N @ mod {: y:n :}
      x y DG= x y = TBOOL=
   loop ;

: BIND-OVER-POLICIES ( -- )
   COLLECT-POLICIES
   N @ 0 ?do
      GPU i IX@ COMBO>POLICY CBIND:BIND CBIND:DIGEST
      CDIGEST-DIGEST:UNMAKE i DG!
   loop
   N @ dup * 0 ?do
      i N @ / {: x:n :}
      i N @ mod {: y:n :}
      GPU x IX@ COMBO>POLICY CBIND:BIND
      GPU y IX@ COMBO>POLICY CBIND:BIND CBIND:SAME? {: same:bool :}
      x y DG= same TBOOL=
      same x y = TBOOL=
   loop ;

public

: RUN ( -- )
   T-RESET
   READBACK
   FEATURE-ALGEBRA
   BAD-ABI
   BAD-ENDIAN
   BAD-PTR
   EMBEDDED
   BAD-FEATURES
   BAD-POLICY
   BAD-BINDING
   FORGERY-REJECTS
   STATIC-REJECTS
   PREIMAGE
   SLOT-ROUNDTRIP
   STABLE
   DOMAIN-SEPARATION
   COLLECT-CONTRACTS
   DIGEST-CONTRACTS
   PAIR-CONTRACTS
   COLLECT-POLICIES
   DIGEST-POLICIES
   PAIR-POLICIES
   SWEEP-POLICY-REJECTS
   BIND-OVER-TARGETS
   BIND-OVER-POLICIES
   T-REPORT ;

;package

CTPOL-TEST:RUN
