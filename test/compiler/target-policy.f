\ target-policy.f - acceptance suite for the compiler target/policy binding.
\
\ Covers src/compiler/digest.f, src/compiler/target.f,
\ src/compiler/numeric-policy.f and src/compiler/binding.f through their public
\ production words. Two properties are owed by the dot, and each is proved over
\ the whole declarable domain rather than by example:
\
\ 1. MISSING OR ILLEGAL COMBINATIONS REJECT. A missing or role-swapped field is a
\    CHECKER refusal - proved with candidate definitions that the checker declines
\    to certify - so it never reaches runtime at all. An incoherent combination of
\    present fields is a named throw, one case per rule, with the exact code.
\
\ 2. Every coherent contract and numerical policy in the finite domain
\    constructs, while the binding and structural readers agree with its parts.
\
\ The enumeration uses independent index arithmetic and legality rules, so a
\ widened rule changes the asserted domain size.

require src/lib/test.f
require src/lib/string.f
require src/compiler/digest.f
require src/compiler/binding.f

package CTPOL-TEST
private

\ ---- enumeration shape -------------------------------------------------------
\ Target contracts: architecture x ABI x byte order x pointer width x every
\ feature mask over the ten defined bits. A single flat index keeps the sweep to
\ one loop; the projections below take it apart.
7 constant ARCH#
7 constant ABI#
2 constant END#
2 constant PTR#
$400 constant RAW#
ARCH# ABI# * END# * PTR# * RAW# * constant COMBO#
\ All ten raw bits are enumerated independently of HAS? implication. The seven
\ architecture and ABI rows admit 1036 coherent contracts.
1036 constant LEGAL-CONTRACTS

\ Numerical policies: the full product of the five families.
2 constant OVF#
2 constant FLT#
2 constant CON#
3 constant FM#
3 constant CMP#
OVF# FLT# * CON# * FM# * CMP# * constant PCOMBO#
60 constant LEGAL-POLICIES

variable N
96 BUFFER: SLOT-BUF

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
   dup 4 = if drop CTARGET-ARCH:C66X exit then
   5 = if CTARGET-ARCH:X86-64 exit then
   CTARGET-ARCH:WASM ;

: N>ABI ( n -- CTARGET:abi )
   dup 0= if drop CTARGET-ABI:AAPCS64-DARWIN exit then
   dup 1 = if drop CTARGET-ABI:AAPCS64-LINUX exit then
   dup 2 = if drop CTARGET-ABI:PTX-KERNEL exit then
   dup 3 = if drop CTARGET-ABI:AAPCS32 exit then
   dup 4 = if drop CTARGET-ABI:C6000-EABI exit then
   5 = if CTARGET-ABI:SYSV-AMD64 exit then
   CTARGET-ABI:HABU-WASM-CELL64-V1 ;

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
   arch 6 = if abi 6 = exit then
   abi 3 = ;

: OK-ENDIAN? ( n n -- bool )
   {: abi:n endian:n :}
   endian 0= if true exit then
   abi 5 = abi 6 = or if false exit then
   abi 1 = abi 3 >= or ;

: OK-PTR? ( n n -- bool )
   {: abi:n ptr:n :}
   abi 5 = if ptr 1 = exit then
   abi 6 = if true exit then
   abi 3 >= if ptr 0= exit then
   ptr 1 = if true exit then
   abi 2 = ;

\ These masks are the suite's independent legal-domain description. The raw
\ F-FP and scalar-FP bits remain separate even though HAS? implies scalar.
: MASK-FOR ( n -- n )
   case
      0 of $25F endof
      1 of $3FB endof
      2 of $25F endof
      3 of $25F endof
      4 of $205 endof
      5 of $25F endof
      6 of $201 endof
      0 swap
   endcase ;

: OK-FEAT? ( n n -- bool )
   {: arch:n raw:n :}
   raw $001 and 0= if false exit then
   raw arch MASK-FOR invert and 0= ;

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
   CTARGET-ARCH:WASM CTARGET-ABI:HABU-WASM-CELL64-V1 CTARGET-ENDIAN:LITTLE
      CTARGET-PTR--WIDTH:BITS32 CTARGET:F-BASE CTARGET:CONTRACT CTARGET:PTR-BITS 32 T=
   CTARGET-ARCH:WASM CTARGET-ABI:HABU-WASM-CELL64-V1 CTARGET-ENDIAN:LITTLE
      CTARGET-PTR--WIDTH:BITS64 CTARGET:F-BASE CTARGET:CONTRACT CTARGET:PTR-BITS 64 T=
   FUSED CNUM:OVERFLOW@ CNUM-OVERFLOW:TRAP CNUM-OVERFLOW:EQ TTRUE
   FUSED CNUM:FLOAT@ CNUM-FLOAT--MODEL:FLUSH-DENORMAL CNUM-FLOAT--MODEL:EQ TTRUE
   FUSED CNUM:CONTRACTION@ CNUM-CONTRACTION:ALLOWED CNUM-CONTRACTION:EQ TTRUE
   FUSED CNUM:FAST-MATH@ CNUM-FAST--MATH:REASSOCIATE CNUM-FAST--MATH:EQ TTRUE
   FUSED CNUM:COMPARE@ CNUM-COMPARE:ASSUME-ORDERED CNUM-COMPARE:EQ TTRUE
   GPU FUSED CBIND:BIND CBIND:TARGET@ GPU CTARGET:SAME? TTRUE
   GPU FUSED CBIND:BIND CBIND:POLICY@ FUSED CNUM:SAME? TTRUE ;

: FEATURE-ALGEBRA ( -- )
   CTARGET:F-BASE CTARGET:F-SCALAR-FP CTARGET:WITH
      CTARGET:F-FP CTARGET:HAS? TFALSE
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH
      CTARGET:F-SCALAR-FP CTARGET:HAS? TTRUE
   CTARGET:F-FP CTARGET:FEATURES-N $002 T=
   CTARGET:F-SCALAR-FP CTARGET:FEATURES-N $200 T=
   CTARGET-ARCH:WASM CTARGET:ARCH-MASK CTARGET:FEATURES-N $201 T=
   CTARGET-ARCH:C66X CTARGET:ARCH-MASK CTARGET:F-SCALAR-FP CTARGET:HAS? TTRUE
   CTARGET-ARCH:C66X CTARGET:ARCH-MASK CTARGET:F-FP CTARGET:HAS? TFALSE
   CTARGET-ARCH:PTX CTARGET:ARCH-MASK CTARGET:F-SCALAR-FP CTARGET:HAS? TTRUE
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
   [: CTARGET-ARCH:AARCH64 CTARGET-ABI:HABU-WASM-CELL64-V1 CTARGET-ENDIAN:LITTLE
      CTARGET-PTR--WIDTH:BITS64 CTARGET:F-BASE TRY-CONTRACT ;]
      E-CTGT-ABI TTHROWSQ
   [: CTARGET-ARCH:WASM CTARGET-ABI:SYSV-AMD64 CTARGET-ENDIAN:LITTLE
      CTARGET-PTR--WIDTH:BITS32 CTARGET:F-BASE TRY-CONTRACT ;]
      E-CTGT-ABI TTHROWSQ
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
   [: CTARGET-ARCH:WASM CTARGET-ABI:HABU-WASM-CELL64-V1 CTARGET-ENDIAN:BIG
      CTARGET-PTR--WIDTH:BITS32 CTARGET:F-BASE TRY-CONTRACT ;]
      E-CTGT-ENDIAN TTHROWSQ
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
      TRY-CONTRACT ;] E-CTGT-FEATURE TTHROWSQ ;

: BAD-FEATURES ( -- )
   [: $400 CTARGET:FEATURE-SET CTARGET-FEATURES:UNMAKE drop ;]
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
   [: CTARGET-ARCH:WASM CTARGET-ABI:HABU-WASM-CELL64-V1 CTARGET-ENDIAN:LITTLE
      CTARGET-PTR--WIDTH:BITS32 CTARGET:F-BASE CTARGET:F-SCALAR-FP CTARGET:WITH
      CTARGET:CONTRACT FUSED TRY-BIND ;] E-CBIND-CONTRACT TTHROWSQ
   [: A64-NO-FP STRICT CBIND:BIND DROP-BINDING ;] 0 TTHROWSQ ;

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

\ Every address residue includes the aligned native path and the byte fallback.
\ Inspect canonical bytes independently of SLOT@ and preserve neighboring bytes.
: SLOT-AT ( n n -- )
   {: value:n offset:n :}
   32 0 ?do $A5 SLOT-BUF i + c! loop
   SLOT-BUF offset + {: base:ptr :}
   value base 1 CDIGEST:SLOT!
   base 1 CDIGEST:SLOT@ value T=
   8 0 ?do
      base 8 + i + c@ value i 8 * rshift $FF and T=
   loop
   base 7 + c@ $A5 T=
   base 16 + c@ $A5 T= ;

: SLOT-ROUNDTRIP ( -- )
   CDIGEST:SLOT-BYTES 8 T=
   -1 SLOT-BUF 0 CDIGEST:SLOT!
   SLOT-BUF 0 CDIGEST:SLOT@ -1 T=
   $0123456789ABCDEF SLOT-BUF 3 CDIGEST:SLOT!
   SLOT-BUF 3 CDIGEST:SLOT@ $0123456789ABCDEF T=
   0 SLOT-BUF 3 CDIGEST:SLOT!
   SLOT-BUF 3 CDIGEST:SLOT@ 0 T=
   8 0 ?do
      0 i SLOT-AT
      -1 i SLOT-AT
      $0123456789ABCDEF i SLOT-AT
      $8000000000000000 i SLOT-AT
   loop ;

\ ---- legal-domain construction --------------------------------------------
: VALID-CONTRACTS ( -- )
   COMBO# 200704 T=
   0 N !
   COMBO# 0 ?do
      i OK-COMBO? if
         i COMBO>CONTRACT DROP-CONTRACT
         N @ 1+ N !
      then
   loop
   N @ LEGAL-CONTRACTS T= ;

: VALID-POLICIES ( -- )
   0 N !
   PCOMBO# 0 ?do
      i OK-PCOMBO? if
         i COMBO>POLICY DROP-POLICY
         N @ 1+ N !
      then
   loop
   N @ LEGAL-POLICIES T= ;

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
   STATIC-REJECTS
   SLOT-ROUNDTRIP
   VALID-CONTRACTS
   VALID-POLICIES
   SWEEP-POLICY-REJECTS
   T-REPORT ;

;package

CTPOL-TEST:RUN
