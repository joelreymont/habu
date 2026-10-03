\ Legacy CTARGET schema-1 identities. The native rows are the contracts built
\ by NABI:BINDING on AArch64 Darwin/Linux and X64ABI:BINDING on SysV AMD64.
\ The PTX row is the sample contract in target-policy.f. These are wire pins,
\ independent of which native backend this engine can execute.

require lib/test.f
require src/compiler/target.f

package CTARGET-GOLDEN-TEST
private

create DIGEST-BYTES 32 allot

: HEX-DIGIT ( n -- n )
   {: c:n :}
   c 48 >= c 57 <= and if c 48 - exit then
   c 97 >= c 102 <= and if c 97 - 10 + exit then
   s" target golden: invalid hex" 76 die ;

: HEX-BYTE ( ptr u8 n -- n )
   {: a:ptr i:n :}
   a i 2 * + c@ HEX-DIGIT 4 lshift
   a i 2 * 1+ + c@ HEX-DIGIT or ;

: BYTES=HEX? ( ptr u8 n ptr u8 n -- bool )
   {: actual:ptr size:n expected:ptr chars:n :}
   size 2 * chars <> if false exit then
   size 0 ?do
      actual i + c@ expected i HEX-BYTE <> if false unloop exit then
   loop
   true ;

: PIN ( CTARGET:contract ptr u8 n ptr u8 n -- )
   {: target:CTARGET:contract pre:ptr prelen:n sha:ptr shalen:n :}
   target CTARGET:ENCODE pre prelen BYTES=HEX? TTRUE
   target CTARGET:DIGEST CDIGEST-DIGEST:UNMAKE
   {: w0:n w1:n w2:n w3:n :}
   w0 DIGEST-BYTES 0 CDIGEST:SLOT!
   w1 DIGEST-BYTES 1 CDIGEST:SLOT!
   w2 DIGEST-BYTES 2 CDIGEST:SLOT!
   w3 DIGEST-BYTES 3 CDIGEST:SLOT!
   DIGEST-BYTES 32 sha shalen BYTES=HEX? TTRUE ;

: BASE-FP ( -- CTARGET:features )
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH ;

: DARWIN ( -- CTARGET:contract )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64 BASE-FP CTARGET:CONTRACT ;

: LINUX ( -- CTARGET:contract )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-LINUX CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64 BASE-FP CTARGET:CONTRACT ;

: SYSV ( -- CTARGET:contract )
   CTARGET-ARCH:X86-64 CTARGET-ABI:SYSV-AMD64 CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64 BASE-FP CTARGET:CONTRACT ;

: SAMPLE ( -- CTARGET:contract )
   CTARGET-ARCH:PTX CTARGET-ABI:PTX-KERNEL CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   BASE-FP CTARGET:F-MMA CTARGET:WITH CTARGET:CONTRACT ;

public

: RUN ( -- )
   T-RESET
   s" aarch64-darwin schema-1 preimage and SHA-256" T-LABEL
   DARWIN
   s" 0100000000000000010000000000000000000000000000000000000000000000000000000000000001000000000000000300000000000000"
   s" 8a24904ea8ff46daa809c2359efdf0aa6969d34cb02588c131946549c3bc658a" PIN
   s" aarch64-linux schema-1 preimage and SHA-256" T-LABEL
   LINUX
   s" 0100000000000000010000000000000000000000000000000100000000000000000000000000000001000000000000000300000000000000"
   s" 037df51cafab998ffa35d2326bae50110bab5794a342e389e5d0b3e537d9aa62" PIN
   s" sysv-amd64 schema-1 preimage and SHA-256" T-LABEL
   SYSV
   s" 0100000000000000010000000000000005000000000000000500000000000000000000000000000001000000000000000300000000000000"
   s" f8c1bf94726f5ecd6b512042661fbee4bab60a0df2812badcfda6ff51fba5404" PIN
   s" PTX sample schema-1 preimage and SHA-256" T-LABEL
   SAMPLE
   s" 0100000000000000010000000000000001000000000000000200000000000000000000000000000001000000000000008300000000000000"
   s" 4bfe7331794fe8cd5ba6cfab63621c8ea04900d1df46b45b3321a406774d3401" PIN
   T-REPORT ;

;package

CTARGET-GOLDEN-TEST:RUN
