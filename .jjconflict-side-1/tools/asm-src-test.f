\ asm-src-test.f - unchecked source ARM64 encoder regression.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f tools/asm-src-test.f

require lib/errors.f
require lib/string.f
require lib/test.f

\ Probed per file and loaded with `included`, which never records the load, so a
\ later `require src/arch/arm64/asm.f` in the same process read the file again
\ and redefined it. `require` records what it loads and skips what is recorded.
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/arch/arm64/mnem.f

\ The ARM64 encoders are package A64ASM's public surface (src/arch/arm64/asm.f);
\ the trusted rows below name them the way the dictionary now records them.
using A64ASM

\ Raw ARM64 fixture effects retire with habu-builder-trust-rows-c5d41af6.
\ MOVZHW and ENC-ADD/LDR/LDAR/BLR expose their instruction encoders.
s" A64ASM:MOVZHW" s" n n n -- n" TRUST
s" A64ASM:ENC-ADD" s" n n n -- n" TRUST
s" A64ASM:ENC-LDR" s" n n n -- n" TRUST
s" A64ASM:ENC-LDAR" s" n n -- n" TRUST
s" A64ASM:ENC-BLR" s" n -- n" TRUST
\ >LIMM exposes immediate layout; ENC-ANDI the logical-immediate encoder.
s" A64ASM:>LIMM" s" n -- n" TRUST
s" A64ASM:ENC-ANDI" s" n n n -- n" TRUST
\ CW@ exposes code bytes; CODE-BYTE+ preserves byte-pointer arithmetic.
s" CW@" s" n -- ptr u8" TRUST
s" CODE-BYTE+" s" ptr u8 n -- ptr u8" TRUST
\ ARESET resets the buffer; ADD,/LDAR, emit fixture instructions.
s" ARESET" s" --" TRUST
s" ADD," s" n n n --" TRUST
s" LDAR," s" n n --" TRUST
\ ASM-LEN reads emitted length; LIT64, emits the literal fixture.
s" ASM-LEN" s" -- n" TRUST
s" LIT64," s" n n --" TRUST

$2000000000000000 constant AST-DNAME-EXT

: AST-U32@ ( n -- n )
   CW@ dup c@
   swap 1 CODE-BYTE+ dup c@ $8 lshift
   swap 1 CODE-BYTE+ dup c@ $10 lshift
   swap 1 CODE-BYTE+ c@ $18 lshift
   or or or ;

: AST-TEST-CORE ( -- )
   1 2 3 ENC-ADD $8B030041 T=
   1 2 3 MOVZHW $D2E00041 T=
   1 2 $18 ENC-LDR $F9400C41 T=
   14 15 ENC-LDAR $C8DFFDEE T=
   11 5 ENC-STLR $C89FFCAB T=
   7 ENC-BLR $D63F00E0 T= ;

: AST-TEST-LIMM ( -- )
   1 >LIMM $1000 T=
   $FF >LIMM $1007 T=
   AST-DNAME-EXT >LIMM $10C0 T=
   14 14 AST-DNAME-EXT >LIMM ENC-ANDI $924301CE T= ;

: AST-TEST-ICODE ( -- )
   ARESET
   1 2 3 ADD,
   0 AST-U32@ $8B030041 T=
   ARESET
   14 15 LDAR,
   0 AST-U32@ $C8DFFDEE T=
   ARESET
   11 5 STLR,
   0 AST-U32@ $C89FFCAB T=
   ARESET
   9 AST-DNAME-EXT LIT64,
   ASM-LEN $4 T=
   0 AST-U32@ $D2E40009 T= ;

: AST-MAIN ( -- )
   T-RESET
   AST-TEST-CORE
   AST-TEST-LIMM
   AST-TEST-ICODE
   T-REPORT
   s" asm-src-test: ok" type cr ;

AST-MAIN

;using
