\ asm-src-test.f - unchecked source ARM64 encoder regression.
\ Run: bin/hb --load src/lib/error.f src/lib/string.f tools/asm-src-test.f

require src/lib/error.f
require src/lib/string.f
require src/lib/test/assert.f

\ `require`, never `included`: an unrecorded load is read again and redefined by
\ a later require of the same file.
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/arch/arm64/mnem.f

using A64ASM
using A64ICODE

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
   14 15 ENC-LDARW $88DFFDEE T=
   11 5 ENC-STLRW $889FFCAB T=
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
;using
