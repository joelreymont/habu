\ asm-src-test.f - unchecked source ARM64 encoder regression.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f tools/asm-src-test.f

: AST-WORD? ( ptr u8 n -- bool )
   XREF-FIND 0= 0= ;

: AST-LOAD-ASM ( -- )
   s" ENC-ADD" AST-WORD? 0= if s" src/arch/arm64/asm.f" included then
   s" LIT64," AST-WORD? 0= if s" src/arch/arm64/icode.f" included then
   s" ADD," AST-WORD? 0= if s" src/arch/arm64/mnem.f" included then ;

AST-LOAD-ASM

s" MOVZHW" s" n n n -- n" TRUST
s" ENC-ADD" s" n n n -- n" TRUST
s" ENC-LDR" s" n n n -- n" TRUST
s" ENC-BLR" s" n -- n" TRUST
s" >LIMM" s" n -- n" TRUST
s" ENC-ANDI" s" n n n -- n" TRUST
s" CW@" s" n -- ptr u8" TRUST
s" CODE-BYTE+" s" ptr u8 n -- ptr u8" TRUST
s" ARESET" s" --" TRUST
s" ADD," s" n n n --" TRUST
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
