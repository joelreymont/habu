\ t-sh-asm2.fs — the engine-ISA encoder extension, golden against habu. Three parts:
\ (1) LIT64, minimal movz/movn+movk sequences must match habu's REAL assembler
\     (src/cg LIT-Z/LIT-N) word for word — stage2 byte-identity hinges on this;
\ (2) the new encoders (divides/shifts/logical-imm/blr/cache/adr) match habu's
\     encode-pass constants; (3) ADR, forward-ref patching + DCQ, data cells.
\ Run: gforth test/t-sh-asm2.fs -e bye
require ../bootstrap/cg/icode.fs
require sh-driver.fs
create VALS 0 , 1 , $FFFF , $10000 , $FFFFFFFF , $100000000 , -1 , -2 ,
   $FFFFFFFFFFFF0000 , $1234000056780000 , $FFFF00000000FFFF , 42 , -42 ,
   $7FFFFFFFFFFFFFFF , $8000000000000000 ,
15 constant NV
create RBUF 8192 allot
create EB 65536 allot  variable EL
: E+ ( a u -- )  bounds ?do i c@ EB EL @ + c! 1 EL +! loop ;
: N+ ( n -- )  0 <# 10 hold #s #> E+ ;          \ "decimal\n" (u32s, always positive)
: w@ ( i -- u32 )  4 * RBUF +  dup c@  over 1+ c@ 8 lshift or
   over 2 + c@ 16 lshift or  swap 3 + c@ 24 lshift or ;
: U+ ( u -- )  0 <# #s #> +B s"  " +B ;          \ unsigned decimal into CBUF (wraps back)
\ part 1: LIT64 sequences — habu reference via the real assembler
: LITREF ( -- )  0 EL !  ICODE-RESET
   NV 0 ?do  9 VALS i cells + @ LIT64,  loop
   RBUF ASSEMBLE 4 /  0 ?do i w@ N+ loop ;
: LITGEN ( -- a u )
   0 CL !
   s" src/arch/arm64/asm.f" +F  s" src/arch/arm64/icode.f" +F  s" src/core/util.f" +F
   s" : GO ASM-INIT " +B
   NV 0 ?do  s" 9 " +B  VALS i cells + @ U+  s" LIT64, " +B  loop
   s" 0 BEGIN dup CP @ < WHILE dup CW@ RD32 . 1 + REPEAT drop ; GO" +B
   CBUF CL @ NF-RUN  NFOUT 2@ ;
LITREF
T{ LITGEN  EB EL @ compare 0= -> true }T
\ part 2: new encoders — formula reference (habu's encode-pass constants)
: ENCREF ( -- )  0 EL !
   $9AC00C00 5 or 1 5 lshift or 2 16 lshift or N+   \ sdiv x5,x1,x2
   $9AC00800 5 or 1 5 lshift or 2 16 lshift or N+   \ udiv
   $9AC02000 5 or 1 5 lshift or 2 16 lshift or N+   \ lslv
   $9AC02400 5 or 1 5 lshift or 2 16 lshift or N+   \ lsrv
   $9AC02800 5 or 1 5 lshift or 2 16 lshift or N+   \ asrv
   $9340FC00 5 or 1 5 lshift or 2 16 lshift or N+   \ asr x5,x1,#2
   $92000000 5 or 1 5 lshift or $1234 10 lshift or N+   \ and-imm (nis=$1234)
   $B2000000 5 or 1 5 lshift or $1234 10 lshift or N+   \ orr-imm
   $D2000000 5 or 1 5 lshift or $1234 10 lshift or N+   \ eor-imm
   $D63F0000 7 5 lshift or N+                       \ blr x7
   $D61F0000 7 5 lshift or N+                       \ br x7
   $D4200000 N+  $D503201F N+                       \ brk, nop
   $D50B7520 3 or N+  $D50B7B20 3 or N+             \ ic/dc cvau, x3
   $D5033B9F N+  $D5033FDF N+                       \ dsb ish, isb
   $10000000 5 or -8 3 and 29 lshift or -8 4 / $7FFFF and 5 lshift or N+ ;  \ adr x5,.-8
: ENCGEN ( -- a u )
   0 CL !
   s" src/arch/arm64/asm.f" +F
   s" : GO 5 1 2 ENC-SDIV . 5 1 2 ENC-UDIV . 5 1 2 ENC-LSLV . 5 1 2 ENC-LSRV . " +B
   s" 5 1 2 ENC-ASRV . 5 1 2 ENC-ASRI . 5 1 $1234 ENC-ANDI . 5 1 $1234 ENC-ORRI . " +B
   s" 5 1 $1234 ENC-EORI . 7 ENC-BLR . 7 ENC-BR . ENC-BRK . ENC-NOP . " +B
   s" 3 ENC-ICIVAU . 3 ENC-DCCVAU . ENC-DSB-ISH . ENC-ISB . 5 -8 ENC-ADR . ; GO" +B
   CBUF CL @ NF-RUN  NFOUT 2@ ;
ENCREF
T{ ENCGEN  EB EL @ compare 0= -> true }T
\ part 3: ADR, forward patch + DCQ, cells via the standalone assembler
: P3REF ( -- )  0 EL !
   $10000000 5 or 4 3 and 29 lshift or 4 4 / $7FFFF and 5 lshift or N+  \ adr x5,.+4
   $55667788 N+  $11223344 N+ ;                                          \ dcq LE halves
: P3GEN ( -- a u )
   0 CL !
   s" src/arch/arm64/asm.f" +F  s" src/arch/arm64/icode.f" +F  s" src/core/util.f" +F
   s" : GO ASM-INIT LBL dup 5 swap ADR, LBL, $1122334455667788 DCQ, " +B
   s" 0 BEGIN dup CP @ < WHILE dup CW@ RD32 . 1 + REPEAT drop ; GO" +B
   CBUF CL @ NF-RUN  NFOUT 2@ ;
P3REF
T{ P3GEN  EB EL @ compare 0= -> true }T
