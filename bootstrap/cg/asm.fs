\ asm.fs — ARM64 encoders: ICode records -> machine code (little-endian u32s).
\ Pure bit-field recipes (ported from habu src/ir/arm64.zig + src/jit/
\ stencils.zig). Two passes: PASS1 sizes records and binds labels; PASS2
\ encodes via a table indexed by op tag. Branches and immediates are
\ range-checked — out of range throws, never wraps. The pure register-form
\ recipes are dogfooded: written as TYPED habu and verified by habu's own checker;
\ the return-stack/memory/table machinery stays TRUSTED (unchecked).

require ../habu.fs        \ the checker, so we can check our own recipes
CHECKING-ON? off            \ metaprogramming (IR mutators, tables, memory) is unchecked
require icode.fs

s" cg: branch out of range"    exception constant E-BRANCH-RANGE
s" cg: undefined label"        exception constant E-UNDEF-LBL
s" cg: immediate out of range" exception constant E-IMM-RANGE
s" cg: missing encoder"        exception constant E-NO-ENC

variable ABUF  variable WPOS

: EMITW ( u32 -- )  ABUF @ WPOS @ 4 * + l!  1 WPOS +! ;

\ --- range checks ---
: ?REL26 ( d -- d )  dup -33554432 33554432 within 0= if E-BRANCH-RANGE throw then ;

: ?REL19 ( d -- d )  dup -262144 262144 within 0= if E-BRANCH-RANGE throw then ;

: ?SC8   ( off -- imm12 )
   dup 0 32761 within 0=  over 7 and 0<> or  if E-IMM-RANGE throw then  3 rshift ;

: ?IMM12 ( off -- imm12 )  dup 0 4096 within 0= if E-IMM-RANGE throw then ;

: ?IMM9  ( off -- u9 )  dup -256 256 within 0= if E-IMM-RANGE throw then  $1FF and ;

: ?SC7   ( off -- u7 )
   dup -512 505 within 0=  over 7 and 0<> or  if E-IMM-RANGE throw then  8 / $7F and ;

\ --- constant synthesis ENC-LIT: minimal MOVZ/MOVN + MOVK chain ---
: CHUNK ( x n -- u16 )  4 lshift rshift $FFFF and ;

: NZ-CHUNKS ( x -- n )  0 4 0 ?do  over i CHUNK 0<> -  loop nip ;

: NF-CHUNKS ( x -- n )  0 4 0 ?do  over i CHUNK $FFFF <> -  loop nip ;

: LIT-LEN ( x -- n )  dup NZ-CHUNKS 1 max  swap NF-CHUNKS 1 max  min ;

: MOVZHW ( rd imm16 hw -- u32 )  21 lshift swap 5 lshift or or $D2800000 or ;

: MOVKHW ( rd imm16 hw -- u32 )  21 lshift swap 5 lshift or or $F2800000 or ;

: MOVNHW ( rd imm16 hw -- u32 )  21 lshift swap 5 lshift or or $92800000 or ;

: 1ST-NZ ( x -- i )  4 0 ?do  dup i CHUNK if drop i unloop exit then  loop drop 0 ;

: 1ST-NF ( x -- i )  4 0 ?do  dup i CHUNK $FFFF <> if drop i unloop exit then  loop drop 0 ;

: LIT-Z ( rd x -- )  {: RD x | f :}  x 1ST-NZ to f
   RD x f CHUNK f MOVZHW EMITW
   4 0 ?do  i f <> if x i CHUNK ?dup if RD swap i MOVKHW EMITW then then  loop ;

: LIT-N ( rd x -- )  {: RD x | f :}  x 1ST-NF to f
   RD x f CHUNK invert $FFFF and f MOVNHW EMITW
   4 0 ?do  i f <> if x i CHUNK dup $FFFF <> if RD swap i MOVKHW EMITW else drop then then  loop ;

: MOVN-SHORTER? ( x -- f )  dup NF-CHUNKS 1 max  swap NZ-CHUNKS 1 max  < ;

\ --- label / branch helpers ---
: LBL@ ( lbl -- w )  cells LBLPOS + @  dup 0< if E-UNDEF-LBL throw then ;

: BDELTA ( i -- d )  IC-A LBL@ WPOS @ - ;   \ words, branch-relative

\ --- dogfood: register-form recipes as TYPED habu, checked by habu itself ---
\ Chart the trusted leaves the checked recipes call (they do memory/pointer work).
: CHART-EFF ( eff-a eff-u na nu -- )  2>r ARENA-RESET PARSE-SIG 2r> CHART ;
s" R i64 -- R i64" s" IC-A"  CHART-EFF    s" R i64 -- R i64" s" IC-B" CHART-EFF
s" R i64 -- R i64" s" IC-C"  CHART-EFF    s" R i64 -- R i64" s" IC-D" CHART-EFF
s" R i64 -- R"     s" EMITW" CHART-EFF
CHECKING-ON? on

: RRR      ( R i64 i64 -- R i64 )  swap >r  r@ IC-A or  r@ IC-B 5 LSHIFT or  r@ IC-C 16 LSHIFT or
   r> IC-D  dup 6 RSHIFT 22 LSHIFT  swap 63 AND 10 LSHIFT or  or ;

: ENC-ADD  ( R i64 -- R )  $8B000000 RRR EMITW ;

: ENC-SUB  ( R i64 -- R )  $CB000000 RRR EMITW ;

: ENC-MUL  ( R i64 -- R )  $9B007C00 RRR EMITW ;

: ENC-SDIV ( R i64 -- R )  $9AC00C00 RRR EMITW ;

: ENC-UDIV ( R i64 -- R )  $9AC00800 RRR EMITW ;

: ENC-AND  ( R i64 -- R )  $8A000000 RRR EMITW ;

: ENC-ORR  ( R i64 -- R )  $AA000000 RRR EMITW ;

: ENC-EOR  ( R i64 -- R )  $CA000000 RRR EMITW ;
CHECKING-ON? off   \ logical-immediate encode/encoders are metaprogramming (bit math, loops)

\ logical immediate: IC-C = nis = N<<12|immr<<6|imms.  base | N<<22|immr<<16|imms<<10|rn<<5|rd
: RRI ( i base -- word )  swap {: ic :}              \ ic = record index (like RRR's convention)
   ic IC-A or  ic IC-B 5 lshift or  {: w :}
   ic IC-C {: nis :}
   w  nis 12 rshift 22 lshift or  nis 6 rshift 63 and 16 lshift or  nis 63 and 10 lshift or ;

: ENC-ANDI ( i -- )  $92000000 RRI EMITW ;

: ENC-ORRI ( i -- )  $B2000000 RRI EMITW ;

: ENC-EORI ( i -- )  $D2000000 RRI EMITW ;
\ encodeBitMasks (ENC-LOGIMM, all element sizes) lives in icode.fs now — the
\ ANDI,/ORRI,/EORI, mnemonics take PLAIN masks and encode at build time;
\ regstack.fs still probes ENC-LOGIMM to pick the #imm form.
CHECKING-ON? on

: ENC-LSLV ( R i64 -- R )  $9AC02000 RRR EMITW ;

: ENC-LSRV ( R i64 -- R )  $9AC02400 RRR EMITW ;

: ENC-ASRV ( R i64 -- R )  $9AC02800 RRR EMITW ;
CHECKING-ON? off

\ --- trusted encoders (return-stack juggling / table-memory machinery) ---
: ENC-MOVZ ( i -- )  >r r@ IC-A r> IC-B 0 MOVZHW EMITW ;

: ENC-MOVK ( i -- )  >r r@ IC-A r@ IC-B r> IC-C 16 / MOVKHW EMITW ;

: ENC-MOVN ( i -- )  >r r@ IC-A r> IC-B 0 MOVNHW EMITW ;

: ENC-MOV  ( i -- )  >r $AA0003E0 r@ IC-A or r> IC-B 16 lshift or EMITW ;

: ENC-LIT  ( i -- )  >r r@ IC-A r> IC-B dup MOVN-SHORTER? if LIT-N else LIT-Z then ;

: ENC-ADDI ( i -- )  >r $91000000 r@ IC-A or r@ IC-B 5 lshift or r> IC-C ?IMM12 10 lshift or EMITW ;

: ENC-SUBI ( i -- )  >r $D1000000 r@ IC-A or r@ IC-B 5 lshift or r> IC-C ?IMM12 10 lshift or EMITW ;

: ENC-LSLI ( i -- )  >r $D3400000 r@ IC-A or r@ IC-B 5 lshift or
   r@ IC-C 64 swap - 63 and 16 lshift or  r> IC-C 63 swap - 10 lshift or EMITW ;

: ENC-LSRI ( i -- )  >r $D340FC00 r@ IC-A or r@ IC-B 5 lshift or r> IC-C 16 lshift or EMITW ;

: ENC-ASRI ( i -- )  >r $9340FC00 r@ IC-A or r@ IC-B 5 lshift or r> IC-C 16 lshift or EMITW ;

: ENC-CMP  ( i -- )  >r $EB00001F r@ IC-A 5 lshift or r> IC-B 16 lshift or EMITW ;

: ENC-CMPI ( i -- )  >r $F100001F r@ IC-A 5 lshift or r> IC-B ?IMM12 10 lshift or EMITW ;

: ENC-CSET ( i -- )  >r $9A9F07E0 r@ IC-A or r> IC-B 1 xor 12 lshift or EMITW ;

: ENC-B    ( i -- )  BDELTA ?REL26 $3FFFFFF and $14000000 or EMITW ;

: ENC-BL   ( i -- )  BDELTA ?REL26 $3FFFFFF and $94000000 or EMITW ;

: ENC-BCOND ( i -- )  >r r@ BDELTA ?REL19 $7FFFF and 5 lshift $54000000 or r> IC-B or EMITW ;

: ENC-CBZ  ( i -- )  >r r@ BDELTA ?REL19 $7FFFF and 5 lshift $B4000000 or r> IC-B or EMITW ;

: ENC-CBNZ ( i -- )  >r r@ BDELTA ?REL19 $7FFFF and 5 lshift $B5000000 or r> IC-B or EMITW ;

: ENC-BR   ( i -- )  IC-A 5 lshift $D61F0000 or EMITW ;

: ENC-BLR  ( i -- )  IC-A 5 lshift $D63F0000 or EMITW ;

: ENC-RET  ( i -- )  drop $D65F03C0 EMITW ;

: ENC-ADR  ( i -- )  {: i :}  i BDELTA 4 * {: d :}
   d -1048576 1048576 within 0= if E-BRANCH-RANGE throw then
   $10000000  d 3 and 29 lshift or  d 4 / $7FFFF and 5 lshift or  i IC-B or EMITW ;

: ENC-LDR  ( i -- )  >r $F9400000 r@ IC-A or r@ IC-B 5 lshift or r> IC-C ?SC8 10 lshift or EMITW ;

: ENC-STR  ( i -- )  >r $F9000000 r@ IC-A or r@ IC-B 5 lshift or r> IC-C ?SC8 10 lshift or EMITW ;

: ENC-LDRB ( i -- )  >r $39400000 r@ IC-A or r@ IC-B 5 lshift or r> IC-C ?IMM12 10 lshift or EMITW ;

: ENC-STRB ( i -- )  >r $39000000 r@ IC-A or r@ IC-B 5 lshift or r> IC-C ?IMM12 10 lshift or EMITW ;

: ?SC4 ( off -- imm12 )  dup 0 16381 within 0=  over 3 and 0<> or  if E-IMM-RANGE throw then  2 rshift ;

: ENC-LDRW ( i -- )  >r $B9400000 r@ IC-A or r@ IC-B 5 lshift or r> IC-C ?SC4 10 lshift or EMITW ;

: ENC-STRW ( i -- )  >r $B9000000 r@ IC-A or r@ IC-B 5 lshift or r> IC-C ?SC4 10 lshift or EMITW ;

: ENC-LDRPO ( i -- )  >r $F8400400 r@ IC-A or r@ IC-B 5 lshift or r> IC-C ?IMM9 12 lshift or EMITW ;

: ENC-STRPR ( i -- )  >r $F8000C00 r@ IC-A or r@ IC-B 5 lshift or r> IC-C ?IMM9 12 lshift or EMITW ;

: ENC-LDPPO ( i -- )  >r $A8C00000 r@ IC-A or r@ IC-B 10 lshift or r@ IC-C 5 lshift or
   r> IC-D ?SC7 15 lshift or EMITW ;

: ENC-STPPR ( i -- )  >r $A9800000 r@ IC-A or r@ IC-B 10 lshift or r@ IC-C 5 lshift or
   r> IC-D ?SC7 15 lshift or EMITW ;

: ENC-SVC  ( i -- )  IC-A 5 lshift $D4000001 or EMITW ;

: ENC-NOP  ( i -- )  drop $D503201F EMITW ;

: ENC-ICIV ( i -- )  IC-A $D50B7520 or EMITW ;

: ENC-DCCV ( i -- )  IC-A $D50B7B20 or EMITW ;

: ENC-BRK  ( i -- )  drop $D4200000 EMITW ;       \ brk #0 — traps (SIGILL)

\ FP (double): Rd=IC-A Rn=IC-B Rm=IC-C, in the D-register file
: FRR ( base i -- word )  >r  r@ IC-A or  r@ IC-B 5 lshift or  r> IC-C 16 lshift or ;

: FRD ( base i -- word )  >r  r@ IC-A or  r> IC-B 5 lshift or ;

: ENC-FMOVXD ( i -- )  $9E670000 swap FRD EMITW ;

: ENC-FMOVDX ( i -- )  $9E660000 swap FRD EMITW ;

: ENC-FMOVDD ( i -- )  $1E604000 swap FRD EMITW ;

: ENC-FADD ( i -- )  $1E602800 swap FRR EMITW ;

: ENC-FSUB ( i -- )  $1E603800 swap FRR EMITW ;

: ENC-FMUL ( i -- )  $1E600800 swap FRR EMITW ;

: ENC-FDIV ( i -- )  $1E601800 swap FRR EMITW ;

: ENC-FNEG ( i -- )  $1E614000 swap FRD EMITW ;

: ENC-FABS ( i -- )  $1E60C000 swap FRD EMITW ;

: ENC-FSQRT ( i -- ) $1E61C000 swap FRD EMITW ;

: ENC-FCMP ( i -- )  >r $1E602000 r@ IC-A 5 lshift or r> IC-B 16 lshift or EMITW ;

: ENC-FCMP0 ( i -- ) IC-A 5 lshift $1E602008 or EMITW ;

: ENC-SCVTF ( i -- ) $9E620000 swap FRD EMITW ;

: ENC-FCVTZS ( i -- ) $9E780000 swap FRD EMITW ;

: ENC-DSB  ( i -- )  drop $D5033B9F EMITW ;

: ENC-ISB  ( i -- )  drop $D5033FDF EMITW ;

: ENC-NONE ( i -- )  drop ;

: ENC-NO-ENC ( i -- )  drop E-NO-ENC throw ;

\ embedded data: copy bytes into the buffer, pad to a 4-byte word
: ENC-BYTES ( i -- )
   dup IC-A swap IC-B {: src u :}
   ABUF @ WPOS @ 4 * +  {: dst :}
   src dst u move
   u 3 + -4 and {: padded :}
   padded u ?do  0  dst i + c!  loop          \ zero the pad tail
   padded 4 / WPOS +! ;

: LEN-BYTES ( i -- n )  IC-B 3 + -4 and 4 / ;

: ENC-DCQ ( i -- )  IC-A  ABUF @ WPOS @ 4 * +  !  2 WPOS +! ;

: LEN-DCQ ( i -- 2 )  drop 2 ;

: ENC-DLBL ( i -- )  IC-A LBL@ 4 *  ABUF @ WPOS @ 4 * +  !  2 WPOS +! ;  \ cell = label byte-offset

: LEN-DLBL ( i -- 2 )  drop 2 ;

\ --- dispatch tables (indexed by op tag) ---
create ENCODERS #IOPS cells allot
create LENXTS   #IOPS cells allot

: ENC! ( xt iop -- )  cells ENCODERS + ! ;

: LEN! ( xt iop -- )  cells LENXTS + ! ;

: LEN1 ( i -- 1 )  drop 1 ;

: LEN0 ( i -- 0 )  drop 0 ;

: LENLIT ( i -- n )  IC-B LIT-LEN ;

: INIT-TABLES ( -- )  #IOPS 0 ?do  ['] ENC-NO-ENC i ENC!  ['] LEN1 i LEN!  loop ;
INIT-TABLES
' ENC-MOVZ IOP-MOVZ ENC!   ' ENC-MOVK IOP-MOVK ENC!   ' ENC-MOVN IOP-MOVN ENC!
' ENC-MOV  IOP-MOV  ENC!   ' ENC-LIT  IOP-LIT  ENC!
' ENC-ADD  IOP-ADD  ENC!   ' ENC-ADDI IOP-ADDI ENC!
' ENC-SUB  IOP-SUB  ENC!   ' ENC-SUBI IOP-SUBI ENC!
' ENC-MUL  IOP-MUL  ENC!   ' ENC-SDIV IOP-SDIV ENC!   ' ENC-UDIV IOP-UDIV ENC!
' ENC-AND  IOP-AND  ENC!   ' ENC-ORR  IOP-ORR  ENC!   ' ENC-EOR  IOP-EOR  ENC!
' ENC-LSLI IOP-LSLI ENC!   ' ENC-LSRI IOP-LSRI ENC!   ' ENC-ASRI IOP-ASRI ENC!
' ENC-LSLV IOP-LSLV ENC!   ' ENC-LSRV IOP-LSRV ENC!   ' ENC-ASRV IOP-ASRV ENC!
' ENC-CMP  IOP-CMP  ENC!   ' ENC-CMPI IOP-CMPI ENC!   ' ENC-CSET IOP-CSET ENC!
' ENC-B    IOP-B    ENC!   ' ENC-BL   IOP-BL   ENC!   ' ENC-BCOND IOP-BCOND ENC!
' ENC-CBZ  IOP-CBZ  ENC!   ' ENC-CBNZ IOP-CBNZ ENC!
' ENC-BR   IOP-BR   ENC!   ' ENC-BLR  IOP-BLR  ENC!   ' ENC-RET  IOP-RET  ENC!
' ENC-ADR  IOP-ADR  ENC!
' ENC-LDR  IOP-LDR  ENC!   ' ENC-STR  IOP-STR  ENC!
' ENC-LDRB IOP-LDRB ENC!   ' ENC-STRB IOP-STRB ENC!
' ENC-LDRPO IOP-LDRPO ENC! ' ENC-STRPR IOP-STRPR ENC!
' ENC-LDPPO IOP-LDPPO ENC! ' ENC-STPPR IOP-STPPR ENC!
' ENC-SVC  IOP-SVC  ENC!   ' ENC-NOP  IOP-NOP  ENC!
' ENC-ICIV IOP-ICIV ENC!   ' ENC-DSB  IOP-DSB  ENC!   ' ENC-ISB  IOP-ISB  ENC!
' ENC-DCCV IOP-DCCV ENC!
' ENC-BRK  IOP-BRK  ENC!
' ENC-ANDI IOP-ANDI ENC!   ' ENC-ORRI IOP-ORRI ENC!   ' ENC-EORI IOP-EORI ENC!
' ENC-FMOVXD IOP-FMOVXD ENC!  ' ENC-FMOVDX IOP-FMOVDX ENC!  ' ENC-FMOVDD IOP-FMOVDD ENC!
' ENC-FADD IOP-FADD ENC!   ' ENC-FSUB IOP-FSUB ENC!
' ENC-FMUL IOP-FMUL ENC!   ' ENC-FDIV IOP-FDIV ENC!
' ENC-FNEG IOP-FNEG ENC!   ' ENC-FABS IOP-FABS ENC!   ' ENC-FSQRT IOP-FSQRT ENC!
' ENC-FCMP IOP-FCMP ENC!   ' ENC-FCMP0 IOP-FCMP0 ENC!
' ENC-SCVTF IOP-SCVTF ENC!  ' ENC-FCVTZS IOP-FCVTZS ENC!
' ENC-NONE IOP-LABEL ENC!  ' ENC-NONE IOP-DEAD ENC!
' ENC-BYTES IOP-BYTES ENC!  ' ENC-DCQ IOP-DCQ ENC!  ' ENC-DLBL IOP-DLBL ENC!
' ENC-LDRW IOP-LDRW ENC!    ' ENC-STRW IOP-STRW ENC!
' LENLIT IOP-LIT LEN!    ' LEN0 IOP-LABEL LEN!    ' LEN0 IOP-DEAD LEN!
' LEN-BYTES IOP-BYTES LEN!   ' LEN-DCQ IOP-DCQ LEN!   ' LEN-DLBL IOP-DLBL LEN!

\ --- assembly: size + bind labels, then encode ---
: IC-LEN ( i -- n )  dup IC-OP cells LENXTS + @ execute ;

: PASS1 ( -- )  0 WPOS !
   #IC @ 0 ?do
      i IC-OP IOP-LABEL = if WPOS @ i IC-A cells LBLPOS + ! then
      i IC-LEN WPOS +!
   loop ;

: PASS2 ( -- )  0 WPOS !
   #IC @ 0 ?do  i dup IC-OP cells ENCODERS + @ execute  loop ;

: ASSEMBLE ( buf -- nbytes )  ABUF !  PASS1 PASS2  WPOS @ 4 * ;
