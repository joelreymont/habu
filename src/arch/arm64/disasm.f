\ disasm.f - table-driven ARM64 disassembler. DIS1 prints one u32 as one line, no
\ newline, as binutils' objdump prints it: the mnemonic, the alias objdump picks
\ included, then the operands after one space; a word no row decodes prints ?
\ and 8 hex digits. DIS1 has no address, so a branch, ADR or ADRP target prints
\ as #byte-offset from the instruction.

require src/lib/fmt.f

package A64DIS

: U64, ( n -- )
   {: v :}
   8 0 ?do v i 8 * rshift 255 and c, loop ;


variable DTB-N

\ Lays a DTB row [mask:8][val:8][kind:1][nlen:1][name]. DIS1 takes the first row
\ that matches and DVALID? admits, so an alias's row precedes its instruction's.
: ROW, ( n n n str -- )
   {: mask val kind name:str :}
   mask U64,  val U64,  kind c,  name STR:LEN c,
   name STR:LEN 0 ?do name STR:DATA i + c@ c, loop
   1 DTB-N +! ;


\ Operand forms: each kind is one way DOUT prints a row's operands.
 0 constant K-RRR      1 constant K-MOVZ     2 constant K-MOVN     3 constant K-MOVW
 4 constant K-MOVSP    5 constant K-ADDI     6 constant K-CMPI     7 constant K-CMPSR
 8 constant K-NEGSR    9 constant K-ADDSR   10 constant K-MOVR    11 constant K-MVN
12 constant K-LOGSR   13 constant K-MOVLI   14 constant K-LOGI    15 constant K-SHRI
16 constant K-LSLI    17 constant K-BFIZ    18 constant K-BFX     19 constant K-SXT
20 constant K-MADD    21 constant K-CSEL    22 constant K-CSET    23 constant K-CINC
24 constant K-LDU     25 constant K-LD9     26 constant K-PRE     27 constant K-POST
28 constant K-PAIR    29 constant K-PAIRPRE 30 constant K-PAIRPOST 31 constant K-LDAR
32 constant K-CAS     33 constant K-B26     34 constant K-BCOND   35 constant K-CB
36 constant K-RN      37 constant K-NONE    38 constant K-ADR     39 constant K-ADRP
40 constant K-IMM16   41 constant K-SYS     42 constant K-MRS     43 constant K-DDD
44 constant K-DD      45 constant K-FMOVI   46 constant K-FCMP    47 constant K-FCMP0
48 constant K-FCSEL   49 constant K-DX      50 constant K-XD      51 constant K-VLP
52 constant K-VL      53 constant K-VMOV    54 constant K-V3B     55 constant K-V3
56 constant K-V2B     57 constant K-SHAH    58 constant K-V3S     59 constant K-V2S


\ Data-processing rows decode the 64-bit forms only. A K-SYS row's name is the
\ operation, a K-MRS row's the system register.
: DTB-ROWS ( -- )
   $FF800000 $D2800000 K-MOVZ s" mov" ROW,
   $FF800000 $92800000 K-MOVN s" mov" ROW,
   $FF800000 $D2800000 K-MOVW s" movz" ROW,
   $FF800000 $92800000 K-MOVW s" movn" ROW,
   $FF800000 $F2800000 K-MOVW s" movk" ROW,

   $FF800000 $91000000 K-MOVSP s" mov" ROW,
   $FF800000 $91000000 K-ADDI s" add" ROW,
   $FF800000 $D1000000 K-ADDI s" sub" ROW,
   $FF80001F $B100001F K-CMPI s" cmn" ROW,
   $FF80001F $F100001F K-CMPI s" cmp" ROW,

   $FF20001F $EB00001F K-CMPSR s" cmp" ROW,
   $FF20001F $AB00001F K-CMPSR s" cmn" ROW,
   $FF2003E0 $CB0003E0 K-NEGSR s" neg" ROW,
   $FF2003E0 $EB0003E0 K-NEGSR s" negs" ROW,
   $FF200000 $8B000000 K-ADDSR s" add" ROW,
   $FF200000 $CB000000 K-ADDSR s" sub" ROW,
   $FF200000 $AB000000 K-ADDSR s" adds" ROW,
   $FF200000 $EB000000 K-ADDSR s" subs" ROW,

   $FFE0FFE0 $AA0003E0 K-MOVR s" mov" ROW,
   $FF2003E0 $AA2003E0 K-MVN s" mvn" ROW,
   $FF200000 $8A000000 K-LOGSR s" and" ROW,
   $FF200000 $8A200000 K-LOGSR s" bic" ROW,
   $FF200000 $AA000000 K-LOGSR s" orr" ROW,
   $FF200000 $AA200000 K-LOGSR s" orn" ROW,
   $FF200000 $CA000000 K-LOGSR s" eor" ROW,
   $FF200000 $CA200000 K-LOGSR s" eon" ROW,

   $FF8003E0 $B20003E0 K-MOVLI s" mov" ROW,
   $FF800000 $92000000 K-LOGI s" and" ROW,
   $FF800000 $B2000000 K-LOGI s" orr" ROW,
   $FF800000 $D2000000 K-LOGI s" eor" ROW,

   $FFC0FC00 $D340FC00 K-SHRI s" lsr" ROW,
   $FFC00000 $D3400000 K-LSLI s" lsl" ROW,
   $FFC00000 $D3400000 K-BFIZ s" ubfiz" ROW,
   $FFC00000 $D3400000 K-BFX s" ubfx" ROW,
   $FFC0FC00 $9340FC00 K-SHRI s" asr" ROW,
   $FFC00000 $93400000 K-BFIZ s" sbfiz" ROW,
   $FFFFFC00 $93401C00 K-SXT s" sxtb" ROW,
   $FFFFFC00 $93403C00 K-SXT s" sxth" ROW,
   $FFFFFC00 $93407C00 K-SXT s" sxtw" ROW,
   $FFC00000 $93400000 K-BFX s" sbfx" ROW,

   $FFE0FC00 $9AC02000 K-RRR s" lsl" ROW,
   $FFE0FC00 $9AC02400 K-RRR s" lsr" ROW,
   $FFE0FC00 $9AC02800 K-RRR s" asr" ROW,
   $FFE0FC00 $9AC02C00 K-RRR s" ror" ROW,
   $FFE0FC00 $9AC00800 K-RRR s" udiv" ROW,
   $FFE0FC00 $9AC00C00 K-RRR s" sdiv" ROW,
   $FFE0FC00 $9B007C00 K-RRR s" mul" ROW,
   $FFE0FC00 $9B00FC00 K-RRR s" mneg" ROW,
   $FFE08000 $9B000000 K-MADD s" madd" ROW,
   $FFE08000 $9B008000 K-MADD s" msub" ROW,

   $FFE00C00 $9A800000 K-CSEL s" csel" ROW,
   $FFFF0FE0 $9A9F07E0 K-CSET s" cset" ROW,
   $FFE00C00 $9A800400 K-CINC s" cinc" ROW,
   $FFE00C00 $9A800400 K-CSEL s" csinc" ROW,
   $FFFF0FE0 $DA9F03E0 K-CSET s" csetm" ROW,
   $FFE00C00 $DA800000 K-CINC s" cinv" ROW,
   $FFE00C00 $DA800000 K-CSEL s" csinv" ROW,

   $FFC00000 $39400000 K-LDU s" ldrb" ROW,
   $FFC00000 $39000000 K-LDU s" strb" ROW,
   $FFC00000 $79400000 K-LDU s" ldrh" ROW,
   $FFC00000 $79000000 K-LDU s" strh" ROW,
   $BBC00000 $B9400000 K-LDU s" ldr" ROW,
   $BBC00000 $B9000000 K-LDU s" str" ROW,
   $BBE00C00 $B8400000 K-LD9 s" ldur" ROW,
   $BBE00C00 $B8000000 K-LD9 s" stur" ROW,
   $BBE00C00 $B8400C00 K-PRE s" ldr" ROW,
   $BBE00C00 $B8000C00 K-PRE s" str" ROW,
   $BBE00C00 $B8400400 K-POST s" ldr" ROW,
   $BBE00C00 $B8000400 K-POST s" str" ROW,
   $FFC00000 $A9000000 K-PAIR s" stp" ROW,
   $FFC00000 $A9400000 K-PAIR s" ldp" ROW,
   $FFC00000 $A9800000 K-PAIRPRE s" stp" ROW,
   $FFC00000 $A9C00000 K-PAIRPRE s" ldp" ROW,
   $FFC00000 $A8800000 K-PAIRPOST s" stp" ROW,
   $FFC00000 $A8C00000 K-PAIRPOST s" ldp" ROW,
   $BFFFFC00 $88DFFC00 K-LDAR s" ldar" ROW,
   $BFFFFC00 $889FFC00 K-LDAR s" stlr" ROW,
   $FFE0FC00 $C8E0FC00 K-CAS s" casal" ROW,
   $FFE0FC00 $F8E00000 K-CAS s" ldaddal" ROW,

   $FC000000 $14000000 K-B26 s" b" ROW,
   $FC000000 $94000000 K-B26 s" bl" ROW,
   $FF000010 $54000000 K-BCOND s" b." ROW,
   $FF000000 $B4000000 K-CB s" cbz" ROW,
   $FF000000 $B5000000 K-CB s" cbnz" ROW,
   $FFFFFC1F $D63F0000 K-RN s" blr" ROW,
   $FFFFFC1F $D61F0000 K-RN s" br" ROW,
   $FFFFFFFF $D65F03C0 K-NONE s" ret" ROW,
   $FFFFFC1F $D65F0000 K-RN s" ret" ROW,
   $9F000000 $10000000 K-ADR s" adr" ROW,
   $9F000000 $90000000 K-ADRP s" adrp" ROW,

   $FFE0001F $D4000001 K-IMM16 s" svc" ROW,
   $FFE0001F $D4200000 K-IMM16 s" brk" ROW,
   $FFFFFFFF $D503201F K-NONE s" nop" ROW,
   $FFFFFFFF $D503203F K-NONE s" yield" ROW,
   $FFFFFFFF $D5033FDF K-NONE s" isb" ROW,
   $FFFFFFFF $D5033B9F K-NONE s" dsb ish" ROW,
   $FFFFFFFF $D5033BBF K-NONE s" dmb ish" ROW,
   $FFFFFFE0 $D50B7520 K-SYS s" ic ivau" ROW,
   $FFFFFFE0 $D50B7B20 K-SYS s" dc cvau" ROW,
   $FFFFFFE0 $D53BD040 K-MRS s" tpidr_el0" ROW,
   $FFFFFFE0 $D53B0020 K-MRS s" ctr_el0" ROW,

   $FFE0FC00 $1E602800 K-DDD s" fadd" ROW,
   $FFE0FC00 $1E603800 K-DDD s" fsub" ROW,
   $FFE0FC00 $1E600800 K-DDD s" fmul" ROW,
   $FFE0FC00 $1E601800 K-DDD s" fdiv" ROW,
   $FFFFFC00 $1E604000 K-DD s" fmov" ROW,
   $FFFFFC00 $1E614000 K-DD s" fneg" ROW,
   $FFFFFC00 $1E60C000 K-DD s" fabs" ROW,
   $FFFFFC00 $1E61C000 K-DD s" fsqrt" ROW,
   $FFE01FE0 $1E601000 K-FMOVI s" fmov" ROW,
   $FFE0FC1F $1E602000 K-FCMP s" fcmp" ROW,
   $FFFFFC1F $1E602008 K-FCMP0 s" fcmp" ROW,
   $FFE00C00 $1E600C00 K-FCSEL s" fcsel" ROW,
   $FFFFFC00 $9E670000 K-DX s" fmov" ROW,
   $FFFFFC00 $9E620000 K-DX s" scvtf" ROW,
   $FFFFFC00 $9E660000 K-XD s" fmov" ROW,
   $FFFFFC00 $9E780000 K-XD s" fcvtzs" ROW,

   $FFFFF000 $4CDF7000 K-VLP s" ld1" ROW,
   $FFFFF000 $4CDF2000 K-VLP s" ld1" ROW,
   $FFFFF000 $4C408000 K-VL s" ld2" ROW,
   $FFFFF000 $4C008000 K-VL s" st2" ROW,
   $FFE0FC00 $4EA01C00 K-VMOV s" mov" ROW,
   $FFE0FC00 $4EA01C00 K-V3B s" orr" ROW,
   $FF20FC00 $4E208400 K-V3 s" add" ROW,
   $FFFFFC00 $6E200800 K-V2B s" rev32" ROW,
   $FFE0FC00 $5E004000 K-SHAH s" sha256h" ROW,
   $FFE0FC00 $5E005000 K-SHAH s" sha256h2" ROW,
   $FFE0FC00 $5E006000 K-V3S s" sha256su1" ROW,
   $FFFFFC00 $5E282800 K-V2S s" sha256su0" ROW, ;

create DTB DTB-ROWS


: U64@ ( ^u8 -- n ) {: p :}
   p c@  p 1 + c@ 8 lshift or  p 2 + c@ 16 lshift or  p 3 + c@ 24 lshift or
   p 4 + c@ 32 lshift or  p 5 + c@ 40 lshift or  p 6 + c@ 48 lshift or  p 7 + c@ 56 lshift or ;

: U32@ ( ^u8 -- n ) {: p :}
   p c@  p 1 + c@ 8 lshift or  p 2 + c@ 16 lshift or  p 3 + c@ 24 lshift or ;

: FRD ( n -- n ) {: w :} w 31 and ;

: FRN ( n -- n ) {: w :} w 5 rshift 31 and ;

: FRM ( n -- n ) {: w :} w 16 rshift 31 and ;

\ Ra of a 3-source instruction and Rt2 of a pair, bits 14-10.
: FRA ( n -- n ) {: w :} w 10 rshift 31 and ;

: FI12 ( n -- n ) {: w :} w 10 rshift 4095 and ;

: FI16 ( n -- n ) {: w :} w 5 rshift 65535 and ;

\ Move-wide's shift in bits: hw, bits 22-21, times 16.
: FHW ( n -- n ) {: w :} w 21 rshift 3 and 16 * ;

\ A shifted register's amount and a bitfield's imms, bits 15-10.
: FIMMS ( n -- n ) {: w :} w 10 rshift 63 and ;

: FIMMR ( n -- n ) {: w :} w 16 rshift 63 and ;

\ A shifted register's shift type, bits 23-22: lsl, lsr, asr, ror.
: FSHT ( n -- n ) {: w :} w 22 rshift 3 and ;

\ A conditional select's condition, bits 15-12.
: FCOND ( n -- n ) {: w :} w 12 rshift 15 and ;

\ A load or store's size, bits 31-30: the access is 2^size bytes.
: FSIZE ( n -- n ) {: w :} w 30 rshift 3 and ;

\ A load or store's V, bit 26: the register is a floating-point one.
: FV? ( n -- bool ) {: w :} w 26 rshift 1 and 0<> ;

\ An add or sub immediate's sh, bit 22: imm12 is shifted left 12.
: FSH12? ( n -- bool ) {: w :} w 22 rshift 1 and 0<> ;


\ v, below 2^bits, read as a bits-wide two's-complement number.
: SEXT ( n n -- n )
   {: v bits :}
   v 1 bits 1 - lshift and 0<> IF v 1 bits lshift - ELSE v THEN ;


: FI19 ( n -- n )
   {: w :}
   w 5 rshift $7FFFF and 19 SEXT 4 * ;


: FI26 ( n -- n )
   {: w :}
   w $3FFFFFF and 26 SEXT 4 * ;


: FI9 ( n -- n )
   {: w :}
   w 12 rshift 511 and 9 SEXT ;


\ A pair's offset: imm7, bits 21-15, in 8-byte units.
: FI7 ( n -- n )
   {: w :}
   w 15 rshift 127 and 7 SEXT 8 * ;


\ ADR's offset: immhi in bits 23-5 above immlo in bits 30-29.
: FADR ( n -- n )
   {: w :}
   w 5 rshift $7FFFF and 2 lshift  w 29 rshift 3 and or 21 SEXT ;


create CHB 4 allot

: EMITC ( n -- ) {: c :} c CHB c! CHB 1 >str type ;

: SPC ( -- ) 32 EMITC ;

: CM ( -- ) s" , " type ;

: DTRUE ( -- bool ) 0 0= ;

: DFALSE ( -- bool ) DTRUE 0= ;


\ The count of hex digits of u read unsigned, at least one.
: NIBBLES ( n -- n )
   1 swap BEGIN 4 rshift dup 0<> WHILE swap 1 + swap REPEAT drop ;


\ u read unsigned, in lowercase hex after #0x.
: #H ( n -- )
   {: u :}
   s" #0x" type
   u NIBBLES {: n :}
   n 0 ?do
      s" 0123456789abcdef" STR:DATA  u n 1 - i - 4 * rshift 15 and + c@ EMITC
   loop ;


: #D ( n -- ) 35 EMITC FMT:.INT ;

: .RNUM ( n str -- ) {: r p:str :} p type r FMT:.INT ;

\ A register where 31 is the zero register; .XSP's 31 is the stack pointer.
: .X ( n -- ) {: r :} r 31 = IF s" xzr" type ELSE r s" x" .RNUM THEN ;

: .XSP ( n -- ) {: r :} r 31 = IF s" sp" type ELSE r s" x" .RNUM THEN ;

: .W ( n -- ) {: r :} r 31 = IF s" wzr" type ELSE r s" w" .RNUM THEN ;

: .D ( n -- ) s" d" .RNUM ;


\ Condition names by bits 3-0, two letters each.
: DCOND ( n -- )
   {: w :}
   s" eqnecsccmiplvsvchilsgeltgtlealnv" STR:DATA
   w 15 and 2 * + 2 >str type ;


\ An arrangement by size, bits 23-22, of a 128-bit vector.
: .ARR ( n -- )
   {: size :}
   size 0 = IF s" .16b" type THEN
   size 1 = IF s" .8h" type THEN
   size 2 = IF s" .4s" type THEN
   size 3 = IF s" .2d" type THEN ;


: .V ( n n -- ) {: r size :} r s" v" .RNUM size .ARR ;


\ Index of the highest set bit, -1 for zero.
: HIGH-BIT ( n -- n )
   -1 swap BEGIN dup 0<> WHILE 1 rshift swap 1 + swap REPEAT drop ;


\ The low n bits set; n below 64.
: ONES ( n -- n )
   {: n :}
   1 n lshift 1 - ;


\ v, below 2^esize, repeated through all 64 bits; esize a power of two.
: REPLICATE ( n n -- n )
   BEGIN dup 64 < WHILE >r dup r@ lshift or r> 2 * REPEAT drop ;


\ The architecture's DecodeBitMasks for a 64-bit logical immediate (N bit 22,
\ immr bits 21-16, imms bits 15-10); false for a reserved encoding.
: BITMASK ( n -- n bool )
   {: w :}
   w 22 rshift 1 and 6 lshift  w 10 rshift 63 and invert 63 and or HIGH-BIT
   {: len :}
   len 1 < IF 0 DFALSE EXIT THEN
   len ONES {: levels :}
   w 10 rshift 63 and levels and {: s :}
   s levels = IF 0 DFALSE EXIT THEN
   w 16 rshift 63 and levels and {: r :}
   1 len lshift {: esize :}
   s 1 + ONES {: welem :}
   r 0= IF welem ELSE welem r rshift welem esize r - lshift or THEN
   esize 64 < IF esize ONES and THEN
   esize REPLICATE DTRUE ;


\ u has at most one nonzero 16-bit half at a multiple of 16 bits.
: HALF? ( n -- bool )
   {: u :}
   u $FFFF and u =  u $FFFF0000 and u = or
   u $FFFF00000000 and u = or  u $FFFF000000000000 and u = or ;


\ objdump names a logical immediate mov only when MOVZ or MOVN cannot load it.
: WIDE? ( n -- bool ) {: u :} u HALF? u invert HALF? or ;


\ A shifted register's shift, omitted when it is lsl #0.
: .SH ( n -- )
   {: w :}
   w FSHT {: t :}
   w FIMMS {: a :}
   a 0<> t 0<> or IF
      CM s" lsllsrasrror" STR:DATA t 3 * + 3 >str type SPC a #D
   THEN ;


: .I12 ( n -- )
   {: w :}
   w FI12 #H  w FSH12? IF s" , lsl #12" type THEN ;


: .MEM ( n n -- )
   {: rn off :}
   91 EMITC rn .XSP off 0<> IF CM off #D THEN 93 EMITC ;


: .PRE ( n n -- )
   {: rn off :}
   91 EMITC rn .XSP CM off #D s" ]!" type ;


: .POST ( n n -- )
   {: rn off :}
   91 EMITC rn .XSP s" ], " type off #D ;


\ A load or store's register: size and V give its width.
: .RT ( n n -- )
   {: w r :}
   w FV? IF
      w FSIZE 3 = IF r .D ELSE r s" s" .RNUM THEN
   ELSE
      w FSIZE 3 = IF r .X ELSE r .W THEN
   THEN ;


\ fmov's imm8 a:b:cd:efgh is (-1)^a * (16+efgh)/16 * 2^e, e+3 = cd+4 when b is
\ clear and cd when set; value*10^7 is an integer, printed as objdump's %.18e.
: POW10 ( n -- n ) 1 swap 0 ?do 10 * loop ;

: NDIGITS ( n -- n )
   1 swap BEGIN 10 / dup 0<> WHILE swap 1 + swap REPEAT drop ;

: .FIMM ( n -- )
   {: w :}
   w 13 rshift 255 and {: imm :}
   s" #" type
   imm 7 rshift 0<> IF 45 EMITC THEN
   imm 4 rshift 3 and {: cd :}
   imm 6 rshift 1 and 0= IF cd 4 + ELSE cd THEN {: e3 :}
   imm 15 and 16 + e3 lshift 78125 * {: big :}
   big NDIGITS {: nd :}
   19 0 ?do
      i nd < IF big nd 1 - i - POW10 / 10 mod ELSE 0 THEN 48 + EMITC
      i 0= IF 46 EMITC THEN
   loop
   nd 8 - {: x :}
   x 0 < IF s" e-" type ELSE s" e+" type THEN
   x abs 10 / 48 + EMITC  x abs 10 mod 48 + EMITC ;


\ A vector register list's length by opcode, bits 15-12, of a multiple-structure
\ load or store.
: VNREGS ( n -- n )
   {: w :}
   w 12 rshift 15 and {: op :}
   op 7 = IF 1 EXIT THEN
   op 8 = op 10 = or IF 2 EXIT THEN
   op 4 = op 6 = or IF 3 EXIT THEN
   4 ;


: .VLIST ( n -- )
   {: w :}
   w 10 rshift 3 and {: size :}
   123 EMITC w FRD size .V
   w VNREGS 1 > IF 45 EMITC w FRD w VNREGS + 1 - 31 and size .V THEN
   s" }, " type ;


: DVALID? ( n n -- bool )
   {: w kind :}
   kind K-MOVZ = kind K-MOVN = or IF w FI16 0= w FHW 0<> and 0= EXIT THEN
   kind K-MOVSP = IF
      w FI12 0= w FSH12? 0= and  w FRD 31 = w FRN 31 = or and EXIT
   THEN
   kind K-CMPSR = kind K-NEGSR = or kind K-ADDSR = or IF w FSHT 3 <> EXIT THEN
   kind K-MOVLI = IF w BITMASK IF WIDE? 0= ELSE drop DFALSE THEN EXIT THEN
   kind K-LOGI = IF w BITMASK nip EXIT THEN
   kind K-LSLI = IF w FIMMS 63 <> w FIMMS 1 + w FIMMR = and EXIT THEN
   kind K-BFIZ = IF w FIMMS w FIMMR < EXIT THEN
   kind K-CSET = IF w FCOND 14 < EXIT THEN
   kind K-CINC = IF w FRN w FRM = w FRN 31 <> and w FCOND 14 < and EXIT THEN
   kind K-VMOV = IF w FRN w FRM = EXIT THEN
   DTRUE ;


\ The register forms: Rd, Rn, Rm and Ra in that order.
: DARGS-REG ( n n -- )
   {: w kind :}
   kind K-RRR = IF w FRD .X CM w FRN .X CM w FRM .X EXIT THEN
   kind K-ADDSR = kind K-LOGSR = or IF
      w FRD .X CM w FRN .X CM w FRM .X w .SH EXIT
   THEN
   kind K-CMPSR = IF w FRN .X CM w FRM .X w .SH EXIT THEN
   kind K-NEGSR = kind K-MVN = or IF w FRD .X CM w FRM .X w .SH EXIT THEN
   kind K-MOVR = IF w FRD .X CM w FRM .X EXIT THEN
   kind K-MADD = IF w FRD .X CM w FRN .X CM w FRM .X CM w FRA .X EXIT THEN
   kind K-CSEL = IF w FRD .X CM w FRN .X CM w FRM .X CM w FCOND DCOND EXIT THEN
   kind K-CSET = IF w FRD .X CM w FCOND 1 xor DCOND EXIT THEN
   kind K-CINC = IF w FRD .X CM w FRN .X CM w FCOND 1 xor DCOND EXIT THEN
   kind K-SXT = IF w FRD .X CM w FRN .W EXIT THEN
   kind K-RN = IF w FRN .X EXIT THEN ;


: DARGS-IMM ( n n -- )
   {: w kind :}
   kind K-MOVZ = IF w FRD .X CM w FI16 w FHW lshift #H EXIT THEN
   kind K-MOVN = IF w FRD .X CM w FI16 w FHW lshift invert #H EXIT THEN
   kind K-MOVW = IF
      w FRD .X CM w FI16 #H  w FHW 0<> IF s" , lsl #" type w FHW FMT:.INT THEN EXIT
   THEN
   kind K-MOVSP = IF w FRD .XSP CM w FRN .XSP EXIT THEN
   kind K-ADDI = IF w FRD .XSP CM w FRN .XSP CM w .I12 EXIT THEN
   kind K-CMPI = IF w FRN .XSP CM w .I12 EXIT THEN
   kind K-MOVLI = IF w FRD .XSP CM w BITMASK drop #H EXIT THEN
   kind K-LOGI = IF w FRD .XSP CM w FRN .X CM w BITMASK drop #H EXIT THEN
   kind K-SHRI = IF w FRD .X CM w FRN .X CM w FIMMR #D EXIT THEN
   kind K-LSLI = IF w FRD .X CM w FRN .X CM 63 w FIMMS - #D EXIT THEN
   kind K-BFIZ = IF
      w FRD .X CM w FRN .X CM 64 w FIMMR - #D CM w FIMMS 1 + #D EXIT
   THEN
   kind K-BFX = IF
      w FRD .X CM w FRN .X CM w FIMMR #D CM w FIMMS w FIMMR - 1 + #D EXIT
   THEN
   kind K-IMM16 = IF w FI16 #H EXIT THEN ;


: DARGS-MEM ( n n -- )
   {: w kind :}
   kind K-LDU = IF w w FRD .RT CM w FRN w FI12 w FSIZE lshift .MEM EXIT THEN
   kind K-LD9 = IF w w FRD .RT CM w FRN w FI9 .MEM EXIT THEN
   kind K-PRE = IF w w FRD .RT CM w FRN w FI9 .PRE EXIT THEN
   kind K-POST = IF w w FRD .RT CM w FRN w FI9 .POST EXIT THEN
   kind K-PAIR = IF w FRD .X CM w FRA .X CM w FRN w FI7 .MEM EXIT THEN
   kind K-PAIRPRE = IF w FRD .X CM w FRA .X CM w FRN w FI7 .PRE EXIT THEN
   kind K-PAIRPOST = IF w FRD .X CM w FRA .X CM w FRN w FI7 .POST EXIT THEN
   kind K-LDAR = IF
      w 30 rshift 1 and 0<> IF w FRD .X ELSE w FRD .W THEN CM w FRN 0 .MEM EXIT
   THEN
   kind K-CAS = IF w FRM .X CM w FRD .X CM w FRN 0 .MEM EXIT THEN
   kind K-VLP = IF w .VLIST w FRN 0 .MEM s" , #" type w VNREGS 16 * FMT:.INT EXIT THEN
   kind K-VL = IF w .VLIST w FRN 0 .MEM EXIT THEN ;


\ The branches and PC-relative addresses.
: DARGS-PC ( n n -- )
   {: w kind :}
   kind K-B26 = IF w FI26 #D EXIT THEN
   kind K-CB = IF w FRD .X CM w FI19 #D EXIT THEN
   kind K-ADR = IF w FRD .X CM w FADR #D EXIT THEN
   kind K-ADRP = IF w FRD .X CM w FADR 4096 * #D EXIT THEN ;


: DARGS-FP ( n n -- )
   {: w kind :}
   kind K-DDD = IF w FRD .D CM w FRN .D CM w FRM .D EXIT THEN
   kind K-DD = IF w FRD .D CM w FRN .D EXIT THEN
   kind K-FMOVI = IF w FRD .D CM w .FIMM EXIT THEN
   kind K-FCMP = IF w FRN .D CM w FRM .D EXIT THEN
   kind K-FCMP0 = IF w FRN .D s" , #0.0" type EXIT THEN
   kind K-FCSEL = IF w FRD .D CM w FRN .D CM w FRM .D CM w FCOND DCOND EXIT THEN
   kind K-DX = IF w FRD .D CM w FRN .X EXIT THEN
   kind K-XD = IF w FRD .X CM w FRN .D EXIT THEN
   kind K-VMOV = IF w FRD 0 .V CM w FRN 0 .V EXIT THEN
   kind K-V3B = IF w FRD 0 .V CM w FRN 0 .V CM w FRM 0 .V EXIT THEN
   kind K-V3 = IF
      w 22 rshift 3 and {: size :}
      w FRD size .V CM w FRN size .V CM w FRM size .V EXIT
   THEN
   kind K-V2B = IF w FRD 0 .V CM w FRN 0 .V EXIT THEN
   kind K-SHAH = IF w FRD s" q" .RNUM CM w FRN s" q" .RNUM CM w FRM 2 .V EXIT THEN
   kind K-V3S = IF w FRD 2 .V CM w FRN 2 .V CM w FRM 2 .V EXIT THEN
   kind K-V2S = IF w FRD 2 .V CM w FRN 2 .V EXIT THEN ;


\ Prints a decoded word: the name, then the operands its kind lays out.
: DOUT ( n n str -- )
   {: w kind name:str :}
   kind K-MRS = IF s" mrs " type w FRD .X CM name type EXIT THEN
   name type
   kind K-NONE = IF EXIT THEN
   kind K-SYS = IF CM w FRD .X EXIT THEN
   kind K-BCOND = IF w DCOND SPC w FI19 #D EXIT THEN
   SPC
   w kind DARGS-REG  w kind DARGS-IMM  w kind DARGS-MEM  w kind DARGS-PC
   w kind DARGS-FP ;


: DUNKNOWN ( n -- )
   {: w :}
   63 EMITC SPC  SB-RESET w 8 FMT:SB-HEX-WIDTH SB$ type ;


public

: DIS1 ( n -- )
   {: w :}
   DTB 0
   BEGIN dup DTB-N @ < WHILE
      {: row i :}
      row 16 + c@ {: kind :}
      row 17 + c@ {: nlen :}
      w row U64@ and row 8 + U64@ = IF
         w kind DVALID? IF w kind row 18 + nlen >str DOUT EXIT THEN
      THEN
      row 18 + nlen +  i 1 +
   REPEAT
   2drop w DUNKNOWN ;


\ The str is the code's bytes: one line per 4-byte instruction.
: DISASM ( str -- )
   {: s:str :}
   0 BEGIN dup s STR:LEN 4 / < WHILE
      s STR:DATA over 4 * + U32@ DIS1 cr
      1 +
   REPEAT drop ;

;package
