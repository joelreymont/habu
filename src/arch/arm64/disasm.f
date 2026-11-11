\ disasm.f - table-driven ARM64 disassembler. DIS1 prints one u32 as one line, no
\ newline: the mnemonic and each operand after one space, or ? and 8 hex digits.

require src/lib/fmt.f

: U64, ( n -- )
   {: v :}
   8 0 ?do v i 8 * rshift 255 and c, loop ;


variable DTB-N

\ Lays a DTB row [mask:8][val:8][kind:1][nlen:1][name]; the first row whose mask
\ and value match a word decides, and DROW-EMIT prints each kind's operands.
: ROW, ( n n n ptr u8 n -- )
   {: mask val kind name:ptr u :}
   mask U64,  val U64,  kind c,  u c,
   u 0 ?do name i + c@ c, loop
   1 DTB-N +! ;


: DTB-ROWS ( -- )
   $FF800000 $D2800000 2 s" movz" ROW,
   $FF800000 $F2800000 2 s" movk" ROW,
   $FF800000 $92800000 2 s" movn" ROW,
   $FFE0FC00 $8B000000 0 s" add" ROW,
   $FFE0FC00 $CB000000 0 s" sub" ROW,
   $FFE0FC00 $9B007C00 0 s" mul" ROW,
   $FFE0FC00 $8A000000 0 s" and" ROW,
   $FFE0FC00 $AA000000 0 s" orr" ROW,
   $FFE0FC00 $CA000000 0 s" eor" ROW,
   $FF800000 $91000000 1 s" add" ROW,
   $FF800000 $D1000000 1 s" sub" ROW,
   $FFC00000 $F9400000 3 s" ldr" ROW,
   $FFC00000 $F9000000 3 s" str" ROW,
   $FFE0FC1F $EB00001F 6 s" cmp" ROW,
   $FF80001F $F100001F 8 s" cmp" ROW,
   $FFE00C00 $F8400C00 9 s" ldr-pre" ROW,
   $FFE00C00 $F8400400 9 s" ldr-post" ROW,
   $FFE00C00 $F8000C00 9 s" str-pre" ROW,
   $FFE00C00 $F8000400 9 s" str-post" ROW,
   $FF000010 $54000000 10 s" b." ROW,
   $FF800000 $92000000 11 s" and" ROW,
   $9F000000 $10000000 12 s" adr" ROW,
   $FFFFFC1F $D63F0000 13 s" blr" ROW,
   $FFFFFC1F $D61F0000 13 s" br" ROW,
   $FF000000 $B4000000 14 s" cbz" ROW,
   $FF000000 $B5000000 14 s" cbnz" ROW,
   $FC000000 $14000000 7 s" b" ROW,
   $FC000000 $94000000 7 s" bl" ROW,
   $FFFFFFFF $D65F03C0 4 s" ret" ROW,
   $FFE0001F $D4000001 5 s" svc" ROW, ;

create DTB DTB-ROWS


: U64@ ( ptr u8 -- n ) {: p :}
   p c@  p 1 + c@ 8 lshift or  p 2 + c@ 16 lshift or  p 3 + c@ 24 lshift or
   p 4 + c@ 32 lshift or  p 5 + c@ 40 lshift or  p 6 + c@ 48 lshift or  p 7 + c@ 56 lshift or ;

: U32@ ( ptr u8 -- n ) {: p :}
   p c@  p 1 + c@ 8 lshift or  p 2 + c@ 16 lshift or  p 3 + c@ 24 lshift or ;

: FRD ( n -- n ) {: w :} w 31 and ;

: FRN ( n -- n ) {: w :} w 5 rshift 31 and ;

: FRM ( n -- n ) {: w :} w 16 rshift 31 and ;

: FI12 ( n -- n ) {: w :} w 10 rshift 4095 and ;

: FI16 ( n -- n ) {: w :} w 5 rshift 65535 and ;


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


\ ADR's offset: immhi in bits 23-5 above immlo in bits 30-29.
: FADR ( n -- n )
   {: w :}
   w 5 rshift $7FFFF and 2 lshift  w 29 rshift 3 and or 21 SEXT ;


create CHB 4 allot

: EMITC ( n -- ) {: c :} c CHB c! CHB 1 type ;

: SPC ( -- ) 32 EMITC ;

: DOUT ( n -- ) SPC FMT:.INT ;

: DTRUE ( -- bool ) 0 0= ;

: DFALSE ( -- bool ) DTRUE 0= ;


\ Condition names by bits 3-0, two letters each.
: DCOND ( n -- )
   {: w :}
   s" eqnecsccmiplvsvchilsgeltgtlealnv" drop w 15 and 2 * + 2 type ;


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

: OP? ( n n -- bool ) {: w val :} w $FFC00000 and val = ;


: DLS1 ( n ptr u8 n -- )
   {: w name:ptr u :}
   name u type  w FRD DOUT w FRN DOUT w FI12 DOUT ;


: DLS4 ( n ptr u8 n -- )
   {: w name:ptr u :}
   name u type  w FRD DOUT w FRN DOUT w FI12 4 * DOUT ;


: DLS-SPECIAL ( n -- bool ) {: w :}
   w $39400000 OP? IF w s" ldrb" DLS1 DTRUE EXIT THEN
   w $39000000 OP? IF w s" strb" DLS1 DTRUE EXIT THEN
   w $B9400000 OP? IF w s" ldrw" DLS4 DTRUE EXIT THEN
   w $B9000000 OP? IF w s" strw" DLS4 DTRUE EXIT THEN
   DFALSE ;


\ Branch, ADR and index offsets are signed byte counts.
: DROW-EMIT ( n n -- )
   {: w kind :}
   kind 0 = IF w FRD DOUT w FRN DOUT w FRM DOUT THEN
   kind 1 = IF w FRD DOUT w FRN DOUT w FI12 DOUT THEN
   kind 2 = IF w FRD DOUT w FI16 DOUT THEN
   kind 3 = IF w FRD DOUT w FRN DOUT w FI12 8 * DOUT THEN
   kind 6 = IF w FRN DOUT w FRM DOUT THEN
   kind 7 = IF w FI26 DOUT THEN
   kind 8 = IF w FRN DOUT w FI12 DOUT THEN
   kind 9 = IF w FRD DOUT w FRN DOUT w FI9 DOUT THEN
   kind 10 = IF w DCOND w FI19 DOUT THEN
   kind 11 = IF w FRD DOUT w FRN DOUT w BITMASK drop DOUT THEN
   kind 12 = IF w FRD DOUT w FADR DOUT THEN
   kind 13 = IF w FRN DOUT THEN
   kind 14 = IF w FRD DOUT w FI19 DOUT THEN
   kind 5 = IF w FI16 DOUT THEN ;


: DVALID? ( n n -- bool )
   {: w kind :}
   kind 11 = IF w BITMASK nip ELSE DTRUE THEN ;


: DUNKNOWN ( n -- )
   {: w :}
   63 EMITC SPC  SB-RESET w 8 FMT:SB-HEX-WIDTH SB$ type ;


\ No locals inside the loop (they corrupt the frame): row fields go through variables.
variable DDONE  variable DRI  variable DMSK  variable DVAL  variable DKIND  variable DNL
PTR-VARIABLE DRP

: DRP@ ( -- ptr u8 )
   DRP @ ;


: DIS1 ( n -- )
   {: w :}
   w DLS-SPECIAL IF EXIT THEN
   DTB DRP !
   0 DDONE !
   0 DRI !
   BEGIN DRI @ DTB-N @ < WHILE
     DRP@ U64@ DMSK !  DRP@ 8 + U64@ DVAL !
     DRP@ 16 + c@ DKIND !  DRP@ 17 + c@ DNL !
     DDONE @ 0= w DMSK @ and DVAL @ = and IF
       w DKIND @ DVALID? IF
         DRP@ 18 + DNL @ type  w DKIND @ DROW-EMIT
       ELSE w DUNKNOWN THEN
       1 DDONE !  THEN
     DRP@ 18 + DNL @ + DRP !  DRI @ 1 + DRI !
   REPEAT
   DDONE @ 0= IF w DUNKNOWN THEN ;


: DISASM ( ptr u8 n -- )
   {: a:ptr n :}
   0 BEGIN dup n < WHILE
      a over 4 * + U32@ DIS1 cr
      1 +
   REPEAT drop ;
