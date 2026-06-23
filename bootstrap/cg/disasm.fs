\ disasm.fs — a small ARM64 disassembler in Forth covering the instruction set
\ habu emits. Decodes a u32 to readable text — used to verify emitted code (incl.
\ the native Forth's runtime-emitted code) without otool. Mirrors bootstrap/cg/asm.fs.
\ It is a debugging aid: accurate for habu's own forms, ".word" for the rest.

require asm.fs

: FLD   ( u lo width -- f )  >r  rshift  r> 1 swap lshift 1- and ;

\ sign-extend a width-bit field to a full cell
: SIGNX ( f width -- n )  >r  r@ 1- 1 swap lshift  over and if  -1 r@ lshift or  then  r> drop ;

: (.)   ( n -- )  s>d  tuck dabs <# #s rot sign #> type ;

: .REG  ( n -- )  dup 31 = if drop ." sp" else ." x" (.) then ;

: .#    ( n -- )  ." #" (.) ;

: RD ( u -- n )  0  5 FLD ;

: RN ( u -- n )  5  5 FLD ;

: RM ( u -- n )  16 5 FLD ;

: DDD ( u name$ -- )  type space  dup RD .REG ." ,"  dup RN .REG ." ,"  RM .REG ;

: DDI ( u name$ -- )  type space  dup RD .REG ." ,"  dup RN .REG ." ,#"  10 12 FLD (.) ;

: .HW  ( u -- )  16 2 FLD ?dup if ." ,lsl#" 4 lshift (.) then ;

: D-MOVZ ( u -- )  ." movz "  dup RD .REG ." ,"  dup 5 16 FLD .#  .HW ;

: D-MOVK ( u -- )  ." movk "  dup RD .REG ." ,"  dup 5 16 FLD .#  .HW ;

: D-MOVN ( u -- )  ." movn "  dup RD .REG ." ,"  5 16 FLD .# ;

: D-ORR  ( u -- )  dup RN 31 = if ." mov " dup RD .REG ." ," RM .REG drop else s" orr" DDD then ;

: D-CMP  ( u -- )  ." cmp "  dup RN .REG ." ,"  RM .REG ;

: D-CMPI ( u -- )  ." cmp "  dup RN .REG ." ,#"  10 12 FLD (.) ;

: D-CSET ( u -- )  ." cset "  dup RD .REG ." ,cc"  12 4 FLD 1 xor (.) ;

: D-SVC  ( u -- )  ." svc #"  5 16 FLD (.) ;

: D-BIMM ( u name$ -- )  type space ." ."  0 26 FLD 26 SIGNX 4 * dup 0< 0= if ." +" then (.) ;

: D-B    ( u -- )  s" b"  D-BIMM ;

: D-BL   ( u -- )  s" bl" D-BIMM ;

: D-BCOND ( u -- ) ." b.cc" dup 0 4 FLD (.) ."  .+"  5 19 FLD 19 SIGNX 4 * (.) ;

: D-CBZ  ( u -- )  ." cbz "  dup RD .REG ." ,.+"  5 19 FLD 19 SIGNX 4 * (.) ;

: D-CBNZ ( u -- )  ." cbnz " dup RD .REG ." ,.+"  5 19 FLD 19 SIGNX 4 * (.) ;

: D-BLR  ( u -- )  ." blr "  RN .REG ;

: D-BR   ( u -- )  ." br "   RN .REG ;

: D-RET  ( u -- )  drop ." ret" ;

: D-NOP  ( u -- )  drop ." nop" ;

: D-ADR  ( u -- )  ." adr "  dup RD .REG ." ,.+"  dup 5 19 FLD 19 over 29 2 FLD swap 2 lshift or 21 SIGNX (.) ;

: .MEM ( u scale -- )  >r  ." ,["  dup RN .REG  10 12 FLD r> lshift ?dup if ." ,#" (.) then ." ]" ;

: D-LDR  ( u -- )  ." ldr "   dup RD .REG  3 .MEM ;

: D-STR  ( u -- )  ." str "   dup RD .REG  3 .MEM ;

: D-LDRB ( u -- )  ." ldrb w"  dup RD (.)  0 .MEM ;

: D-STRB ( u -- )  ." strb w"  dup RD (.)  0 .MEM ;

: D-LDRW ( u -- )  ." ldr w"   dup RD (.)  2 .MEM ;

: D-STRW ( u -- )  ." str w"   dup RD (.)  2 .MEM ;

: D-?    ( u -- )  ." .word $"  base @ >r hex 0 <# # # # # # # # # #> type r> base ! ;

\ --- dispatch table: records of [mask | match | xt]; first match wins ---
create DTAB 192 cells allot   variable #DT   0 #DT !

: D: ( mask match xt -- )
   #DT @ 3 * cells DTAB +  >r
   r@ 2 cells + !   r@ cell+ !   r> !   1 #DT +! ;
$FF800000 $D2800000 ' D-MOVZ D:    $FF800000 $F2800000 ' D-MOVK D:
$FF800000 $92800000 ' D-MOVN D:
$FFE0FC00 $8B000000 [: s" add" DDD ;] D:    $FF800000 $91000000 [: s" add" DDI ;] D:
$FFE0FC00 $CB000000 [: s" sub" DDD ;] D:    $FF800000 $D1000000 [: s" sub" DDI ;] D:
$FFE0FC00 $9B007C00 [: s" mul"  DDD ;] D:   $FFE0FC00 $9AC00C00 [: s" sdiv" DDD ;] D:
$FFE0FC00 $9AC00800 [: s" udiv" DDD ;] D:
$FFE0FC00 $8A000000 [: s" and" DDD ;] D:    $FFE0FC00 $AA000000 ' D-ORR D:
$FFE0FC00 $CA000000 [: s" eor" DDD ;] D:
$FFE0FC00 $9AC02000 [: s" lsl" DDD ;] D:    $FFE0FC00 $9AC02400 [: s" lsr" DDD ;] D:
$FFE0FC00 $9AC02800 [: s" asr" DDD ;] D:
$FFE0FC1F $EB00001F ' D-CMP D:     $FF8003FF $F100001F ' D-CMPI D:
$FFE08C00 $9A800400 ' D-CSET D:
$FC000000 $14000000 ' D-B D:       $FC000000 $94000000 ' D-BL D:
$FF000010 $54000000 ' D-BCOND D:
$FF000000 $B4000000 ' D-CBZ D:     $FF000000 $B5000000 ' D-CBNZ D:
$FFFFFC1F $D63F0000 ' D-BLR D:     $FFFFFC1F $D61F0000 ' D-BR D:
$FFFFFFFF $D65F03C0 ' D-RET D:     $FFFFFFFF $D503201F ' D-NOP D:
$9F000000 $10000000 ' D-ADR D:
$FFC00000 $F9400000 ' D-LDR D:     $FFC00000 $F9000000 ' D-STR D:
$FFC00000 $39400000 ' D-LDRB D:    $FFC00000 $39000000 ' D-STRB D:
$FFC00000 $B9400000 ' D-LDRW D:    $FFC00000 $B9000000 ' D-STRW D:
$FFE0001F $D4000001 ' D-SVC D:

: D# ( u -- )                                  \ decode + print one instruction
   #DT @ 0 ?do
      i 3 * cells DTAB +  >r
      dup r@ @ and  r@ cell+ @ =  if  r> 2 cells + @ execute  unloop exit  then
      r> drop
   loop  D-? ;

: DISASM ( addr nwords -- )                    \ dump a code region
   0 ?do  dup i 4 * +  dup ." $" base @ >r hex 0 <# # # # # #> type r> base !
          ."   "  l@ D#  cr  loop  drop ;
