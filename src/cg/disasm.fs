\ disasm.fs — a small ARM64 disassembler in Forth covering the instruction set
\ habu emits. Decodes a u32 to readable text — used to verify emitted code (incl.
\ the native Forth's runtime-emitted code) without otool. Mirrors src/cg/asm.fs.
\ It is a debugging aid: accurate for habu's own forms, ".word" for the rest.

require asm.fs

: fld   ( u lo width -- f )  >r  rshift  r> 1 swap lshift 1- and ;
\ sign-extend a width-bit field to a full cell
: signx ( f width -- n )  >r  r@ 1- 1 swap lshift  over and if  -1 r@ lshift or  then  r> drop ;
: (.)   ( n -- )  s>d  tuck dabs <# #s rot sign #> type ;
: .reg  ( n -- )  dup 31 = if drop ." sp" else ." x" (.) then ;
: .#    ( n -- )  ." #" (.) ;

: Rd ( u -- n )  0  5 fld ;   : Rn ( u -- n )  5  5 fld ;   : Rm ( u -- n )  16 5 fld ;

: ddd ( u name$ -- )  type space  dup Rd .reg ." ,"  dup Rn .reg ." ,"  Rm .reg ;
: ddi ( u name$ -- )  type space  dup Rd .reg ." ,"  dup Rn .reg ." ,#"  10 12 fld (.) ;
: .hw  ( u -- )  16 2 fld ?dup if ." ,lsl#" 4 lshift (.) then ;
: d-movz ( u -- )  ." movz "  dup Rd .reg ." ,"  dup 5 16 fld .#  .hw ;
: d-movk ( u -- )  ." movk "  dup Rd .reg ." ,"  dup 5 16 fld .#  .hw ;
: d-movn ( u -- )  ." movn "  dup Rd .reg ." ,"  5 16 fld .# ;
: d-orr  ( u -- )  dup Rn 31 = if ." mov " dup Rd .reg ." ," Rm .reg drop else s" orr" ddd then ;
: d-cmp  ( u -- )  ." cmp "  dup Rn .reg ." ,"  Rm .reg ;
: d-cmpi ( u -- )  ." cmp "  dup Rn .reg ." ,#"  10 12 fld (.) ;
: d-cset ( u -- )  ." cset "  dup Rd .reg ." ,cc"  12 4 fld 1 xor (.) ;
: d-svc  ( u -- )  ." svc #"  5 16 fld (.) ;
: d-bimm ( u name$ -- )  type space ." ."  0 26 fld 26 signx 4 * dup 0< 0= if ." +" then (.) ;
: d-b    ( u -- )  s" b"  d-bimm ;
: d-bl   ( u -- )  s" bl" d-bimm ;
: d-bcond ( u -- ) ." b.cc" dup 0 4 fld (.) ."  .+"  5 19 fld 19 signx 4 * (.) ;
: d-cbz  ( u -- )  ." cbz "  dup Rd .reg ." ,.+"  5 19 fld 19 signx 4 * (.) ;
: d-cbnz ( u -- )  ." cbnz " dup Rd .reg ." ,.+"  5 19 fld 19 signx 4 * (.) ;
: d-blr  ( u -- )  ." blr "  Rn .reg ;
: d-br   ( u -- )  ." br "   Rn .reg ;
: d-ret  ( u -- )  drop ." ret" ;
: d-nop  ( u -- )  drop ." nop" ;
: d-adr  ( u -- )  ." adr "  dup Rd .reg ." ,.+"  dup 5 19 fld 19 over 29 2 fld swap 2 lshift or 21 signx (.) ;
: .mem ( u scale -- )  >r  ." ,["  dup Rn .reg  10 12 fld r> lshift ?dup if ." ,#" (.) then ." ]" ;
: d-ldr  ( u -- )  ." ldr "   dup Rd .reg  3 .mem ;
: d-str  ( u -- )  ." str "   dup Rd .reg  3 .mem ;
: d-ldrb ( u -- )  ." ldrb w"  dup Rd (.)  0 .mem ;
: d-strb ( u -- )  ." strb w"  dup Rd (.)  0 .mem ;
: d-ldrw ( u -- )  ." ldr w"   dup Rd (.)  2 .mem ;
: d-strw ( u -- )  ." str w"   dup Rd (.)  2 .mem ;
: d-?    ( u -- )  ." .word $"  base @ >r hex 0 <# # # # # # # # # #> type r> base ! ;

\ --- dispatch table: records of [mask | match | xt]; first match wins ---
create DTAB 192 cells allot   variable #DT   0 #DT !
: D: ( mask match xt -- )
   #DT @ 3 * cells DTAB +  >r
   r@ 2 cells + !   r@ cell+ !   r> !   1 #DT +! ;
$FF800000 $D2800000 ' d-movz D:    $FF800000 $F2800000 ' d-movk D:
$FF800000 $92800000 ' d-movn D:
$FFE0FC00 $8B000000 [: s" add" ddd ;] D:    $FF800000 $91000000 [: s" add" ddi ;] D:
$FFE0FC00 $CB000000 [: s" sub" ddd ;] D:    $FF800000 $D1000000 [: s" sub" ddi ;] D:
$FFE0FC00 $9B007C00 [: s" mul"  ddd ;] D:   $FFE0FC00 $9AC00C00 [: s" sdiv" ddd ;] D:
$FFE0FC00 $9AC00800 [: s" udiv" ddd ;] D:
$FFE0FC00 $8A000000 [: s" and" ddd ;] D:    $FFE0FC00 $AA000000 ' d-orr D:
$FFE0FC00 $CA000000 [: s" eor" ddd ;] D:
$FFE0FC00 $9AC02000 [: s" lsl" ddd ;] D:    $FFE0FC00 $9AC02400 [: s" lsr" ddd ;] D:
$FFE0FC00 $9AC02800 [: s" asr" ddd ;] D:
$FFE0FC1F $EB00001F ' d-cmp D:     $FF8003FF $F100001F ' d-cmpi D:
$FFE08C00 $9A800400 ' d-cset D:
$FC000000 $14000000 ' d-b D:       $FC000000 $94000000 ' d-bl D:
$FF000010 $54000000 ' d-bcond D:
$FF000000 $B4000000 ' d-cbz D:     $FF000000 $B5000000 ' d-cbnz D:
$FFFFFC1F $D63F0000 ' d-blr D:     $FFFFFC1F $D61F0000 ' d-br D:
$FFFFFFFF $D65F03C0 ' d-ret D:     $FFFFFFFF $D503201F ' d-nop D:
$9F000000 $10000000 ' d-adr D:
$FFC00000 $F9400000 ' d-ldr D:     $FFC00000 $F9000000 ' d-str D:
$FFC00000 $39400000 ' d-ldrb D:    $FFC00000 $39000000 ' d-strb D:
$FFC00000 $B9400000 ' d-ldrw D:    $FFC00000 $B9000000 ' d-strw D:
$FFE0001F $D4000001 ' d-svc D:

: D# ( u -- )                                  \ decode + print one instruction
   #DT @ 0 ?do
      i 3 * cells DTAB +  >r
      dup r@ @ and  r@ cell+ @ =  if  r> 2 cells + @ execute  unloop exit  then
      r> drop
   loop  d-? ;
: DISASM ( addr nwords -- )                    \ dump a code region
   0 ?do  dup i 4 * +  dup ." $" base @ >r hex 0 <# # # # # #> type r> base !
          ."   "  l@ D#  cr  loop  drop ;
