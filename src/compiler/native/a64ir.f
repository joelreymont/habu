\ a64ir.f - the ARM64 machine dialect: operations that stand for real ARM64
\ instruction forms, over virtual registers as SSA values.

require src/lib/prelude.f
require src/lib/errors.f
require src/lib/string.f
require src/compiler/native-effect.f
require src/compiler/native/machine.f
require src/compiler/target.f
require src/compiler/binding.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/type.f
require src/compiler/ir/schema.f
require src/compiler/ir/symbol.f
require src/compiler/ir/build.f
require src/arch/arm64/asm.f
require src/arch/arm64/backend.f
require src/arch/arm64/machine.f
require src/compiler/native/regfile.f
require src/compiler/native/dialect.f
require src/compiler/native/emission.f

package A64IR
public

ENUM opcode DERIVE eq
   movz
   movk
   mov
   add
   sub
   mul
   sdiv
   and
   orr
   eor
   lslv
   lsrv
   mvn
   store
   load
   reserve
   release
   dtake
   dload
   dstore
   dpublish
   dpush
   dpop
   fdpush
   fdpop
   aload
   astore
   pstore
   pbstore
   abload
   abstore
   flag
   selz
   cmpsel
   br
   brz
   cmpbr
   call
   wordcall
   linksave
   linkload
   ret
   fadd
   fsub
   fmul
   fdiv
   fneg
   fabs
   fsqrt
   scvtf
   fcvtzs
   fmovxd
   fmovdx
   fmovdd
   fmovdi
   fload
   fstore
   faload
   fastore
   fdload
   fdstore
   fflag
   fflagz
   fcmpbr
   fcmpbrz
   selzd
   cmpseld
   fcmpsel
   fcmpselz
   fcmpseld
   fcmpselzd
   tailcall
   trap
   madd
   addi
   subi
   movn
   andi
   orri
   eori
   flagi
   cmpbri
   codeaddr
   dataaddr
   sdivnz
;ENUM

\ `equal`, because DERIVE eq defines `eq`; `mi` serves only the float forms.
ENUM cond DERIVE eq
   lt
   le
   gt
   ge
   equal
   ne
   mi
;ENUM

private

\ ---- the machine bounds ------------------------------------------------------
64 constant XBITS                    \ bits in a general register
16 constant IMM-BITS                 \ the move-wide immediate field
2 constant HW-BITS                   \ the move-wide half selector

1 IMM-BITS lshift constant IMM-LIM   \ a half holds 0 .. IMM-LIM-1
1 HW-BITS lshift constant HALVES-N   \ four selectable halves
XBITS HALVES-N / constant HALF-N     \ bits per half

$FFFF constant HALF-MASK

\ ---- the frame bounds --------------------------------------------------------
XBITS 8 / constant SLOT-BYTES        \ bytes one frame access moves

12 constant OFF-BITS                 \ the add/sub immediate and the offset field
1 OFF-BITS lshift 1- constant OFF-MAX

\ ---- the writeback field -----------------------------------------------------
\ The signed byte count an indexed load or store moves its base register by.
\ A64ASM's ?SIMM9 holds the two encoders to the same nine bits.
9 constant WB-BITS
1 WB-BITS 1- lshift 1- constant WB-MAX   \ the magnitude either sign of it holds

\ ---- the condition field -----------------------------------------------------
\ Fcmp sets N=0 Z=0 C=1 V=1 on a NaN. Against those flags lt, le and ne read
\ true and ge, gt, equal and mi read false, so a float less-than lowers under
\ `mi`, never `lt`: the float comparisons use only mi, gt and equal, and each
\ answers false for a NaN.
4 constant COND-BITS
1 COND-BITS lshift constant COND-LIM
11 constant COND-LT                  \ signed less than
13 constant COND-LE                  \ signed less than or equal
12 constant COND-GT                  \ signed greater than
10 constant COND-GE                  \ signed greater than or equal
0 constant COND-EQ                   \ equal
1 constant COND-NE                   \ not equal
4 constant COND-MI                   \ negative - less than, after an Fcmp

\ ---- the branch fields -------------------------------------------------------
26 constant B-BITS
19 constant BZ-BITS
19 constant BCOND-BITS

21 constant ADR-BITS

\ Every instruction is four bytes, which is why every displacement field above
\ counts instructions rather than bytes.
4 constant INSN-BYTES

\ ---- the dialect's own symbols -----------------------------------------------

: TARGET ( -- )
   CTARGET-ARCH:AARCH64 CTARGET:F-BASE IR-SCHEMA:SET-TARGET ;

\ A machine without a floating unit cannot hold these schemas at all.
: FP-TARGET ( -- )
   CTARGET-ARCH:AARCH64 CTARGET:F-BASE CTARGET:F-SCALAR-FP CTARGET:WITH
   IR-SCHEMA:SET-TARGET ;

: PURE-VALUE ( -- )
   false 0 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE ;

: TOTAL ( -- )
   false IR-SCHEMA:SET-TRAP ;

public

\ ---- the dialect identity ----------------------------------------------------
: NAME ( -- ptr u8 n )
   s" a64" ;

\ Every consumer compares the version exactly, so a table with a form and one
\ without are two different tables.
0 constant MAJOR
19 constant MINOR

\ ---- the machine bounds, for a consumer that has to agree with them -----------
: HALVES ( -- n )        HALVES-N ;
: HALF-BITS ( -- n )     HALF-N ;
: IMM-LIMIT ( -- n )     IMM-LIM ;

\ The shift is logical, so a negative value reads as the bit pattern the machine
\ holds, which is what a move-wide chain has to reproduce.
: HALF-OF ( n n -- n )
   {: v:n i:n :}
   i 0 < i HALVES-N >= or if E-A64IR-SHIFT throw then
   v i HALF-N * rshift HALF-MASK and ;

: HALF-SHIFT ( n -- n )
   {: i:n :}
   i 0 < i HALVES-N >= or if E-A64IR-SHIFT throw then
   i HALF-N * ;

\ VFPExpandImm for binary64 has a zero low 48-bit fraction and exponent
\ 0x3fc..0x403. The top exponent bits encode the immediate's bit 6; the
\ remaining exponent and fraction bits occupy its low six bits.
1 48 lshift 1- constant FIMM-LOW-MASK
$3FC constant FIMM-EXP-MIN
$403 constant FIMM-EXP-MAX

: FIMM8? ( n -- n bool )
   {: bits:n :}
   bits FIMM-LOW-MASK and 0<> if 0 false exit then
   bits 52 rshift $7FF and {: e:n :}
   e FIMM-EXP-MIN < e FIMM-EXP-MAX > or if 0 false exit then
   bits 56 rshift $80 and
   bits 55 rshift $40 and or
   bits 48 rshift $3F and or true ;

: DATA-OFFSET ( n -- n )
   dup 0 < over DATA-SIZE > or
   over DATA-VA VA>N + $FFFFFFFFFFFF > or if E-A64IR-DATA throw then ;

\ A number, or one of NEMIT's address-site kinds.
0 constant ADDR-NONE

private

\ ---- checked move-wide operands ----------------------------------------------
: IMM16 ( n -- n )
   dup 0 < over IMM-LIM >= or if E-A64IR-IMM throw then ;

: FIMM8 ( n -- n )
   dup 0 < over 256 >= or if E-A64IR-IMM throw then ;

: HALF ( n -- n )
   dup 0 < over XBITS >= or if E-A64IR-SHIFT throw then
   dup HALF-N mod 0<> if E-A64IR-SHIFT throw then ;

\ An unknown number would reach the emitter as a site class it has no rule for.
: ADDR-KIND ( n -- n )
   dup 0 < over NEMIT:ADDR-KIND-MAX > or if E-A64IR-IMM throw then ;

\ ---- checked frame operands --------------------------------------------------
: SLOT ( n -- n )
   dup 0 < if E-A64IR-SLOT throw then
   dup SLOT-BYTES mod 0<> if E-A64IR-SLOT throw then
   dup SLOT-BYTES A64M:SLOT-REACH > if E-A64IR-SLOT throw then ;

\ Keeps the stack pointer aligned and the frame inside the region NEFF can
\ describe. Past one add/sub immediate the emitter moves the pointer by whole
\ pages (src/compiler/native/emit.f WORD-RESERVE).
: FRAME ( n -- n )
   dup 0 < if E-A64IR-FRAME throw then
   dup A64M:SP-ALIGN mod 0<> if E-A64IR-FRAME throw then
   dup A64M:FRAME-MAX > if E-A64IR-FRAME throw then ;

\ Twelve bits, UNSIGNED, with the shift bit hardwired to zero, so a negative
\ immediate is a value these forms cannot express - `cmp rn, #-k` is `cmn`.
: OFF ( n -- n )
   dup 0 < if E-A64IR-OFF throw then
   dup OFF-MAX > if E-A64IR-OFF throw then ;

\ Not a range: the logical immediate is RECONSTRUCTED from a thirteen-bit
\ description, so the packer is asked rather than having its rule restated.
: MASK ( n -- n )
   dup A64ASM:LIMM? 0= if E-A64IR-MASK throw then ;

\ ---- checked data-stack operands ---------------------------------------------
: DSLOT ( n -- n )
   dup A64M:SLOT-BACK negate < if E-A64IR-DSLOT throw then
   dup SLOT-BYTES mod 0<> if E-A64IR-DSLOT throw then
   dup SLOT-BYTES A64M:SLOT-REACH > if E-A64IR-DSLOT throw then ;

\ A whole number of cells, and the same twelve-bit field serves the Add and the
\ Sub, so what is bounded is the MAGNITUDE. The data stack is cell-aligned.
: DBYTES ( n -- n )
   dup SLOT-BYTES mod 0<> if E-A64IR-DBYTES throw then
   dup abs OFF-MAX > if E-A64IR-DBYTES throw then ;

\ The base-register writeback of a load or store: nine signed bits of bytes,
\ narrower than the add/sub immediate. The post-indexed store moves by +n and
\ the pre-indexed load by -n, so the bound is on the magnitude. Zero is refused:
\ a zero move is the plain access, which the selector emits instead.
: DWB ( n -- n )
   dup 0= if E-A64IR-DWB throw then
   dup SLOT-BYTES mod 0<> if E-A64IR-DWB throw then
   dup abs WB-MAX > if E-A64IR-DWB throw then ;

\ How far away it is is not asked here: the distance depends on where the
\ CALLING routine is written, so the reach stays the emitter's.
: ENTRY ( n -- n )
   dup 0 <= if E-A64IR-ENTRY throw then
   dup INSN-BYTES mod 0<> if E-A64IR-ENTRY throw then ;

\ ---- the checked function ordinal --------------------------------------------
: FUN-ORD ( n -- n )
   dup 0 < if E-A64IR-FUN throw then ;

\ ---- the checked condition operand -------------------------------------------
: COND ( n -- n )
   dup 0 < over COND-LIM >= or if E-A64IR-COND throw then ;

\ ---- the checked branch displacement -----------------------------------------
: FITS? ( n n -- bool )
   {: d:n bits:n :}
   1 bits 1- lshift {: half:n :}
   d half negate >= d half < and ;

public

\ ---- the machine this compilation is for --------------------------------------
: CONTRACT@ ( IR-CTX:ctx -- CTARGET:contract )
   IR-CTX:BINDING@ CBIND:VALIDATE CBIND:TARGET@ ;

\ A coherent foreign target can own HIR. This dialect's pure provider
\ predicate declines it before allocating, even before full pass registration.
: CHECK-TARGET ( IR-CTX:ctx -- )
   CONTRACT@ CTARGET:VALIDATE A64BACK:SERVES? 0= if E-IR-SCHEMA-TARGET throw then ;

: GPR-TYPE ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )
   IR--TYPE-WIDTH:W64 IR--TYPE-SIGN:SIGNED IR-BUILD:INTERN-INT ;

\ ---- the type of the memory token --------------------------------------------
: MEM-TYPE ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )
   IR--TYPE-DOMAIN:DATA-MEM IR-BUILD:INTERN-TOKEN ;

\ The machine really has two register files: an instruction naming a D register
\ cannot name an X register in the same field.
: FPR-TYPE ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )
   IR--TYPE-FMT:DOUBLE IR-BUILD:INTERN-FLT ;

\ ---- the bytes one frame access moves ----------------------------------------
: SLOT-WIDTH ( -- n )    SLOT-BYTES ;

\ Lets a pass about A64IR get the machine description without naming an
\ architecture.
: MACHINE ( -- NMACH:mach )   A64M:MACHINE ;

\ ---- this machine's register files, for the passes that are not about it ------
\ For the linear-scan allocator, which is not an ARM64 pass. Every field is
\ derived: file size and reserved set from src/compiler/native-effect.f (which
\ folds in x18 on Darwin and the registers src/habu/layout.f reserves), slot
\ width from SLOT-WIDTH. Every allocatable register is call-destroyed because
\ src/compiler/native/abi.f declares the whole pool destroyed on every routine.
: REGFILE ( -- NREGFILE:file )
   MACHINE NMACH:REGFILE ;

: OFF-LIMIT ( -- n )     OFF-MAX ;

\ A pass CHOOSING between the immediate and the register form asks this; the
\ bound at MASK is what makes a pass that forgot fail loudly.
: MASK-IMM? ( n -- bool )   A64ASM:LIMM? ;

\ The same question for the two fused forms: a selector CHOOSES between one
\ transfer that carries the move and the two operations that do not, and DWB is
\ what makes a selector that chose wrong fail loudly.
: WRITEBACK? ( n -- bool )
   {: d:n :}
   d 0= if false exit then
   d SLOT-BYTES mod 0<> if false exit then
   d abs WB-MAX <= ;

\ ---- the closed opcode vocabulary -------------------------------------------
\ These ordinals predate the enum declaration order and are kept stable for the
\ native passes that store them in their own tables.
85 constant OPCODES

: ORD ( A64IR:opcode -- n )
   MATCH opcode
      movz      OF 0  ENDOF
      movk      OF 1  ENDOF
      mov       OF 2  ENDOF
      add       OF 3  ENDOF
      sub       OF 4  ENDOF
      mul       OF 5  ENDOF
      store     OF 6  ENDOF
      load      OF 7  ENDOF
      reserve   OF 8  ENDOF
      release   OF 9  ENDOF
      dtake     OF 10 ENDOF
      dload     OF 11 ENDOF
      dstore    OF 12 ENDOF
      dpublish  OF 13 ENDOF
      flag      OF 14 ENDOF
      br        OF 15 ENDOF
      brz       OF 16 ENDOF
      ret       OF 17 ENDOF
      aload     OF 18 ENDOF
      astore    OF 19 ENDOF
      sdiv      OF 20 ENDOF
      abload    OF 21 ENDOF
      abstore   OF 22 ENDOF
      call      OF 23 ENDOF
      linksave  OF 24 ENDOF
      linkload  OF 25 ENDOF
      cmpbr     OF 26 ENDOF
      wordcall  OF 27 ENDOF
      and       OF 28 ENDOF
      orr       OF 29 ENDOF
      eor       OF 30 ENDOF
      lslv      OF 31 ENDOF
      lsrv      OF 32 ENDOF
      mvn       OF 33 ENDOF
      fadd      OF 34 ENDOF
      fsub      OF 35 ENDOF
      fmul      OF 36 ENDOF
      fdiv      OF 37 ENDOF
      fneg      OF 38 ENDOF
      fabs      OF 39 ENDOF
      fsqrt     OF 40 ENDOF
      scvtf     OF 41 ENDOF
      fcvtzs    OF 42 ENDOF
      fmovxd    OF 43 ENDOF
      fmovdx    OF 44 ENDOF
      fflag     OF 45 ENDOF
      fflagz    OF 46 ENDOF
      fcmpbr    OF 47 ENDOF
      fcmpbrz   OF 48 ENDOF
      fmovdd    OF 49 ENDOF
      selz      OF 50 ENDOF
      cmpsel    OF 51 ENDOF
      selzd     OF 52 ENDOF
      cmpseld   OF 53 ENDOF
      fcmpsel   OF 54 ENDOF
      fcmpselz  OF 55 ENDOF
      fcmpseld  OF 56 ENDOF
      fcmpselzd OF 57 ENDOF
      tailcall  OF 58 ENDOF
      madd      OF 59 ENDOF
      addi      OF 60 ENDOF
      subi      OF 61 ENDOF
      movn      OF 62 ENDOF
      andi      OF 63 ENDOF
      orri      OF 64 ENDOF
      eori      OF 65 ENDOF
      fload     OF 66 ENDOF
      fstore    OF 67 ENDOF
      faload    OF 68 ENDOF
      fastore   OF 69 ENDOF
      fdload    OF 70 ENDOF
      fdstore   OF 71 ENDOF
      trap      OF 72 ENDOF
      codeaddr  OF 73 ENDOF
      flagi     OF 74 ENDOF
      cmpbri    OF 75 ENDOF
      dpush     OF 76 ENDOF
      dpop      OF 77 ENDOF
      fdpush    OF 78 ENDOF
      fdpop     OF 79 ENDOF
      dataaddr  OF 80 ENDOF
      sdivnz    OF 81 ENDOF
      fmovdi    OF 82 ENDOF
      pstore    OF 83 ENDOF
      pbstore   OF 84 ENDOF
   ;MATCH ;

: NTH ( n -- A64IR:opcode )
   case
      0  of A64IR-OPCODE:MOVZ      endof
      1  of A64IR-OPCODE:MOVK      endof
      2  of A64IR-OPCODE:MOV       endof
      3  of A64IR-OPCODE:ADD       endof
      4  of A64IR-OPCODE:SUB       endof
      5  of A64IR-OPCODE:MUL       endof
      6  of A64IR-OPCODE:STORE     endof
      7  of A64IR-OPCODE:LOAD      endof
      8  of A64IR-OPCODE:RESERVE   endof
      9  of A64IR-OPCODE:RELEASE   endof
      10 of A64IR-OPCODE:DTAKE     endof
      11 of A64IR-OPCODE:DLOAD     endof
      12 of A64IR-OPCODE:DSTORE    endof
      13 of A64IR-OPCODE:DPUBLISH  endof
      14 of A64IR-OPCODE:FLAG      endof
      15 of A64IR-OPCODE:BR        endof
      16 of A64IR-OPCODE:BRZ       endof
      17 of A64IR-OPCODE:RET       endof
      18 of A64IR-OPCODE:ALOAD     endof
      19 of A64IR-OPCODE:ASTORE    endof
      20 of A64IR-OPCODE:SDIV      endof
      21 of A64IR-OPCODE:ABLOAD    endof
      22 of A64IR-OPCODE:ABSTORE   endof
      23 of A64IR-OPCODE:CALL      endof
      24 of A64IR-OPCODE:LINKSAVE  endof
      25 of A64IR-OPCODE:LINKLOAD  endof
      26 of A64IR-OPCODE:CMPBR     endof
      27 of A64IR-OPCODE:WORDCALL  endof
      28 of A64IR-OPCODE:AND       endof
      29 of A64IR-OPCODE:ORR       endof
      30 of A64IR-OPCODE:EOR       endof
      31 of A64IR-OPCODE:LSLV      endof
      32 of A64IR-OPCODE:LSRV      endof
      33 of A64IR-OPCODE:MVN       endof
      34 of A64IR-OPCODE:FADD      endof
      35 of A64IR-OPCODE:FSUB      endof
      36 of A64IR-OPCODE:FMUL      endof
      37 of A64IR-OPCODE:FDIV      endof
      38 of A64IR-OPCODE:FNEG      endof
      39 of A64IR-OPCODE:FABS      endof
      40 of A64IR-OPCODE:FSQRT     endof
      41 of A64IR-OPCODE:SCVTF     endof
      42 of A64IR-OPCODE:FCVTZS    endof
      43 of A64IR-OPCODE:FMOVXD    endof
      44 of A64IR-OPCODE:FMOVDX    endof
      45 of A64IR-OPCODE:FFLAG     endof
      46 of A64IR-OPCODE:FFLAGZ    endof
      47 of A64IR-OPCODE:FCMPBR    endof
      48 of A64IR-OPCODE:FCMPBRZ   endof
      49 of A64IR-OPCODE:FMOVDD    endof
      50 of A64IR-OPCODE:SELZ      endof
      51 of A64IR-OPCODE:CMPSEL    endof
      52 of A64IR-OPCODE:SELZD     endof
      53 of A64IR-OPCODE:CMPSELD   endof
      54 of A64IR-OPCODE:FCMPSEL   endof
      55 of A64IR-OPCODE:FCMPSELZ  endof
      56 of A64IR-OPCODE:FCMPSELD  endof
      57 of A64IR-OPCODE:FCMPSELZD endof
      58 of A64IR-OPCODE:TAILCALL  endof
      59 of A64IR-OPCODE:MADD      endof
      60 of A64IR-OPCODE:ADDI      endof
      61 of A64IR-OPCODE:SUBI      endof
      62 of A64IR-OPCODE:MOVN      endof
      63 of A64IR-OPCODE:ANDI      endof
      64 of A64IR-OPCODE:ORRI      endof
      65 of A64IR-OPCODE:EORI      endof
      66 of A64IR-OPCODE:FLOAD     endof
      67 of A64IR-OPCODE:FSTORE    endof
      68 of A64IR-OPCODE:FALOAD    endof
      69 of A64IR-OPCODE:FASTORE   endof
      70 of A64IR-OPCODE:FDLOAD    endof
      71 of A64IR-OPCODE:FDSTORE   endof
      72 of A64IR-OPCODE:TRAP      endof
      73 of A64IR-OPCODE:CODEADDR  endof
      74 of A64IR-OPCODE:FLAGI     endof
      75 of A64IR-OPCODE:CMPBRI    endof
      76 of A64IR-OPCODE:DPUSH     endof
      77 of A64IR-OPCODE:DPOP      endof
      78 of A64IR-OPCODE:FDPUSH    endof
      79 of A64IR-OPCODE:FDPOP     endof
      80 of A64IR-OPCODE:DATAADDR  endof
      81 of A64IR-OPCODE:SDIVNZ    endof
      82 of A64IR-OPCODE:FMOVDI    endof
      83 of A64IR-OPCODE:PSTORE    endof
      84 of A64IR-OPCODE:PBSTORE   endof
      E-A64IR-OPCODE throw
   endcase ;

private

\ ---- the opcode names --------------------------------------------------------
\ Three exact immutable spans per stable ordinal: name, rule and renderer.
\ The typed opcode selects its row through ORD, never through enum TAG.
255 PTR-U8-TABLE TEXT-A
255 TYPED-BUFFER TEXT-U n

: TEXT! ( ptr u8 n n -- ) {: a:ptr u:n idx:n :}
   u idx TEXT-U !
   TEXT-A idx ptr-field {: cell:ptr :}
   cell ptr-cell-mark
   a cell ! ;

: NAMES! ( ptr u8 n ptr u8 n ptr u8 n A64IR:opcode -- )
   {: name:ptr nu:n rule:ptr ru:n render:ptr eu:n op:A64IR:opcode :}
   op ORD 3 * {: idx:n :}
   name nu idx TEXT!
   rule ru idx 1+ TEXT!
   render eu idx 2 + TEXT! ;

s" a64.movz" s" a64.rule.movz" s" a64.render.movz" A64IR-OPCODE:MOVZ NAMES!
s" a64.movk" s" a64.rule.movk" s" a64.render.movk" A64IR-OPCODE:MOVK NAMES!
s" a64.mov" s" a64.rule.mov" s" a64.render.mov" A64IR-OPCODE:MOV NAMES!
s" a64.add" s" a64.rule.add" s" a64.render.add" A64IR-OPCODE:ADD NAMES!
s" a64.sub" s" a64.rule.sub" s" a64.render.sub" A64IR-OPCODE:SUB NAMES!
s" a64.mul" s" a64.rule.mul" s" a64.render.mul" A64IR-OPCODE:MUL NAMES!
s" a64.sdiv" s" a64.rule.sdiv" s" a64.render.sdiv" A64IR-OPCODE:SDIV NAMES!
s" a64.and" s" a64.rule.and" s" a64.render.and" A64IR-OPCODE:AND NAMES!
s" a64.orr" s" a64.rule.orr" s" a64.render.orr" A64IR-OPCODE:ORR NAMES!
s" a64.eor" s" a64.rule.eor" s" a64.render.eor" A64IR-OPCODE:EOR NAMES!
s" a64.lslv" s" a64.rule.lslv" s" a64.render.lslv" A64IR-OPCODE:LSLV NAMES!
s" a64.lsrv" s" a64.rule.lsrv" s" a64.render.lsrv" A64IR-OPCODE:LSRV NAMES!
s" a64.mvn" s" a64.rule.mvn" s" a64.render.mvn" A64IR-OPCODE:MVN NAMES!
s" a64.str" s" a64.rule.str" s" a64.render.str" A64IR-OPCODE:STORE NAMES!
s" a64.ldr" s" a64.rule.ldr" s" a64.render.ldr" A64IR-OPCODE:LOAD NAMES!
s" a64.reserve" s" a64.rule.reserve" s" a64.render.reserve" A64IR-OPCODE:RESERVE NAMES!
s" a64.release" s" a64.rule.release" s" a64.render.release" A64IR-OPCODE:RELEASE NAMES!
s" a64.dtake" s" a64.rule.dtake" s" a64.render.dtake" A64IR-OPCODE:DTAKE NAMES!
s" a64.dload" s" a64.rule.dload" s" a64.render.dload" A64IR-OPCODE:DLOAD NAMES!
s" a64.dstore" s" a64.rule.dstore" s" a64.render.dstore" A64IR-OPCODE:DSTORE NAMES!
s" a64.dpublish" s" a64.rule.dpublish" s" a64.render.dpublish" A64IR-OPCODE:DPUBLISH NAMES!
s" a64.dpush" s" a64.rule.dpush" s" a64.render.dpush" A64IR-OPCODE:DPUSH NAMES!
s" a64.dpop" s" a64.rule.dpop" s" a64.render.dpop" A64IR-OPCODE:DPOP NAMES!
s" a64.fdpush" s" a64.rule.fdpush" s" a64.render.fdpush" A64IR-OPCODE:FDPUSH NAMES!
s" a64.fdpop" s" a64.rule.fdpop" s" a64.render.fdpop" A64IR-OPCODE:FDPOP NAMES!
s" a64.aldr" s" a64.rule.aldr" s" a64.render.aldr" A64IR-OPCODE:ALOAD NAMES!
s" a64.astr" s" a64.rule.astr" s" a64.render.astr" A64IR-OPCODE:ASTORE NAMES!
s" a64.pstore" s" a64.rule.pstore" s" a64.render.pstore" A64IR-OPCODE:PSTORE NAMES!
s" a64.pbstore" s" a64.rule.pbstore" s" a64.render.pbstore" A64IR-OPCODE:PBSTORE NAMES!
s" a64.aldrb" s" a64.rule.aldrb" s" a64.render.aldrb" A64IR-OPCODE:ABLOAD NAMES!
s" a64.astrb" s" a64.rule.astrb" s" a64.render.astrb" A64IR-OPCODE:ABSTORE NAMES!
s" a64.flag" s" a64.rule.flag" s" a64.render.flag" A64IR-OPCODE:FLAG NAMES!
s" a64.selz" s" a64.rule.selz" s" a64.render.selz" A64IR-OPCODE:SELZ NAMES!
s" a64.cmpsel" s" a64.rule.cmpsel" s" a64.render.cmpsel" A64IR-OPCODE:CMPSEL NAMES!
s" a64.b" s" a64.rule.b" s" a64.render.b" A64IR-OPCODE:BR NAMES!
s" a64.cbz" s" a64.rule.cbz" s" a64.render.cbz" A64IR-OPCODE:BRZ NAMES!
s" a64.cmpbr" s" a64.rule.cmpbr" s" a64.render.cmpbr" A64IR-OPCODE:CMPBR NAMES!
s" a64.call" s" a64.rule.call" s" a64.render.call" A64IR-OPCODE:CALL NAMES!
s" a64.wordcall" s" a64.rule.wordcall" s" a64.render.wordcall" A64IR-OPCODE:WORDCALL NAMES!
s" a64.lnkstr" s" a64.rule.lnkstr" s" a64.render.lnkstr" A64IR-OPCODE:LINKSAVE NAMES!
s" a64.lnkldr" s" a64.rule.lnkldr" s" a64.render.lnkldr" A64IR-OPCODE:LINKLOAD NAMES!
s" a64.ret" s" a64.rule.ret" s" a64.render.ret" A64IR-OPCODE:RET NAMES!
s" a64.fadd" s" a64.rule.fadd" s" a64.render.fadd" A64IR-OPCODE:FADD NAMES!
s" a64.fsub" s" a64.rule.fsub" s" a64.render.fsub" A64IR-OPCODE:FSUB NAMES!
s" a64.fmul" s" a64.rule.fmul" s" a64.render.fmul" A64IR-OPCODE:FMUL NAMES!
s" a64.fdiv" s" a64.rule.fdiv" s" a64.render.fdiv" A64IR-OPCODE:FDIV NAMES!
s" a64.fneg" s" a64.rule.fneg" s" a64.render.fneg" A64IR-OPCODE:FNEG NAMES!
s" a64.fabs" s" a64.rule.fabs" s" a64.render.fabs" A64IR-OPCODE:FABS NAMES!
s" a64.fsqrt" s" a64.rule.fsqrt" s" a64.render.fsqrt" A64IR-OPCODE:FSQRT NAMES!
s" a64.scvtf" s" a64.rule.scvtf" s" a64.render.scvtf" A64IR-OPCODE:SCVTF NAMES!
s" a64.fcvtzs" s" a64.rule.fcvtzs" s" a64.render.fcvtzs" A64IR-OPCODE:FCVTZS NAMES!
s" a64.fmovxd" s" a64.rule.fmovxd" s" a64.render.fmovxd" A64IR-OPCODE:FMOVXD NAMES!
s" a64.fmovdx" s" a64.rule.fmovdx" s" a64.render.fmovdx" A64IR-OPCODE:FMOVDX NAMES!
s" a64.fmovdd" s" a64.rule.fmovdd" s" a64.render.fmovdd" A64IR-OPCODE:FMOVDD NAMES!
s" a64.fmovdi" s" a64.rule.fmovdi" s" a64.render.fmovdi" A64IR-OPCODE:FMOVDI NAMES!
s" a64.fldr" s" a64.rule.fldr" s" a64.render.fldr" A64IR-OPCODE:FLOAD NAMES!
s" a64.fstr" s" a64.rule.fstr" s" a64.render.fstr" A64IR-OPCODE:FSTORE NAMES!
s" a64.faldr" s" a64.rule.faldr" s" a64.render.faldr" A64IR-OPCODE:FALOAD NAMES!
s" a64.fastr" s" a64.rule.fastr" s" a64.render.fastr" A64IR-OPCODE:FASTORE NAMES!
s" a64.fdload" s" a64.rule.fdload" s" a64.render.fdload" A64IR-OPCODE:FDLOAD NAMES!
s" a64.fdstore" s" a64.rule.fdstore" s" a64.render.fdstore" A64IR-OPCODE:FDSTORE NAMES!
s" a64.fflag" s" a64.rule.fflag" s" a64.render.fflag" A64IR-OPCODE:FFLAG NAMES!
s" a64.fflagz" s" a64.rule.fflagz" s" a64.render.fflagz" A64IR-OPCODE:FFLAGZ NAMES!
s" a64.fcmpbr" s" a64.rule.fcmpbr" s" a64.render.fcmpbr" A64IR-OPCODE:FCMPBR NAMES!
s" a64.fcmpbrz" s" a64.rule.fcmpbrz" s" a64.render.fcmpbrz" A64IR-OPCODE:FCMPBRZ NAMES!
s" a64.selzd" s" a64.rule.selzd" s" a64.render.selzd" A64IR-OPCODE:SELZD NAMES!
s" a64.cmpseld" s" a64.rule.cmpseld" s" a64.render.cmpseld" A64IR-OPCODE:CMPSELD NAMES!
s" a64.fcmpsel" s" a64.rule.fcmpsel" s" a64.render.fcmpsel" A64IR-OPCODE:FCMPSEL NAMES!
s" a64.fcmpselz" s" a64.rule.fcmpselz" s" a64.render.fcmpselz" A64IR-OPCODE:FCMPSELZ NAMES!
s" a64.fcmpseld" s" a64.rule.fcmpseld" s" a64.render.fcmpseld" A64IR-OPCODE:FCMPSELD NAMES!
s" a64.fcmpselzd" s" a64.rule.fcmpselzd" s" a64.render.fcmpselzd" A64IR-OPCODE:FCMPSELZD NAMES!
s" a64.tailcall" s" a64.rule.tailcall" s" a64.render.tailcall" A64IR-OPCODE:TAILCALL NAMES!
s" a64.trap" s" a64.rule.trap" s" a64.render.trap" A64IR-OPCODE:TRAP NAMES!
s" a64.madd" s" a64.rule.madd" s" a64.render.madd" A64IR-OPCODE:MADD NAMES!
s" a64.addi" s" a64.rule.addi" s" a64.render.addi" A64IR-OPCODE:ADDI NAMES!
s" a64.subi" s" a64.rule.subi" s" a64.render.subi" A64IR-OPCODE:SUBI NAMES!
s" a64.movn" s" a64.rule.movn" s" a64.render.movn" A64IR-OPCODE:MOVN NAMES!
s" a64.andi" s" a64.rule.andi" s" a64.render.andi" A64IR-OPCODE:ANDI NAMES!
s" a64.orri" s" a64.rule.orri" s" a64.render.orri" A64IR-OPCODE:ORRI NAMES!
s" a64.eori" s" a64.rule.eori" s" a64.render.eori" A64IR-OPCODE:EORI NAMES!
s" a64.flagi" s" a64.rule.flagi" s" a64.render.flagi" A64IR-OPCODE:FLAGI NAMES!
s" a64.cmpbri" s" a64.rule.cmpbri" s" a64.render.cmpbri" A64IR-OPCODE:CMPBRI NAMES!
s" a64.codeaddr" s" a64.rule.codeaddr" s" a64.render.codeaddr" A64IR-OPCODE:CODEADDR NAMES!
s" a64.dataaddr" s" a64.rule.dataaddr" s" a64.render.dataaddr" A64IR-OPCODE:DATAADDR NAMES!
s" a64.sdivnz" s" a64.rule.sdivnz" s" a64.render.sdivnz" A64IR-OPCODE:SDIVNZ NAMES!

: TEXT@ ( n -- ptr u8 n ) {: idx:n :}
   idx TEXT-U @ TEXT-A idx ptr-field @ swap ;

: OP-NAME ( A64IR:opcode -- ptr u8 n )
   ORD 3 * TEXT@ ;

private

\ ---- the dialect's fixed attribute keys --------------------------------------
\ Enumerable: a session prototype interns this vocabulary by walking it.
0 constant K-IMM
1 constant K-SHIFT
2 constant K-ADDR
3 constant K-SLOT
4 constant K-FRAME
5 constant K-OFF
6 constant K-MASK
7 constant K-DSLOT
8 constant K-DBYTES
9 constant K-DBACK
10 constant K-ENTRY
11 constant K-TRAP-ENTRY
12 constant K-FUN
13 constant K-COND
14 constant K-DWB
15 constant K-COLD-ENTRY
16 constant K-DATA-OFFSET
17 constant K-ORIGIN
18 constant K-TAIL-IN
19 constant K-TAIL-OUT
20 constant K-GUARD-ENTRY
21 constant K-REMAT-MARK
22 constant KEYS

: KEY-NAME ( n -- ptr u8 n )
   case
      K-IMM        of s" a64.imm" endof
      K-SHIFT      of s" a64.shift" endof
      K-ADDR       of s" a64.addr" endof
      K-SLOT       of s" a64.slot" endof
      K-FRAME      of s" a64.frame" endof
      K-OFF        of s" a64.off" endof
      K-MASK       of s" a64.mask" endof
      K-DSLOT      of s" a64.dslot" endof
      K-DBYTES     of s" a64.dbytes" endof
      K-DBACK      of s" a64.dback" endof
      K-ENTRY      of s" a64.entry" endof
      K-TRAP-ENTRY of s" a64.trap-entry" endof
      K-FUN        of s" a64.fun" endof
      K-COND       of s" a64.cond" endof
      K-DWB        of s" a64.dwb" endof
      K-COLD-ENTRY of s" a64.cold-entry" endof
      K-DATA-OFFSET of s" a64.data-offset" endof
      K-ORIGIN of s" a64.origin" endof
      K-TAIL-IN of s" a64.tail-in" endof
      K-TAIL-OUT of s" a64.tail-out" endof
      K-GUARD-ENTRY of s" a64.guard-entry" endof
      K-REMAT-MARK of s" a64.remat-mark" endof
      E-A64IR-DIALECT throw
   endcase ;

\ ---- the vocabulary memo -----------------------------------------------------
\ The memo holds one ordinal per vocabulary entry (opcodes, then keys) and the
\ module they belong to; the module half of a symbol identity changes per
\ definition. A hit mints with IR-ID:PACK-SYMBOL and still passes
\ IR-BUILD:SYMBOL-CK, so stale, frozen and foreign builders are refused. A
\ module cloned from the session prototype adopts the whole memo at birth; any
\ other starts empty. Keys are memoised because re-interning them was the
\ largest single cost of building one definition's IR.
OPCODES KEYS + constant VOCAB        \ memo entries: the opcodes, then the keys
VOCAB TYPED-BUFFER PROTO-ORD n       \ ordinals in the session prototype
VOCAB TYPED-BUFFER MEMO-ORD n        \ ordinals in the module the memo names
VOCAB TYPED-BUFFER MEMO-SEEN bool
1 TYPED-BUFFER MEMO-MOD IR-ID:ir-module-id
variable MEMO-OWNED                  \ MEMO-MOD names a module

: MEMO-FORGET ( -- )
   0 MEMO-OWNED !
   VOCAB 0 ?do false i MEMO-SEEN ! loop ;
MEMO-FORGET

\ Whose ordinals the memo holds. Answering no is the per-hit owner check: the
\ ordinals of another module name other spellings, or nothing at all.
: MEMO-MINE? ( IR-ID:ir-module-id -- bool )
   MEMO-OWNED @ 0= if drop false exit then
   0 MEMO-MOD @ IR-ID:MODULE-SAME? ;

: MEMO-START ( IR-ID:ir-module-id -- )
   0 MEMO-MOD !
   1 MEMO-OWNED !
   VOCAB 0 ?do false i MEMO-SEEN ! loop ;

: MEMO-ADOPT ( IR-BUILD:builder -- )
   IR-BUILD:MODULE@ 0 MEMO-MOD !
   1 MEMO-OWNED !
   VOCAB 0 ?do
      i PROTO-ORD @ i MEMO-ORD !
      true i MEMO-SEEN !
   loop ;

\ The spelling a memo entry names, in the order PROTOTYPE walked and recorded.
: VOCAB-NAME ( n -- ptr u8 n )
   {: i:n :}
   i OPCODES < if i NTH OP-NAME exit then
   i OPCODES - KEY-NAME ;

: MEMO-BIND ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-symbol-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder idx:n :}
   b IR-BUILD:MODULE@ {: owner:IR-ID:ir-module-id :}
   owner MEMO-MINE? 0= if owner MEMO-START then
   idx MEMO-SEEN @ if
      b IR-BUILD:MODULE-KEY idx MEMO-ORD @ IR-ID:PACK-SYMBOL
      {: prior:IR-ID:ir-symbol-id :}
      c b prior IR-BUILD:SYMBOL-CK
      prior exit
   then
   c b idx VOCAB-NAME IR-BUILD:INTERN-SYMBOL {: sym:IR-ID:ir-symbol-id :}
   sym IR-ID:SYMBOL-LOCAL idx MEMO-ORD !
   true idx MEMO-SEEN !
   sym ;

: KEY-BIND ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-symbol-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:n :}
   c b OPCODES k + MEMO-BIND ;

public

: BIND ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-symbol-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder idx:n :}
   idx NTH drop                       \ an ordinal this dialect has an opcode for
   c b idx MEMO-BIND ;

\ Interning deduplicates: asking twice answers the same identity.
: OPCODE ( IR-CTX:ctx IR-BUILD:builder A64IR:opcode -- IR-ID:ir-symbol-id )
   ORD BIND ;

private

: MEMO-OPCODES? ( -- bool )
   MEMO-OWNED @ 0= if false exit then
   OPCODES 0 ?do i MEMO-SEEN @ 0= if false unloop exit then loop
   true ;

: MEMO-OPCODES! ( IR-CTX:ctx IR-BUILD:builder ptr IR-ID:ir-symbol-id n -- IR-ID:ir-module-id bool )
   {: c:IR-CTX:ctx b:IR-BUILD:builder dst:ptr cap:n :}
   c b 0 MEMO-MOD @ 0 MEMO-ORD OPCODES dst cap IR-BUILD:BIND-SYMBOLS? ;

public

\ Missing entries take BIND's ordinary interning path, which may grow the symbol
\ arena, so the batch reopens its storage afterwards.
: BIND-OPCODES! ( IR-CTX:ctx IR-BUILD:builder ptr IR-ID:ir-symbol-id n -- IR-ID:ir-module-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder dst:ptr cap:n :}
   cap OPCODES < cap $7FFFFFFFFFFFFFFF 1 cells / > or
   if E-IR-SYM-RANGE throw then
   dst cap cells + dst < if E-IR-SYM-RANGE throw then
   MEMO-OPCODES? if
      c b dst cap MEMO-OPCODES! if exit then drop
   then
   OPCODES 0 ?do c b i BIND drop loop
   c b dst cap MEMO-OPCODES! 0= if drop E-IR-BUILD-STATE throw then ;

\ ---- the condition a comparison is made under --------------------------------
: COND-CODE ( A64IR:cond -- n )
   MATCH cond
      lt    OF COND-LT ENDOF
      le    OF COND-LE ENDOF
      gt    OF COND-GT ENDOF
      ge    OF COND-GE ENDOF
      equal OF COND-EQ ENDOF
      ne    OF COND-NE ENDOF
      mi    OF COND-MI ENDOF
   ;MATCH ;

\ ---- the reach of each branch form -------------------------------------------
: B-FITS? ( n -- bool )      B-BITS FITS? ;
: BZ-FITS? ( n -- bool )     BZ-BITS FITS? ;
: BCOND-FITS? ( n -- bool )  BCOND-BITS FITS? ;

: ADR-FITS? ( n -- bool )    ADR-BITS FITS? ;

: KEY-IMM ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   K-IMM KEY-BIND ;

: KEY-DATA-OFFSET ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   K-DATA-OFFSET KEY-BIND ;

: DATA-OFFSET-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   DATA-OFFSET IR-BUILD:INTERN-INT-ATTR ;

: KEY-SHIFT ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   K-SHIFT KEY-BIND ;

\ A move-wide starting an address chain has to be found again after
\ publication, and a relocation pass may not decode region bytes. It is a
\ REQUIRED key, so a rewrite that drops it stops the compilation.
: KEY-ADDR ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   K-ADDR KEY-BIND ;

: KEY-ORIGIN ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   K-ORIGIN KEY-BIND ;

\ A move-wide the spill rewriter wrote again in front of a read carries 1, every
\ other one 0, so the allocator does not put the re-emission away again
\ (regalloc.f MB-RETURNED?).
: KEY-REMAT-MARK ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   K-REMAT-MARK KEY-BIND ;

: IMM-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   IMM16 IR-BUILD:INTERN-INT-ATTR ;

: FIMM-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   FIMM8 IR-BUILD:INTERN-INT-ATTR ;

: SHIFT-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   HALF IR-BUILD:INTERN-INT-ATTR ;

: ADDR-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   ADDR-KIND IR-BUILD:INTERN-INT-ATTR ;

: KEY-SLOT ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   K-SLOT KEY-BIND ;

: KEY-FRAME ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   K-FRAME KEY-BIND ;

: SLOT-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   SLOT IR-BUILD:INTERN-INT-ATTR ;

: FRAME-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   FRAME IR-BUILD:INTERN-INT-ATTR ;

\ Its own key and not the move-wide's, because the fields have different widths.
\ One key serves four forms because all four share one twelve-bit field.
: KEY-OFF ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   K-OFF KEY-BIND ;

: OFF-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   OFF IR-BUILD:INTERN-INT-ATTR ;

\ Its own key because the two fields admit different values: the logical one
\ carries the mask -2, which the arithmetic field cannot hold at all.
: KEY-MASK ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   K-MASK KEY-BIND ;

: MASK-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   MASK IR-BUILD:INTERN-INT-ATTR ;

: KEY-DSLOT ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   K-DSLOT KEY-BIND ;

: KEY-DBYTES ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   K-DBYTES KEY-BIND ;

: KEY-DBACK ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   K-DBACK KEY-BIND ;

\ Its own key and not `a64.dbytes`, because three passes tell a pointer move
\ that stands alone from one a transfer carries by ASKING which key it is under.
: KEY-DWB ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   K-DWB KEY-BIND ;

: KEY-ENTRY ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   K-ENTRY KEY-BIND ;

: KEY-GUARD-ENTRY ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   K-GUARD-ENTRY KEY-BIND ;

: KEY-TAIL-IN ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   K-TAIL-IN KEY-BIND ;

: KEY-TAIL-OUT ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   K-TAIL-OUT KEY-BIND ;

\ Its own key, because two passes recognise a tail branch by its ATTRIBUTES and
\ a trap under `a64.entry` would BE one to both of them.
: KEY-TRAP-ENTRY ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   K-TRAP-ENTRY KEY-BIND ;

\ Its own key: the entry a refusal branches to, carried by an ordinary
\ value-producing operation the guard branches over. Under `a64.entry` it would
\ read as a tail branch, under `a64.trap-entry` as a terminator.
: KEY-COLD-ENTRY ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   K-COLD-ENTRY KEY-BIND ;

\ An ordinal and not an address: there is no address until the emitter has laid
\ the emission out. How many functions there are is the emitter's fact.
: KEY-FUN ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   K-FUN KEY-BIND ;

: FUN-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   FUN-ORD IR-BUILD:INTERN-INT-ATTR ;

: KEY-COND ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   K-COND KEY-BIND ;

: COND-ATTR ( IR-CTX:ctx IR-BUILD:builder A64IR:cond -- IR-ID:ir-attr-id )
   COND-CODE COND IR-BUILD:INTERN-INT-ATTR ;

: DSLOT-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   DSLOT IR-BUILD:INTERN-INT-ATTR ;

: DBYTES-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   DBYTES IR-BUILD:INTERN-INT-ATTR ;

: DBACK-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   DBYTES IR-BUILD:INTERN-INT-ATTR ;

: DWB-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   DWB IR-BUILD:INTERN-INT-ATTR ;

: ENTRY-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   ENTRY IR-BUILD:INTERN-INT-ATTR ;

\ ---- what this dialect tells the register allocator about itself --------------
\ The A64IR names regalloc.f and regalloc-verify.f need, in one value: the copy
\ is `a64.mov`, the rematerialised form `a64.movz`, an address literal is four
\ move-wides that stay contiguous through spill insertion, and the transfer
\ forms move the data-stack pointer in the access. Symbols are this module's
\ ordinals, so the value is built per module. The stand is `survey`:
\ src/compiler/native/select.f stands the pointer where the fewest boundary
\ transfers need an adjustment.

: VOCABULARY ( IR-CTX:ctx IR-BUILD:builder -- NDIALECT:vocab )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b NAME IR-BUILD:INTERN-SYMBOL
   MAJOR MINOR
   CTARGET-ARCH:AARCH64
   c b GPR-TYPE  c b FPR-TYPE  c b MEM-TYPE
   c b KEY-SLOT  c b KEY-FRAME
   c b KEY-DSLOT  c b KEY-DBYTES  c b KEY-DBACK
   c b KEY-TAIL-IN  c b KEY-TAIL-OUT
   c b KEY-DWB NDIALECT-OPTSYM:PRESENT
   c b KEY-ENTRY  c b KEY-TRAP-ENTRY
   c b KEY-ADDR  c b KEY-SHIFT
   c b A64IR-OPCODE:MOV OPCODE
   c b A64IR-OPCODE:MOVZ OPCODE
   c b KEY-REMAT-MARK
   HALVES SLOT-WIDTH
   NDIALECT-DSTAND:SURVEY
   NDIALECT-VOCAB:MAKE ;

\ ---- what this dialect tells the spill rewriter about itself ------------------
\ The frame forms src/compiler/native/spill.f reads and writes. A record apart
\ from VOCABULARY only because one record would pass MAKE's staged arity.
\ Floating spills use `a64.fstr`/`a64.fldr` and the link register is saved by
\ `a64.lnkstr`/`a64.lnkldr`; a dialect without them says `absent`.
: LOWERING ( IR-CTX:ctx IR-BUILD:builder -- NDIALECT:lowering )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b NAME IR-BUILD:INTERN-SYMBOL
   MAJOR MINOR
   c b GPR-TYPE  c b FPR-TYPE  c b MEM-TYPE
   c b KEY-SLOT  c b KEY-FRAME
   c b A64IR-OPCODE:MOV OPCODE
   c b A64IR-OPCODE:MOVZ OPCODE
   c b KEY-REMAT-MARK
   c b A64IR-OPCODE:RESERVE OPCODE
   c b A64IR-OPCODE:RELEASE OPCODE
   c b A64IR-OPCODE:STORE OPCODE
   c b A64IR-OPCODE:LOAD OPCODE
   c b A64IR-OPCODE:FSTORE OPCODE NDIALECT-OPTSYM:PRESENT
   c b A64IR-OPCODE:FLOAD OPCODE NDIALECT-OPTSYM:PRESENT
   c b A64IR-OPCODE:TRAP OPCODE
   c b A64IR-OPCODE:LINKSAVE OPCODE NDIALECT-OPTSYM:PRESENT
   c b A64IR-OPCODE:LINKLOAD OPCODE NDIALECT-OPTSYM:PRESENT
   NDIALECT-LOWERING:MAKE ;

private

\ ---- the schema definitions --------------------------------------------------
: RULE ( IR-CTX:ctx IR-BUILD:builder A64IR:opcode -- IR-ID:ir-symbol-id )
   ORD 3 * 1+ TEXT@ IR-BUILD:INTERN-SYMBOL ;

: RENDERER ( IR-CTX:ctx IR-BUILD:builder A64IR:opcode -- IR-ID:ir-symbol-id )
   ORD 3 * 2 + TEXT@ IR-BUILD:INTERN-SYMBOL ;

: FINISH-OP ( IR-CTX:ctx IR-BUILD:builder A64IR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder o:A64IR:opcode :}
   c b o RULE IR-SCHEMA:SET-RULE
   c b o RENDERER IR-SCHEMA:SET-RENDERER
   c b IR-BUILD:DEFINE-OP ;

\ All three forms share the list, and MOVN's answer is constrained by the
\ emitter, which refuses a movn claiming to carry an address.
: MOVE-ATTRS ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b KEY-IMM IR-SCHEMA:ADD-ATTR
   c b KEY-SHIFT IR-SCHEMA:ADD-ATTR
   c b KEY-ADDR IR-SCHEMA:ADD-ATTR
   c b KEY-ORIGIN IR-SCHEMA:ADD-ATTR ;

: DEF-MOVZ ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:MOVZ OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-RESULT
   c b MOVE-ATTRS
   c b KEY-REMAT-MARK IR-SCHEMA:ADD-ATTR
   PURE-VALUE
   TOTAL
   TARGET
   c b A64IR-OPCODE:MOVZ FINISH-OP ;

: DEF-CODEADDR ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:CODEADDR OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-RESULT
   c b KEY-FUN IR-SCHEMA:ADD-ATTR
   PURE-VALUE
   TOTAL
   TARGET
   c b A64IR-OPCODE:CODEADDR FINISH-OP ;

\ One operation, so spill insertion cannot split the relocatable carrier.
\ Its result is a shared pointer, independent of the task-local DATA register.
: DEF-DATAADDR ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:DATAADDR OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-RESULT
   c b KEY-DATA-OFFSET IR-SCHEMA:ADD-ATTR
   c b KEY-ORIGIN IR-SCHEMA:ADD-ATTR
   PURE-VALUE
   TOTAL
   TARGET
   c b A64IR-OPCODE:DATAADDR FINISH-OP ;

: DEF-MOVN ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:MOVN OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-RESULT
   c b MOVE-ATTRS
   PURE-VALUE
   TOTAL
   TARGET
   c b A64IR-OPCODE:MOVN FINISH-OP ;

\ The instruction names one register field for both, so the schema declares
\ result 0 tied to operand 0.
: DEF-MOVK ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:MOVK OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   0 0 IR-SCHEMA:ADD-TIE
   c b MOVE-ATTRS
   PURE-VALUE
   TOTAL
   TARGET
   c b A64IR-OPCODE:MOVK FINISH-OP ;

: DEF-UNARY ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id A64IR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id o:A64IR:opcode :}
   c b o OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   PURE-VALUE
   TOTAL
   TARGET
   c b o FINISH-OP ;

\ The form exists so that the two registers CAN be different. A copy whose ends
\ coalesce is a no-op, and whether to elide it is the allocator's decision.
: DEF-MOV ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   A64IR-OPCODE:MOV DEF-UNARY ;

: DEF-MVN ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   A64IR-OPCODE:MVN DEF-UNARY ;

: DEF-BINARY ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id A64IR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id o:A64IR:opcode :}
   c b o OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   PURE-VALUE
   TOTAL
   TARGET
   c b o FINISH-OP ;

: DEF-LOGICAL-IMM ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id A64IR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id o:A64IR:opcode :}
   c b o OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   c b KEY-MASK IR-SCHEMA:ADD-ATTR
   PURE-VALUE
   TOTAL
   TARGET
   c b o FINISH-OP ;

\ The immediate is an ATTRIBUTE and not an operand, which is why the form costs
\ no register. It declares no tie, and `sub` is not commutative: only the value
\ being subtracted may be folded, never the value subtracted from.
: DEF-BINARY-IMM ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id A64IR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id o:A64IR:opcode :}
   c b o OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   c b KEY-OFF IR-SCHEMA:ADD-ATTR
   PURE-VALUE
   TOTAL
   TARGET
   c b o FINISH-OP ;

\ Operands are rn, rm then the addend ra. It declares NO tie - ARM64 names four
\ independent fields - and its addend may never be the zero register, because
\ `madd rd, rn, rm, xzr` IS `mul rd, rn, rm`, the same four bytes.
: DEF-MADD ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:MADD OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   PURE-VALUE
   TOTAL
   TARGET
   c b A64IR-OPCODE:MADD FINISH-OP ;

\ Three instructions: `cbnz` of the divisor over a `bl` to the engine's sealed
\ (DIV-ZERO) helper, which throws ARITH-ABI:E-DIV-ZERO, then `sdiv`, as the
\ engine's `/` does. The helper entry rides under KEY-COLD-ENTRY because only
\ the selector can ask NDICT:HELPER-TARGET where it is.
: DEF-SDIV ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:SDIV OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   c b KEY-COLD-ENTRY IR-SCHEMA:ADD-ATTR
   PURE-VALUE
   true IR-SCHEMA:SET-TRAP
   TARGET
   c b A64IR-OPCODE:SDIV FINISH-OP ;

\ ---- the frame forms ---------------------------------------------------------
: FRAME-MEM ( IR-SCHEMA:effect -- )
   {: e:IR-SCHEMA:effect :}
   false 0 0 IR-SCHEMA:SET-CONTROL
   IR--TYPE-SPACE:LOCAL IR--SCHEMA-ALIAS:UNALIASED e IR-SCHEMA:SET-MEMORY ;

: DEF-STR ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id A64IR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id k:IR-ID:ir-type-id o:A64IR:opcode :}
   c b o OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-RESULT
   c b KEY-SLOT IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:WRITE FRAME-MEM
   TOTAL
   TARGET
   c b o FINISH-OP ;

: DEF-LDR ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id A64IR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id k:IR-ID:ir-type-id o:A64IR:opcode :}
   c b o OPCODE IR-SCHEMA:BEGIN-OP
   k IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   k IR-SCHEMA:ADD-RESULT
   c b KEY-SLOT IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:READ FRAME-MEM
   TOTAL
   TARGET
   c b o FINISH-OP ;

: DEF-RESERVE ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:RESERVE OPCODE IR-SCHEMA:BEGIN-OP
   k IR-SCHEMA:ADD-RESULT
   c b KEY-FRAME IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:WRITE FRAME-MEM
   TOTAL
   TARGET
   c b A64IR-OPCODE:RESERVE FINISH-OP ;

: DEF-RELEASE ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:RELEASE OPCODE IR-SCHEMA:BEGIN-OP
   k IR-SCHEMA:ADD-OPERAND
   c b KEY-FRAME IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:WRITE FRAME-MEM
   TOTAL
   TARGET
   c b A64IR-OPCODE:RELEASE FINISH-OP ;

\ ---- the data-stack forms ----------------------------------------------------
\ Unrestricted aliasing is an ORDERING statement: it forbids moving one access
\ across another it cannot prove is elsewhere, and nothing more than that.
: DSTACK-MEM ( IR-SCHEMA:effect -- )
   {: e:IR-SCHEMA:effect :}
   false 0 0 IR-SCHEMA:SET-CONTROL
   IR--TYPE-SPACE:GENERIC IR--SCHEMA-ALIAS:UNRESTRICTED e IR-SCHEMA:SET-MEMORY ;

: DSTACK-TERM-MEM ( IR-SCHEMA:effect -- )
   {: e:IR-SCHEMA:effect :}
   true 0 0 IR-SCHEMA:SET-CONTROL
   IR--TYPE-SPACE:GENERIC IR--SCHEMA-ALIAS:UNRESTRICTED e IR-SCHEMA:SET-MEMORY ;

: DEF-DTAKE ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:DTAKE OPCODE IR-SCHEMA:BEGIN-OP
   k IR-SCHEMA:ADD-RESULT
   c b KEY-DBYTES IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:WRITE DSTACK-MEM
   TOTAL
   TARGET
   c b A64IR-OPCODE:DTAKE FINISH-OP ;

: DEF-DLOAD ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id A64IR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id k:IR-ID:ir-type-id o:A64IR:opcode :}
   c b o OPCODE IR-SCHEMA:BEGIN-OP
   k IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   k IR-SCHEMA:ADD-RESULT
   c b KEY-DSLOT IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:READ DSTACK-MEM
   TOTAL
   TARGET
   c b o FINISH-OP ;

: DEF-DSTORE ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id A64IR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id k:IR-ID:ir-type-id o:A64IR:opcode :}
   c b o OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-RESULT
   c b KEY-DSLOT IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:WRITE DSTACK-MEM
   TOTAL
   TARGET
   c b o FINISH-OP ;

: DEF-DPUBLISH ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:DPUBLISH OPCODE IR-SCHEMA:BEGIN-OP
   k IR-SCHEMA:ADD-OPERAND
   c b KEY-DBYTES IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:WRITE DSTACK-MEM
   TOTAL
   TARGET
   c b A64IR-OPCODE:DPUBLISH FINISH-OP ;

\ ---- the fused forms ---------------------------------------------------------
\ The machine writes the base back as part of the access, so one instruction is
\ a store plus its publish, or a take plus its load. No slot attribute: the
\ post-indexed store writes at the base then moves it, the pre-indexed load
\ moves then reads, so `a64.dwb` is all they carry. The fused load is
\ READ-WRITE where a plain a64.dload is READ.
: DEF-DPUSH ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id A64IR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id k:IR-ID:ir-type-id o:A64IR:opcode :}
   c b o OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-RESULT
   c b KEY-DWB IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:WRITE DSTACK-MEM
   TOTAL
   TARGET
   c b o FINISH-OP ;

: DEF-DPOP ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id A64IR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id k:IR-ID:ir-type-id o:A64IR:opcode :}
   c b o OPCODE IR-SCHEMA:BEGIN-OP
   k IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   k IR-SCHEMA:ADD-RESULT
   c b KEY-DWB IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:READ-WRITE DSTACK-MEM
   TOTAL
   TARGET
   c b o FINISH-OP ;

\ ---- the two addressed forms -------------------------------------------------
: ADDR-MEM ( IR-SCHEMA:effect -- )
   {: e:IR-SCHEMA:effect :}
   false 0 0 IR-SCHEMA:SET-CONTROL
   IR--TYPE-SPACE:GENERIC IR--SCHEMA-ALIAS:UNRESTRICTED e IR-SCHEMA:SET-MEMORY ;

\ There is no offset attribute: the form encodes at offset zero. The base and
\ the transfer are two TYPES, because a64.faload reads through an X register
\ and lands the bytes in a D one.
: DEF-ALDR ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id IR-ID:ir-type-id A64IR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder a:IR-ID:ir-type-id v:IR-ID:ir-type-id k:IR-ID:ir-type-id o:A64IR:opcode :}
   c b o OPCODE IR-SCHEMA:BEGIN-OP
   a IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-OPERAND
   v IR-SCHEMA:ADD-RESULT
   k IR-SCHEMA:ADD-RESULT
   IR--SCHEMA-EFFECT:READ ADDR-MEM
   TOTAL
   TARGET
   c b o FINISH-OP ;

: DEF-ASTR ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id IR-ID:ir-type-id A64IR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder a:IR-ID:ir-type-id v:IR-ID:ir-type-id k:IR-ID:ir-type-id o:A64IR:opcode :}
   c b o OPCODE IR-SCHEMA:BEGIN-OP
   v IR-SCHEMA:ADD-OPERAND
   a IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-RESULT
   IR--SCHEMA-EFFECT:WRITE ADDR-MEM
   TOTAL
   TARGET
   c b o FINISH-OP ;

\ A checked scalar store calls the sealed span guard before writing. The copied
\ address is pinned to x10; x11 holds the width and the guard may overwrite
\ x12/x13. Fixed results reserve those early clobbers even while this form
\ reads its value. The guard entry is distinct from ordinary calls, so this
\ form publishes no data-stack arguments or results.
: DEF-PSTORE ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id A64IR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id k:IR-ID:ir-type-id o:A64IR:opcode :}
   c b o OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   t IR-SCHEMA:ADD-RESULT
   t IR-SCHEMA:ADD-RESULT
   k IR-SCHEMA:ADD-RESULT
   IR--SCHEMA-SIDE:OPERAND 1 10 IR-SCHEMA:ADD-FIXED
   IR--SCHEMA-SIDE:RESULT 0 11 IR-SCHEMA:ADD-FIXED
   IR--SCHEMA-SIDE:RESULT 1 12 IR-SCHEMA:ADD-FIXED
   IR--SCHEMA-SIDE:RESULT 2 13 IR-SCHEMA:ADD-FIXED
   c b KEY-GUARD-ENTRY IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:WRITE ADDR-MEM
   true IR-SCHEMA:SET-TRAP
   TARGET
   c b o FINISH-OP ;

\ The width is the FORM: the machine has separate Ldrb and Strb encodings. The
\ loaded byte arrives zero-extended, which is what `c@` leaves.
: DEF-ALDRB ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id k:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:ABLOAD OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   k IR-SCHEMA:ADD-RESULT
   IR--SCHEMA-EFFECT:READ ADDR-MEM
   TOTAL
   TARGET
   c b A64IR-OPCODE:ABLOAD FINISH-OP ;

: DEF-ASTRB ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id k:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:ABSTORE OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-RESULT
   IR--SCHEMA-EFFECT:WRITE ADDR-MEM
   TOTAL
   TARGET
   c b A64IR-OPCODE:ABSTORE FINISH-OP ;

\ ---- the comparison form -----------------------------------------------------
\ ONE operation and three instructions - compare, set one on the condition,
\ negate - because the flags between them are a single architectural resource
\ no value stands for and the allocator may not hand out.
: DEF-FLAG ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:FLAG OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   c b KEY-COND IR-SCHEMA:ADD-ATTR
   PURE-VALUE
   TOTAL
   TARGET
   c b A64IR-OPCODE:FLAG FINISH-OP ;

\ The operand is the LEFT-hand side and the immediate the right, so a rewriter
\ may fold only the second operand of a comparison.
: DEF-FLAG-IMM ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:FLAGI OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   c b KEY-COND IR-SCHEMA:ADD-ATTR
   c b KEY-OFF IR-SCHEMA:ADD-ATTR
   PURE-VALUE
   TOTAL
   TARGET
   c b A64IR-OPCODE:FLAGI FINISH-OP ;

\ ---- the two conditional-select forms ----------------------------------------

: DEF-SELZ ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:SELZ OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   PURE-VALUE
   TOTAL
   TARGET
   c b A64IR-OPCODE:SELZ FINISH-OP ;

: DEF-CMPSEL ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:CMPSEL OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   c b KEY-COND IR-SCHEMA:ADD-ATTR
   PURE-VALUE
   TOTAL
   TARGET
   c b A64IR-OPCODE:CMPSEL FINISH-OP ;

\ ---- the two branch forms ----------------------------------------------------
: DEF-BR ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:BR OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND-TAIL
   true 1 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE
   TOTAL
   TARGET
   c b A64IR-OPCODE:BR FINISH-OP ;

: DEF-BRZ ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:BRZ OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   true 2 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE
   TOTAL
   TARGET
   c b A64IR-OPCODE:BRZ FINISH-OP ;

\ The first successor is the CONDITION-HOLDS one, measured: putting the
\ condition-false arm first costs the loop rows four to six per cent. Neither
\ successor may take arguments; it defines no value, which is the whole saving.
: DEF-CMPBR ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:CMPBR OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   c b KEY-COND IR-SCHEMA:ADD-ATTR
   true 2 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE
   TOTAL
   TARGET
   c b A64IR-OPCODE:CMPBR FINISH-OP ;

\ Every sentence of a64.cmpbr carries over; the operand it keeps is the LEFT-hand
\ side, because the machine subtracts the immediate FROM the register.
: DEF-CMPBR-IMM ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:CMPBRI OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   c b KEY-COND IR-SCHEMA:ADD-ATTR
   c b KEY-OFF IR-SCHEMA:ADD-ATTR
   true 2 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE
   TOTAL
   TARGET
   c b A64IR-OPCODE:CMPBRI FINISH-OP ;

: DEF-RET ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:RET OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND-TAIL
   true 0 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE
   TOTAL
   TARGET
   c b A64IR-OPCODE:RET FINISH-OP ;

\ ---- the call, and the link register it costs --------------------------------
: DEF-CALL ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:CALL OPCODE IR-SCHEMA:BEGIN-OP
   k IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-RESULT
   c b KEY-DBYTES IR-SCHEMA:ADD-ATTR
   c b KEY-DBACK IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:READ-WRITE DSTACK-MEM
   true IR-SCHEMA:SET-TRAP
   TARGET
   c b A64IR-OPCODE:CALL FINISH-OP ;

: DEF-WORDCALL ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:WORDCALL OPCODE IR-SCHEMA:BEGIN-OP
   k IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-RESULT
   c b KEY-DBYTES IR-SCHEMA:ADD-ATTR
   c b KEY-DBACK IR-SCHEMA:ADD-ATTR
   c b KEY-ENTRY IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:READ-WRITE DSTACK-MEM
   true IR-SCHEMA:SET-TRAP
   TARGET
   c b A64IR-OPCODE:WORDCALL FINISH-OP ;

\ A terminator: publish the callee's actual argument width, then branch into its
\ entry. It takes the data-stack order and ends it.
: DEF-TAILCALL ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:TAILCALL OPCODE IR-SCHEMA:BEGIN-OP
   k IR-SCHEMA:ADD-OPERAND
   c b KEY-ENTRY IR-SCHEMA:ADD-ATTR
   c b KEY-TAIL-IN IR-SCHEMA:ADD-ATTR
   c b KEY-TAIL-OUT IR-SCHEMA:ADD-ATTR
   c b KEY-DBYTES IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:WRITE DSTACK-TERM-MEM
   true IR-SCHEMA:SET-TRAP
   TARGET
   c b A64IR-OPCODE:TAILCALL FINISH-OP ;

\ Its own form and not the tail branch with another target: a tail branch is how
\ a routine RETURNS, and this publishes nothing and comes back from nowhere. It
\ carries an adjustment over the diagnostic address, length and exit code.
: DEF-TRAP ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:TRAP OPCODE IR-SCHEMA:BEGIN-OP
   k IR-SCHEMA:ADD-OPERAND
   c b KEY-TRAP-ENTRY IR-SCHEMA:ADD-ATTR
   c b KEY-DBYTES IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:WRITE DSTACK-TERM-MEM
   true IR-SCHEMA:SET-TRAP
   TARGET
   c b A64IR-OPCODE:TRAP FINISH-OP ;

\ The register they move is x30, which is named by the FORM: NEFF keeps it out
\ of every general-register set, so no operand could ever name it.
: DEF-LNKSTR ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:LINKSAVE OPCODE IR-SCHEMA:BEGIN-OP
   k IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-RESULT
   c b KEY-SLOT IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:WRITE FRAME-MEM
   TOTAL
   TARGET
   c b A64IR-OPCODE:LINKSAVE FINISH-OP ;

: DEF-LNKLDR ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:LINKLOAD OPCODE IR-SCHEMA:BEGIN-OP
   k IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-RESULT
   c b KEY-SLOT IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:READ FRAME-MEM
   TOTAL
   TARGET
   c b A64IR-OPCODE:LINKLOAD FINISH-OP ;

\ ---- the floating forms ------------------------------------------------------




\ None of the floating forms may trap: dividing by zero answers an infinity,
\ zero by zero and the square root of a negative the default NaN.
: DEF-FBINARY ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id A64IR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder f:IR-ID:ir-type-id o:A64IR:opcode :}
   c b o OPCODE IR-SCHEMA:BEGIN-OP
   f IR-SCHEMA:ADD-OPERAND
   f IR-SCHEMA:ADD-OPERAND
   f IR-SCHEMA:ADD-RESULT
   PURE-VALUE
   TOTAL
   FP-TARGET
   c b o FINISH-OP ;

: DEF-FCROSS ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id A64IR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder ti:IR-ID:ir-type-id to:IR-ID:ir-type-id
      o:A64IR:opcode :}
   c b o OPCODE IR-SCHEMA:BEGIN-OP
   ti IR-SCHEMA:ADD-OPERAND
   to IR-SCHEMA:ADD-RESULT
   PURE-VALUE
   TOTAL
   FP-TARGET
   c b o FINISH-OP ;

: DEF-FMOVDI ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder f:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:FMOVDI OPCODE IR-SCHEMA:BEGIN-OP
   f IR-SCHEMA:ADD-RESULT
   c b KEY-IMM IR-SCHEMA:ADD-ATTR
   PURE-VALUE
   TOTAL
   FP-TARGET
   c b A64IR-OPCODE:FMOVDI FINISH-OP ;

\ ---- the four float comparison forms -----------------------------------------

: DEF-FFLAG ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder f:IR-ID:ir-type-id t:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:FFLAG OPCODE IR-SCHEMA:BEGIN-OP
   f IR-SCHEMA:ADD-OPERAND
   f IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   c b KEY-COND IR-SCHEMA:ADD-ATTR
   PURE-VALUE
   TOTAL
   FP-TARGET
   c b A64IR-OPCODE:FFLAG FINISH-OP ;

: DEF-FFLAGZ ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder f:IR-ID:ir-type-id t:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:FFLAGZ OPCODE IR-SCHEMA:BEGIN-OP
   f IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   c b KEY-COND IR-SCHEMA:ADD-ATTR
   PURE-VALUE
   TOTAL
   FP-TARGET
   c b A64IR-OPCODE:FFLAGZ FINISH-OP ;

\ When either operand is a NaN the relation does NOT hold, whichever condition
\ is named, so control goes to the SECOND successor - which is the whole of how
\ this dialect keeps the engine's NaN rule through a fused branch.
: DEF-FCMPBR ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder f:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:FCMPBR OPCODE IR-SCHEMA:BEGIN-OP
   f IR-SCHEMA:ADD-OPERAND
   f IR-SCHEMA:ADD-OPERAND
   c b KEY-COND IR-SCHEMA:ADD-ATTR
   true 2 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE
   TOTAL
   FP-TARGET
   c b A64IR-OPCODE:FCMPBR FINISH-OP ;

: DEF-FCMPBRZ ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder f:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:FCMPBRZ OPCODE IR-SCHEMA:BEGIN-OP
   f IR-SCHEMA:ADD-OPERAND
   c b KEY-COND IR-SCHEMA:ADD-ATTR
   true 2 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE
   TOTAL
   FP-TARGET
   c b A64IR-OPCODE:FCMPBRZ FINISH-OP ;

\ ---- the two selects that answer a double ------------------------------------

: DEF-SELZD ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id f:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:SELZD OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   f IR-SCHEMA:ADD-OPERAND
   f IR-SCHEMA:ADD-OPERAND
   f IR-SCHEMA:ADD-RESULT
   PURE-VALUE
   TOTAL
   FP-TARGET
   c b A64IR-OPCODE:SELZD FINISH-OP ;

: DEF-CMPSELD ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id f:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:CMPSELD OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   f IR-SCHEMA:ADD-OPERAND
   f IR-SCHEMA:ADD-OPERAND
   f IR-SCHEMA:ADD-RESULT
   c b KEY-COND IR-SCHEMA:ADD-ATTR
   PURE-VALUE
   TOTAL
   FP-TARGET
   c b A64IR-OPCODE:CMPSELD FINISH-OP ;

\ ---- the four selects whose flags an Fcmp wrote ------------------------------

\ The flags-writer is the OPCODE and never a field, because what a condition
\ MEANS depends on which instruction wrote the flags.
: DEF-FCMPSEL ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id f:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:FCMPSEL OPCODE IR-SCHEMA:BEGIN-OP
   f IR-SCHEMA:ADD-OPERAND
   f IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   c b KEY-COND IR-SCHEMA:ADD-ATTR
   PURE-VALUE
   TOTAL
   FP-TARGET
   c b A64IR-OPCODE:FCMPSEL FINISH-OP ;

: DEF-FCMPSELZ ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id f:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:FCMPSELZ OPCODE IR-SCHEMA:BEGIN-OP
   f IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   c b KEY-COND IR-SCHEMA:ADD-ATTR
   PURE-VALUE
   TOTAL
   FP-TARGET
   c b A64IR-OPCODE:FCMPSELZ FINISH-OP ;

: DEF-FCMPSELD ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder f:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:FCMPSELD OPCODE IR-SCHEMA:BEGIN-OP
   f IR-SCHEMA:ADD-OPERAND
   f IR-SCHEMA:ADD-OPERAND
   f IR-SCHEMA:ADD-OPERAND
   f IR-SCHEMA:ADD-OPERAND
   f IR-SCHEMA:ADD-RESULT
   c b KEY-COND IR-SCHEMA:ADD-ATTR
   PURE-VALUE
   TOTAL
   FP-TARGET
   c b A64IR-OPCODE:FCMPSELD FINISH-OP ;

: DEF-FCMPSELZD ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder f:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:FCMPSELZD OPCODE IR-SCHEMA:BEGIN-OP
   f IR-SCHEMA:ADD-OPERAND
   f IR-SCHEMA:ADD-OPERAND
   f IR-SCHEMA:ADD-OPERAND
   f IR-SCHEMA:ADD-RESULT
   c b KEY-COND IR-SCHEMA:ADD-ATTR
   PURE-VALUE
   TOTAL
   FP-TARGET
   c b A64IR-OPCODE:FCMPSELZD FINISH-OP ;

\ ---- the table this dialect may fill -----------------------------------------
\ The table's dialect name and version are fixed when the module is created, so
\ reading them back off the live module decides whose table it is.
: DIALECT-CK ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b  c b IR-BUILD:DIALECT@  NAME IR-BUILD:SYMBOL-IS?
   0= if E-A64IR-DIALECT throw then
   c b IR-BUILD:SCHEMA-MAJOR@ MAJOR <> if E-A64IR-DIALECT throw then
   c b IR-BUILD:SCHEMA-MINOR@ MINOR <> if E-A64IR-DIALECT throw then ;

private

\ ---- the session prototype ---------------------------------------------------
\ A load interns the vocabulary once into a session interner; every module built
\ while it stands starts as a copy, holding the spellings at the same ordinals.
\ PROTOTYPE-CLEAR gives it up with the session; an interner that outlives its
\ context is refused by the arena's seal.
2 TYPED-BUFFER PROTO IR-ARENA:arena
variable PROTO-ON
0 PROTO-ON !

: PRE ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key ptr u8 n -- IR-ID:ir-symbol-id )
   IR-SYM:INTERN ;

: PRE-OP ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena k:IR-ID:ir-module-key i:n :}
   c a r k i NTH OP-NAME PRE IR-ID:SYMBOL-LOCAL i PROTO-ORD ! ;

: PRE-KEY ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena k:IR-ID:ir-module-key i:n :}
   c a r k i KEY-NAME PRE IR-ID:SYMBOL-LOCAL  OPCODES i + PROTO-ORD ! ;

public

\ Walked, not listed, so the prototype cannot fall behind the tables above.
: PROTOTYPE ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena k:IR-ID:ir-module-key :}
   c a r k NAME PRE drop
   OPCODES 0 ?do c a r k i PRE-OP loop
   KEYS 0 ?do c a r k i PRE-KEY loop
   a 0 PROTO !
   r 1 PROTO !
   1 PROTO-ON ! ;

: PROTOTYPE-CLEAR ( -- )
   0 PROTO-ON !
   MEMO-FORGET
   \ Both ordinals and the module handle belonged to the retired session.
   0 0 PROTO BYTE-VIEW CELL-VIEW !  0 1 PROTO BYTE-VIEW CELL-VIEW !
   0 0 MEMO-MOD BYTE-VIEW CELL-VIEW !
   VOCAB 0 ?do 0 i PROTO-ORD !  0 i MEMO-ORD ! loop ;

\ ---- creation and registration -----------------------------------------------
: NEW-BUILDER ( IR-CTX:ctx -- IR-BUILD:builder )
   dup CHECK-TARGET
   PROTO-ON @ 0= if NAME MAJOR MINOR IR-BUILD:NEW-BUILDER exit then
   NAME MAJOR MINOR 0 PROTO @ 1 PROTO @ IR-BUILD:NEW-BUILDER-FROM
   dup MEMO-ADOPT ;

\ Definition is one opcode at a time, so a refusal leaves the opcodes already
\ defined and defines no more.
: REGISTER ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b DIALECT-CK
   c b GPR-TYPE {: t:IR-ID:ir-type-id :}
   c b MEM-TYPE {: k:IR-ID:ir-type-id :}
   c b t DEF-MOVZ
   c b t DEF-MOVK
   c b t DEF-MOVN
   c b t DEF-MOV
   c b t A64IR-OPCODE:ADD DEF-BINARY
   c b t A64IR-OPCODE:SUB DEF-BINARY
   c b t A64IR-OPCODE:ADDI DEF-BINARY-IMM
   c b t A64IR-OPCODE:SUBI DEF-BINARY-IMM
   c b t A64IR-OPCODE:ANDI DEF-LOGICAL-IMM
   c b t A64IR-OPCODE:ORRI DEF-LOGICAL-IMM
   c b t A64IR-OPCODE:EORI DEF-LOGICAL-IMM
   c b t A64IR-OPCODE:MUL DEF-BINARY
   c b t DEF-MADD
   c b t DEF-SDIV
   c b t A64IR-OPCODE:SDIVNZ DEF-BINARY
   c b t A64IR-OPCODE:AND DEF-BINARY
   c b t A64IR-OPCODE:ORR DEF-BINARY
   c b t A64IR-OPCODE:EOR DEF-BINARY
   c b t A64IR-OPCODE:LSLV DEF-BINARY
   c b t A64IR-OPCODE:LSRV DEF-BINARY
   c b t DEF-MVN
   c b t k A64IR-OPCODE:STORE DEF-STR
   c b t k A64IR-OPCODE:LOAD DEF-LDR
   c b k DEF-RESERVE
   c b k DEF-RELEASE
   c b k DEF-DTAKE
   c b t k A64IR-OPCODE:DLOAD DEF-DLOAD
   c b t k A64IR-OPCODE:DSTORE DEF-DSTORE
   c b k DEF-DPUBLISH
   c b t k A64IR-OPCODE:DPUSH DEF-DPUSH
   c b t k A64IR-OPCODE:DPOP DEF-DPOP
   c b t t k A64IR-OPCODE:ALOAD DEF-ALDR
   c b t t k A64IR-OPCODE:ASTORE DEF-ASTR
   c b t k A64IR-OPCODE:PSTORE DEF-PSTORE
   c b t k A64IR-OPCODE:PBSTORE DEF-PSTORE
   c b t k DEF-ALDRB
   c b t k DEF-ASTRB
   c b t DEF-FLAG
   c b t DEF-FLAG-IMM
   c b t DEF-SELZ
   c b t DEF-CMPSEL
   c b t DEF-BR
   c b t DEF-BRZ
   c b t DEF-CMPBR
   c b t DEF-CMPBR-IMM
   c b k DEF-CALL
   c b k DEF-WORDCALL
   c b k DEF-TAILCALL
   c b k DEF-TRAP
   c b k DEF-LNKSTR
   c b k DEF-LNKLDR
   c b t DEF-RET
   c b t DEF-CODEADDR
   c b t DEF-DATAADDR
   c b FPR-TYPE {: f:IR-ID:ir-type-id :}
   c b f A64IR-OPCODE:FADD DEF-FBINARY
   c b f A64IR-OPCODE:FSUB DEF-FBINARY
   c b f A64IR-OPCODE:FMUL DEF-FBINARY
   c b f A64IR-OPCODE:FDIV DEF-FBINARY
   c b f f A64IR-OPCODE:FNEG DEF-FCROSS
   c b f f A64IR-OPCODE:FABS DEF-FCROSS
   c b f f A64IR-OPCODE:FSQRT DEF-FCROSS
   c b t f A64IR-OPCODE:SCVTF DEF-FCROSS
   c b f t A64IR-OPCODE:FCVTZS DEF-FCROSS
   c b t f A64IR-OPCODE:FMOVXD DEF-FCROSS
   c b f t A64IR-OPCODE:FMOVDX DEF-FCROSS
   c b f f A64IR-OPCODE:FMOVDD DEF-FCROSS
   c b f DEF-FMOVDI
   c b f t DEF-FFLAG
   c b f t DEF-FFLAGZ
   c b f DEF-FCMPBR
   c b f DEF-FCMPBRZ
   c b t f DEF-SELZD
   c b t f DEF-CMPSELD
   c b t f DEF-FCMPSEL
   c b t f DEF-FCMPSELZ
   c b f DEF-FCMPSELD
   c b f DEF-FCMPSELZD
   c b f k A64IR-OPCODE:FSTORE DEF-STR
   c b f k A64IR-OPCODE:FLOAD DEF-LDR
   c b f k A64IR-OPCODE:FDLOAD DEF-DLOAD
   c b f k A64IR-OPCODE:FDSTORE DEF-DSTORE
   c b f k A64IR-OPCODE:FDPUSH DEF-DPUSH
   c b f k A64IR-OPCODE:FDPOP DEF-DPOP
   c b t f k A64IR-OPCODE:FALOAD DEF-ALDR
   c b t f k A64IR-OPCODE:FASTORE DEF-ASTR ;

private

: DEFINE-ONE ( IR-CTX:ctx IR-BUILD:builder A64IR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder o:A64IR:opcode :}
   o MATCH opcode
      movz      OF c b c b GPR-TYPE DEF-MOVZ ENDOF
      movk      OF c b c b GPR-TYPE DEF-MOVK ENDOF
      movn      OF c b c b GPR-TYPE DEF-MOVN ENDOF
      mov       OF c b c b GPR-TYPE DEF-MOV ENDOF
      add       OF c b c b GPR-TYPE A64IR-OPCODE:ADD DEF-BINARY ENDOF
      sub       OF c b c b GPR-TYPE A64IR-OPCODE:SUB DEF-BINARY ENDOF
      addi      OF c b c b GPR-TYPE A64IR-OPCODE:ADDI DEF-BINARY-IMM ENDOF
      subi      OF c b c b GPR-TYPE A64IR-OPCODE:SUBI DEF-BINARY-IMM ENDOF
      andi      OF c b c b GPR-TYPE A64IR-OPCODE:ANDI DEF-LOGICAL-IMM ENDOF
      orri      OF c b c b GPR-TYPE A64IR-OPCODE:ORRI DEF-LOGICAL-IMM ENDOF
      eori      OF c b c b GPR-TYPE A64IR-OPCODE:EORI DEF-LOGICAL-IMM ENDOF
      mul       OF c b c b GPR-TYPE A64IR-OPCODE:MUL DEF-BINARY ENDOF
      madd      OF c b c b GPR-TYPE DEF-MADD ENDOF
      sdiv      OF c b c b GPR-TYPE DEF-SDIV ENDOF
      sdivnz    OF c b c b GPR-TYPE A64IR-OPCODE:SDIVNZ DEF-BINARY ENDOF
      and       OF c b c b GPR-TYPE A64IR-OPCODE:AND DEF-BINARY ENDOF
      orr       OF c b c b GPR-TYPE A64IR-OPCODE:ORR DEF-BINARY ENDOF
      eor       OF c b c b GPR-TYPE A64IR-OPCODE:EOR DEF-BINARY ENDOF
      lslv      OF c b c b GPR-TYPE A64IR-OPCODE:LSLV DEF-BINARY ENDOF
      lsrv      OF c b c b GPR-TYPE A64IR-OPCODE:LSRV DEF-BINARY ENDOF
      mvn       OF c b c b GPR-TYPE DEF-MVN ENDOF
      store     OF c b c b GPR-TYPE c b MEM-TYPE A64IR-OPCODE:STORE DEF-STR ENDOF
      load      OF c b c b GPR-TYPE c b MEM-TYPE A64IR-OPCODE:LOAD DEF-LDR ENDOF
      reserve   OF c b c b MEM-TYPE DEF-RESERVE ENDOF
      release   OF c b c b MEM-TYPE DEF-RELEASE ENDOF
      dtake     OF c b c b MEM-TYPE DEF-DTAKE ENDOF
      dload     OF c b c b GPR-TYPE c b MEM-TYPE A64IR-OPCODE:DLOAD DEF-DLOAD ENDOF
      dstore    OF c b c b GPR-TYPE c b MEM-TYPE A64IR-OPCODE:DSTORE DEF-DSTORE ENDOF
      dpublish  OF c b c b MEM-TYPE DEF-DPUBLISH ENDOF
      dpush     OF c b c b GPR-TYPE c b MEM-TYPE A64IR-OPCODE:DPUSH DEF-DPUSH ENDOF
      dpop      OF c b c b GPR-TYPE c b MEM-TYPE A64IR-OPCODE:DPOP DEF-DPOP ENDOF
      aload     OF c b c b GPR-TYPE c b GPR-TYPE c b MEM-TYPE A64IR-OPCODE:ALOAD DEF-ALDR ENDOF
      astore    OF c b c b GPR-TYPE c b GPR-TYPE c b MEM-TYPE A64IR-OPCODE:ASTORE DEF-ASTR ENDOF
      pstore    OF c b c b GPR-TYPE c b MEM-TYPE A64IR-OPCODE:PSTORE DEF-PSTORE ENDOF
      pbstore   OF c b c b GPR-TYPE c b MEM-TYPE A64IR-OPCODE:PBSTORE DEF-PSTORE ENDOF
      abload    OF c b c b GPR-TYPE c b MEM-TYPE DEF-ALDRB ENDOF
      abstore   OF c b c b GPR-TYPE c b MEM-TYPE DEF-ASTRB ENDOF
      flag      OF c b c b GPR-TYPE DEF-FLAG ENDOF
      flagi     OF c b c b GPR-TYPE DEF-FLAG-IMM ENDOF
      selz      OF c b c b GPR-TYPE DEF-SELZ ENDOF
      cmpsel    OF c b c b GPR-TYPE DEF-CMPSEL ENDOF
      br        OF c b c b GPR-TYPE DEF-BR ENDOF
      brz       OF c b c b GPR-TYPE DEF-BRZ ENDOF
      cmpbr     OF c b c b GPR-TYPE DEF-CMPBR ENDOF
      cmpbri    OF c b c b GPR-TYPE DEF-CMPBR-IMM ENDOF
      call      OF c b c b MEM-TYPE DEF-CALL ENDOF
      wordcall  OF c b c b MEM-TYPE DEF-WORDCALL ENDOF
      tailcall  OF c b c b MEM-TYPE DEF-TAILCALL ENDOF
      trap      OF c b c b MEM-TYPE DEF-TRAP ENDOF
      linksave  OF c b c b MEM-TYPE DEF-LNKSTR ENDOF
      linkload  OF c b c b MEM-TYPE DEF-LNKLDR ENDOF
      ret       OF c b c b GPR-TYPE DEF-RET ENDOF
      codeaddr  OF c b c b GPR-TYPE DEF-CODEADDR ENDOF
      dataaddr  OF c b c b GPR-TYPE DEF-DATAADDR ENDOF
      fadd      OF c b c b FPR-TYPE A64IR-OPCODE:FADD DEF-FBINARY ENDOF
      fsub      OF c b c b FPR-TYPE A64IR-OPCODE:FSUB DEF-FBINARY ENDOF
      fmul      OF c b c b FPR-TYPE A64IR-OPCODE:FMUL DEF-FBINARY ENDOF
      fdiv      OF c b c b FPR-TYPE A64IR-OPCODE:FDIV DEF-FBINARY ENDOF
      fneg      OF c b c b FPR-TYPE c b FPR-TYPE A64IR-OPCODE:FNEG DEF-FCROSS ENDOF
      fabs      OF c b c b FPR-TYPE c b FPR-TYPE A64IR-OPCODE:FABS DEF-FCROSS ENDOF
      fsqrt     OF c b c b FPR-TYPE c b FPR-TYPE A64IR-OPCODE:FSQRT DEF-FCROSS ENDOF
      scvtf     OF c b c b GPR-TYPE c b FPR-TYPE A64IR-OPCODE:SCVTF DEF-FCROSS ENDOF
      fcvtzs    OF c b c b FPR-TYPE c b GPR-TYPE A64IR-OPCODE:FCVTZS DEF-FCROSS ENDOF
      fmovxd    OF c b c b GPR-TYPE c b FPR-TYPE A64IR-OPCODE:FMOVXD DEF-FCROSS ENDOF
      fmovdx    OF c b c b FPR-TYPE c b GPR-TYPE A64IR-OPCODE:FMOVDX DEF-FCROSS ENDOF
      fmovdd    OF c b c b FPR-TYPE c b FPR-TYPE A64IR-OPCODE:FMOVDD DEF-FCROSS ENDOF
      fmovdi    OF c b c b FPR-TYPE DEF-FMOVDI ENDOF
      fflag     OF c b c b FPR-TYPE c b GPR-TYPE DEF-FFLAG ENDOF
      fflagz    OF c b c b FPR-TYPE c b GPR-TYPE DEF-FFLAGZ ENDOF
      fcmpbr    OF c b c b FPR-TYPE DEF-FCMPBR ENDOF
      fcmpbrz   OF c b c b FPR-TYPE DEF-FCMPBRZ ENDOF
      selzd     OF c b c b GPR-TYPE c b FPR-TYPE DEF-SELZD ENDOF
      cmpseld   OF c b c b GPR-TYPE c b FPR-TYPE DEF-CMPSELD ENDOF
      fcmpsel   OF c b c b GPR-TYPE c b FPR-TYPE DEF-FCMPSEL ENDOF
      fcmpselz  OF c b c b GPR-TYPE c b FPR-TYPE DEF-FCMPSELZ ENDOF
      fcmpseld  OF c b c b FPR-TYPE DEF-FCMPSELD ENDOF
      fcmpselzd OF c b c b FPR-TYPE DEF-FCMPSELZD ENDOF
      fstore    OF c b c b FPR-TYPE c b MEM-TYPE A64IR-OPCODE:FSTORE DEF-STR ENDOF
      fload     OF c b c b FPR-TYPE c b MEM-TYPE A64IR-OPCODE:FLOAD DEF-LDR ENDOF
      fdload    OF c b c b FPR-TYPE c b MEM-TYPE A64IR-OPCODE:FDLOAD DEF-DLOAD ENDOF
      fdstore   OF c b c b FPR-TYPE c b MEM-TYPE A64IR-OPCODE:FDSTORE DEF-DSTORE ENDOF
      fdpush    OF c b c b FPR-TYPE c b MEM-TYPE A64IR-OPCODE:FDPUSH DEF-DPUSH ENDOF
      fdpop     OF c b c b FPR-TYPE c b MEM-TYPE A64IR-OPCODE:FDPOP DEF-DPOP ENDOF
      faload    OF c b c b GPR-TYPE c b FPR-TYPE c b MEM-TYPE A64IR-OPCODE:FALOAD DEF-ALDR ENDOF
      fastore   OF c b c b GPR-TYPE c b FPR-TYPE c b MEM-TYPE A64IR-OPCODE:FASTORE DEF-ASTR ENDOF
   ;MATCH ;

\ The form this dialect spells with a name. The table is the authority on the
\ spelling in both directions, so a name that is not one of them is refused as
\ the foreign form it is rather than defined as something near it.
: OP-NAMED ( ptr u8 n -- A64IR:opcode )
   {: a:ptr u:n :}
   OPCODES 0 ?do
      a u i NTH OP-NAME STR= if i NTH unloop exit then
   loop
   E-A64IR-OPCODE throw ;

public

\ Materialize only a requested opcode, retaining the module's schema as the
\ sole presence authority. OPCODE and BIND remain pure symbol interning.
: ENSURE-OP ( IR-CTX:ctx IR-BUILD:builder A64IR:opcode -- IR-ID:ir-symbol-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder o:A64IR:opcode :}
   c b DIALECT-CK
   c b o ORD MEMO-BIND {: op:IR-ID:ir-symbol-id :}
   c b op IR-BUILD:SCHEMA-DEFINED? 0= if c b o DEFINE-ONE then
   op ;

\ For a pass rebuilding a module: it holds only the copied module's spelling, a
\ symbol that means nothing in the module being written.
: ENSURE-NAMED ( IR-CTX:ctx IR-BUILD:builder ptr u8 n -- IR-ID:ir-symbol-id )
   OP-NAMED ENSURE-OP ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
