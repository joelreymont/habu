\ jit.f - C-EMITW, the row-comparison mode (CF-DEF-GUARD) habu2.f's row
\ registrars honour, and the native `:` compiler's abstract value stack (VS)
\ keyword rows: each *-ENTRY registrar emits a keyword's spelling test and its
\ handler, and the EMIT-*KW words lay down the strings the tests compare. Entry
\ tags: 0 x-register, 1 integer constant (no code yet), 2 d-register, 3 float
\ constant; VS state is in REGALLOC-ABI's DATA cells (src/habu/regalloc-abi.f).
\ Load after prof.f, before habu2.f.
require src/arch/arm64/icode.f
using A64ASM
using A64ICODE
using REGALLOC-ABI
using REGALLOC

package ENGINE-EMIT
public
variable LVSPILL   variable LVLITPUSH   variable LVPUSHC
variable LFBINPREP
variable LKWFPLUS  variable LKWFMINUS  variable LKWFSTAR  variable LKWFSLASH
variable LVFOLDPUT
variable LVMOVK  variable LVFORCEK  variable LVBINPREP  variable LVBINIPREP  variable LVPUSHR
variable LVNEED    variable LVDEST
private
 0 XDS  8 ENC-STRPOST  constant W-PUSHR   \ str xR,[x19],#8  (or with R)
 0 XDS -8 ENC-LDRPRE   constant W-POPR    \ ldr xR,[x19,#-8]!

public
: C-EMITW ( n -- ) 9 swap LIT64,  LCEMIT LABEL@ BL, ;

private

public
\ Row-comparison mode. A routine that asks whether a name spells a dispatch row
\ replays the row registrars with CF-DEF-GUARD set: each then emits only its
\ spelling test, a branch to the label CF-DEF-HIT holds when LKWCMP matches, in
\ place of its handler. The mode
\ exists only in this image generator; a generated engine holds the tests.
variable CF-DEF-GUARD
variable CF-DEF-HIT

: CF-DEF-GUARD-ROW ( ptr n n -- ) {: kwvar:ptr kwlen:n :}
   0 kwvar LABEL@ ADR,  1 kwlen MOVZ,  LKWCMP LABEL@ BL,
   0 CF-DEF-HIT @ CBNZ, ;

variable LKWPLUS  variable LKWMINUS  variable LKWSTAR  variable LKWDIV  variable LKWMOD
variable LKWAND2  variable LKWOR2   variable LKWXOR2
private
variable FESK

: VF+ ( -- )
   11 11 12 ADD, ;   \ fold helpers -- NOT f+/f-/f*: those are the FLOAT prims

: VF- ( -- )
   11 11 12 SUB, ;

: VF* ( -- )
   11 11 12 MUL, ;

: FAND ( -- )
   11 11 12 AND, ;

: FOR2 ( -- )
   11 11 12 ORR, ;

: FXOR2 ( -- )
   11 11 12 EOR, ;

public
\ The keyword-string label cells are global like every other LKW* cell in this
\ file: the habu2.f LABELS allocator (its OPS block) mints them, and it is not
\ this package.
variable LKWLSH  variable LKWRSH
;package
using ENGINE-EMIT

package ENGINE-EMIT

: EMIT-FOLDKW ( -- )
   LKWPLUS LABEL@ LBL,  s" +" BYTES,    LKWMINUS LABEL@ LBL,  s" -" BYTES,
   LKWSTAR LABEL@ LBL,  s" *" BYTES,    LKWAND2 LABEL@ LBL,   s" and" BYTES,
   LKWOR2 LABEL@ LBL,   s" or" BYTES,   LKWXOR2 LABEL@ LBL,   s" xor" BYTES,
   LKWLSH LABEL@ LBL,   s" lshift" BYTES,  LKWRSH LABEL@ LBL,  s" rshift" BYTES,
   LKWDIV LABEL@ LBL,   s" /" BYTES,       LKWMOD LABEL@ LBL,  s" mod" BYTES, ;

;package

package ENGINE-EMIT
public
variable LKWDUP2  variable LKWDROP2  variable LKWSWAP2  variable LKWOVER2  variable LKWNIP2

private
: EMIT-SHUFKW ( -- )
   LKWDUP2 LABEL@ LBL,   s" dup" BYTES,    LKWDROP2 LABEL@ LBL,  s" drop" BYTES,
   LKWSWAP2 LABEL@ LBL,  s" swap" BYTES,   LKWOVER2 LABEL@ LBL,  s" over" BYTES,
   LKWNIP2 LABEL@ LBL,   s" nip" BYTES, ;

;package

package ENGINE-EMIT
variable FESK2

: VOP-ENTRY ( label ptr n n [ -- ] [ -- ] -- ) {: lmainlbl:label kwvar:ptr kwlen:n foldxt emitxt :}
   CF-DEF-GUARD @ IF kwvar kwlen CF-DEF-GUARD-ROW exit THEN
   LBL FESK !  LBL FESK2 !
   0 kwvar LABEL@ ADR,  1 kwlen MOVZ,  LKWCMP LABEL@ BL,
   0 FESK LABEL@ CBZ,
   LVBINPREP LABEL@ BL,
   13 FESK LABEL@ CBZ,
   13 1 CMPI,  C-NE FESK2 LABEL@ BCOND,
      foldxt execute
      LVFOLDPUT LABEL@ BL,
      lmainlbl B,
   FESK2 LABEL@ LBL,
      emitxt execute
      9 8 14 ORR,  7 12 5 LSLI,  9 9 7 ORR,  7 15 16 LSLI,  9 9 7 ORR,  LCEMIT LABEL@ BL,
      lmainlbl B,
   FESK LABEL@ LBL, ;

variable FESK6
;package

package ENGINE-EMIT

: VOPI-ENTRY ( label ptr n n [ -- ] [ -- ] [ -- ] n -- ) {: lmainlbl:label kwvar:ptr kwlen:n foldxt emitxt immxt max:n :}
   CF-DEF-GUARD @ IF kwvar kwlen CF-DEF-GUARD-ROW exit THEN
   LBL FESK !  LBL FESK2 !  LBL FESK6 !
   0 kwvar LABEL@ ADR,  1 kwlen MOVZ,  LKWCMP LABEL@ BL,
   0 FESK LABEL@ CBZ,
   10 max MOVZ,
   LVBINIPREP LABEL@ BL,
   13 FESK LABEL@ CBZ,
   13 1 CMPI,  C-NE FESK2 LABEL@ BCOND,
      foldxt execute
      LVFOLDPUT LABEL@ BL,
      lmainlbl B,
   FESK2 LABEL@ LBL,
   13 3 CMPI,  C-NE FESK6 LABEL@ BCOND,
      immxt execute
      lmainlbl B,
   FESK6 LABEL@ LBL,
      emitxt execute
      9 8 14 ORR,  7 12 5 LSLI,  9 9 7 ORR,  7 15 16 LSLI,  9 9 7 ORR,  LCEMIT LABEL@ BL,
      lmainlbl B,
   FESK LABEL@ LBL, ;

\ vdivi-entry: `/` and `mod` by a literal divisor in 1..DIV-IMM-MAX divide in place (sdiv; `mod` as
\ habu1.f BMOD computes it, a - (a/b)*b) without the baked bodies' pops and zero test. A zero,
\ negative or wider literal and a register divisor keep the call and its ARITH-ABI:E-DIV-ZERO test,
\ and MIN-N -1 never reaches the row. The range test runs before LVBINIPREP, which keeps it from
\ mode 2: the reachable modes are 0, 1 and 3 (x14 quotient, x12 dividend, x15 divisor).
65535 constant DIV-IMM-MAX            \ one movz
$D2800010 constant W-MOVZ16           \ movz x16,#imm (or with imm<<5)
$9AD00C00 constant W-SDIV16           \ sdiv xd,xn,x16 (or with d, n<<5)
$9B108220 constant W-MSUB1716         \ msub xd,x17,x16,xa (or with d, a<<10)

\ Branch to skip unless the VS top is an integer constant in 1..DIV-IMM-MAX;
\ leaves x10 = DIV-IMM-MAX, LVBINIPREP's immediate bound.
: C-VDIV-TOP? ( label -- ) {: skip:label :}
   6 DATA VSP-CELL LDR,  6 skip CBZ,
   5 6 1 SUBI,  7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,
   7 1 CMPI,  C-NE skip BCOND,
   8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  12 8 0 LDR,
   12 1 CMPI,  C-LT skip BCOND,
   10 DIV-IMM-MAX MOVZ,  12 10 CMP,  C-GT skip BCOND, ;

: VDIVI-ENTRY ( label ptr n n [ -- ] [ -- ] -- ) {: lmainlbl:label kwvar:ptr kwlen:n foldxt immxt :}
   CF-DEF-GUARD @ IF kwvar kwlen CF-DEF-GUARD-ROW exit THEN
   LBL LBL {: skip imm :}
   0 kwvar LABEL@ ADR,  1 kwlen MOVZ,  LKWCMP LABEL@ BL,
   0 skip CBZ,
   skip C-VDIV-TOP?
   LVBINIPREP LABEL@ BL,
   13 skip CBZ,
   13 1 CMPI,  C-NE imm BCOND,
      foldxt execute
      LVFOLDPUT LABEL@ BL,
      lmainlbl B,
   imm LBL,
      immxt execute
      lmainlbl B,
   skip LBL, ;

: VF/ ( -- )
   11 11 12 SDIV, ;

: VFMOD ( -- )
   13 11 12 SDIV,  13 13 12 MUL,  11 11 13 SUB, ;

\ x16 and x17 are never pooled.
: EDIVK ( -- )
   8 W-MOVZ16 LIT64,  7 15 5 LSLI,  9 8 7 ORR,  LCEMIT LABEL@ BL, ;

: EI/ ( -- )
   EDIVK
   8 W-SDIV16 LIT64,  9 8 14 ORR,  7 12 5 LSLI,  9 9 7 ORR,  LCEMIT LABEL@ BL, ;

: EIMOD ( -- )
   EDIVK
   8 W-SDIV16 17 or LIT64,  7 12 5 LSLI,  9 8 7 ORR,  LCEMIT LABEL@ BL,
   8 W-MSUB1716 LIT64,  9 8 14 ORR,  7 12 10 LSLI,  9 9 7 ORR,  LCEMIT LABEL@ BL, ;

: FLSH ( -- )
   11 11 12 LSLV, ;

: FRSH ( -- )
   11 11 12 LSRV, ;

: E+ ( -- )
   8 $8B000000 LIT64, ;

: E- ( -- )
   8 $CB000000 LIT64, ;

: E* ( -- )
   8 $9B007C00 LIT64, ;

: EAND ( -- )
   8 $8A000000 LIT64, ;

: EOR2 ( -- )
   8 $AA000000 LIT64, ;

: EXOR ( -- )
   8 $CA000000 LIT64, ;

: ELSH ( -- )
   8 $9AC02000 LIT64, ;

: ERSH ( -- )
   8 $9AC02400 LIT64, ;

: EI2N ( -- )
   9 8 14 ORR,  7 12 5 LSLI,  9 9 7 ORR,  7 15 10 LSLI,  9 9 7 ORR,  LCEMIT LABEL@ BL, ;

: EI+ ( -- )
   8 $91000000 LIT64,  EI2N ;

: EI- ( -- )
   8 $D1000000 LIT64,  EI2N ;

: EILSH ( -- )
   8 $D3400000 LIT64,  9 8 14 ORR,  7 12 5 LSLI,  9 9 7 ORR,
   8 64 MOVZ,  8 8 15 SUB,  8 8 63 ANDI,  8 8 16 LSLI,  9 9 8 ORR,
   8 63 MOVZ,  8 8 15 SUB,  8 8 10 LSLI,  9 9 8 ORR,  LCEMIT LABEL@ BL, ;

: EIRSH ( -- )
   8 $D340FC00 LIT64,  9 8 14 ORR,  7 12 5 LSLI,  9 9 7 ORR,
   7 15 16 LSLI,  9 9 7 ORR,  LCEMIT LABEL@ BL, ;

;package

package ENGINE-EMIT
public
variable LKWEQ2  variable LKWNE2  variable LKWLT2  variable LKWGT2  variable LKWLE2  variable LKWGE2

private
\ comparison entry: fold -> dispatch computes the flag; registers -> emit
\ cmp rd,rm (cmp rd,#imm12 when the top is a constant in 0..4095) ;
\ csetm rd,cond  (Forth flag 0/-1)
: VCMP-ENTRY ( label ptr n n n -- ) {: lmainlbl:label kwvar:ptr kwlen:n cond:n :}
   CF-DEF-GUARD @ IF kwvar kwlen CF-DEF-GUARD-ROW exit THEN
   LBL FESK !  LBL FESK2 !  LBL FESK6 !  LBL {: flag :}
   0 kwvar LABEL@ ADR,  1 kwlen MOVZ,  LKWCMP LABEL@ BL,
   0 FESK LABEL@ CBZ,
   10 4095 MOVZ,
   LVBINIPREP LABEL@ BL,
   13 FESK LABEL@ CBZ,
   13 1 CMPI,  C-NE FESK2 LABEL@ BCOND,
      11 12 CMP,  11 cond CSETM,
      LVFOLDPUT LABEL@ BL,
      lmainlbl B,
   FESK2 LABEL@ LBL,
   13 3 CMPI,  C-NE FESK6 LABEL@ BCOND,
      8 $F100001F LIT64,  7 12 5 LSLI,  9 8 7 ORR,  7 15 10 LSLI,  9 9 7 ORR,  LCEMIT LABEL@ BL,
      flag B,
   FESK6 LABEL@ LBL,
      8 $EB00001F LIT64,  7 12 5 LSLI,  9 8 7 ORR,  7 15 16 LSLI,  9 9 7 ORR,  LCEMIT LABEL@ BL,
   flag LBL,
      8 $DA9F03E0 cond 1 xor 12 lshift or LIT64,  9 8 14 ORR,  LCEMIT LABEL@ BL,
      lmainlbl B,
   FESK LABEL@ LBL, ;

public
variable LVFUSE

private

: EMIT-CMPKW ( -- )
   LKWEQ2 LABEL@ LBL,  s" =" BYTES,    LKWNE2 LABEL@ LBL,  s" <>" BYTES,
   LKWLT2 LABEL@ LBL,  s" <" BYTES,    LKWGT2 LABEL@ LBL,  s" >" BYTES,
   LKWLE2 LABEL@ LBL,  s" <=" BYTES,   LKWGE2 LABEL@ LBL,  s" >=" BYTES, ;
public
variable LVDROP  variable LVSWAPX  variable LVNIPX  variable LVCOPY
private
$AA0003E0 constant W-MOVRR        \ orr rd,xzr,rs (| rd | rs<<16)

variable FESK5

\ FOP-ENTRY: a float binop on two forceable d-registers; anything else falls through.
: FOP-ENTRY ( label ptr n n n -- ) {: lmainlbl:label kwvar:ptr kwlen:n base:n :}
   CF-DEF-GUARD @ IF kwvar kwlen CF-DEF-GUARD-ROW exit THEN
   LBL FESK5 !
   0 kwvar LABEL@ ADR,  1 kwlen MOVZ,  LKWCMP LABEL@ BL,
   0 FESK5 LABEL@ CBZ,
   LFBINPREP LABEL@ BL,
   13 FESK5 LABEL@ CBZ,
   8 base LIT64,  9 8 14 ORR,  7 14 5 LSLI,  9 9 7 ORR,  7 15 16 LSLI,  9 9 7 ORR,  LCEMIT LABEL@ BL,
   lmainlbl B,
   FESK5 LABEL@ LBL, ;

: EMIT-FOPKW ( -- )
   LKWFPLUS LABEL@ LBL,  s" f+" BYTES,   LKWFMINUS LABEL@ LBL,  s" f-" BYTES,
   LKWFSTAR LABEL@ LBL,  s" f*" BYTES,   LKWFSLASH LABEL@ LBL,  s" f/" BYTES, ;

public
variable LVSNAP  variable LVRECON  variable LVRECONAT  variable LVFPOP  variable LVSETF  variable LVRESNAP

private

variable FESK3

\ vshuf-entry: reg-aware stack ops — relabels and register moves, no memory
\ traffic beyond pulling the need operands the op reads (LVNEED). A row that
\ reads no value (drop) pulls nothing: its handler refuses an empty VS itself.
: VSHUF-ENTRY ( label ptr n n n [ -- ] -- ) {: lmainlbl:label kwvar:ptr kwlen:n need:n sxt :}
   CF-DEF-GUARD @ IF kwvar kwlen CF-DEF-GUARD-ROW exit THEN
   LBL FESK3 !
   0 kwvar LABEL@ ADR,  1 kwlen MOVZ,  LKWCMP LABEL@ BL,
   0 FESK3 LABEL@ CBZ,
   need 0<> IF  5 need MOVZ,  LVNEED LABEL@ BL,  13 FESK3 LABEL@ CBZ,  THEN
   sxt execute
   13 FESK3 LABEL@ CBZ,
   lmainlbl B,
   FESK3 LABEL@ LBL, ;

: XDUP ( -- )
   6 DATA VSP-CELL LDR,  5 6 1 SUBI,  LVCOPY LABEL@ BL, ;

: XOVER ( -- )
   6 DATA VSP-CELL LDR,  5 6 2 SUBI,  LVCOPY LABEL@ BL, ;

: XDROP ( -- )
   LVDROP LABEL@ BL, ;

: XSWAP ( -- )
   LVSWAPX LABEL@ BL, ;

: XNIP ( -- )
   LVNIPX LABEL@ BL, ;
public
variable LKWINC  variable LKWDEC  variable LKWZEQ  variable LKWZLT
variable LKWNEG2  variable LKWINV2
variable LKWCFETCH  variable LKWCELLS  variable LKWCELLP  variable LKWPFIELD
private
variable FESK4

\ vun-entry: unary op on the VS top. Tag 1 (integer constant) folds at JIT time
\ (no code); tag 0 (integer register) gets one op into LVDEST's register (in
\ place unless the register is shared); tags 2 and 3 (float register, float
\ constant) fall through to the prim. An empty VS first pulls the operand off
\ the data stack (LVNEED); an exhausted pool falls through.
: VUN-ENTRY ( label ptr n n [ -- ] [ -- ] -- ) {: lmainlbl:label kwvar:ptr kwlen:n foldxt emitxt :}
   CF-DEF-GUARD @ IF kwvar kwlen CF-DEF-GUARD-ROW exit THEN
   LBL FESK4 !  LBL FESK2 !
   0 kwvar LABEL@ ADR,  1 kwlen MOVZ,  LKWCMP LABEL@ BL,
   0 FESK4 LABEL@ CBZ,
   5 1 MOVZ,  LVNEED LABEL@ BL,  13 FESK4 LABEL@ CBZ,
   6 DATA VSP-CELL LDR,
   5 6 1 SUBI,  7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,
   8 5 3 LSLI,  8 8 VVAL-OFF ADDI,  8 DATA 8 ADD,  11 8 0 LDR,
   7 FESK2 LABEL@ CBZ,
   7 1 CMPI,  C-NE FESK4 LABEL@ BCOND,
      foldxt execute
      11 8 0 STR,
      lmainlbl B,
   FESK2 LABEL@ LBL,
      LVDEST LABEL@ BL,  14 FESK4 LABEL@ CBZ,
      emitxt execute
      lmainlbl B,
   FESK4 LABEL@ LBL, ;

: FU1+ ( -- )
   11 11 1 ADDI, ;

: FU1- ( -- )
   11 11 1 SUBI, ;

: FUNEG ( -- )
   11 SP 11 SUB, ;

: FUINV ( -- )
   7 0 MOVN,  11 11 7 EOR, ;

: FU0= ( -- )
   11 0 CMPI,  11 0 CSETM, ;

: FU0< ( -- )
   11 0 CMPI,  11 11 CSETM, ;

: EU2R ( -- )
   9 8 14 ORR,  7 12 16 LSLI,  9 9 7 ORR,  LCEMIT LABEL@ BL, ;   \ base | rd | rm<<16

: EU2N ( -- )
   9 8 14 ORR,  7 12 5 LSLI,  9 9 7 ORR,  LCEMIT LABEL@ BL, ;    \ base | rd | rn<<5

: EU1+ ( -- )
   8 $91000400 LIT64,  EU2N ;

: EU1- ( -- )
   8 $D1000400 LIT64,  EU2N ;

: EUNEG ( -- )
   8 $CB0003E0 LIT64,  EU2R ;

: EUINV ( -- )
   8 $AA2003E0 LIT64,  EU2R ;

: EUCMP0 ( -- )
   8 $F100001F LIT64,  7 12 5 LSLI,  9 8 7 ORR,  LCEMIT LABEL@ BL, ;

: EUCSETM ( n -- ) {: cond:n :}
   8 $DA9F03E0 cond 1 xor 12 lshift or LIT64,
   9 8 14 ORR,
   LCEMIT LABEL@ BL, ;

: EU0= ( -- )
   EUCMP0  0 EUCSETM ;

: EU0< ( -- )
   EUCMP0  11 EUCSETM ;

: EMIT-UNKW ( -- )
   LKWINC LABEL@ LBL,   s" 1+" BYTES,      LKWDEC LABEL@ LBL,   s" 1-" BYTES,
   LKWZEQ LABEL@ LBL,   s" 0=" BYTES,      LKWZLT LABEL@ LBL,   s" 0<" BYTES,
   LKWNEG2 LABEL@ LBL,  s" negate" BYTES,  LKWINV2 LABEL@ LBL,  s" invert" BYTES, ;

\ Memory words on the VS: none of their primitive bodies carries a guard,
\ so one instruction on the operand registers is the whole word; `!` and the
\ other stores keep their guarded bodies (habu1.f PROT-GUARD:CALL).

\ vload-entry: a load from the VS top address into LVDEST's register (result tag 0).
\ A constant address (tag 1) is forced into a register first, so the load is a
\ runtime instruction: memory is never read at compile time. Tags 2 and 3, an
\ empty pull or an exhausted pool fall through to the primitive.
: VLOAD-ENTRY ( label ptr n n n -- ) {: lmainlbl:label kwvar:ptr kwlen:n base:n :}
   CF-DEF-GUARD @ IF kwvar kwlen CF-DEF-GUARD-ROW exit THEN
   LBL FESK4 !
   0 kwvar LABEL@ ADR,  1 kwlen MOVZ,  LKWCMP LABEL@ BL,
   0 FESK4 LABEL@ CBZ,
   5 1 MOVZ,  LVNEED LABEL@ BL,  13 FESK4 LABEL@ CBZ,
   6 DATA VSP-CELL LDR,  5 6 1 SUBI,
   7 5 VTAG-OFF ADDI,  7 DATA 7 ADD,  7 7 0 LDRB,
   7 2 CMPI,  C-GE FESK4 LABEL@ BCOND,
   LVFORCEK LABEL@ BL,  14 FESK4 LABEL@ CBZ,
   LVDEST LABEL@ BL,  14 FESK4 LABEL@ CBZ,
   8 base LIT64,  EU2N
   lmainlbl B,
   FESK4 LABEL@ LBL, ;

$F9400000 constant W-LDR0        \ ldr xT,[xN]  (| rt | rn<<5)
$39400000 constant W-LDRB0       \ ldrb wT,[xN], zero-extending

: FUCELLS ( -- )
   11 11 3 LSLI, ;

: FUCELLP ( -- )
   11 11 8 ADDI, ;

: EUCELLS ( -- )
   8 $D37DF000 LIT64,  EU2N ;    \ lsl xd,xn,#3 (ubfm xd,xn,#61,#60)

: EUCELLP ( -- )
   8 $91002000 LIT64,  EU2N ;    \ add xd,xn,#8

\ ptr-field ( base index -- base+index*8 ): the slot address, not its contents.
: FPFIELD ( -- )
   12 12 3 LSLI,  11 11 12 ADD, ;

: EPFIELD ( -- )
   8 $8B000C00 LIT64, ;          \ add xd,xn,xm,lsl #3

: EMIT-MEMKW ( -- )
   LKWCFETCH LABEL@ LBL,  s" c@" BYTES,     LKWCELLS LABEL@ LBL,   s" cells" BYTES,
   LKWCELLP LABEL@ LBL,   s" cell+" BYTES,  LKWPFIELD LABEL@ LBL,  s" ptr-field" BYTES, ;
;package

;using
;using
;using
;using
;using
