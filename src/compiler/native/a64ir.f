\ a64ir.f - the ARM64 machine dialect: the closed set of operations that stand
\ for real ARM64 instruction forms, with virtual registers as SSA values.
\
\ A condition is four bits, and what they MEAN depends on which instruction wrote
\ the flags. Fcmp raises the unordered condition for a NaN by setting
\ N=0 Z=0 C=1 V=1, and against those bits the conditions read:
\
\   lt    11  N != V          TRUE on unordered
\   le    13  Z=1 or N != V   TRUE
\   ne     1  Z = 0           TRUE
\   ge    10  N  = V          false
\   gt    12  Z=0 and N = V   false
\   equal  0  Z = 1           false
\   mi     4  N = 1           false
\
\ So a float less-than lowers under `mi` and never under `lt`, and the three
\ conditions the float comparisons reach - mi, gt and equal - are exactly the
\ three that are false on unordered. That is why every float comparison this
\ engine has answers false for a NaN.

require lib/prelude.f
require lib/errors.f
require src/compiler/a64-effect.f
require src/compiler/target.f
require src/compiler/binding.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/type.f
require src/compiler/ir/schema.f
require src/compiler/ir/build.f
require src/arch/arm64/asm.f

package A64IR
public

\ An ENUM, so a selection rule cannot name an operation this dialect does not
\ have and every MATCH over it has to answer for every member.
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
   aload
   astore
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
;ENUM

\ One condition per SOURCE relation, so a lowering is never an operand order in
\ one place and a condition in another. `equal` is spelled so because the ENUM's
\ derived comparison word takes `eq`. `mi` is the seventh, for the float forms.
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
OFF-MAX dup A64EFF:SP-ALIGN mod - constant FRAME-LIM

\ ---- the condition field -----------------------------------------------------
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
   CTARGET-ARCH:AARCH64 CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH
   IR-SCHEMA:SET-TARGET ;

\ A value-producing machine operation ends no block, names no successor, holds
\ no region and carries no effect token.
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
10 constant MINOR

\ ---- the machine bounds, for a consumer that has to agree with them -----------
: REG-BITS ( -- n )      XBITS ;
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

\ A small enumeration rather than a flag, so the CODE kind is a new member.
0 constant ADDR-NONE
1 constant ADDR-DATA
ADDR-DATA constant ADDR-KIND-MAX

private

\ ---- checked move-wide operands ----------------------------------------------
: IMM16 ( n -- n )
   dup 0 < over IMM-LIM >= or if E-A64IR-IMM throw then ;

: HALF ( n -- n )
   dup 0 < over XBITS >= or if E-A64IR-SHIFT throw then
   dup HALF-N mod 0<> if E-A64IR-SHIFT throw then ;

\ An unknown number would reach the emitter as a site class it has no rule for.
: ADDR-KIND ( n -- n )
   dup 0 < over ADDR-KIND-MAX > or if E-A64IR-IMM throw then ;

\ ---- checked frame operands --------------------------------------------------
: SLOT ( n -- n )
   dup 0 < if E-A64IR-SLOT throw then
   dup SLOT-BYTES mod 0<> if E-A64IR-SLOT throw then
   dup SLOT-BYTES A64EFF:SLOT-REACH > if E-A64IR-SLOT throw then ;

\ The stack pointer stays aligned, the frame stays inside the region A64EFF can
\ describe, and it stays inside the one immediate that claims it.
: FRAME ( n -- n )
   dup 0 < if E-A64IR-FRAME throw then
   dup A64EFF:SP-ALIGN mod 0<> if E-A64IR-FRAME throw then
   dup A64EFF:FRAME-MAX > if E-A64IR-FRAME throw then
   dup FRAME-LIM > if E-A64IR-FRAME throw then ;

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
   dup A64EFF:SLOT-BACK negate < if E-A64IR-DSLOT throw then
   dup SLOT-BYTES mod 0<> if E-A64IR-DSLOT throw then
   dup SLOT-BYTES A64EFF:SLOT-REACH > if E-A64IR-DSLOT throw then ;

\ A whole number of cells, and the same twelve-bit field serves the Add and the
\ Sub, so what is bounded is the MAGNITUDE. The data stack is cell-aligned.
: DBYTES ( n -- n )
   dup SLOT-BYTES mod 0<> if E-A64IR-DBYTES throw then
   dup abs OFF-MAX > if E-A64IR-DBYTES throw then ;

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

\ ---- the type of a virtual register ------------------------------------------
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

: FRAME-LIMIT ( -- n )   FRAME-LIM ;

: OFF-LIMIT ( -- n )     OFF-MAX ;

\ A pass CHOOSING between the immediate and the register form asks this; the
\ bound at MASK is what makes a pass that forgot fail loudly.
: MASK-IMM? ( n -- bool )   A64ASM:LIMM? ;

\ ---- the opcode names --------------------------------------------------------
: OPCODE ( IR-CTX:ctx IR-BUILD:builder A64IR:opcode -- IR-ID:ir-symbol-id )
   MATCH opcode
      movz    OF s" a64.movz"    ENDOF
      movk    OF s" a64.movk"    ENDOF
      mov     OF s" a64.mov"     ENDOF
      add     OF s" a64.add"     ENDOF
      sub     OF s" a64.sub"     ENDOF
      mul     OF s" a64.mul"     ENDOF
      sdiv    OF s" a64.sdiv"    ENDOF
      and     OF s" a64.and"     ENDOF
      orr     OF s" a64.orr"     ENDOF
      eor     OF s" a64.eor"     ENDOF
      lslv    OF s" a64.lslv"    ENDOF
      lsrv    OF s" a64.lsrv"    ENDOF
      mvn     OF s" a64.mvn"     ENDOF
      store    OF s" a64.str"      ENDOF
      load     OF s" a64.ldr"      ENDOF
      reserve  OF s" a64.reserve"  ENDOF
      release  OF s" a64.release"  ENDOF
      dtake    OF s" a64.dtake"    ENDOF
      dload    OF s" a64.dload"    ENDOF
      dstore   OF s" a64.dstore"   ENDOF
      dpublish OF s" a64.dpublish" ENDOF
      aload    OF s" a64.aldr"     ENDOF
      astore   OF s" a64.astr"     ENDOF
      abload   OF s" a64.aldrb"    ENDOF
      abstore  OF s" a64.astrb"    ENDOF
      flag     OF s" a64.flag"     ENDOF
      selz     OF s" a64.selz"     ENDOF
      cmpsel   OF s" a64.cmpsel"   ENDOF
      br       OF s" a64.b"        ENDOF
      brz      OF s" a64.cbz"      ENDOF
      cmpbr    OF s" a64.cmpbr"    ENDOF
      call     OF s" a64.call"     ENDOF
      wordcall OF s" a64.wordcall" ENDOF
      linksave OF s" a64.lnkstr"   ENDOF
      linkload OF s" a64.lnkldr"   ENDOF
      ret      OF s" a64.ret"      ENDOF
      fadd     OF s" a64.fadd" ENDOF
      fsub     OF s" a64.fsub" ENDOF
      fmul     OF s" a64.fmul" ENDOF
      fdiv     OF s" a64.fdiv" ENDOF
      fneg     OF s" a64.fneg" ENDOF
      fabs     OF s" a64.fabs" ENDOF
      fsqrt    OF s" a64.fsqrt" ENDOF
      scvtf    OF s" a64.scvtf" ENDOF
      fcvtzs   OF s" a64.fcvtzs" ENDOF
      fmovxd   OF s" a64.fmovxd" ENDOF
      fmovdx   OF s" a64.fmovdx" ENDOF
      fmovdd   OF s" a64.fmovdd" ENDOF
      fload    OF s" a64.fldr" ENDOF
      fstore   OF s" a64.fstr" ENDOF
      faload   OF s" a64.faldr" ENDOF
      fastore  OF s" a64.fastr" ENDOF
      fdload   OF s" a64.fdload" ENDOF
      fdstore  OF s" a64.fdstore" ENDOF
      fflag    OF s" a64.fflag" ENDOF
      fflagz   OF s" a64.fflagz" ENDOF
      fcmpbr   OF s" a64.fcmpbr" ENDOF
      fcmpbrz  OF s" a64.fcmpbrz" ENDOF
      selzd    OF s" a64.selzd" ENDOF
      cmpseld  OF s" a64.cmpseld" ENDOF
      fcmpsel   OF s" a64.fcmpsel" ENDOF
      fcmpselz  OF s" a64.fcmpselz" ENDOF
      fcmpseld  OF s" a64.fcmpseld" ENDOF
      fcmpselzd OF s" a64.fcmpselzd" ENDOF
      tailcall  OF s" a64.tailcall" ENDOF
      trap      OF s" a64.trap"     ENDOF
      madd      OF s" a64.madd"     ENDOF
      addi      OF s" a64.addi"     ENDOF
      subi      OF s" a64.subi"     ENDOF
      movn      OF s" a64.movn"     ENDOF
      andi      OF s" a64.andi"     ENDOF
      orri      OF s" a64.orri"     ENDOF
      eori      OF s" a64.eori"     ENDOF
      flagi     OF s" a64.flagi"    ENDOF
      cmpbri    OF s" a64.cmpbri"   ENDOF
      codeaddr  OF s" a64.codeaddr" ENDOF
   ;MATCH
   IR-BUILD:INTERN-SYMBOL ;

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

: N>COND ( n -- A64IR:cond )
   case
      COND-LT of A64IR-COND:LT endof
      COND-LE of A64IR-COND:LE endof
      COND-GT of A64IR-COND:GT endof
      COND-GE of A64IR-COND:GE endof
      COND-EQ of A64IR-COND:EQUAL endof
      COND-NE of A64IR-COND:NE endof
      COND-MI of A64IR-COND:MI endof
      E-A64IR-COND throw
   endcase ;

\ ---- the reach of each branch form -------------------------------------------
: B-FITS? ( n -- bool )      B-BITS FITS? ;
: BZ-FITS? ( n -- bool )     BZ-BITS FITS? ;
: BCOND-FITS? ( n -- bool )  BCOND-BITS FITS? ;

: ADR-FITS? ( n -- bool )    ADR-BITS FITS? ;

: KEY-IMM ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" a64.imm" IR-BUILD:INTERN-SYMBOL ;

: KEY-SHIFT ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" a64.shift" IR-BUILD:INTERN-SYMBOL ;

\ A move-wide starting an address chain has to be found again after
\ publication, and a relocation pass may not decode region bytes. It is a
\ REQUIRED key, so a rewrite that drops it stops the compilation.
: KEY-ADDR ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" a64.addr" IR-BUILD:INTERN-SYMBOL ;

: IMM-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   IMM16 IR-BUILD:INTERN-INT-ATTR ;

: SHIFT-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   HALF IR-BUILD:INTERN-INT-ATTR ;

: ADDR-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   ADDR-KIND IR-BUILD:INTERN-INT-ATTR ;

: KEY-SLOT ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" a64.slot" IR-BUILD:INTERN-SYMBOL ;

: KEY-FRAME ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" a64.frame" IR-BUILD:INTERN-SYMBOL ;

: SLOT-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   SLOT IR-BUILD:INTERN-INT-ATTR ;

: FRAME-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   FRAME IR-BUILD:INTERN-INT-ATTR ;

\ Its own key and not the move-wide's, because the fields have different widths.
\ One key serves four forms because all four share one twelve-bit field.
: KEY-OFF ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" a64.off" IR-BUILD:INTERN-SYMBOL ;

: OFF-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   OFF IR-BUILD:INTERN-INT-ATTR ;

\ Its own key because the two fields admit different values: the logical one
\ carries the mask -2, which the arithmetic field cannot hold at all.
: KEY-MASK ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" a64.mask" IR-BUILD:INTERN-SYMBOL ;

: MASK-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   MASK IR-BUILD:INTERN-INT-ATTR ;

: KEY-DSLOT ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" a64.dslot" IR-BUILD:INTERN-SYMBOL ;

: KEY-DBYTES ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" a64.dbytes" IR-BUILD:INTERN-SYMBOL ;

: KEY-DBACK ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" a64.dback" IR-BUILD:INTERN-SYMBOL ;

: KEY-ENTRY ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" a64.entry" IR-BUILD:INTERN-SYMBOL ;

\ Its own key, because two passes recognise a tail branch by its ATTRIBUTES and
\ a trap under `a64.entry` would BE one to both of them.
: KEY-TRAP-ENTRY ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" a64.trap-entry" IR-BUILD:INTERN-SYMBOL ;

\ An ordinal and not an address: there is no address until the emitter has laid
\ the emission out. How many functions there are is the emitter's fact.
: KEY-FUN ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" a64.fun" IR-BUILD:INTERN-SYMBOL ;

: FUN-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   FUN-ORD IR-BUILD:INTERN-INT-ATTR ;

: KEY-COND ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" a64.cond" IR-BUILD:INTERN-SYMBOL ;

: COND-ATTR ( IR-CTX:ctx IR-BUILD:builder A64IR:cond -- IR-ID:ir-attr-id )
   COND-CODE COND IR-BUILD:INTERN-INT-ATTR ;

: DSLOT-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   DSLOT IR-BUILD:INTERN-INT-ATTR ;

: DBYTES-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   DBYTES IR-BUILD:INTERN-INT-ATTR ;

: DBACK-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   DBYTES IR-BUILD:INTERN-INT-ATTR ;

: ENTRY-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   ENTRY IR-BUILD:INTERN-INT-ATTR ;

private

\ ---- the schema definitions --------------------------------------------------
: RULE ( IR-CTX:ctx IR-BUILD:builder A64IR:opcode -- IR-ID:ir-symbol-id )
   MATCH opcode
      movz    OF s" a64.rule.movz"    ENDOF
      movk    OF s" a64.rule.movk"    ENDOF
      mov     OF s" a64.rule.mov"     ENDOF
      add     OF s" a64.rule.add"     ENDOF
      sub     OF s" a64.rule.sub"     ENDOF
      mul     OF s" a64.rule.mul"     ENDOF
      sdiv    OF s" a64.rule.sdiv"    ENDOF
      and     OF s" a64.rule.and"     ENDOF
      orr     OF s" a64.rule.orr"     ENDOF
      eor     OF s" a64.rule.eor"     ENDOF
      lslv    OF s" a64.rule.lslv"    ENDOF
      lsrv    OF s" a64.rule.lsrv"    ENDOF
      mvn     OF s" a64.rule.mvn"     ENDOF
      store    OF s" a64.rule.str"      ENDOF
      load     OF s" a64.rule.ldr"      ENDOF
      reserve  OF s" a64.rule.reserve"  ENDOF
      release  OF s" a64.rule.release"  ENDOF
      dtake    OF s" a64.rule.dtake"    ENDOF
      dload    OF s" a64.rule.dload"    ENDOF
      dstore   OF s" a64.rule.dstore"   ENDOF
      dpublish OF s" a64.rule.dpublish" ENDOF
      aload    OF s" a64.rule.aldr"     ENDOF
      astore   OF s" a64.rule.astr"     ENDOF
      abload   OF s" a64.rule.aldrb"    ENDOF
      abstore  OF s" a64.rule.astrb"    ENDOF
      flag     OF s" a64.rule.flag"     ENDOF
      selz     OF s" a64.rule.selz"     ENDOF
      cmpsel   OF s" a64.rule.cmpsel"   ENDOF
      br       OF s" a64.rule.b"        ENDOF
      brz      OF s" a64.rule.cbz"      ENDOF
      cmpbr    OF s" a64.rule.cmpbr"    ENDOF
      call     OF s" a64.rule.call"     ENDOF
      wordcall OF s" a64.rule.wordcall" ENDOF
      linksave OF s" a64.rule.lnkstr"   ENDOF
      linkload OF s" a64.rule.lnkldr"   ENDOF
      ret      OF s" a64.rule.ret"      ENDOF
      fadd     OF s" a64.rule.fadd" ENDOF
      fsub     OF s" a64.rule.fsub" ENDOF
      fmul     OF s" a64.rule.fmul" ENDOF
      fdiv     OF s" a64.rule.fdiv" ENDOF
      fneg     OF s" a64.rule.fneg" ENDOF
      fabs     OF s" a64.rule.fabs" ENDOF
      fsqrt    OF s" a64.rule.fsqrt" ENDOF
      scvtf    OF s" a64.rule.scvtf" ENDOF
      fcvtzs   OF s" a64.rule.fcvtzs" ENDOF
      fmovxd   OF s" a64.rule.fmovxd" ENDOF
      fmovdx   OF s" a64.rule.fmovdx" ENDOF
      fmovdd   OF s" a64.rule.fmovdd" ENDOF
      fload    OF s" a64.rule.fldr" ENDOF
      fstore   OF s" a64.rule.fstr" ENDOF
      faload   OF s" a64.rule.faldr" ENDOF
      fastore  OF s" a64.rule.fastr" ENDOF
      fdload   OF s" a64.rule.fdload" ENDOF
      fdstore  OF s" a64.rule.fdstore" ENDOF
      fflag    OF s" a64.rule.fflag" ENDOF
      fflagz   OF s" a64.rule.fflagz" ENDOF
      fcmpbr   OF s" a64.rule.fcmpbr" ENDOF
      fcmpbrz  OF s" a64.rule.fcmpbrz" ENDOF
      selzd    OF s" a64.rule.selzd" ENDOF
      cmpseld  OF s" a64.rule.cmpseld" ENDOF
      fcmpsel   OF s" a64.rule.fcmpsel" ENDOF
      fcmpselz  OF s" a64.rule.fcmpselz" ENDOF
      fcmpseld  OF s" a64.rule.fcmpseld" ENDOF
      fcmpselzd OF s" a64.rule.fcmpselzd" ENDOF
      tailcall  OF s" a64.rule.tailcall" ENDOF
      trap      OF s" a64.rule.trap"     ENDOF
      madd      OF s" a64.rule.madd"     ENDOF
      addi      OF s" a64.rule.addi"     ENDOF
      subi      OF s" a64.rule.subi"     ENDOF
      movn      OF s" a64.rule.movn"     ENDOF
      andi      OF s" a64.rule.andi"     ENDOF
      orri      OF s" a64.rule.orri"     ENDOF
      eori      OF s" a64.rule.eori"     ENDOF
      flagi     OF s" a64.rule.flagi"    ENDOF
      cmpbri    OF s" a64.rule.cmpbri"   ENDOF
      codeaddr  OF s" a64.rule.codeaddr" ENDOF
   ;MATCH
   IR-BUILD:INTERN-SYMBOL ;

: RENDERER ( IR-CTX:ctx IR-BUILD:builder A64IR:opcode -- IR-ID:ir-symbol-id )
   MATCH opcode
      movz    OF s" a64.render.movz"    ENDOF
      movk    OF s" a64.render.movk"    ENDOF
      mov     OF s" a64.render.mov"     ENDOF
      add     OF s" a64.render.add"     ENDOF
      sub     OF s" a64.render.sub"     ENDOF
      mul     OF s" a64.render.mul"     ENDOF
      sdiv    OF s" a64.render.sdiv"    ENDOF
      and     OF s" a64.render.and"     ENDOF
      orr     OF s" a64.render.orr"     ENDOF
      eor     OF s" a64.render.eor"     ENDOF
      lslv    OF s" a64.render.lslv"    ENDOF
      lsrv    OF s" a64.render.lsrv"    ENDOF
      mvn     OF s" a64.render.mvn"     ENDOF
      store    OF s" a64.render.str"      ENDOF
      load     OF s" a64.render.ldr"      ENDOF
      reserve  OF s" a64.render.reserve"  ENDOF
      release  OF s" a64.render.release"  ENDOF
      dtake    OF s" a64.render.dtake"    ENDOF
      dload    OF s" a64.render.dload"    ENDOF
      dstore   OF s" a64.render.dstore"   ENDOF
      dpublish OF s" a64.render.dpublish" ENDOF
      aload    OF s" a64.render.aldr"     ENDOF
      astore   OF s" a64.render.astr"     ENDOF
      abload   OF s" a64.render.aldrb"    ENDOF
      abstore  OF s" a64.render.astrb"    ENDOF
      flag     OF s" a64.render.flag"     ENDOF
      selz     OF s" a64.render.selz"     ENDOF
      cmpsel   OF s" a64.render.cmpsel"   ENDOF
      br       OF s" a64.render.b"        ENDOF
      brz      OF s" a64.render.cbz"      ENDOF
      cmpbr    OF s" a64.render.cmpbr"    ENDOF
      call     OF s" a64.render.call"     ENDOF
      wordcall OF s" a64.render.wordcall" ENDOF
      linksave OF s" a64.render.lnkstr"   ENDOF
      linkload OF s" a64.render.lnkldr"   ENDOF
      ret      OF s" a64.render.ret"      ENDOF
      fadd     OF s" a64.render.fadd" ENDOF
      fsub     OF s" a64.render.fsub" ENDOF
      fmul     OF s" a64.render.fmul" ENDOF
      fdiv     OF s" a64.render.fdiv" ENDOF
      fneg     OF s" a64.render.fneg" ENDOF
      fabs     OF s" a64.render.fabs" ENDOF
      fsqrt    OF s" a64.render.fsqrt" ENDOF
      scvtf    OF s" a64.render.scvtf" ENDOF
      fcvtzs   OF s" a64.render.fcvtzs" ENDOF
      fmovxd   OF s" a64.render.fmovxd" ENDOF
      fmovdx   OF s" a64.render.fmovdx" ENDOF
      fmovdd   OF s" a64.render.fmovdd" ENDOF
      fload    OF s" a64.render.fldr" ENDOF
      fstore   OF s" a64.render.fstr" ENDOF
      faload   OF s" a64.render.faldr" ENDOF
      fastore  OF s" a64.render.fastr" ENDOF
      fdload   OF s" a64.render.fdload" ENDOF
      fdstore  OF s" a64.render.fdstore" ENDOF
      fflag    OF s" a64.render.fflag" ENDOF
      fflagz   OF s" a64.render.fflagz" ENDOF
      fcmpbr   OF s" a64.render.fcmpbr" ENDOF
      fcmpbrz  OF s" a64.render.fcmpbrz" ENDOF
      selzd    OF s" a64.render.selzd" ENDOF
      cmpseld  OF s" a64.render.cmpseld" ENDOF
      fcmpsel   OF s" a64.render.fcmpsel" ENDOF
      fcmpselz  OF s" a64.render.fcmpselz" ENDOF
      fcmpseld  OF s" a64.render.fcmpseld" ENDOF
      fcmpselzd OF s" a64.render.fcmpselzd" ENDOF
      tailcall  OF s" a64.render.tailcall" ENDOF
      trap      OF s" a64.render.trap"     ENDOF
      madd      OF s" a64.render.madd"     ENDOF
      addi      OF s" a64.render.addi"     ENDOF
      subi      OF s" a64.render.subi"     ENDOF
      movn      OF s" a64.render.movn"     ENDOF
      andi      OF s" a64.render.andi"     ENDOF
      orri      OF s" a64.render.orri"     ENDOF
      eori      OF s" a64.render.eori"     ENDOF
      flagi     OF s" a64.render.flagi"    ENDOF
      cmpbri    OF s" a64.render.cmpbri"   ENDOF
      codeaddr  OF s" a64.render.codeaddr" ENDOF
   ;MATCH
   IR-BUILD:INTERN-SYMBOL ;

: NAMED ( IR-CTX:ctx IR-BUILD:builder A64IR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder o:A64IR:opcode :}
   c b o RULE IR-SCHEMA:SET-RULE
   c b o RENDERER IR-SCHEMA:SET-RENDERER ;

\ All three forms share the list, and MOVN's answer is constrained by the
\ emitter, which refuses a movn claiming to carry an address.
: MOVE-ATTRS ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b KEY-IMM IR-SCHEMA:ADD-ATTR
   c b KEY-SHIFT IR-SCHEMA:ADD-ATTR
   c b KEY-ADDR IR-SCHEMA:ADD-ATTR ;

: DEF-MOVZ ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:MOVZ OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-RESULT
   c b MOVE-ATTRS
   PURE-VALUE
   TOTAL
   TARGET
   c b A64IR-OPCODE:MOVZ NAMED
   c b IR-BUILD:DEFINE-OP ;

: DEF-CODEADDR ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:CODEADDR OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-RESULT
   c b KEY-FUN IR-SCHEMA:ADD-ATTR
   PURE-VALUE
   TOTAL
   TARGET
   c b A64IR-OPCODE:CODEADDR NAMED
   c b IR-BUILD:DEFINE-OP ;

: DEF-MOVN ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:MOVN OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-RESULT
   c b MOVE-ATTRS
   PURE-VALUE
   TOTAL
   TARGET
   c b A64IR-OPCODE:MOVN NAMED
   c b IR-BUILD:DEFINE-OP ;

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
   c b A64IR-OPCODE:MOVK NAMED
   c b IR-BUILD:DEFINE-OP ;

: DEF-UNARY ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id A64IR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id o:A64IR:opcode :}
   c b o OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   PURE-VALUE
   TOTAL
   TARGET
   c b o NAMED
   c b IR-BUILD:DEFINE-OP ;

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
   c b o NAMED
   c b IR-BUILD:DEFINE-OP ;

: DEF-LOGICAL-IMM ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id A64IR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id o:A64IR:opcode :}
   c b o OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   c b KEY-MASK IR-SCHEMA:ADD-ATTR
   PURE-VALUE
   TOTAL
   TARGET
   c b o NAMED
   c b IR-BUILD:DEFINE-OP ;

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
   c b o NAMED
   c b IR-BUILD:DEFINE-OP ;

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
   c b A64IR-OPCODE:MADD NAMED
   c b IR-BUILD:DEFINE-OP ;

\ The one form that may raise, and three instructions: branch over the trap when
\ the divisor is not zero, the trap, the divide - which is what the engine's `/` is.
: DEF-SDIV ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:SDIV OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   PURE-VALUE
   true IR-SCHEMA:SET-TRAP
   TARGET
   c b A64IR-OPCODE:SDIV NAMED
   c b IR-BUILD:DEFINE-OP ;

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
   c b o NAMED
   c b IR-BUILD:DEFINE-OP ;

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
   c b o NAMED
   c b IR-BUILD:DEFINE-OP ;

: DEF-RESERVE ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:RESERVE OPCODE IR-SCHEMA:BEGIN-OP
   k IR-SCHEMA:ADD-RESULT
   c b KEY-FRAME IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:WRITE FRAME-MEM
   TOTAL
   TARGET
   c b A64IR-OPCODE:RESERVE NAMED
   c b IR-BUILD:DEFINE-OP ;

: DEF-RELEASE ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:RELEASE OPCODE IR-SCHEMA:BEGIN-OP
   k IR-SCHEMA:ADD-OPERAND
   c b KEY-FRAME IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:WRITE FRAME-MEM
   TOTAL
   TARGET
   c b A64IR-OPCODE:RELEASE NAMED
   c b IR-BUILD:DEFINE-OP ;

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
   c b A64IR-OPCODE:DTAKE NAMED
   c b IR-BUILD:DEFINE-OP ;

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
   c b o NAMED
   c b IR-BUILD:DEFINE-OP ;

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
   c b o NAMED
   c b IR-BUILD:DEFINE-OP ;

: DEF-DPUBLISH ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:DPUBLISH OPCODE IR-SCHEMA:BEGIN-OP
   k IR-SCHEMA:ADD-OPERAND
   c b KEY-DBYTES IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:WRITE DSTACK-MEM
   TOTAL
   TARGET
   c b A64IR-OPCODE:DPUBLISH NAMED
   c b IR-BUILD:DEFINE-OP ;

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
   c b o NAMED
   c b IR-BUILD:DEFINE-OP ;

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
   c b o NAMED
   c b IR-BUILD:DEFINE-OP ;

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
   c b A64IR-OPCODE:ABLOAD NAMED
   c b IR-BUILD:DEFINE-OP ;

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
   c b A64IR-OPCODE:ABSTORE NAMED
   c b IR-BUILD:DEFINE-OP ;

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
   c b A64IR-OPCODE:FLAG NAMED
   c b IR-BUILD:DEFINE-OP ;

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
   c b A64IR-OPCODE:FLAGI NAMED
   c b IR-BUILD:DEFINE-OP ;

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
   c b A64IR-OPCODE:SELZ NAMED
   c b IR-BUILD:DEFINE-OP ;

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
   c b A64IR-OPCODE:CMPSEL NAMED
   c b IR-BUILD:DEFINE-OP ;

\ ---- the two branch forms ----------------------------------------------------
: DEF-BR ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:BR OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND-TAIL
   true 1 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE
   TOTAL
   TARGET
   c b A64IR-OPCODE:BR NAMED
   c b IR-BUILD:DEFINE-OP ;

: DEF-BRZ ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:BRZ OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   true 2 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE
   TOTAL
   TARGET
   c b A64IR-OPCODE:BRZ NAMED
   c b IR-BUILD:DEFINE-OP ;

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
   c b A64IR-OPCODE:CMPBR NAMED
   c b IR-BUILD:DEFINE-OP ;

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
   c b A64IR-OPCODE:CMPBRI NAMED
   c b IR-BUILD:DEFINE-OP ;

: DEF-RET ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:RET OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND-TAIL
   true 0 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE
   TOTAL
   TARGET
   c b A64IR-OPCODE:RET NAMED
   c b IR-BUILD:DEFINE-OP ;

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
   c b A64IR-OPCODE:CALL NAMED
   c b IR-BUILD:DEFINE-OP ;

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
   c b A64IR-OPCODE:WORDCALL NAMED
   c b IR-BUILD:DEFINE-OP ;

\ A TERMINATOR and not a call. It carries NO adjustment, which is what makes it
\ one instruction: the selector only chooses it where the pointer already stands
\ at the callee's entry base. It takes the data-stack order and ends it.
: DEF-TAILCALL ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:TAILCALL OPCODE IR-SCHEMA:BEGIN-OP
   k IR-SCHEMA:ADD-OPERAND
   c b KEY-ENTRY IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:WRITE DSTACK-TERM-MEM
   true IR-SCHEMA:SET-TRAP
   TARGET
   c b A64IR-OPCODE:TAILCALL NAMED
   c b IR-BUILD:DEFINE-OP ;

\ Its own form and not the tail branch with another target: a tail branch is how
\ a routine RETURNS, and this publishes nothing and comes back from nowhere. It
\ carries an adjustment, over the one cell the trap routine takes.
: DEF-TRAP ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:TRAP OPCODE IR-SCHEMA:BEGIN-OP
   k IR-SCHEMA:ADD-OPERAND
   c b KEY-TRAP-ENTRY IR-SCHEMA:ADD-ATTR
   c b KEY-DBYTES IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:WRITE DSTACK-TERM-MEM
   true IR-SCHEMA:SET-TRAP
   TARGET
   c b A64IR-OPCODE:TRAP NAMED
   c b IR-BUILD:DEFINE-OP ;

\ The register they move is x30, which is named by the FORM: A64EFF keeps it out
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
   c b A64IR-OPCODE:LINKSAVE NAMED
   c b IR-BUILD:DEFINE-OP ;

: DEF-LNKLDR ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:LINKLOAD OPCODE IR-SCHEMA:BEGIN-OP
   k IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-RESULT
   c b KEY-SLOT IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:READ FRAME-MEM
   TOTAL
   TARGET
   c b A64IR-OPCODE:LINKLOAD NAMED
   c b IR-BUILD:DEFINE-OP ;

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
   c b o NAMED
   c b IR-BUILD:DEFINE-OP ;

: DEF-FCROSS ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id A64IR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder ti:IR-ID:ir-type-id to:IR-ID:ir-type-id
      o:A64IR:opcode :}
   c b o OPCODE IR-SCHEMA:BEGIN-OP
   ti IR-SCHEMA:ADD-OPERAND
   to IR-SCHEMA:ADD-RESULT
   PURE-VALUE
   TOTAL
   FP-TARGET
   c b o NAMED
   c b IR-BUILD:DEFINE-OP ;

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
   c b A64IR-OPCODE:FFLAG NAMED
   c b IR-BUILD:DEFINE-OP ;

: DEF-FFLAGZ ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder f:IR-ID:ir-type-id t:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:FFLAGZ OPCODE IR-SCHEMA:BEGIN-OP
   f IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   c b KEY-COND IR-SCHEMA:ADD-ATTR
   PURE-VALUE
   TOTAL
   FP-TARGET
   c b A64IR-OPCODE:FFLAGZ NAMED
   c b IR-BUILD:DEFINE-OP ;

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
   c b A64IR-OPCODE:FCMPBR NAMED
   c b IR-BUILD:DEFINE-OP ;

: DEF-FCMPBRZ ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder f:IR-ID:ir-type-id :}
   c b A64IR-OPCODE:FCMPBRZ OPCODE IR-SCHEMA:BEGIN-OP
   f IR-SCHEMA:ADD-OPERAND
   c b KEY-COND IR-SCHEMA:ADD-ATTR
   true 2 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE
   TOTAL
   FP-TARGET
   c b A64IR-OPCODE:FCMPBRZ NAMED
   c b IR-BUILD:DEFINE-OP ;

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
   c b A64IR-OPCODE:SELZD NAMED
   c b IR-BUILD:DEFINE-OP ;

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
   c b A64IR-OPCODE:CMPSELD NAMED
   c b IR-BUILD:DEFINE-OP ;

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
   c b A64IR-OPCODE:FCMPSEL NAMED
   c b IR-BUILD:DEFINE-OP ;

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
   c b A64IR-OPCODE:FCMPSELZ NAMED
   c b IR-BUILD:DEFINE-OP ;

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
   c b A64IR-OPCODE:FCMPSELD NAMED
   c b IR-BUILD:DEFINE-OP ;

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
   c b A64IR-OPCODE:FCMPSELZD NAMED
   c b IR-BUILD:DEFINE-OP ;

\ ---- the table this dialect may fill -----------------------------------------
\ The table's dialect name and version are fixed when the module is created, so
\ reading them back off the live module decides whose table it is.
: DIALECT-CK ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b  c b IR-BUILD:DIALECT@  NAME IR-BUILD:SYMBOL-IS?
   0= if E-A64IR-DIALECT throw then
   c b IR-BUILD:SCHEMA-MAJOR@ MAJOR <> if E-A64IR-DIALECT throw then
   c b IR-BUILD:SCHEMA-MINOR@ MINOR <> if E-A64IR-DIALECT throw then ;

public

\ ---- creation and registration -----------------------------------------------
: NEW-BUILDER ( IR-CTX:ctx -- IR-BUILD:builder )
   NAME MAJOR MINOR IR-BUILD:NEW-BUILDER ;

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
   c b t t k A64IR-OPCODE:ALOAD DEF-ALDR
   c b t t k A64IR-OPCODE:ASTORE DEF-ASTR
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
   c b t f k A64IR-OPCODE:FALOAD DEF-ALDR
   c b t f k A64IR-OPCODE:FASTORE DEF-ASTR ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
