\ insn-schema.f - the shared frozen description of what the ARM64 assembler emits.
\
\ The module lives in `package COMPILER-INSN-PROOF`. Its subject is the shipped
\ assembler - `src/arch/arm64/asm.f` with the mnemonic layer in
\ `src/arch/arm64/mnem.f` and the label and branch layer in
\ `src/arch/arm64/icode.f` - and the machine-checked model of it in
\ `formal/Common/Insn.v`.
\
\ It holds data and nothing else. Four tables:
\
\   1. The encoding vectors. A row names a form, its operands as the shipped
\      mnemonic takes them, and the 32-bit word the ARM64 encoding gives. These
\      rows are the one copy. `test/compiler/insn-cases.f` drives each row
\      through the REAL emitter words into the real code buffer and reads the
\      word back; `test/compiler/insn-obligations.f` turns the very same row
\      into a Rocq obligation about `Habu.Common.Insn`. Neither side carries a
\      copy, so weakening a row asks both sides a weaker question and deleting
\      one stops both sides asking.
\
\   2. The overflow vectors. Each row is an operand that does not fit its
\      field. The shipped encoders bound nothing, so none of these is refused:
\      the row records the word that comes out anyway. The Habu side proves the
\      shipped word really emits it; the Rocq side proves the operands are not
\      well formed and that the word is the encoding of a DIFFERENT
\      instruction, or of none at all.
\
\   3. The reserved-register vectors. x18 is Darwin platform-reserved, and
\      `XREG?` in `src/arch/arm64/asm.f` refuses it at encode time. A row names
\      an operand slot holding x18 and what the shipped code does with it:
\      exit code 72 where the check runs, and the emitted word where it does
\      not. The second kind is a FINDING, not a design - the file's own header
\      says the check runs on every X-register operand field.
\
\   4. The logical-immediate vectors. `>LIMM` turns a plain mask into the
\      packed N:immr:imms the encoding carries. The packing is what the model
\      covers; the mask synthesis itself is a MODEL GAP, so these rows bind the
\      two by example on the Habu side and carry the packed value the model
\      uses.
\
\ Where the two sides are not literally the same shape, and why that is sound:
\
\   - The model's operands are the MNEMONIC's operands, not the architecture
\     manual's. A load carries a byte offset because `LDR,` divides by the
\     access size; `MOVK,` carries a byte shift because it divides by 16, while
\     the `MOVZ`/`MOVN` rows carry the raw two-bit hw field because `MOVZ,`
\     fixes it at zero and the wider chain goes through `MOVZHW` directly.
\   - A branch row carries the instruction-relative delta in WORDS that
\     `src/arch/arm64/icode.f` computes from a label. The cases file builds a
\     label at that distance, so a negative delta exercises the immediate
\     resolve and a positive one exercises the forward fixup and backpatch.
\
\ Consumers: `test/compiler/insn-cases.f`,
\ `test/compiler/insn-obligations.f`, `test/compiler/insn-refusal.f`.

require lib/errors.f
require lib/string.f

\ The shipped assembler is standalone Forth with no stack effects of its own -
\ it has to be checkable by the Gforth recovery compiler before the native
\ checker exists. Loading it here once and declaring the effects the checker
\ needs is the named boundary this gate calls the real emitter through; the
\ missing typed capability (checked effects on the standalone assembler layer)
\ is tracked by dot.
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/arch/arm64/mnem.f

s" ASM-INIT" s" --" TRUST
s" CW@" s" n -- ptr u8" TRUST
s" CODE-BYTE+" s" ptr u8 n -- ptr u8" TRUST
s" ASM-LEN" s" -- n" TRUST
s" LBL" s" -- n" TRUST
s" LBL," s" n --" TRUST
s" MOVZHW" s" n n n -- n" TRUST
s" MOVNHW" s" n n n -- n" TRUST
s" EMITW" s" n --" TRUST
s" MOVZ," s" n n --" TRUST
s" MOVN," s" n n --" TRUST
s" MOVK," s" n n n --" TRUST
s" ADD," s" n n n --" TRUST
s" SUB," s" n n n --" TRUST
s" AND," s" n n n --" TRUST
s" ORR," s" n n n --" TRUST
s" EOR," s" n n n --" TRUST
s" MUL," s" n n n --" TRUST
s" SDIV," s" n n n --" TRUST
s" UDIV," s" n n n --" TRUST
s" LSLV," s" n n n --" TRUST
s" LSRV," s" n n n --" TRUST
s" ADDI," s" n n n --" TRUST
s" SUBI," s" n n n --" TRUST
s" ANDI," s" n n n --" TRUST
s" ORRI," s" n n n --" TRUST
s" EORI," s" n n n --" TRUST
s" LSLI," s" n n n --" TRUST
s" LSRI," s" n n n --" TRUST
s" ASRI," s" n n n --" TRUST
s" LDR," s" n n n --" TRUST
s" STR," s" n n n --" TRUST
s" LDRB," s" n n n --" TRUST
s" STRB," s" n n n --" TRUST
s" LDRW," s" n n n --" TRUST
s" STRW," s" n n n --" TRUST
s" LDAR," s" n n --" TRUST
s" STLR," s" n n --" TRUST
s" CMP," s" n n --" TRUST
s" CMPI," s" n n --" TRUST
s" CSET," s" n n --" TRUST
s" B," s" n --" TRUST
s" BL," s" n --" TRUST
s" BCOND," s" n n --" TRUST
s" CBZ," s" n n --" TRUST
s" CBNZ," s" n n --" TRUST
s" ADR," s" n n --" TRUST
s" SVC," s" n --" TRUST
s" RET," s" --" TRUST
s" BRK," s" --" TRUST
s" NOP," s" --" TRUST
s" DSB-ISH," s" --" TRUST
s" ISB," s" --" TRUST
s" BLR," s" n --" TRUST
s" BR," s" n --" TRUST
s" ICIVAU," s" n --" TRUST
s" DCCVAU," s" n --" TRUST
s" >LIMM" s" n -- n" TRUST

package COMPILER-INSN-PROOF
public

\ ---- the emitted vocabulary --------------------------------------------------
\ One code per constructor of `Habu.Common.Insn`, in the order that file
\ declares them, so a row index is the same form on both sides.

 0 constant F-MOVZ      1 constant F-MOVN      2 constant F-MOVK
 3 constant F-ADD       4 constant F-SUB       5 constant F-AND
 6 constant F-ORR       7 constant F-EOR       8 constant F-MUL
 9 constant F-SDIV     10 constant F-UDIV     11 constant F-LSLV
12 constant F-LSRV     13 constant F-ADDI     14 constant F-SUBI
15 constant F-ANDI     16 constant F-ORRI     17 constant F-EORI
18 constant F-LSLI     19 constant F-LSRI     20 constant F-ASRI
21 constant F-LDR      22 constant F-STR      23 constant F-LDRB
24 constant F-STRB     25 constant F-LDRW     26 constant F-STRW
27 constant F-LDAR     28 constant F-STLR     29 constant F-CMP
30 constant F-CMPI     31 constant F-CSET     32 constant F-B
33 constant F-BL       34 constant F-BCOND    35 constant F-CBZ
36 constant F-CBNZ     37 constant F-ADR      38 constant F-SVC
39 constant F-RET      40 constant F-BRK      41 constant F-NOP
42 constant F-DSB-ISH  43 constant F-ISB      44 constant F-BLR
45 constant F-BR       46 constant F-ICIVAU   47 constant F-DCCVAU
48 constant FORMS

\ The constructor name in `formal/Common/Insn.v`. It names the form in a
\ failing row's label and in the generated Rocq obligation, so the two reports
\ point at the same thing.
: FORM-NAME$ ( n -- ptr u8 n )
   case
       0 of s" Movz" endof      1 of s" Movn" endof     2 of s" Movk" endof
       3 of s" Add" endof       4 of s" Sub" endof      5 of s" And" endof
       6 of s" Orr" endof       7 of s" Eor" endof      8 of s" Mul" endof
       9 of s" Sdiv" endof     10 of s" Udiv" endof    11 of s" Lslv" endof
      12 of s" Lsrv" endof     13 of s" Addi" endof    14 of s" Subi" endof
      15 of s" Andi" endof     16 of s" Orri" endof    17 of s" Eori" endof
      18 of s" Lsli" endof     19 of s" Lsri" endof    20 of s" Asri" endof
      21 of s" Ldr" endof      22 of s" Str" endof     23 of s" Ldrb" endof
      24 of s" Strb" endof     25 of s" Ldrw" endof    26 of s" Strw" endof
      27 of s" Ldar" endof     28 of s" Stlr" endof    29 of s" Cmp" endof
      30 of s" Cmpi" endof     31 of s" Cset" endof    32 of s" B" endof
      33 of s" Bl" endof       34 of s" Bcond" endof   35 of s" Cbz" endof
      36 of s" Cbnz" endof     37 of s" Adr" endof     38 of s" Svc" endof
      39 of s" Ret" endof      40 of s" Brk" endof     41 of s" Nop" endof
      42 of s" DsbIsh" endof   43 of s" Isb" endof     44 of s" Blr" endof
      45 of s" Br" endof       46 of s" IcIvau" endof  47 of s" DcCvau" endof
      E-CIE-FORM throw
   endcase ;

\ How many operands the form carries. Both readers use it: the cases file to
\ know how many to hand the emitter, the obligations file to know how many to
\ write after the constructor.

: NULLARY-FORM? ( n -- bool ) {: k:n :}
   k F-RET = k F-BRK = or k F-NOP = or k F-DSB-ISH = or k F-ISB = or ;

: UNARY-FORM? ( n -- bool ) {: k:n :}
   k F-B = k F-BL = or k F-SVC = or k F-BLR = or k F-BR = or
   k F-ICIVAU = or k F-DCCVAU = or ;

: BINARY-FORM? ( n -- bool ) {: k:n :}
   k F-LDAR = k F-STLR = or k F-CMP = or k F-CMPI = or k F-CSET = or
   k F-BCOND = or k F-CBZ = or k F-CBNZ = or k F-ADR = or ;

: FORM-ARITY ( n -- n ) {: k:n :}
   k 0 < k FORMS >= or if E-CIE-FORM throw then
   k NULLARY-FORM? if 0 exit then
   k UNARY-FORM? if 1 exit then
   k BINARY-FORM? if 2 exit then
   3 ;

\ The forms whose last operand is a label distance rather than a plain number.
\ The cases file has to build a label that far away instead of pushing a value.
: FORM-BRANCH? ( n -- bool ) {: k:n :}
   k F-B = k F-BL = or k F-BCOND = or k F-CBZ = or k F-CBNZ = or
   k F-ADR = or ;

private

\ ---- storage -----------------------------------------------------------------

$100 constant VEC-CAP
$20 constant OVF-CAP
$20 constant RES-CAP
$10 constant LIM-CAP

create VEC-FORM VEC-CAP cells allot
create VEC-A VEC-CAP cells allot
create VEC-B VEC-CAP cells allot
create VEC-C VEC-CAP cells allot
create VEC-MASK VEC-CAP cells allot
create VEC-WORD VEC-CAP cells allot

create OVF-FORM OVF-CAP cells allot
create OVF-A OVF-CAP cells allot
create OVF-B OVF-CAP cells allot
create OVF-C OVF-CAP cells allot
create OVF-WORD OVF-CAP cells allot

create RES-FORM RES-CAP cells allot
create RES-A RES-CAP cells allot
create RES-B RES-CAP cells allot
create RES-C RES-CAP cells allot
create RES-RC RES-CAP cells allot
create RES-WORD RES-CAP cells allot

create LIM-MASK LIM-CAP cells allot
create LIM-NIS LIM-CAP cells allot

variable VEC-N
variable OVF-N
variable RES-N
variable LIM-N

: VEC-RANGE ( n -- ) {: i:n :}
   i 0 < i VEC-N @ >= or if E-CIE-ROW throw then ;

: OVF-RANGE ( n -- ) {: i:n :}
   i 0 < i OVF-N @ >= or if E-CIE-ROW throw then ;

: RES-RANGE ( n -- ) {: i:n :}
   i 0 < i RES-N @ >= or if E-CIE-ROW throw then ;

: LIM-RANGE ( n -- ) {: i:n :}
   i 0 < i LIM-N @ >= or if E-CIE-ROW throw then ;

\ ---- table builders ----------------------------------------------------------

: VEC+ ( n n n n n n -- ) {: form:n a:n b:n c:n mask:n word:n :}
   VEC-N @ VEC-CAP >= if E-CIE-ROW throw then
   form VEC-FORM VEC-N @ cells + !
   a VEC-A VEC-N @ cells + !
   b VEC-B VEC-N @ cells + !
   c VEC-C VEC-N @ cells + !
   mask VEC-MASK VEC-N @ cells + !
   word VEC-WORD VEC-N @ cells + !
   VEC-N @ 1+ VEC-N ! ;

\ One row per operand count, so a row reads as the mnemonic call it drives.
: V0 ( n n -- ) {: form:n word:n :}       form 0 0 0 0 word VEC+ ;
: V1 ( n n n -- ) {: form:n a:n word:n :} form a 0 0 0 word VEC+ ;
: V2 ( n n n n -- ) {: form:n a:n b:n word:n :}  form a b 0 0 word VEC+ ;
: V3 ( n n n n n -- ) {: form:n a:n b:n c:n word:n :} form a b c 0 word VEC+ ;

\ A logical-immediate row also carries the plain mask the mnemonic is handed;
\ the third operand is the packed value `>LIMM` must build from it.
: VL ( n n n n n n -- ) {: form:n a:n b:n nis:n mask:n word:n :}
   form a b nis mask word VEC+ ;

: OVF+ ( n n n n n -- ) {: form:n a:n b:n c:n word:n :}
   OVF-N @ OVF-CAP >= if E-CIE-ROW throw then
   form OVF-FORM OVF-N @ cells + !
   a OVF-A OVF-N @ cells + !
   b OVF-B OVF-N @ cells + !
   c OVF-C OVF-N @ cells + !
   word OVF-WORD OVF-N @ cells + !
   OVF-N @ 1+ OVF-N ! ;

: RES+ ( n n n n n n -- ) {: form:n a:n b:n c:n rc:n word:n :}
   RES-N @ RES-CAP >= if E-CIE-ROW throw then
   form RES-FORM RES-N @ cells + !
   a RES-A RES-N @ cells + !
   b RES-B RES-N @ cells + !
   c RES-C RES-N @ cells + !
   rc RES-RC RES-N @ cells + !
   word RES-WORD RES-N @ cells + !
   RES-N @ 1+ RES-N ! ;

: LIM+ ( n n -- ) {: mask:n nis:n :}
   LIM-N @ LIM-CAP >= if E-CIE-ROW throw then
   mask LIM-MASK LIM-N @ cells + !
   nis LIM-NIS LIM-N @ cells + !
   LIM-N @ 1+ LIM-N ! ;

\ ---- the encoding vectors ----------------------------------------------------
\ Every expected word here comes from the ARM64 encoding and was cross-checked
\ against an independent assembler (clang -c -arch arm64, read back with
\ objdump) before it was written down. Nothing was read out of the Habu
\ encoders, so a row cannot agree with a bug in them by construction.

: MOVE-WIDE-VECTORS ( -- )
   F-MOVZ 0 2 0 $D2800040 V3
   F-MOVZ 9 $FFFF 0 $D29FFFE9 V3
   F-MOVZ 17 0 0 $D2800011 V3
   F-MOVZ 5 $1234 1 $D2A24685 V3
   F-MOVZ 9 $FFFF 3 $D2FFFFE9 V3
   F-MOVN 3 0 0 $92800003 V3
   F-MOVN 12 $FFF 0 $9281FFEC V3
   F-MOVN 12 $FFF 2 $92C1FFEC V3
   F-MOVK 5 $1234 0 $F2824685 V3
   F-MOVK 5 $1234 16 $F2A24685 V3
   F-MOVK 9 $FFFF 48 $F2FFFFE9 V3 ;

: SHIFTED-REGISTER-VECTORS ( -- )
   F-ADD 3 3 2 $8B020063 V3
   F-ADD 22 22 14 $8B0E02D6 V3
   F-SUB 4 9 17 $CB110124 V3
   F-SUB 2 13 12 $CB0C01A2 V3
   F-AND 7 6 5 $8A0500C7 V3
   F-ORR 9 9 5 $AA050129 V3
   F-ORR 9 8 7 $AA070109 V3
   F-EOR 1 2 3 $CA030041 V3
   F-MUL 10 11 12 $9B0C7D6A V3
   F-SDIV 4 5 6 $9AC60CA4 V3
   F-UDIV 7 8 9 $9AC90907 V3
   F-LSLV 5 5 9 $9AC920A5 V3
   F-LSLV 3 4 5 $9AC52083 V3
   F-LSRV 6 7 8 $9AC824E6 V3 ;

: IMMEDIATE-VECTORS ( -- )
   F-ADDI 6 11 0 $91000166 V3
   F-ADDI 22 22 2 $91000AD6 V3
   F-ADDI 19 19 $FFF $913FFE73 V3
   F-SUBI 19 19 8 $D1002273 V3
   F-SUBI 31 31 16 $D10043FF V3
   F-ANDI 14 14 $10C0 $2000000000000000 $924301CE VL
   F-ANDI 9 9 $1007 $FF $92401D29 VL
   F-ORRI 3 4 $1000 1 $B2400083 VL
   F-EORI 5 6 $100F $FFFF $D2403CC5 VL ;

: SHIFT-VECTORS ( -- )
   F-LSLI 7 7 5 $D37BE8E7 V3
   F-LSLI 3 4 1 $D37FF883 V3
   F-LSLI 9 9 63 $D3410129 V3
   F-LSRI 7 6 16 $D350FCC7 V3
   F-LSRI 9 9 32 $D360FD29 V3
   F-LSRI 2 3 63 $D37FFC62 V3
   F-ASRI 5 5 1 $9341FCA5 V3
   F-ASRI 8 9 63 $937FFD28 V3 ;

: MEMORY-VECTORS ( -- )
   F-LDR 8 31 40 $F94017E8 V3
   F-LDR 13 8 0 $F940010D V3
   F-LDR 6 2 32760 $F97FFC46 V3
   F-STR 12 8 0 $F900010C V3
   F-STR 19 31 8 $F90007F3 V3
   F-LDRB 9 6 0 $394000C9 V3
   F-LDRB 4 17 1 $39400624 V3
   F-LDRB 14 13 $FFF $397FFDAE V3
   F-STRB 9 6 0 $390000C9 V3
   F-STRB 4 17 1 $39000624 V3
   F-LDRW 15 8 24 $B940190F V3
   F-LDRW 3 4 16380 $B97FFC83 V3
   F-STRW 5 6 4 $B90004C5 V3
   F-LDAR 14 15 $C8DFFDEE V2
   F-STLR 11 5 $C89FFCAB V2 ;

: COMPARE-VECTORS ( -- )
   F-CMP 9 8 $EB08013F V2
   F-CMP 15 14 $EB0E01FF V2
   F-CMP 17 6 $EB06023F V2
   F-CMPI 12 16 $F100419F V2
   F-CMPI 4 35 $F1008C9F V2
   F-CMPI 1 $FFF $F13FFC3F V2
   F-CSET 13 2 $9A9F37ED V2
   F-CSET 7 0 $9A9F17E7 V2 ;

\ Branch rows carry the word-relative delta. A row with a delta at or below
\ zero resolves against a label already bound, which is the immediate path in
\ `BR-EMIT`; a positive delta records a fixup and is backpatched by `LBL,`,
\ which is the path the snapshot relocation pass depends on.
: BRANCH-VECTORS ( -- )
   F-B 0 $14000000 V1
   F-B 2 $14000002 V1
   F-B -1 $17FFFFFF V1
   F-B -3 $17FFFFFD V1
   F-BL 0 $94000000 V1
   F-BL 4 $94000004 V1
   F-BL 5 $94000005 V1
   F-BL -1 $97FFFFFF V1
   F-BL -3 $97FFFFFD V1
   F-BCOND 1 -1 $54FFFFE1 V2
   F-BCOND 0 3 $54000060 V2
   F-BCOND 3 0 $54000003 V2
   F-BCOND 10 2 $5400004A V2
   F-BCOND 11 -4 $54FFFF8B V2
   F-BCOND 12 1 $5400002C V2
   F-BCOND 9 0 $54000009 V2
   F-CBZ 13 -1 $B4FFFFED V2
   F-CBZ 2 3 $B4000062 V2
   F-CBNZ 15 0 $B500000F V2
   F-CBNZ 9 2 $B5000049 V2
   F-ADR 1 -8 $10FFFFC1 V2
   F-ADR 1 16 $10000081 V2 ;

: SYSTEM-VECTORS ( -- )
   F-SVC 0 $D4000001 V1
   F-SVC 128 $D4001001 V1
   F-RET $D65F03C0 V0
   F-BRK $D4200000 V0
   F-NOP $D503201F V0
   F-DSB-ISH $D5033B9F V0
   F-ISB $D5033FDF V0
   F-BLR 7 $D63F00E0 V1
   F-BR 3 $D61F0060 V1
   F-ICIVAU 5 $D50B7525 V1
   F-DCCVAU 6 $D50B7B26 V1 ;

\ ---- the overflow vectors ----------------------------------------------------
\ Each row hands an emitter an operand one past the field it goes in. None of
\ them is refused. The first four run into the neighbouring field and encode a
\ different instruction; the last two leave the emitted vocabulary altogether.

: OVERFLOW-VECTORS ( -- )
   F-MOVZ 0 $10000 0 $D2A00000 OVF+          \ imm16 + 1 becomes hw = 1
   F-CSET 1 16 0 $9A9F17E1 OVF+              \ cond 16 becomes cond eq
   F-MOVK 5 $1234 8 $F2824685 OVF+           \ a shift of 8 rounds down to 0
   F-LDR 1 2 12 $F9400441 OVF+               \ an offset of 12 rounds down to 8
   F-ADDI 1 2 $1000 $91400041 OVF+           \ imm12 + 1 sets the shift-by-12 bit
   F-ADD 1 2 32 $8B200041 OVF+ ;             \ register 32 sets the extend bit

\ ---- the reserved-register vectors -------------------------------------------
\ Exit code 72 is what `die` reports for the refusal in `XREG?`. A row with
\ code 72 runs in a child engine, because the refusal ends the process; a row
\ with code 0 runs in this one and is compared against the word that came out.

public
72 constant RESERVED-RC
private

: RESERVED-VECTORS ( -- )
   F-LDR 18 15 0 RESERVED-RC 0 RES+
   F-LDR 1 18 0 RESERVED-RC 0 RES+
   F-ADD 18 2 3 RESERVED-RC 0 RES+
   F-ADD 1 18 3 RESERVED-RC 0 RES+
   F-ADD 1 2 18 RESERVED-RC 0 RES+
   F-MOVZ 18 0 0 RESERVED-RC 0 RES+
   F-CMP 18 8 0 RESERVED-RC 0 RES+
   F-CMP 9 18 0 RESERVED-RC 0 RES+
   F-CMPI 18 16 0 RESERVED-RC 0 RES+
   F-CSET 18 0 0 RESERVED-RC 0 RES+
   F-ADR 18 0 0 RESERVED-RC 0 RES+
   F-BLR 18 0 0 RESERVED-RC 0 RES+
   \ FINDING: no check reaches these slots. LDAR and STLR never call XREG?,
   \ and CBZ,/CBNZ, in src/arch/arm64/icode.f build their word without going
   \ through the ENC-CBZ/ENC-CBNZ encoders that would have called it.
   F-LDAR 18 15 0 0 $C8DFFDF2 RES+
   F-LDAR 14 18 0 0 $C8DFFE4E RES+
   F-STLR 18 5 0 0 $C89FFCB2 RES+
   F-CBZ 18 0 0 0 $B4000012 RES+
   F-CBNZ 18 0 0 0 $B5000012 RES+
   \ and an immediate that happens to be 18 is not a register, so it stands.
   F-LDR 1 2 144 0 $F9404841 RES+ ;

\ ---- the logical-immediate vectors -------------------------------------------

: LIMM-VECTORS ( -- )
   $2000000000000000 $10C0 LIM+
   $FF $1007 LIM+
   1 $1000 LIM+
   $FFFF $100F LIM+ ;

: BUILD-VECTORS ( -- )
   0 VEC-N !
   0 OVF-N !
   0 RES-N !
   0 LIM-N !
   MOVE-WIDE-VECTORS
   SHIFTED-REGISTER-VECTORS
   IMMEDIATE-VECTORS
   SHIFT-VECTORS
   MEMORY-VECTORS
   COMPARE-VECTORS
   BRANCH-VECTORS
   SYSTEM-VECTORS
   OVERFLOW-VECTORS
   RESERVED-VECTORS
   LIMM-VECTORS ;

BUILD-VECTORS

public

\ ---- reading the tables ------------------------------------------------------

: VECTORS ( -- n )        VEC-N @ ;
: OVERFLOWS ( -- n )      OVF-N @ ;
: RESERVEDS ( -- n )      RES-N @ ;
: LIMMS ( -- n )          LIM-N @ ;

: ROW-FORM@ ( n -- n )    dup VEC-RANGE cells VEC-FORM + @ ;
: ROW-A@ ( n -- n )       dup VEC-RANGE cells VEC-A + @ ;
: ROW-B@ ( n -- n )       dup VEC-RANGE cells VEC-B + @ ;
: ROW-C@ ( n -- n )       dup VEC-RANGE cells VEC-C + @ ;
: ROW-MASK@ ( n -- n )    dup VEC-RANGE cells VEC-MASK + @ ;
: ROW-WORD@ ( n -- n )    dup VEC-RANGE cells VEC-WORD + @ ;

: OVF-FORM@ ( n -- n )    dup OVF-RANGE cells OVF-FORM + @ ;
: OVF-A@ ( n -- n )       dup OVF-RANGE cells OVF-A + @ ;
: OVF-B@ ( n -- n )       dup OVF-RANGE cells OVF-B + @ ;
: OVF-C@ ( n -- n )       dup OVF-RANGE cells OVF-C + @ ;
: OVF-WORD@ ( n -- n )    dup OVF-RANGE cells OVF-WORD + @ ;

: RES-FORM@ ( n -- n )    dup RES-RANGE cells RES-FORM + @ ;
: RES-A@ ( n -- n )       dup RES-RANGE cells RES-A + @ ;
: RES-B@ ( n -- n )       dup RES-RANGE cells RES-B + @ ;
: RES-C@ ( n -- n )       dup RES-RANGE cells RES-C + @ ;
: RES-RC@ ( n -- n )      dup RES-RANGE cells RES-RC + @ ;
: RES-WORD@ ( n -- n )    dup RES-RANGE cells RES-WORD + @ ;

: LIM-MASK@ ( n -- n )    dup LIM-RANGE cells LIM-MASK + @ ;
: LIM-NIS@ ( n -- n )     dup LIM-RANGE cells LIM-NIS + @ ;

\ The two masks `>LIMM` must refuse: a logical immediate cannot be all zeroes
\ or all ones, because neither is a rotated run of ones inside a repeating
\ element. Both end the process, so both run in a child engine.
: LIMM-BAD-MASK ( n -- n )
   case
      0 of 0 endof
      1 of -1 endof
      E-CIE-ROW throw
   endcase ;

: LIMM-BADS ( -- n ) 2 ;

: MODEL-FILE$ ( -- ptr u8 n )
   s" formal/Common/Insn.v" ;

;package
