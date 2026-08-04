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
\   2. The out-of-range vectors. Each row is an operand that does not fit the
\      field the encoder drops it into, or a byte operand that the encoder's
\      scale division would round down. Every one of them is refused: the
\      shipped word ends the process with code 72 before any bit is packed, so
\      each row runs in a child engine and is judged by its exit status. The
\      Rocq side asks the model whether the same operands are well formed, and
\      the answer must be no. That is what binds the two bounds together: a
\      bound the shipped code loosened would emit instead of dying, and a bound
\      the model loosened would answer `true` here.
\
\   3. The reserved-register vectors. x18 is Darwin platform-reserved, and
\      `XREG?` in `src/arch/arm64/asm.f` refuses it at encode time. A row names
\      an operand slot holding x18 and what the shipped code does with it. There
\      is one row for every X-register operand slot of every form, because the
\      model's `checked_regs` is a claim about each slot on its own, and a row
\      is what makes that claim answerable. All of them refuse; the one row that
\      does not is the control, an immediate that happens to be 18.
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
\ How the bounds are covered, and what is deliberately not covered:
\
\   - Every form has one encoding row with all of its operands at their largest
\     legal value, so a bound that became one too tight fails on that row.
\   - Every X-register operand slot has an x18 row.
\   - Every operand that is not a register has an out-of-range row, one per
\     encoder, because each encoder applies the bound at its own call site.
\   - A register that is merely too large is one row per way a register reaches
\     the guard (`XMW3`, `XR3` in each of its three slots, `XRDI` in each of its
\     two, `XR2` in each of its two, `XR2ND`, and a bare `XREG?`), not one per
\     slot. The x18 rows already show that every slot reaches `XREG?`; the field
\     bound then lives inside that one word, so a row per slot would be the same
\     fact under seventy-six names.
\   - Three bounds have no row because the shipped mnemonic cannot reach them.
\     The packed logical immediate is built by `>LIMM`, which never returns a
\     value outside the field; a branch or ADR displacement is a label distance
\     that `src/arch/arm64/icode.f` bounds first; and an ADR displacement is a
\     word position times four, so it can never be misaligned. Each is stated
\     here rather than left to be discovered.
\   - The floating-point encoders bound their D-register operands too, through
\     `DR2` and `DR3` in `src/arch/arm64/asm.f`, but they have no row here at
\     all: the floating-point forms are not in the modelled vocabulary, which
\     `formal/Common/Insn.v` records as a MODEL GAP with its own dot. When that
\     gap closes, those bounds get rows like every other form's.
\   - `wf` calls a left shift of zero malformed and the shipped encoder does
\     not refuse it. That is not a missing bound: zero is inside the six-bit
\     shift field, and the reason the model excludes it is that the word is
\     also a right shift of zero. `lsli_lsri_alias_at_zero` in
\     `formal/Common/Insn.v` is where that lives.
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
s" CSEL," s" n n n n --" TRUST
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
30 constant F-CMPI     31 constant F-CSET     32 constant F-CSEL
33 constant F-B        34 constant F-BL       35 constant F-BCOND
36 constant F-CBZ      37 constant F-CBNZ     38 constant F-ADR
39 constant F-SVC      40 constant F-RET      41 constant F-BRK
42 constant F-NOP      43 constant F-DSB-ISH  44 constant F-ISB
45 constant F-BLR      46 constant F-BR       47 constant F-ICIVAU
48 constant F-DCCVAU
49 constant FORMS

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
      30 of s" Cmpi" endof     31 of s" Cset" endof    32 of s" Csel" endof
      33 of s" B" endof        34 of s" Bl" endof      35 of s" Bcond" endof
      36 of s" Cbz" endof      37 of s" Cbnz" endof    38 of s" Adr" endof
      39 of s" Svc" endof      40 of s" Ret" endof     41 of s" Brk" endof
      42 of s" Nop" endof      43 of s" DsbIsh" endof  44 of s" Isb" endof
      45 of s" Blr" endof      46 of s" Br" endof      47 of s" IcIvau" endof
      48 of s" DcCvau" endof
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

\ The one form with four: a conditional select names a destination, two
\ sources and the condition that chooses between them.
: QUATERNARY-FORM? ( n -- bool ) {: k:n :}
   k F-CSEL = ;

: FORM-ARITY ( n -- n ) {: k:n :}
   k 0 < k FORMS >= or if E-CIE-FORM throw then
   k NULLARY-FORM? if 0 exit then
   k UNARY-FORM? if 1 exit then
   k BINARY-FORM? if 2 exit then
   k QUATERNARY-FORM? if 4 exit then
   3 ;

\ The forms whose last operand is a label distance rather than a plain number.
\ The cases file has to build a label that far away instead of pushing a value.
: FORM-BRANCH? ( n -- bool ) {: k:n :}
   k F-B = k F-BL = or k F-BCOND = or k F-CBZ = or k F-CBNZ = or
   k F-ADR = or ;

\ What `die` reports for every refusal in `src/arch/arm64/asm.f`: the reserved
\ register, an operand outside its field, and an operand a scale division would
\ round. A row that carries it runs in a child engine, because the refusal ends
\ the process.
72 constant RESERVED-RC

private

\ ---- storage -----------------------------------------------------------------

$100 constant VEC-CAP
$40 constant OOR-CAP
$80 constant RES-CAP
$10 constant LIM-CAP

create VEC-FORM VEC-CAP cells allot
create VEC-A VEC-CAP cells allot
create VEC-B VEC-CAP cells allot
create VEC-C VEC-CAP cells allot
create VEC-D VEC-CAP cells allot
create VEC-MASK VEC-CAP cells allot
create VEC-WORD VEC-CAP cells allot

create OOR-FORM OOR-CAP cells allot
create OOR-A OOR-CAP cells allot
create OOR-B OOR-CAP cells allot
create OOR-C OOR-CAP cells allot
create OOR-D OOR-CAP cells allot

create RES-FORM RES-CAP cells allot
create RES-A RES-CAP cells allot
create RES-B RES-CAP cells allot
create RES-C RES-CAP cells allot
create RES-D RES-CAP cells allot
create RES-MASK RES-CAP cells allot
create RES-RC RES-CAP cells allot
create RES-WORD RES-CAP cells allot

create LIM-MASK LIM-CAP cells allot
create LIM-NIS LIM-CAP cells allot

variable VEC-N
variable OOR-N
variable RES-N
variable LIM-N

: VEC-RANGE ( n -- ) {: i:n :}
   i 0 < i VEC-N @ >= or if E-CIE-ROW throw then ;

: OOR-RANGE ( n -- ) {: i:n :}
   i 0 < i OOR-N @ >= or if E-CIE-ROW throw then ;

: RES-RANGE ( n -- ) {: i:n :}
   i 0 < i RES-N @ >= or if E-CIE-ROW throw then ;

: LIM-RANGE ( n -- ) {: i:n :}
   i 0 < i LIM-N @ >= or if E-CIE-ROW throw then ;

\ ---- table builders ----------------------------------------------------------

: VEC+ ( n n n n n n n -- ) {: form:n a:n b:n c:n d:n mask:n word:n :}
   VEC-N @ VEC-CAP >= if E-CIE-ROW throw then
   form VEC-FORM VEC-N @ cells + !
   a VEC-A VEC-N @ cells + !
   b VEC-B VEC-N @ cells + !
   c VEC-C VEC-N @ cells + !
   d VEC-D VEC-N @ cells + !
   mask VEC-MASK VEC-N @ cells + !
   word VEC-WORD VEC-N @ cells + !
   VEC-N @ 1+ VEC-N ! ;

\ One row per operand count, so a row reads as the mnemonic call it drives.
: V0 ( n n -- ) {: form:n word:n :}       form 0 0 0 0 0 word VEC+ ;
: V1 ( n n n -- ) {: form:n a:n word:n :} form a 0 0 0 0 word VEC+ ;
: V2 ( n n n n -- ) {: form:n a:n b:n word:n :}  form a b 0 0 0 word VEC+ ;
: V3 ( n n n n n -- ) {: form:n a:n b:n c:n word:n :} form a b c 0 0 word VEC+ ;
: V4 ( n n n n n n -- ) {: form:n a:n b:n c:n d:n word:n :}
   form a b c d 0 word VEC+ ;

\ A logical-immediate row also carries the plain mask the mnemonic is handed;
\ the third operand is the packed value `>LIMM` must build from it.
: VL ( n n n n n n -- ) {: form:n a:n b:n nis:n mask:n word:n :}
   form a b nis 0 mask word VEC+ ;

\ An out-of-range row carries no expected word: the shipped code refuses it, so
\ there is nothing to read back. It carries no mask either, because the only
\ operand a mask would be for - the packed logical immediate - cannot be driven
\ out of range through the shipped mnemonic.
: OOR4+ ( n n n n n -- ) {: form:n a:n b:n c:n d:n :}
   OOR-N @ OOR-CAP >= if E-CIE-ROW throw then
   form OOR-FORM OOR-N @ cells + !
   a OOR-A OOR-N @ cells + !
   b OOR-B OOR-N @ cells + !
   c OOR-C OOR-N @ cells + !
   d OOR-D OOR-N @ cells + !
   OOR-N @ 1+ OOR-N ! ;

: OOR+ ( n n n n -- ) {: form:n a:n b:n c:n :}
   form a b c 0 OOR4+ ;

\ A reserved-register row carries the plain mask as well, because the logical
\ mnemonics take one and build the packed operand from it; the `c` column is
\ that packed value, which is what the model is asked about.
: RES4+ ( n n n n n n n n -- ) {: form:n a:n b:n c:n d:n mask:n rc:n word:n :}
   RES-N @ RES-CAP >= if E-CIE-ROW throw then
   form RES-FORM RES-N @ cells + !
   a RES-A RES-N @ cells + !
   b RES-B RES-N @ cells + !
   c RES-C RES-N @ cells + !
   d RES-D RES-N @ cells + !
   mask RES-MASK RES-N @ cells + !
   rc RES-RC RES-N @ cells + !
   word RES-WORD RES-N @ cells + !
   RES-N @ 1+ RES-N ! ;

: RES+ ( n n n n n n n -- ) {: form:n a:n b:n c:n mask:n rc:n word:n :}
   form a b c 0 mask rc word RES4+ ;

\ Almost every reserved-register row is the same shape: an operand slot holding
\ x18, no mask, refused before a word exists.
: X18 ( n n n n -- ) {: form:n a:n b:n c:n :}
   form a b c 0 RESERVED-RC 0 RES+ ;

: X184 ( n n n n n -- ) {: form:n a:n b:n c:n d:n :}
   form a b c d 0 RESERVED-RC 0 RES4+ ;

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
   F-MOVK 9 $FFFF 48 $F2FFFFE9 V3
   F-MOVZ 31 $FFFF 3 $D2FFFFFF V3
   F-MOVN 31 $FFFF 3 $92FFFFFF V3
   F-MOVK 31 $FFFF 48 $F2FFFFFF V3 ;

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
   F-LSRV 6 7 8 $9AC824E6 V3
   F-ADD 31 31 31 $8B1F03FF V3
   F-SUB 31 31 31 $CB1F03FF V3
   F-AND 31 31 31 $8A1F03FF V3
   F-ORR 31 31 31 $AA1F03FF V3
   F-EOR 31 31 31 $CA1F03FF V3
   F-MUL 31 31 31 $9B1F7FFF V3
   F-SDIV 31 31 31 $9ADF0FFF V3
   F-UDIV 31 31 31 $9ADF0BFF V3
   F-LSLV 31 31 31 $9ADF23FF V3
   F-LSRV 31 31 31 $9ADF27FF V3 ;

: IMMEDIATE-VECTORS ( -- )
   F-ADDI 6 11 0 $91000166 V3
   F-ADDI 22 22 2 $91000AD6 V3
   F-ADDI 19 19 $FFF $913FFE73 V3
   F-SUBI 19 19 8 $D1002273 V3
   F-SUBI 31 31 16 $D10043FF V3
   F-ANDI 14 14 $10C0 $2000000000000000 $924301CE VL
   F-ANDI 9 9 $1007 $FF $92401D29 VL
   F-ORRI 3 4 $1000 1 $B2400083 VL
   F-EORI 5 6 $100F $FFFF $D2403CC5 VL
   F-ADDI 31 31 $FFF $913FFFFF V3
   F-SUBI 31 31 $FFF $D13FFFFF V3
   F-ANDI 31 31 $1007 $FF $92401FFF VL
   F-ORRI 31 31 $1000 1 $B24003FF VL
   F-EORI 31 31 $100F $FFFF $D2403FFF VL ;

: SHIFT-VECTORS ( -- )
   F-LSLI 7 7 5 $D37BE8E7 V3
   F-LSLI 3 4 1 $D37FF883 V3
   F-LSLI 9 9 63 $D3410129 V3
   F-LSRI 7 6 16 $D350FCC7 V3
   F-LSRI 9 9 32 $D360FD29 V3
   F-LSRI 2 3 63 $D37FFC62 V3
   F-ASRI 5 5 1 $9341FCA5 V3
   F-ASRI 8 9 63 $937FFD28 V3
   F-LSLI 31 31 63 $D34103FF V3
   F-LSRI 31 31 63 $D37FFFFF V3
   F-ASRI 31 31 63 $937FFFFF V3 ;

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
   F-STLR 11 5 $C89FFCAB V2
   F-LDR 31 31 32760 $F97FFFFF V3
   F-STR 31 31 32760 $F93FFFFF V3
   F-LDRB 31 31 $FFF $397FFFFF V3
   F-STRB 31 31 $FFF $393FFFFF V3
   F-LDRW 31 31 16380 $B97FFFFF V3
   F-STRW 31 31 16380 $B93FFFFF V3
   F-LDAR 31 31 $C8DFFFFF V2
   F-STLR 31 31 $C89FFFFF V2 ;

: COMPARE-VECTORS ( -- )
   F-CMP 9 8 $EB08013F V2
   F-CMP 15 14 $EB0E01FF V2
   F-CMP 17 6 $EB06023F V2
   F-CMPI 12 16 $F100419F V2
   F-CMPI 4 35 $F1008C9F V2
   F-CMPI 1 $FFF $F13FFC3F V2
   F-CSET 13 2 $9A9F37ED V2
   F-CSET 7 0 $9A9F17E7 V2
   F-CMP 31 31 $EB1F03FF V2
   F-CMPI 31 $FFF $F13FFFFF V2
   F-CSET 31 15 $9A9FE7FF V2 ;

\ The conditional select: a destination, the source taken when the condition
\ holds, the source taken when it does not, and the condition. Its three
\ register operands go through `XR3`, exactly as a shifted-register form's do,
\ so the field bound on a register needs no row of its own here - the Add rows
\ already ask that question of that word. What is this form's own is the
\ four-bit condition field, which is bounded by `?COND` at this call site.
: CONDITIONAL-SELECT-VECTORS ( -- )
   F-CSEL 3 4 5 0 $9A850083 V4
   F-CSEL 13 9 21 11 $9A95B12D V4
   F-CSEL 0 1 2 1 $9A821020 V4
   F-CSEL 7 8 9 12 $9A89C107 V4
   F-CSEL 20 2 30 4 $9A9E4054 V4
   F-CSEL 31 31 31 15 $9A9FF3FF V4 ;

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
   F-ADR 1 16 $10000081 V2
   F-BCOND 15 0 $5400000F V2
   F-CBZ 31 0 $B400001F V2
   F-CBNZ 31 0 $B500001F V2
   F-ADR 31 0 $1000001F V2 ;

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
   F-DCCVAU 6 $D50B7B26 V1
   F-SVC $FFFF $D41FFFE1 V1
   F-BLR 31 $D63F03E0 V1
   F-BR 31 $D61F03E0 V1
   F-ICIVAU 31 $D50B753F V1
   F-DCCVAU 31 $D50B7B3F V1 ;

\ ---- the out-of-range vectors ------------------------------------------------
\ Each row hands an emitter an operand the field cannot hold, or a byte operand
\ the encoder's scale division would round down. Every one is refused before a
\ bit is packed, so every one runs in a child engine and is judged by its exit
\ status. The comment on a row says what the shipped code used to emit instead.

\ A register one past the five-bit field, once for each way a register reaches
\ the guard. `XMW3` carries the move-wide destination, `XR3` the three operands
\ of a shifted-register form, `XRDI` the destination and base of an immediate
\ or load/store form, `XR2` both operands of an acquire/release form, `XR2ND`
\ the register of a two-operand form whose second operand is not one, and
\ `XREG?` on its own the single operand of an indirect branch.
: REGISTER-RANGE-VECTORS ( -- )
   F-MOVZ 32 0 0 OOR+                        \ was: register 32 became hw = 1
   F-ADD 32 2 3 OOR+
   F-ADD 1 32 3 OOR+
   F-ADD 1 2 32 OOR+                         \ was: register 32 set the extend bit
   F-ADDI 32 2 0 OOR+
   F-ADDI 1 32 0 OOR+
   F-LDAR 32 15 0 OOR+
   F-LDAR 14 32 0 OOR+
   F-CSET 32 0 0 OOR+
   F-BLR 32 0 0 OOR+ ;

\ Every operand that is not a register, one row per encoder that bounds it.
: IMMEDIATE-RANGE-VECTORS ( -- )
   F-MOVZ 0 $10000 0 OOR+                    \ was: imm16 + 1 became hw = 1
   F-MOVN 0 $10000 0 OOR+
   F-MOVK 0 $10000 0 OOR+
   F-SVC $10000 0 0 OOR+
   F-MOVZ 0 0 4 OOR+                         \ the shifted half is two bits wide
   F-MOVN 0 0 4 OOR+
   F-MOVK 5 $1234 64 OOR+                    \ a shift of 64 is a fifth half
   F-ADDI 1 2 $1000 OOR+                     \ was: imm12 + 1 set the shift-by-12 bit
   F-SUBI 1 2 $1000 OOR+
   F-LDRB 1 2 $1000 OOR+
   F-STRB 1 2 $1000 OOR+
   F-CMPI 1 $1000 0 OOR+
   F-LDR 1 2 32768 OOR+                      \ 32768 / 8 is one past the field
   F-STR 1 2 32768 OOR+
   F-LDRW 1 2 16384 OOR+
   F-STRW 1 2 16384 OOR+
   F-LSLI 1 2 64 OOR+
   F-LSRI 1 2 64 OOR+
   F-ASRI 1 2 64 OOR+
   F-CSET 1 16 0 OOR+                        \ was: cond 16 became cond eq
   F-CSEL 1 2 2 16 OOR4+                     \ was: cond 16 ran into the second source
   F-BCOND 16 0 0 OOR+ ;

\ A byte operand the encoder divides by its access scale. Each was rounded down
\ and encoded as the aligned operand below it.
: MISALIGNED-VECTORS ( -- )
   F-MOVK 5 $1234 8 OOR+                     \ was: a shift of 8 rounded down to 0
   F-LDR 1 2 12 OOR+                         \ was: an offset of 12 rounded down to 8
   F-STR 1 2 12 OOR+
   F-LDRW 1 2 6 OOR+
   F-STRW 1 2 6 OOR+ ;

\ ---- the reserved-register vectors -------------------------------------------
\ One row per X-register operand slot of every form. A row with code 72 runs in
\ a child engine, because the refusal ends the process; the one row with code 0
\ runs in this one and is compared against the word that came out.

: RESERVED-MOVE-WIDE ( -- )
   F-MOVZ 18 0 0 X18
   F-MOVN 18 0 0 X18
   F-MOVK 18 0 0 X18 ;

: RESERVED-SHIFTED-REGISTER ( -- )
   F-ADD 18 2 3 X18   F-ADD 1 18 3 X18   F-ADD 1 2 18 X18
   F-SUB 18 2 3 X18   F-SUB 1 18 3 X18   F-SUB 1 2 18 X18
   F-AND 18 2 3 X18   F-AND 1 18 3 X18   F-AND 1 2 18 X18
   F-ORR 18 2 3 X18   F-ORR 1 18 3 X18   F-ORR 1 2 18 X18
   F-EOR 18 2 3 X18   F-EOR 1 18 3 X18   F-EOR 1 2 18 X18
   F-MUL 18 2 3 X18   F-MUL 1 18 3 X18   F-MUL 1 2 18 X18
   F-SDIV 18 2 3 X18  F-SDIV 1 18 3 X18  F-SDIV 1 2 18 X18
   F-UDIV 18 2 3 X18  F-UDIV 1 18 3 X18  F-UDIV 1 2 18 X18
   F-LSLV 18 2 3 X18  F-LSLV 1 18 3 X18  F-LSLV 1 2 18 X18
   F-LSRV 18 2 3 X18  F-LSRV 1 18 3 X18  F-LSRV 1 2 18 X18 ;

\ The logical forms take the plain mask and pack it themselves, so these rows
\ carry the mask the mnemonic is handed and the packed value the model sees.
: RESERVED-IMMEDIATE ( -- )
   F-ADDI 18 2 0 X18   F-ADDI 1 18 0 X18
   F-SUBI 18 2 0 X18   F-SUBI 1 18 0 X18
   F-ANDI 18 2 $1007 $FF RESERVED-RC 0 RES+
   F-ANDI 1 18 $1007 $FF RESERVED-RC 0 RES+
   F-ORRI 18 2 $1000 1 RESERVED-RC 0 RES+
   F-ORRI 1 18 $1000 1 RESERVED-RC 0 RES+
   F-EORI 18 2 $100F $FFFF RESERVED-RC 0 RES+
   F-EORI 1 18 $100F $FFFF RESERVED-RC 0 RES+ ;

: RESERVED-SHIFT ( -- )
   F-LSLI 18 2 1 X18   F-LSLI 1 18 1 X18
   F-LSRI 18 2 0 X18   F-LSRI 1 18 0 X18
   F-ASRI 18 2 0 X18   F-ASRI 1 18 0 X18 ;

: RESERVED-MEMORY ( -- )
   F-LDR 18 15 0 X18   F-LDR 1 18 0 X18
   F-STR 18 15 0 X18   F-STR 1 18 0 X18
   F-LDRB 18 15 0 X18  F-LDRB 1 18 0 X18
   F-STRB 18 15 0 X18  F-STRB 1 18 0 X18
   F-LDRW 18 15 0 X18  F-LDRW 1 18 0 X18
   F-STRW 18 15 0 X18  F-STRW 1 18 0 X18
   F-LDAR 18 15 0 X18  F-LDAR 14 18 0 X18
   F-STLR 18 5 0 X18   F-STLR 14 18 0 X18 ;

: RESERVED-COMPARE-AND-BRANCH ( -- )
   F-CMP 18 8 0 X18    F-CMP 9 18 0 X18
   F-CMPI 18 16 0 X18
   F-CSET 18 0 0 X18
   F-CSEL 18 2 3 0 X184   F-CSEL 1 18 3 0 X184   F-CSEL 1 2 18 0 X184
   F-CBZ 18 0 0 X18
   F-CBNZ 18 0 0 X18
   F-ADR 18 0 0 X18
   F-BLR 18 0 0 X18
   F-BR 18 0 0 X18
   F-ICIVAU 18 0 0 X18
   F-DCCVAU 18 0 0 X18 ;

\ The control: an immediate that happens to be 18 is not a register, so it
\ still encodes. Without it a check that refused the number 18 everywhere,
\ rather than the register x18, would look correct here.
: RESERVED-CONTROL ( -- )
   F-LDR 1 2 144 0 0 $F9404841 RES+ ;

: RESERVED-VECTORS ( -- )
   RESERVED-MOVE-WIDE
   RESERVED-SHIFTED-REGISTER
   RESERVED-IMMEDIATE
   RESERVED-SHIFT
   RESERVED-MEMORY
   RESERVED-COMPARE-AND-BRANCH
   RESERVED-CONTROL ;

\ ---- the logical-immediate vectors -------------------------------------------

: LIMM-VECTORS ( -- )
   $2000000000000000 $10C0 LIM+
   $FF $1007 LIM+
   1 $1000 LIM+
   $FFFF $100F LIM+ ;

: BUILD-VECTORS ( -- )
   0 VEC-N !
   0 OOR-N !
   0 RES-N !
   0 LIM-N !
   MOVE-WIDE-VECTORS
   SHIFTED-REGISTER-VECTORS
   IMMEDIATE-VECTORS
   SHIFT-VECTORS
   MEMORY-VECTORS
   COMPARE-VECTORS
   CONDITIONAL-SELECT-VECTORS
   BRANCH-VECTORS
   SYSTEM-VECTORS
   REGISTER-RANGE-VECTORS
   IMMEDIATE-RANGE-VECTORS
   MISALIGNED-VECTORS
   RESERVED-VECTORS
   LIMM-VECTORS ;

BUILD-VECTORS

public

\ ---- reading the tables ------------------------------------------------------

: VECTORS ( -- n )        VEC-N @ ;
: OUT-OF-RANGES ( -- n )  OOR-N @ ;
: RESERVEDS ( -- n )      RES-N @ ;
: LIMMS ( -- n )          LIM-N @ ;

: ROW-FORM@ ( n -- n )    dup VEC-RANGE cells VEC-FORM + @ ;
: ROW-A@ ( n -- n )       dup VEC-RANGE cells VEC-A + @ ;
: ROW-B@ ( n -- n )       dup VEC-RANGE cells VEC-B + @ ;
: ROW-C@ ( n -- n )       dup VEC-RANGE cells VEC-C + @ ;
: ROW-D@ ( n -- n )       dup VEC-RANGE cells VEC-D + @ ;
: ROW-MASK@ ( n -- n )    dup VEC-RANGE cells VEC-MASK + @ ;
: ROW-WORD@ ( n -- n )    dup VEC-RANGE cells VEC-WORD + @ ;

: OOR-FORM@ ( n -- n )    dup OOR-RANGE cells OOR-FORM + @ ;
: OOR-A@ ( n -- n )       dup OOR-RANGE cells OOR-A + @ ;
: OOR-B@ ( n -- n )       dup OOR-RANGE cells OOR-B + @ ;
: OOR-C@ ( n -- n )       dup OOR-RANGE cells OOR-C + @ ;
: OOR-D@ ( n -- n )       dup OOR-RANGE cells OOR-D + @ ;

: RES-FORM@ ( n -- n )    dup RES-RANGE cells RES-FORM + @ ;
: RES-A@ ( n -- n )       dup RES-RANGE cells RES-A + @ ;
: RES-B@ ( n -- n )       dup RES-RANGE cells RES-B + @ ;
: RES-C@ ( n -- n )       dup RES-RANGE cells RES-C + @ ;
: RES-D@ ( n -- n )       dup RES-RANGE cells RES-D + @ ;
: RES-MASK@ ( n -- n )    dup RES-RANGE cells RES-MASK + @ ;
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
