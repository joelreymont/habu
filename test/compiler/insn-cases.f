\ insn-cases.f - the shared instruction rows, asked of the shipped assembler.
\
\ The module lives in `package COMPILER-INSN-CASES`. It reads the frozen rows in
\ `package COMPILER-INSN-PROOF` and drives each one through the REAL emitter
\ words - `MOVZ,`, `LDRB,`, `BL,`, `BCOND,` and the rest of
\ `src/arch/arm64/mnem.f` and `src/arch/arm64/icode.f` - into the real code
\ buffer, then reads the emitted word back out of that buffer through the same
\ `CW@` accessor the assembler itself writes through. Nothing here re-implements
\ an encoding: if a row passes, the shipped word really produced that number.
\
\ A branch row is emitted against a real label at the row's distance, so a
\ delta at or below zero goes through the immediate resolve in `BR-EMIT` and a
\ positive delta goes through the fixup record and the `LBL,` backpatch - the
\ path the snapshot relocation pass depends on.
\
\ Two halves, because one of them ends processes:
\
\   - `HABU-SIDE` runs in this engine. It covers the encoding vectors, the
\     reserved-register rows the shipped code does NOT refuse, and the `>LIMM`
\     bindings.
\   - `REFUSAL-SIDE` runs the rows the shipped code DOES refuse: the reserved
\     register, an operand outside its field, an operand a scale division would
\     round, and a mask `>LIMM` cannot pack. Every refusal reports itself by
\     ending the process with `die`, so each of those rows runs in a child
\     engine through `test/compiler/insn-refusal.f` and is judged by its exit
\     status.
\
\ Consumers: `test/compiler/insn-manifest.f` (the first half only) and
\ `test/compiler/insn-proof.f` (both, plus the proof assistant).

require lib/prelude.f
require lib/errors.f
require lib/string.f
require lib/fmt.f
require lib/test.f
require lib/test/outcome.f
require lib/process-command.f
require test/compiler/insn-schema.f

package COMPILER-INSN-CASES
using COMPILER-INSN-PROOF
private

\ ---- reading the code buffer -------------------------------------------------
\ `EMITW` stores the word little-endian at `CODE[ASM-CP]`; this is the same
\ address arithmetic read back.

: U32@ ( n -- n )
   CW@ dup c@
   swap 1 CODE-BYTE+ dup c@ $8 lshift
   swap 1 CODE-BYTE+ dup c@ $10 lshift
   swap 1 CODE-BYTE+ c@ $18 lshift
   or or or ;

: NOPS ( n -- ) {: k:n :}
   k 0 ?do NOP, loop ;

\ ---- calling the shipped mnemonic --------------------------------------------
\ One branch per form. `MOVZ,` and `MOVN,` fix the shifted-half field at zero,
\ so a row that wants a shifted half goes through `MOVZHW`/`MOVNHW` and the same
\ `EMITW`; both are the shipped encoder and the shipped store.

: MOVE-WIDE-CALL ( n n n n -- bool ) {: form:n a:n b:n c:n :}
   form F-MOVZ = if
      c 0= if a b MOVZ, else a b c MOVZHW EMITW then true exit then
   form F-MOVN = if
      c 0= if a b MOVN, else a b c MOVNHW EMITW then true exit then
   form F-MOVK = if a b c MOVK, true exit then
   false ;

: ARITH-CALL ( n n n n -- bool ) {: form:n a:n b:n c:n :}
   form F-ADD = if a b c ADD, true exit then
   form F-SUB = if a b c SUB, true exit then
   form F-AND = if a b c AND, true exit then
   form F-ORR = if a b c ORR, true exit then
   form F-EOR = if a b c EOR, true exit then
   form F-MUL = if a b c MUL, true exit then
   form F-SDIV = if a b c SDIV, true exit then
   form F-UDIV = if a b c UDIV, true exit then
   form F-LSLV = if a b c LSLV, true exit then
   form F-LSRV = if a b c LSRV, true exit then
   false ;

\ A logical-immediate mnemonic takes the plain MASK; `>LIMM` inside it builds
\ the packed value the row's third operand records, and that binding is checked
\ separately in `LIMM-ROW`.
: IMM-CALL ( n n n n n -- bool ) {: form:n a:n b:n c:n mask:n :}
   form F-ADDI = if a b c ADDI, true exit then
   form F-SUBI = if a b c SUBI, true exit then
   form F-ANDI = if a b mask ANDI, true exit then
   form F-ORRI = if a b mask ORRI, true exit then
   form F-EORI = if a b mask EORI, true exit then
   form F-LSLI = if a b c LSLI, true exit then
   form F-LSRI = if a b c LSRI, true exit then
   form F-ASRI = if a b c ASRI, true exit then
   false ;

: MEM-CALL ( n n n n -- bool ) {: form:n a:n b:n c:n :}
   form F-LDR = if a b c LDR, true exit then
   form F-STR = if a b c STR, true exit then
   form F-LDRB = if a b c LDRB, true exit then
   form F-STRB = if a b c STRB, true exit then
   form F-LDRW = if a b c LDRW, true exit then
   form F-STRW = if a b c STRW, true exit then
\ The two unscaled forms reach their encoder directly and go into the buffer
\ through the same EMITW every mnemonic ends in, exactly as a shifted move-wide
\ row does above. They have no mnemonic of their own because nothing writes
\ them by hand: the chain's emitter calls ENC-LDUR and ENC-STUR itself
\ (src/compiler/native/emit.f), so the encoder IS the shipped word here and a
\ mnemonic wrapper would be vocabulary the engine builder never writes.
   form F-LDUR = if a b c ENC-LDUR EMITW true exit then
   form F-STUR = if a b c ENC-STUR EMITW true exit then
   form F-LDAR = if a b LDAR, true exit then
   form F-STLR = if a b STLR, true exit then
   false ;

: COMPARE-CALL ( n n n -- bool ) {: form:n a:n b:n :}
   form F-CMP = if a b CMP, true exit then
   form F-CMPI = if a b CMPI, true exit then
   form F-CSET = if a b CSET, true exit then
   false ;

\ The two forms that carry four operands, which is why they have a branch of
\ their own rather than a fourth argument on the word above. They take the same
\ four in the same order; the file the three registers come out of is the whole
\ of what separates them.
: SELECT-CALL ( n n n n n -- bool ) {: form:n a:n b:n c:n d:n :}
   form F-CSEL = if a b c d CSEL, true exit then
   form F-FCSEL = if a b c d FCSEL, true exit then
   false ;

: SYSTEM-CALL ( n n -- bool ) {: form:n a:n :}
   form F-SVC = if a SVC, true exit then
   form F-RET = if RET, true exit then
   form F-BRK = if BRK, true exit then
   form F-NOP = if NOP, true exit then
   form F-DSB-ISH = if DSB-ISH, true exit then
   form F-ISB = if ISB, true exit then
   form F-BLR = if a BLR, true exit then
   form F-BR = if a BR, true exit then
   form F-ICIVAU = if a ICIVAU, true exit then
   form F-DCCVAU = if a DCCVAU, true exit then
   false ;

: BRANCH-CALL ( n n n -- ) {: form:n a:n lb:n :}
   form F-B = if lb B, exit then
   form F-BL = if lb BL, exit then
   form F-BCOND = if a lb BCOND, exit then
   form F-CBZ = if a lb CBZ, exit then
   form F-CBNZ = if a lb CBNZ, exit then
   form F-ADR = if a lb ADR, exit then
   E-CIE-FORM throw ;

\ The word delta a branch row asks for. `B,` and `BL,` carry the delta as their
\ only operand; the conditional and compare-and-branch forms carry it second;
\ `ADR,` carries a BYTE delta, and the assembler works in words.
: BRANCH-DELTA ( n n n -- n ) {: form:n a:n b:n :}
   form F-B = form F-BL = or if a exit then
   form F-ADR = if b 4 / exit then
   b ;

: BRANCH-REG ( n n -- n ) {: form:n a:n :}
   form F-B = form F-BL = or if 0 exit then
   a ;

\ Emit the branch against a label at the row's distance and answer the word
\ position it landed at. A delta at or below zero binds the label first and
\ resolves immediately; a positive delta records a fixup that `LBL,` backpatches.
: BRANCH-EMIT ( n n n -- n ) {: form:n reg:n dw:n :}
   LBL {: lb:n :}
   dw 0 <= if
      lb LBL,
      dw negate NOPS
      form reg lb BRANCH-CALL
      dw negate
   else
      form reg lb BRANCH-CALL
      dw 1- NOPS
      lb LBL,
      0
   then ;

public

\ Emit one row through the shipped words and answer the position of its word.
: EMIT-ROW ( n n n n n n -- n ) {: form:n a:n b:n c:n d:n mask:n :}
   ASM-INIT
   form FORM-BRANCH? if
      form  form a BRANCH-REG  form a b BRANCH-DELTA  BRANCH-EMIT exit
   then
   form a b c MOVE-WIDE-CALL if 0 exit then
   form a b c ARITH-CALL if 0 exit then
   form a b c mask IMM-CALL if 0 exit then
   form a b c MEM-CALL if 0 exit then
   form a b COMPARE-CALL if 0 exit then
   form a b c d SELECT-CALL if 0 exit then
   form a SYSTEM-CALL if 0 exit then
   E-CIE-FORM throw ;

: EMITTED-WORD ( n n n n n n -- n )
   EMIT-ROW U32@ ;

private

\ ---- the encoding vectors ----------------------------------------------------

: ROW-LABEL ( n -- ) {: r:n :}
   SB-RESET
   s" the shipped " SB-APPEND
   r ROW-FORM@ FORM-NAME$ SB-APPEND
   s"  emitter packs row " SB-APPEND
   r FMT:SB-INT
   s"  the way the ARM64 encoding says" SB-APPEND
   SB$ T-LABEL ;

: VECTOR-ROW ( n -- ) {: r:n :}
   r ROW-LABEL
   r ROW-FORM@ r ROW-A@ r ROW-B@ r ROW-C@ r ROW-D@ r ROW-MASK@ EMITTED-WORD
   r ROW-WORD@ T= ;

\ How many words the row should have left in the buffer. Every plain mnemonic
\ emits exactly one; a branch row also emits the filler that puts its label at
\ the row's distance, so it emits one more than the distance. Reading the wrong
\ position would otherwise answer with someone else's instruction, and a
\ mnemonic that started emitting two words would go unnoticed.
: ROW-WORDS ( n -- n ) {: r:n :}
   r ROW-FORM@ FORM-BRANCH? 0= if 1 exit then
   r ROW-FORM@ r ROW-A@ r ROW-B@ BRANCH-DELTA {: dw:n :}
   dw 0 <= if 1 dw - else dw then ;

: VECTOR-LENGTH ( n -- ) {: r:n :}
   s" the row left exactly the words its shape calls for" T-LABEL
   ASM-LEN 4 / r ROW-WORDS T= ;

: PHASE-VECTORS ( -- )
   VECTORS 0 ?do
      i VECTOR-ROW
      i VECTOR-LENGTH
   loop ;

\ Every form the model declares has to be asked about, or a form could be
\ renamed, re-based, or dropped with no row noticing.
: FORM-COVERED? ( n -- bool ) {: k:n :}
   VECTORS 0 ?do
      i ROW-FORM@ k = if true unloop exit then
   loop
   false ;

: PHASE-COVERAGE ( -- )
   FORMS 0 ?do
      SB-RESET
      s" the vectors ask about " SB-APPEND
      i FORM-NAME$ SB-APPEND
      SB$ T-LABEL
      i FORM-COVERED? TTRUE
   loop ;

\ ---- the reserved-register rows the shipped code lets through ----------------

: RESERVED-ALLOWED-ROW ( n -- ) {: r:n :}
   r RES-RC@ 0 <> if exit then
   SB-RESET
   s" x18 in that operand of " SB-APPEND
   r RES-FORM@ FORM-NAME$ SB-APPEND
   s"  is not refused, and encodes" SB-APPEND
   SB$ T-LABEL
   r RES-FORM@ r RES-A@ r RES-B@ r RES-C@ r RES-D@ r RES-MASK@ EMITTED-WORD
   r RES-WORD@ T= ;

: PHASE-RESERVED-ALLOWED ( -- )
   RESERVEDS 0 ?do i RESERVED-ALLOWED-ROW loop ;

\ ---- the logical-immediate bindings ------------------------------------------

: LIMM-ROW ( n -- ) {: r:n :}
   SB-RESET
   s" >LIMM packs mask row " SB-APPEND
   r FMT:SB-INT
   s"  into the value the model encodes" SB-APPEND
   SB$ T-LABEL
   r LIM-MASK@ >LIMM r LIM-NIS@ T= ;

: PHASE-LIMM ( -- )
   LIMMS 0 ?do i LIMM-ROW loop ;

public

: HABU-SIDE ( -- )
   PHASE-VECTORS
   PHASE-COVERAGE
   PHASE-RESERVED-ALLOWED
   PHASE-LIMM ;

private

\ ---- the rows that end the process -------------------------------------------
\ `XREG?` and `>LIMM` refuse by calling `die`, which writes a diagnostic and
\ exits 72. A refusal therefore cannot be observed from inside the engine that
\ triggered it, so each of these rows runs in a child engine and is judged by
\ its exit status.

600000 constant CHILD-MS

: FIXTURE$ ( -- ptr u8 n )
   s" test/compiler/insn-refusal.f" ;

: ROW-ENV$ ( -- ptr u8 n )
   s" HABU_INSN_RESERVED_ROW" ;

: RANGE-ENV$ ( -- ptr u8 n )
   s" HABU_INSN_RANGE_ROW" ;

: LIMM-ENV$ ( -- ptr u8 n )
   s" HABU_INSN_LIMM_ROW" ;

: INDEX$ ( n -- ptr u8 n ) {: k:n :}
   SB-RESET k FMT:SB-INT SB$ ;

: CHILD-OUTCOME ( ptr u8 n n -- outcome ) {: name:ptr nameu:n k:n :}
   PROC-CMD:RESET
   name nameu >LEN k INDEX$ >LEN PROC-CMD:ENV+
   s" --load" >LEN PROC-CMD:ARG+
   FIXTURE$ >LEN PROC-CMD:ARG+
   s" bin/hb" >LEN CHILD-MS >MS PROC-CMD:RUN-OUTCOME ;

: RESERVED-REFUSED-ROW ( n -- ) {: r:n :}
   r RES-RC@ 0= if exit then
   SB-RESET
   s" x18 in that operand of " SB-APPEND
   r RES-FORM@ FORM-NAME$ SB-APPEND
   s"  is refused before a word is emitted" SB-APPEND
   SB$ T-LABEL
   ROW-ENV$ r CHILD-OUTCOME
   r RES-RC@ T-OUTCOME-EXITED= ;

: OUT-OF-RANGE-ROW ( n -- ) {: r:n :}
   SB-RESET
   s" an operand outside its field in " SB-APPEND
   r OOR-FORM@ FORM-NAME$ SB-APPEND
   s"  is refused before a word is emitted" SB-APPEND
   SB$ T-LABEL
   RANGE-ENV$ r CHILD-OUTCOME
   RESERVED-RC T-OUTCOME-EXITED= ;

: LIMM-BAD-ROW ( n -- ) {: r:n :}
   s" >LIMM refuses a mask that is not a rotated run of ones" T-LABEL
   LIMM-ENV$ r CHILD-OUTCOME
   RESERVED-RC T-OUTCOME-EXITED= ;

public

: REFUSAL-SIDE ( -- )
   RESERVEDS 0 ?do i RESERVED-REFUSED-ROW loop
   OUT-OF-RANGES 0 ?do i OUT-OF-RANGE-ROW loop
   LIMM-BADS 0 ?do i LIMM-BAD-ROW loop ;

;using
;package
