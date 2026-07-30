\ reloc-vm.f - a small AArch64 machine that runs the shipped relocation passes.
\
\ The module lives in `package RELOC-VM`. It exists because of a hard problem in
\ binding a proof to this particular piece of code: the snapshot relocation
\ passes are EMITTED ASSEMBLY. `SNAP-RELOC:EMIT-CALLS`, `SNAP-RELOC:EMIT-XT` and
\ `SNAP-RELOC:EMIT-ADDRS` in src/habu/habu2.f are Forth words whose effect is to
\ write AArch64 instructions into the engine being built, and no test can call
\ the machine code they produce: it only exists inside a running engine, reached
\ from the snapshot writer and from the boot loader.
\
\ The weak answer would be to write the same arithmetic a second time in Habu
\ and check that copy against the model. That proves the copy and says nothing
\ about the engine. This module takes the other road. It reads the shipped
\ definition's own token stream through the shared source lexer, decodes it into
\ instructions, and RUNS them over a synthetic region and a synthetic call map.
\ The arithmetic under test is therefore the arithmetic in habu2.f, operand for
\ operand: change `7 10 2 ASRI,` to `3 ASRI` there and this machine computes a
\ different answer, exactly as the engine would.
\
\ What it is not. It is not an emulator of the engine, and it does not execute
\ the encoded instruction words - the encoders in src/arch/arm64 sit between
\ this source and the bytes a CPU sees, and they are covered by their own tests.
\ It reads a mnemonic and its operands and applies the meaning that mnemonic
\ has. So this machine binds the model to the SHIPPED INSTRUCTION SEQUENCE, one
\ step short of the shipped bytes: the encoders, and the machine code a CPU
\ finally runs, are the remaining step, and the dot records what is and is not
\ measured about it.
\
\ Fail-closed by construction. Every mnemonic, every operand spelling, every
\ condition code and every memory access has to be one the machine was taught.
\ An unknown mnemonic, an unresolvable name, a branch to a label that was never
\ defined, an operand count that does not match, a load or store outside the
\ segments the caller declared, or a run that does not finish inside its step
\ budget each raise a named error. Nothing is skipped and nothing defaults, so a
\ pass that grows an instruction this machine has never seen stops the gate
\ instead of being interpreted approximately.
\
\ Consumer: `test/compiler/reloc-cases.f`.

require lib/prelude.f
require lib/errors.f
require lib/string.f
require test/compiler/ir-id-source.f

package RELOC-VM
private

$100 constant CODE-CAP
32 constant LOC-CAP
32 constant SYM-CAP
16 constant GLB-CAP
8 constant SEG-CAP
$1000 constant MEM-CAP
8 constant PEND-CAP
$400 constant POOL-CAP
1000000 constant STEP-BUDGET
32 constant REG-N
$5EC0000000000000 constant MSG-VA   \ where an ADR of a diagnostic label points

$3A constant COLON-C
$2C constant COMMA-C
$24 constant DOLLAR-C
$2D constant MINUS-C

\ ---- instruction vocabulary --------------------------------------------------

0 constant OP-NOP
1 constant OP-MOVZ
2 constant OP-ADD
3 constant OP-ADDI
4 constant OP-SUB
5 constant OP-SUBI
6 constant OP-ORR
7 constant OP-ANDI
8 constant OP-LSLI
9 constant OP-LSRI
10 constant OP-ASRI
11 constant OP-CMP
12 constant OP-CMPI
13 constant OP-BCOND
14 constant OP-CBZ
15 constant OP-CBNZ
16 constant OP-B
17 constant OP-LDR
18 constant OP-STR
19 constant OP-LDRB
20 constant OP-LDRW
21 constant OP-STRW
22 constant OP-ADR
23 constant OP-SYS
24 constant OP-RET
25 constant OP-LIT64
26 constant OP-AND

\ ---- operand kinds -----------------------------------------------------------

0 constant K-NUM      \ a literal, a register number, or a resolved symbol
1 constant K-LOCAL    \ a label declared by this definition's locals group
2 constant K-GNAME    \ a label variable's name, before LABEL@ reads it
3 constant K-GLOBAL   \ that same label after LABEL@
4 constant K-SYSW     \ the write syscall
5 constant K-SYSX     \ the exit-group syscall

\ ---- state -------------------------------------------------------------------

create I-OP CODE-CAP cells allot
create I-A CODE-CAP cells allot
create I-B CODE-CAP cells allot
create I-C CODE-CAP cells allot

create LOC-TOK LOC-CAP cells allot
create LBL-AT LOC-CAP cells allot

create SYM-OFF SYM-CAP cells allot
create SYM-LEN SYM-CAP cells allot
create SYM-VAL SYM-CAP cells allot
create GLB-OFF GLB-CAP cells allot
create GLB-LEN GLB-CAP cells allot
create POOL POOL-CAP allot

create SEG-BASE SEG-CAP cells allot
create SEG-OFF SEG-CAP cells allot
create SEG-LEN SEG-CAP cells allot
create MEM MEM-CAP allot

create PEND-K PEND-CAP cells allot
create PEND-V PEND-CAP cells allot

create REG REG-N cells allot

variable CODE-N
variable LOC-N
variable SYM-N
variable GLB-N
variable POOL-U
variable SEG-N
variable PEND-N
variable PC
variable CMP-A
variable CMP-B
variable HALT
variable EXIT-RC
variable NUM-ACC
variable SCAN-K

\ ---- byte pool ---------------------------------------------------------------

: POOL+ ( ptr u8 n -- n ) {: a:ptr u:n :}
   POOL-U @ u + POOL-CAP > if E-CRL-DECODE throw then
   POOL-U @ {: at:n :}
   a POOL at + u BYTE-COPY
   at u + POOL-U !
   at ;

: POOL$ ( n n -- ptr u8 n ) {: off:n u:n :}
   POOL off + u ;

\ ---- number and name reading -------------------------------------------------

: HEX-DIGIT ( n -- n ) {: c:n :}
   c $30 >= c $39 <= and if c $30 - exit then
   c $41 >= c $46 <= and if c $41 - 10 + exit then
   c $61 >= c $66 <= and if c $61 - 10 + exit then
   E-CRL-DECODE throw ;

: HEX>N ( ptr u8 n -- n ) {: a:ptr u:n :}
   u 2 < if E-CRL-DECODE throw then
   0 NUM-ACC !
   u 1 ?do
      NUM-ACC @ 4 lshift a i + c@ HEX-DIGIT or NUM-ACC !
   loop
   NUM-ACC @ ;

: DEC-DIGIT? ( n -- bool ) {: c:n :}
   c $30 >= c $39 <= and ;

: DEC>N ( ptr u8 n -- n ) {: a:ptr u:n :}
   u 0= if E-CRL-DECODE throw then
   0 NUM-ACC !
   u 0 ?do
      a i + c@ {: c:n :}
      c DEC-DIGIT? 0= if E-CRL-DECODE throw then
      NUM-ACC @ 10 * c $30 - + NUM-ACC !
   loop
   NUM-ACC @ ;

: NUMERIC? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0= if false exit then
   a c@ DOLLAR-C = if true exit then
   a c@ DEC-DIGIT? if true exit then
   a c@ MINUS-C = u 1 > and ;

: NUM ( ptr u8 n -- n ) {: a:ptr u:n :}
   a c@ DOLLAR-C = if a u HEX>N exit then
   a c@ MINUS-C = if a 1+ u 1- DEC>N negate exit then
   a u DEC>N ;

\ The head of a name up to its type suffix: `scl:label` is the label `scl`.
: STEM$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u 0 ?do
      a i + c@ COLON-C = if a i unloop exit then
   loop
   a u ;

: COMMA-TAIL? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0= if false exit then
   a u 1- + c@ COMMA-C = ;

\ ---- the tables the caller fills ---------------------------------------------

public

\ Every bare name the shipped source may use, and what it stands for. The caller
\ reads each value out of a shipped source rather than writing it down, so a
\ renumbered register alias or condition code changes what this machine runs.
: SYM+ ( ptr u8 n n -- ) {: a:ptr u:n v:n :}
   SYM-N @ SYM-CAP >= if E-CRL-DECODE throw then
   a u POOL+ SYM-OFF SYM-N @ cells + !
   u SYM-LEN SYM-N @ cells + !
   v SYM-VAL SYM-N @ cells + !
   SYM-N @ 1+ SYM-N ! ;

\ A label variable the definition may take the address of. Named explicitly, so
\ a misspelled symbol cannot pass as one.
: GLABEL+ ( ptr u8 n -- ) {: a:ptr u:n :}
   GLB-N @ GLB-CAP >= if E-CRL-DECODE throw then
   a u POOL+ GLB-OFF GLB-N @ cells + !
   u GLB-LEN GLB-N @ cells + !
   GLB-N @ 1+ GLB-N ! ;

private

: SYM? ( ptr u8 n -- n ) {: a:ptr u:n :}
   SYM-N @ 0 ?do
      i cells SYM-OFF + @ i cells SYM-LEN + @ POOL$ a u STR= if
         i cells SYM-VAL + @ unloop exit
      then
   loop
   E-CRL-DECODE throw ;

: SYM-HAS? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   SYM-N @ 0 ?do
      i cells SYM-OFF + @ i cells SYM-LEN + @ POOL$ a u STR= if
         true unloop exit
      then
   loop
   false ;

: GLB-HAS? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   GLB-N @ 0 ?do
      i cells GLB-OFF + @ i cells GLB-LEN + @ POOL$ a u STR= if
         true unloop exit
      then
   loop
   false ;

: LOC-NAME$ ( n -- ptr u8 n ) {: k:n :}
   k cells LOC-TOK + @ COMPILER-ID-SRC:TOKEN$ STEM$ ;

: LOC? ( ptr u8 n -- n ) {: a:ptr u:n :}
   LOC-N @ 0 ?do
      i LOC-NAME$ a u STR= if i unloop exit then
   loop
   -1 ;

\ ---- decoding ----------------------------------------------------------------

: MNEM-IS? ( ptr u8 n ptr u8 n -- bool )
   STR= ;

\ The opcode and operand count of one mnemonic. Every instruction the two
\ shipped passes use is here; anything else stops the gate rather than being
\ approximated.
: MNEM ( ptr u8 n -- n n ) {: a:ptr u:n :}
   a u s" MOVZ," MNEM-IS? if OP-MOVZ 2 exit then
   a u s" ADD," MNEM-IS? if OP-ADD 3 exit then
   a u s" ADDI," MNEM-IS? if OP-ADDI 3 exit then
   a u s" SUB," MNEM-IS? if OP-SUB 3 exit then
   a u s" SUBI," MNEM-IS? if OP-SUBI 3 exit then
   a u s" ORR," MNEM-IS? if OP-ORR 3 exit then
   a u s" ANDI," MNEM-IS? if OP-ANDI 3 exit then
   a u s" AND," MNEM-IS? if OP-AND 3 exit then
   a u s" LSLI," MNEM-IS? if OP-LSLI 3 exit then
   a u s" LSRI," MNEM-IS? if OP-LSRI 3 exit then
   a u s" ASRI," MNEM-IS? if OP-ASRI 3 exit then
   a u s" CMP," MNEM-IS? if OP-CMP 2 exit then
   a u s" CMPI," MNEM-IS? if OP-CMPI 2 exit then
   a u s" BCOND," MNEM-IS? if OP-BCOND 2 exit then
   a u s" CBZ," MNEM-IS? if OP-CBZ 2 exit then
   a u s" CBNZ," MNEM-IS? if OP-CBNZ 2 exit then
   a u s" B," MNEM-IS? if OP-B 1 exit then
   a u s" LBL," MNEM-IS? if OP-NOP 1 exit then
   a u s" LDR," MNEM-IS? if OP-LDR 3 exit then
   a u s" STR," MNEM-IS? if OP-STR 3 exit then
   a u s" LDRB," MNEM-IS? if OP-LDRB 3 exit then
   a u s" LDRW," MNEM-IS? if OP-LDRW 3 exit then
   a u s" STRW," MNEM-IS? if OP-STRW 3 exit then
   a u s" ADR," MNEM-IS? if OP-ADR 2 exit then
   a u s" SYS," MNEM-IS? if OP-SYS 1 exit then
   a u s" RET," MNEM-IS? if OP-RET 0 exit then
   a u s" LIT64," MNEM-IS? if OP-LIT64 2 exit then
   E-CRL-DECODE throw ;

: PEND+ ( n n -- ) {: k:n v:n :}
   PEND-N @ PEND-CAP >= if E-CRL-DECODE throw then
   k PEND-K PEND-N @ cells + !
   v PEND-V PEND-N @ cells + !
   PEND-N @ 1+ PEND-N ! ;

: PEND-K@ ( n -- n ) {: i:n :}
   i 0 < i PEND-N @ >= or if E-CRL-DECODE throw then
   i cells PEND-K + @ ;

: PEND-V@ ( n -- n ) {: i:n :}
   i 0 < i PEND-N @ >= or if E-CRL-DECODE throw then
   i cells PEND-V + @ ;

: WANT-NUM ( n -- n ) {: i:n :}
   i PEND-K@ K-NUM <> if E-CRL-DECODE throw then
   i PEND-V@ ;

: WANT-LOCAL ( n -- n ) {: i:n :}
   i PEND-K@ K-LOCAL <> if E-CRL-DECODE throw then
   i PEND-V@ ;

: PUSH-TOKEN ( ptr u8 n -- ) {: a:ptr u:n :}
   a u NUMERIC? if K-NUM a u NUM PEND+ exit then
   a u LOC? {: id:n :}
   id 0 >= if K-LOCAL id PEND+ exit then
   a u s" NR-WRITE" STR= if K-SYSW 0 PEND+ exit then
   a u s" NR-EXIT-GROUP" STR= if K-SYSX 0 PEND+ exit then
   a u SYM-HAS? if K-NUM a u SYM? PEND+ exit then
   a u GLB-HAS? if K-GNAME 0 PEND+ exit then
   E-CRL-DECODE throw ;

\ `LABEL@` turns the label variable already pushed into an address operand.
: TAKE-ADDRESS ( -- )
   PEND-N @ 0= if E-CRL-DECODE throw then
   PEND-N @ 1- {: top:n :}
   top PEND-K@ K-GNAME <> if E-CRL-DECODE throw then
   K-GLOBAL top cells PEND-K + ! ;

: I+ ( n n n n -- ) {: op:n a:n b:n c:n :}
   CODE-N @ CODE-CAP >= if E-CRL-DECODE throw then
   op I-OP CODE-N @ cells + !
   a I-A CODE-N @ cells + !
   b I-B CODE-N @ cells + !
   c I-C CODE-N @ cells + !
   CODE-N @ 1+ CODE-N ! ;

\ A label definition: remember where it lands and leave a step that does
\ nothing, so an instruction index is a stable branch target.
: EMIT-LBL ( -- )
   0 PEND-K@ {: k:n :}
   k K-LOCAL = if
      0 PEND-V@ {: id:n :}
      id cells LBL-AT + @ 0 >= if E-CRL-DECODE throw then
      CODE-N @ id cells LBL-AT + !
   else
      k K-GLOBAL <> if E-CRL-DECODE throw then
   then
   OP-NOP 0 0 0 I+ ;

: EMIT-SYS ( -- )
   0 PEND-K@ {: k:n :}
   k K-SYSW = if OP-SYS 0 0 0 I+ exit then
   k K-SYSX <> if E-CRL-DECODE throw then
   OP-SYS 1 0 0 I+ ;

: EMIT-ADR ( -- )
   1 PEND-K@ K-GLOBAL <> if E-CRL-DECODE throw then
   OP-ADR 0 WANT-NUM 0 0 I+ ;

: EMIT-BRANCH ( n -- ) {: op:n :}
   op OP-B = if op 0 WANT-LOCAL 0 0 I+ exit then
   op 0 WANT-NUM 1 WANT-LOCAL 0 I+ ;

: EMIT-PLAIN ( n n -- ) {: op:n arity:n :}
   arity 0 = if op 0 0 0 I+ exit then
   arity 1 = if op 0 WANT-NUM 0 0 I+ exit then
   arity 2 = if op 0 WANT-NUM 1 WANT-NUM 0 I+ exit then
   op 0 WANT-NUM 1 WANT-NUM 2 WANT-NUM I+ ;

: EMIT ( ptr u8 n -- ) {: a:ptr u:n :}
   a u MNEM {: op:n arity:n :}
   PEND-N @ arity <> if E-CRL-DECODE throw then
   a u s" LBL," STR= if EMIT-LBL else
   op OP-SYS = if EMIT-SYS else
   op OP-ADR = if EMIT-ADR else
   op OP-BCOND = op OP-CBZ = or op OP-CBNZ = or op OP-B = or if
      op EMIT-BRANCH
   else
      op arity EMIT-PLAIN
   then then then then
   0 PEND-N ! ;

\ The locals group declares this definition's labels. Its names are gathered
\ first so a later `scl LBL,` is recognised as a label definition rather than as
\ an unknown symbol, and decoding starts after the group's closer.
: LOCALS-OPEN ( n n -- n ) {: b:n e:n :}
   b SCAN-K !
   begin SCAN-K @ e < while
      SCAN-K @ COMPILER-ID-SRC:TOKEN$ s" {:" STR= if SCAN-K @ 1+ exit then
      SCAN-K @ 1+ SCAN-K !
   repeat
   -1 ;

: LOCAL+ ( n -- ) {: k:n :}
   k COMPILER-ID-SRC:WORD-TOKEN? 0= if exit then
   LOC-N @ LOC-CAP >= if E-CRL-DECODE throw then
   k LOC-TOK LOC-N @ cells + !
   LOC-N @ 1+ LOC-N ! ;

: GATHER-LOCALS ( n n -- n ) {: b:n e:n :}
   0 LOC-N !
   b e LOCALS-OPEN {: start:n :}
   start 0 < if b exit then
   start SCAN-K !
   begin SCAN-K @ e < while
      SCAN-K @ COMPILER-ID-SRC:TOKEN$ s" :}" STR= if SCAN-K @ 1+ exit then
      SCAN-K @ LOCAL+
      SCAN-K @ 1+ SCAN-K !
   repeat
   E-CRL-DECODE throw ;

: DECODE-TOKEN ( n -- ) {: k:n :}
   k COMPILER-ID-SRC:WORD-TOKEN? 0= if exit then
   k COMPILER-ID-SRC:TOKEN$ {: t:ptr tu:n :}
   t tu COMMA-TAIL? if t tu EMIT exit then
   t tu s" LABEL@" STR= if TAKE-ADDRESS exit then
   t tu PUSH-TOKEN ;

public

\ Forget the symbol tables, the code and the segments. A caller decodes one
\ definition at a time.
: RESET ( -- )
   0 CODE-N !  0 LOC-N !  0 SYM-N !  0 GLB-N !  0 POOL-U !
   0 SEG-N !  0 PEND-N !
   LOC-CAP 0 ?do -1 LBL-AT i cells + ! loop ;

\ Decode the named definition out of the source the shared lexer last scanned.
\ The whole body is decoded: an instruction the machine has never seen, a name
\ it cannot resolve, or operands left over at the end each raise E-CRL-DECODE.
: DECODE ( ptr u8 n -- ) {: a:ptr u:n :}
   0 CODE-N !  0 PEND-N !
   LOC-CAP 0 ?do -1 LBL-AT i cells + ! loop
   a u COMPILER-ID-SRC:BODY-SPAN {: b:n e:n :}
   b e GATHER-LOCALS {: start:n :}
   e start ?do i DECODE-TOKEN loop
   PEND-N @ 0<> if E-CRL-DECODE throw then ;

: INSTRUCTIONS ( -- n )
   CODE-N @ ;

\ ---- the machine's memory ----------------------------------------------------
\ A segment maps a span of the machine's address space onto a window of the
\ machine's own store. An access outside every segment is a fault, not a wrapped
\ or ignored write, so a pass that indexes the wrong band stops the gate rather
\ than quietly reading whatever is next door.

\ Declare one span: the address the pass will see, where it lives in the store,
\ and how long it is.
: SEG+ ( n n n -- ) {: base:n off:n u:n :}
   SEG-N @ SEG-CAP >= if E-CRL-FAULT throw then
   off 0 < off u + MEM-CAP > or u 0 < or if E-CRL-FAULT throw then
   base SEG-BASE SEG-N @ cells + !
   off SEG-OFF SEG-N @ cells + !
   u SEG-LEN SEG-N @ cells + !
   SEG-N @ 1+ SEG-N ! ;

\ Forget every span and clear the store, so one row cannot read what the row
\ before it left behind.
: SEG-RESET ( -- )
   0 SEG-N !
   MEM-CAP 0 ?do 0 MEM i + c! loop ;

private

: SEG-HIT? ( n n -- bool ) {: addr:n i:n :}
   i cells SEG-BASE + @ {: base:n :}
   addr base >= addr base i cells SEG-LEN + @ + < and ;

: SEG-PTR ( n n -- ptr u8 ) {: addr:n i:n :}
   MEM i cells SEG-OFF + @ + addr i cells SEG-BASE + @ - + ;

: ADDR>PTR ( n -- ptr u8 ) {: addr:n :}
   SEG-N @ 0 ?do
      addr i SEG-HIT? if addr i SEG-PTR unloop exit then
   loop
   E-CRL-FAULT throw ;

: L1 ( n -- n )
   ADDR>PTR c@ ;

: S1 ( n n -- ) {: v:n addr:n :}
   v $FF and addr ADDR>PTR c! ;

: LN ( n n -- n ) {: addr:n n:n :}
   0 NUM-ACC !
   n 0 ?do
      addr i + L1 i 8 * lshift NUM-ACC @ or NUM-ACC !
   loop
   NUM-ACC @ ;

: SN ( n n n -- ) {: v:n addr:n n:n :}
   n 0 ?do
      v i 8 * rshift addr i + S1
   loop ;

\ ---- registers and flags -----------------------------------------------------

: RG@ ( n -- n ) {: r:n :}
   r 0 < r REG-N >= or if E-CRL-DECODE throw then
   r cells REG + @ ;

: RG! ( n n -- ) {: v:n r:n :}
   r 0 < r REG-N >= or if E-CRL-DECODE throw then
   v r cells REG + ! ;

: ASR ( n n -- n ) {: x:n s:n :}
   s 0= if x exit then
   x 0< if x s rshift -1 64 s - lshift or exit then
   x s rshift ;

\ An unsigned comparison of two 64-bit registers. Habu's `<` reads a cell as
\ signed, so both operands are folded by the sign bit first; that turns the
\ unsigned order into the signed one without narrowing either value. The
\ address-literal pass compares a chain's value against its band this way, and
\ modelling those compares as signed would let a band above 2^63 pass a test the
\ hardware fails.
$8000000000000000 constant SIGN-FOLD

: ULT ( n n -- bool ) {: a:n b:n :}
   a SIGN-FOLD xor b SIGN-FOLD xor < ;

\ The conditions the shipped passes use. One they do not use raises rather than
\ guessing, so a pass that starts branching on a new condition stops the gate.
: COND? ( n -- bool ) {: c:n :}
   c s" C-EQ" SYM? = if CMP-A @ CMP-B @ = exit then
   c s" C-NE" SYM? = if CMP-A @ CMP-B @ <> exit then
   c s" C-GE" SYM? = if CMP-A @ CMP-B @ >= exit then
   c s" C-LT" SYM? = if CMP-A @ CMP-B @ < exit then
   c s" C-GT" SYM? = if CMP-A @ CMP-B @ > exit then
   c s" C-LE" SYM? = if CMP-A @ CMP-B @ <= exit then
   c s" C-CC" SYM? = if CMP-A @ CMP-B @ ULT exit then
   c s" C-CS" SYM? = if CMP-A @ CMP-B @ ULT 0= exit then
   c s" C-HI" SYM? = if CMP-B @ CMP-A @ ULT exit then
   E-CRL-DECODE throw ;

: TARGET ( n -- n ) {: id:n :}
   id 0 < id LOC-CAP >= or if E-CRL-DECODE throw then
   id cells LBL-AT + @ {: at:n :}
   at 0 < if E-CRL-DECODE throw then
   at ;

\ ---- one step ----------------------------------------------------------------

: STEP-ARITH ( n n n n -- ) {: op:n d:n n:n m:n :}
   op OP-ADD = if n RG@ m RG@ + d RG! exit then
   op OP-ADDI = if n RG@ m + d RG! exit then
   op OP-SUB = if n RG@ m RG@ - d RG! exit then
   op OP-SUBI = if n RG@ m - d RG! exit then
   op OP-ORR = if n RG@ m RG@ or d RG! exit then
   op OP-ANDI = if n RG@ m and d RG! exit then
   op OP-AND = if n RG@ m RG@ and d RG! exit then
   op OP-LSLI = if n RG@ m lshift d RG! exit then
   op OP-LSRI = if n RG@ m rshift d RG! exit then
   op OP-ASRI = if n RG@ m ASR d RG! exit then
   E-CRL-DECODE throw ;

: STEP-MEM ( n n n n -- ) {: op:n d:n base:n off:n :}
   base RG@ off + {: at:n :}
   op OP-LDR = if at 8 LN d RG! exit then
   op OP-STR = if d RG@ at 8 SN exit then
   op OP-LDRB = if at 1 LN d RG! exit then
   op OP-LDRW = if at 4 LN d RG! exit then
   op OP-STRW = if d RG@ at 4 SN exit then
   E-CRL-DECODE throw ;

: STEP-BRANCH ( n n n -- ) {: op:n a:n b:n :}
   op OP-B = if a TARGET PC ! exit then
   op OP-BCOND = if a COND? if b TARGET PC ! then exit then
   op OP-CBZ = if a RG@ 0= if b TARGET PC ! then exit then
   a RG@ 0<> if b TARGET PC ! then ;

: BRANCH-OP? ( n -- bool ) {: op:n :}
   op OP-B = op OP-BCOND = or op OP-CBZ = or op OP-CBNZ = or ;

: MEM-OP? ( n -- bool ) {: op:n :}
   op OP-LDR = op OP-STR = or op OP-LDRB = or op OP-LDRW = or op OP-STRW = or ;

: STEP ( -- )
   PC @ {: at:n :}
   at 0 < at CODE-N @ >= or if E-CRL-DECODE throw then
   at cells I-OP + @ {: op:n :}
   at cells I-A + @ {: a:n :}
   at cells I-B + @ {: b:n :}
   at cells I-C + @ {: c:n :}
   at 1+ PC !
   op OP-NOP = if exit then
   op OP-RET = if 1 HALT ! exit then
   op OP-MOVZ = if b a RG! exit then
   op OP-LIT64 = if b a RG! exit then
   op OP-ADR = if MSG-VA a RG! exit then
   op OP-CMP = if a RG@ CMP-A ! b RG@ CMP-B ! exit then
   op OP-CMPI = if a RG@ CMP-A ! b CMP-B ! exit then
   op OP-SYS = if
      a 1 = if 2 HALT ! 0 RG@ EXIT-RC ! then
      exit
   then
   op BRANCH-OP? if op a b STEP-BRANCH exit then
   op MEM-OP? if op a b c STEP-MEM exit then
   op a b c STEP-ARITH ;

public

: R! ( n n -- )
   RG! ;

: R@ ( n -- n )
   RG@ ;

\ Read and write the machine's memory the way the pass sees it: by address and
\ width, through the same segment check, so a fixture that lays out its region
\ wrongly faults here instead of silently agreeing with itself.
: PEEK ( n n -- n )
   LN ;

: POKE ( n n n -- )
   SN ;

\ Run the decoded pass from its first instruction until it returns or exits. A
\ run that does neither inside the step budget faults rather than hanging.
: RUN ( -- )
   0 PC !  0 HALT !  0 EXIT-RC !  0 CMP-A !  0 CMP-B !
   STEP-BUDGET 0 ?do
      HALT @ 0<> if unloop exit then
      STEP
   loop
   E-CRL-FAULT throw ;

\ Zero when the pass returned to its caller, otherwise the status it exited
\ with.
: HALT-CODE ( -- n )
   HALT @ 2 = if EXIT-RC @ exit then
   0 ;

;package
