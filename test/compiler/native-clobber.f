\ native-clobber.f - published clobber rows and their call-site effects.

require lib/test.f
require lib/codegen.f
require src/compiler/native/compiler.f
require src/compiler/native/clobber.f

package NCLOB-TEST

private

\ `evaluate` is the metaprogramming boundary the checker does not model, and it
\ is how this suite compiles a caller for a word that did not exist when the
\ suite was compiled.
TRUSTED: EV ( ptr u8 n -- )
   evaluate ;

TRUSTED: EV-N ( ptr u8 n -- n )
   evaluate ;

8 constant REGS                      \ scratch registers the compiled routines may use
0 constant GLOBAL-WID
4 constant INSN-BYTES

: HOST-N ( n n -- n )
   HB-TARGET-LINUX? if drop exit then
   HB-TARGET-MACOS? if nip exit then
   E-CTGT-ABI throw ;

\ ---- addresses no code occupies ----------------------------------------------
\ The row rules are about a table keyed by an address, so the cases that are
\ about the TABLE use addresses of their own rather than borrowing a published
\ routine's: a case that recorded a second row over a real publication would be
\ changing what a real caller was compiled against. They are instruction aligned
\ because every address this record is ever handed is.
$10000 constant A1
$10004 constant A2
$10008 constant A3

: GPRS ( n -- A64EFF:gprs )
   A64EFF:GPR-SET ;

: FPRS ( n -- A64EFF:fprs )
   A64EFF:FPR-SET ;

: GPR-AT ( n -- n )
   {: e:n :}
   e A64EFF:GPR-ALL NCLOB:GPR-CLOB A64EFF:GPRS-N ;

: FPR-AT ( n -- n )
   {: e:n :}
   e A64EFF:FPR-ALL NCLOB:FPR-CLOB A64EFF:FPRS-N ;

\ How many general registers a row names. The cases that are about a COUNT of
\ saves ask this rather than writing the same width down a second time as a
\ number, so a row that changed width cannot leave a count agreeing with a stale
\ one. The loop runs the register file's own width, which is the reach of the
\ field the set is stored in.
: ROW-WIDTH ( n -- n )
   {: e:n :}
   e GPR-AT {: bits:n :}
   0
   A64EFF:FILE-SIZE 0 ?do
      bits 1 i lshift and 0<> if 1+ then
   loop ;

\ The same row with its lowest-numbered register taken out: a strict subset of
\ any non-empty set, and the empty set when the row named one register. Clearing
\ the lowest set bit is `v and (v-1)`, and it is used where a case needs a row
\ that is narrower than a real one WITHOUT knowing how wide the real one is.
: WITHOUT-LOWEST ( n -- n )
   {: v:n :}
   v v 1 - and ;

: FLAG# ( bool -- n )
   if 1 else 0 then ;

: RECORD-CASES ( -- )
   s" an address with no row answers the worst case the caller states" T-LABEL
   A3 GPR-AT  A64EFF:GPR-ALL A64EFF:GPRS-N T=
   A3 NCLOB:KNOWN? FLAG# 0 T=

   s" a recorded row answers what it was told, per file" T-LABEL
   A1  $5 GPRS  $3 FPRS  NCLOB:RECORD
   A1 NCLOB:KNOWN? FLAG# 1 T=
   A1 GPR-AT $5 T=
   A1 FPR-AT $3 T=

   s" and a second address is a second row, not an overwrite" T-LABEL
   A2  $8 GPRS  $0 FPRS  NCLOB:RECORD
   A2 GPR-AT $8 T=
   A1 GPR-AT $5 T=

   s" a row may be re-recorded NARROWER, because every caller saved more" T-LABEL
   A1  $4 GPRS  $1 FPRS  NCLOB:RECORD
   A1 GPR-AT $4 T=
   A1 FPR-AT $1 T=

   s" and the same set again is the same row" T-LABEL
   A1  $4 GPRS  $1 FPRS  NCLOB:RECORD
   A1 GPR-AT $4 T= ;

\ A widening is refused per FILE, because a caller skipped saving in both.
: WIDEN-CASES ( -- )
   s" a row that would destroy a general register it did not is refused" T-LABEL
   [: A1  $C GPRS  $1 FPRS  NCLOB:RECORD ;] E-NCLOB-WIDEN TTHROWSQ

   s" and so is one that would destroy a floating register it did not" T-LABEL
   [: A1  $4 GPRS  $3 FPRS  NCLOB:RECORD ;] E-NCLOB-WIDEN TTHROWSQ

   s" a refused widening leaves the row it was refused against" T-LABEL
   A1 GPR-AT $4 T=
   A1 FPR-AT $1 T= ;

\ ---- reading a live word's own machine code ----------------------------------
\ The code start and the code length come off the word's own dictionary record,
\ which is where the publication seam wrote them.
variable CODE-AT

: CODE-PTR ( -- ptr u8 )
   CODE-AT 0 ptr-field @ ;

: U32@ ( ptr u8 -- n ) {: p:ptr :}
   p c@
   p 1 + c@ 8 lshift or
   p 2 + c@ 16 lshift or
   p 3 + c@ 24 lshift or ;

\ The Str and Ldr of a whole cell against the data-stack pointer, in BOTH of the
\ addressing modes the chain writes them in. The two directions differ in one bit
\ of the opc field, and the base register is the field bits nine to five, so this
\ is the form and the register and nothing else about the instruction.
\
\ WHY THERE ARE FOUR FORMS AND NOT TWO. Since dot habu-place-the-data-9f128e58
\ the chain stands its data-stack pointer where the fewest adjustments are
\ needed, so a cell it reaches can be UNDER the pointer as well as over it, and
\ under it is the unscaled signed encoding - Ldur and Stur - of the same access.
\ A counter that knew only the scaled forms would report a routine that touches
\ the caller's stack as touching it not at all, which is exactly the claim these
\ cases are about.
$FFC00000 constant MEM-MASK
$F9000000 constant STR-OP
$F9400000 constant LDR-OP
$F8000000 constant STUR-OP
$F8400000 constant LDUR-OP

: DSTACK-AT? ( n n -- bool ) {: w:n op:n :}
   w MEM-MASK and op =
   w 5 rshift $1F and  A64EFF:DSTACK-GPR =  and ;

: DS-COUNT ( ptr u8 n n -- n ) {: a:ptr u:n op:n :}
   a u GLOBAL-WID XREF-FIND-WL
   dup XREF-FOUND? 0= if drop E-NPUB-NAME throw then
   dup XREF-START CODE-AT !
   XREF-LEN {: len:n :}
   0
   len INSN-BYTES / 0 ?do
      CODE-PTR i INSN-BYTES * + U32@ op DSTACK-AT? if 1+ then
   loop ;

: DS-STORES ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u STR-OP DS-COUNT
   a u STUR-OP DS-COUNT + ;

: DS-LOADS ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u LDR-OP DS-COUNT
   a u LDUR-OP DS-COUNT + ;

: ENTRY-OF ( ptr u8 n -- n )
   GLOBAL-WID XREF-FIND-WL
   dup XREF-FOUND? 0= if drop E-NPUB-NAME throw then
   XREF-START ;

: DEFINED? ( ptr u8 n -- bool )
   GLOBAL-WID XREF-FIND-WL XREF-FOUND? ;

\ ---- what a real publication records -----------------------------------------
\ The callee below is compiled through the production source boundary
\ and the seam recorded what it destroys. Its body is six additions rather than
\ one because a one-addition body is copied into its caller instead
\ of calling: the head of this file says why that would leave nothing to measure.
: STEP! ( -- )
   s" : NCLOB-STEP ( n -- n ) 1 + 2 + 3 + 4 + 5 + 6 + ;" EV ;

: PUBLISHED-CASES ( -- )
   s" a compilation records a row for the address it published at" T-LABEL
   s" NCLOB-STEP" ENTRY-OF NCLOB:KNOWN? FLAG# 1 T=

   s" and the row is narrower than the whole register file" T-LABEL
   s" NCLOB-STEP" ENTRY-OF GPR-AT  A64EFF:GPR-ALL A64EFF:GPRS-N T<>

   s" and it names no register outside the pool the routine was given" T-LABEL
   s" NCLOB-STEP" ENTRY-OF GPR-AT
   1 REGS lshift 1 -  invert and  0 T= ;

\ ---- the narrowing, measured -------------------------------------------------
\ The loop's index and limit are live across every call. The accumulator is the
\ call's own argument and goes through a slot, so the counts below pin the saves
\ left after the callee's published clobber row narrows them.
\
\ WHY THE COUNTS ARE SMALL RATHER THAN EIGHT AND EIGHT. The residency pass in
\ src/compiler/native/select.f writes no store for a value the cell it would
\ write already holds, and builds no load for a value nothing reads out of a
\ register. The accumulator is handed in on the caller's stack, handed straight
\ to the callee and handed straight back, so it crosses the body without ever
\ reaching a register. What remains is the caller's own entry traffic.

: NARROW! ( -- )
   s" : NCLOB-NARROW ( n n -- n ) {: seed:n len:n :} seed len 0 ?do NCLOB-STEP NCLOB-STEP loop ;"
   EV ;

: NARROW-CASES ( -- )
   s" the caller answers what its body says" T-LABEL
   s" 0 4 NCLOB-NARROW" EV-N 168 T=
   s" 5 0 NCLOB-NARROW" EV-N 5 T=

   s" the published row removes unnecessary caller saves" T-LABEL
   s" NCLOB-NARROW" DS-STORES 0 T=
   s" NCLOB-NARROW" DS-LOADS 1 T= ;

\ ---- the narrowing, measured where it can actually be lost -------------------
\ The case above leaves most of the pool free, so its row has slack. The pressure
\ caller below holds more live values than the row leaves free; every register
\ the row names then moves exactly one store and one load. Its control has one
\ fewer live value and fits without those saves.
\
\ WHAT MAKES THIS PAIR PRESS, AND WHY IT NEEDS ITS OWN CALLEE. Two things have to
\ be true at once and neither is true of the callee the case above uses. The
\ callee's row has to name SEVERAL registers, or there is nothing for a caller to
\ save and nothing a deleted register could change; and the caller has to hold
\ more values across the call than the row leaves free, or the allocator has room
\ to keep them all out of the row's way. NCLOB-STEP is six additions of small
\ constants, and since the selection stage emits the immediate forms
\ (src/compiler/native/select.f) none of those constants is ever materialised:
\ its whole emission lives in ONE register, so its row names one and a caller of
\ it can never save more than one thing. The pressure case therefore has a callee
\ of its own, sized for the property.
\
\ AND THE CALLEE'S PRESSURE IS OF A KIND NO LATER PASS CAN TAKE AWAY. Its body
\ interleaves multiplication with xor and and, over five distinct constants each
\ read once, and KEEPS THREE OF ITS OWN INTERMEDIATES LIVE AT A TIME. That last
\ property is the one the row rests on, and it is worth saying plainly because
\ the others have since been eaten and the row did not move.
\
\ WHAT THE COMBINE PASS TOOK, AND WHY IT DID NOT MATTER. The logical-immediate
\ fold now folds `5 xor`, `7 and` and `13 xor` into the instruction, so three of
\ the five constants no longer reach a register at all. The row is still exactly
\ three registers wide, because those constants were never what made it three:
\ each was live for the one instruction that read it, while the three
\ intermediates are live ACROSS each other. A fold that removes a value with a
\ one-instruction live range cannot narrow a row that a simultaneous liveness
\ made. The multiplies are what keeps that liveness reachable - a multiplication
\ has no immediate form for any of these folds to choose, so `3 *` and `11 *`
\ still materialise their constants and still force the intermediates apart.
\
\ Mixing the bitwise operations in makes the routine NOT a linear function of its
\ argument, so no reassociation can gather the whole body into a single multiply
\ the way it could if the body were four multiplies and three adds - which is the
\ shape this callee was almost written as, and the shape a future arithmetic pass
\ would have collapsed to one register. That is what makes the three registers
\ this row names a fact about the routine and not about the compiler that happens
\ to be compiling it today. The row's width is not written down twice: the store
\ count is asserted to BE the number of registers the row names, so a change to
\ either has to be a change to both.
\
\ THE CALLER IS SIZED AGAINST THE MACHINE, not against a budget: no caller states
\ one any more, so the pressure has to be real. NABI:SCRATCH leaves a Linux
\ routine twenty-five registers and a Darwin routine twenty-four. The generated
\ caller carries twenty-three sums on Linux or twenty-two on Darwin, keeping the
\ same pressure against the three-register row on both hosts.
\
\ THE CONTROL IS THE SAME BODY ONE VALUE SMALLER, against the same callee,
\ because without it "spills three" could mean "always spills three". One sum
\ fewer and the allocator has room to keep every live value out of the row, and
\ the count drops to the caller's own entry traffic.
: PSTEP! ( -- )
   s" : NCLOB-PSTEP ( n -- n ) dup 3 * over 5 xor + swap 7 and + dup 11 * + 13 xor ;"
   EV ;

1024 CODEGEN:BUFFER PRESSURE-TEXT

: PRESSURE-SRC+ ( ptr u8 n -- )
   PRESSURE-TEXT CODEGEN:APPEND-STRING ;

: PRESSURE-TERMS ( -- n )
   23 22 HOST-N ;

: PRESSURE-SRC ( ptr u8 n ptr u8 n n -- ptr u8 n )
   {: name:ptr nameu:n callee:ptr calleeu:n terms:n :}
   PRESSURE-TEXT CODEGEN:RESET
   s" : " PRESSURE-SRC+  name nameu PRESSURE-SRC+
   s"  ( n -- n ) {: s:n :} " PRESSURE-SRC+
   terms 1 + 1 ?do
      s" s " PRESSURE-SRC+
      i PRESSURE-TEXT CODEGEN:APPEND-DECIMAL
      s"  + " PRESSURE-SRC+
   loop
   s" s " PRESSURE-SRC+  callee calleeu PRESSURE-SRC+  s"  " PRESSURE-SRC+
   terms 0 ?do s" + " PRESSURE-SRC+ loop
   s" ;" PRESSURE-SRC+
   PRESSURE-TEXT CODEGEN:CONTENTS ;

: PN! ( -- )
   s" NCLOB-PN" s" NCLOB-PSTEP" PRESSURE-TERMS PRESSURE-SRC EV ;

: PC! ( -- )
   s" NCLOB-PC" s" NCLOB-PSTEP" PRESSURE-TERMS 1 - PRESSURE-SRC
   EV ;

: PRESSURE-CASES ( -- )
   s" the pressure caller answers what its body says" T-LABEL
   s" 5 NCLOB-PN" EV-N 644 616 HOST-N T=
   s" 0 NCLOB-PN" EV-N 325 302 HOST-N T=

   s" the pressure callee really destroys three registers" T-LABEL
   s" NCLOB-PSTEP" ENTRY-OF GPR-AT $7 T=
   s" NCLOB-PSTEP" ENTRY-OF ROW-WIDTH 3 T=

   s" a caller with more live values than the row leaves room for saves one per register the row names" T-LABEL
   s" NCLOB-PN" DS-STORES  s" NCLOB-PSTEP" ENTRY-OF ROW-WIDTH T=
   s" NCLOB-PN" DS-STORES 3 T=
   s" NCLOB-PN" DS-LOADS 3 T=

   s" and one value fewer fits inside the room, leaving only its own traffic" T-LABEL
   s" NCLOB-PC" DS-STORES 1 T=
   s" NCLOB-PC" DS-STORES  s" NCLOB-PN" DS-STORES  < TTRUE ;

\ ---- a row dies with the code it describes ------------------------------------
\ The engine compiles every definition into one bump pointer and FORGET-DEFS-FROM
\ moves that pointer BACK to the start of the record it forgets. A compiled
\ word's record starts at the address the publication seam wrote its routine at,
\ so forgetting the compiled word puts the free code slot exactly there and the
\ next definition the engine compiles is written over that routine. Everything
\ below therefore drives the engine's own FORGET, and the collision is ASSERTED
\ rather than assumed - if the engine ever stopped reusing the slot the case
\ would go red instead of quietly measuring nothing.
variable ROWS-BEFORE
variable GONE-ENTRY

: BUILD-RECLAIMED ( -- )
   NCLOB:ROWS ROWS-BEFORE !
   s" : NCLOB-GONE ( n -- n ) 1 + 2 + 3 + 4 + 5 + 6 + ;" EV
   s" NCLOB-GONE" ENTRY-OF GONE-ENTRY ! ;

: RECLAIM-CASES ( -- )
   s" a compilation is recorded at the address it published at" T-LABEL
   GONE-ENTRY @ NCLOB:KNOWN? FLAG# 1 T=
   NCLOB:ROWS ROWS-BEFORE @ 1+ T=

   s" forgetting it puts the free code slot back at that address" T-LABEL
   s" NCLOB-GONE" FORGET-DEFS-FROM
   cp@ GONE-ENTRY @ T=

   s" and the row went with the code, giving its table slot back" T-LABEL
   GONE-ENTRY @ NCLOB:KNOWN? FLAG# 0 T=
   NCLOB:ROWS ROWS-BEFORE @ T=

   s" the next production definition takes that exact slot" T-LABEL
   s" : NCLOB-RECYCLED ( n -- n ) 1 + 2 + 3 + 4 + 5 + 6 + ;" EV
   s" NCLOB-RECYCLED" ENTRY-OF GONE-ENTRY @ T=

   s" and the new publication owns a live row at the reused slot" T-LABEL
   s" NCLOB-RECYCLED" ENTRY-OF NCLOB:KNOWN? FLAG# 1 T=
   NCLOB:ROWS ROWS-BEFORE @ 1+ T=

   s" and a floor above the free slot reclaims nothing and is refused" T-LABEL
   [: cp@ INSN-BYTES + CODE-RECLAIM:TRUNCATE ;] CODE-RECLAIM:E-FLOOR TTHROWSQ ;

\ A caller of the word at the recycled slot, with ten values live across
\ the call so that skipping the save of even one of them shows up in the answer:
\ 55 from the ten sums plus 21 from the callee is 76, and the stale row made this
\ caller answer 86.

: RECLAIM-CALLER! ( -- )
   s" : NCLOB-RECYCLED-CALLER ( n -- n ) {: s:n :} s 1 + s 2 + s 3 + s 4 + s 5 + s 6 + s 7 + s 8 + s 9 + s 10 + s NCLOB-RECYCLED + + + + + + + + + + ;"
   EV ;

: RECLAIM-CALLER-CASES ( -- )
   s" a caller of the word at a reclaimed slot computes what its body says" T-LABEL
   s" 0 NCLOB-RECYCLED-CALLER" EV-N 76 T= ;

\ ---- a refusal from this record costs nothing ---------------------------------
\ The widen refusal has to be raised BEFORE the seam writes a byte, because the
\ default transaction must leave no definition or publication behind. Reaching
\ it needs a row already sitting at the slot the
\ seam is about to claim, and the slot is learnt the only honest way: the same
\ source is compiled once, forgotten back to the same anchor, and compiled again
\ - the compiler emits the identical text from the identical free slot, so the
\ second run claims the address the first one did. The refusal itself proves the
\ collision: without it the second compilation would simply succeed.
\
\ AND THE ROW THAT IS SEEDED IS DERIVED FROM THE REAL ONE, NOT WRITTEN DOWN. The
\ seed has to be strictly narrower than what the second compilation will record, or
\ there is no widening and the case silently stops testing anything - which is
\ exactly what happened when the seed was the literal `$1`: a compiler change
\ narrowed this routine's emission to one register, the literal became the whole
\ of the real row, and the refusal stopped firing. So the first compilation's own
\ row is read before the forget drops it, its lowest register is taken out, and
\ THAT is what is seeded. It is a strict subset whatever the allocator does, the
\ case asserts it is one, and no future allocation can make the two coincide.
variable ANCHOR-ENTRY
variable REAL-ROW
variable SEED-ROW

: ORDER-ANCHOR ( -- )
   s" : NCLOB-ANCHOR ( -- ) ;" EV ;

: REPLAY! ( -- )
   s" : NCLOB-REPLAY ( n -- n ) 1 + 2 + 3 + 4 + 5 + 6 + ;" EV ;

: ORDER-CASES ( -- )
   ORDER-ANCHOR
   REPLAY!
   s" NCLOB-REPLAY" ENTRY-OF ANCHOR-ENTRY !
   ANCHOR-ENTRY @ GPR-AT REAL-ROW !
   REAL-ROW @ WITHOUT-LOWEST SEED-ROW !

   s" NCLOB-ANCHOR" FORGET-DEFS-FROM
   ANCHOR-ENTRY @  SEED-ROW @ GPRS  $0 FPRS  NCLOB:RECORD

   s" the seeded row really is narrower than the one the compilation will write"
   T-LABEL
   SEED-ROW @ REAL-ROW @ T<>
   SEED-ROW @ REAL-ROW @ and  SEED-ROW @ T=

   s" a publication whose row would widen an existing one is refused" T-LABEL
   ORDER-ANCHOR
   [: REPLAY! ;] E-NCLOB-WIDEN TTHROWSQ

   s" and the refusal leaves no definition or publication behind" T-LABEL
   s" NCLOB-REPLAY" DEFINED? TFALSE
   s" NCLOB-REPLAY" GLOBAL-WID NPUB:REPUBLISHED? TFALSE

   s" with the seeded row exactly as the refusal found it" T-LABEL
   ANCHOR-ENTRY @ GPR-AT SEED-ROW @ T= ;

\ ---- the table holds as many routines as the program has ----------------------
\ WHAT THIS IS FOR. The rows used to live in a fixed array of 128 cells, which is
\ what the system compiled when the record was written. A whole-tree census then
\ measured the compiler against lib/ and reported that it compiles EXACTLY
\ 128 definitions - not because the 129th is a shape the dialect lacks, but
\ because the 129th publication was refused a table slot after selection,
\ allocation, verification and emission had all accepted it. A table that cannot
\ grow is a bound on how much of a program the compiler may compile, and this case
\ is what says it no longer is one.
\
\ IT IS DRIVEN THROUGH THE RECORD'S OWN ENTRIES, which is what the publication
\ seam calls: RECORD writes a row, GPR-CLOB and FPR-CLOB read one, and
\ CODE-RECLAIM:TRUNCATE is the notice that drops them. Nothing here re-implements
\ the table or reaches around it - there is no other door to reach around it
\ through.
\
\ WHY THE ADDRESSES START AT THE FREE CODE SLOT. Every row this record holds
\ describes a routine below that slot, so a row recorded above it is above every
\ live row - which is the order the publication seam produces and the order the
\ reclamation cut below rests on. They are four bytes apart because that is the
\ closest two published routines can ever be.
\
\ AND WHAT IT MEASURES IS NOT JUST THE COUNT. The rows written before the storage
\ first grew are read back AFTER it has grown six times, and the widen refusal is
\ asked of one of them: a growth that lost or moved a row would answer a stale
\ set, and answering a stale narrow set is precisely the failure the whole record
\ exists to prevent. The narrowing write goes to a copied row for the same
\ reason.
128 constant OLD-CEILING              \ the fixed array this record used to be
5000 constant GROWN-ROWS              \ past the population lib+src needs
variable GROW-BASE
variable GROW-BEFORE

: GROW-AT ( n -- n ) {: k:n :}
   GROW-BASE @ k INSN-BYTES * + ;

\ A set per row that depends on the row, so a lookup that answered a neighbour
\ would answer the neighbour's registers and the case would see it.
: GROW-GPRS ( n -- n ) {: k:n :}
   k $F and 1 + ;

: GROW-FPRS ( n -- n ) {: k:n :}
   k 3 and 1 + ;

\ The free code slot is above every routine this record holds a row for, because
\ a publication claims that slot and moves it past the routine it wrote. The fill
\ asserts it rather than assuming it: a row already sitting up there would make
\ the first row below a RE-record of somebody else's routine, and the case would
\ report a widen refusal instead of what it is about.
: GROW-FILL ( -- )
   cp@ GROW-BASE !
   NCLOB:ROWS GROW-BEFORE !

   s" no row describes a routine at or above the free code slot" T-LABEL
   GROW-BASE @ NCLOB:KNOWN? FLAG# 0 T=

   GROWN-ROWS 0 ?do
      i GROW-AT  i GROW-GPRS GPRS  i GROW-FPRS FPRS  NCLOB:RECORD
   loop ;

: GROW-ROW-EXACT ( n -- ) {: k:n :}
   k GROW-AT GPR-AT k GROW-GPRS T=
   k GROW-AT FPR-AT k GROW-FPRS T= ;

: GROW-CASES ( -- )
   s" the record holds one row per published routine, not 128" T-LABEL
   NCLOB:ROWS GROW-BEFORE @ GROWN-ROWS + T=
   GROWN-ROWS OLD-CEILING > TTRUE

   s" a row written before the storage grew still answers what it was told"
   T-LABEL
   0 GROW-ROW-EXACT
   OLD-CEILING 1 - GROW-ROW-EXACT

   s" and so does the row the fixed array had no slot for at all" T-LABEL
   OLD-CEILING GROW-ROW-EXACT
   GROWN-ROWS 1 - GROW-ROW-EXACT

   s" the widen refusal still stands on a row the growth copied" T-LABEL
   [: 0 GROW-AT  0 GROW-GPRS WITHOUT-LOWEST $10 or GPRS  0 GROW-FPRS FPRS
      NCLOB:RECORD ;] E-NCLOB-WIDEN TTHROWSQ
   0 GROW-ROW-EXACT

   s" and a narrowing of a copied row is written where the reader looks" T-LABEL
   0 GROW-AT  0 GROW-GPRS WITHOUT-LOWEST GPRS  0 GROW-FPRS FPRS NCLOB:RECORD
   0 GROW-AT GPR-AT  0 GROW-GPRS WITHOUT-LOWEST T=

   s" reclaiming the code they describe gives every one of those slots back"
   T-LABEL
   GROW-BASE @ CODE-RECLAIM:TRUNCATE
   NCLOB:ROWS GROW-BEFORE @ T=
   0 GROW-AT NCLOB:KNOWN? FLAG# 0 T=
   GROWN-ROWS 1 - GROW-AT NCLOB:KNOWN? FLAG# 0 T=

   s" and the rows below the floor are exactly as they were" T-LABEL
   A1 GPR-AT $4 T=
   A2 GPR-AT $8 T= ;

public

: RUN ( -- )
   T-RESET
   RECORD-CASES
   WIDEN-CASES
   STEP!
   PUBLISHED-CASES
   NARROW!
   NARROW-CASES
   PSTEP!
   PN!
   PC!
   PRESSURE-CASES
   BUILD-RECLAIMED
   RECLAIM-CASES
   RECLAIM-CALLER!
   RECLAIM-CALLER-CASES
   GROW-FILL
   GROW-CASES
   ORDER-CASES
   T-REPORT ;

;package

NCLOB-TEST:RUN
