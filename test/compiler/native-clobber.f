\ native-clobber.f - what a published routine destroys, and what a call site does
\ with the answer. One concern: src/compiler/native/clobber.f and the narrowing
\ every stage of the chain hangs off it.
\
\ WHAT THIS SUITE HAS TO SHOW. Seven things, and the last four are the ones the
\ record exists for.
\
\   1. That the record answers about an ADDRESS, keeps what it was told, and
\      hands an address it has no row for back to the caller's own worst case.
\   2. That it will NARROW a row and will not WIDEN one. Every call site
\      compiled against a row skipped saving exactly the registers outside it, so
\      a row that grew would make code already emitted wrong; the refusal is
\      E-NCLOB-WIDEN and it is the whole reason a narrow contract is sound.
\   3. That a real migration writes a row for the address it published at, and
\      that the row is what the emission's own allocation said - not the whole
\      pool, which is what the answer would be if the derivation had quietly
\      given up.
\   4. That the narrowing is REAL and that its absence is real too. Two callers
\      of the same shape are migrated: one calls a word the chain published and
\      the other calls a word the ENGINE compiled, which has no row and never
\      will. Their emitted code is counted instruction by instruction - how many
\      stores and loads against the engine's data-stack pointer each contains -
\      and the first has fewer. That is the measurement the whole change is for,
\      and the second is the proof that a callee nobody knows anything about
\      still gets the full caller-save discipline.
\   5. That the narrowing is measured UNDER PRESSURE. A caller with fewer live
\      values than the row leaves free registers spills nothing whatever the row
\      says, so a suite that only measures such a caller passes with a register
\      deleted from the record. The pressure pair below holds more values live
\      across the call than the row leaves room for, so every register in the
\      row moves one store and one load.
\   6. That a ROW DIES WITH ITS CODE. A FORGET hands the bytes above the code
\      pointer back to the engine, and the next definition is compiled over
\      them; a row left behind would tell a later caller that the routine it is
\      about to branch to destroys the registers of a routine that no longer
\      exists. So a migrated word is forgotten through the engine's own
\      FORGET-DEFS-FROM, the freed slot is taken by an ENGINE-compiled word of
\      the same shape, and a caller of THAT word is migrated: it must find no
\      row, keep the whole discipline, and compute the right answer. It computed
\      the wrong one before rows were dropped. The row's slot has to come back
\      too, or a forget-and-re-migrate cycle would burn the table.
\   7. That a refusal from this record reaches the publication seam BEFORE the
\      seam writes a byte. The widen refusal used to be raised after the routine
\      was in the arena and its dictionary record retargeted, which left a live
\      word described by a row belonging to something else - and the next caller
\      compiled against that row computed the wrong answer. The case below seeds
\      a narrow row at the exact slot a replayed migration will claim and
\      requires the refusal to leave the word's record pointing at the code the
\      engine compiled.
\
\ WHY THE COUNT IS OF INSTRUCTIONS AND NOT OF BYTES. A byte count moves for any
\ reason at all. What the narrowing removes is exactly the traffic between a
\ caller's registers and its own data-stack slots, so what is counted is that:
\ the Str and Ldr forms whose base register is the one the running engine keeps
\ its data-stack pointer in. A change that made the code smaller some other way
\ would not move these numbers, and a change that stopped narrowing would.
\
\ AND WHY THE CALLEE IS DELIBERATELY NOT A TINY ONE. A callee small enough that
\ copying its body into a caller costs no more than the call did is not called at
\ all - src/compiler/native/inline.f records such a body and the elaborator
\ splices it - and a caller with no call in it saves nothing, so it would measure
\ this record's narrowing at zero against zero. The callee below is therefore
\ sized past that rule on purpose: what this suite is about is what a CALL that
\ really happens costs, and the copying is measured where it belongs, in
\ test/compiler/native-inline.f.

require lib/test.f
require src/compiler/native/migrate.f
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

8 constant REGS                      \ scratch registers the migrated routines may use
0 constant GLOBAL-WID
4 constant INSN-BYTES

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

\ ---- what a real publication records -----------------------------------------
\ The callee both loss cases below call. It is migrated, so the chain compiled it
\ and the seam recorded what it destroys. Its body is six additions rather than
\ one because a one-addition body is one the chain COPIES into its caller instead
\ of calling: the head of this file says why that would leave nothing to measure.
: MIGRATE-CALLEE ( -- )
   s" : NCLOB-STEP ( n -- n ) 1 + 2 + 3 + 4 + 5 + 6 + ;" 1 1 REGS NMIGRATE:DEFINE ;

\ The same body, compiled by the ENGINE and never migrated. Nothing knows what it
\ destroys and nothing ever will, so a call site that reaches it saves
\ everything - and that is the whole difference between the two callers below.
\ It is defined through the interpret path so that it lands beside the migrated
\ words, in the wordlist a plain name resolves in. It is the same body as the
\ migrated one, so the two callers below differ in nothing but which callee they
\ reach.
: DEFINE-ENGINE-CALLEE ( -- )
   s" : NCLOB-ENGINE-STEP ( n -- n ) 1 + 2 + 3 + 4 + 5 + 6 + ;" EV ;

: PUBLISHED-CASES ( -- )
   s" a migration records a row for the address it published at" T-LABEL
   s" NCLOB-STEP" ENTRY-OF NCLOB:KNOWN? FLAG# 1 T=

   s" and the row is narrower than the whole register file" T-LABEL
   s" NCLOB-STEP" ENTRY-OF GPR-AT  A64EFF:GPR-ALL A64EFF:GPRS-N T<>

   s" and it names no register outside the pool the routine was given" T-LABEL
   s" NCLOB-STEP" ENTRY-OF GPR-AT
   1 REGS lshift 1 -  invert and  0 T=

   s" a word the engine compiled has no row and never will" T-LABEL
   s" NCLOB-ENGINE-STEP" ENTRY-OF NCLOB:KNOWN? FLAG# 0 T= ;

\ ---- the narrowing, measured -------------------------------------------------
\ Two callers of one shape. Two values are live across every call in both - the
\ loop's index and its limit; the accumulator is the call's own argument and goes
\ out through a slot either way - so the difference between their counts is the
\ discipline and nothing else, and both counts are measured so that a change in
\ either is visible rather than only their order.
\
\ WHY THE COUNTS ARE SMALL RATHER THAN EIGHT AND EIGHT. The residency pass in
\ src/compiler/native/select.f writes no store for a value the cell it would
\ write already holds, and builds no load for a value nothing reads out of a
\ register. The accumulator is handed in on the caller's stack, handed straight
\ to the callee and handed straight back, so it crosses either body without ever
\ reaching a register - which removes the accumulator's own traffic from both
\ rows. What is left in the wide row is the loop's index and limit going out and
\ coming back at every call, which is the discipline the narrowing removes, and
\ the assertions below hold the two rows against each other as well as against
\ their own numbers.
2 constant CALL-IN
1 constant CALL-OUT

: MIGRATE-NARROW ( -- )
   s" NCLOB-STEP" s" NCLOB-STEP" ENTRY-OF 1 1 NMIGRATE:CALLEE
   s" : NCLOB-NARROW ( n n -- n ) {: seed:n len:n :} seed len 0 ?do NCLOB-STEP NCLOB-STEP loop ;"
   CALL-IN CALL-OUT REGS NMIGRATE:DEFINE-CALLING ;

: MIGRATE-WIDE ( -- )
   s" NCLOB-ENGINE-STEP"  s" NCLOB-ENGINE-STEP" ENTRY-OF  1 1 NMIGRATE:CALLEE
   s" : NCLOB-WIDE ( n n -- n ) {: seed:n len:n :} seed len 0 ?do NCLOB-ENGINE-STEP NCLOB-ENGINE-STEP loop ;"
   CALL-IN CALL-OUT REGS NMIGRATE:DEFINE-CALLING ;

: NARROW-CASES ( -- )
   s" both callers answer what their body says, on the same inputs" T-LABEL
   s" 0 4 NCLOB-NARROW" EV-N 168 T=
   s" 0 4 NCLOB-WIDE" EV-N 168 T=
   s" 5 0 NCLOB-NARROW" EV-N 5 T=
   s" 5 0 NCLOB-WIDE" EV-N 5 T=

   s" the caller of a word the chain published saves less than the other" T-LABEL
   s" NCLOB-NARROW" DS-STORES  s" NCLOB-WIDE" DS-STORES  < TTRUE
   s" NCLOB-NARROW" DS-LOADS  s" NCLOB-WIDE" DS-LOADS  < TTRUE

   s" and what it still stores is its arguments, not its live values" T-LABEL
   s" NCLOB-NARROW" DS-STORES 0 T=
   s" NCLOB-NARROW" DS-LOADS 1 T=

   s" while the caller of an engine-compiled word keeps the whole discipline" T-LABEL
   s" NCLOB-WIDE" DS-STORES 3 T=
   s" NCLOB-WIDE" DS-LOADS 5 T=

   s" and the narrowing is the whole of the gap between the two rows" T-LABEL
   s" NCLOB-WIDE" DS-STORES  s" NCLOB-NARROW" DS-STORES -  3 T=
   s" NCLOB-WIDE" DS-LOADS  s" NCLOB-NARROW" DS-LOADS -  4 T= ;

\ ---- the narrowing, measured where it can actually be lost -------------------
\ The pair above holds two values live across each call while the callee's row
\ leaves six registers free, so the row has slack: delete a register from it and
\ both callers still spill nothing extra and every count stays where it was.
\ This pair holds EIGHT values live across one call - seven named sums and the
\ accumulator - which is more than the row leaves room for, so the caller spills
\ the excess and the number it spills is (live - room). Every register the row
\ names therefore moves exactly one store and one load, in both directions: a
\ record that named one register fewer would take these counts to two, and the
\ engine-compiled twin, whose room is nothing at all, spills all eight.
: MIGRATE-PRESSURE-NARROW ( -- )
   s" NCLOB-STEP" s" NCLOB-STEP" ENTRY-OF 1 1 NMIGRATE:CALLEE
   s" : NCLOB-PN ( n -- n ) {: s:n :} s 1 + s 2 + s 3 + s 4 + s 5 + s 6 + s 7 + s NCLOB-STEP + + + + + + + ;"
   1 1 REGS NMIGRATE:DEFINE-CALLING ;

: MIGRATE-PRESSURE-WIDE ( -- )
   s" NCLOB-ENGINE-STEP" s" NCLOB-ENGINE-STEP" ENTRY-OF 1 1 NMIGRATE:CALLEE
   s" : NCLOB-PW ( n -- n ) {: s:n :} s 1 + s 2 + s 3 + s 4 + s 5 + s 6 + s 7 + s NCLOB-ENGINE-STEP + + + + + + + ;"
   1 1 REGS NMIGRATE:DEFINE-CALLING ;

: PRESSURE-CASES ( -- )
   s" both pressure callers answer what their body says" T-LABEL
   s" 0 NCLOB-PN" EV-N 49 T=
   s" 0 NCLOB-PW" EV-N 49 T=

   s" a caller with more live values than the row leaves room for spills the excess" T-LABEL
   s" NCLOB-PN" DS-STORES 3 T=
   s" NCLOB-PN" DS-LOADS 3 T=

   s" while its engine-callee twin, with no room at all, spills every one" T-LABEL
   s" NCLOB-PW" DS-STORES 9 T=
   s" NCLOB-PW" DS-LOADS 9 T= ;

\ ---- a row dies with the code it describes ------------------------------------
\ The engine compiles every definition into one bump pointer and FORGET-DEFS-FROM
\ moves that pointer BACK to the start of the record it forgets. A migrated
\ word's record starts at the address the publication seam wrote its routine at,
\ so forgetting the migrated word puts the free code slot exactly there and the
\ next definition the engine compiles is written over that routine. Everything
\ below therefore drives the engine's own FORGET, and the collision is ASSERTED
\ rather than assumed - if the engine ever stopped reusing the slot the case
\ would go red instead of quietly measuring nothing.
variable ROWS-BEFORE
variable GONE-ENTRY

: BUILD-RECLAIMED ( -- )
   NCLOB:ROWS ROWS-BEFORE !
   s" : NCLOB-GONE ( n -- n ) 1 + 2 + 3 + 4 + 5 + 6 + ;" 1 1 REGS NMIGRATE:DEFINE
   s" NCLOB-GONE" ENTRY-OF GONE-ENTRY ! ;

\ The word that takes the freed slot is compiled by the ENGINE, so nothing knows
\ what it destroys and a caller of it must save everything it holds. That is the
\ shape the stale row broke: the caller used to be told the forgotten routine's
\ two registers and skipped saving the two the engine's emitter really writes.
: RECLAIM-CASES ( -- )
   s" a migration is recorded at the address it published at" T-LABEL
   GONE-ENTRY @ NCLOB:KNOWN? FLAG# 1 T=
   NCLOB:ROWS ROWS-BEFORE @ 1+ T=

   s" forgetting it puts the free code slot back at that address" T-LABEL
   s" NCLOB-GONE" FORGET-DEFS-FROM
   cp@ GONE-ENTRY @ T=

   s" and the row went with the code, giving its table slot back" T-LABEL
   GONE-ENTRY @ NCLOB:KNOWN? FLAG# 0 T=
   NCLOB:ROWS ROWS-BEFORE @ T=

   s" the next definition the engine compiles takes that exact slot" T-LABEL
   s" : NCLOB-RECYCLED ( n -- n ) 1 + 2 + 3 + 4 + 5 + 6 + ;" EV
   s" NCLOB-RECYCLED" ENTRY-OF GONE-ENTRY @ T=

   s" and nothing claims to know what the word now living there destroys" T-LABEL
   s" NCLOB-RECYCLED" ENTRY-OF NCLOB:KNOWN? FLAG# 0 T=
   s" NCLOB-RECYCLED" ENTRY-OF GPR-AT  A64EFF:GPR-ALL A64EFF:GPRS-N T=

   s" and a floor above the free slot reclaims nothing and is refused" T-LABEL
   [: cp@ INSN-BYTES + CODE-RECLAIM:TRUNCATE ;] CODE-RECLAIM:E-FLOOR TTHROWSQ

   s" three files asked to be told, which is why any of this happened" T-LABEL
   CODE-RECLAIM:WATCHERS 3 T= ;

\ A caller of the word at the recycled slot, migrated with ten values live across
\ the call so that skipping the save of even one of them shows up in the answer:
\ 55 from the ten sums plus 21 from the callee is 76, and the stale row made this
\ caller answer 86. Its twin is the identical body against the engine-compiled
\ callee that has never had a row, so the two counts are the same measurement of
\ the same discipline and the comparison needs no number written down here.
14 constant RECLAIM-REGS

: MIGRATE-RECLAIM-CALLER ( -- )
   s" NCLOB-RECYCLED" s" NCLOB-RECYCLED" ENTRY-OF 1 1 NMIGRATE:CALLEE
   s" : NCLOB-RECYCLED-CALLER ( n -- n ) {: s:n :} s 1 + s 2 + s 3 + s 4 + s 5 + s 6 + s 7 + s 8 + s 9 + s 10 + s NCLOB-RECYCLED + + + + + + + + + + ;"
   1 1 RECLAIM-REGS NMIGRATE:DEFINE-CALLING ;

: MIGRATE-RECLAIM-TWIN ( -- )
   s" NCLOB-ENGINE-STEP" s" NCLOB-ENGINE-STEP" ENTRY-OF 1 1 NMIGRATE:CALLEE
   s" : NCLOB-RECYCLED-TWIN ( n -- n ) {: s:n :} s 1 + s 2 + s 3 + s 4 + s 5 + s 6 + s 7 + s 8 + s 9 + s 10 + s NCLOB-ENGINE-STEP + + + + + + + + + + ;"
   1 1 RECLAIM-REGS NMIGRATE:DEFINE-CALLING ;

: RECLAIM-CALLER-CASES ( -- )
   s" a caller of the word at a reclaimed slot computes what its body says" T-LABEL
   s" 0 NCLOB-RECYCLED-CALLER" EV-N 76 T=
   s" 0 NCLOB-RECYCLED-TWIN" EV-N 76 T=

   s" and it keeps exactly the discipline a never-recorded callee earns" T-LABEL
   s" NCLOB-RECYCLED-CALLER" DS-STORES  s" NCLOB-RECYCLED-TWIN" DS-STORES T=
   s" NCLOB-RECYCLED-CALLER" DS-LOADS  s" NCLOB-RECYCLED-TWIN" DS-LOADS T=

   s" which is more than a caller of a word that still has its row keeps" T-LABEL
   s" NCLOB-PN" DS-STORES  s" NCLOB-RECYCLED-CALLER" DS-STORES  < TTRUE ;

\ ---- a refusal from this record costs nothing ---------------------------------
\ The widen refusal has to be raised BEFORE the seam writes a byte, because the
\ seam's own contract is that a refused publication leaves the word running the
\ code it was running. Reaching it needs a row already sitting at the slot the
\ seam is about to claim, and the slot is learnt the only honest way: the same
\ source is migrated once, forgotten back to the same anchor, and migrated again
\ - the engine compiles the identical text from the identical free slot, so the
\ second run claims the address the first one did. The refusal itself proves the
\ collision: without it the second migration would simply succeed.
variable ANCHOR-ENTRY

: ORDER-ANCHOR ( -- )
   s" : NCLOB-ANCHOR ( -- ) ;" EV ;

: ORDER-MIGRATE ( -- )
   s" : NCLOB-REPLAY ( n -- n ) 1 + 2 + 3 + 4 + 5 + 6 + ;" 1 1 REGS NMIGRATE:DEFINE ;

: ORDER-CASES ( -- )
   ORDER-ANCHOR
   ORDER-MIGRATE
   s" NCLOB-REPLAY" ENTRY-OF ANCHOR-ENTRY !

   s" NCLOB-ANCHOR" FORGET-DEFS-FROM
   ANCHOR-ENTRY @  $1 GPRS  $0 FPRS  NCLOB:RECORD

   s" a publication whose row would widen an existing one is refused" T-LABEL
   ORDER-ANCHOR
   [: ORDER-MIGRATE ;] E-NCLOB-WIDEN TTHROWSQ

   s" and the refusal leaves the word running the code the engine compiled" T-LABEL
   s" NCLOB-REPLAY" ENTRY-OF ANCHOR-ENTRY @ T<>
   s" NCLOB-REPLAY" ENTRY-OF NCLOB:KNOWN? FLAG# 0 T=
   s" 0 NCLOB-REPLAY" EV-N 21 T=

   s" with the seeded row exactly as the refusal found it" T-LABEL
   ANCHOR-ENTRY @ GPR-AT $1 T= ;

public

: RUN ( -- )
   T-RESET
   RECORD-CASES
   WIDEN-CASES
   DEFINE-ENGINE-CALLEE
   MIGRATE-CALLEE
   PUBLISHED-CASES
   MIGRATE-NARROW
   MIGRATE-WIDE
   NARROW-CASES
   MIGRATE-PRESSURE-NARROW
   MIGRATE-PRESSURE-WIDE
   PRESSURE-CASES
   BUILD-RECLAIMED
   RECLAIM-CASES
   MIGRATE-RECLAIM-CALLER
   MIGRATE-RECLAIM-TWIN
   RECLAIM-CALLER-CASES
   ORDER-CASES
   T-REPORT ;

;package

NCLOB-TEST:RUN
