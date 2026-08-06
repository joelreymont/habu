\ codegen-compare-corpus2.f - the second pinned word corpus the codegen
\ comparison measures: seven words taken from the places this system spends its
\ time. One concern: the subject words themselves.
\
\ WHY THERE IS A SECOND CORPUS AND NOT SEVEN MORE ROWS IN THE FIRST. The eleven
\ words of tools/codegen-compare-corpus.f and the table committed for them are
\ pinned: their recorded bytes are the yardstick a compiler change is read
\ against, and adding a row to that file would be a change to the artifact the
\ measurement is compared with. So this file is a corpus of its own, with a
\ committed table of its own, measured by the same engine in the same run. The
\ first corpus and its baseline do not move.
\
\ WHERE THESE SEVEN COME FROM. A survey of the words this system really runs
\ hot, each picked because it is a SHAPE the first corpus does not have:
\
\   TAG             the smallest hot word there is: one mask. Every inspection
\                   of a type term in the checker begins with it, and its
\                   sibling PAY (`3 rshift`) ends it. src/core/checker.f:152.
\   WS?             four equality tests folded together with `or` - the whole
\                   comparison-and-bitwise vocabulary in one line, and the shape
\                   a scanner's inner test really has. lib/json-read.f:252.
\   SYM-FOLD-C      two range tests, each leaving the word from the middle
\                   through `exit`, then one `or`: ASCII case folding, run once
\                   per byte of every symbol the checker compares.
\                   src/core/checker.f:3542.
\   MAX-DIM         the two-armed branch written with `else`, which the first
\                   corpus has no example of. maki/tensor.f:76.
\   COUNT-CHAR      a `begin … while … repeat` scan carrying TWO values round
\                   the loop - the counter and the cursor - over a byte span.
\                   lib/string.f:103.
\   T-RES-WALK      a loop whose TEST IS A CALL. The whole body is
\                   `begin TV-NEXT? while repeat`, so every turn of the loop
\                   crosses a call boundary and the value the loop carries is
\                   the callee's answer. src/core/checker.f:887.
\   VEC-COPY-CELLS  a counted `?do` loop with TWO CALLS IN ITS BODY, one
\                   computing the address it reads and one the address it
\                   writes. lib/vector.f:139.
\
\ The two call shapes are the point of this corpus. The first corpus's only call
\ is FACT's recursion, which is a call in straight-line code; a call in a loop
\ test and a call in a loop body are different questions, and this file asks
\ them.
\
\ WHICH BODIES ARE VERBATIM AND WHICH ARE ANALOGUES.
\
\ Five of the seven are the surveyed body, word for word: TAG, WS?, SYM-FOLD-C,
\ MAX-DIM and COUNT-CHAR. What is added to them is a declared stack effect where
\ the original omitted one (TAG is written `: TAG 7 and ;` in the checker, whose
\ core is compiled before the checker can state effects) and, for WS?, the four
\ byte constants it names, declared here exactly as lib/json-read.f declares
\ them. Nothing else is touched HERE: no literal is respelled, no name is
\ replaced by the number behind it, and no guard is dropped, so what the old
\ column measures is the program the survey found.
\
\ THE NEW COLUMN RESPELLS A CONSTANT IN TWO OF THEM, AND SAYS WHICH.
\ tools/codegen-compare-migrated2.f compiles WS? with 32, 9, 10 and 13 where
\ this file names SP, TAB, LF and CR, and SYM-FOLD-C with 65, 90 and 32 where
\ this file writes $41, $5A and $20 - because a migrated body may name at most
\ one word outside the dialect (dot habu-let-a-migrated-77d34d82) and because
\ the tape cannot read a hexadecimal spelling back (dot
\ habu-record-the-engine-79c570ed). That file lists both substitutions in full
\ with the measurement behind them: the engine compiles each body and its
\ respelled twin to byte-identical code, and tools/codegen-compare-test.f pins
\ that equality, so the two columns are two compilations of one program. What is
\ NOT done anywhere is respelling something STRUCTURAL to buy a row: a body the
\ chain cannot compile is a gap row in tools/codegen-compare-new2.f that names
\ the capability and the dot, which is a result.
\
\ Two of the seven are ANALOGUES, because their real bodies walk state that
\ belongs to a package and cannot be replicated honestly here:
\
\   T-RES-WALK      walks the checker's type-variable binding table, which the
\                   checker owns, sizes and mutates while it runs. The analogue
\                   keeps the SHAPE exactly - the same loop, the same callee,
\                   the same three ways out of that callee - over a binding
\                   table this file owns. See TV-NEXT? below for what is
\                   inlined and why.
\   VEC-COPY-CELLS  copies between two vectors' storage through VEC-CELL-FIELD,
\                   whose guard throws, and behind a VEC-CHECK-LEN that throws
\                   too. Nothing in the native chain's dialect can throw, so a
\                   body carrying those guards could never be a row of the new
\                   column at all - it would be a gap for a reason that says
\                   nothing about calls in loops, which is the shape this row
\                   exists to measure. The analogue keeps the loop, both call
\                   sites and the cell arithmetic, and drops the two guards; the
\                   file says so here rather than in a footnote.
\
\ An analogue's own callee is a word of this corpus, so the row measures the
\ whole two-word program the way the real one is a two-word program.
\
\ THE CORPUS IS PINNED, exactly as the first one is. Do not "improve" a word
\ here. Adding a word adds a row; changing an existing body invalidates the
\ recorded baseline for that word and must be done in the same change that
\ regenerates it, with the reason written down.
\
\ THE TWO WORDS WHOSE POINT IS A SIDE EFFECT are T-RES-WALK's binding table and
\ VEC-COPY-CELLS's destination, and both are this file's own storage, published
\ for reading. Both columns of the comparison walk and write the SAME memory, so
\ the head-to-head check is about the loads and the stores and not only about the
\ arithmetic between them - the discipline CELL-BUMP established in the first
\ corpus.

require lib/prelude.f

package CODEGEN-CORPUS2

\ ---- the state the two analogues own ----------------------------------------

\ T-RES-WALK's binding table, standing in for the checker's TVT. A type term is
\ one cell: the low three bits are its tag and the rest is its payload, which is
\ what TAG and PAY read. A term whose tag is T-VAR is a type variable, and this
\ table holds what each variable is bound to, indexed by its payload; UNBOUND
\ means it is bound to nothing yet. SETUP below fills it with the one chain
\ every measurement walks.
32 constant TV-SLOTS
create TV-TABLE TV-SLOTS cells allot

\ VEC-COPY-CELLS's two buffers. The source is filled once and never written
\ again; the destination is what the copy writes, and the cell past the copied
\ ones is left alone on purpose so a routine that ran the loop one turn too far
\ shows up as a changed output rather than as nothing at all.
8 constant COPY-CELLS
create COPY-SRC COPY-CELLS cells allot
create COPY-DST COPY-CELLS cells allot

public

\ ---- what the harness reads the side effects through -------------------------

\ This corpus used to publish its binding table's ADDRESS here, through the same
\ named unchecked boundary the first corpus kept for its cell, because a compiler
\ needs the number a data word pushes and the checker gives a `create`d word the
\ type `ptr a` so that a pointer cannot be done arithmetic on by accident. No
\ harness names an address any more: the chain asks the engine what the data word
\ is, so both columns walk the same table without anyone writing down where it is.
\
\ One cell of the destination buffer, read the checked way. This is what both
\ columns verify the copy through.
: COPY-DST@ ( n -- n )
   cells COPY-DST + @ ;

\ ---- the seven subjects ------------------------------------------------------

\ src/core/checker.f:152, verbatim. The checker writes `: TAG 7 and ;` with no
\ declared effect, because that part of the core is compiled before the checker
\ can state one; the effect is stated here and the body is untouched. Its
\ sibling `: PAY 3 rshift ;` stands on the next line there and is inlined into
\ TV-NEXT? below, where the walk needs it.
: TAG ( n -- n )
   7 and ;

\ lib/json-read.f:45-49, verbatim: the four byte values a JSON scanner treats as
\ whitespace, spelled the way that file spells them.
32 constant SP
9 constant TAB
10 constant LF
13 constant CR

\ lib/json-read.f:252, verbatim.
: WS? ( n -- bool )
   dup SP = over TAB = or over LF = or swap CR = or ;

\ src/core/checker.f:3542, verbatim, hexadecimal literals and all. $41 is `A`,
\ $5A is `Z` and $20 is the bit that separates an upper-case letter from its
\ lower-case one, so the word answers its argument unchanged outside that range
\ and the folded byte inside it.
: SYM-FOLD-C ( n -- n ) {: c:n :}
   c $41 < if c exit then
   c $5A > if c exit then
   c $20 or ;

\ maki/tensor.f:76, verbatim.
: MAX-DIM ( n n -- n ) {: a:n b:n :}
   a b > if a else b then ;

\ lib/string.f:103, verbatim, including its two unannotated locals - the corpus
\ is pinned to the surveyed body and an annotation would be an edit to it.
\ typed-local-lint: allow-bare-local - u and c are lib/string.f:103's own
\ spelling; this corpus records the body that runs, not a rewrite of it.
: COUNT-CHAR ( ptr u8 n n -- n ) {: a:ptr u c :}
   0 0 begin dup u < while
      dup a + c@ c = if swap 1+ swap then
      1+
   repeat drop ;

\ T-RES-WALK's callee, as an analogue of src/core/checker.f:872. The real one
\ reads
\
\   : TV-NEXT? ( n -- n bool )
\      dup ISVAR 0= IF RES-FALSE EXIT THEN
\      dup PAY TV@ dup UNBOUND = IF drop RES-FALSE ELSE nip RES-TRUE THEN ;
\
\ and every name in it is spelled out below: ISVAR is `TAG T-VAR =`, TAG is
\ `7 and` and T-VAR is 1; PAY is `3 rshift`; TV@ is `cells TVT + @` against the
\ checker's table, and against this file's table here; UNBOUND is -1; and
\ RES-TRUE and RES-FALSE are `0 0=` and `0 0= 0=`, which is how
\ src/core/checker.f:799 writes them. The three ways out are the three the real
\ word has: not a variable at all, a variable bound to nothing, and a variable
\ whose binding is the next term of the chain.
\
\ WHY THE NAMES ARE SPELLED OUT AND NOT CALLED. A body the chain compiles may
\ name one word outside the dialect's own vocabulary - one `create`d data word
\ or one callee, not both and not two of either (src/compiler/native/migrate.f).
\ This body's one name is its table, so TAG, PAY and the two flag words are
\ written as what they are. Dot habu-let-a-migrated-77d34d82 is the capability
\ that lets a migrated body name as many as it mentions; when it lands this body
\ calls TAG above instead of masking inline.
: TV-NEXT? ( n -- n bool )
   dup 7 and 1 = 0= if 0 0= 0= exit then
   dup 3 rshift cells TV-TABLE + @
   dup -1 = if drop 0 0= 0= else nip 0 0= then ;

\ src/core/checker.f:887, verbatim over the callee above. The loop test is the
\ call and the loop body is empty, which is the whole shape.
: T-RES-WALK ( n -- n )
   begin TV-NEXT? while repeat ;

\ VEC-COPY-CELLS's callee, as an analogue of lib/vector.f:55. The real one reads
\
\   : VEC-CELL-FIELD ( ptr a n -- ptr a ) {: base:ptr off :}
\      off 0 < if E-VEC-BOUNDS throw then
\      base off cells + ;
\
\ and this is that arithmetic without the guard, because nothing in the native
\ chain's dialect can throw. What the row measures is the call in the loop body,
\ and a guard that can only leave through an error would make the row a gap for
\ a reason that is not about calls in loops.
: CELL-FIELD ( ptr n n -- ptr n )
   cells + ;

\ lib/vector.f:139, over the callee above, with the throwing VEC-CHECK-LEN and
\ the newtype narrowing LEN>N left out for the same reason. The loop, both call
\ sites and the cell arithmetic are the real word's.
: VEC-COPY-CELLS ( ptr n ptr n n -- ) {: src:ptr dst:ptr len:n :}
   len 0 ?do
      src i CELL-FIELD @ dst i CELL-FIELD !
   loop ;

\ ---- the fixture state, filled once ------------------------------------------

private

\ A type term from its tag and its payload: the encoding TAG and PAY read.
: TERM ( n n -- n ) {: pay:n tag:n :}
   pay 3 lshift tag or ;

: TV! ( n n -- ) {: pay:n term:n :}
   term pay cells TV-TABLE + ! ;

\ Every variable is bound to nothing, and then one chain is laid down: the
\ variable with payload 1 is bound to the variable with payload 2, which is
\ bound to the constructed term with payload 3. So a walk from the first
\ variable takes two steps and stops on a term that is not a variable, a walk
\ from that term takes none, and a walk from the variable with payload 4 takes
\ none because nothing is bound to it. Those are the three exits TV-NEXT? has.
: BIND-CHAIN ( -- )
   TV-SLOTS 0 ?do -1 i cells TV-TABLE + ! loop
   1  2 1 TERM  TV!
   2  3 0 TERM  TV! ;

: FILL-COPY ( -- )
   COPY-CELLS 0 ?do
      i 100 * 7 +  i cells COPY-SRC + !
      0  i cells COPY-DST + !
   loop ;

public

\ The terms the walk is measured on. A variable at the head of the two-link
\ chain, a term that is not a variable at all, and a variable bound to nothing.
: CHAIN-HEAD ( -- n ) 1 1 TERM ;
: NOT-A-VAR ( -- n ) 3 0 TERM ;
: UNBOUND-VAR ( -- n ) 4 1 TERM ;

\ Where a measured copy reads from and writes to, and how many cells it moves.
\ The buffers hold eight cells and a measured copy moves four, so the cells past
\ the copy are the evidence that the loop stopped where it was told to.
4 constant COPY-LEN
: COPY-FROM ( -- ptr n ) COPY-SRC ;
: COPY-TO ( -- ptr n ) COPY-DST ;

: SETUP ( -- )
   BIND-CHAIN
   FILL-COPY ;

SETUP

;package
