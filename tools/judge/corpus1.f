\ judge/corpus1.f - the judged rows of tools/codegen-compare-corpus.f: ten
\ shapes, each the smallest honest example of something a code generator has to
\ handle. One concern: which subject is judged, on which pinned input, against
\ which C twin.
\
\ WHAT THIS FILE STATES is what tools/judge/corpus4.f states and no more: the
\ subjects, one pinned input each, and the C symbol that is each one's twin. The
\ programs come from the corpus file's own bytes and the measuring is
\ tools/judge/pass.f's.
\
\ EVERY INPUT HERE IS tools/codegen-compare-cases.f'S TIMED INPUT, unchanged, so
\ the two harnesses time the same program on the same numbers while they run
\ side by side.
\
\ THE ELEVENTH SUBJECT IS NOT A ROW. NOOP is the empty call the old harness
\ calibrates against, and the judge measures its own floor over an empty
\ quotation instead, so an empty word would be a row about nothing.
\
\ THE BYTE SPAN IS THIS FILE'S, AND THE TWIN'S IS THE TWIN'S. Two subjects scan
\ a string, and a string is a pointer: the twin is a different program and
\ carries its own copy of those bytes (tools/clang/twins.c, c1_subject). So the
\ habu columns are handed the span published here and the reference column is
\ handed the twin's, and the LENGTH both are measured over is this one's own -
\ read off it rather than written down a second time, which is the one part of a
\ span two copies could disagree about without either being obviously wrong.
\
\ CELL-BUMP ANSWERS WHAT ITS CELL HOLDS, which a routine that never touched
\ memory could answer just as well. So the row reads the cell back after the
\ call, as a second number beside the answer: the value says the arithmetic
\ happened and the witness says the store did.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require tools/codegen-compare-cabi.f
require tools/codegen-compare-corpus.f
require tools/judge/pass.f

package JUDGE-CORPUS1

private

variable SUBJ-CELL

public

\ The byte span BYTE-SUM and BYTE-FIND are measured over, in the habu columns.
: SUBJECT$ ( -- ptr u8 n )
   s" habu codegen baseline" ;

\ The same span in the reference world: the twin's own copy of those bytes, and
\ the length of the span above, so the two columns scan the same number of them.
\ The pointer is asked for once and kept, because a generated TIMING body runs
\ its inputs a quarter of a million times and a foreign call inside that loop
\ would be timed as part of the row.
: C-SUBJ ( -- n n )
   SUBJ-CELL @ 0= if
      s" hc1_subject_ptr" CODEGEN-CABI:FN CODEGEN-CABI:I0 SUBJ-CELL !
   then
   SUBJ-CELL @
   SUBJECT$ nip ;

\ What CELL-BUMP left in memory, in each world.
: BUMP-READ ( -- n )
   CODEGEN-CORPUS:BUMP-CELL@ ;

: C-BUMP-READ ( -- n )
   s" hc1_bump_get" CODEGEN-CABI:FN CODEGEN-CABI:I0 ;

private

: SOURCE$ ( -- ptr u8 n )
   s" tools/codegen-compare-corpus.f" ;

\ A suffix of this corpus's own, because the judge measures the corpora one
\ after another into one dictionary and two of them may spell a subject the same
\ way.
: SUFFIX$ ( -- ptr u8 n )
   s" -J1" ;

\ The package this corpus publishes its subjects in, and therefore the package
\ the derived words are published in too: CELL-BUMP names a cell PRIVATE to it,
\ and a word compiled anywhere else could not see that cell.
: QUALIFIER$ ( -- ptr u8 n )
   s" CODEGEN-CORPUS:" ;

\ ---- the span, and the cell ---------------------------------------------------

: SPAN+ ( -- )
   s" JUDGE-CORPUS1:SUBJECT$ " s" JUDGE-CORPUS1:C-SUBJ " JUDGE-PASS:STORE+ ;

: BUMP-WITNESS+ ( -- )
   s" JUDGE-CORPUS1:BUMP-READ" s" JUDGE-CORPUS1:C-BUMP-READ" JUDGE-PASS:READ+ ;

\ ---- the ten rows, written once ----------------------------------------------
\ typed-local-lint: allow-bare-local - row is the caller's own body, and a local
\ annotation cannot carry a quotation effect.
: EACH ( [ -- ] -- ) {: row :}
   s" ADD3" s" hc1_add3" JUDGE-PASS:ROW!
      s" 1 2 3" JUDGE-PASS:IN+  row execute
   s" SQUARE-SUM" s" hc1_square_sum" JUDGE-PASS:ROW!
      s" 3 4" JUDGE-PASS:IN+  row execute
   s" MAX2" s" hc1_max2" JUDGE-PASS:ROW!
      s" 3 4" JUDGE-PASS:IN+  row execute
   s" LERP" s" hc1_lerp" JUDGE-PASS:ROW!
      s" 10 20 50" JUDGE-PASS:IN+  row execute
   s" SUM-TO" s" hc1_sum_to" JUDGE-PASS:ROW!
      s" 16" JUDGE-PASS:IN+  row execute
   s" COUNT-DOWN" s" hc1_count_down" JUDGE-PASS:ROW!
      s" 16" JUDGE-PASS:IN+  row execute
   s" FACT" s" hc1_fact" JUDGE-PASS:ROW!
      s" 10" JUDGE-PASS:IN+  row execute
   s" CELL-BUMP" s" hc1_cell_bump" JUDGE-PASS:ROW!
      s" 7" JUDGE-PASS:IN+  BUMP-WITNESS+  row execute
   s" BYTE-SUM" s" hc1_byte_sum" JUDGE-PASS:ROW!
      SPAN+  row execute
   s" BYTE-FIND" s" hc1_byte_find" JUDGE-PASS:ROW!
      SPAN+  s" $67" JUDGE-PASS:IN+  row execute ;   \ $67 is `g`, in the span

: OPEN-CORPUS ( -- )
   SOURCE$ SUFFIX$ QUALIFIER$ JUDGE-PASS:CORPUS! ;

public

\ Read the corpus source and compile every subject through the chain. Runs at
\ load, from inside the corpus's package.
: PUBLISH-ALL ( -- )
   OPEN-CORPUS
   [: JUDGE-PASS:PUBLISH ;] EACH ;

\ Judge every subject of this corpus: the bytes, then the answers, then the
\ times.
: JUDGE ( -- )
   OPEN-CORPUS
   [: JUDGE-PASS:BYTES ;] EACH
   [: JUDGE-PASS:VALUE ;] EACH
   JUDGE-PASS:TIME-PASSES 0 ?do [: JUDGE-PASS:TIME ;] EACH loop
   JUDGE-PASS:FLOOR ;

;package

\ The corpus's package, open around the publication, so a derived body that names
\ the corpus's private cell can see it. It is written out here because `package`
\ parses its operand.
package CODEGEN-CORPUS
public

JUDGE-CORPUS1:PUBLISH-ALL

;package
