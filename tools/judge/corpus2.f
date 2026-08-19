\ judge/corpus2.f - the judged rows of tools/codegen-compare-corpus2.f: seven
\ shapes taken from the places this system spends its time. One concern: which
\ subject is judged, on which pinned input, against which C twin.
\
\ WHAT THIS FILE STATES is what tools/judge/corpus4.f states and no more, and
\ the measuring is tools/judge/pass.f's. Every input here is
\ tools/codegen-compare-cases2.f's timed input, unchanged.
\
\ ONE ROW IS REFUSED, AND IT IS THE FINDING THIS CORPUS CARRIES. The chain
\ cannot record a hexadecimal literal on its tape (dot
\ habu-record-the-engine-79c570ed, measured here as E-NFEED-LITERAL), and
\ SYM-FOLD-C is src/core/checker.f's own body, `$41`, `$5A` and `$20` and all.
\ So the judge hands the chain the corpus's text and the chain declines it. The
\ old harness's column reads GREEN on that row because it does not compile that
\ text: tools/codegen-compare-migrated2.f:100 writes 65, 90 and 32 instead, and
\ says so at its own line 28. Both statements are true of different programs -
\ the green one is about a body respelled in decimal, the REFUSED one is about
\ the body the corpus contains - and it is the corpus's own text that the engine
\ compiled and that this tool exists to judge. The corpus is not respelled to
\ buy the row back.
\
\ THE BYTE SPAN IS THIS FILE'S, AND THE TWIN'S IS THE TWIN'S, for the reason
\ tools/judge/corpus1.f gives: a span is a pointer, the twin is a different
\ program, and the length both are measured over is read off the span published
\ here rather than written down a second time.
\
\ VEC-COPY-CELLS LEAVES NOTHING AT ALL. What it does is write, so the row reads
\ the destination back after the call - the four cells the copy moved and the
\ fifth, which it must not have - as the number the columns are compared on. A
\ row whose subject leaves no value and whose memory nobody reads would be two
\ columns agreeing that zero equals zero.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require tools/codegen-compare-cabi.f
require tools/codegen-compare-corpus2.f
require tools/judge/pass.f

package JUDGE-CORPUS2

private

variable SUBJ-CELL
variable SRC-CELL
variable DST-CELL

public

\ The byte span COUNT-CHAR is measured over, in the habu columns: an `a` at the
\ first byte, an `a` in the middle and an `a` at the last one.
: SUBJECT$ ( -- ptr u8 n )
   s" aha aha aha" ;

\ The EMPTY span, which is the arm the pinned one never reaches.
: EMPTY-SPAN ( -- ptr u8 n )
   s" " ;

\ The same span in the reference world, and the length of the one above.
: C-SUBJ ( -- n n )
   SUBJ-CELL @ 0= if
      s" hc2_subject_ptr" CODEGEN-CABI:FN CODEGEN-CABI:I0 SUBJ-CELL !
   then
   SUBJ-CELL @
   SUBJECT$ nip ;

\ The twin's own pointer with no bytes after it, so both worlds scan none.
: C-EMPTY-SPAN ( -- n n )
   C-SUBJ drop 0 ;

\ The twins' own two buffers, asked for once each: a generated TIMING body runs
\ its inputs a quarter of a million times, and a foreign call inside that loop
\ would be timed as part of the row.
: C-COPY-SRC ( -- n )
   SRC-CELL @ 0<> if SRC-CELL @ exit then
   s" hc2_copy_src_ptr" CODEGEN-CABI:FN CODEGEN-CABI:I0 dup SRC-CELL ! ;

: C-COPY-DST ( -- n )
   DST-CELL @ 0<> if DST-CELL @ exit then
   s" hc2_copy_dst_ptr" CODEGEN-CABI:FN CODEGEN-CABI:I0 dup DST-CELL ! ;

private

: C-DST@ ( n -- n )
   s" hc2_copy_dst_get" CODEGEN-CABI:FN CODEGEN-CABI:I1 ;

public

\ What the copy left in memory: the four cells it moved and the fifth it must
\ not have. A reader is asked for outside every timing loop - a VALUE body runs
\ once - so the symbol lookups here cost nothing measured.
: DST-READ ( -- n )
   0 CODEGEN-CORPUS2:COPY-DST@
   1 CODEGEN-CORPUS2:COPY-DST@ xor
   2 CODEGEN-CORPUS2:COPY-DST@ xor
   3 CODEGEN-CORPUS2:COPY-DST@ xor
   4 CODEGEN-CORPUS2:COPY-DST@ xor ;

: C-DST-READ ( -- n )
   0 C-DST@
   1 C-DST@ xor
   2 C-DST@ xor
   3 C-DST@ xor
   4 C-DST@ xor ;

private

: SOURCE$ ( -- ptr u8 n )
   s" tools/codegen-compare-corpus2.f" ;

: SUFFIX$ ( -- ptr u8 n )
   s" -J2" ;

\ The package this corpus publishes its subjects in: TV-NEXT?, which T-RES-WALK
\ calls, names the binding table PRIVATE to it.
: QUALIFIER$ ( -- ptr u8 n )
   s" CODEGEN-CORPUS2:" ;

\ ---- the span, the buffers, and the reader ------------------------------------

: SPAN+ ( -- )
   s" JUDGE-CORPUS2:SUBJECT$ " s" JUDGE-CORPUS2:C-SUBJ " JUDGE-PASS:STORE+ ;

: EMPTY-SPAN+ ( -- )
   s" JUDGE-CORPUS2:EMPTY-SPAN " s" JUDGE-CORPUS2:C-EMPTY-SPAN "
   JUDGE-PASS:STORE+ ;

: BUFFERS+ ( -- )
   s" CODEGEN-CORPUS2:COPY-FROM CODEGEN-CORPUS2:COPY-TO "
   s" JUDGE-CORPUS2:C-COPY-SRC JUDGE-CORPUS2:C-COPY-DST " JUDGE-PASS:STORE+
   s" JUDGE-CORPUS2:DST-READ" s" JUDGE-CORPUS2:C-DST-READ" JUDGE-PASS:READ+ ;

\ ---- the seven rows, written once ---------------------------------------------
\ typed-local-lint: allow-bare-local - row is the caller's own body, and a local
\ annotation cannot carry a quotation effect.
: EACH ( [ -- ] -- ) {: row :}
   s" TAG" s" hc2_tag" JUDGE-PASS:ROW!
      s" 9" JUDGE-PASS:IN+
      JUDGE-PASS:ALSO s" 24" JUDGE-PASS:IN+
      JUDGE-PASS:ALSO s" 255" JUDGE-PASS:IN+
      row execute
   \ One input per arm, then one that is no arm at all.
   s" WS?" s" hc2_ws" JUDGE-PASS:ROW!
      s" $20" JUDGE-PASS:IN+                       \ a space, the first arm
      JUDGE-PASS:ALSO s" $09" JUDGE-PASS:IN+       \ a tab
      JUDGE-PASS:ALSO s" $0A" JUDGE-PASS:IN+       \ a line feed
      JUDGE-PASS:ALSO s" $0D" JUDGE-PASS:IN+       \ a carriage return
      JUDGE-PASS:ALSO s" $61" JUDGE-PASS:IN+       \ `a`, which is none of them
      row execute
   \ Both bounds, and one byte outside each of them.
   s" SYM-FOLD-C" s" hc2_sym_fold_c" JUDGE-PASS:ROW!
      s" $41" JUDGE-PASS:IN+                       \ `A`, the lower bound itself
      JUDGE-PASS:ALSO s" $40" JUDGE-PASS:IN+       \ one under it
      JUDGE-PASS:ALSO s" $5A" JUDGE-PASS:IN+       \ `Z`, the upper bound itself
      JUDGE-PASS:ALSO s" $5B" JUDGE-PASS:IN+       \ one over it
      JUDGE-PASS:ALSO s" $61" JUDGE-PASS:IN+       \ already lower case
      row execute
   s" MAX-DIM" s" hc2_max_dim" JUDGE-PASS:ROW!
      s" 3 7" JUDGE-PASS:IN+
      JUDGE-PASS:ALSO s" 7 3" JUDGE-PASS:IN+       \ the other order
      JUDGE-PASS:ALSO s" 5 5" JUDGE-PASS:IN+       \ equal, which is neither arm
      row execute
   s" COUNT-CHAR" s" hc2_count_char" JUDGE-PASS:ROW!
      SPAN+  s" $61" JUDGE-PASS:IN+                \ `a`, at both ends and between
      JUDGE-PASS:ALSO SPAN+  s" $7A" JUDGE-PASS:IN+   \ `z`, in none of it
      JUDGE-PASS:ALSO EMPTY-SPAN+  s" $61" JUDGE-PASS:IN+  \ nothing to scan
      row execute
   s" T-RES-WALK" s" hc2_t_res_walk" JUDGE-PASS:ROW!
      s" CODEGEN-CORPUS2:CHAIN-HEAD" JUDGE-PASS:IN+
      JUDGE-PASS:ALSO s" CODEGEN-CORPUS2:NOT-A-VAR" JUDGE-PASS:IN+
      JUDGE-PASS:ALSO s" CODEGEN-CORPUS2:UNBOUND-VAR" JUDGE-PASS:IN+
      row execute
   \ The copy states ONE input. Its point is the store, and what it wrote is
   \ read back through the row's own reader over five cells - the four the copy
   \ moved and the fifth it must not have - so the arms a second input would
   \ reach are already reached by the witness.
   s" VEC-COPY-CELLS" s" hc2_vec_copy_cells" JUDGE-PASS:ROW!
      BUFFERS+  s" CODEGEN-CORPUS2:COPY-LEN" JUDGE-PASS:IN+  row execute ;

: OPEN-CORPUS ( -- )
   SOURCE$ SUFFIX$ QUALIFIER$ JUDGE-PASS:CORPUS! ;

public

\ Read the corpus source and compile every subject through the chain. Runs at
\ load, from inside the corpus's package.
: PUBLISH-ALL ( -- )
   OPEN-CORPUS
   [: JUDGE-PASS:PUBLISH ;] EACH ;

: JUDGE ( -- )
   OPEN-CORPUS
   [: JUDGE-PASS:BYTES ;] EACH
   [: JUDGE-PASS:VALUE ;] EACH
   JUDGE-PASS:TIME-PASSES 0 ?do [: JUDGE-PASS:TIME ;] EACH loop
   JUDGE-PASS:FLOOR ;

;package

package CODEGEN-CORPUS2
public

JUDGE-CORPUS2:PUBLISH-ALL

;package
