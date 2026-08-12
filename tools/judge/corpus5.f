\ judge/corpus5.f - the judged rows of tools/codegen-compare-corpus5.f: seven
\ shapes about one decision, a call in tail position. One concern: which
\ subject is judged, on which pinned input, against which C twin.
\
\ WHAT THIS FILE STATES is what tools/judge/corpus4.f states and no more: the
\ subjects, one pinned input each, and the C symbol that is each one's twin.
\ The programs come from the corpus file's own bytes, and what the chain can
\ and cannot compile is measured rather than listed. The measuring is
\ tools/judge/pass.f's, which every corpus shares.
\
\ THIS CORPUS READS NO STORAGE. Every subject takes its argument and hands it
\ to a callee, so no row appends anything but its own number.
\
\ SIX OF THE SEVEN LEAVE BY A BRANCH, which is what the corpus exists to
\ measure, and it is why their byte counts are not the whole of their program:
\ a routine that tail-branches carries four bytes and the callee it reaches
\ carries the rest. The judged table says so on those rows rather than adding a
\ shared callee into each of them, which would claim its bytes six times over.
\
\ TAIL-PAIR LEAVES TWO VALUES. A generated body accounts for every value its
\ subject leaves - a timed one drops them, a valued one folds them together
\ with `xor` - so the row is compared on both of its results rather than on
\ whichever was on top.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require tools/codegen-compare-corpus5.f
require tools/judge/pass.f

package JUDGE-CORPUS5

private

: SOURCE$ ( -- ptr u8 n )
   s" tools/codegen-compare-corpus5.f" ;

\ A suffix of this corpus's own, because the judge measures the corpora one
\ after another into one dictionary and two of them may spell a subject the same
\ way.
: SUFFIX$ ( -- ptr u8 n )
   s" -J5" ;

\ The package this corpus publishes its subjects in, and therefore the package
\ the derived words are published in too. JUDGE runs with this package open, and
\ the reader that takes a size off a dictionary record is given the qualifier,
\ because that reader resolves a spelling as written.
: QUALIFIER$ ( -- ptr u8 n )
   s" CODEGEN-CORPUS5:" ;

\ ---- the seven rows, written once --------------------------------------------
\ typed-local-lint: allow-bare-local - row is the caller's own body, and a local
\ annotation cannot carry a quotation effect.
: EACH ( [ -- ] -- ) {: row :}
   s" TAIL-BIG" s" hc5_tail_big" JUDGE-PASS:ROW!
      s" 7" JUDGE-PASS:IN+  row execute
   s" TAIL-WORK" s" hc5_tail_work" JUDGE-PASS:ROW!
      s" 7" JUDGE-PASS:IN+  row execute
   s" NONTAIL" s" hc5_nontail" JUDGE-PASS:ROW!
      s" 7" JUDGE-PASS:IN+  row execute
   s" TAIL-MID" s" hc5_tail_mid" JUDGE-PASS:ROW!
      s" 7" JUDGE-PASS:IN+  row execute
   s" TAIL-CHAIN" s" hc5_tail_chain" JUDGE-PASS:ROW!
      s" 7" JUDGE-PASS:IN+  row execute
   s" TAIL-PAIR" s" hc5_tail_pair" JUDGE-PASS:ROW!
      s" 7 9" JUDGE-PASS:IN+  row execute
   s" TAIL-AFTER" s" hc5_tail_after" JUDGE-PASS:ROW!
      s" 7" JUDGE-PASS:IN+  row execute ;

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

\ The corpus's package, open around the publication, so the derived words land
\ beside the words they are compared against. It is written out here because
\ `package` parses its operand.
package CODEGEN-CORPUS5
public

JUDGE-CORPUS5:PUBLISH-ALL

;package
