\ judge.f - command line entry for the code generator judge.
\
\   bin/hb --load tools/judge.f
\       Judge every corpus and print the artifact on stdout. This is how the
\       committed file is regenerated:
\
\         bin/hb --load tools/judge.f > test/compiler/judge-baseline.txt
\
\   bin/hb --load tools/judge.f -- --check
\       Judge every corpus and compare what this tree produces with the
\       committed artifact, byte for byte. Names the line they first differ on
\       and exits non-zero; prints one line and exits zero when they agree.
\
\ WHAT THE JUDGE IS AND WHY IT IS NOT THE OLD COMPARISON. The old-versus-new
\ comparison, now deleted, held every subject TWICE - a real
\ definition in a corpus file and a hand-retyped string literal beside it - and
\ keeps a hand-written list of the subjects the chain cannot compile. The judge
\ holds each subject once, derives the chain's copy from the corpus file's own
\ bytes, and MEASURES the refusal instead of listing it. The two harnesses run
\ side by side until the cut; this one is what the campaign reads from.
\
\ THE SAME COMPARISON IS WHAT THE GATE RUNS. tools/judge-test.f asserts on
\ tools/judge/check.f, which is the word this entry drives, so what passes in
\ the gate is what this command does.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/process-argv.f
require tools/judge/check.f

package JUDGE-ENTRY

private

1 constant FAIL-RC

: CHECK-FLAG$ ( -- ptr u8 n )
   s" --check" ;

: CHECKING? ( -- bool )
   false
   SCRIPT-ARGC 0 ?do
      i SCRIPT-ARGV$ CHECK-FLAG$ STR= if drop true then
   loop ;

: CHECK ( -- )
   JUDGE-CHECK:JUDGE-ALL
   JUDGE-CHECK:DIFF-AT {: at:n :}
   at 0 >= if
      at JUDGE-CHECK:SAY-DIFF
      S\" judge: the committed artifact is not what this tree produces\n" FAIL-RC die
   then
   s" judge: " type JUDGE-ROW:ROWS FMT:.U
   s"  row(s) agree with " type JUDGE-CHECK:ARTIFACT$ type cr ;

: RENDER ( -- )
   JUDGE-CHECK:JUDGE-ALL
   JUDGE-REPORT:TEXT$ type ;

public

: MAIN ( -- )
   CHECKING? if CHECK exit then
   RENDER ;

;package

JUDGE-ENTRY:MAIN
