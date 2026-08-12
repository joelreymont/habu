\ judge/check.f - judging every corpus and holding the result against the
\ committed artifact. One concern: the run, and where it disagrees with what is
\ committed.
\
\ WHY THIS IS NOT IN THE COMMAND LINE ENTRY. Two callers ask the same question:
\ tools/judge.f, which a person runs and which exits non-zero on a
\ disagreement, and tools/judge-test.f, which a gate schedules and which asserts
\ on it. A second copy of the comparison in one of them would be a test checking
\ its own arithmetic instead of the tool's, so the comparison is here and both
\ callers reach it.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require tools/lint/text.f
require tools/judge/row.f
require tools/judge/report.f
require tools/judge/corpus4.f
require tools/judge/corpus5.f

package JUDGE-CHECK

private

$8000 constant FILE-MAX
create FILE FILE-MAX allot
variable FILE-U

public

: ARTIFACT$ ( -- ptr u8 n )
   s" test/compiler/judge-baseline.txt" ;

\ Measure every corpus into the table. One pass, in one process, because the
\ chain publishes its derived words into the dictionary this process holds.
\
\ THE CHAIN'S WORDS ARE ALREADY PUBLISHED when this runs. Each corpus file
\ publishes its own at load, from a top-level line inside its corpus's package,
\ because a migration publishes where the INTERPRETER's wordlist points and a
\ derived body may name storage private to that corpus. What is left here is
\ the measuring, which needs no package open: every generated body names its
\ words qualified.
: JUDGE-ALL ( -- )
   JUDGE-ROW:RESET
   JUDGE-CORPUS4:JUDGE
   JUDGE-CORPUS5:JUDGE ;

\ What the committed artifact holds.
: COMMITTED$ ( -- ptr u8 n )
   ARTIFACT$ FILE FILE-MAX READ-FILE {: a:ptr u:n :}
   u FILE-U !
   FILE FILE-U @ ;

private

\ Where a text says its checked half ends. The artifact carries a costs section
\ under a marker line, and none of it is compared: a cost is a measurement and
\ a gate is a loaded machine. A text with no marker is refused rather than
\ compared whole - a truncated artifact would otherwise be checked against a
\ prefix of itself and pass.
: MARK-AT ( ptr u8 n -- n ) {: a:ptr u:n :}
   JUDGE-REPORT:MARK-TEXT$ {: ma:ptr mu:n :}
   -1
   u mu < if 0 else u mu - 1+ then 0 ?do
      dup 0 < if
         a i + mu ma mu STR= if drop i then
      then
   loop
   dup 0 < if E-JUDGE-CHECK-MARK throw then ;

\ The half of a text that is compared: everything before the marker line.
: CHECKED$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   a u MARK-AT {: at:n :}
   a at ;

\ The first byte at which two texts differ, or -1 when one is a prefix of the
\ other and they are the same length. A shorter file is a disagreement at its
\ own end, which is what a truncated artifact looks like.
: FIRST-DIFF ( ptr u8 n ptr u8 n -- n ) {: a:ptr u:n b:ptr v:n :}
   -1
   u v < if u else v then 0 ?do
      dup 0 < i a + c@ i b + c@ <> and if drop i then
   loop
   dup 0 < u v <> and if
      drop u v < if u else v then
   then ;

public

\ Where this tree's judgement and the committed artifact first differ, or -1
\ when their checked halves are the same bytes.
: DIFF-AT ( -- n )
   JUDGE-REPORT:TEXT$ CHECKED$ COMMITTED$ CHECKED$ FIRST-DIFF ;

\ Which line of the rendered artifact a byte offset falls on, counting from one.
: LINE-OF ( n -- n ) {: at:n :}
   JUDGE-REPORT:TEXT$ {: a:ptr u:n :}
   1
   at u < if at else u then 0 ?do
      i a + c@ $0A = if 1+ then
   loop ;

: SAY-DIFF ( n -- ) {: at:n :}
   s" judge: the tree and " type ARTIFACT$ type
   s"  disagree at byte " type at FMT:.U
   s" , line " type at LINE-OF FMT:.U cr
   s" judge: regenerate with  bin/hb --load tools/judge.f > " type ARTIFACT$ type cr ;

;package
