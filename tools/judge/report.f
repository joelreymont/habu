\ judge/report.f - the judged table as text, and the committed artifact it is
\ checked against. One concern: rendering what a run measured, and saying
\ whether the tree still agrees with it.
\
\ WHY THE ARTIFACT IS COMPARED AS TEXT. Every column in it is exact: a byte
\ count is read off a word's own dictionary record and is the same number on
\ every host in every run, a refusal code is what the compiler answered, and a
\ verdict follows from the two. So the whole file is a value the tree either
\ still produces or does not, and the check is a comparison of what this run
\ renders with what is committed - line for line, byte for byte. There is
\ nothing to parse and so nothing a parser could read past.
\
\ WHAT MOVES THE ARTIFACT, AND WHAT THAT MEANS. The chain emitting different
\ code moves a byte column; the chain gaining a capability turns a REFUSED row
\ into a compiled one; the chain losing one turns a compiled row into REFUSED;
\ and a change to the C twins moves the reference column. Every one of those is
\ a thing somebody did on purpose, so the artifact is regenerated in the same
\ change, and the diff is the record of what moved.
\
\ THE REFERENCE COLUMN IS PART OF THE COMPARISON AND THE ABSENCE OF IT IS NOT.
\ A host with no C compiler has no reference column, and the artifact says so on
\ its own line rather than falling silent; a host that HAS one must produce the
\ committed numbers, because what clang emits for a fixed C file with fixed
\ flags is a fact about that toolchain and a change in it is worth seeing.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/fmt.f
require tools/codegen-compare-clang.f
require tools/judge/row.f

package JUDGE-REPORT

private

$4000 constant TEXT-MAX
create TEXT TEXT-MAX allot
variable TEXT-U

32 constant NAME-COL
7 constant NUM-COL

: APPEND ( ptr u8 n -- ) {: a:ptr u:n :}
   TEXT-U @ u + TEXT-MAX > if E-JUDGE-REPORT-CAP throw then
   a  TEXT TEXT-U @ +  u STR-LEN BYTE-COPY-LEN
   TEXT-U @ u + TEXT-U ! ;

: NL ( -- )
   S\" \n" APPEND ;

: LINE ( ptr u8 n -- )
   APPEND NL ;

: PAD ( n -- ) {: n:n :}
   n 0 ?do s"  " APPEND loop ;

: PAD-RIGHT ( ptr u8 n n -- ) {: a:ptr u:n width:n :}
   a u APPEND
   width u > if width u - PAD then ;

: NUM$ ( n -- ptr u8 n ) {: v:n :}
   SB-RESET v FMT:SB-INT SB$ ;

: NUM-RIGHT ( n n -- ) {: v:n width:n :}
   v NUM$ {: a:ptr u:n :}
   width u > if width u - PAD then
   a u APPEND ;

: DASH-RIGHT ( n -- ) {: width:n :}
   width 1- PAD s" -" APPEND ;

\ The chain's cell: its byte count, or the code it refused the subject with.
: CHAIN-CELL ( n -- ) {: k:n :}
   k JUDGE-ROW:REFUSED? if k JUDGE-ROW:NEW-RC@ NUM-COL NUM-RIGHT exit then
   k JUDGE-ROW:NEW-BYTES@ NUM-COL NUM-RIGHT ;

: REF-CELL ( n -- ) {: k:n :}
   k JUDGE-ROW:COVERED? 0= if NUM-COL DASH-RIGHT exit then
   k JUDGE-ROW:REF-BYTES@ NUM-COL NUM-RIGHT ;

: ROW-LINE ( n -- ) {: k:n :}
   k JUDGE-ROW:NAME$ NAME-COL PAD-RIGHT
   k JUDGE-ROW:OLD-BYTES@ NUM-COL NUM-RIGHT
   k CHAIN-CELL
   k REF-CELL
   s"   " APPEND
   k JUDGE-ROW:VERDICT JUDGE-ROW:VERDICT$ LINE ;

: HEAD ( -- )
   s" habu code generator judge" LINE
   s" =========================" LINE
   NL
   s" One row per corpus subject, three code generators, judged on the bytes of" LINE
   s" machine code each emitted for the SAME program. The engine's emitter and the" LINE
   s" native chain compile one text - the corpus source file itself, read by" LINE
   s" tools/judge/src.f - and clang -O2 compiles a C twin of it." LINE
   NL
   s" Regenerate this file with ONE command, from the repository root:" LINE
   NL
   s"   bin/hb --load tools/judge.f > test/compiler/judge-baseline.txt" LINE
   NL
   s" and check the tree against it with:" LINE
   NL
   s"   bin/hb --load tools/judge.f -- --check" LINE
   NL
   s" THE CHAIN COLUMN HOLDS EITHER BYTES OR A REFUSAL CODE, and which of the two" LINE
   s" is what the verdict says. A refusal is measured every run by handing the" LINE
   s" subject to the chain and recording what came back, so a subject that starts" LINE
   s" being refused and one that stops both move this file. Nothing here is a list" LINE
   s" of names anybody keeps in step by hand." LINE
   NL
   s" A LARGER row is a finding and the check exits non-zero on it. A REFUSED row" LINE
   s" is a raw measurement printed with its code: the capability it waits for is a" LINE
   s" dot, and a check that failed on it would fail every day until that dot lands." LINE
   NL
   s" WHAT IS NOT HERE YET: the cost column. Timing a row means calling it, and a" LINE
   s" call has to name the word, which a refused subject does not have. The three" LINE
   s" columns' costs are measured today by bin/hb --load tools/codegen-compare.f," LINE
   s" run by hand on a quiet machine, and dot habu-judge-both-chains-2b07fd19" LINE
   s" carries bringing them here." LINE
   NL
   s" word                                old  chain  clang  verdict" LINE
   s" --------------------------------  -----  -----  -----  -------" LINE ;

: REFERENCE-NOTE ( -- )
   NL
   CODEGEN-CLANG:PRESENT? if
      s" clang flags: " APPEND CODEGEN-CLANG:FLAGS$ LINE
      exit
   then
   s" no clang column on this host: " APPEND CODEGEN-CLANG:ABSENT-WHY$ LINE ;

: TALLY ( -- )
   NL
   s" rows: " APPEND JUDGE-ROW:ROWS NUM$ APPEND
   s" , refused by the chain: " APPEND JUDGE-ROW:REFUSED-ROWS NUM$ APPEND
   s" , larger than the engine's: " APPEND JUDGE-ROW:LARGER-ROWS NUM$ LINE ;

public

\ The whole artifact, as the bytes a committed file holds.
: TEXT$ ( -- ptr u8 n )
   0 TEXT-U !
   HEAD
   JUDGE-ROW:ROWS 0 ?do i ROW-LINE loop
   REFERENCE-NOTE
   TALLY
   TEXT TEXT-U @ ;

;package
