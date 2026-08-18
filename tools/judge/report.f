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
require tools/codegen-compare-core.f
require tools/judge/pass.f

package JUDGE-REPORT

private

$4000 constant TEXT-MAX
create TEXT TEXT-MAX allot
variable TEXT-U

32 constant NAME-COL
7 constant NUM-COL
8 constant TRAF-COL
9 constant COST-COL

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

\ A data-stack traffic cell. A column with no routine to read - the chain on a
\ refused row - carries a dash rather than a zero, because zero accesses is
\ something a routine can really make and a missing routine is not.
: TRAFFIC-CELL ( n -- ) {: n-acc:n :}
   n-acc JUDGE-ROW:NOT-MEASURED = if TRAF-COL DASH-RIGHT exit then
   n-acc TRAF-COL NUM-RIGHT ;

: ROW-LINE ( n -- ) {: k:n :}
   k JUDGE-ROW:NAME$ NAME-COL PAD-RIGHT
   k JUDGE-ROW:OLD-BYTES@ NUM-COL NUM-RIGHT
   k CHAIN-CELL
   k REF-CELL
   k JUDGE-ROW:OLD-TRAFFIC@ TRAFFIC-CELL
   k JUDGE-ROW:NEW-TRAFFIC@ TRAFFIC-CELL
   s"   " APPEND
   k JUDGE-ROW:VERDICT JUDGE-ROW:VERDICT$ APPEND
   k JUDGE-ROW:NEW-TAIL? if s"  (tail)" APPEND then
   NL ;

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
   s" A ROW MARKED (tail) IS NOT LIKE FOR LIKE. The chain's routine there leaves by" LINE
   s" a branch to a callee, so its byte count is the routine and not the whole of" LINE
   s" the program: the callee is real and its bytes are under some other row or" LINE
   s" under none. The callee is NOT added in, because one callee is reached by" LINE
   s" several rows and adding it to each would claim its bytes several times over." LINE
   s" Read the gap on those rows as arithmetic rather than as a verdict." LINE
   NL
   s" A LARGER row is a finding and the check exits non-zero on it. A REFUSED row" LINE
   s" is a raw measurement printed with its code: the capability it waits for is a" LINE
   s" dot, and a check that failed on it would fail every day until that dot lands." LINE
   s" NO ROW IS REFUSED. The last one was PRESSURE-LOOP, whose -8508 was the" LINE
   s" allocator declining to spill inside a loop; it closed by the loop no longer" LINE
   s" holding the values, because src/compiler/native/loop.f moves reads that" LINE
   s" cannot change with the turn out of the body. tools/judge-test.f asserts the" LINE
   s" count at zero AND fails on a refusal carrying any other code, so a gap" LINE
   s" cannot arrive unnamed and a row cannot go back unnoticed." LINE
   NL
   s" THE TABLE IS FINITE AND THE ORACLE BESIDE IT IS NOT. Every row here is a" LINE
   s" program somebody wrote on an input somebody pinned, so what it can catch is" LINE
   s" bounded by what anybody thought to write down. tools/judge/fuzz.f generates" LINE
   s" straight-line integer programs from a CONSTANT seed, compiles each through" LINE
   s" both code generators from one text, and requires the same cell back on the" LINE
   s" ends of the signed range and on generated inputs. The gate runs a small" LINE
   s" fixed number of them; the full sweep is a hand run:" LINE
   NL
   s"   bin/hb --load tools/judge-fuzz.f" LINE
   NL
   s" WHAT NEITHER THE ORACLE NOR THIS TABLE CAN SEE. Both columns of a two-way" LINE
   s" row read ONE text. A reader that derived the wrong program hands the SAME" LINE
   s" wrong program to both compilers, they agree about it, and no differential" LINE
   s" test can notice - a shared-mode failure is the honest boundary of this" LINE
   s" tool's coverage. What guards the shared half is the reader's own fixtures," LINE
   s" tools/judge/src-test.f, which attack it with sources built to fool it." LINE
   NL
   s" ds-old AND ds-new ARE THE CALLER'S OWN DATA STACK, touched. They count the" LINE
   s" loads and stores each column's emitted code makes against the register the" LINE
   s" running engine keeps its data-stack pointer in - all four spellings of one" LINE
   s" access, over and under the pointer and in either register file. That is the" LINE
   s" structural difference between the two code generators: the engine moves every" LINE
   s" intermediate through a slot in memory and the chain holds it in a register" LINE
   s" and touches the slot at the edges of the routine. It is EXACT, it moves for" LINE
   s" one reason and it does not turn on host load, so it is compared here rather" LINE
   s" than printed below with the costs. It is NOT the cost claim restated: a chain" LINE
   s" that made the same accesses and took twice as long would pass every row of" LINE
   s" it. That claim is bin/hb --load tools/judge-timed.f, run by hand." LINE
   NL
   s" ONE ROW TOUCHES IT MORE OFTEN THAN THE ENGINE DOES and the tally says so." LINE
   s" On CODEGEN-CORPUS4:CALL-FAN-BIG the engine CALLS its callee five times, so" LINE
   s" the caller's own code touches no slot at all and the five callees do that" LINE
   s" work inside their own routines; the chain copies the five bodies in and" LINE
   s" makes one entry and one exit of its own. Neither number is the whole" LINE
   s" program, which is the same caveat a (tail) row carries. A row heavier here" LINE
   s" is a fact to read beside its bytes and its cost and not a verdict of its" LINE
   s" own, which is why every number is pinned and none of them is a finding by" LINE
   s" itself." LINE
   NL
   s" THE COSTS ARE BELOW THIS TABLE AND ARE NOT PART OF THE CHECK. A byte count" LINE
   s" is the same number on every host in every run; a cost is a measurement, and" LINE
   s" a machine with every core busy - which is what a gate is - moves one by more" LINE
   s" than any honest tolerance would catch. So the check compares everything down" LINE
   s" to the line that says so, and the costs are printed under it as what they" LINE
   s" are, with the spread this run actually measured beside them." LINE
   NL
   s" word                                old  chain  clang  ds-old  ds-new  verdict" LINE
   s" --------------------------------  -----  -----  -----  ------  ------  -------" LINE ;

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
   s" , larger than the engine's: " APPEND JUDGE-ROW:LARGER-ROWS NUM$ APPEND
   s" , columns disagreeing on the answer: " APPEND JUDGE-ROW:DISAGREEING-ROWS NUM$ APPEND
   s" , leaving by a tail branch: " APPEND JUDGE-ROW:TAIL-ROWS NUM$ APPEND
   s" , touching the caller's stack more often: " APPEND JUDGE-ROW:HEAVIER-ROWS NUM$ LINE ;

\ ---- the measured half -------------------------------------------------------
\ Everything below MARK is printed and none of it is compared.

: MARK$ ( -- ptr u8 n )
   s" ---- measured on this host, printed and not checked -------------------" ;

\ The machine a cost was measured on, as far as the engine can say: the target
\ it was built for. The numbers below are local to one host and are not
\ comparable with numbers from another - which is the whole reason they are
\ printed here rather than checked.
: TARGET$ ( -- ptr u8 n )
   HB-TARGET-LINUX? if s" linux-arm64" exit then
   HB-TARGET-MACOS? if s" macos-arm64" exit then
   s" unknown-target" ;

\ Nanoseconds, to two places, from picoseconds. A body smaller than the
\ measurement's own floor can come out slightly negative; that is the noise and
\ it is printed rather than clamped, because a clamped zero would read as a
\ measurement.
: NS-COL ( n n -- ) {: picos:n width:n :}
   SB-RESET picos s>f 1000.0 f/ 2 FMT:SB-FIX SB$ {: a:ptr u:n :}
   width u > if width u - PAD then
   a u APPEND ;

: DASH-NS ( n -- ) {: width:n :}
   width 1- PAD s" -" APPEND ;

: COST-LINE ( n -- ) {: k:n :}
   k JUDGE-ROW:NAME$ NAME-COL PAD-RIGHT
   k JUDGE-ROW:OLD-PICOS@ JUDGE-ROW:FLOOR@ - COST-COL NS-COL
   k JUDGE-ROW:REFUSED? if COST-COL DASH-NS
   else k JUDGE-ROW:NEW-PICOS@ JUDGE-ROW:FLOOR@ - COST-COL NS-COL then
   k JUDGE-ROW:COVERED? if
      k JUDGE-ROW:REF-PICOS@ k JUDGE-ROW:REF-FLOOR@ - COST-COL NS-COL
   else COST-COL DASH-NS then
   NL ;

: COSTS ( -- )
   NL
   MARK$ LINE
   NL
   s" One call, in nanoseconds, with the measurement's own floor taken off. The" LINE
   s" two habu columns are entered the same way - a quotation that calls one word" LINE
   s" - so they lose the same constant and their difference is exact. The" LINE
   s" reference column is reached through a foreign call, so it loses a floor of" LINE
   s" its own: the row's own body run again against an empty C function of the" LINE
   s" same arity, which marshals the same arguments into the same places." LINE
   NL
   s" MACHINE: " APPEND TARGET$ APPEND
   s" . These numbers are local to one host and mean nothing" LINE
   s" against numbers measured on another." LINE
   NL
   s" METHOD. Each body is generated from the row's ONE pinned input, certified by" LINE
   s" the checker and compiled, then run " APPEND
   CODEGEN-COMPARE:REPS NUM$ APPEND
   s"  times per timed run, " APPEND
   CODEGEN-COMPARE:RUNS NUM$ APPEND
   s"  timed" LINE
   s" runs, fastest run kept. The whole row list is measured " APPEND
   JUDGE-PASS:TIME-PASSES NUM$ APPEND
   s"  times over, so the columns" LINE
   s" are interleaved rather than measured in blocks; each column keeps its" LINE
   s" fastest." LINE
   NL
   s" EVERY PASS GENERATES ITS OWN BODY, so a row's two measurements sit at" LINE
   s" different addresses with a cold instruction cache each time. That widens the" LINE
   s" spread and it is deliberate: a placement a row was lucky in once is not a" LINE
   s" property of the code the compiler emitted." LINE
   NL
   s" NOISE FLOOR, MEASURED THIS RUN: " APPEND JUDGE-ROW:SPREAD@ NUM$ APPEND
   s"  permille. That is the widest gap between" LINE
   s" two measurements of ONE program in this process, so a difference between two" LINE
   s" columns smaller than it is not a difference. The floor itself measured " APPEND
   JUDGE-ROW:FLOOR@ NUM$ APPEND s"  ps." LINE
   NL
   s" word                                  old    chain    clang" LINE
   s" --------------------------------  -------  -------  -------" LINE
   JUDGE-ROW:ROWS 0 ?do i COST-LINE loop ;

public

\ Everything a committed artifact holds, checked half first.
: TEXT$ ( -- ptr u8 n )
   0 TEXT-U !
   HEAD
   JUDGE-ROW:ROWS 0 ?do i ROW-LINE loop
   REFERENCE-NOTE
   TALLY
   COSTS
   TEXT TEXT-U @ ;

\ Where the checked half ends. The check compares the bytes before this line
\ and prints what follows it.
: MARK-TEXT$ ( -- ptr u8 n )
   MARK$ ;

;package
