\ codegen-workload-report.f - the report a person reads after a workload run.
\ One concern: turning what the run recorded into text.
\
\ FIVE SECTIONS, AND WHY EACH IS THERE.
\
\   1  WHO THE ENGINE CALLS AND WHO IT COPIES, over the surveyed hot words of
\      the live engine. This is the first thing a reader has to see, because it
\      decides what a migration could possibly change: a word the engine copies
\      into its callers has no call instruction anywhere pointing at it, so
\      republishing it changes nothing about any caller that already exists.
\   2  THE SUBJECTS OF THIS RUN, as the two code generators compiled them, with
\      the same question asked of each.
\   3  HOW EACH ARM REACHES ITS SUBJECT, read off the arm's own machine code. An
\      after-arm whose driver still calls the before-arm's word would report a
\      delta of nothing and look perfectly healthy, so the report shows which
\      record each arm's call instruction actually enters.
\   4  THE WORKLOADS: each one's two arms, its delta, and the two rows that say
\      whether the delta is readable - the control and the floor.
\   5  THE VERDICT PER WORKLOAD, in one line each, with no hedging: a delta that
\      does not clear its own control and floor is reported as not measurable
\      rather than as a small improvement.
\
\ NOTHING HERE THROWS ON A NUMBER. A report that failed when a row came out slow
\ would be a gate, and a gate on a timing fails for host load; the gate this work
\ carries is tools/codegen-workload-test.f, and every assertion in it is a fact
\ about compiled code.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/fmt.f
require tools/codegen-workload-scan.f
require tools/codegen-workload-time.f
require tools/codegen-workload-run.f

package CODEGEN-WREPORT

private

32 constant SPACE-BYTE
10 constant PER-CENT-SCALE      \ a permille is a tenth of a per cent

: SP ( n -- )
   dup 0 <= if drop exit then
   0 ?do SPACE-BYTE emit loop ;

: WIDTH ( n -- n ) {: v:n :}
   SB-RESET v FMT:SB-INT SB$ nip ;

: N. ( n n -- ) {: v:n w:n :}
   w v WIDTH - SP
   v FMT:.INT ;

: T. ( ptr u8 n n -- ) {: a:ptr u:n w:n :}
   a u type
   w u - SP ;

\ The same, right-aligned, for a heading over a column of numbers.
: TR. ( ptr u8 n n -- ) {: a:ptr u:n w:n :}
   w u - SP
   a u type ;

: RULE ( -- )
   s" ---------------------------------------------------------------------" type cr ;

\ A permille, as a signed percentage with one decimal. The delta and the two
\ noise figures are all permille, so they all print through here and a reader
\ never has to work out which unit a column is in.
: PCT. ( n -- ) {: v:n :}
   v 0 < if s" -" type then
   v 0 < if 0 v - else v then {: m:n :}
   m PER-CENT-SCALE / FMT:.INT
   s" ." type
   m PER-CENT-SCALE mod FMT:.INT
   s" %" type ;

: PCT-WIDTH ( n -- n ) {: v:n :}
   v 0 < if 1 else 0 then
   v 0 < if 0 v - else v then {: m:n :}
   m PER-CENT-SCALE / WIDTH + 3 + ;

: PCT-COL ( n n -- ) {: v:n w:n :}
   w v PCT-WIDTH - SP
   v PCT. ;

: YESNO ( bool ptr u8 n ptr u8 n -- ) {: f:bool ya:ptr yu:n na:ptr nu:n :}
   f if ya yu type exit then
   na nu type ;

\ ---- 1. the engine's own hot words ------------------------------------------

: ENGINE-ROW ( ptr u8 n -- ) {: a:ptr u:n :}
   a u 26 T.
   a u CODEGEN-SCAN:LIVE? 0= if s" not in this image" type cr exit then
   a u CODEGEN-SCAN:WORD-BYTES 7 N.
   s"   " type
   a u CODEGEN-SCAN:ENGINE-COPIES? s" copied" s" called" YESNO
   a u CODEGEN-SCAN:COPY-BYTES 9 N.
   a u CODEGEN-SCAN:CALL-SITES 9 N.
   a u CODEGEN-SCAN:CALLERS-OF 9 N.
   cr ;

public

: ENGINE-TABLE ( -- )
   s" 1. WHO THE ENGINE CALLS, AND WHO IT COPIES" type cr
   RULE
   s" The surveyed hot words, in the engine this process is running. A word the" type cr
   s" engine COPIES has its body pasted into every caller compiled after it was" type cr
   s" published, so no call instruction anywhere points at it and republishing" type cr
   s" it cannot reach a caller that already exists." type cr
   cr
   s" word" 26 T. s" bytes" 7 TR. s"    engine" type
   s" copied" 9 TR. s" sites" 9 TR. s" callers" 9 TR. cr
   s" TAG" ENGINE-ROW
   s" PAY" ENGINE-ROW
   s" SYM-FOLD-C" ENGINE-ROW
   s" TV-NEXT?" ENGINE-ROW
   s" T-RES-WALK" ENGINE-ROW
   s" T-COMPRESS" ENGINE-ROW
   cr ;

private

: SUBJECT-ROW ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: na:ptr nu:n oa:ptr ou:n ca:ptr cu:n :}
   na nu 12 T.
   oa ou CODEGEN-SCAN:WORD-BYTES 7 N.
   ca cu CODEGEN-SCAN:WORD-BYTES 7 N.
   s"    " type
   oa ou CODEGEN-SCAN:ENGINE-COPIES? s" copied" s" called" YESNO
   s"    " type
   ca cu CODEGEN-SCAN:ENGINE-COPIES? s" copied" s" called" YESNO
   cr ;

public

: SUBJECT-TABLE ( -- )
   s" 2. THE SUBJECTS OF THIS RUN" type cr
   RULE
   s" One body each, compiled twice: by the engine and by the native chain. The" type cr
   s" last two columns are the same question section 1 asks, of each column's" type cr
   s" own machine code." type cr
   cr
   s" subject" 12 T. s" old" 7 TR. s" new" 7 TR. s"    old is   new is" type cr
   s" FOLD-C"   s" HOT-ENGINE:FOLD-C"   s" HOT-CHAIN:FOLD-C"   SUBJECT-ROW
   s" COUNT-CH" s" HOT-ENGINE:COUNT-CH" s" HOT-CHAIN:COUNT-CH" SUBJECT-ROW
   s" TERM-TAG" s" HOT-ENGINE:TERM-TAG" s" HOT-CHAIN:TERM-TAG" SUBJECT-ROW
   s" TERM-PAY" s" HOT-ENGINE:TERM-PAY" s" HOT-CHAIN:TERM-PAY" SUBJECT-ROW
   cr ;

private

: ARM-ROW ( ptr u8 n ptr u8 n -- ) {: da:ptr du:n sa:ptr su:n :}
   da du 22 T.
   da du CODEGEN-SCAN:WORD-BYTES 7 N.
   da du CODEGEN-SCAN:BLS-IN 7 N.
   s"    " type
   da du sa su CODEGEN-SCAN:CALLS? if
      s" calls " type sa su type
   else
      s" carries a copy of the body" type
   then
   cr ;

public

: ARM-TABLE ( -- )
   s" 3. HOW EACH ARM REACHES ITS SUBJECT" type cr
   RULE
   s" Read off the arm's own compiled code. Every arm has to reach ITS OWN" type cr
   s" column's word: an after-arm still entering the before-arm's record would" type cr
   s" measure one code generator twice and report a delta of nothing." type cr
   cr
   s" driver" 22 T. s" bytes" 7 TR. s" calls" 7 TR. s"    reaches" type cr
   s" WORKLOAD:SCAN-OLD"    s" HOT-ENGINE:FOLD-C" ARM-ROW
   s" WORKLOAD:SCAN-NEW"    s" HOT-CHAIN:FOLD-C" ARM-ROW
   s" WORKLOAD:SCAN-CTL-A"  s" HOT-FIXED:FOLD-C" ARM-ROW
   s" WORKLOAD:SCAN-CTL-B"  s" HOT-FIXED:FOLD-C" ARM-ROW
   s" WORKLOAD:COUNT-OLD"   s" HOT-ENGINE:COUNT-CH" ARM-ROW
   s" WORKLOAD:COUNT-NEW"   s" HOT-CHAIN:COUNT-CH" ARM-ROW
   s" WORKLOAD:COUNT-CTL-A" s" HOT-FIXED:COUNT-CH" ARM-ROW
   s" WORKLOAD:COUNT-CTL-B" s" HOT-FIXED:COUNT-CH" ARM-ROW
   s" WORKLOAD:TERM-OLD"    s" HOT-ENGINE:TERM-TAG" ARM-ROW
   s" WORKLOAD:TERM-NEW"    s" HOT-CHAIN:TERM-TAG" ARM-ROW
   s" WORKLOAD:TERM-CTL-A"  s" HOT-FIXED:TERM-TAG" ARM-ROW
   s" WORKLOAD:TERM-CTL-B"  s" HOT-FIXED:TERM-TAG" ARM-ROW
   cr ;

private

: ROW. ( n -- ) {: k:n :}
   k CODEGEN-CLOCK:NAME$ 16 T.
   k CODEGEN-CLOCK:OLD-NS 12 N.
   k CODEGEN-CLOCK:NEW-NS 12 N.
   k CODEGEN-CLOCK:DELTA-PERMILLE 9 PCT-COL
   k CODEGEN-CLOCK:OLD-SPREAD 9 PCT-COL
   k CODEGEN-CLOCK:NEW-SPREAD 9 PCT-COL
   s"   " type
   k CODEGEN-CLOCK:INTERLEAVED? s" woven" s" split" YESNO
   s"   " type
   k CODEGEN-CLOCK:SAME-ANSWER? s" agree" s" DISAGREE" YESNO
   cr ;

public

: ROW-TABLE ( -- )
   s" 4. THE WORKLOADS" type cr
   RULE
   s" Fastest run of each arm, in nanoseconds; the delta is what the new code" type cr
   s" generator saved. `spread` is how far apart that arm's fastest and slowest" type cr
   s" runs were. `woven` means the two arms' runs were threaded through each" type cr
   s" other; `split` means the workload compiles something and its arms had to" type cr
   s" be measured on either side of the migration." type cr
   cr
   s" row" 16 T. s" old ns" 12 TR. s" new ns" 12 TR.
   s" delta" 9 TR. s" old sprd" 9 TR. s" new sprd" 9 TR. s"   order   answers" type cr
   CODEGEN-CLOCK:ROWS 0 ?do i ROW. loop
   cr ;

private

\ ---- 5. the verdict ---------------------------------------------------------
: MAG ( n -- n ) {: v:n :}
   v 0 < if 0 v - exit then
   v ;

\ How big a delta this harness produces when NOTHING changed. A control row and
\ a floor row each run old code against old code, under the same fastest-of-N
\ rule as the workload row, so the size of their delta is this measurement's own
\ false-positive size and is the honest bar for the workload row beside them.
\
\ The within-arm SPREAD is deliberately not part of the bar. A spread says how
\ far apart that arm's own runs were, which is what the fastest-run rule exists
\ to cut through: the scan rows here routinely spread by sixty per cent while
\ their fastest runs repeat to within a few, and building the bar out of spreads
\ made a workload with a control delta of two parts in a thousand fail to clear
\ it. The spreads are printed in the row table for the reader to judge; the bar
\ is made of the artifact deltas that were actually measured.
: ARTIFACT-OF ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u CODEGEN-CLOCK:ROW-OF {: k:n :}
   k 0 < if 0 exit then
   k CODEGEN-CLOCK:DELTA-PERMILLE MAG ;

: VERDICT ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: wa:ptr wu:n ca:ptr cu:n fa:ptr fu:n :}
   wa wu 16 T.
   wa wu CODEGEN-CLOCK:ROW-OF {: k:n :}
   k 0 < if s" not measured" type cr exit then
   k CODEGEN-CLOCK:DELTA-PERMILLE {: d:n :}
   ca cu ARTIFACT-OF {: cf:n :}
   fa fu ARTIFACT-OF {: ff:n :}
   cf ff > if cf else ff then {: bar:n :}
   d PCT. s"  saved, against a " type bar PCT. s"  bar" type
   d MAG bar > if
      d 0 > if s"   - REAL" type else s"   - REAL LOSS" type then
   else
      s"   - NOT MEASURABLE" type
   then
   cr ;

\ A row with no control and no floor beside it. The compile-shaped workload is
\ the only one: its arms straddle the migration, so there is no way to compile a
\ second pair of them that differs in nothing. All it has to hold its delta
\ against is how far apart its own runs were, which is a weaker bar and is
\ labelled as one.
: VERDICT-SOLO ( ptr u8 n -- ) {: wa:ptr wu:n :}
   wa wu 16 T.
   wa wu CODEGEN-CLOCK:ROW-OF {: k:n :}
   k 0 < if s" not measured" type cr exit then
   k CODEGEN-CLOCK:DELTA-PERMILLE {: d:n :}
   k CODEGEN-CLOCK:NOISE-PERMILLE {: bar:n :}
   d PCT. s"  saved, no control row, own spread " type bar PCT.
   d MAG bar > if
      d 0 > if s"   - REAL" type else s"   - REAL LOSS" type then
   else
      s"   - NOT MEASURABLE" type
   then
   cr ;

public

: VERDICT-TABLE ( -- )
   s" 5. WHAT EACH WORKLOAD'S NUMBER MEANS" type cr
   RULE
   s" The bar a delta has to clear is the larger of the two deltas this harness" type cr
   s" produced when NOTHING changed: the control row, whose two arms are old code" type cr
   s" compiled either side of the migration, and the floor row, whose two arms are" type cr
   s" old code reaching two different publications of the same subject. A delta" type cr
   s" under that bar is not a small win; it is a win this measurement cannot see." type cr
   cr
   s" scan"  s" scan-control"  s" scan-floor"  VERDICT
   s" count" s" count-control" s" count-floor" VERDICT
   s" term"  s" term-control"  s" term-floor"  VERDICT
   s" check-batch" VERDICT-SOLO
   cr ;

: TITLE ( -- )
   s" habu code generator - end to end workload measurement" type cr
   s" =====================================================" type cr
   cr
   s" One process. A program is put into the dictionary twice - once compiled by" type cr
   s" the engine's emitter, once by the native chain - and the same workloads are" type cr
   s" run against both. Everything below is measured in this run; nothing is read" type cr
   s" from a committed file." type cr
   cr ;

: PRINT ( -- )
   TITLE
   ENGINE-TABLE
   SUBJECT-TABLE
   ARM-TABLE
   ROW-TABLE
   VERDICT-TABLE ;

;package
