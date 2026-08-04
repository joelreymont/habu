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
\   4  THE WORKLOADS: each one's two arms, its delta, and beneath it the null
\      rows of its own family - the rows that ran the same program on both arms
\      and so measure what this harness invents when nothing changed.
\   5  THE VERDICT PER WORKLOAD, in one line each, with no hedging: a delta that
\      does not clear the largest of its family's null rows is reported as not
\      measurable rather than as a small improvement.
\
\ NOTHING HERE THROWS ON A NUMBER, AND EXACTLY ONE THING HERE THROWS. A report
\ that failed when a row came out slow would be a gate, and a gate on a timing
\ fails for host load; the gate this work carries is
\ tools/codegen-workload-test.f, and every assertion in it is a fact about
\ compiled code. What this file does refuse is a verdict with no bar behind it.
\ The bar used to be assembled from row names written out by hand here, and a
\ name that matched no row scored as a bar of nothing - renaming one row made
\ every verdict in the table read REAL, and the run still exited zero. So the
\ bar is no longer named here at all: each row records which family it belongs
\ to and whether it is a real comparison or a null draw, and a family with no
\ null draw throws E-WLTIME-BAR instead of being judged against zero.

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
   s" HOT-REACH:SYM-FOLD-C" ENGINE-ROW
   s" TV-NEXT?" ENGINE-ROW
   s" T-RES-WALK" ENGINE-ROW
   s" T-COMPRESS" ENGINE-ROW
   cr
   s" The fold in the middle is the checker's own, and its row now reads zero" type cr
   s" sites for a reason: it was migrated into HOT-REACH before the after-arm," type cr
   s" and the " type CODEGEN-HOT:REACHED FMT:.INT
   s"  call instructions that entered the engine's code were" type cr
   s" moved onto the chain's routine (src/compiler/native/reach.f). So the" type cr
   s" compile-shaped arms below are separated by a migration that really did" type cr
   s" reach the callers already in this binary." type cr
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

: SCAN-ARMS ( -- )
   s" WORKLOAD:SCAN-OLD"    s" HOT-ENGINE:FOLD-C" ARM-ROW
   s" WORKLOAD:SCAN-NEW"    s" HOT-CHAIN:FOLD-C" ARM-ROW
   s" WORKLOAD:SCAN-CTL-A"  s" HOT-FIXED:FOLD-C" ARM-ROW
   s" WORKLOAD:SCAN-CTL-B"  s" HOT-FIXED:FOLD-C" ARM-ROW
   s" WORKLOAD:SCAN-F1"     s" HOT-F1:FOLD-C" ARM-ROW
   s" WORKLOAD:SCAN-F2"     s" HOT-F2:FOLD-C" ARM-ROW
   s" WORKLOAD:SCAN-F3"     s" HOT-F3:FOLD-C" ARM-ROW
   s" WORKLOAD:SCAN-F4"     s" HOT-F4:FOLD-C" ARM-ROW ;

: COUNT-ARMS ( -- )
   s" WORKLOAD:COUNT-OLD"   s" HOT-ENGINE:COUNT-CH" ARM-ROW
   s" WORKLOAD:COUNT-NEW"   s" HOT-CHAIN:COUNT-CH" ARM-ROW
   s" WORKLOAD:COUNT-CTL-A" s" HOT-FIXED:COUNT-CH" ARM-ROW
   s" WORKLOAD:COUNT-CTL-B" s" HOT-FIXED:COUNT-CH" ARM-ROW
   s" WORKLOAD:COUNT-F1"    s" HOT-F1:COUNT-CH" ARM-ROW
   s" WORKLOAD:COUNT-F2"    s" HOT-F2:COUNT-CH" ARM-ROW
   s" WORKLOAD:COUNT-F3"    s" HOT-F3:COUNT-CH" ARM-ROW
   s" WORKLOAD:COUNT-F4"    s" HOT-F4:COUNT-CH" ARM-ROW ;

\ The mixed-coverage drivers each hold THREE calls: the passes the migration is
\ meant to reach, named bare and resolved by the arm's search order, and the
\ passes it is not, which name HOT-FIXED outright. The row below names the leg
\ the migration is supposed to reach; the call count beside it is 3 in every arm,
\ which is what says the three passes are three real calls and not one call the
\ compiler folded together.
: MIX-ARMS ( -- )
   s" WORKLOAD:MIX66-OLD"   s" HOT-ENGINE:COUNT-CH" ARM-ROW
   s" WORKLOAD:MIX66-NEW"   s" HOT-CHAIN:COUNT-CH" ARM-ROW
   s" WORKLOAD:MIX66-CTL-A" s" HOT-FIXED:COUNT-CH" ARM-ROW
   s" WORKLOAD:MIX66-CTL-B" s" HOT-FIXED:COUNT-CH" ARM-ROW
   s" WORKLOAD:MIX66-F1"    s" HOT-F1:COUNT-CH" ARM-ROW
   s" WORKLOAD:MIX66-F2"    s" HOT-F2:COUNT-CH" ARM-ROW
   s" WORKLOAD:MIX66-F3"    s" HOT-F3:COUNT-CH" ARM-ROW
   s" WORKLOAD:MIX66-F4"    s" HOT-F4:COUNT-CH" ARM-ROW
   s" WORKLOAD:MIX33-OLD"   s" HOT-ENGINE:COUNT-CH" ARM-ROW
   s" WORKLOAD:MIX33-NEW"   s" HOT-CHAIN:COUNT-CH" ARM-ROW
   s" WORKLOAD:MIX33-CTL-A" s" HOT-FIXED:COUNT-CH" ARM-ROW
   s" WORKLOAD:MIX33-CTL-B" s" HOT-FIXED:COUNT-CH" ARM-ROW
   s" WORKLOAD:MIX33-F1"    s" HOT-F1:COUNT-CH" ARM-ROW
   s" WORKLOAD:MIX33-F2"    s" HOT-F2:COUNT-CH" ARM-ROW
   s" WORKLOAD:MIX33-F3"    s" HOT-F3:COUNT-CH" ARM-ROW
   s" WORKLOAD:MIX33-F4"    s" HOT-F4:COUNT-CH" ARM-ROW ;

: TERM-ARMS ( -- )
   s" WORKLOAD:TERM-OLD"    s" HOT-ENGINE:TERM-TAG" ARM-ROW
   s" WORKLOAD:TERM-NEW"    s" HOT-CHAIN:TERM-TAG" ARM-ROW
   s" WORKLOAD:TERM-CTL-A"  s" HOT-FIXED:TERM-TAG" ARM-ROW
   s" WORKLOAD:TERM-CTL-B"  s" HOT-FIXED:TERM-TAG" ARM-ROW
   s" WORKLOAD:TERM-F1"     s" HOT-F1:TERM-TAG" ARM-ROW
   s" WORKLOAD:TERM-F2"     s" HOT-F2:TERM-TAG" ARM-ROW
   s" WORKLOAD:TERM-F3"     s" HOT-F3:TERM-TAG" ARM-ROW
   s" WORKLOAD:TERM-F4"     s" HOT-F4:TERM-TAG" ARM-ROW ;

public

: ARM-TABLE ( -- )
   s" 3. HOW EACH ARM REACHES ITS SUBJECT" type cr
   RULE
   s" Read off the arm's own compiled code. Every arm has to reach ITS OWN" type cr
   s" column's word: an after-arm still entering the before-arm's record would" type cr
   s" measure one code generator twice and report a delta of nothing. The four" type cr
   s" F arms of each workload belong to its placement row, and each has to reach" type cr
   s" its OWN publication: two of them entering one record would measure a body" type cr
   s" against itself and hand the verdict a bar that is too small." type cr
   cr
   s" driver" 22 T. s" bytes" 7 TR. s" calls" 7 TR. s"    reaches" type cr
   SCAN-ARMS
   COUNT-ARMS
   MIX-ARMS
   TERM-ARMS
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
   s"   " type
   k CODEGEN-CLOCK:REAL? s" judged" s" null" YESNO
   cr ;

public

: ROW-TABLE ( -- )
   s" 4. THE WORKLOADS" type cr
   RULE
   s" Fastest run of each arm, in nanoseconds; the delta is what the new code" type cr
   s" generator saved. `spread` is how far apart that arm's fastest and slowest" type cr
   s" runs were. `woven` means the two arms' runs were threaded through each" type cr
   s" other; `split` means the workload compiles something and its arms had to" type cr
   s" be measured on either side of the migration. A `null` row ran the SAME" type cr
   s" program on both arms, so its delta is what this harness manufactures when" type cr
   s" nothing changed; a `judged` row is one the next section rules on. On a" type cr
   s" `-placement` row the two columns are not two arms but the FASTEST and the" type cr
   s" SLOWEST of five publications of one identical body, so its delta is the" type cr
   s" widest gap between any two of them." type cr
   cr
   s" WHAT COUNT IS, AND WHY THE MIX ROWS ARE BESIDE IT. The count driver makes" type cr
   s" one call per repetition into a word that scans the whole buffer itself, so" type cr
   s" essentially ALL of its time is inside the word the migration replaced. Its" type cr
   s" delta is therefore that word's own speed-up and not what a migration is" type cr
   s" worth to a program: it is the endpoint of a curve. The mix rows are two" type cr
   s" points inside that curve. Each makes three passes over the same buffer;" type cr
   s" mix66 sends two of the three through the migrated word and mix33 sends one," type cr
   s" and the passes they do not send go through a publication nothing migrates." type cr
   s" So the migration reaches exactly two thirds and one third of the old arm's" type cr
   s" work, and each row's delta should come out at that fraction of the count" type cr
   s" row's - which is the arithmetic every claim about this work rests on." type cr
   s" Note also what the migrated word is: lib/string.f's COUNT-CHAR has no" type cr
   s" caller under src/ at all. Its callers are tests and the codegen corpora. It" type cr
   s" is measured here for its SHAPE - the whole loop inside the callee - and not" type cr
   s" because the system spends its time in it." type cr
   cr
   s" row" 16 T. s" old ns" 12 TR. s" new ns" 12 TR.
   s" delta" 9 TR. s" old sprd" 9 TR. s" new sprd" 9 TR.
   s"   order   answers   role" type cr
   CODEGEN-CLOCK:ROWS 0 ?do i ROW. loop
   cr ;

private

\ ---- 5. the verdict ---------------------------------------------------------
\ The bar comes out of the recorded rows and is not named here. Each row carries
\ the family it belongs to and whether it is a real comparison or a null draw,
\ and the bar for a real row is the largest magnitude its own family's null draws
\ produced. Two things follow, and both are the point:
\
\ A BAR IS A MAXIMUM OVER SEVERAL DRAWS, NOT ONE MEASUREMENT. The confound the
\ null rows measure is not a small symmetric wobble: two byte-identical
\ publications of one body have come out two per cent apart on one run and
\ thirty-five per cent apart on another. A bar taken from a single draw of that
\ landed under a scan delta three runs in a row and printed REAL LOSS each time,
\ which is an artifact of the draw and not a finding about the code.
\
\ A MISSING BAR IS AN ERROR, NOT A ZERO. This section used to name its bar rows
\ by hand; a name that matched no row scored zero and every verdict beside it
\ read REAL. BAR-PERMILLE throws on a family with no null draw instead.
\
\ The within-arm SPREAD is deliberately not a bar. A spread says how far apart
\ one arm's own runs were, which is exactly what the fastest-run rule exists to
\ cut through: the scan rows here routinely spread by sixty per cent while their
\ fastest runs repeat to within a few. The spreads are printed in the row table
\ for a reader to judge the host by; they are not evidence about a delta.
: VERDICT ( n -- ) {: k:n :}
   k CODEGEN-CLOCK:NAME$ 16 T.
   k CODEGEN-CLOCK:DELTA-PERMILLE PCT.
   s"  saved, against a " type
   k CODEGEN-CLOCK:FAM$ CODEGEN-CLOCK:BAR-PERMILLE PCT.
   s"  bar - the largest of " type
   k CODEGEN-CLOCK:FAM$ CODEGEN-CLOCK:NULLS FMT:.INT
   s"  null rows" type
   k CODEGEN-CLOCK:OVER-BAR? if
      k CODEGEN-CLOCK:DELTA-PERMILLE 0 > if
         s"   - REAL" type
      else
         s"   - REAL LOSS" type
      then
   else
      s"   - NOT MEASURABLE" type
   then
   cr ;

public

: VERDICT-TABLE ( -- )
   s" 5. WHAT EACH WORKLOAD'S NUMBER MEANS" type cr
   RULE
   s" The bar a delta has to clear is the LARGEST delta this harness produced on" type cr
   s" the same workload when nothing changed at all: its control row, whose two" type cr
   s" arms are old code compiled either side of the migration, and its placement" type cr
   s" row, which times five publications of the identical subject against each" type cr
   s" other and reports the widest gap between any two of them. Five and not two," type cr
   s" because that confound is not a wobble around a centre: the publications of" type cr
   s" one body fall into a fast group and a slow group tens of per cent apart, so" type cr
   s" a bar taken from one named pair depends on which pair was named - and a bar" type cr
   s" taken that way reported a workload as a REAL LOSS three runs running with" type cr
   s" nothing whatever having slowed it down. A delta under the bar is not a" type cr
   s" small win; it is a win this measurement cannot see." type cr
   cr
   s" The compile-shaped row is judged the same way. Its arms straddle the" type cr
   s" migration and cannot be woven, so its null draws are pairs of consecutive" type cr
   s" batch sequences compiled before it, with nothing between them but the" type cr
   s" dictionary they grew - which is the confound its two arms are separated by." type cr
   cr
   s" AND ITS ARMS ARE NOW SEPARATED BY A MIGRATION THAT REACHED THE CHECKER." type cr
   s" Section 1 shows it: the checker's own fold was migrated and every call" type cr
   s" instruction that entered it was moved onto the chain's routine. If this row" type cr
   s" still reads NOT MEASURABLE, that is a fact about where a batch spends its" type cr
   s" time and not about whether the migration landed. It was measured: one batch" type cr
   s" enters that fold about twenty-four thousand times, and twenty-four thousand" type cr
   s" calls of it cost well under a tenth of one per cent of a batch through" type cr
   s" either code generator, while more than half of a batch scales with the SIZE" type cr
   s" of the dictionary - the engine's own linear name lookup, which is engine" type cr
   s" text and not a dictionary record, so no republication can reach it at all." type cr
   s" Dot habu-compile-shaped-cost-4e74a181 carries that measurement and the work" type cr
   s" behind it." type cr
   cr
   CODEGEN-CLOCK:ROWS 0 ?do
      i CODEGEN-CLOCK:REAL? if i VERDICT then
   loop
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
