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
\   4  THE WORKLOADS: each one's two arms, its delta, its paired-delta
\      interval, and beside it the null rows of its own family - the rows that
\      ran the same program on both arms and so measure what this harness
\      invents when nothing changed.
\
\ THERE IS NO VERDICT SECTION, AND THAT IS DELIBERATE. The report used to rule
\ REAL / REAL LOSS / NOT MEASURABLE per row, judging each delta against the
\ largest delta its family's null draws produced. Two to four null draws are
\ not a distribution: on unchanged trees the same workload ruled REAL LOSS
\ three runs in five (dot habu-pair-and-alternate-60b04c6a). Until the harness
\ is calibrated, this file prints the raw data - both columns, the fastest-run
\ delta, the per-round paired interval, and the null rows beside the real ones
\ - and no label stronger than the data. NOTHING HERE THROWS ON A NUMBER: a
\ report that failed when a row came out slow would be a gate, and a gate on a
\ timing fails for host load; the gate this work carries is
\ tools/codegen-workload-test.f, and every assertion in it is a fact about
\ compiled code.

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

: PAIR-COLS ( n -- ) {: k:n :}
   k CODEGEN-CLOCK:PAIRED? 0= if
      s" -" 9 TR. s" -" 9 TR. exit
   then
   k CODEGEN-CLOCK:DELTA-LO 9 PCT-COL
   k CODEGEN-CLOCK:DELTA-HI 9 PCT-COL ;

: ROW. ( n -- ) {: k:n :}
   k CODEGEN-CLOCK:NAME$ 16 T.
   k CODEGEN-CLOCK:OLD-NS 12 N.
   k CODEGEN-CLOCK:NEW-NS 12 N.
   k CODEGEN-CLOCK:DELTA-PERMILLE 9 PCT-COL
   k PAIR-COLS
   k CODEGEN-CLOCK:OLD-SPREAD 9 PCT-COL
   k CODEGEN-CLOCK:NEW-SPREAD 9 PCT-COL
   s"   " type
   k CODEGEN-CLOCK:INTERLEAVED? s" woven" s" split" YESNO
   s"   " type
   k CODEGEN-CLOCK:SAME-ANSWER? s" agree" s" DISAGREE" YESNO
   s"   " type
   k CODEGEN-CLOCK:REAL? s" subject" s" null" YESNO
   cr ;

public

: ROW-TABLE ( -- )
   s" 4. THE WORKLOADS" type cr
   RULE
   s" Fastest run of each arm, in nanoseconds; the delta is what the new code" type cr
   s" generator saved. `d-lo`/`d-hi` are the extremes of the PER-ROUND paired" type cr
   s" deltas on a woven row - each round's two runs compared inside the same" type cr
   s" pair of adjacent windows, the order alternating round by round - so an" type cr
   s" interval that straddles zero says the rounds did not agree on a" type cr
   s" direction, whatever the headline delta reads. `spread` is how far apart" type cr
   s" one arm's own fastest and slowest runs were. `woven` means the two arms'" type cr
   s" runs were threaded through each other; `split` means the workload" type cr
   s" compiles something and its arms had to be measured on either side of the" type cr
   s" migration. A `null` row ran the SAME program on both arms, so everything" type cr
   s" it reports is what this harness manufactures when nothing changed: read" type cr
   s" the subject rows against the null rows printed beside them - no verdict" type cr
   s" is derived, because two to four null draws are not a distribution. On a" type cr
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
   s" delta" 9 TR. s" d-lo" 9 TR. s" d-hi" 9 TR.
   s" old sprd" 9 TR. s" new sprd" 9 TR.
   s"   order   answers   role" type cr
   CODEGEN-CLOCK:ROWS 0 ?do i ROW. loop
   cr ;

private

: TITLE ( -- )
   s" habu code generator - end to end workload measurement" type cr
   s" =====================================================" type cr
   cr
   s" One process. A program is put into the dictionary twice - once compiled by" type cr
   s" the engine's emitter, once by the native chain - and the same workloads are" type cr
   s" run against both. Everything below is measured in this run; nothing is read" type cr
   s" from a committed file." type cr
   cr ;

public

: PRINT ( -- )
   TITLE
   ENGINE-TABLE
   SUBJECT-TABLE
   ARM-TABLE
   ROW-TABLE ;

;package
