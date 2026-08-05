\ codegen-compare-gaps.f - the distance from the chain to the reference, across
\ every corpus. One concern: which rows are furthest from what clang makes of
\ the same program.
\
\ WHY THIS OUTLIVES A PASS. The measurement store holds one corpus at a time -
\ each pass resets it - so a table of the widest gaps built inside a pass could
\ only ever rank a corpus against itself, and ten of thirteen rows is not a
\ priority list. Every corpus adds its rows here as it finishes, and the run
\ prints one ranking over all four at the end. That ranking is what the
\ optimisation lanes are aimed at.
\
\ TWO RANKINGS, BECAUSE THEY DISAGREE. The row the chain is furthest behind on
\ in nanoseconds is not the row it is furthest behind on in bytes: a loop clang
\ turned into a closed form is an enormous time gap and a small byte one, and a
\ body clang vectorised is the other way round. Both are printed, both ten deep,
\ and a lane picks from whichever it is meant to close.
\
\ WHAT A GAP IS. Bytes: the chain's machine code minus the reference's, exactly,
\ both read off the record or the symbol the code actually occupies. Time: the
\ chain's cost with its entry taken off minus the reference's cost with ITS
\ entry taken off - which is not the same call and is not assumed to be, which
\ is why each side subtracts its own floor before either is compared.
\
\ AND WHAT A GAP IS NOT: a finding. Parity with a production optimising compiler
\ is the goal being measured, not a gate; a run whose gaps grew is a run that
\ says so, and it is the committed chain baseline - not this table - that turns
\ a byte regression against OURSELVES into something the run exits non-zero on.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/fmt.f
require tools/codegen-compare-core.f
require tools/codegen-compare-ns.f

package CODEGEN-GAPS

private

64 constant GAP-MAX
CODEGEN-COMPARE:NAME-MAX constant NAME-MAX
10 constant TOP-N
28 constant NAME-COL
9 constant WIDE-COL

GAP-MAX NAME-MAX * BUFFER: NAME-BYTES
create NAME-LENS GAP-MAX cells allot
create CHAIN-SIZES GAP-MAX cells allot
create REF-SIZES GAP-MAX cells allot
create CHAIN-PICOS GAP-MAX cells allot
create REF-PICOS GAP-MAX cells allot

variable GAP-N

: SLOT ( ptr a n -- ptr a )
   cells + ;

: GAP-OK ( n -- n )
   dup 0 < over GAP-N @ >= or if E-CODEGEN-COMPARE-ROW throw then ;

: NAME-AT ( n -- ptr u8 )
   NAME-MAX * NAME-BYTES + ;

public

\ Cleared at the start of a run, because a ranking is over the corpora this run
\ measured and not over whatever the last one left behind.
: RESET ( -- )
   0 GAP-N ! ;

private

: NAME$ ( n -- ptr u8 n ) {: k:n :}
   k GAP-OK NAME-AT
   NAME-LENS k SLOT @ ;

: BYTE-GAP ( n -- n ) {: k:n :}
   k GAP-OK drop
   CHAIN-SIZES k SLOT @  REF-SIZES k SLOT @ - ;

: TIME-GAP ( n -- n ) {: k:n :}
   k GAP-OK drop
   CHAIN-PICOS k SLOT @  REF-PICOS k SLOT @ - ;

: CHAIN-SIZE ( n -- n ) {: k:n :}
   CHAIN-SIZES k GAP-OK SLOT @ ;

: REF-SIZE ( n -- n ) {: k:n :}
   REF-SIZES k GAP-OK SLOT @ ;

: CHAIN-PICO ( n -- n ) {: k:n :}
   CHAIN-PICOS k GAP-OK SLOT @ ;

: REF-PICO ( n -- n ) {: k:n :}
   REF-PICOS k GAP-OK SLOT @ ;

private

: RECORD ( ptr u8 n n n n n -- )
   {: a:ptr u:n cbytes:n rbytes:n cpicos:n rpicos:n :}
   GAP-N @ GAP-MAX >= if E-CODEGEN-COMPARE-CAP throw then
   u NAME-MAX > if E-CODEGEN-COMPARE-CAP throw then
   a  GAP-N @ NAME-AT  u STR-LEN BYTE-COPY-LEN
   u NAME-LENS GAP-N @ SLOT !
   cbytes CHAIN-SIZES GAP-N @ SLOT !
   rbytes REF-SIZES GAP-N @ SLOT !
   cpicos CHAIN-PICOS GAP-N @ SLOT !
   rpicos REF-PICOS GAP-N @ SLOT !
   GAP-N @ 1+ GAP-N ! ;

\ One old row of the pass just measured, taken up only when BOTH other columns
\ carry it: a row the chain cannot compile yet, or one with no reference twin,
\ has no gap to rank and is left out rather than ranked as a zero.
: TAKE-ROW ( n -- ) {: k:n :}
   CODEGEN-COMPARE:PATH-NEW k CODEGEN-COMPARE:NAME$
   CODEGEN-COMPARE:FIND-ROW {: j:n :}
   j 0 < if exit then
   CODEGEN-COMPARE:PATH-CLANG k CODEGEN-COMPARE:NAME$
   CODEGEN-COMPARE:FIND-ROW {: c:n :}
   c 0 < if exit then
   k CODEGEN-COMPARE:NAME$
   j CODEGEN-COMPARE:SIZE
   c CODEGEN-COMPARE:SIZE
   j CODEGEN-COMPARE:BODY-PICOS
   c CODEGEN-COMPARE:BODY-PICOS
   RECORD ;

public

\ Take up every row of the pass just measured. Called by the driver after a
\ corpus has been reported, while its store still holds it.
: TAKE ( -- )
   CODEGEN-COMPARE:ROWS 0 ?do
      i CODEGEN-COMPARE:PATH@ CODEGEN-COMPARE:PATH-OLD = if i TAKE-ROW then
   loop ;

private

\ ---- the two rankings --------------------------------------------------------
\ Selection by repeated maximum over a used-mark, rather than a sort, because
\ what is wanted is ten of sixty and the ranking is printed twice under two
\ different keys. Nothing is moved, so the two rankings read the same rows.

create USED GAP-MAX cells allot

: CLEAR-USED ( -- )
   GAP-MAX 0 ?do 0 USED i SLOT ! loop ;

: USED? ( n -- bool ) {: k:n :}
   USED k SLOT @ 0<> ;

: USE ( n -- ) {: k:n :}
   -1 USED k SLOT ! ;

\ Which unused row has the largest gap under this ranking, or -1. The ranking is
\ a number rather than a quotation because the checker will not execute a
\ routine fetched from untyped memory, and a two-armed selector is honest about
\ there being exactly two rankings.
0 constant BY-BYTES
1 constant BY-TIME

\ THE ORDER IS A FUNCTION OF THE MEASUREMENT, AND SAYING SO TOOK THREE KEYS.
\ A byte gap is exact, so a ranking by bytes is the same table on every run -
\ measured, ten runs in a row, identical. A time gap is a measurement, and the
\ rows below the first few sit within a nanosecond of each other, so ranking on
\ the raw picosecond gap reordered them on every run and made a priority list
\ look like news.
\
\ So the time ranking is taken at the resolution it is PRINTED at - whole
\ nanoseconds - and ties at that resolution are broken by the byte gap, which is
\ exact, and then by the order the row was measured in, which is fixed. Two rows
\ whose gaps differ by less than a nanosecond therefore keep a stable order
\ instead of trading places, and a swap in the printed table means a row really
\ did cross a nanosecond. What this does NOT claim is that the two rows differ:
\ within a nanosecond they do not, and the table's own note says so.
: NS-TRUNC ( n -- n ) {: picos:n :}
   picos 0 < if picos negate CODEGEN-COMPARE:PICOS-PER-NS / negate exit then
   picos CODEGEN-COMPARE:PICOS-PER-NS / ;

: RANK-KEY ( n n -- n ) {: k:n rank:n :}
   rank BY-TIME = if k TIME-GAP NS-TRUNC exit then
   k BYTE-GAP ;

\ What breaks a tie in the first key, and it is EXACT in both rankings. A time
\ tie is broken by the byte gap, which is a byte count. A byte tie is broken by
\ nothing at all - it falls straight through to measurement order - because the
\ only other number available is a timing, and letting a timing decide the byte
\ table would have made the exact ranking wobble with the host. It did: with the
\ time gap as the byte table's tiebreak, one run in ten came out in a different
\ order from the other nine, on a column where every number is exact.
: TIE-KEY ( n n -- n ) {: k:n rank:n :}
   rank BY-TIME = if k BYTE-GAP exit then
   0 ;

\ Does a come before b in this ranking? Larger gap first; then larger tie key;
\ then the row measured first, which is a fixed order and leaves nothing to
\ chance.
: PRECEDES? ( n n n -- bool ) {: a:n b:n rank:n :}
   a rank RANK-KEY  b rank RANK-KEY  {: ka:n kb:n :}
   ka kb <> if ka kb > exit then
   a rank TIE-KEY  b rank TIE-KEY  {: ta:n tb:n :}
   ta tb <> if ta tb > exit then
   a b < ;

: WORST ( n -- n ) {: rank:n :}
   -1
   GAP-N @ 0 ?do
      i USED? 0= if
         dup 0 < if
            drop i
         else
            dup i swap rank PRECEDES? if drop i then
         then
      then
   loop ;

: NUM-WIDTH ( n -- n )
   SB-RESET FMT:SB-INT SB$ nip ;

: PAD ( n -- )
   dup 0 <= if drop exit then
   0 ?do s"  " type loop ;

: PAD-LEFT ( n n -- ) {: value:n width:n :}
   width value NUM-WIDTH - PAD
   value FMT:.INT ;

: PAD-RIGHT ( ptr u8 n n -- ) {: width:n :}
   dup {: used:n :}
   type
   width used - PAD ;

: NS-COL ( n n -- ) {: picos:n width:n :}
   picos CODEGEN-NS:NS$ {: a:ptr u:n :}
   width u - PAD
   a u type ;

: TIME-HEAD ( -- )
   s" the ten largest chain-vs-clang gaps by TIME, worst first" type cr
   s" word                          chain ns   clang ns     gap ns" type cr
   s" ----------------------------  ---------  ---------  ---------" type cr ;

: TIME-ROW ( n -- ) {: k:n :}
   k NAME$ NAME-COL PAD-RIGHT
   2 PAD
   k CHAIN-PICO WIDE-COL NS-COL
   2 PAD
   k REF-PICO WIDE-COL NS-COL
   2 PAD
   k TIME-GAP WIDE-COL NS-COL
   cr ;

: BYTE-HEAD ( -- )
   s" the ten largest chain-vs-clang gaps by BYTES, worst first" type cr
   s" word                          chain byt  clang byt   gap byte" type cr
   s" ----------------------------  ---------  ---------  ---------" type cr ;

: BYTE-ROW ( n -- ) {: k:n :}
   k NAME$ NAME-COL PAD-RIGHT
   2 PAD
   k CHAIN-SIZE WIDE-COL PAD-LEFT
   2 PAD
   k REF-SIZE WIDE-COL PAD-LEFT
   2 PAD
   k BYTE-GAP WIDE-COL PAD-LEFT
   cr ;

\ The ten worst rows under one key. Fewer than ten rows prints all of them, and
\ a run that measured nothing prints nothing rather than a table of dashes.
: SAY-RANK ( n -- ) {: rank:n :}
   CLEAR-USED
   rank BY-TIME = if TIME-HEAD else BYTE-HEAD then
   TOP-N 0 ?do
      rank WORST {: k:n :}
      k 0 < if leave then
      k USE
      rank BY-TIME = if k TIME-ROW else k BYTE-ROW then
   loop ;

public

\ The whole ranking, printed once at the end of a run over every corpus that
\ was measured. Informational and named as such: parity with clang is the goal
\ being measured, so a gap is a priority and never a finding.
: SAY-TOP ( -- )
   cr
   GAP-N @ 0= if
      s" chain against clang: no row was measured on both columns, so there is" type
      s"  no ranking" type cr exit
   then
   s" CHAIN AGAINST CLANG - informational, and the priority list for the" type
   s"  optimisation lanes." type cr
   s" A positive gap is the chain behind the reference. Rows measured: " type
   GAP-N @ FMT:.U cr
   s" The byte ranking is exact. The time ranking is taken at whole nanoseconds" type cr
   s" and ties there are broken by the byte gap and then by measurement order, so" type cr
   s" the table is a function of the run; rows within a nanosecond of each other" type cr
   s" are not being claimed to differ." type cr
   cr
   BY-TIME SAY-RANK
   cr
   BY-BYTES SAY-RANK ;

;package
