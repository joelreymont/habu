\ tier-census-join.f - put two tools/tier-census.f reports side by side.
\
\ Run:
\     bin/hb --load tools/tier-census-join.f -- <tier0-report> <tier1-report>
\
\ Prints the corpus totals for each tier with the difference, one row per word,
\ and the words whose code grew and shrank most between the two.
\
\ The reports must come from the same corpus in the same order: the join walks
\ both row by row and refuses on the first name that disagrees. A table joined
\ silently out of step would read as a per-word result while comparing two
\ different words, which is the one failure that makes a census lie.
\
\ Storage is sized from the input, so a corpus of any size joins. Both reports
\ are read whole, each to its end, into one growing byte buffer; the
\ tier-0 names are offsets into that buffer rather than copies. The tier-0
\ report is scanned once to count its `W` rows, and the row tables are reserved
\ to that count before the rows are read. The mappings live for the run.
\
\ Ratios are printed in parts per thousand of the tier-0 value (`d/1000`), not
\ as a decimal: -234 is tier 1 at 23.4 percent below tier 0. An integer column
\ keeps the arithmetic exact and this tool free of the float formatter.
\
\ `sp0`/`sp1` are the frame traffic: stack-pointer-based 64-bit loads plus
\ stores, which is what a spill, a reload and a local access all cost.
\ `mov0`/`mov1` are register-to-register moves, the cost of a copy whose two
\ ends the allocator gave different registers; `mvk0`/`mvk1` are the keep-moves
\ in literal materialisation, including two per compact DATA address and three
\ per full absolute address.

require src/lib/string.f
require src/lib/argv.f
require src/lib/source.f

-7731 constant E-JOIN-SHAPE
-7732 constant E-JOIN-MISMATCH

package TIER-JOIN
private

12 constant TOP-N
$0A constant LF
$20 constant SP
$2D constant MINUS-C
48 constant ZERO-C
87 constant W-TAG                  \ 'W', a per-word row
84 constant T-TAG                  \ 'T', the totals row
6 constant METRICS                 \ bytes, bl, ldr-sp, str-sp, mov, movk
8 constant TOTALS                  \ words, bytes, instr, bl, ldr-sp, str-sp, mov, movk
32 constant NUM-CAP
$7FFFFFFFFFFFFFF constant NO-BOUND

DYNAMIC-BUFFER SRC u8               \ both reports, back to back
variable SRC-U
variable POS

DYNAMIC-BUFFER N-OFF n               \ a tier-0 name's offset in SRC, per row
DYNAMIC-BUFFER N-LEN n
DYNAMIC-BUFFER M0 n                  \ METRICS cells per row, tier 0
DYNAMIC-BUFFER M1 n
variable NWORDS
variable ROW

create TOT0 TOTALS cells allot
create TOT1 TOTALS cells allot

variable TOK-A
variable TOK-U

create NUM-BUF NUM-CAP allot
create NUM-DIG NUM-CAP allot
variable NUM-U
variable NUM-V
variable NUM-K

variable PICK
variable BEST
variable BOUND

: @N ( ptr n n -- n ) {: a:ptr ix :} a ix cells + @ ;
: !N ( n ptr n n -- ) {: v a:ptr ix :} v a ix cells + ! ;
: SLOT ( n n -- n ) {: ix k :} ix METRICS * k + ;

\ ---- reading a report -------------------------------------------------------

: AT ( n -- n ) SRC c@ ;

: SEPARATOR? ( n -- bool ) {: c :}
   c SP = c LF = or ;

: TOKEN-TAIL ( -- )
   begin POS @ SRC-U @ < while
      POS @ AT SEPARATOR? if POS @ TOK-A @ - TOK-U ! exit then
      POS @ 1 + POS !
   repeat
   POS @ TOK-A @ - TOK-U ! ;

\ Leading blanks are skipped; a line that is only a newline yields a zero-length
\ token, which every caller treats as "not a row I know".
: TOKEN ( -- )
   begin POS @ SRC-U @ < while
      POS @ AT SP <> if
         POS @ TOK-A !
         TOKEN-TAIL exit
      then
      POS @ 1 + POS !
   repeat
   POS @ TOK-A !  0 TOK-U ! ;

: TOK$ ( -- ptr u8 n ) TOK-A @ SRC TOK-U @ ;

: NUM ( -- n )
   TOKEN
   TOK-U @ 0= if E-JOIN-SHAPE throw then
   0 TOK-U @ 0 ?do
      TOK-A @ i + AT {: c :}
      c ZERO-C < c ZERO-C 9 + > or if E-JOIN-SHAPE throw then
      10 * c ZERO-C - +
   loop ;

: END-LINE ( -- )
   begin POS @ SRC-U @ < while
      POS @ 1 + POS !
      POS @ 1 - AT LF = if exit then
   repeat ;

\ Append one report to SRC, sized from the descriptor it is read through, and
\ point the reader at its first byte.
: ROOM ( n -- ptr u8 ) SRC-U @ + SRC-RESERVE SRC-U @ SRC ;

: LOAD-FILE ( ptr u8 n -- )
   SRC-U @ POS !
   [: ROOM ;] SOURCE:READ-WHOLE-FILE SRC-U @ + SRC-U ! ;

: TAG? ( n -- bool ) {: t :}
   TOK-U @ 1 <> if false exit then
   TOK-A @ AT t = ;

: READ-TOTALS ( ptr n -- ) {: t:ptr :}
   TOTALS 0 ?do NUM t i !N loop ;

\ ---- the first report defines the rows --------------------------------------

: KEEP-NAME ( -- )
   TOK-A @ NWORDS @ N-OFF !
   TOK-U @ NWORDS @ N-LEN ! ;

\ The instruction column is the byte column over four, so it is read and dropped
\ rather than stored twice.
: ROW0 ( -- )
   TOKEN KEEP-NAME
   NUM NWORDS @ 0 SLOT M0 !
   NUM drop
   NUM NWORDS @ 1 SLOT M0 !
   NUM NWORDS @ 2 SLOT M0 !
   NUM NWORDS @ 3 SLOT M0 !
   NUM NWORDS @ 4 SLOT M0 !
   NUM NWORDS @ 5 SLOT M0 !
   NWORDS @ 1 + NWORDS ! ;

\ The same tokenizer as the row pass, so the count and the rows cannot differ.
: COUNT-ROWS ( -- n )
   0
   begin POS @ SRC-U @ < while
      TOKEN
      W-TAG TAG? if 1 + then
      END-LINE
   repeat ;

: LOAD0 ( ptr u8 n -- )
   LOAD-FILE
   POS @ {: start :}
   COUNT-ROWS {: rows :}
   rows N-OFF-RESERVE  rows N-LEN-RESERVE
   rows METRICS * M0-RESERVE  rows METRICS * M1-RESERVE
   start POS !
   0 NWORDS !
   begin POS @ SRC-U @ < while
      TOKEN
      W-TAG TAG? if ROW0 then
      T-TAG TAG? if TOT0 READ-TOTALS then
      END-LINE
   repeat ;

\ ---- the second report fills the other half ---------------------------------

: NAME= ( n -- bool ) {: ix :}
   ix N-LEN @ TOK-U @ <> if STR-FALSE exit then
   ix N-OFF @ SRC TOK-U @ TOK$ STR= ;

: ROW1 ( -- )
   ROW @ NWORDS @ >= if E-JOIN-MISMATCH throw then
   TOKEN
   ROW @ NAME= 0= if E-JOIN-MISMATCH throw then
   NUM ROW @ 0 SLOT M1 !
   NUM drop
   NUM ROW @ 1 SLOT M1 !
   NUM ROW @ 2 SLOT M1 !
   NUM ROW @ 3 SLOT M1 !
   NUM ROW @ 4 SLOT M1 !
   NUM ROW @ 5 SLOT M1 !
   ROW @ 1 + ROW ! ;

: LOAD1 ( ptr u8 n -- )
   LOAD-FILE
   0 ROW !
   begin POS @ SRC-U @ < while
      TOKEN
      W-TAG TAG? if ROW1 then
      T-TAG TAG? if TOT1 READ-TOTALS then
      END-LINE
   repeat
   ROW @ NWORDS @ <> if E-JOIN-MISMATCH throw then ;

\ ---- output -----------------------------------------------------------------
\ The number formatter writes its own buffer instead of the shared builder: a
\ row is assembled in the builder, so a formatter that reset it would erase the
\ row it was called to extend.

: NUM-C ( n -- )
   NUM-BUF NUM-U @ + c!
   NUM-U @ 1 + NUM-U ! ;

: NUM-DIGITS ( -- )
   0 NUM-K !
   begin NUM-V @ 0 > while
      NUM-V @ 10 mod ZERO-C + NUM-DIG NUM-K @ + c!
      NUM-K @ 1 + NUM-K !
      NUM-V @ 10 / NUM-V !
   repeat
   begin NUM-K @ 0 > while
      NUM-K @ 1 - NUM-K !
      NUM-DIG NUM-K @ + c@ NUM-C
   repeat ;

: NUM$ ( n -- ptr u8 n )
   NUM-V !
   0 NUM-U !
   NUM-V @ 0= if ZERO-C NUM-C NUM-BUF NUM-U @ exit then
   NUM-V @ 0 < if MINUS-C NUM-C  0 NUM-V @ - NUM-V ! then
   NUM-DIGITS
   NUM-BUF NUM-U @ ;

: PAD ( n -- ) {: n :} n 0 ?do SP SB-APPEND-C loop ;

: RIGHT$ ( ptr u8 n n -- ) {: a:ptr u w :}
   w u > if w u - PAD then
   a u SB-APPEND ;

: LEFT$ ( ptr u8 n n -- ) {: a:ptr u w :}
   a u SB-APPEND
   w u > if w u - PAD then ;

: COL ( n n -- ) {: v w :} v NUM$ w RIGHT$ ;

\ Parts per thousand of the tier-0 value; 0 when there is nothing to compare to.
: PERMILLE ( n n -- n ) {: from to :}
   from 0= if 0 exit then
   to from - 1000 * from / ;

: SUM-HEAD ( -- )
   SB-RESET
   s" metric" 10 LEFT$
   s" tier0" 10 RIGHT$  s" tier1" 10 RIGHT$
   s" delta" 10 RIGHT$  s" d/1000" 10 RIGHT$
   SB$ type cr ;

: TOTAL-LINE ( ptr u8 n n -- ) {: lbl:ptr lu ix :}
   TOT0 ix @N {: a :}
   TOT1 ix @N {: b :}
   SB-RESET
   lbl lu 10 LEFT$
   a 10 COL  b 10 COL  b a - 10 COL  a b PERMILLE 10 COL
   SB$ type cr ;

: SUMMARY ( -- )
   SUM-HEAD
   s" words" 0 TOTAL-LINE
   s" bytes" 1 TOTAL-LINE
   s" instr" 2 TOTAL-LINE
   s" bl" 3 TOTAL-LINE
   s" ldr-sp" 4 TOTAL-LINE
   s" str-sp" 5 TOTAL-LINE
   s" mov" 6 TOTAL-LINE
   s" movk" 7 TOTAL-LINE ;

: NAME$ ( n -- ptr u8 n ) {: ix :} ix N-OFF @ SRC ix N-LEN @ ;

: WORD-HEAD ( -- )
   SB-RESET
   s" word" 34 LEFT$
   s" b0" 8 RIGHT$  s" b1" 8 RIGHT$  s" db" 8 RIGHT$  s" d/1000" 8 RIGHT$
   s" bl0" 6 RIGHT$ s" bl1" 6 RIGHT$
   s" sp0" 6 RIGHT$ s" sp1" 6 RIGHT$
   s" mov0" 6 RIGHT$ s" mov1" 6 RIGHT$
   s" mvk0" 6 RIGHT$ s" mvk1" 6 RIGHT$
   SB$ type cr ;

: WORD-LINE ( n -- ) {: ix :}
   ix 0 SLOT M0 @ {: a :}
   ix 0 SLOT M1 @ {: b :}
   SB-RESET
   ix NAME$ 34 LEFT$
   a 8 COL  b 8 COL  b a - 8 COL  a b PERMILLE 8 COL
   ix 1 SLOT M0 @ 6 COL  ix 1 SLOT M1 @ 6 COL
   ix 2 SLOT M0 @ ix 3 SLOT M0 @ + 6 COL
   ix 2 SLOT M1 @ ix 3 SLOT M1 @ + 6 COL
   ix 4 SLOT M0 @ 6 COL  ix 4 SLOT M1 @ 6 COL
   ix 5 SLOT M0 @ 6 COL  ix 5 SLOT M1 @ 6 COL
   SB$ type cr ;

: ALL-WORDS ( -- )
   WORD-HEAD
   NWORDS @ 0 ?do i WORD-LINE loop ;

\ TOP picks by repeated scan rather than by sorting: TOP-N is small and a sort
\ would need a second index array for no gain. Each round takes the largest
\ value strictly below the previous round's, so equal deltas do not repeat.
: SCAN-BEST ( n -- n ) {: sign :}
   -1 PICK !  0 BEST !
   NWORDS @ 0 ?do
      i 0 SLOT M1 @ i 0 SLOT M0 @ - sign * {: d :}
      d BEST @ > d BOUND @ < and if d BEST ! i PICK ! then
   loop
   PICK @ ;

: TOP ( ptr u8 n n -- ) {: lbl:ptr lu sign :}
   lbl lu type cr
   WORD-HEAD
   NO-BOUND BOUND !
   TOP-N 0 ?do
      sign SCAN-BEST {: ix :}
      ix 0 < if leave then
      ix WORD-LINE
      BEST @ BOUND !
   loop ;

public

: MAIN ( -- )
   s" bin/hb --load tools/tier-census-join.f -- <tier0-report> <tier1-report>"
   ARGV:USAGE!
   ARGV:PARSE
   2 2 ARGV:EXPECT-POS
   0 ARGV:POS$ LOAD0
   1 ARGV:POS$ LOAD1
   SUMMARY cr
   ALL-WORDS cr
   s" grew most (tier 1 larger)" 1 TOP cr
   s" shrank most (tier 1 smaller)" -1 TOP ;

;package

TIER-JOIN:MAIN
