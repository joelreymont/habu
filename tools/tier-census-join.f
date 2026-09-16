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
\ Ratios are printed in parts per thousand of the tier-0 value (`d/1000`), not
\ as a decimal: -234 is tier 1 at 23.4 percent below tier 0. An integer column
\ keeps the arithmetic exact and this tool free of the float formatter.
\
\ `sp0`/`sp1` are the frame traffic: stack-pointer-based 64-bit loads plus
\ stores, which is what a spill, a reload and a local access all cost.
\ `mov0`/`mov1` are register-to-register moves, the cost of a copy whose two
\ ends the allocator gave different registers; `mvk0`/`mvk1` are the keep-moves
\ of the four-instruction stencil every relocatable address is pinned to.

require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/argv.f

-7730 constant E-JOIN-CAPACITY
-7731 constant E-JOIN-SHAPE
-7732 constant E-JOIN-MISMATCH

package TIER-JOIN
private

8192 constant MAX-WORDS
$40000 constant NAME-CAP
$400000 constant SRC-CAP
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

create SRC SRC-CAP allot
variable SRC-U
variable POS

create NAMES NAME-CAP allot
variable NAME-FILL
create N-OFF MAX-WORDS cells allot
create N-LEN MAX-WORDS cells allot

create M0 MAX-WORDS METRICS * cells allot
create M1 MAX-WORDS METRICS * cells allot
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
: M@ ( ptr n n n -- n ) {: m:ptr ix k :} m ix METRICS * k + @N ;
: M! ( n ptr n n n -- ) {: v m:ptr ix k :} v m ix METRICS * k + !N ;

\ ---- reading a report -------------------------------------------------------

: AT ( n -- n ) {: off :} SRC off + c@ ;

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

: TOK$ ( -- ptr u8 n ) SRC TOK-A @ + TOK-U @ ;

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

: LOAD-FILE ( ptr u8 n -- )
   SRC SRC-CAP READ-ALL {: got :}
   got 0 < if E-JOIN-SHAPE throw then
   got SRC-CAP >= if E-JOIN-CAPACITY throw then
   got SRC-U !
   0 POS ! ;

: TAG? ( n -- bool ) {: t :}
   TOK-U @ 1 = TOK-A @ AT t = and ;

: READ-TOTALS ( ptr n -- ) {: t:ptr :}
   TOTALS 0 ?do NUM t i !N loop ;

\ ---- the first report defines the rows --------------------------------------

: KEEP-NAME ( -- )
   NAME-FILL @ TOK-U @ + NAME-CAP > if E-JOIN-CAPACITY throw then
   NAME-FILL @ N-OFF NWORDS @ !N
   TOK-U @ N-LEN NWORDS @ !N
   TOK-U @ 0 ?do TOK-A @ i + AT NAMES NAME-FILL @ + i + c! loop
   NAME-FILL @ TOK-U @ + NAME-FILL ! ;

\ The instruction column is the byte column over four, so it is read and dropped
\ rather than stored twice.
: ROW0 ( -- )
   NWORDS @ MAX-WORDS >= if E-JOIN-CAPACITY throw then
   TOKEN KEEP-NAME
   NUM M0 NWORDS @ 0 M!
   NUM drop
   NUM M0 NWORDS @ 1 M!
   NUM M0 NWORDS @ 2 M!
   NUM M0 NWORDS @ 3 M!
   NUM M0 NWORDS @ 4 M!
   NUM M0 NWORDS @ 5 M!
   NWORDS @ 1 + NWORDS ! ;

: LOAD0 ( ptr u8 n -- )
   LOAD-FILE
   0 NWORDS !  0 NAME-FILL !
   begin POS @ SRC-U @ < while
      TOKEN
      W-TAG TAG? if ROW0 then
      T-TAG TAG? if TOT0 READ-TOTALS then
      END-LINE
   repeat ;

\ ---- the second report fills the other half ---------------------------------

: NAME= ( n -- bool ) {: ix :}
   N-LEN ix @N TOK-U @ <> if STR-FALSE exit then
   NAMES N-OFF ix @N + TOK-U @ TOK$ STR= ;

: ROW1 ( -- )
   ROW @ NWORDS @ >= if E-JOIN-MISMATCH throw then
   TOKEN
   ROW @ NAME= 0= if E-JOIN-MISMATCH throw then
   NUM M1 ROW @ 0 M!
   NUM drop
   NUM M1 ROW @ 1 M!
   NUM M1 ROW @ 2 M!
   NUM M1 ROW @ 3 M!
   NUM M1 ROW @ 4 M!
   NUM M1 ROW @ 5 M!
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
   NUM-U @ NUM-CAP >= if E-JOIN-CAPACITY throw then
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

: NAME$ ( n -- ptr u8 n ) {: ix :} NAMES N-OFF ix @N + N-LEN ix @N ;

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
   M0 ix 0 M@ {: a :}
   M1 ix 0 M@ {: b :}
   SB-RESET
   ix NAME$ 34 LEFT$
   a 8 COL  b 8 COL  b a - 8 COL  a b PERMILLE 8 COL
   M0 ix 1 M@ 6 COL  M1 ix 1 M@ 6 COL
   M0 ix 2 M@ M0 ix 3 M@ + 6 COL
   M1 ix 2 M@ M1 ix 3 M@ + 6 COL
   M0 ix 4 M@ 6 COL  M1 ix 4 M@ 6 COL
   M0 ix 5 M@ 6 COL  M1 ix 5 M@ 6 COL
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
      M1 i 0 M@ M0 i 0 M@ - sign * {: d :}
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
