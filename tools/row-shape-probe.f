\ row-shape-probe.f - how the checker's recorded rows are SHAPED, counted rather
\ than argued.
\
\ WHY THIS EXISTS. src/compiler/native/dict.f turns a recorded row into a glue
\ mask by walking the per-term slots, and it can only do that while the row's
\ TERM count and its CELL count agree. It used to answer a row where they
\ disagree "glued throughout", which is deliberately more than the truth - safe
\ only while the consumer's response to "glued" was a REFUSAL. The consumer now
\ SEGMENTS the vector into values, which is what a row-wise rename does, and an
\ over-answer there merges two values into one and moves the wrong cells with
\ every count still balancing. So the classification had to become exact-or-
\ not-known, and this walk is what says which shapes really occur.
\
\ So the question this walk answers is which shapes really occur: how many
\ recorded rows the slot walk can segment exactly, how many disagree on the two
\ counts with ONE term (where "the whole row is one value" is not an
\ approximation but the truth), and how many disagree with SEVERAL terms - the
\ only bucket where no exact answer is available and a consumer has to refuse.
\ It also counts the two shapes the walk itself has to handle: a row carrying two
\ multi-cell values NEXT TO each other, which is the case a per-cell glue bit
\ cannot tell from one wide value, and a row whose slots claim a run reaching
\ past its own end.
\
\ THE SWEEP SEES GLOBALS ONLY, AND THAT IS A PROPERTY OF THE QUESTION RATHER THAN
\ A HOLE. It walks the dictionary in its own scope - no package open - and asks
\ the checker about each record's bare spelling, which is exactly what an
\ unqualified reference resolves; a PRIVATE word of a package nothing has open
\ answers absent, the same way it answers absent for the chain. The rows that
\ interest this question most are package words - a generated constructor is
\ published as `FAMILY:NAME` - so every name after `--` is asked about
\ INDIVIDUALLY and reported with its two counts and its slots, which is how the
\ parametric shapes quoted in src/compiler/native/dict.f were measured.
\
\ Run: bin/hb --load tools/row-shape-probe.f
\      bin/hb --load lib/adt/option.f tools/row-shape-probe.f -- OPTION:SOME OPTION:NONE

require lib/prelude.f
require lib/errors.f
require lib/argv.f
require src/compiler/native/dict.f

package ROW-SHAPE-PROBE

private

variable N-SEEN        \ records asked
variable N-RETIRED
variable N-EFFECT      \ records the checker holds an effect for
variable N-ROWS        \ rows examined (two per effect)
variable N-EXACT       \ terms = cells: the slot walk segments the row exactly
variable N-WIDE-1      \ terms <> cells with ONE term: the whole row is one value
variable N-WIDE-N      \ terms <> cells with SEVERAL terms: no exact segmentation
variable N-UNSIZED     \ the checker declines to state the row's width
variable N-ADJACENT    \ rows holding two multi-cell values next to each other
variable N-PAST        \ rows whose slots claim a run reaching past the row's end
variable N-BUNDLED     \ rows holding at least one multi-cell value

\ One boundary, the shape tools/callable-arity-probe.f and the census both use:
\ the checker's query entry is name-stripped past the seal, so a checked caller
\ reaches it behind a declared signature.
TRUSTED: HAS-EFFECT? ( ptr u8 n -- bool )
   EFFECT-QUERY ;

32 constant LIST-MAX
variable N-LISTED

: LIST-RESET ( -- )   0 N-LISTED ! ;

: LIST-ONE ( ptr u8 n -- )
   N-LISTED @ LIST-MAX < if
      ."     " type cr
      N-LISTED @ 1+ N-LISTED !
   else 2drop then ;

\ ---- one row's shape ----------------------------------------------------------
\ The slot walk src/compiler/native/dict.f ROW-GLUE keeps, with the two facts it
\ throws away kept instead: where each value ENDS, so two values next to each
\ other are told apart from one twice as wide.
variable RS-I          \ term cursor, counted from the TOP as the checker numbers them
variable RS-S          \ that term's slot+1
variable RS-PREV       \ whether the value immediately above this term was multi-cell
variable RS-RUNS       \ multi-cell values found in this row
variable RS-ADJ        \ pairs of them lying next to each other
variable RS-PAST       \ a run claiming more terms than the row has

: RS-SLOT ( n bool -- n ) {: i:n din:bool :}
   din if i EFFECT-DIN-SLOT else i EFFECT-DOUT-SLOT then ;

: RS-RESET ( -- )
   0 RS-I !  0 RS-S !  0 RS-PREV !  0 RS-RUNS !  0 RS-ADJ !  0 RS-PAST ! ;

: RS-SCALAR ( -- )
   0 RS-PREV !
   RS-I @ 1+ RS-I ! ;

: RS-RUN ( -- )
   RS-PREV @ 0<> if RS-ADJ @ 1+ RS-ADJ ! then
   1 RS-PREV !
   RS-RUNS @ 1+ RS-RUNS !
   RS-I @ RS-S @ + RS-I ! ;

: RS-STEP ( n bool -- ) {: terms:n din:bool :}
   RS-I @ din RS-SLOT RS-S !
   RS-S @ 2 < if RS-SCALAR exit then
   RS-I @ RS-S @ + terms > if
      1 RS-PAST !
      terms RS-I !
      exit
   then
   RS-RUN ;

: RS-WALK ( n bool -- ) {: terms:n din:bool :}
   RS-RESET
   begin RS-I @ terms < while
      terms din RS-STEP
   repeat ;

\ ---- classifying one row ------------------------------------------------------
\ CELLS-NONE is the checker's own spelling for a width it declines to state, and
\ a row that cannot be sized cannot be walked either: the term-to-cell
\ correspondence the walk rests on is exactly the two counts agreeing.
: ROW-UNSIZED? ( n -- bool ) {: cells:n :}
   cells 0 < ;

: ROW-TALLY ( ptr u8 n n n bool -- )
   {: a u:n terms:n cells:n din:bool :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   N-ROWS @ 1+ N-ROWS !
   cells ROW-UNSIZED? if  N-UNSIZED @ 1+ N-UNSIZED !  exit  then
   terms cells <> if
      terms 1 = if N-WIDE-1 @ 1+ N-WIDE-1 !
      else
         N-WIDE-N @ 1+ N-WIDE-N !
         a u LIST-ONE
      then
      exit
   then
   N-EXACT @ 1+ N-EXACT !
   terms din RS-WALK
   RS-RUNS @ 0<> if N-BUNDLED @ 1+ N-BUNDLED ! then
   RS-ADJ @ 0<> if N-ADJACENT @ 1+ N-ADJACENT ! then
   RS-PAST @ 0<> if N-PAST @ 1+ N-PAST ! then ;

: TALLY ( ptr u8 n -- )
   {: a u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   N-SEEN @ 1+ N-SEEN !
   a u HAS-EFFECT? 0= if exit then
   N-EFFECT @ 1+ N-EFFECT !
   a u  EFFECT-DIN-N   EFFECT-DIN-CELLS   true   ROW-TALLY
   a u  EFFECT-DOUT-N  EFFECT-DOUT-CELLS  false  ROW-TALLY ;

\ ---- one NAMED row, reported rather than counted -------------------------------
\ The two counts and every slot, so a reader can check a shape this file's prose
\ claims instead of believing it. A name the checker holds no effect for in THIS
\ scope says so rather than being counted as anything.
: SHOW-ROW ( ptr u8 n bool -- ) {: a u:n din:bool :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   din if ."   din  t=" EFFECT-DIN-N . ." c=" EFFECT-DIN-CELLS .
   else ."   dout t=" EFFECT-DOUT-N . ." c=" EFFECT-DOUT-CELLS . then
   ." slots:"
   din if EFFECT-DIN-N else EFFECT-DOUT-N then 0 ?do
      i din RS-SLOT .
   loop
   cr ;

: SHOW-NAME ( ptr u8 n -- )
   {: a u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   a u type cr
   a u HAS-EFFECT? 0= if ."   (no effect in this scope)" cr exit then
   a u true SHOW-ROW
   a u false SHOW-ROW ;

public

: RUN ( -- )
   0 N-SEEN !  0 N-RETIRED !  0 N-EFFECT !  0 N-ROWS !
   0 N-EXACT !  0 N-WIDE-1 !  0 N-WIDE-N !  0 N-UNSIZED !
   0 N-ADJACENT !  0 N-PAST !  0 N-BUNDLED !
   ." -- rows whose terms and cells disagree with SEVERAL terms ----" cr
   LIST-RESET
   0 begin dup ndict@ < while
      dup XREF-REC {: rec:ptr :}
      rec XREF-RETIRED? if N-RETIRED @ 1+ N-RETIRED !
      else rec XREF-NAME$ TALLY then
      1+
   repeat drop
   ." -- counts ---------------------------------------------------" cr
   ."   records            " ndict@ . cr
   ."   retired            " N-RETIRED @ . cr
   ."   asked              " N-SEEN @ . cr
   ."   effects held       " N-EFFECT @ . cr
   ."   rows examined      " N-ROWS @ . cr
   ."     exact (t = c)    " N-EXACT @ . cr
   ."     wide, one term   " N-WIDE-1 @ . cr
   ."     wide, many terms " N-WIDE-N @ . cr
   ."     unsizeable       " N-UNSIZED @ . cr
   ."   of the exact rows:" cr
   ."     hold a bundle    " N-BUNDLED @ . cr
   ."     two adjacent     " N-ADJACENT @ . cr
   ."     run past the row " N-PAST @ . cr
   ARGV:POS# 0= if exit then
   ." -- the rows named on the command line -----------------------" cr
   0 begin dup ARGV:POS# < while
      dup ARGV:POS$ SHOW-NAME
      1+
   repeat drop ;

;package

s" tools/row-shape-probe.f [-- NAME ...]" ARGV:USAGE!
ARGV:PARSE
ROW-SHAPE-PROBE:RUN
