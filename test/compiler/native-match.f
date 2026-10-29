\ native-match.f - production MATCH, case, and construct compilation.

require test/compiler/native-eval-fixture.f
require src/lib/errors.f
require src/lib/string.f
require src/lib/le.f
require src/lib/test.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/adt/option.f
require src/compiler/native/compiler.f
require src/compiler/native/branch.f
require src/compiler/native/dict.f
require src/compiler/native/trap.f
require src/habu/code-bytes.f
require src/arch/x86-64/disasm.f
require test/compiler/native-match-layout.f

package NMX
private

4 constant INSN-BYTES

public

\ A payload-free enum: one cell, so its bundle carries no glue.
ENUM hue
   red
   green
   blue
;ENUM

\ A sum with payloads of three different widths in one family: the widest
\ variant fixes how many cells every value of it occupies, so the narrower two
\ are padded up to it and each arm drops a different number of pads.
SUMTYPE box
   VARIANT nil ;VARIANT
   VARIANT one n ;VARIANT
   VARIANT two n n ;VARIANT
;SUMTYPE

\ pt is two cells and one value, where box's widest variant is two cells and two
\ values: the arm's glue rule tells the two apart, and the pair of cases below
\ binds it.
PRODUCT pt
   FIELD x n
   FIELD y n
;PRODUCT

SUMTYPE holder
   VARIANT empty ;VARIANT
   VARIANT full pt ;VARIANT
;SUMTYPE

\ Three cells, which box and holder do not reach: a third cell makes an arm's
\ drop count and keep count two numbers a fixture can tell apart. trio carries
\ three independent cells and hold3 three cells that are one value, the pair box
\ and holder draw one cell narrower.
PRODUCT pt3
   FIELD x n
   FIELD y n
   FIELD z n
;PRODUCT

SUMTYPE trio
   VARIANT t0 ;VARIANT
   VARIANT t1 n ;VARIANT
   VARIANT t3 n n n ;VARIANT
;SUMTYPE

SUMTYPE hold3
   VARIANT empty3 ;VARIANT
   VARIANT full3 pt3 ;VARIANT
;SUMTYPE

\ A parametric family this package owns, for the `construct` half of a wide
\ instantiation: `construct option none` cannot be spelled outside option's
\ declaring package. opt2 matches option (one parameter, an empty variant and a
\ one-field one), so `OPTION:NONE`, a call, and `construct opt2 n2`, pushed
\ inline, are held against each other.
ENUM opt2<a>
   VARIANT n2 ;VARIANT
   VARIANT s2 FIELD value a ;VARIANT
;ENUM

\ A wide variant that still carries a payload: at grow<pt> the instantiation
\ reserves four payload slots where the declaration reserves two, so g1 arrives
\ with a two-cell payload and needs a cell added between it and its declared
\ pad. This shape fixes where added cells go.
SUMTYPE grow<a>
   VARIANT g1 a ;VARIANT
   VARIANT g2 a a ;VARIANT
;SUMTYPE

\ Two parameters instantiated to different widths, the shape of
\ src/lib/process.f RUN-CAPTURE's `result<pcap:captured,pcap:failed>` (a
\ two-cell ok, a three-cell err): ok needs a cell added and err none. The chain
\ refused it with -8503 (measured).
SUMTYPE pair<a,b>
   VARIANT lo a ;VARIANT
   VARIANT hi b ;VARIANT
;SUMTYPE

\ The widest declared variant is not the widest instantiated one: at narrow<pt3>
\ the instantiation reserves three slots where the declaration reserves two, and
\ p1, declaring one pad, instantiates none. A lowering that only adds cells
\ cannot correct that, so the checker refuses the construction
\ (src/core/type-family.f TFC-XPAD-NARROW-REJECT): the negative of every case
\ below.
SUMTYPE narrow<a>
   VARIANT w2 n n ;VARIANT
   VARIANT p1 a ;VARIANT
;SUMTYPE

\ One variant lets the growth cases put many independent dispatch facts before
\ a widened construction, so losing a suffix cannot hide behind one arm count.
ENUM sol ov ;ENUM

\ Four arms, which is the shape the four-armed selector cost was measured on.
ENUM quad
   q0 q1 q2 q3
;ENUM

\ Seven arms, and sixteen.
ENUM step
   p0 p1 p2 p3 p4 p5 p6
;ENUM

ENUM wide
   w0 w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11 w12 w13 w14 w15
;ENUM

\ Seventeen arms need the selector to grow past sixteen.
ENUM over
   v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16
;ENUM

private

: E-HUE ( hue -- n )
   MATCH hue
      red OF 10 ENDOF
      green OF 20 ENDOF
      blue OF 30 ENDOF
   ;MATCH ;

: E-BOX ( box -- n )
   MATCH box
      nil OF 0 ENDOF
      one OF ENDOF
      two OF + ENDOF
   ;MATCH ;

: E-UNW ( n option<n> -- n )
   MATCH option
      none OF ENDOF
      some OF nip ENDOF
   ;MATCH ;

: E-SWAPPED ( box -- n )
   MATCH box
      nil OF 0 ENDOF
      one OF ENDOF
      two OF swap - ENDOF
   ;MATCH ;

: E-HOLD ( n holder -- n )
   MATCH holder
      empty OF ENDOF
      full OF NMX-PT:UNMAKE + + ENDOF
   ;MATCH ;

\ The payload discarded instead of taken apart: two cells and one value, so
\ `drop` takes both, a row-wise rename inside an arm. The chain refused this
\ body while a rename counted cells.
: E-DROPPED ( n holder -- n )
   MATCH holder
      empty OF ENDOF
      full OF drop ENDOF
   ;MATCH ;

: E-QUAD ( quad -- n )
   MATCH quad
      q0 OF 1 ENDOF
      q1 OF 2 ENDOF
      q2 OF 3 ENDOF
      q3 OF 4 ENDOF
   ;MATCH ;

: E-STEP ( step -- n )
   MATCH step
      p0 OF 100 ENDOF   p1 OF 101 ENDOF   p2 OF 102 ENDOF
      p3 OF 103 ENDOF   p4 OF 104 ENDOF   p5 OF 105 ENDOF
      p6 OF 106 ENDOF
   ;MATCH ;

: E-WIDE ( wide -- n )
   MATCH wide
      w0 OF 200 ENDOF   w1 OF 201 ENDOF   w2 OF 202 ENDOF
      w3 OF 203 ENDOF   w4 OF 204 ENDOF   w5 OF 205 ENDOF
      w6 OF 206 ENDOF   w7 OF 207 ENDOF   w8 OF 208 ENDOF
      w9 OF 209 ENDOF   w10 OF 210 ENDOF  w11 OF 211 ENDOF
      w12 OF 212 ENDOF  w13 OF 213 ENDOF  w14 OF 214 ENDOF
      w15 OF 215 ENDOF
   ;MATCH ;

: E-CASE ( n -- n )
   case
      1 of 10 endof
      2 of 20 endof
      99 swap
   endcase ;

: E-MK ( n -- box )
   construct box one ;

: E-MK2 ( n n -- box )
   construct box two ;

: E-MK0 ( -- box )
   construct box nil ;

\ A dead arm: the path ends at the `throw`, so it hands the join nothing and the
\ dispatch's other arms state the whole of what the join takes.
: E-DEAD ( hue -- n )
   MATCH hue
      red OF 1 ENDOF
      green OF E-A-EMPTY throw ENDOF
      blue OF 3 ENDOF
   ;MATCH ;

\ `option<a>` reserves one payload slot and `option<pt>` needs two, so its value
\ is three cells where the declaration says two. The width depends on a resolved
\ type term, so the checker files the instantiated bundle width and each arm's
\ pad count under the tokens that publish them. The two payload cells carry
\ distinct odd weights: a commutative combination would answer the same
\ whichever cell came back where. A bare `OPTION:NONE` is `option<a>` and
\ grounds to nothing; a maker whose declared output names the instantiation
\ states it.
: E-MKI ( n -- option<pt> )
   dup 0 > if  dup 3 *  swap 5 *  NMX-PT:MAKE OPTION:SOME  else  drop OPTION:NONE  then ;

: E-INST ( option<pt> -- n )
   MATCH option
      none OF 0 ENDOF
      some OF NMX-PT:UNMAKE 7 * swap 11 * + ENDOF
   ;MATCH ;

\ The cheapest scrutinee, for the ceiling case alone: its name is spent thirteen
\ times in a source that must overflow the checker's table while staying inside
\ the recorder's text cap.
: MS ( -- sol ) construct sol ov ;

\ The same one cell wider, so the arm's drop count and its keep count are two
\ different numbers: `option<pt3>` is four cells, its `some` arm drops the tag
\ alone and keeps three, and its `none` arm drops all four.
: E-MKI3 ( n -- option<pt3> )
   dup 0 > if  dup 3 *  over 5 *  rot 7 *  NMX-PT3:MAKE OPTION:SOME  else  drop OPTION:NONE  then ;

: E-INST3 ( option<pt3> -- n )
   MATCH option
      none OF 0 ENDOF
      some OF NMX-PT3:UNMAKE 5 * swap 11 * + swap 17 * + ENDOF
   ;MATCH ;

\ Two dispatches of different instantiated widths in one body: both name
\ `option`, one pops four cells and the other three. The widths are filed under
\ the publishing token; a store keyed on the family, the definition or the form
\ order would drop the wrong cells with every count agreeing.
: E-TWOW ( n -- n )
   dup E-MKI3 MATCH option
      none OF 0 ENDOF
      some OF NMX-PT3:UNMAKE 5 * swap 11 * + swap 17 * + ENDOF
   ;MATCH
   swap E-MKI MATCH option
      none OF 0 ENDOF
      some OF NMX-PT:UNMAKE 7 * swap 13 * + ENDOF
   ;MATCH
   + ;

\ A string literal before a dispatch: a literal is one token reported through
\ its own event, so a report path that did not step the ordinal would file this
\ form's width under the token before it, which publishes nothing.
: E-STRINST ( n -- n )
   s" MATCH option some OF ;MATCH" 2drop
   E-MKI MATCH option
      none OF 0 ENDOF
      some OF NMX-PT:UNMAKE 7 * swap 11 * + ENDOF
   ;MATCH ;

\ Arms of three different widths joining, each cell weighted so an exchange
\ shows in the answer.
: E-TRIO ( trio -- n )
   MATCH trio
      t0 OF 0 ENDOF
      t1 OF 3 * ENDOF
      t3 OF 5 * swap 11 * + swap 17 * + ENDOF
   ;MATCH ;

\ A payload that crosses a nested `if` inside its arm: the cells are live at the
\ branch, at the join, and after it, so an arm that handed the inner structure
\ the wrong number of them would disagree on one side only.
: E-ARMIF ( trio -- n )
   MATCH trio
      t0 OF 0 ENDOF
      t1 OF dup 0 > if 3 * else 5 * then ENDOF
      t3 OF
         over 0 > if  7 *  else  11 *  then
         swap 13 * +  swap 17 * + ENDOF
   ;MATCH ;

\ And one that crosses a counted loop inside its arm. The trip count is a
\ constant, so the loop terminates whatever the payload is; the payload's second
\ cell is read on every turn, which is what makes it travel the back edge.
: E-ARMLOOP ( trio -- n )
   MATCH trio
      t0 OF 0 ENDOF
      t1 OF 3 * ENDOF
      t3 OF  3 0 ?do  over i *  +  loop  nip  swap 5 * + ENDOF
   ;MATCH ;

\ Three cells that are one value, which is `holder` one cell wider: the arm
\ keeps a bundle rather than three independent cells, and the glue it puts back
\ is what stops a rename from taking a `pt3` apart.
: E-HOLD3 ( hold3 -- n )
   MATCH hold3
      empty3 OF 0 ENDOF
      full3 OF NMX-PT3:UNMAKE 5 * swap 11 * + swap 17 * + ENDOF
   ;MATCH ;

\ Building a value of a wide instantiation. A construction pushes what the
\ family declares (the generated constructor is compiled once; `construct` reads
\ the declared pads from the registry), so at `option<pt>` both spellings are
\ one zero cell short. The checker files the difference under the construction's
\ token and the chain adds the cells at the site, as native lowering does. The
\ two arms of each maker need different numbers (`some` at `option<pt>` nothing,
\ `none` one cell), so a store keyed per family or per word would hand one arm
\ the other's number, and the two errors cancel in any count that sums the arms.
: E-MKC ( n -- opt2<pt> )
   dup 0 > if  dup 3 *  swap 5 *  NMX-PT:MAKE construct opt2 s2  else  drop construct opt2 n2  then ;

: E-RDC ( opt2<pt> -- n )
   MATCH opt2
      n2 OF 0 ENDOF
      s2 OF NMX-PT:UNMAKE 7 * swap 11 * + ENDOF
   ;MATCH ;

\ A record reader's next-line shape: dispatch over `option<pt>` and build
\ `option<pt3>` in both arms, where `some` needs nothing and `none` two cells. A
\ `none` arm constructing one cell short made the chain refuse the join (-8503,
\ measured).
: E-RELAY ( option<pt> -- option<pt3> )
   MATCH option
      none OF OPTION:NONE ENDOF
      some OF NMX-PT:UNMAKE over 3 * NMX-PT3:MAKE OPTION:SOME ENDOF
   ;MATCH ;

\ A wide construction the loop carries round its back edge. The value the body
\ leaves is the value the next turn takes apart, so its pads travel the edge and
\ the join at the top of the loop is where a bundle one cell short would meet one
\ of the right width. The trip count is a constant, so the loop terminates
\ whatever the payload is, and the payload still changes on every turn.
: E-LOOPC ( n -- option<pt> )
   dup 0 > if  dup 3 *  swap 5 *  NMX-PT:MAKE OPTION:SOME  else  drop OPTION:NONE  then
   3 0 ?do
      E-INST i +
      dup 0 > if  dup 3 *  swap 5 *  NMX-PT:MAKE OPTION:SOME  else  drop OPTION:NONE  then
   loop ;

\ A string literal before a construction, for E-STRINST's reason: without the
\ ordinal step this construction would be one cell short.
: E-STRCON ( n -- option<pt> )
   s" OPTION:NONE construct opt2 n2" 2drop
   dup 0 > if  dup 3 *  swap 5 *  NMX-PT:MAKE OPTION:SOME  else  drop OPTION:NONE  then ;

\ Two constructions of different instantiated widths in one body, of two
\ families: the checker resolves a construction's arguments from the first
\ bundle of that family in the declared output, so two `option`s would both take
\ the first one's width, and it refuses the body (measured). `option<pt>` adds
\ one cell and `opt2<pt3>` two.
: E-TWOC ( -- option<pt> opt2<pt3> )
   OPTION:NONE  construct opt2 n2 ;

: E-RDC3 ( opt2<pt3> -- n )
   MATCH opt2
      n2 OF 0 ENDOF
      s2 OF NMX-PT3:UNMAKE 5 * swap 11 * + swap 17 * + ENDOF
   ;MATCH ;

\ A construction with a payload that still needs cells added: g1 at grow<pt>
\ carries a two-cell payload and the instantiation reserves two more slots, so a
\ cell goes between the payload and the declared pad. Every other construction
\ here adds cells for an empty variant; this pair pins that they go above the
\ payload.
: E-MKG ( n -- grow<pt> )
   dup 3 * swap 5 * NMX-PT:MAKE NMX-GROW:G1 ;

: E-MKGC ( n -- grow<pt> )
   dup 3 * swap 5 * NMX-PT:MAKE construct grow g1 ;

: E-MKGC2 ( n -- grow<pt> )
   {: k:n :}
   k 3 * k 5 * NMX-PT:MAKE
   k 7 * k 11 * NMX-PT:MAKE construct grow g2 ;

: E-RDG ( grow<pt> -- n )
   MATCH grow
      g1 OF NMX-PT:UNMAKE 7 * swap 11 * + ENDOF
      g2 OF NMX-PT:UNMAKE 13 * swap 17 * + >r NMX-PT:UNMAKE 19 * swap 23 * + r> + ENDOF
   ;MATCH ;

\ The two parameters of one family instantiated to different widths: both arms
\ carry a payload and only one needs a cell added, so a number taken from the
\ family rather than the arm's own token is wrong for one of them.
: E-MKP ( n -- pair<pt,pt3> )
   dup 0 > if  dup 3 * swap 5 * NMX-PT:MAKE NMX-PAIR:LO
          else  dup 3 * over 5 * rot 7 * NMX-PT3:MAKE NMX-PAIR:HI  then ;

: E-RDP ( pair<pt,pt3> -- n )
   MATCH pair
      lo OF NMX-PT:UNMAKE 7 * swap 11 * + ENDOF
      hi OF NMX-PT3:UNMAKE 5 * swap 11 * + swap 17 * + ENDOF
   ;MATCH ;

\ The two answers are combined through the return stack because the value left
\ under the one just read is a bundle, and a rename over a bundle is refused.
: E-RDTWO ( option<pt> opt2<pt3> -- n )
   E-RDC3 >r E-INST r> 3 * + ;

\ The family's own name inside a comment, and inside a string literal. Neither is
\ a token of the dispatch grammar; both bodies must compile and answer.
: E-CMT ( hue -- n )
   MATCH hue
      red OF 1 ENDOF
      green OF ( hue blue OF ENDOF ;MATCH ) 2 ENDOF
      blue OF 3 ENDOF
   ;MATCH ;

: E-STR ( hue -- n )
   MATCH hue
      red OF s" MATCH hue blue OF ENDOF ;MATCH" 2drop 1 ENDOF
      green OF 2 ENDOF
      blue OF 3 ENDOF
   ;MATCH ;


: EV-RC ( ptr u8 n -- n )
   NATIVE-EVAL:DEFINE-RC ;

: TRY ( ptr u8 n -- n )
   NELAB:REFUSED-RESET
   EV-RC ;

: OVER$ ( -- ptr u8 n )
   s" : C-OVER ( over -- n ) MATCH over v0 OF 300 ENDOF v1 OF 301 ENDOF v2 OF 302 ENDOF v3 OF 303 ENDOF v4 OF 304 ENDOF v5 OF 305 ENDOF v6 OF 306 ENDOF v7 OF 307 ENDOF v8 OF 308 ENDOF v9 OF 309 ENDOF v10 OF 310 ENDOF v11 OF 311 ENDOF v12 OF 312 ENDOF v13 OF 313 ENDOF v14 OF 314 ENDOF v15 OF 315 ENDOF v16 OF 316 ENDOF ;MATCH ;" ;

: NONEXH$ ( -- ptr u8 n )
   s" : C-NONEXH ( hue -- n ) MATCH hue red OF 1 ENDOF green OF 2 ENDOF ;MATCH ;" ;

: DUPVAR$ ( -- ptr u8 n )
   s" : C-DUPVAR ( hue -- n ) MATCH hue red OF 1 ENDOF red OF 2 ENDOF blue OF 3 ENDOF ;MATCH ;" ;

: NOFAM$ ( -- ptr u8 n )
   s" : C-NOFAM ( hue -- n ) MATCH nosuchfamily red OF 1 ENDOF ;MATCH ;" ;

: NOTSUM$ ( -- ptr u8 n )
   s" : C-NOTSUM ( hue -- n ) MATCH n red OF 1 ENDOF ;MATCH ;" ;

: NOOF$ ( -- ptr u8 n )
   s" : C-NOOF ( hue -- n ) MATCH hue red 1 ENDOF green OF 2 ENDOF blue OF 3 ENDOF ;MATCH ;" ;

: STRAY$ ( -- ptr u8 n )
   s" : C-STRAY ( n -- n ) 1 + ;match ;" ;

: ROWS24$ ( -- ptr u8 n )
   s" : C-ROWS24 ( -- n ) 0 MS MATCH sol ov OF 1 ENDOF ;MATCH + MS MATCH sol ov OF 3 ENDOF ;MATCH + MS MATCH sol ov OF 5 ENDOF ;MATCH + MS MATCH sol ov OF 7 ENDOF ;MATCH + MS MATCH sol ov OF 9 ENDOF ;MATCH + MS MATCH sol ov OF 11 ENDOF ;MATCH + MS MATCH sol ov OF 13 ENDOF ;MATCH + MS MATCH sol ov OF 15 ENDOF ;MATCH + MS MATCH sol ov OF 17 ENDOF ;MATCH + MS MATCH sol ov OF 19 ENDOF ;MATCH + MS MATCH sol ov OF 21 ENDOF ;MATCH + MS MATCH sol ov OF 23 ENDOF ;MATCH + ;" ;

: ROWS26$ ( -- ptr u8 n )
   s" : C-ROWS26 ( -- n ) 0 MS MATCH sol ov OF 1 ENDOF ;MATCH + MS MATCH sol ov OF 3 ENDOF ;MATCH + MS MATCH sol ov OF 5 ENDOF ;MATCH + MS MATCH sol ov OF 7 ENDOF ;MATCH + MS MATCH sol ov OF 9 ENDOF ;MATCH + MS MATCH sol ov OF 11 ENDOF ;MATCH + MS MATCH sol ov OF 13 ENDOF ;MATCH + MS MATCH sol ov OF 15 ENDOF ;MATCH + MS MATCH sol ov OF 17 ENDOF ;MATCH + MS MATCH sol ov OF 19 ENDOF ;MATCH + MS MATCH sol ov OF 21 ENDOF ;MATCH + MS MATCH sol ov OF 23 ENDOF ;MATCH + MS MATCH sol ov OF 25 ENDOF ;MATCH + ;" ;

: CONFIT$ ( -- ptr u8 n )
   s" : C-CONFIT ( -- option<pt> ) 0 MS MATCH sol ov OF 1 ENDOF ;MATCH + MS MATCH sol ov OF 3 ENDOF ;MATCH + MS MATCH sol ov OF 5 ENDOF ;MATCH + MS MATCH sol ov OF 7 ENDOF ;MATCH + MS MATCH sol ov OF 9 ENDOF ;MATCH + MS MATCH sol ov OF 11 ENDOF ;MATCH + MS MATCH sol ov OF 13 ENDOF ;MATCH + MS MATCH sol ov OF 15 ENDOF ;MATCH + MS MATCH sol ov OF 17 ENDOF ;MATCH + MS MATCH sol ov OF 19 ENDOF ;MATCH + MS MATCH sol ov OF 21 ENDOF ;MATCH + drop OPTION:NONE ;" ;

: CONOVER$ ( -- ptr u8 n )
   s" : C-CONOVER ( -- option<pt> ) 0 MS MATCH sol ov OF 1 ENDOF ;MATCH + MS MATCH sol ov OF 3 ENDOF ;MATCH + MS MATCH sol ov OF 5 ENDOF ;MATCH + MS MATCH sol ov OF 7 ENDOF ;MATCH + MS MATCH sol ov OF 9 ENDOF ;MATCH + MS MATCH sol ov OF 11 ENDOF ;MATCH + MS MATCH sol ov OF 13 ENDOF ;MATCH + MS MATCH sol ov OF 15 ENDOF ;MATCH + MS MATCH sol ov OF 17 ENDOF ;MATCH + MS MATCH sol ov OF 19 ENDOF ;MATCH + MS MATCH sol ov OF 21 ENDOF ;MATCH + MS MATCH sol ov OF 23 ENDOF ;MATCH + drop OPTION:NONE ;" ;

: NARROWC$ ( -- ptr u8 n )
   s" : C-NARROWC ( n n n -- narrow<pt3> ) NMX-PT3:MAKE construct narrow p1 ;" ;

: NARROWK$ ( -- ptr u8 n )
   s" : C-NARROWK ( n n n -- narrow<pt3> ) NMX-PT3:MAKE NMX-NARROW:P1 ;" ;

variable RC-NONEXH  variable ROW-NONEXH
variable RC-DUPVAR  variable ROW-DUPVAR
variable RC-NOFAM   variable ROW-NOFAM
variable RC-NOTSUM
variable RC-NOOF    variable ROW-NOOF
variable RC-STRAY   variable ROW-STRAY
variable RC-OVER
variable RC-ROWS24  variable RC-ROWS26
variable RC-CONFIT  variable RC-CONOVER
variable RC-NARROWC variable ROW-NARROWC
variable RC-NARROWK variable ROW-NARROWK

: RUN-DYNAMIC-CASES ( -- )
   NONEXH$ TRY RC-NONEXH !  NELAB:REFUSED-ROW ROW-NONEXH !
   DUPVAR$ TRY RC-DUPVAR !  NELAB:REFUSED-ROW ROW-DUPVAR !
   NOFAM$ TRY RC-NOFAM !    NELAB:REFUSED-ROW ROW-NOFAM !
   NOTSUM$ TRY RC-NOTSUM !
   NOOF$ TRY RC-NOOF !      NELAB:REFUSED-ROW ROW-NOOF !
   STRAY$ TRY RC-STRAY !    NELAB:REFUSED-ROW ROW-STRAY !
   OVER$ TRY RC-OVER !
   ROWS24$ TRY RC-ROWS24 !
   ROWS26$ TRY RC-ROWS26 !
   CONFIT$ TRY RC-CONFIT !
   CONOVER$ TRY RC-CONOVER !
   NARROWC$ TRY RC-NARROWC ! NELAB:REFUSED-ROW ROW-NARROWC !
   NARROWK$ TRY RC-NARROWK ! NELAB:REFUSED-ROW ROW-NARROWK ! ;

RUN-DYNAMIC-CASES

\ Source compilation retires its temporary emission. Inspect the full committed
\ span: legacy lengths omit a trailing RET; exact spans can put cold traps after
\ the return. XREF-CODE-BYTES accounts for either representation.
variable CODE-BASE
variable CODE-LEN

: CODE! ( ptr u8 n -- )
   get-current XREF-FIND-WL {: rec:ptr :}
   rec XREF-FOUND? 0= if s" native-match: missing published code" 76 die then
   rec XREF-START CODE-BASE !
   rec XREF-CODE-BYTES CODE-LEN ! ;

\ A published record starts at an integer code address; its words are read as bytes.
CAST: CODE-BYTES ( n -- ptr u8 )

: CODE-WORD@ ( n -- n )
   INSN-BYTES * CODE-BASE @ + CODE-BYTES LE:U32@ ;

: CODE-INSNS ( -- n ) CODE-LEN @ INSN-BYTES / ;

\ Count complete x86-64 instructions so a branch byte inside an immediate
\ cannot satisfy the dispatch shape. FLOW-END also names UD2; only C3 is a RET.
variable X-AT
variable X-COND
variable X-RET
variable X-TRAP

: X-CODE-COUNTS ( -- n n n )
   CODE-BASE @ X-AT !
   0 X-COND !  0 X-RET !  0 X-TRAP !
   NTRAP:ROUTINE {: trap:n :}
   begin X-AT @ CODE-BASE @ CODE-LEN @ + < while
      X-AT @ CODE-BASE @ CODE-LEN @ + X-AT @ - CODE-BYTES:AT X-AT @ X64DIS:STEP
      {: bytes:n kind:n target:n :}
      kind X64DIS:FLOW-COND = if 1 X-COND +! then
      kind X64DIS:FLOW-END =
      X-AT @ 1 CODE-BYTES:AT drop c@ $C3 = and if 1 X-RET +! then
      kind X64DIS:FLOW-CALL = target trap = and if 1 X-TRAP +! then
      bytes X-AT +!
   repeat
   X-COND @ X-RET @ X-TRAP @ ;

: CODE-BRANCHES? ( -- bool )
   HB-TARGET-LINUX-X86-64? if X-CODE-COUNTS 2drop 0 > exit then
   CODE-INSNS 0 ?do
      i CODE-WORD@ NBR:COND? if true unloop exit then
   loop false ;

: CODE-RETURNS ( -- n )
   HB-TARGET-LINUX-X86-64? if X-CODE-COUNTS drop nip exit then
   0 CODE-INSNS 0 ?do
      i CODE-WORD@ NBR:RET? if 1+ then
   loop ;

: TRAP-BR? ( n n -- bool ) {: k:n t:n :}
   k CODE-WORD@ NBR:BL? 0= if false exit then
   CODE-BASE @ k INSN-BYTES * + k CODE-WORD@ NBR:BL-TARGET t = ;

: TRAP-BRANCHES ( -- n )
   HB-TARGET-LINUX-X86-64? if X-CODE-COUNTS nip nip exit then
   NTRAP:ROUTINE {: t:n :}
   0
   CODE-INSNS 0 ?do
      i t TRAP-BR? if 1+ then
   loop ;

variable EMIT-RC
variable EMIT-BRANCH
variable EMIT-RET
variable EMIT-TRAPS
variable UNW-SIZE
variable QUAD-SIZE
variable UNW-RET
variable QUAD-RET

: PROBE-HUE$ ( -- ptr u8 n )
   s" : PROBE-HUE ( hue -- n ) MATCH hue red OF 10 ENDOF green OF 20 ENDOF blue OF 30 ENDOF ;MATCH ;" ;

: PROBE-UNW$ ( -- ptr u8 n )
   s" : PROBE-UNW ( n option<n> -- n ) MATCH option none OF ENDOF some OF nip ENDOF ;MATCH ;" ;

: PROBE-QUAD$ ( -- ptr u8 n )
   s" : PROBE-QUAD ( quad -- n ) MATCH quad q0 OF 1 ENDOF q1 OF 2 ENDOF q2 OF 3 ENDOF q3 OF 4 ENDOF ;MATCH ;" ;

: CAPTURE-EMISSION ( -- )
   PROBE-HUE$ TRY EMIT-RC !
   s" PROBE-HUE" CODE!
   CODE-BRANCHES? if 1 else 0 then EMIT-BRANCH !
   CODE-RETURNS EMIT-RET !
   TRAP-BRANCHES EMIT-TRAPS !
   PROBE-UNW$ TRY drop s" PROBE-UNW" CODE!
   CODE-LEN @ UNW-SIZE ! CODE-RETURNS UNW-RET !
   PROBE-QUAD$ TRY drop s" PROBE-QUAD" CODE!
   CODE-LEN @ QUAD-SIZE ! CODE-RETURNS QUAD-RET ! ;

\ These instruction assertions describe optimizing emission. Other cases keep
\ the caller's tier, including when the suite is loaded directly from the REPL.
: CAPTURE-NATIVE-EMISSION ( -- )
   tier@ {: prior:n :}
   1 set-tier
   ['] CAPTURE-EMISSION catch {: rc:n :}
   prior set-tier
   rc 0<> if rc throw then ;

CAPTURE-NATIVE-EMISSION

: BASIC-CASE ( -- )
   s" empty and payload arms preserve their declared values" T-LABEL
   NMX-HUE:RED E-HUE 10 T=
   NMX-HUE:GREEN E-HUE 20 T=
   NMX-HUE:BLUE E-HUE 30 T=
   NMX-BOX:NIL E-BOX 0 T=
   7 NMX-BOX:ONE E-BOX 7 T=
   3 4 NMX-BOX:TWO E-BOX 7 T=
   9 OPTION:NONE E-UNW 9 T=
   9 42 OPTION:SOME E-UNW 42 T=

   s" four, seven, and sixteen-arm dispatches select every arm" T-LABEL
   NMX-QUAD:Q0 E-QUAD 1 T=
   NMX-QUAD:Q1 E-QUAD 2 T=
   NMX-QUAD:Q2 E-QUAD 3 T=
   NMX-QUAD:Q3 E-QUAD 4 T=
   NMX-STEP:P0 E-STEP 100 T=
   NMX-STEP:P1 E-STEP 101 T=
   NMX-STEP:P2 E-STEP 102 T=
   NMX-STEP:P3 E-STEP 103 T=
   NMX-STEP:P4 E-STEP 104 T=
   NMX-STEP:P5 E-STEP 105 T=
   NMX-STEP:P6 E-STEP 106 T=
   NMX-WIDE:W0 E-WIDE 200 T=
   NMX-WIDE:W1 E-WIDE 201 T=
   NMX-WIDE:W2 E-WIDE 202 T=
   NMX-WIDE:W3 E-WIDE 203 T=
   NMX-WIDE:W4 E-WIDE 204 T=
   NMX-WIDE:W5 E-WIDE 205 T=
   NMX-WIDE:W6 E-WIDE 206 T=
   NMX-WIDE:W7 E-WIDE 207 T=
   NMX-WIDE:W8 E-WIDE 208 T=
   NMX-WIDE:W9 E-WIDE 209 T=
   NMX-WIDE:W10 E-WIDE 210 T=
   NMX-WIDE:W11 E-WIDE 211 T=
   NMX-WIDE:W12 E-WIDE 212 T=
   NMX-WIDE:W13 E-WIDE 213 T=
   NMX-WIDE:W14 E-WIDE 214 T=
   NMX-WIDE:W15 E-WIDE 215 T=

   s" case keeps both arms and its default" T-LABEL
   1 E-CASE 10 T=
   2 E-CASE 20 T=
   5 E-CASE 99 T= ;

: CONSTRUCTOR-CASE ( -- )
   s" construct produces every payload width" T-LABEL
   55 E-MK E-BOX 55 T=
   3 4 E-MK2 E-BOX 7 T=
   E-MK0 E-BOX 0 T= ;

: DEAD-CASE ( -- )
   s" live arms around a dead arm return normally" T-LABEL
   NMX-HUE:RED E-DEAD 1 T=
   NMX-HUE:BLUE E-DEAD 3 T=
   [: NMX-HUE:GREEN E-DEAD drop ;] E-A-EMPTY TTHROWSQ ;

: HIDDEN-CASE ( -- )
   s" family spellings in comments and strings are not dispatch tokens" T-LABEL
   NMX-HUE:RED E-CMT 1 T=
   NMX-HUE:GREEN E-CMT 2 T=
   NMX-HUE:BLUE E-CMT 3 T=
   NMX-HUE:RED E-STR 1 T=
   NMX-HUE:GREEN E-STR 2 T=
   NMX-HUE:BLUE E-STR 3 T= ;

: PAYLOAD-CASE ( -- )
   s" two independent payload cells and one two-cell value keep distinct glue" T-LABEL
   3 4 NMX-BOX:TWO E-SWAPPED 1 T=
   9 NMX-HOLDER:EMPTY E-HOLD 9 T=
   9 3 4 NMX-PT:MAKE NMX-HOLDER:FULL E-HOLD 16 T=
   9 NMX-HOLDER:EMPTY E-DROPPED 9 T=
   9 3 4 NMX-PT:MAKE NMX-HOLDER:FULL E-DROPPED 9 T=

   s" instantiated payload widths keep cell order" T-LABEL
   0 E-MKI E-INST 0 T=
   3 E-MKI E-INST 204 T=
   5 E-MKI E-INST 340 T=
   0 E-MKI3 E-INST3 0 T=
   3 E-MKI3 E-INST3 423 T= ;

: ORDINAL-CASE ( -- )
   s" separate dispatch tokens and a preceding string keep their own width rows" T-LABEL
   0 E-TWOW 0 T=
   3 E-TWOW 645 T=
   0 E-STRINST 0 T=
   3 E-STRINST 204 T= ;

: TRIPLE-CASE ( -- )
   s" three-cell arms keep count and order across joins" T-LABEL
   NMX-TRIO:T0 E-TRIO 0 T=
   7 NMX-TRIO:T1 E-TRIO 21 T=
   3 5 9 NMX-TRIO:T3 E-TRIO 151 T=
   9 5 3 NMX-TRIO:T3 E-TRIO 223 T=

   s" three cells survive branch and loop control inside an arm" T-LABEL
   7 NMX-TRIO:T1 E-ARMIF 21 T=
   -7 NMX-TRIO:T1 E-ARMIF -35 T=
   3 5 9 NMX-TRIO:T3 E-ARMIF 179 T=
   3 -5 9 NMX-TRIO:T3 E-ARMIF 85 T=
   3 0 9 NMX-TRIO:T3 E-ARMIF 150 T=
   3 5 9 NMX-TRIO:T3 E-ARMLOOP 39 T=
   NMX-HOLD3:EMPTY3 E-HOLD3 0 T=
   3 5 9 NMX-PT3:MAKE NMX-HOLD3:FULL3 E-HOLD3 151 T= ;

: BUILD-CASE ( -- )
   s" wide constructors preserve empty and payload arms" T-LABEL
   0 E-MKC E-RDC 0 T=
   3 E-MKC E-RDC 204 T=

   s" dispatch and loop constructions preserve the widened value" T-LABEL
   0 E-MKI E-RELAY E-INST3 0 T=
   3 E-MKI E-RELAY E-INST3 453 T=
   0 E-LOOPC E-INST 4760 T=
   1 E-LOOPC E-INST 21386136 T=
   0 E-STRCON E-INST 0 T=
   3 E-STRCON E-INST 204 T=

   s" distinct construction tokens and payload placements stay independent" T-LABEL
   E-TWOC E-RDTWO 0 T=
   3 E-MKG E-RDG 204 T=
   3 E-MKGC E-RDG 204 T=
   3 E-MKGC2 E-RDG 1278 T=
   3 E-MKP E-RDP 204 T=
   -3 E-MKP E-RDP -423 T= ;

70 constant CHECKER-REJECT

: REFUSED-CASE ( -- )
   s" malformed and non-exhaustive dispatches stop before elaboration" T-LABEL
   RC-NONEXH @ CHECKER-REJECT T=  ROW-NONEXH @ -1 T=
   RC-DUPVAR @ CHECKER-REJECT T=  ROW-DUPVAR @ -1 T=
   RC-NOFAM @ CHECKER-REJECT T=   ROW-NOFAM @ -1 T=
   RC-NOTSUM @ CHECKER-REJECT T=
   RC-NOOF @ CHECKER-REJECT T=    ROW-NOOF @ -1 T=
   RC-STRAY @ CHECKER-REJECT T=   ROW-STRAY @ -1 T=

   s" narrower construction spellings remain checker refusals" T-LABEL
   RC-NARROWC @ CHECKER-REJECT T= ROW-NARROWC @ -1 T=
   RC-NARROWK @ CHECKER-REJECT T= ROW-NARROWK @ -1 T= ;

: GROWTH-CASE ( -- )
   s" dispatch beyond the former sixteen-arm selector ceiling remains executable" T-LABEL
   RC-OVER @ 0 T=
   NMX-OVER:V0 C-OVER 300 T=
   NMX-OVER:V1 C-OVER 301 T=
   NMX-OVER:V2 C-OVER 302 T=
   NMX-OVER:V3 C-OVER 303 T=
   NMX-OVER:V4 C-OVER 304 T=
   NMX-OVER:V5 C-OVER 305 T=
   NMX-OVER:V6 C-OVER 306 T=
   NMX-OVER:V7 C-OVER 307 T=
   NMX-OVER:V8 C-OVER 308 T=
   NMX-OVER:V9 C-OVER 309 T=
   NMX-OVER:V10 C-OVER 310 T=
   NMX-OVER:V11 C-OVER 311 T=
   NMX-OVER:V12 C-OVER 312 T=
   NMX-OVER:V13 C-OVER 313 T=
   NMX-OVER:V14 C-OVER 314 T=
   NMX-OVER:V15 C-OVER 315 T=
   NMX-OVER:V16 C-OVER 316 T=

   s" dispatch facts grow past twenty-four rows" T-LABEL
   RC-ROWS24 @ 0 T=
   C-ROWS24 144 T=
   RC-ROWS26 @ 0 T=
   C-ROWS26 169 T=

   s" widened constructions remain recorded after many dispatches" T-LABEL
   RC-CONFIT @ 0 T=
   C-CONFIT E-INST 0 T=
   RC-CONOVER @ 0 T=
   C-CONOVER E-INST 0 T= ;

: EMISSION-CASE ( -- )
   s" a compiled dispatch branches, returns, and uses one shared trap edge" T-LABEL
   EMIT-RC @ 0 T=
   EMIT-BRANCH @ 1 T=
   EMIT-RET @ 1 T=
   UNW-RET @ 1 T=
   QUAD-RET @ 1 T=
   EMIT-TRAPS @ 1 T=

   s" dispatch code stays bounded and grows with its arm count" T-LABEL
   HB-TARGET-LINUX-X86-64? if
      UNW-SIZE @ 256 < TTRUE
      QUAD-SIZE @ 384 < TTRUE
   else
      UNW-SIZE @ 128 < TTRUE
      QUAD-SIZE @ 184 < TTRUE
   then
   QUAD-SIZE @ UNW-SIZE @ > TTRUE ;

$4000 constant CAP-CAP
30000 constant CHILD-MS

create OUT-BUF CAP-CAP allot
create ERR-BUF CAP-CAP allot
create PRODUCT-PATH PATH-CAP allot

variable CHILD-OUT-N
variable CHILD-ERR-N
variable CHILD-RC

: PRODUCT$ ( -- ptr u8 n )
   SOURCE-ROOT:CWD$ s" bin/hb" PRODUCT-PATH FS:JOIN-PATH {: u:n :}
   PRODUCT-PATH u ;

: PRODUCT-ENV! ( -- )
   PROC-ENV-RESET
   s" HABU_UNDER_TEST" >LEN PRODUCT$ >LEN PROC-ENV+
   s" HABU_FIXPOINT_ENGINE" >LEN PRODUCT$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING ;

: CHILD-ARGV ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/compiler/aot-mode.f" >LEN PROC-ARGV+
   s" test/compiler/native-match-forge.f" >LEN PROC-ARGV+ ;

: CHILD-RUN ( -- )
   CHILD-ARGV
   PRODUCT-ENV!
   s" bin/hb" >LEN
   OUT-BUF CAP-CAP >LEN
   ERR-BUF CAP-CAP >LEN
   CHILD-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME
   PROC-OUTCOME>RC RC>N CHILD-RC !
   LEN>N CHILD-ERR-N !
   LEN>N CHILD-OUT-N ! ;

: CHILD-ERR$ ( -- ptr u8 n )
   ERR-BUF CHILD-ERR-N @ ;

: FORGE-CASE ( -- )
   CHILD-RUN
   s" a hostile tag reaches the shared trap and exits" T-LABEL
   CHILD-RC @ ENGINE-ERROR:BAD-TAG T=
   CHILD-ERR$ s" hb: bad hue tag" CONTAINS? TTRUE
   CHILD-ERR$ s" hb: bad box tag" CONTAINS? TFALSE ;

public

: MAIN ( -- )
   T-RESET
   BASIC-CASE
   CONSTRUCTOR-CASE
   DEAD-CASE
   HIDDEN-CASE
   PAYLOAD-CASE
   ORDINAL-CASE
   TRIPLE-CASE
   BUILD-CASE
   REFUSED-CASE
   GROWTH-CASE
   EMISSION-CASE
   FORGE-CASE
   NMX-LAYOUT:TEST
   T-REPORT
   s" native-match: ok" type cr ;

;package

NMX:MAIN
