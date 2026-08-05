\ real-lit.f - the cell a float literal's spelling stands for. One concern:
\ reading a real literal back the way the engine's own number parser reads it.
\
\ WHY THIS EXISTS AT ALL. The source tape records a literal's VALUE, because a
\ literal is not a name and the elaborator has nothing to resolve. The engine
\ computes that value in its own compiled number parser, at a point this code
\ cannot reach: the checker's reader hands over a token's bytes, not the number
\ the interpreter will push for them. So the value has to be read back from the
\ spelling - which is exactly what src/compiler/native/feed.f already does for an
\ integer literal, with the stdlib's decimal reader.
\
\ AND WHY THE STDLIB'S FLOAT READER IS NOT THE ONE TO USE. lib/float.f parses a
\ decimal by accumulating the significand in a double and scaling it by a power
\ of ten. The engine does something else: it accumulates the integer part and the
\ fractional digits in two INTEGER cells with a third holding the power of ten,
\ and finishes with three signed conversions, one division and one addition
\ (src/habu/habu1.f, C-NUM-FLOAT-FINISH: `0 11 SCVTF, 1 4 SCVTF, 2 3 SCVTF,
\ 1 1 2 FDIV, 0 0 1 FADD,` and a negation when the spelling opened with a minus).
\ Those two routes do not agree on every spelling - three roundings against two
\ round differently, and the survey at the head of tools/codegen-compare-corpus3.f
\ measures one of the disagreements: `1.9482199351819093` reads one bit below the
\ nearest double. A compiler whose literal is one bit from the interpreter's
\ literal computes a different program, so this file reproduces the engine's
\ route instruction for instruction rather than a better one.
\
\ THAT MEANS IT REPRODUCES THE ENGINE'S BUGS, DELIBERATELY. The fractional digits
\ go into one cell, so past eighteen of them the accumulation wraps and the value
\ is wrong - survey (10b), dot habu-fix-the-float-1d1467c8. Reading such a
\ spelling as something else here would make the compiled word disagree with the
\ interpreted one, which is a worse fault than the one being repaired. The tape
\ records what the reader produced; when that dot lands, this file changes with
\ it and the equality test in test/compiler/native-feed.f is what says so.
\
\ WHAT IT ANSWERS AND WHAT IT REFUSES. A spelling it can read answers the CELL
\ the double is - a double on a Habu stack is one unboxed cell holding its own
\ IEEE754 bits, so the cell is the value and no conversion happens on the way
\ out. A spelling it cannot read answers nothing at all, and the caller decides
\ what that means: this file has no opinion about which stage refuses a token.
\ What it cannot read is anything the engine's float path cannot read either - a
\ byte that is neither a digit nor the one dot, no dot, a second dot, or a dot
\ with no digit after it.

require lib/prelude.f
require lib/adt/option.f

package NREAL

private

\ A double IS a cell, so answering the cell is a retype and not a conversion.
\ It is the CAST: form, which the checker certifies, rather than a trusted
\ boundary, for the same reason CODEGEN-COMPARE:REAL-BITS is.
CAST: BITS ( r -- n ) ;

45 constant MINUS
46 constant DOT
48 constant ZERO
10 constant BASE

\ The engine's five accumulators, under the names its registers carry: the
\ integer part, the fractional digits as one integer, the power of ten they are
\ over, whether the spelling opened with a minus, and whether the dot has been
\ passed. They are variables and not locals because the scan leaves through a
\ refusal from the middle of its loop.
variable R-INT
variable R-FRAC
variable R-SCALE
variable R-NEG
variable R-DOT
variable R-IX
variable R-C
variable R-OK

: DIGIT? ( n -- bool )
   dup ZERO 1- > swap ZERO BASE + < and ;

: RESET ( -- )
   0 R-INT !  0 R-FRAC !  1 R-SCALE !  0 R-NEG !  0 R-DOT !  0 R-IX !
   0 R-C !  -1 R-OK ! ;

\ One digit, into whichever accumulator the parser mode says - which is the
\ engine's C-NUM-INT-STEP and C-NUM-FRAC-STEP, multiplication and addition on
\ cells, wrapping exactly as the machine's MUL and ADD wrap.
: DIGIT+ ( n -- )
   {: d:n :}
   R-DOT @ 0= if
      R-INT @ BASE * d + R-INT !
      exit
   then
   R-FRAC @ BASE * d + R-FRAC !
   R-SCALE @ BASE * R-SCALE ! ;

\ The dot, which opens the fractional part and may be met only once - the
\ engine's C-NUM-DOT, which resets the two fractional accumulators as it turns
\ the mode on.
: DOT+ ( -- )
   R-DOT @ 0<> if 0 R-OK ! exit then
   1 R-DOT !  0 R-FRAC !  1 R-SCALE ! ;

: BYTE+ ( -- )
   R-C @ DOT = if DOT+ exit then
   R-C @ DIGIT? 0= if 0 R-OK ! exit then
   R-C @ ZERO - DIGIT+ ;

: SCAN ( ptr u8 n -- )
   {: a:ptr u:n :}
   RESET
   u 0 > if
      a c@ MINUS = if 1 R-NEG !  1 R-IX ! then
   then
   begin R-IX @ u < R-OK @ 0<> and while
      a R-IX @ + c@ R-C !
      BYTE+
      R-IX @ 1+ R-IX !
   repeat ;

\ The engine's C-NUM-FLOAT-FINISH: three signed conversions, one division, one
\ addition, and the negation the sign asks for. A scale still standing at one
\ means the spelling carried no fractional digit, which is the engine's own
\ `3 1 CMPI, C-EQ NUM-DONE BCOND,` and covers both "no dot at all" and a
\ trailing dot.
: FINISH ( -- n )
   R-INT @ s>f
   R-FRAC @ s>f  R-SCALE @ s>f  f/
   f+
   R-NEG @ 0<> if fnegate then
   BITS ;

: READ? ( -- bool )
   R-OK @ 0= if false exit then
   R-SCALE @ 1 = if false exit then
   true ;

public

\ The cell the spelling stands for, or nothing. The caller owns the refusal: a
\ stage that meets a spelling this cannot read decides for itself whether that
\ is a malformed token or a token of some other class.
: READ ( ptr u8 n -- option<n> )
   SCAN
   READ? 0= if OPTION:NONE exit then
   FINISH OPTION:SOME ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
