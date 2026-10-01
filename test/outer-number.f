\ OUTER:NUMBER against the engine's own reader. Every spelling must answer
\ num-parse's value, float flag and ok. num-parse hides range refusal, so the
\ interpreter is its oracle: with a word of the spelling's name defined,
\ evaluating a range-refused spelling still exits 70 (undefined), and any
\ other spelling reads as its number or runs the word.
require lib/test.f
require lib/fmt.f
require test/gate-common.f
require src/habu/outer.f

package OUTER-NUMBER-TEST

variable COMPARED
variable REFUSALS


: RANGE? ( ptr u8 n -- bool )
   OUTER:NUMBER {: v:n flt:bool ok:bool range:bool :} range ;


: FLAG= ( bool bool -- ) {: got:bool want:bool :}
   want if got TTRUE else got TFALSE then ;


: SAME-PARSE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u num-parse {: want:n want-flt:bool want-ok:bool :}
   a u OUTER:NUMBER drop {: got:n flt:bool ok:bool :}
   a u T-LABEL got want T=
   a u T-LABEL flt want-flt FLAG=
   a u T-LABEL ok want-ok FLAG= ;


: HOSTILE-EVAL ( ptr u8 n -- n ) {: a:ptr u:n :}
   GE-SRC-RESET
   s" : " GE-SRC+ a u GE-SRC+ s"  ( -- n ) 42 ;" GE-SRC-LINE
   a u GE-SRC+ s"  drop" GE-SRC-LINE
   GE-EVAL-FORK-CAPTURE
   a u GE-RC@ ;


: SAME-RANGE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u RANGE? {: range:bool :}
   range if 1 REFUSALS +! 70 else 0 then {: want:n :}
   a u HOSTILE-EVAL {: rc:n :}
   a u T-LABEL rc want T= ;


\ A spelling that can name a word: both oracles.
: SPELLING ( ptr u8 n -- ) {: a:ptr u:n :}
   a u SAME-PARSE
   a u SAME-RANGE
   1 COMPARED +! ;


\ Text that cannot name a word (empty, or two tokens): num-parse only.
: TEXT ( ptr u8 n -- )
   SAME-PARSE
   1 COMPARED +! ;


\ test/compiler/integer-literals.f VALUES and REFUSALS.
: INTEGERS ( -- )
   s" 0" SPELLING  s" -0" SPELLING
   s" 9223372036854775807" SPELLING  s" -9223372036854775808" SPELLING
   s" 0009223372036854775807" SPELLING  s" -0009223372036854775808" SPELLING
   s" $7FFFFFFFFFFFFFFF" SPELLING  s" $8000000000000000" SPELLING
   s" $FFFFFFFFFFFFFFFF" SPELLING  s" $ffffffffffffffff" SPELLING
   s" -$FFFFFFFFFFFFFFFF" SPELLING  s" -$8000000000000000" SPELLING
   s" $000FFFFFFFFFFFFFFFF" SPELLING
   s" 18446744073709551616X" SPELLING  s" $10000000000000000G" SPELLING ;


: OVERFLOWS ( -- )
   s" 9223372036854775808" SPELLING  s" -9223372036854775809" SPELLING
   s" 18446744073709551615" SPELLING  s" -18446744073709551615" SPELLING
   s" 18446744073709551616" SPELLING  s" -18446744073709551616" SPELLING
   s" 36893488147419103232" SPELLING  s" -36893488147419103232" SPELLING
   s" $10000000000000000" SPELLING  s" -$10000000000000000" SPELLING
   s" $1FFFFFFFFFFFFFFFF" SPELLING  s" -$1FFFFFFFFFFFFFFFF" SPELLING
   s" 18446744073709551617" SPELLING  s" 18446744073709551609" SPELLING
   s" $FFFFFFFFFFFFFFFF0" SPELLING ;


\ test/compiler/native-feed.f DECLINE-CASE.
: DECLINES ( -- )
   s" 5." SPELLING  s" 1.2.3" SPELLING  s" 1.5e3" SPELLING  s" 12a" SPELLING
   s" $" SPELLING  s" -" SPELLING  s" " TEXT
   s" 0.0000000000000000000" SPELLING
   s" -0.0085031157383406233" SPELLING
   s" 9223372036854775808.0" SPELLING  s" -9223372036854775808.0" SPELLING ;


\ Prefix and digit shapes.
: SHAPES ( -- )
   s" -$" SPELLING  s" ." SPELLING  s" -." SPELLING  s" -$0" SPELLING
   s" +1" SPELLING  s" --1" SPELLING  s" 0x10" SPELLING
   s" $aBcDeF" SPELLING  s" $ABCDEF" SPELLING  s" 1 2" TEXT
   s" $12g" SPELLING  s" -$-1" SPELLING  s" $-1" SPELLING  s" $G" SPELLING
   s" $1.5" SPELLING  s" $.5" SPELLING  s" 1e5" SPELLING  s" ..5" SPELLING
   s" 1..5" SPELLING  s" 1.5." SPELLING  s" 1." SPELLING ;


\ Floats: the sign of zero, the sum order, the fraction and integer-part
\ bounds, and a wrapped integer part that ends in a bare point.
: FLOATS ( -- )
   s" .5" SPELLING  s" -.5" SPELLING  s" 1.5" SPELLING  s" -1.5" SPELLING
   s" 0.1" SPELLING  s" 123.456" SPELLING  s" -123.456" SPELLING
   s" -0.0" SPELLING  s" -.0" SPELLING  s" 9007199254740993.5" SPELLING
   s" 0.000000000000000001" SPELLING  s" 0.0000000000000000001" SPELLING
   s" 1.000000000000000000" SPELLING  s" 1.0000000000000000000" SPELLING
   s" 9223372036854775807.5" SPELLING  s" 9223372036854775808.5" SPELLING
   s" -9223372036854775808.5" SPELLING  s" 18446744073709551615.5" SPELLING
   s" 18446744073709551616.5" SPELLING  s" 99999999999999999999." SPELLING ;


: SUMMARY ( -- )
   s" outer-number: " type COMPARED @ FMT:.INT
   s"  spellings compared with num-parse, " type REFUSALS @ FMT:.INT
   s"  range refusals with evaluate" type cr ;


T-RESET
INTEGERS
OVERFLOWS
DECLINES
SHAPES
FLOATS
SUMMARY
T-REPORT

;package
