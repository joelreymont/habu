\ OUTER:NUMBER against the engine's own reader. Every spelling must answer
\ num-parse's value, float flag and ok. num-parse hides range refusal, so the
\ interpreter is its oracle: with a word of the spelling's name defined,
\ evaluating a range-refused spelling still exits 70 (undefined), and any
\ other spelling reads as its number or runs the word.
require src/lib/test.f
require src/lib/fmt.f
require src/lib/f64-text.f
require test/gate-common.f
require src/habu/outer.f

package OUTER-NUMBER-TEST

variable COMPARED
variable REFUSALS
create DECIMAL-TEXT F64-TEXT:MAX-BYTES allot

\ Fixed IEEE-754 vectors, independently rounded by C strtod in FE_TONEAREST.
\ Reader parity alone cannot detect a double rounding both readers share.
: FLOAT-BITS ( ptr u8 n n -- )
   {: a:ptr u:n bits:n :}
   a u num-parse {: got:n flt:bool ok:bool :}
   a u T-LABEL got bits T= flt TTRUE ok TTRUE
   a u OUTER:NUMBER {: outer:n outer-flt:bool outer-ok:bool range:bool :}
   a u T-LABEL outer bits T= outer-flt TTRUE outer-ok TTRUE range TFALSE ;

: ROUNDING ( -- )
   s" 9.9996" $4023FFCB923A29C7 FLOAT-BITS
   s" -9.9996" $C023FFCB923A29C7 FLOAT-BITS
   s" 0.0" 0 FLOAT-BITS
   s" -0.0" $8000000000000000 FLOAT-BITS
   s" 9007199254740993.0" $4340000000000000 FLOAT-BITS
   s" 9007199254740993.5" $4340000000000001 FLOAT-BITS
   s" 4503599627370496.5" $4330000000000000 FLOAT-BITS
   s" 4503599627370497.5" $4330000000000002 FLOAT-BITS
   s" 9007199254740991.5" $4340000000000000 FLOAT-BITS
   s" 0.000000000000000001" $3C32725DD1D243AC FLOAT-BITS
   s" 9223372036854775807.0" $43E0000000000000 FLOAT-BITS ;

\ Replay text emitted by the wider text API through a fresh source compiler.
\ This value's shortest spelling is in the language reader's decimal domain.
: FORMATTED-REPLAY ( -- )
   $4023FFCB923A29C7 IEEE754:BITS>F64
   DECIMAL-TEXT F64-TEXT:MAX-BYTES F64-TEXT:FORMAT
   MATCH result ok OF ENDOF err OF drop -9899 throw ENDOF ;MATCH
   {: len:n :}
   GE-SRC-RESET
   s" require src/lib/ieee754.f" GE-SRC-LINE
   s" : REPLAY ( -- r ) " GE-SRC+ DECIMAL-TEXT len GE-SRC+
   s"  ;" GE-SRC-LINE
   s" : CHECK-REPLAY ( -- ) REPLAY IEEE754:F64>BITS $4023FFCB923A29C7 <> if 1 throw then ; CHECK-REPLAY" GE-SRC-LINE
   s" formatted decimal source replay" GE-HB-RUN-STDIN ;


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


: DECLINES ( -- )
   s" 5." SPELLING  s" 1.2.3" SPELLING  s" 1.5e3" SPELLING  s" 12a" SPELLING
   s" $" SPELLING  s" -" SPELLING  s" " TEXT
   s" 0.0000000000000000000" SPELLING
   s" -0.0085031157383406233" SPELLING
   s" 9223372036854775808.0" SPELLING  s" -9223372036854775808.0" SPELLING ;


: SHAPES ( -- )
   s" -$" SPELLING  s" ." SPELLING  s" -." SPELLING  s" -$0" SPELLING
   s" +1" SPELLING  s" --1" SPELLING  s" 0x10" SPELLING
   s" $aBcDeF" SPELLING  s" $ABCDEF" SPELLING  s" 1 2" TEXT
   s" $12g" SPELLING  s" -$-1" SPELLING  s" $-1" SPELLING  s" $G" SPELLING
   s" $1.5" SPELLING  s" $.5" SPELLING  s" 1e5" SPELLING  s" ..5" SPELLING
   s" 1..5" SPELLING  s" 1.5." SPELLING  s" 1." SPELLING ;


\ Floats: the sign of zero, the fraction and integer-part
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
ROUNDING
FORMATTED-REPLAY
SUMMARY
T-REPORT

;package
