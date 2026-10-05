\ numeric-rows.f - the numeric golden rows a Wasm build must print as native
\ does: N01-N06 of docs/portability.md §25.3 and the three `?do` rows of W32
\ (§25.5), as data.
\
\ A row is three strings and the runner's ROW word:
\
\     s" NAME"  s" SOURCE"  s\" EXPECTED" ROW
\
\ SOURCE is a whole program: it defines a word named NAME, with any helper
\ named NAME-..., and requires what it uses. The program prints its own results
\ with `.`, `u.` and `f.`, and ends with its stack as `depth .` and then `.s`,
\ bottom first, before dropping it; nothing else reads its stack.
\ A double on the stack is its bits, so `.s` prints the bits as a signed cell.
\ A throw row catches in its own source and prints the code. EXPECTED is every
\ byte the program prints, each printer's newline included.
\
\ A runner includes this file (`include`, never `require`) with ROW in scope;
\ ROW evaluates SOURCE and then calls NAME. test/wasm/numeric.f runs the rows
\ natively; the Wasm differential runs the same rows on a Wasm build. The file
\ defines no word of its own.

\ ---- N01 signed and unsigned limits, wrapping add, subtract, multiply ----------
s" N01-WRAP"
s" : N01-WRAP ( -- ) $7FFFFFFFFFFFFFFF 1 + $8000000000000000 1 - $7FFFFFFFFFFFFFFF 2 * $7FFFFFFFFFFFFFFF dup * $8000000000000000 -1 * $8000000000000000 negate depth . .s 2drop 2drop 2drop ;"
s\" 6\n-9223372036854775808\n9223372036854775807\n-2\n1\n-9223372036854775808\n-9223372036854775808\n" ROW

s" N01-PRINT"
s" : N01-PRINT ( -- ) $8000000000000000 . $7FFFFFFFFFFFFFFF . -1 u. $8000000000000000 u. depth . ;"
s\" -9223372036854775808\n9223372036854775807\n18446744073709551615\n9223372036854775808\n0\n" ROW

\ ---- N02 a zero divisor throws -6400, MIN-N / -1 wraps -------------------------
s" N02-DIV-ZERO"
s" : N02-DIV-ZERO ( -- ) [: 1 0 / drop ;] catch [: 1 0 mod drop ;] catch [: 1 0 /mod 2drop ;] catch depth . .s 2drop drop ;"
s\" 3\n-6400\n-6400\n-6400\n" ROW

s" N02-MIN-BY-MINUS-ONE"
s" : N02-MIN-BY-MINUS-ONE ( -- ) $8000000000000000 -1 / $8000000000000000 -1 mod $8000000000000000 -1 /mod depth . .s 2drop 2drop ;"
s\" 4\n-9223372036854775808\n0\n0\n-9223372036854775808\n" ROW

\ ---- N03 a condition with only a high bit set is true --------------------------
\ A checked condition is a bool, so a cell reaches `if`, `until` and `while`
\ through `0<>`, which must test all 64 bits rather than the low 32.
s" N03-HIGH-IF"
s" : N03-HIGH-IF ( -- ) $100000000 0<> if 1 else 0 then $10000000000 0<> if 1 else 0 then $8000000000000000 0<> if 1 else 0 then $10000000000 0= $10000000000 0<> depth . .s 2drop 2drop drop ;"
s\" 5\n1\n1\n1\n0\n-1\n" ROW

\ until takes one turn; while halves bit 40 down to bit 0, so 41 turns.
s" N03-HIGH-LOOPS"
s" : N03-HIGH-LOOPS ( -- ) 0 begin 1+ $10000000000 0<> until 0 $10000000000 begin dup 0<> while 1 rshift swap 1+ swap repeat drop depth . .s 2drop ;"
s\" 2\n1\n41\n" ROW

\ ---- N04 a Habu flag is a whole-cell mask -------------------------------------
s" N04-MASKS"
s" : N04-MASKS ( -- ) 3 3 = 3 4 = $8000000000000000 0< 1 s>f 1 s>f f= 3 3 = 0= depth . .s 2drop 2drop drop ;"
s\" 5\n-1\n0\n-1\n-1\n0\n" ROW

\ ---- N05 shifts, remainder sign, integer and real conversions -----------------
s" N05-SHIFTS"
s" : N05-SHIFTS ( -- ) 1 0 lshift 1 63 lshift 1 64 lshift 1 65 lshift -1 1 lshift $8000000000000000 63 rshift -1 1 rshift -8 1 rshift -1 64 rshift depth . .s 2drop 2drop 2drop 2drop drop ;"
s\" 9\n1\n-9223372036854775808\n1\n2\n-2\n1\n9223372036854775807\n9223372036854775804\n-1\n" ROW

s" N05-REMAINDERS"
s" : N05-REMAINDERS ( -- ) 7 2 mod -7 2 mod 7 -2 mod -7 -2 mod -7 2 / -7 2 /mod depth . .s 2drop 2drop 2drop drop ;"
s\" 7\n1\n-1\n1\n-1\n-3\n-1\n-3\n" ROW

\ f>s truncates; s>f rounds to nearest even; 2^63 saturates on the way back.
s" N05-CONVERSIONS"
s" : N05-CONVERSIONS ( -- ) 5 s>f 2 s>f f/ f>s -5 s>f 2 s>f f/ f>s $20000000000001 s>f f>s $20000000000003 s>f f>s $7FFFFFFFFFFFFFFF s>f $7FFFFFFFFFFFFFFF s>f f>s $8000000000000000 s>f f>s depth . .s 2drop fdrop 2drop 2drop ;"
s\" 7\n2\n-2\n9007199254740992\n9007199254740996\n4890909195324358656\n9223372036854775807\n-9223372036854775808\n" ROW

s" N05-SPECIAL-TO-INT"
s" : N05-SPECIAL-TO-INT ( -- ) 1 s>f 0 s>f f/ f>s -1 s>f 0 s>f f/ f>s 0 s>f 0 s>f f/ f>s depth . .s 2drop drop ;"
s\" 3\n9223372036854775807\n-9223372036854775808\n0\n" ROW

\ ---- N06 NaNs, signed zero, infinities, subnormals, contraction --------------
\ Every operation that makes a NaN answers $7FF8000000000000 (portability.md §10.1).
s" N06-MADE-NAN"
s" : N06-MADE-NAN-INF ( -- r ) 1 s>f 0 s>f f/ ; : N06-MADE-NAN ( -- ) -1 s>f fsqrt 0 s>f 0 s>f f/ N06-MADE-NAN-INF N06-MADE-NAN-INF f- 0 s>f N06-MADE-NAN-INF f* N06-MADE-NAN-INF N06-MADE-NAN-INF f/ N06-MADE-NAN-INF fnegate N06-MADE-NAN-INF f+ depth . .s fdrop fdrop fdrop fdrop fdrop fdrop ;"
s\" 6\n9221120237041090560\n9221120237041090560\n9221120237041090560\n9221120237041090560\n9221120237041090560\n9221120237041090560\n" ROW

\ A quiet NaN operand passes through unchanged, the left one of two; payloads
\ $ABC and $DEF tell the operands apart, and a negative NaN keeps its sign.
s" N06-NAN-PASSED"
s" require lib/ieee754.f : N06-NAN-PASSED-R ( n -- r ) IEEE754:BITS>F64 ; : N06-NAN-PASSED ( -- ) $7FF8000000000ABC N06-NAN-PASSED-R $7FF8000000000DEF N06-NAN-PASSED-R f+ $7FF8000000000DEF N06-NAN-PASSED-R $7FF8000000000ABC N06-NAN-PASSED-R f* 1 s>f $7FF8000000000ABC N06-NAN-PASSED-R f- $FFF8000000000000 N06-NAN-PASSED-R 1 s>f f+ $FFF8000000000000 N06-NAN-PASSED-R fsqrt $7FF8000000000ABC N06-NAN-PASSED-R fdup f= depth . .s drop fdrop fdrop fdrop fdrop fdrop ;"
s\" 6\n9221120237041093308\n9221120237041094127\n9221120237041093308\n-2251799813685248\n-2251799813685248\n0\n" ROW

s" N06-SIGNED-ZERO"
s" : N06-SIGNED-ZERO-NEG ( -- r ) 0 s>f fnegate ; : N06-SIGNED-ZERO ( -- ) N06-SIGNED-ZERO-NEG N06-SIGNED-ZERO-NEG 0 s>f f+ N06-SIGNED-ZERO-NEG N06-SIGNED-ZERO-NEG f+ N06-SIGNED-ZERO-NEG f0= N06-SIGNED-ZERO-NEG 0 s>f f= 1 s>f N06-SIGNED-ZERO-NEG f/ 1 s>f 0 s>f f/ depth . .s fdrop fdrop 2drop fdrop fdrop fdrop ;"
s\" 7\n-9223372036854775808\n0\n-9223372036854775808\n-1\n-1\n-4503599627370496\n9218868437227405312\n" ROW

s" N06-INFINITIES"
s" require lib/ieee754.f : N06-INFINITIES-INF ( -- r ) 1 s>f 0 s>f f/ ; : N06-INFINITIES ( -- ) N06-INFINITIES-INF 1 s>f f+ N06-INFINITIES-INF fnegate $7FEFFFFFFFFFFFFF IEEE754:BITS>F64 2 s>f f* $7FEFFFFFFFFFFFFF IEEE754:BITS>F64 N06-INFINITIES-INF f< depth . .s drop fdrop fdrop fdrop ;"
s\" 4\n9218868437227405312\n-4503599627370496\n9218868437227405312\n-1\n" ROW

\ Subnormals are kept, never flushed to zero; half the least of them rounds to 0.
s" N06-SUBNORMALS"
s" require lib/ieee754.f : N06-SUBNORMALS-R ( n -- r ) IEEE754:BITS>F64 ; : N06-SUBNORMALS ( -- ) 1 N06-SUBNORMALS-R 1 s>f f* 1 N06-SUBNORMALS-R 1 N06-SUBNORMALS-R f+ $0010000000000000 N06-SUBNORMALS-R 2 s>f f/ $0010000000000000 N06-SUBNORMALS-R 1 N06-SUBNORMALS-R f- 1 N06-SUBNORMALS-R 2 s>f f/ 1 N06-SUBNORMALS-R f0= depth . .s drop fdrop fdrop fdrop fdrop fdrop ;"
s\" 6\n1\n2\n2251799813685248\n4503599627370495\n0\n0\n" ROW

\ (1+2^-30) squared is 1 + 2^-29 + 2^-60: minus 1+2^-29 it is 2^-60 fused and
\ 0 rounded twice. Habu forbids contraction, so 0.
s" N06-CONTRACTION"
s" require lib/ieee754.f : N06-CONTRACTION ( -- ) $3FF0000000400000 IEEE754:BITS>F64 fdup f* $3FF0000000800000 IEEE754:BITS>F64 f- depth . .s fdrop ;"
s\" 1\n0\n" ROW

\ The three NaN print rows.
s" N06-PRINT-SQRT"
s" : N06-PRINT-SQRT ( -- ) -1 s>f fsqrt f. depth . ;"
s\" 0.000000\n0\n" ROW

s" N06-PRINT-DIV"
s" : N06-PRINT-DIV ( -- ) 0 s>f 0 s>f f/ f. depth . ;"
s\" 0.000000\n0\n" ROW

s" N06-PRINT-SUB"
s" : N06-PRINT-SUB-INF ( -- r ) 1 s>f 0 s>f f/ ; : N06-PRINT-SUB ( -- ) N06-PRINT-SUB-INF N06-PRINT-SUB-INF f- f. depth . ;"
s\" 0.000000\n0\n" ROW

\ ---- W32 ?do enters only below its limit; ?do +loop skips only equal bounds ---
s" W32-QDO-NEGATIVE"
s" : W32-QDO-NEGATIVE ( -- ) 0 -1 0 ?do 1+ loop depth . .s drop ;"
s\" 1\n0\n" ROW

s" W32-QDO-MIN"
s" : W32-QDO-MIN ( -- ) 0 $8000000000000000 0 ?do 1+ loop depth . .s drop ;"
s\" 1\n0\n" ROW

\ Eleven turns, i from 10 down to 0.
s" W32-QDO-DOWN"
s" : W32-QDO-DOWN ( -- ) 0 0 10 ?do 1+ -1 +loop 0 0 10 ?do i + -1 +loop depth . .s 2drop ;"
s\" 2\n11\n55\n" ROW
