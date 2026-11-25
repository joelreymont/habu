\ divide-literal.f - `/` and `mod` by a literal, which the compiler opens into
\ the target's own instructions, answer what the kernel's `/` and `mod` answer
\ with the divisor in a local.
require src/lib/test/assert.f

package DIVIDE-LITERAL-TEST

1 63 lshift constant LEAST
LEAST 1- constant GREATEST
$9E3779B97F4A7C15 constant SEED

\ xorshift64
: NEXT-RANDOM ( n -- n )
   dup 13 lshift xor
   dup 7 rshift xor
   dup 17 lshift xor ;

: ONE ( n n [ n -- n ] [ n -- n ] -- )
   {: x:n v:n quot rem :}
   x quot execute x v / T=
   x rem execute x v mod T= ;

: SIGNS ( n n [ n -- n ] [ n -- n ] -- )
   {: x:n v:n quot rem :}
   x v quot rem ONE
   x negate v quot rem ONE ;

: NEAR ( n n [ n -- n ] [ n -- n ] -- )
   {: x:n v:n quot rem :}
   x 1- v quot rem SIGNS
   x v quot rem SIGNS
   x 1+ v quot rem SIGNS ;

: RANDOMS ( n [ n -- n ] [ n -- n ] -- )
   {: v:n quot rem :}
   SEED 1000 0 ?do
      NEXT-RANDOM dup v quot rem ONE
   loop
   drop ;

\ The last multiple of v below 2^63 is the dividend a reciprocal fails first at.
: DIVIDES ( n [ n -- n ] [ n -- n ] -- )
   {: v:n quot rem :}
   0 v quot rem NEAR
   v v quot rem NEAR
   v 2 * v quot rem NEAR
   v 3 * v quot rem NEAR
   v 1000 * v quot rem NEAR
   GREATEST v / v * v quot rem NEAR
   LEAST v quot rem NEAR
   v quot rem RANDOMS ;

: VALUES ( -- )
   1 [: 1 / ;] [: 1 mod ;] DIVIDES
   2 [: 2 / ;] [: 2 mod ;] DIVIDES
   3 [: 3 / ;] [: 3 mod ;] DIVIDES
   4 [: 4 / ;] [: 4 mod ;] DIVIDES
   5 [: 5 / ;] [: 5 mod ;] DIVIDES
   7 [: 7 / ;] [: 7 mod ;] DIVIDES
   8 [: 8 / ;] [: 8 mod ;] DIVIDES
   9 [: 9 / ;] [: 9 mod ;] DIVIDES
   10 [: 10 / ;] [: 10 mod ;] DIVIDES
   12 [: 12 / ;] [: 12 mod ;] DIVIDES
   16 [: 16 / ;] [: 16 mod ;] DIVIDES
   25 [: 25 / ;] [: 25 mod ;] DIVIDES
   26 [: 26 / ;] [: 26 mod ;] DIVIDES
   60 [: 60 / ;] [: 60 mod ;] DIVIDES
   64 [: 64 / ;] [: 64 mod ;] DIVIDES
   100 [: 100 / ;] [: 100 mod ;] DIVIDES
   125 [: 125 / ;] [: 125 mod ;] DIVIDES
   251 [: 251 / ;] [: 251 mod ;] DIVIDES
   641 [: 641 / ;] [: 641 mod ;] DIVIDES
   1000 [: 1000 / ;] [: 1000 mod ;] DIVIDES
   1024 [: 1024 / ;] [: 1024 mod ;] DIVIDES
   3402 [: 3402 / ;] [: 3402 mod ;] DIVIDES
   1000000 [: 1000000 / ;] [: 1000000 mod ;] DIVIDES
   $10000 [: $10000 / ;] [: $10000 mod ;] DIVIDES
   $7FFFFFFF [: $7FFFFFFF / ;] [: $7FFFFFFF mod ;] DIVIDES
   $80000000 [: $80000000 / ;] [: $80000000 mod ;] DIVIDES
   $100000000 [: $100000000 / ;] [: $100000000 mod ;] DIVIDES
   $10000000000 [: $10000000000 / ;] [: $10000000000 mod ;] DIVIDES
   $4000000000000000 [: $4000000000000000 / ;]
   [: $4000000000000000 mod ;] DIVIDES
   -2 [: -2 / ;] [: -2 mod ;] DIVIDES
   -3 [: -3 / ;] [: -3 mod ;] DIVIDES
   -10 [: -10 / ;] [: -10 mod ;] DIVIDES
   -1000 [: -1000 / ;] [: -1000 mod ;] DIVIDES ;

\ Three dividends live in three registers at once: each is divided where it is
\ and leaves the other two as they were.
: LIVE ( [ n n n -- n n n ] n -- )
   {: part v:n :}
   1000003 -2000029 3000073 part execute
   3000073 v / T=
   -2000029 v / T=
   1000003 v / T= ;

: LIVE-MOD ( [ n n n -- n n n ] n -- )
   {: part v:n :}
   1000003 -2000029 3000073 part execute
   3000073 v mod T=
   -2000029 v mod T=
   1000003 v mod T= ;

: REGISTERS ( -- )
   [: 8 / rot 8 / rot 8 / rot ;] 8 LIVE
   [: 8 mod rot 8 mod rot 8 mod rot ;] 8 LIVE-MOD
   [: 25 / rot 25 / rot 25 / rot ;] 25 LIVE
   [: 25 mod rot 25 mod rot 25 mod rot ;] 25 LIVE-MOD
   [: 10 / rot 10 / rot 10 / rot ;] 10 LIVE
   [: 10 mod rot 10 mod rot 10 mod rot ;] 10 LIVE-MOD
   [: -10 / rot -10 / rot -10 / rot ;] -10 LIVE
   [: -10 mod rot -10 mod rot -10 mod rot ;] -10 LIVE-MOD ;

T-RESET
VALUES
REGISTERS
T-REPORT
;package
