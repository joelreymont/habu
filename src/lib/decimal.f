\ decimal.f - exact signed i64 conversion of decimal text (package DECIMAL),
\ with no float or allocation. EXACT takes single interior underscores, an
\ optional point with a digit on one side (`1.`, `.5`) and an optional signed
\ exponent (`1e1_0`), answering SOME only for an integer value in i64 range.
\ INTEGER takes any interior underscores (`1__2`), no point or exponent.
\ STORAGE CLASS. CALLER-OWNED: the module keeps no state.

require src/lib/adt/option.f
require src/lib/string.f

package DECIMAL
public
-9460 constant E-FIRST
-9469 constant E-LAST
-9460 constant E-SOURCE      \ negative length or nonempty null span
private

48 constant ZERO
57 constant NINE
43 constant PLUS
45 constant MINUS
46 constant DOT
69 constant EXP-UPPER
95 constant SEP
101 constant EXP-LOWER
-922337203685477580 constant LIMIT-QUOTIENT

CAST: N>I64 ( n -- i64 )

: DIGIT? ( n -- bool )
   dup ZERO >= swap NINE <= and ;

: BYTE ( ptr u8 n -- n )
   {: a:ptr at:n :}
   a at + c@ ;

: AT ( ptr u8 n n -- n )
   {: a:ptr u:n at:n :}
   at u < if a at BYTE else 0 then ;

: SOURCE ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0 < if E-SOURCE throw then
   u 0 > a 0= and if E-SOURCE throw then ;

\ An invalid separator remains unconsumed.
: DIGITS ( ptr u8 n n bool -- n n n n )
   {: a:ptr u:n start:n repeat?:bool :}
   start 0 0 0 begin
      {: at:n count:n leading:n trailing:n :}
      a u at AT {: ch:n :}
      ch DIGIT? if
         at 1+ count 1+
         ch ZERO = count leading = and if leading 1+ else leading then
         ch ZERO = if trailing 1+ else 0 then
      else
         ch SEP = count 0 > and if
            at 1+ begin
               dup u < repeat? and if a over BYTE SEP = else false then
            while 1+ repeat
            {: pos:n :}
            a u pos AT DIGIT? if
               \ The digit is processed on the next turn.
               pos count leading trailing
            else
               at count leading trailing exit
            then
         else
            at count leading trailing exit
         then
      then
   again ;

\ Exponent magnitude is capped beyond any useful decimal shift. A nonzero
\ mantissa spans at most u digits, so values beyond u+19 cannot fit i64.
: EXP-CAP ( n -- n )
   STR-MAX-I64 20 - min 20 + ;

: EXP-MAGNITUDE ( ptr u8 n n n -- n )
   {: a:ptr start:n end:n cap:n :}
   0 end start ?do
      a i BYTE dup DIGIT? if
         ZERO - {: digit:n :}
         dup cap 10 / > if drop cap else
            dup cap 10 / = digit cap 10 mod > and if drop cap else
               10 * digit +
            then
         then
      else drop then
   loop ;

\ An absent marker has zero exponent. A malformed marker remains unconsumed,
\ so the caller's end-of-input check rejects it.
: EXPONENT ( ptr u8 n n -- n n bool )
   {: a:ptr u:n start:n :}
   a u start AT dup EXP-LOWER = swap EXP-UPPER = or 0= if
      start 0 false exit
   then
   start 1+ {: after-e:n :}
   a u after-e AT MINUS = {: negative?:bool :}
   a u after-e AT dup MINUS = swap PLUS = or if after-e 1+ else after-e then {: digits-at:n :}
   a u digits-at false DIGITS {: end:n count:n leading:n trailing:n :}
   count 0= if start 0 false exit then
   a digits-at end u EXP-CAP EXP-MAGNITUDE {: magnitude:n :}
   end magnitude negative? ;

\ The negative accumulator admits i64 minimum without overflow.
: ACCUM ( ptr u8 n n n n bool -- option<i64> )
   {: a:ptr start:n end:n leading:n wanted:n negative?:bool :}
   negative? if 8 else 7 then {: last:n :}
   0 0 0 end start ?do
      a i BYTE {: ch:n :}
      ch DIGIT? if
         {: seen:n taken:n acc:n :}
         seen leading >= taken wanted < and if
            ch ZERO - {: digit:n :}
            acc LIMIT-QUOTIENT < acc LIMIT-QUOTIENT = digit last > and or if
               OPTION:NONE unloop exit
            then
            seen 1+ taken 1+ acc 10 * digit -
         else
            seen 1+ taken acc
         then
      then
   loop
   {: seen:n taken:n acc:n :}
   acc wanted taken - 0 ?do
      dup LIMIT-QUOTIENT < if drop OPTION:NONE unloop exit then
      10 *
   loop
   negative? 0= if 0 swap - then N>I64 OPTION:SOME ;

\ All zero mantissas are exact regardless of exponent size. For a nonzero
\ mantissa, `base` is the count of whole digits after leading zeros. Compare
\ exponent before adding it, keeping the resulting significant width 1..19.
: VALUE ( ptr u8 n n n n n n n bool bool -- option<i64> )
   {: a:ptr start:n end:n whole:n total:n leading:n trailing:n magnitude:n exp-negative?:bool negative?:bool :}
   whole leading - {: base:n :}
   exp-negative? if
      base magnitude <= if OPTION:NONE exit then
      base magnitude -
   else
      base 19 > if OPTION:NONE exit then
      magnitude 19 base - > if OPTION:NONE exit then
      base magnitude +
   then {: wanted:n :}
   wanted 0 <= wanted 19 > or if OPTION:NONE exit then
   total leading - wanted - trailing > if OPTION:NONE exit then
   a start end leading wanted negative? ACCUM ;

: CONVERT ( ptr u8 n bool -- option<i64> )
   {: a:ptr u:n integer?:bool :}
   a u SOURCE
   u 0= if OPTION:NONE exit then
   a 0 BYTE MINUS = {: negative?:bool :}
   a 0 BYTE dup MINUS = swap PLUS = or if 1 else 0 then {: start:n :}
   a u start integer? DIGITS {: int-end:n int-count:n int-lead:n int-trail:n :}
   integer? if
      int-count 0= int-end u <> or if OPTION:NONE exit then
      int-count int-lead = if 0 N>I64 OPTION:SOME exit then
      a start int-end int-count int-count int-lead int-trail 0 false negative? VALUE exit
   then
   a u int-end AT DOT = if
      a u int-end 1+ false DIGITS
   else
      int-end 0 0 0
   then {: mant-end:n frac-count:n frac-lead:n frac-trail:n :}
   int-count frac-count + 0= if OPTION:NONE exit then
   a u mant-end EXPONENT {: end:n magnitude:n exp-negative?:bool :}
   end u <> if OPTION:NONE exit then
   int-count int-lead = if int-count frac-lead + else int-lead then {: leading:n :}
   frac-count 0= if int-trail else
      frac-count frac-lead = if frac-count int-trail + else frac-trail then
   then {: trailing:n :}
   int-count frac-count + leading = if 0 N>I64 OPTION:SOME exit then
   a start mant-end int-count int-count frac-count + leading trailing magnitude exp-negative? negative? VALUE ;

public

: EXACT ( ptr u8 n -- option<i64> )
   false CONVERT ;

: INTEGER ( ptr u8 n -- option<i64> )
   true CONVERT ;

;package
