\ A long branch chain with string results exercises residency at every join.

require lib/test.f
require lib/string.f
require src/compiler/native/compiler.f

package NSWITCH-TEST
private

10777 constant E-SWITCH-KEY

: NAME ( n -- ptr u8 n )
   case
      0 of s" first" endof
      1 of s" tag-01" endof
      2 of s" tag-02" endof
      3 of s" tag-03" endof
      4 of s" tag-04" endof
      5 of s" tag-05" endof
      6 of s" tag-06" endof
      7 of s" tag-07" endof
      8 of s" tag-08" endof
      9 of s" tag-09" endof
      10 of s" tag-10" endof
      11 of s" tag-11" endof
      12 of s" tag-12" endof
      13 of s" tag-13" endof
      14 of s" tag-14" endof
      15 of s" tag-15" endof
      16 of s" tag-16" endof
      17 of s" tag-17" endof
      18 of s" tag-18" endof
      19 of s" tag-19" endof
      20 of s" tag-20" endof
      21 of s" middle-value" endof
      22 of s" tag-22" endof
      23 of s" tag-23" endof
      24 of s" tag-24" endof
      25 of s" tag-25" endof
      26 of s" tag-26" endof
      27 of s" tag-27" endof
      28 of s" tag-28" endof
      29 of s" tag-29" endof
      30 of s" tag-30" endof
      31 of s" tag-31" endof
      32 of s" tag-32" endof
      33 of s" tag-33" endof
      34 of s" tag-34" endof
      35 of s" tag-35" endof
      36 of s" tag-36" endof
      37 of s" tag-37" endof
      38 of s" tag-38" endof
      39 of s" tag-39" endof
      40 of s" tag-40" endof
      41 of s" last" endof
      E-SWITCH-KEY throw
   endcase ;

: KEYS ( -- )
   s" every arm reaches the common string result" T-LABEL
   42 0 ?do
      i NAME {: a:ptr u:n :}
      i 0 = if a u s" first" STR= TTRUE else
      i 21 = if a u s" middle-value" STR= TTRUE else
      i 41 = if a u s" last" STR= TTRUE else
         u 6 T=
         a 4 s" tag-" STR= TTRUE
         a 4 + c@ i 10 / $30 + T=
         a 5 + c@ i 10 mod $30 + T=
      then then then
   loop ;

public

: RUN ( -- )
   T-RESET
   KEYS
   s" the default arm still throws for keys outside the switch" T-LABEL
   [: -1 NAME 2drop ;] E-SWITCH-KEY TTHROWSQ
   [: 42 NAME 2drop ;] E-SWITCH-KEY TTHROWSQ
   T-REPORT ;

;package

NSWITCH-TEST:RUN
