\ date.f - checked Gregorian UTC date helpers.
\
\ The module lives in `package DATE`; `tools/date.f` was a duplicate and is gone,
\ so this is the one date module for both the stdlib and the tool CLIs. External
\ callers use the qualified public API: DATE:PARSE-YMD parses YYYY-MM-DD into a
\ Unix epoch day, DATE:FORMAT-YMD / DATE:FORMAT-EPOCH-UTC write YYYY-MM-DD /
\ YYYY-MM-DDTHH:MM:SSZ into a caller buffer (throwing E-TIME-CAPACITY when the
\ buffer is too small and E-TIME-RANGE for a negative epoch), DATE:YMD>DAYS /
\ DATE:DAYS>YMD convert between a calendar date and the epoch day, and
\ DATE:LEAP-YEAR? / DATE:MONTH-DAYS / DATE:VALID-YMD? / DATE:DIGIT? / DATE:N /
\ DATE:WIDTH! expose the calendar predicates and the field parse/format
\ primitives. DATE:LEN / DATE:TIME-LEN / DATE:SECONDS-DAY are the buffer-size and
\ seconds-per-day constants callers need. Other calendar constants are private.

require lib/errors.f
require lib/adt/option.f                      \ option<n> for DATE-N (switchover wave A)

package DATE

public
10 constant LEN
20 constant TIME-LEN
private
4 constant DATE-YEAR-LEN
2 constant DATE-PART-LEN
4 constant DATE-YEAR-DASH
7 constant DATE-MONTH-DASH
10 constant DATE-T-POS
11 constant DATE-HOUR-POS
13 constant DATE-HOUR-COLON
14 constant DATE-MINUTE-POS
16 constant DATE-MINUTE-COLON
17 constant DATE-SECOND-POS
19 constant DATE-Z-POS
45 constant DATE-DASH
58 constant DATE-COLON
84 constant DATE-T-CHAR
90 constant DATE-Z-CHAR
48 constant DATE-ZERO
10 constant DATE-BASE

1 constant DATE-JAN
2 constant DATE-FEB
3 constant DATE-MAR
4 constant DATE-APR
5 constant DATE-MAY
6 constant DATE-JUN
7 constant DATE-JUL
8 constant DATE-AUG
9 constant DATE-SEP
10 constant DATE-OCT
11 constant DATE-NOV
12 constant DATE-DEC

28 constant DATE-FEB-DAYS
29 constant DATE-FEB-LEAP-DAYS
30 constant DATE-SHORT-MONTH-DAYS
31 constant DATE-LONG-MONTH-DAYS

4 constant DATE-LEAP-YEARS
100 constant DATE-CENTURY-YEARS
365 constant DATE-DAYS-YEAR
400 constant DATE-ERA-YEARS
146097 constant DATE-DAYS-ERA
146096 constant DATE-LAST-DAY-ERA
719468 constant DATE-UNIX-EPOCH-DAY
153 constant DATE-MP-SCALE
2 constant DATE-MP-BIAS
5 constant DATE-MP-DIVISOR
3 constant DATE-MAR-BIAS
9 constant DATE-JAN-FEB-BIAS
10 constant DATE-MP-LIMIT
1460 constant DATE-YOE-LEAP-CORR
36524 constant DATE-YOE-CENTURY-CORR
60 constant DATE-SECONDS-MINUTE
3600 constant DATE-SECONDS-HOUR
public
86400 constant SECONDS-DAY
private

public

: DIGIT? ( n -- bool )
   dup DATE-ZERO 1- > swap DATE-ZERO DATE-BASE + < and ;

: LEAP-YEAR? ( n -- bool ) {: y:n :}
   y DATE-LEAP-YEARS mod 0=  y DATE-CENTURY-YEARS mod 0= 0= and
   y DATE-ERA-YEARS mod 0= or ;

: MONTH-DAYS ( n n -- n ) {: y:n m:n :}
   m DATE-JAN = IF DATE-LONG-MONTH-DAYS exit THEN
   m DATE-FEB = IF y LEAP-YEAR? IF DATE-FEB-LEAP-DAYS ELSE DATE-FEB-DAYS THEN exit THEN
   m DATE-MAR = IF DATE-LONG-MONTH-DAYS exit THEN
   m DATE-APR = IF DATE-SHORT-MONTH-DAYS exit THEN
   m DATE-MAY = IF DATE-LONG-MONTH-DAYS exit THEN
   m DATE-JUN = IF DATE-SHORT-MONTH-DAYS exit THEN
   m DATE-JUL = IF DATE-LONG-MONTH-DAYS exit THEN
   m DATE-AUG = IF DATE-LONG-MONTH-DAYS exit THEN
   m DATE-SEP = IF DATE-SHORT-MONTH-DAYS exit THEN
   m DATE-OCT = IF DATE-LONG-MONTH-DAYS exit THEN
   m DATE-NOV = IF DATE-SHORT-MONTH-DAYS exit THEN
   m DATE-DEC = IF DATE-LONG-MONTH-DAYS exit THEN
   0 ;

: VALID-YMD? ( n n n -- bool ) {: y:n m:n d:n :}
   m DATE-JAN < IF 0 0= 0= exit THEN
   m DATE-DEC > IF 0 0= 0= exit THEN
   d DATE-JAN < IF 0 0= 0= exit THEN
   d y m MONTH-DAYS > IF 0 0= 0= exit THEN
   0 0= ;

: YMD>DAYS ( n n n -- n ) {: y:n m:n d:n :}
   m DATE-FEB <= IF y 1- ELSE y THEN {: yy:n :}
   yy DATE-ERA-YEARS / {: era:n :}
   yy era DATE-ERA-YEARS * - {: yoe:n :}
   m DATE-FEB > IF m DATE-MAR-BIAS - ELSE m DATE-JAN-FEB-BIAS + THEN {: mp:n :}
   DATE-MP-SCALE mp * DATE-MP-BIAS + DATE-MP-DIVISOR / d + 1 - {: doy:n :}
   yoe DATE-DAYS-YEAR *  yoe DATE-LEAP-YEARS / +  yoe DATE-CENTURY-YEARS / -  doy + {: doe:n :}
   era DATE-DAYS-ERA * doe + DATE-UNIX-EPOCH-DAY - ;

: DAYS>YMD ( n -- n n n ) {: days:n :}
   days DATE-UNIX-EPOCH-DAY + {: z:n :}
   z DATE-DAYS-ERA / {: era:n :}
   z era DATE-DAYS-ERA * - {: doe:n :}
   doe  doe DATE-YOE-LEAP-CORR / -  doe DATE-YOE-CENTURY-CORR / +  doe DATE-LAST-DAY-ERA / -  DATE-DAYS-YEAR / {: yoe:n :}
   yoe era DATE-ERA-YEARS * + {: y:n :}
   doe  DATE-DAYS-YEAR yoe *  yoe DATE-LEAP-YEARS / +  yoe DATE-CENTURY-YEARS / -  - {: doy:n :}
   DATE-MP-DIVISOR doy * DATE-MP-BIAS + DATE-MP-SCALE / {: mp:n :}
   doy  DATE-MP-SCALE mp * DATE-MP-BIAS + DATE-MP-DIVISOR /  - 1 + {: d:n :}
   mp DATE-MP-LIMIT < IF mp DATE-MAR-BIAS + ELSE mp DATE-JAN-FEB-BIAS - THEN {: m:n :}
   m DATE-FEB <= IF y 1+ ELSE y THEN m d ;

: N ( ptr u8 n n -- option<n> )
   {: a:ptr pos:n len:n :}   \ SOME parsed field, NONE on a non-digit
   len 0 <= IF 0 OPTION:SOME exit THEN
   0 len 0 ?do
      a pos + i + c@ dup DIGIT? 0= IF drop drop unloop OPTION:NONE exit THEN
      DATE-ZERO - swap DATE-BASE * +
   loop OPTION:SOME ;

: PARSE-YMD ( ptr u8 n -- option<n> )
   {: a:ptr u:n :}   \ SOME Unix epoch day, NONE on bad YYYY-MM-DD
   u LEN <> IF OPTION:NONE exit THEN
   a DATE-YEAR-DASH + c@ DATE-DASH <> IF OPTION:NONE exit THEN
   a DATE-MONTH-DASH + c@ DATE-DASH <> IF OPTION:NONE exit THEN
   a 0 DATE-YEAR-LEN N MATCH option
     none OF OPTION:NONE ENDOF
     some OF
        {: y:n :}
        a DATE-YEAR-DASH 1+ DATE-PART-LEN N MATCH option
          none OF OPTION:NONE ENDOF
          some OF
             {: m:n :}
             a DATE-MONTH-DASH 1+ DATE-PART-LEN N MATCH option
               none OF OPTION:NONE ENDOF
               some OF
                  {: d:n :}
                  y m d VALID-YMD? IF y m d YMD>DAYS OPTION:SOME
                  ELSE OPTION:NONE THEN
               ENDOF
             ;MATCH
          ENDOF
        ;MATCH
     ENDOF
   ;MATCH ;

: WIDTH! ( n n ptr u8 n -- )
   {: n:n width:n dst:ptr pos:n :}
   width 0 <= IF exit THEN
   n width 0 ?do
      dup DATE-BASE mod DATE-ZERO +  dst pos + width 1- i - + c!
      DATE-BASE /
   loop drop ;

: FORMAT-YMD ( n ptr u8 n -- ptr u8 n )
   {: days:n dst:ptr cap:n :}
   cap LEN < IF E-TIME-CAPACITY throw THEN
   days DAYS>YMD {: y:n m:n d:n :}
   y DATE-YEAR-LEN dst 0 WIDTH!
   DATE-DASH dst DATE-YEAR-DASH + c!
   m DATE-PART-LEN dst DATE-YEAR-DASH 1+ WIDTH!
   DATE-DASH dst DATE-MONTH-DASH + c!
   d DATE-PART-LEN dst DATE-MONTH-DASH 1+ WIDTH!
   dst LEN ;

: FORMAT-EPOCH-UTC ( n ptr u8 n -- ptr u8 n )
   {: seconds:n dst:ptr cap:n :}
   cap TIME-LEN < IF E-TIME-CAPACITY throw THEN
   seconds 0 < IF E-TIME-RANGE throw THEN
   seconds SECONDS-DAY / dst cap FORMAT-YMD 2drop
   seconds SECONDS-DAY mod {: rem:n :}
   DATE-T-CHAR dst DATE-T-POS + c!
   rem DATE-SECONDS-HOUR / DATE-PART-LEN dst DATE-HOUR-POS WIDTH!
   rem DATE-SECONDS-HOUR mod {: minute-rem:n :}
   DATE-COLON dst DATE-HOUR-COLON + c!
   minute-rem DATE-SECONDS-MINUTE / DATE-PART-LEN dst DATE-MINUTE-POS WIDTH!
   minute-rem DATE-SECONDS-MINUTE mod {: second:n :}
   DATE-COLON dst DATE-MINUTE-COLON + c!
   second DATE-PART-LEN dst DATE-SECOND-POS WIDTH!
   DATE-Z-CHAR dst DATE-Z-POS + c!
   dst TIME-LEN ;

;package
