\ date.f - checked Gregorian UTC date helpers.

require lib/adt/option.f                      \ option<n> for DATE-N (switchover wave A)

10 constant DATE-LEN
20 constant DATE-TIME-LEN
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
86400 constant DATE-SECONDS-DAY

variable DATE-Y
variable DATE-M
variable DATE-D
variable DATE-REM
variable DATE-Z
variable DATE-ERA
variable DATE-DOE
variable DATE-YOE
variable DATE-DOY
variable DATE-MP
variable DATE-I
variable DATE-RUN

: DATE-DIGIT? ( n -- bool )
   dup DATE-ZERO 1- > swap DATE-ZERO DATE-BASE + < and ;

: LEAP-YEAR? ( n -- bool ) {: y :}
   y DATE-LEAP-YEARS mod 0=  y DATE-CENTURY-YEARS mod 0= 0= and
   y DATE-ERA-YEARS mod 0= or ;

: MONTH-DAYS ( n n -- n ) {: y m :}
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

: VALID-YMD? ( n n n -- bool ) {: y m d :}
   m DATE-JAN < IF 0 0= 0= exit THEN
   m DATE-DEC > IF 0 0= 0= exit THEN
   d DATE-JAN < IF 0 0= 0= exit THEN
   d y m MONTH-DAYS > IF 0 0= 0= exit THEN
   0 0= ;

: YMD>DAYS ( n n n -- n ) {: y m d :}
   y DATE-Y !
   m DATE-FEB <= IF DATE-Y @ 1- DATE-Y ! THEN
   DATE-Y @ DATE-ERA-YEARS / DATE-ERA !
   DATE-Y @ DATE-ERA @ DATE-ERA-YEARS * - DATE-YOE !
   m DATE-FEB > IF m DATE-MAR-BIAS - ELSE m DATE-JAN-FEB-BIAS + THEN DATE-MP !
   DATE-MP-SCALE DATE-MP @ * DATE-MP-BIAS + DATE-MP-DIVISOR / d + 1 - DATE-DOY !
   DATE-YOE @ DATE-DAYS-YEAR *  DATE-YOE @ DATE-LEAP-YEARS / +  DATE-YOE @ DATE-CENTURY-YEARS / -  DATE-DOY @ + DATE-DOE !
   DATE-ERA @ DATE-DAYS-ERA * DATE-DOE @ + DATE-UNIX-EPOCH-DAY - ;

: DAYS>YMD ( n -- n n n ) {: days :}
   days DATE-UNIX-EPOCH-DAY + DATE-Z !
   DATE-Z @ DATE-DAYS-ERA / DATE-ERA !
   DATE-Z @ DATE-ERA @ DATE-DAYS-ERA * - DATE-DOE !
   DATE-DOE @  DATE-DOE @ DATE-YOE-LEAP-CORR / -  DATE-DOE @ DATE-YOE-CENTURY-CORR / +  DATE-DOE @ DATE-LAST-DAY-ERA / -  DATE-DAYS-YEAR / DATE-YOE !
   DATE-YOE @ DATE-ERA @ DATE-ERA-YEARS * + DATE-Y !
   DATE-DOE @  DATE-DAYS-YEAR DATE-YOE @ *  DATE-YOE @ DATE-LEAP-YEARS / +  DATE-YOE @ DATE-CENTURY-YEARS / -  - DATE-DOY !
   DATE-MP-DIVISOR DATE-DOY @ * DATE-MP-BIAS + DATE-MP-SCALE / DATE-MP !
   DATE-DOY @  DATE-MP-SCALE DATE-MP @ * DATE-MP-BIAS + DATE-MP-DIVISOR /  - 1 + DATE-D !
   DATE-MP @ DATE-MP-LIMIT < IF DATE-MP @ DATE-MAR-BIAS + ELSE DATE-MP @ DATE-JAN-FEB-BIAS - THEN DATE-M !
   DATE-M @ DATE-FEB <= IF DATE-Y @ 1+ DATE-Y ! THEN
   DATE-Y @ DATE-M @ DATE-D @ ;

: DATE-N ( ptr u8 n n -- option<n> ) {: a:ptr pos:n len:n :}   \ SOME parsed field, NONE on a non-digit
   0 DATE-I !
   0
   begin DATE-I @ len < while
      a pos + DATE-I @ + c@ dup DATE-DIGIT? 0= IF drop drop OPTION:NONE exit THEN
      DATE-ZERO - swap DATE-BASE * +
      DATE-I @ 1+ DATE-I !
   repeat OPTION:SOME ;

: PARSE-YMD ( ptr u8 n -- n bool ) {: a:ptr u:n :}
   u DATE-LEN <> IF 0 0 0= 0= exit THEN
   a DATE-YEAR-DASH + c@ DATE-DASH <> IF 0 0 0= 0= exit THEN
   a DATE-MONTH-DASH + c@ DATE-DASH <> IF 0 0 0= 0= exit THEN
   a 0 DATE-YEAR-LEN DATE-N MATCH option
     none OF 0 0 0= 0= exit ENDOF
     some OF DATE-Y ! ENDOF
   ;MATCH
   a DATE-YEAR-DASH 1+ DATE-PART-LEN DATE-N MATCH option
     none OF 0 0 0= 0= exit ENDOF
     some OF DATE-M ! ENDOF
   ;MATCH
   a DATE-MONTH-DASH 1+ DATE-PART-LEN DATE-N MATCH option
     none OF 0 0 0= 0= exit ENDOF
     some OF DATE-D ! ENDOF
   ;MATCH
   DATE-Y @ DATE-M @ DATE-D @ VALID-YMD? 0= IF 0 0 0= 0= exit THEN
   DATE-Y @ DATE-M @ DATE-D @ YMD>DAYS 0 0= ;

: DATE-WIDTH! ( n n ptr u8 n -- ) {: n width dst:ptr pos :}
   n DATE-RUN !
   width 1- DATE-I !
   begin DATE-I @ 0 >= while
      DATE-RUN @ DATE-BASE mod DATE-ZERO +  dst pos + DATE-I @ + c!
      DATE-RUN @ DATE-BASE / DATE-RUN !
      DATE-I @ 1- DATE-I !
   repeat ;

: FORMAT-YMD ( n ptr u8 n -- ptr u8 n ) {: days dst:ptr cap :}
   cap DATE-LEN < IF E-TIME-CAPACITY throw THEN
   days DAYS>YMD DATE-D ! DATE-M ! DATE-Y !
   DATE-Y @ DATE-YEAR-LEN dst 0 DATE-WIDTH!
   DATE-DASH dst DATE-YEAR-DASH + c!
   DATE-M @ DATE-PART-LEN dst DATE-YEAR-DASH 1+ DATE-WIDTH!
   DATE-DASH dst DATE-MONTH-DASH + c!
   DATE-D @ DATE-PART-LEN dst DATE-MONTH-DASH 1+ DATE-WIDTH!
   dst DATE-LEN ;

: FORMAT-EPOCH-UTC ( n ptr u8 n -- ptr u8 n ) {: seconds dst:ptr cap :}
   cap DATE-TIME-LEN < IF E-TIME-CAPACITY throw THEN
   seconds 0 < IF E-TIME-RANGE throw THEN
   seconds DATE-SECONDS-DAY / dst cap FORMAT-YMD 2drop
   seconds DATE-SECONDS-DAY mod DATE-REM !
   DATE-T-CHAR dst DATE-T-POS + c!
   DATE-REM @ DATE-SECONDS-HOUR / DATE-PART-LEN dst DATE-HOUR-POS DATE-WIDTH!
   DATE-REM @ DATE-SECONDS-HOUR mod DATE-REM !
   DATE-COLON dst DATE-HOUR-COLON + c!
   DATE-REM @ DATE-SECONDS-MINUTE / DATE-PART-LEN dst DATE-MINUTE-POS DATE-WIDTH!
   DATE-REM @ DATE-SECONDS-MINUTE mod DATE-REM !
   DATE-COLON dst DATE-MINUTE-COLON + c!
   DATE-REM @ DATE-PART-LEN dst DATE-SECOND-POS DATE-WIDTH!
   DATE-Z-CHAR dst DATE-Z-POS + c!
   dst DATE-TIME-LEN ;
