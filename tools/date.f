\ date.f - shared Gregorian UTC date helpers.

10 constant DATE-LEN
45 constant DATE-DASH
48 constant DATE-ZERO

variable DATE-Y
variable DATE-M
variable DATE-D
variable DATE-Z
variable DATE-ERA
variable DATE-DOE
variable DATE-YOE
variable DATE-DOY
variable DATE-MP
variable DATE-I
variable DATE-RUN

: DATE-DIGIT? ( n -- bool )
   dup 47 > swap 58 < and ;

: LEAP-YEAR? {: y :} ( n -- bool )
   y 4 mod 0=  y 100 mod 0= 0= and
   y 400 mod 0= or ;

: MONTH-DAYS {: y m :} ( n n -- n )
   m 1 = IF 31 exit THEN
   m 2 = IF y LEAP-YEAR? IF 29 ELSE 28 THEN exit THEN
   m 3 = IF 31 exit THEN
   m 4 = IF 30 exit THEN
   m 5 = IF 31 exit THEN
   m 6 = IF 30 exit THEN
   m 7 = IF 31 exit THEN
   m 8 = IF 31 exit THEN
   m 9 = IF 30 exit THEN
   m 10 = IF 31 exit THEN
   m 11 = IF 30 exit THEN
   m 12 = IF 31 exit THEN
   0 ;

: VALID-YMD? {: y m d :} ( n n n -- bool )
   m 1 < IF 0 0= 0= exit THEN
   m 12 > IF 0 0= 0= exit THEN
   d 1 < IF 0 0= 0= exit THEN
   d y m MONTH-DAYS > IF 0 0= 0= exit THEN
   0 0= ;

: YMD>DAYS {: y m d :} ( n n n -- n )
   y DATE-Y !
   m 2 <= IF DATE-Y @ 1- DATE-Y ! THEN
   DATE-Y @ 400 / DATE-ERA !
   DATE-Y @ DATE-ERA @ 400 * - DATE-YOE !
   m 2 > IF m 3 - ELSE m 9 + THEN DATE-MP !
   153 DATE-MP @ * 2 + 5 / d + 1 - DATE-DOY !
   DATE-YOE @ 365 *  DATE-YOE @ 4 / +  DATE-YOE @ 100 / -  DATE-DOY @ + DATE-DOE !
   DATE-ERA @ 146097 * DATE-DOE @ + 719468 - ;

: DAYS>YMD {: days :} ( n -- n n n )
   days 719468 + DATE-Z !
   DATE-Z @ 146097 / DATE-ERA !
   DATE-Z @ DATE-ERA @ 146097 * - DATE-DOE !
   DATE-DOE @  DATE-DOE @ 1460 / -  DATE-DOE @ 36524 / +  DATE-DOE @ 146096 / -  365 / DATE-YOE !
   DATE-YOE @ DATE-ERA @ 400 * + DATE-Y !
   DATE-DOE @  365 DATE-YOE @ *  DATE-YOE @ 4 / +  DATE-YOE @ 100 / -  - DATE-DOY !
   5 DATE-DOY @ * 2 + 153 / DATE-MP !
   DATE-DOY @  153 DATE-MP @ * 2 + 5 /  - 1 + DATE-D !
   DATE-MP @ 10 < IF DATE-MP @ 3 + ELSE DATE-MP @ 9 - THEN DATE-M !
   DATE-M @ 2 <= IF DATE-Y @ 1+ DATE-Y ! THEN
   DATE-Y @ DATE-M @ DATE-D @ ;

: DATE-N {: a:ptr pos len :} ( ptr u8 n n -- n bool )
   0 DATE-I !
   0
   begin DATE-I @ len < while
      a pos + DATE-I @ + c@ dup DATE-DIGIT? 0= IF drop drop 0 0 0= 0= exit THEN
      DATE-ZERO - swap 10 * +
      DATE-I @ 1+ DATE-I !
   repeat 0 0= ;

: PARSE-YMD {: a:ptr u :} ( ptr u8 n -- n bool )
   u DATE-LEN <> IF 0 0 0= 0= exit THEN
   a 4 + c@ DATE-DASH <> IF 0 0 0= 0= exit THEN
   a 7 + c@ DATE-DASH <> IF 0 0 0= 0= exit THEN
   a 0 4 DATE-N 0= IF drop 0 0 0= 0= exit THEN DATE-Y !
   a 5 2 DATE-N 0= IF drop 0 0 0= 0= exit THEN DATE-M !
   a 8 2 DATE-N 0= IF drop 0 0 0= 0= exit THEN DATE-D !
   DATE-Y @ DATE-M @ DATE-D @ VALID-YMD? 0= IF 0 0 0= 0= exit THEN
   DATE-Y @ DATE-M @ DATE-D @ YMD>DAYS 0 0= ;

: DATE-WIDTH! {: n width dst:ptr pos :} ( n n ptr u8 n -- )
   n DATE-RUN !
   width 1- DATE-I !
   begin DATE-I @ 0 >= while
      DATE-RUN @ 10 mod DATE-ZERO +  dst pos + DATE-I @ + c!
      DATE-RUN @ 10 / DATE-RUN !
      DATE-I @ 1- DATE-I !
   repeat ;

: FORMAT-YMD {: days dst:ptr cap :} ( n ptr u8 n -- ptr u8 n )
   cap DATE-LEN < IF s" date: output buffer too small" 74 die THEN
   days DAYS>YMD DATE-D ! DATE-M ! DATE-Y !
   DATE-Y @ 4 dst 0 DATE-WIDTH!
   DATE-DASH dst 4 + c!
   DATE-M @ 2 dst 5 DATE-WIDTH!
   DATE-DASH dst 7 + c!
   DATE-D @ 2 dst 8 DATE-WIDTH!
   dst DATE-LEN ;
