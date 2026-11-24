split-str
\ date.f - Gregorian UTC dates, in `package DATE`, the one date module for the
\ stdlib and the tool CLIs. PARSE-YMD accepts exactly YYYY-MM-DD.
\ PARSE-HTTP accepts the three forms RFC 9110 section 5.6.7 makes a recipient
\ accept: IMF-fixdate, RFC 850 and asctime. Each form is a template whose `_`
\ bytes hold fields and whose other bytes must match. A day name is checked
\ against the form, never the date; a leap second 60 reads as 59.
\ RFC 850's two-digit year is the latest year whose timestamp exists and is
\ no more than 50 calendar years after now.
\ FORMAT-YMD and FORMAT-EPOCH-UTC throw E-TIME-CAPACITY for a buffer shorter than
\ LEN / TIME-LEN and E-TIME-RANGE for a year outside 0..9999, which four digits
\ cannot write; FORMAT-EPOCH-UTC also refuses negative epoch seconds.
\ STORAGE CLASS. CALLER-OWNED: output goes to the caller's buffer; no module state.

require src/lib/error.f
require src/lib/adt/option.f
require src/lib/string.f

package DATE

public
10 constant LEN
20 constant TIME-LEN
private
4 constant DATE-YEAR-LEN
9999 constant DATE-YEAR-MAX             \ the largest year DATE-YEAR-LEN digits write
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
   yy 0 < IF yy DATE-ERA-YEARS 1- - ELSE yy THEN DATE-ERA-YEARS / {: era:n :}   \ floor
   yy era DATE-ERA-YEARS * - {: yoe:n :}
   m DATE-FEB > IF m DATE-MAR-BIAS - ELSE m DATE-JAN-FEB-BIAS + THEN {: mp:n :}
   DATE-MP-SCALE mp * DATE-MP-BIAS + DATE-MP-DIVISOR / d + 1 - {: doy:n :}
   yoe DATE-DAYS-YEAR *  yoe DATE-LEAP-YEARS / +  yoe DATE-CENTURY-YEARS / -  doy + {: doe:n :}
   era DATE-DAYS-ERA * doe + DATE-UNIX-EPOCH-DAY - ;

: DAYS>YMD ( n -- n n n ) {: days:n :}
   days DATE-UNIX-EPOCH-DAY + {: z:n :}
   z 0 < IF z DATE-LAST-DAY-ERA - ELSE z THEN DATE-DAYS-ERA / {: era:n :}   \ floor
   z era DATE-DAYS-ERA * - {: doe:n :}
   doe  doe DATE-YOE-LEAP-CORR / -  doe DATE-YOE-CENTURY-CORR / +  doe DATE-LAST-DAY-ERA / -  DATE-DAYS-YEAR / {: yoe:n :}
   yoe era DATE-ERA-YEARS * + {: y:n :}
   doe  DATE-DAYS-YEAR yoe *  yoe DATE-LEAP-YEARS / +  yoe DATE-CENTURY-YEARS / -  - {: doy:n :}
   DATE-MP-DIVISOR doy * DATE-MP-BIAS + DATE-MP-SCALE / {: mp:n :}
   doy  DATE-MP-SCALE mp * DATE-MP-BIAS + DATE-MP-DIVISOR /  - 1 + {: d:n :}
   mp DATE-MP-LIMIT < IF mp DATE-MAR-BIAS + ELSE mp DATE-JAN-FEB-BIAS - THEN {: m:n :}
   m DATE-FEB <= IF y 1+ ELSE y THEN m d ;

: N ( ^u8 n n -- option<n> )
   {: a:^ pos:n len:n :}   \ SOME parsed field, NONE on a non-digit
   0 len 0 ?do
      a pos + i + c@ dup DIGIT? 0= IF drop drop unloop OPTION:NONE exit THEN
      DATE-ZERO - swap DATE-BASE * +
   loop OPTION:SOME ;

: PARSE-YMD ( str -- option<n> )
   {: a:^ u:n :}   \ SOME Unix epoch day, NONE on bad YYYY-MM-DD
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

: WIDTH! ( n n ^u8 n -- )
   {: n:n width:n dst:^ pos:n :}
   n width 0 ?do
      dup DATE-BASE mod DATE-ZERO +  dst pos + width 1- i - + c!
      DATE-BASE /
   loop drop ;

: FORMAT-YMD ( n ^u8 n -- str )
   {: days:n dst:^ cap:n :}
   cap LEN < IF E-TIME-CAPACITY throw THEN
   days DAYS>YMD {: y:n m:n d:n :}
   y 0 < y DATE-YEAR-MAX > or IF E-TIME-RANGE throw THEN
   y DATE-YEAR-LEN dst 0 WIDTH!
   DATE-DASH dst DATE-YEAR-DASH + c!
   m DATE-PART-LEN dst DATE-YEAR-DASH 1+ WIDTH!
   DATE-DASH dst DATE-MONTH-DASH + c!
   d DATE-PART-LEN dst DATE-MONTH-DASH 1+ WIDTH!
   dst LEN ;

: FORMAT-EPOCH-UTC ( n ^u8 n -- str )
   {: seconds:n dst:^ cap:n :}
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

private
3 constant DATE-NAME-LEN                \ a month or short day name
95 constant DATE-HOLE                   \ `_`, a field byte in a template
1970 constant DATE-EPOCH-YEAR
50 constant DATE-YEARS-AHEAD
100 constant DATE-DAY-SPAN              \ more than any two-digit day
23 constant DATE-HOUR-MAX
59 constant DATE-MINUTE-MAX
59 constant DATE-SECOND-MAX
60 constant DATE-LEAP-SECOND
3 constant DATE-CLOCK-MINUTE            \ offsets in HH:MM:SS
6 constant DATE-CLOCK-SECOND
2 constant DATE-STAMP-DAY               \ offsets after the day name
5 constant DATE-STAMP-MONTH
9 constant DATE-STAMP-YEAR
4 constant DATE-ASCTIME-MONTH
8 constant DATE-ASCTIME-DAY
11 constant DATE-ASCTIME-CLOCK
20 constant DATE-ASCTIME-YEAR

: IMF-TEMPLATE ( -- str ) s" ___, __ ___ ____ __:__:__ GMT" ;
: RFC850-TEMPLATE ( -- str ) s" , __-___-__ __:__:__ GMT" ;
: ASCTIME-TEMPLATE ( -- str ) s" ___ ___ __ __:__:__ ____" ;

: TEMPLATE? ( str n str -- bool )
   {: a:^ u:n k:n t:^ tu:n :}   \ t matches the text from byte k on
   k 0 <  u k tu + <> or IF false exit THEN
   tu 0 ?do
      t i + c@ dup DATE-HOLE <>
      swap a k + i + c@ <> and IF unloop false exit THEN
   loop true ;

: DAY-NAME= ( str bool str -- bool )
   {: a:^ u:n short:bool name:^ len:n :}
   short IF a u name DATE-NAME-LEN STR:= exit THEN
   a u name len STR:= ;

: DAY-NAME? ( str bool -- bool )
   {: a:^ u:n short:bool :}   \ short: the name's first three bytes
   a u short s" Monday" DAY-NAME=
   a u short s" Tuesday" DAY-NAME= or
   a u short s" Wednesday" DAY-NAME= or
   a u short s" Thursday" DAY-NAME= or
   a u short s" Friday" DAY-NAME= or
   a u short s" Saturday" DAY-NAME= or
   a u short s" Sunday" DAY-NAME= or ;

: MONTH-NAME ( ^u8 n -- option<n> )
   {: a:^ pos:n :}   \ SOME month 1..12 named at pos
   s" JanFebMarAprMayJunJulAugSepOctNovDec" drop {: names:^ :}
   DATE-DEC 0 ?do
      a pos + DATE-NAME-LEN
      names i DATE-NAME-LEN * + DATE-NAME-LEN STR:=
      IF i 1+ unloop OPTION:SOME exit THEN
   loop OPTION:NONE ;

: HMS ( n n n -- option<n> )
   {: h:n mi:n s:n :}   \ SOME seconds into the day
   h DATE-HOUR-MAX >  mi DATE-MINUTE-MAX > or
   s DATE-LEAP-SECOND > or IF OPTION:NONE exit THEN
   h DATE-SECONDS-HOUR *  mi DATE-SECONDS-MINUTE * +
   s DATE-SECOND-MAX min + OPTION:SOME ;

: CLOCK ( ^u8 n -- option<n> )
   {: a:^ pos:n :}   \ SOME seconds into the day of HH:MM:SS at pos
   a pos DATE-PART-LEN N MATCH option
     none OF OPTION:NONE ENDOF
     some OF
        {: h:n :}
        a pos DATE-CLOCK-MINUTE + DATE-PART-LEN N MATCH option
          none OF OPTION:NONE ENDOF
          some OF
             {: mi:n :}
             a pos DATE-CLOCK-SECOND + DATE-PART-LEN N MATCH option
               none OF OPTION:NONE ENDOF
               some OF {: s:n :} h mi s HMS ENDOF
             ;MATCH
          ENDOF
        ;MATCH
     ENDOF
   ;MATCH ;

: MOMENT ( n n n n -- option<n> )
   {: y:n m:n d:n s:n :}   \ SOME epoch seconds, s seconds into the day
   y DATE-EPOCH-YEAR <  y DATE-YEAR-MAX > or IF OPTION:NONE exit THEN
   y m d VALID-YMD? 0= IF OPTION:NONE exit THEN
   y m d YMD>DAYS SECONDS-DAY * s + OPTION:SOME ;

: YEAR-KEY ( n n n -- n )
   {: m:n d:n s:n :}   \ orders month m, day d, second s within a year
   m DATE-DAY-SPAN * d + SECONDS-DAY * s + ;

: DAY-SECOND ( n -- n n )
   {: t:n :}   \ the floored day of epoch seconds t and its second
   t SECONDS-DAY mod {: r:n :}
   r 0 < IF r SECONDS-DAY + ELSE r THEN {: s:n :}
   t s - SECONDS-DAY / s ;

: FULL-YEAR ( n n n n n -- n )
   {: yy:n m:n d:n s:n now:n :}   \ the year of two digits yy at m d s, now in epoch seconds
   now DAY-SECOND {: sec:n :} DAYS>YMD {: cy:n cm:n cd:n :}
   cy DATE-YEARS-AHEAD + {: top:n :}
   top  top yy - DATE-CENTURY-YEARS mod - {: y:n :}
   m d s YEAR-KEY  cm cd sec YEAR-KEY >  y top = and
   y m d VALID-YMD? 0= or
   IF y DATE-CENTURY-YEARS - ELSE y THEN ;

: STAMP ( ^u8 n n n [ n n n n n -- n ] -- option<n> )
   {: a:^ base:n width:n now:n year :}   \ year: ( yy m d s now -- y )
   a base DATE-STAMP-DAY + DATE-PART-LEN N MATCH option
     none OF OPTION:NONE ENDOF
     some OF
        {: d:n :}
        a base DATE-STAMP-MONTH + MONTH-NAME MATCH option
          none OF OPTION:NONE ENDOF
          some OF
             {: m:n :}
             base DATE-STAMP-YEAR + {: at:n :}
             a at width N MATCH option
               none OF OPTION:NONE ENDOF
               some OF
                  {: yy:n :}
                  a at width + 1+ CLOCK MATCH option
                    none OF OPTION:NONE ENDOF
                    some OF
                       {: s:n :}
                       yy m d s now year execute m d s MOMENT
                    ENDOF
                  ;MATCH
               ENDOF
             ;MATCH
          ENDOF
        ;MATCH
     ENDOF
   ;MATCH ;

: IMF ( ^u8 -- option<n> )
   {: a:^ :}
   a DATE-NAME-LEN true DAY-NAME? 0= IF OPTION:NONE exit THEN
   a DATE-NAME-LEN DATE-YEAR-LEN 0 [: 2drop 2drop ;] STAMP ;

: RFC850 ( str n -- option<n> )
   {: a:^ k:n now:n :}   \ k bytes of day name
   a k false DAY-NAME? 0= IF OPTION:NONE exit THEN
   a k DATE-PART-LEN now [: FULL-YEAR ;] STAMP ;

: ASCTIME-DAY ( ^u8 -- option<n> )
   {: a:^ :}   \ two digits, or a space and one
   a DATE-ASCTIME-DAY + c@ STR:SPACE =
   IF a DATE-ASCTIME-DAY 1+ 1 N exit THEN
   a DATE-ASCTIME-DAY DATE-PART-LEN N ;

: ASCTIME ( ^u8 -- option<n> )
   {: a:^ :}
   a DATE-NAME-LEN true DAY-NAME? 0= IF OPTION:NONE exit THEN
   a DATE-ASCTIME-MONTH MONTH-NAME MATCH option
     none OF OPTION:NONE ENDOF
     some OF
        {: m:n :}
        a ASCTIME-DAY MATCH option
          none OF OPTION:NONE ENDOF
          some OF
             {: d:n :}
             a DATE-ASCTIME-YEAR DATE-YEAR-LEN N MATCH option
               none OF OPTION:NONE ENDOF
               some OF
                  {: y:n :}
                  a DATE-ASCTIME-CLOCK CLOCK MATCH option
                    none OF OPTION:NONE ENDOF
                    some OF {: s:n :} y m d s MOMENT ENDOF
                  ;MATCH
               ENDOF
             ;MATCH
          ENDOF
        ;MATCH
     ENDOF
   ;MATCH ;

public

: PARSE-HTTP ( str n -- option<n> )
   {: a:^ u:n now:n :}   \ SOME epoch seconds; now serves RFC 850's year
   a u 0 IMF-TEMPLATE TEMPLATE? IF a IMF exit THEN
   a u 0 ASCTIME-TEMPLATE TEMPLATE? IF a ASCTIME exit THEN
   u RFC850-TEMPLATE nip - {: k:n :}
   a u k RFC850-TEMPLATE TEMPLATE? IF a k now RFC850 exit THEN
   OPTION:NONE ;

;package
