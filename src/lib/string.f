split-str
\ string.f - checked byte-string helpers over pointer and length (package STR);
\ no word assumes NUL termination. It loads in the kernel after cell.f and
\ pointer-storage.f and requires nothing.
\ STORAGE CLASS: pure.

package STR
public
EXPORT STR:data
EXPORT STR:len
9 constant TAB
10 constant LF
13 constant CR
32 constant SPACE
64 constant BEFORE-A
91 constant AFTER-Z
96 constant BEFORE-LOWER-A
123 constant AFTER-LOWER-Z
43 constant PLUS
45 constant MINUS
48 constant ZERO
10 constant BASE
97 constant LOWER-A
102 constant LOWER-F
16 constant HEX-BASE
16 constant HEX-MAX-DIGITS
19 constant I64-DIGITS
255 constant BYTE-MAX
$7FFFFFFFFFFFFFFF constant MAX-I64
MAX-I64 negate 1 - constant MIN-I64
create MAX-I64$ 57 c, 50 c, 50 c, 51 c, 51 c, 55 c, 50 c, 48 c, 51 c, 54 c, 56 c, 53 c, 52 c, 55 c, 55 c, 53 c, 56 c, 48 c, 55 c,
create MIN-I64$ 57 c, 50 c, 50 c, 51 c, 51 c, 55 c, 50 c, 48 c, 51 c, 54 c, 56 c, 53 c, 52 c, 55 c, 55 c, 53 c, 56 c, 48 c, 56 c,

: TRUE ( -- bool )
   0 0= ;

: FALSE ( -- bool )
   TRUE 0= ;

: ASCII-LOWER ( n -- n )
   dup BEFORE-A > over AFTER-Z < and if SPACE + then ;

: ASCII-UPPER ( n -- n )
   dup BEFORE-LOWER-A > over AFTER-LOWER-Z < and if SPACE - then ;

: =CI ( str str -- bool ) {: a:^ u:n b:^ v:n :}
   u v <> if 0 0= 0= exit then
   0 begin dup u < while
      dup a + c@ ASCII-LOWER over b + c@ ASCII-LOWER <> if
         drop 0 0= 0= exit
      then
      1+
   repeat drop 0 0= ;

: COUNT-CHAR ( str n -- n ) {: a:^ u c :}
   0 0 begin dup u < while
      dup a + c@ c = if swap 1+ swap then
      1+
   repeat drop ;

: LTRIM ( str -- str ) {: a:^ u :}
   0 begin dup u < while
      dup a + c@
      dup SPACE = over TAB = or over LF = or swap CR = or
      0= if dup a + u rot - exit then
      1+
   repeat drop a 0 ;

: RTRIM ( str -- str ) {: a:^ u :}
   u begin dup 0 > while
      a over 1- + c@
      dup SPACE = over TAB = or over LF = or swap CR = or
      if 1- else a swap exit then
   repeat drop a 0 ;

: TRIM ( str -- str )
   LTRIM RTRIM ;

: SPLIT-NEXT ( str n n -- str n bool ) {: a:^ u sep start :}
   start 0 < if a 0 start FALSE exit then
   start u > if a 0 start FALSE exit then
   start begin dup u < while
      dup a + c@ sep = if
         a start + over start - rot 1+ TRUE exit
      then
      1+
   repeat drop
   a start + u start - u 1+ TRUE ;

: DIGIT? ( n -- bool )
   dup ZERO 1- > swap ZERO BASE + < and ;

: DIGIT-VALUE ( n -- n )
   ZERO - ;

: HEX-DIGIT? ( n -- bool )   \ 0-9, A-F or a-f
   dup DIGIT? if drop TRUE exit then
   ASCII-LOWER dup LOWER-A < swap LOWER-F > or 0= ;

: ALNUM? ( n -- bool )   \ 0-9, A-Z or a-z
   dup DIGIT? if drop TRUE exit then
   ASCII-LOWER dup BEFORE-LOWER-A >
   swap AFTER-LOWER-Z < and ;

: HEX-DIGIT-VALUE ( n -- n )   \ the value of a byte HEX-DIGIT? accepts
   dup DIGIT? if DIGIT-VALUE exit then
   ASCII-LOWER LOWER-A - BASE + ;

: DIGITS? ( str -- bool ) {: a:^ u :}
   u 0= if FALSE exit then
   0 begin dup u < while
      dup a + c@ DIGIT? 0= if drop FALSE exit then
      1+
   repeat drop TRUE ;

: DIGITS<= ( str str -- bool ) {: a:^ u b:^ v :}
   u v < if TRUE exit then
   u v > if FALSE exit then
   0 begin dup u < while
      dup a + c@ over b + c@ <> if
         dup a + c@ swap b + c@ < exit
      then
      1+
   repeat drop TRUE ;

: = ( str str -- bool )
   {: a:^ u:n b:^ v:n :}
   u v <> if 0 0= 0= exit then
   u CELL - {: last:n :}
   0
   \ Read only the complete-cell prefix.
   begin dup last <= while
      dup a + CELL-VIEW @ over b + CELL-VIEW @ <> if
         drop 0 0= 0= exit
      then
      CELL +
   repeat
   begin dup u < while
      dup a + c@ over b + c@ <> if drop 0 0= 0= exit then
      1+
   repeat drop 0 0= ;

: STARTS-WITH? ( str str -- bool ) {: a:^ u b:^ v :}
   u v < if 0 0= 0= exit then
   a v b v = ;

: ENDS-WITH? ( str str -- bool ) {: a:^ u b:^ v :}
   u v < if 0 0= 0= exit then
   a u v - + v b v = ;

: COUNT-SUB ( str str -- n )     \ non-overlapping occurrences of b in a; empty needle -> 0
   {: a:^ u:n b:^ v:n :}
   v 0= if 0 exit then
   0 0 begin dup u v - <= while
      dup a + v b v = if swap 1+ swap v + else 1+ then
   repeat drop ;

\ Unsigned byte order, a shorter equal prefix first; no case, locale or UTF-8.
: < ( str str -- bool )
   {: a:^ u:n b:^ v:n :}
   0 begin dup u v min < while
      dup a + c@ over b + c@
      2dup <> if < swap drop exit then
      2drop 1+
   repeat drop u v < ;

;package

: BUFFER: ( n -- )
   create allot does> ( -- ^u8 ) ;

: SCRATCH-BUFFER: ( n -- )
   create SCRATCH-ALLOT does> ( -- ^u8 ) ;
