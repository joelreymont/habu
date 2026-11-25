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

: =CI ( str str -- bool ) {: s:str t:str :}
   s STR:LEN t STR:LEN <> if 0 0= 0= exit then
   0 begin dup s STR:LEN < while
      dup s STR:DATA + c@ ASCII-LOWER over t STR:DATA + c@ ASCII-LOWER <> if
         drop 0 0= 0= exit
      then
      1+
   repeat drop 0 0= ;

: COUNT-CHAR ( str n -- n ) {: s:str c :}
   0 0 begin dup s STR:LEN < while
      dup s STR:DATA + c@ c = if swap 1+ swap then
      1+
   repeat drop ;

: LTRIM ( str -- str ) {: s:str :}
   0 begin dup s STR:LEN < while
      dup s STR:DATA + c@
      dup SPACE = over TAB = or over LF = or swap CR = or
      0= if dup s STR:DATA + s STR:LEN rot - >str exit then
      1+
   repeat drop s STR:DATA 0 >str ;

: RTRIM ( str -- str ) {: s:str :}
   s STR:LEN begin dup 0 > while
      s STR:DATA over 1- + c@
      dup SPACE = over TAB = or over LF = or swap CR = or
      if 1- else s STR:DATA swap >str exit then
   repeat drop s STR:DATA 0 >str ;

: TRIM ( str -- str )
   LTRIM RTRIM ;

: SPLIT-NEXT ( str n n -- str n bool ) {: s:str sep start :}
   start 0 < if s STR:DATA 0 >str start FALSE exit then
   start s STR:LEN > if s STR:DATA 0 >str start FALSE exit then
   start begin dup s STR:LEN < while
      dup s STR:DATA + c@ sep = if
         s STR:DATA start + over start - >str swap 1+ TRUE exit
      then
      1+
   repeat drop
   s STR:DATA start + s STR:LEN start - >str s STR:LEN 1+ TRUE ;

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

: DIGITS? ( str -- bool ) {: s:str :}
   s STR:LEN 0= if FALSE exit then
   0 begin dup s STR:LEN < while
      dup s STR:DATA + c@ DIGIT? 0= if drop FALSE exit then
      1+
   repeat drop TRUE ;

: DIGITS<= ( str str -- bool ) {: s:str t:str :}
   s STR:LEN t STR:LEN < if TRUE exit then
   s STR:LEN t STR:LEN > if FALSE exit then
   0 begin dup s STR:LEN < while
      dup s STR:DATA + c@ over t STR:DATA + c@ <> if
         dup s STR:DATA + c@ swap t STR:DATA + c@ < exit
      then
      1+
   repeat drop TRUE ;

: = ( str str -- bool )
   {: s:str t:str :}
   s STR:LEN t STR:LEN <> if 0 0= 0= exit then
   s STR:LEN CELL - {: last:n :}
   0
   \ Read only the complete-cell prefix.
   begin dup last <= while
      dup s STR:DATA + CELL-VIEW @ over t STR:DATA + CELL-VIEW @ <> if
         drop 0 0= 0= exit
      then
      CELL +
   repeat
   begin dup s STR:LEN < while
      dup s STR:DATA + c@ over t STR:DATA + c@ <> if drop 0 0= 0= exit then
      1+
   repeat drop 0 0= ;

: STARTS-WITH? ( str str -- bool ) {: s:str t:str :}
   s STR:LEN t STR:LEN < if 0 0= 0= exit then
   s STR:DATA t STR:LEN >str t = ;

: ENDS-WITH? ( str str -- bool ) {: s:str t:str :}
   s STR:LEN t STR:LEN < if 0 0= 0= exit then
   s STR:DATA s STR:LEN t STR:LEN - + t STR:LEN >str t = ;

: COUNT-SUB ( str str -- n )     \ non-overlapping occurrences of b in a; empty needle -> 0
   {: s:str t:str :}
   t STR:LEN 0= if 0 exit then
   0 0 begin dup s STR:LEN t STR:LEN - <= while
      dup s STR:DATA + t STR:LEN >str t = if swap 1+ swap t STR:LEN + else 1+ then
   repeat drop ;

\ Unsigned byte order, a shorter equal prefix first; no case, locale or UTF-8.
: < ( str str -- bool )
   {: s:str t:str :}
   0 begin dup s STR:LEN t STR:LEN min < while
      dup s STR:DATA + c@ over t STR:DATA + c@
      2dup <> if < swap drop exit then
      2drop 1+
   repeat drop s STR:LEN t STR:LEN < ;

;package

: BUFFER: ( n -- )
   create allot does> ( -- ^u8 ) ;

: SCRATCH-BUFFER: ( n -- )
   create SCRATCH-ALLOT does> ( -- ^u8 ) ;
