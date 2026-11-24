split-str
\ string.f - checked byte-string helpers over pointer and length (package STR);
\ no word assumes NUL termination. The builders throw E-STR-CAPACITY instead of
\ truncating.
\ STORAGE CLASS: the SB builder is TASK-LOCAL (SB-BUF); the BUF-* family is
\ CALLER-OWNED, the builder for one shared by two tasks; the rest is pure.

s" src/lib/error.f" required
s" src/lib/adt/option.f" required

STRING-ABI:SB-BUF-BYTES constant SB-CAP

package STR
private EXPORT BOOT:data-base
EXPORT STRING-ABI:SB-BUF-OFF
EXPORT STRING-ABI:SB-LEN-OFF
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

: INDEX-OF ( str n -- option<idx> ) {: a:^ u:n c:n :}   \ SOME first index of byte c, else NONE
   0 begin dup u < while
      dup a + c@ c = if >IDX OPTION:SOME exit then
      1+
   repeat drop OPTION:NONE ;

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

: PARSE-POS ( str -- option<n> ) {: a:^ u:n :}   \ SOME parsed nonnegative bounded i64, else NONE
   a u DIGITS? 0= if OPTION:NONE exit then
   a u MAX-I64$ I64-DIGITS DIGITS<= 0= if OPTION:NONE exit then
   0 0 begin dup u < while
      swap BASE * over a + c@ DIGIT-VALUE + swap
      1+
   repeat drop OPTION:SOME ;

: PARSE-NEG ( str -- option<n> ) {: a:^ u:n :}   \ SOME parsed negative bounded i64, else NONE
   a u DIGITS? 0= if OPTION:NONE exit then
   a u MIN-I64$ I64-DIGITS DIGITS<= 0= if OPTION:NONE exit then
   0 0 begin dup u < while
      swap BASE * over a + c@ DIGIT-VALUE - swap
      1+
   repeat drop OPTION:SOME ;

: >NUMBER? ( str -- option<n> ) {: a:^ u:n :}   \ SOME optional-sign bounded i64, else NONE
   u 0= if OPTION:NONE exit then
   a c@ MINUS = if
      u 1 = if OPTION:NONE exit then
      a 1+ u 1- PARSE-NEG exit
   then
   a c@ PLUS = if
      u 1 = if OPTION:NONE exit then
      a 1+ u 1- PARSE-POS exit
   then
   a u PARSE-POS ;

\ Sixteen digits fill the cell, so the top digit sets the sign bit and
\ FFFFFFFFFFFFFFFF is -1: the bit pattern, not a bounded magnitude.
: HEX>NUMBER? ( str -- option<n> )   \ SOME the cell the 1..16 hex digits spell, else NONE
   {: a:^ u:n :}
   u 0= if OPTION:NONE exit then
   u HEX-MAX-DIGITS > if OPTION:NONE exit then
   0 0 begin dup u < while
      dup a + c@ dup HEX-DIGIT? 0= if drop 2drop OPTION:NONE exit then
      HEX-DIGIT-VALUE rot HEX-BASE * + swap
      1+
   repeat drop OPTION:SOME ;

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

\ An offset into the whole text; NONE for a start outside [0, length].
: FIND-SUB-FROM ( str str n -- option<idx> )   \ SOME first offset of b in a at or after start, else NONE
   {: a:^ u:n b:^ v:n start:n :}
   start 0 < start u > or if OPTION:NONE exit then
   v 0= if start >IDX OPTION:SOME exit then
   start begin dup u v - <= while
      dup a + v b v = if >IDX OPTION:SOME exit then
      1+
   repeat drop OPTION:NONE ;

: FIND-SUB ( str str -- option<idx> )   \ SOME first offset of b in a (empty needle -> SOME 0), else NONE
   0 FIND-SUB-FROM ;

: CONTAINS? ( str str -- bool )
   FIND-SUB MATCH option
     none OF FALSE ENDOF
     some OF drop TRUE ENDOF
   ;MATCH ;

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

global

\ TASK-LOCAL: `data-base` is the running task's DATA region, at STRING-ABI's
\ offsets (src/habu/layout.f). This module is baked into the engine below
\ src/lib/ffi-abi.f, which requires it back, so it cannot require
\ src/lib/task.f (the engine build fails on an undefined STR:= in TOK-IS?). A
\ fresh region is zeroed, so a new task's cursor starts at zero.
: SB-BUF ( -- ^u8 )
   data-base SB-BUF-OFF + BYTE-VIEW ;

: SB-LEN ( -- ^n )
   data-base SB-LEN-OFF + ;
;package

: BUFFER: ( n -- )
   create allot does> ( -- ^u8 ) ;

: SCRATCH-BUFFER: ( n -- )
   create SCRATCH-ALLOT does> ( -- ^u8 ) ;

: STR-LEN ( n -- len )
   >LEN ;

: STR-OFF ( n -- off )
   >OFF ;

: STR-COUNT ( n -- count )
   >COUNT ;

: SB-CHECK-LEN-ROOM ( len -- ) {: add :}
   add LEN>N SB-CAP SB-LEN @ - > if E-STR-CAPACITY throw then ;

: SB-CHECK-ROOM ( n -- )
   STR-LEN SB-CHECK-LEN-ROOM ;

: SB-RESET ( -- )
   0 SB-LEN ! ;

: SB-APPEND ( str -- ) {: a:^ u :}
   u SB-CHECK-ROOM
   a SB-BUF SB-LEN @ + u BYTE-COPY
   SB-LEN @ u + SB-LEN ! ;

: SB-APPEND-C ( n -- ) {: c :}
   1 SB-CHECK-ROOM
   c SB-BUF SB-LEN @ + c!
   SB-LEN @ 1+ SB-LEN ! ;

: SB$ ( -- str )
   SB-BUF SB-LEN @ ;

: BUF-CHECK-LEN ( len len ^len -- )
   {: add cap lenp:^ :}
   lenp @ LEN>N cap LEN>N > if E-STR-CAPACITY throw then
   add LEN>N cap LEN>N lenp @ LEN>N - > if E-STR-CAPACITY throw then ;

: BUF-RESET ( ^len -- )
   0 >LEN swap ! ;

: BUF-LEN@ ( ^len -- n )
   @ LEN>N ;

: BUF-APPEND-LEN ( str ^u8 len ^len -- )
   {: src:^ u dst:^ cap lenp:^ :}
   u STR-LEN cap lenp BUF-CHECK-LEN
   src dst lenp @ LEN>N + u BYTE-COPY
   lenp @ LEN>N u + >LEN lenp ! ;

: BUF-APPEND ( str ^u8 n ^len -- )
   {: src:^ u dst:^ cap lenp:^ :}
   src u dst cap STR-LEN lenp BUF-APPEND-LEN ;

: BUF-APPEND-C ( n ^u8 n ^len -- )
   {: c dst:^ cap lenp:^ :}
   1 >LEN cap STR-LEN lenp BUF-CHECK-LEN
   c dst lenp @ LEN>N + c!
   lenp @ LEN>N 1+ >LEN lenp ! ;
