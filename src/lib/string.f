\ string.f - checked byte-string helpers over pointer and length; no word assumes
\ NUL termination. The builders throw E-STR-CAPACITY instead of truncating.
\ STORAGE CLASS: the SB builder is TASK-LOCAL (SB-BUF); the BUF-* family is
\ CALLER-OWNED, the builder for one shared by two tasks; the rest is pure.

s" src/lib/error.f" required
s" src/lib/adt/option.f" required

9 constant STR-TAB
10 constant STR-LF
13 constant STR-CR
32 constant STR-SPACE
64 constant STR-BEFORE-A
91 constant STR-AFTER-Z
96 constant STR-BEFORE-LOWER-A
123 constant STR-AFTER-LOWER-Z
43 constant STR-PLUS
45 constant STR-MINUS
48 constant STR-ZERO
10 constant STR-BASE
97 constant STR-LOWER-A
102 constant STR-LOWER-F
16 constant STR-HEX-BASE
16 constant STR-HEX-MAX-DIGITS
19 constant STR-I64-DIGITS
255 constant STR-BYTE-MAX
STRING-ABI:SB-BUF-BYTES constant SB-CAP
$7FFFFFFFFFFFFFFF constant STR-MAX-I64
STR-MAX-I64 negate 1 - constant STR-MIN-I64

\ TASK-LOCAL: `data-base` is the running task's DATA region, at STRING-ABI's
\ offsets (src/habu/layout.f). This module is baked into the engine below
\ src/lib/ffi-abi.f, which requires it back, so it cannot require
\ src/lib/task.f (the engine build fails on an undefined STR= in TOK-IS?). A
\ fresh region is zeroed, so a new task's cursor starts at zero.
: SB-BUF ( -- ^u8 )
   data-base STRING-ABI:SB-BUF-OFF + BYTE-VIEW ;

: SB-LEN ( -- ^n )
   data-base STRING-ABI:SB-LEN-OFF + ;

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

create STR-MAX-I64$ 57 c, 50 c, 50 c, 51 c, 51 c, 55 c, 50 c, 48 c, 51 c, 54 c, 56 c, 53 c, 52 c, 55 c, 55 c, 53 c, 56 c, 48 c, 55 c,
create STR-MIN-I64$ 57 c, 50 c, 50 c, 51 c, 51 c, 55 c, 50 c, 48 c, 51 c, 54 c, 56 c, 53 c, 52 c, 55 c, 55 c, 53 c, 56 c, 48 c, 56 c,

: STR-TRUE ( -- bool )
   0 0= ;

: STR-FALSE ( -- bool )
   STR-TRUE 0= ;

: ASCII-LOWER ( n -- n )
   dup STR-BEFORE-A > over STR-AFTER-Z < and if STR-SPACE + then ;

: ASCII-UPPER ( n -- n )
   dup STR-BEFORE-LOWER-A > over STR-AFTER-LOWER-Z < and if STR-SPACE - then ;

: STR= ( ^u8 n ^u8 n -- bool )
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

\ Unsigned byte order, a shorter equal prefix first; no case, locale or UTF-8.
: STR< ( ^u8 n ^u8 n -- bool )
   {: a:^ u:n b:^ v:n :}
   0 begin dup u v min < while
      dup a + c@ over b + c@
      2dup <> if < swap drop exit then
      2drop 1+
   repeat drop u v < ;

: STR=CI ( ^u8 n ^u8 n -- bool ) {: a:^ u:n b:^ v:n :}
   u v <> if 0 0= 0= exit then
   0 begin dup u < while
      dup a + c@ ASCII-LOWER over b + c@ ASCII-LOWER <> if
         drop 0 0= 0= exit
      then
      1+
   repeat drop 0 0= ;

: STARTS-WITH? ( ^u8 n ^u8 n -- bool ) {: a:^ u b:^ v :}
   u v < if 0 0= 0= exit then
   a v b v STR= ;

: ENDS-WITH? ( ^u8 n ^u8 n -- bool ) {: a:^ u b:^ v :}
   u v < if 0 0= 0= exit then
   a u v - + v b v STR= ;

\ An offset into the whole text; NONE for a start outside [0, length].
: FIND-SUB-FROM ( ^u8 n ^u8 n n -- option<idx> )   \ SOME first offset of b in a at or after start, else NONE
   {: a:^ u:n b:^ v:n start:n :}
   start 0 < start u > or if OPTION:NONE exit then
   v 0= if start >IDX OPTION:SOME exit then
   start begin dup u v - <= while
      dup a + v b v STR= if >IDX OPTION:SOME exit then
      1+
   repeat drop OPTION:NONE ;

: FIND-SUB ( ^u8 n ^u8 n -- option<idx> )   \ SOME first offset of b in a (empty needle -> SOME 0), else NONE
   0 FIND-SUB-FROM ;

: CONTAINS? ( ^u8 n ^u8 n -- bool )
   FIND-SUB MATCH option
     none OF STR-FALSE ENDOF
     some OF drop STR-TRUE ENDOF
   ;MATCH ;

: INDEX-OF ( ^u8 n n -- option<idx> ) {: a:^ u:n c:n :}   \ SOME first index of byte c, else NONE
   0 begin dup u < while
      dup a + c@ c = if >IDX OPTION:SOME exit then
      1+
   repeat drop OPTION:NONE ;

: COUNT-CHAR ( ^u8 n n -- n ) {: a:^ u c :}
   0 0 begin dup u < while
      dup a + c@ c = if swap 1+ swap then
      1+
   repeat drop ;

: COUNT-SUB ( ^u8 n ^u8 n -- n )     \ non-overlapping occurrences of b in a; empty needle -> 0
   {: a:^ u:n b:^ v:n :}
   v 0= if 0 exit then
   0 0 begin dup u v - <= while
      dup a + v b v STR= if swap 1+ swap v + else 1+ then
   repeat drop ;

: LTRIM ( ^u8 n -- ^u8 n ) {: a:^ u :}
   0 begin dup u < while
      dup a + c@
      dup STR-SPACE = over STR-TAB = or over STR-LF = or swap STR-CR = or
      0= if dup a + u rot - exit then
      1+
   repeat drop a 0 ;

: RTRIM ( ^u8 n -- ^u8 n ) {: a:^ u :}
   u begin dup 0 > while
      a over 1- + c@
      dup STR-SPACE = over STR-TAB = or over STR-LF = or swap STR-CR = or
      if 1- else a swap exit then
   repeat drop a 0 ;

: TRIM ( ^u8 n -- ^u8 n )
   LTRIM RTRIM ;

: SB-CHECK-LEN-ROOM ( len -- ) {: add :}
   add LEN>N SB-CAP SB-LEN @ - > if E-STR-CAPACITY throw then ;

: SB-CHECK-ROOM ( n -- )
   STR-LEN SB-CHECK-LEN-ROOM ;

: SB-RESET ( -- )
   0 SB-LEN ! ;

: SB-APPEND-LEN ( ^u8 len -- ) {: a:^ u :}
   u SB-CHECK-LEN-ROOM
   a SB-BUF SB-LEN @ + u BYTE-COPY-LEN
   SB-LEN @ u LEN>N + SB-LEN ! ;

: SB-APPEND ( ^u8 n -- )
   STR-LEN SB-APPEND-LEN ;

: SB-APPEND-C ( n -- ) {: c :}
   1 SB-CHECK-ROOM
   c SB-BUF SB-LEN @ + c!
   SB-LEN @ 1+ SB-LEN ! ;

: SB$ ( -- ^u8 n )
   SB-BUF SB-LEN @ ;

: BUF-CHECK-LEN ( len len ^len -- )
   {: add cap lenp:^ :}
   lenp @ LEN>N cap LEN>N > if E-STR-CAPACITY throw then
   add LEN>N cap LEN>N lenp @ LEN>N - > if E-STR-CAPACITY throw then ;

: BUF-RESET ( ^len -- )
   0 >LEN swap ! ;

: BUF-LEN@ ( ^len -- n )
   @ LEN>N ;

: BUF-APPEND-LEN ( ^u8 len ^u8 len ^len -- )
   {: src:^ u dst:^ cap lenp:^ :}
   u cap lenp BUF-CHECK-LEN
   src dst lenp @ LEN>N + u BYTE-COPY-LEN
   lenp @ LEN>N u LEN>N + >LEN lenp ! ;

: BUF-APPEND ( ^u8 n ^u8 n ^len -- )
   {: src:^ u dst:^ cap lenp:^ :}
   src u STR-LEN dst cap STR-LEN lenp BUF-APPEND-LEN ;

: BUF-APPEND-C ( n ^u8 n ^len -- )
   {: c dst:^ cap lenp:^ :}
   1 >LEN cap STR-LEN lenp BUF-CHECK-LEN
   c dst lenp @ LEN>N + c!
   lenp @ LEN>N 1+ >LEN lenp ! ;

: SPLIT-NEXT ( ^u8 n n n -- ^u8 n n bool ) {: a:^ u sep start :}
   start 0 < if a 0 start STR-FALSE exit then
   start u > if a 0 start STR-FALSE exit then
   start begin dup u < while
      dup a + c@ sep = if
         a start + over start - rot 1+ STR-TRUE exit
      then
      1+
   repeat drop
   a start + u start - u 1+ STR-TRUE ;

: STR-DIGIT? ( n -- bool )
   dup STR-ZERO 1- > swap STR-ZERO STR-BASE + < and ;

: STR-DIGIT-VALUE ( n -- n )
   STR-ZERO - ;

: STR-HEX-DIGIT? ( n -- bool )   \ 0-9, A-F or a-f
   dup STR-DIGIT? if drop STR-TRUE exit then
   ASCII-LOWER dup STR-LOWER-A < swap STR-LOWER-F > or 0= ;

: STR-HEX-DIGIT-VALUE ( n -- n )   \ the value of a byte STR-HEX-DIGIT? accepts
   dup STR-DIGIT? if STR-DIGIT-VALUE exit then
   ASCII-LOWER STR-LOWER-A - STR-BASE + ;

: STR-DIGITS? ( ^u8 n -- bool ) {: a:^ u :}
   u 0= if STR-FALSE exit then
   0 begin dup u < while
      dup a + c@ STR-DIGIT? 0= if drop STR-FALSE exit then
      1+
   repeat drop STR-TRUE ;

: STR-DIGITS<= ( ^u8 n ^u8 n -- bool ) {: a:^ u b:^ v :}
   u v < if STR-TRUE exit then
   u v > if STR-FALSE exit then
   0 begin dup u < while
      dup a + c@ over b + c@ <> if
         dup a + c@ swap b + c@ < exit
      then
      1+
   repeat drop STR-TRUE ;

: STR-PARSE-POS ( ^u8 n -- option<n> ) {: a:^ u:n :}   \ SOME parsed nonnegative bounded i64, else NONE
   a u STR-DIGITS? 0= if OPTION:NONE exit then
   a u STR-MAX-I64$ STR-I64-DIGITS STR-DIGITS<= 0= if OPTION:NONE exit then
   0 0 begin dup u < while
      swap STR-BASE * over a + c@ STR-DIGIT-VALUE + swap
      1+
   repeat drop OPTION:SOME ;

: STR-PARSE-NEG ( ^u8 n -- option<n> ) {: a:^ u:n :}   \ SOME parsed negative bounded i64, else NONE
   a u STR-DIGITS? 0= if OPTION:NONE exit then
   a u STR-MIN-I64$ STR-I64-DIGITS STR-DIGITS<= 0= if OPTION:NONE exit then
   0 0 begin dup u < while
      swap STR-BASE * over a + c@ STR-DIGIT-VALUE - swap
      1+
   repeat drop OPTION:SOME ;

: STR>NUMBER? ( ^u8 n -- option<n> ) {: a:^ u:n :}   \ SOME optional-sign bounded i64, else NONE
   u 0= if OPTION:NONE exit then
   a c@ STR-MINUS = if
      u 1 = if OPTION:NONE exit then
      a 1+ u 1- STR-PARSE-NEG exit
   then
   a c@ STR-PLUS = if
      u 1 = if OPTION:NONE exit then
      a 1+ u 1- STR-PARSE-POS exit
   then
   a u STR-PARSE-POS ;

\ Sixteen digits fill the cell, so the top digit sets the sign bit and
\ FFFFFFFFFFFFFFFF is -1: the bit pattern, not a bounded magnitude.
: HEX>NUMBER? ( ^u8 n -- option<n> )   \ SOME the cell the 1..16 hex digits spell, else NONE
   {: a:^ u:n :}
   u 0= if OPTION:NONE exit then
   u STR-HEX-MAX-DIGITS > if OPTION:NONE exit then
   0 0 begin dup u < while
      dup a + c@ dup STR-HEX-DIGIT? 0= if drop 2drop OPTION:NONE exit then
      STR-HEX-DIGIT-VALUE rot STR-HEX-BASE * + swap
      1+
   repeat drop OPTION:SOME ;
