\ string.f - checked byte-string helpers.

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
19 constant STR-I64-DIGITS
255 constant STR-BYTE-MAX
1024 constant SB-CAP
$7FFFFFFFFFFFFFFF constant STR-MAX-I64
STR-MAX-I64 negate 1 - constant STR-MIN-I64

create SB-BUF SB-CAP allot
variable SB-LEN

: BUFFER: ( n -- )
   dup 0 < if E-STR-BOUNDS throw then
   create allot does> ( -- ptr u8 ) ;

: STR-LEN ( n -- len )
   dup 0 < if E-STR-BOUNDS throw then
   >LEN ;

: STR-OFF ( n -- off )
   dup 0 < if E-STR-BOUNDS throw then
   >OFF ;

: STR-COUNT ( n -- count )
   dup 0 < if E-STR-BOUNDS throw then
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

: STR= ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n b:ptr v:n :}
   u v <> if 0 0= 0= exit then
   0 begin dup u < while
      dup a + c@ over b + c@ <> if drop 0 0= 0= exit then
      1+
   repeat drop 0 0= ;

: STR=CI ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n b:ptr v:n :}
   u v <> if 0 0= 0= exit then
   0 begin dup u < while
      dup a + c@ ASCII-LOWER over b + c@ ASCII-LOWER <> if
         drop 0 0= 0= exit
      then
      1+
   repeat drop 0 0= ;

: STARTS-WITH? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u b:ptr v :}
   u v < if 0 0= 0= exit then
   a v b v STR= ;

: ENDS-WITH? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u b:ptr v :}
   u v < if 0 0= 0= exit then
   a u v - + v b v STR= ;

: FIND-SUB ( ptr u8 n ptr u8 n -- n ) {: a:ptr u b:ptr v :}
   v 0= if 0 exit then
   u v < if -1 exit then
   0 begin dup u v - <= while
      dup a + v b v STR= if exit then
      1+
   repeat drop -1 ;

: CONTAINS? ( ptr u8 n ptr u8 n -- bool )
   FIND-SUB 0 < 0= ;

: INDEX-OF ( ptr u8 n n -- n ) {: a:ptr u c :}
   0 begin dup u < while
      dup a + c@ c = if exit then
      1+
   repeat drop -1 ;

: COUNT-CHAR ( ptr u8 n n -- n ) {: a:ptr u c :}
   0 0 begin dup u < while
      dup a + c@ c = if swap 1+ swap then
      1+
   repeat drop ;

: LTRIM ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   0 begin dup u < while
      dup a + c@
      dup STR-SPACE = over STR-TAB = or over STR-LF = or swap STR-CR = or
      0= if dup a + u rot - exit then
      1+
   repeat drop a 0 ;

: RTRIM ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   u begin dup 0 > while
      a over 1- + c@
      dup STR-SPACE = over STR-TAB = or over STR-LF = or swap STR-CR = or
      if 1- else a swap exit then
   repeat drop a 0 ;

: TRIM ( ptr u8 n -- ptr u8 n )
   LTRIM RTRIM ;

: SB-CHECK-LEN-ROOM ( len -- ) {: add :}
   add LEN>N SB-CAP SB-LEN @ - > if E-STR-CAPACITY throw then ;

: SB-CHECK-ROOM ( n -- )
   STR-LEN SB-CHECK-LEN-ROOM ;

: SB-RESET ( -- )
   0 SB-LEN ! ;

: SB-APPEND-LEN ( ptr u8 len -- ) {: a:ptr u :}
   u SB-CHECK-LEN-ROOM
   a SB-BUF SB-LEN @ + u BYTE-COPY-LEN
   SB-LEN @ u LEN>N + SB-LEN ! ;

: SB-APPEND ( ptr u8 n -- )
   STR-LEN SB-APPEND-LEN ;

: SB-APPEND-C ( n -- ) {: c :}
   c 0 < if E-STR-BOUNDS throw then
   c STR-BYTE-MAX > if E-STR-BOUNDS throw then
   1 SB-CHECK-ROOM
   c SB-BUF SB-LEN @ + c!
   SB-LEN @ 1+ SB-LEN ! ;

: SB$ ( -- ptr u8 n )
   SB-BUF SB-LEN @ ;

: BUF-CHECK-LEN ( len len ptr len -- )
   {: add cap lenp:ptr :}
   add LEN>N 0 < if E-STR-BOUNDS throw then
   lenp @ LEN>N 0 < if E-STR-BOUNDS throw then
   lenp @ LEN>N cap LEN>N > if E-STR-CAPACITY throw then
   add LEN>N cap LEN>N lenp @ LEN>N - > if E-STR-CAPACITY throw then ;

: BUF-RESET ( ptr len -- )
   0 >LEN swap ! ;

: BUF-LEN@ ( ptr len -- n )
   @ LEN>N ;

: BUF-APPEND-LEN ( ptr u8 len ptr u8 len ptr len -- )
   {: src:ptr u dst:ptr cap lenp:ptr :}
   u cap lenp BUF-CHECK-LEN
   src dst lenp @ LEN>N + u BYTE-COPY-LEN
   lenp @ LEN>N u LEN>N + >LEN lenp ! ;

: BUF-APPEND ( ptr u8 n ptr u8 n ptr len -- )
   {: src:ptr u dst:ptr cap lenp:ptr :}
   src u STR-LEN dst cap STR-LEN lenp BUF-APPEND-LEN ;

: BUF-APPEND-C ( n ptr u8 n ptr len -- )
   {: c dst:ptr cap lenp:ptr :}
   c 0 < if E-STR-BOUNDS throw then
   c STR-BYTE-MAX > if E-STR-BOUNDS throw then
   1 >LEN cap STR-LEN lenp BUF-CHECK-LEN
   c dst lenp @ LEN>N + c!
   lenp @ LEN>N 1+ >LEN lenp ! ;

: SPLIT-NEXT ( ptr u8 n n n -- ptr u8 n n bool ) {: a:ptr u sep start :}
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

: STR-DIGITS? ( ptr u8 n -- bool ) {: a:ptr u :}
   u 0= if STR-FALSE exit then
   0 begin dup u < while
      dup a + c@ STR-DIGIT? 0= if drop STR-FALSE exit then
      1+
   repeat drop STR-TRUE ;

: STR-DIGITS<= ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u b:ptr v :}
   u v < if STR-TRUE exit then
   u v > if STR-FALSE exit then
   0 begin dup u < while
      dup a + c@ over b + c@ <> if
         dup a + c@ swap b + c@ < exit
      then
      1+
   repeat drop STR-TRUE ;

: STR-PARSE-POS ( ptr u8 n -- n bool ) {: a:ptr u :}
   a u STR-DIGITS? 0= if 0 STR-FALSE exit then
   a u STR-MAX-I64$ STR-I64-DIGITS STR-DIGITS<= 0= if 0 STR-FALSE exit then
   0 0 begin dup u < while
      swap STR-BASE * over a + c@ STR-DIGIT-VALUE + swap
      1+
   repeat drop STR-TRUE ;

: STR-PARSE-NEG ( ptr u8 n -- n bool ) {: a:ptr u :}
   a u STR-DIGITS? 0= if 0 STR-FALSE exit then
   a u STR-MIN-I64$ STR-I64-DIGITS STR-DIGITS<= 0= if 0 STR-FALSE exit then
   0 0 begin dup u < while
      swap STR-BASE * over a + c@ STR-DIGIT-VALUE - swap
      1+
   repeat drop STR-TRUE ;

: STR>NUMBER? ( ptr u8 n -- n bool ) {: a:ptr u :}
   u 0= if 0 STR-FALSE exit then
   a c@ STR-MINUS = if
      u 1 = if 0 STR-FALSE exit then
      a 1+ u 1- STR-PARSE-NEG exit
   then
   a c@ STR-PLUS = if
      u 1 = if 0 STR-FALSE exit then
      a 1+ u 1- STR-PARSE-POS exit
   then
   a u STR-PARSE-POS ;
