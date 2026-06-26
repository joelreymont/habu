\ image-bytes.f - shared executable image byte cursor and patch helpers.

$90000 constant MSIZE
$1002 constant M-MAP-PRIVATE-ANON
75 constant M-BOUNDS-RC
variable MBUF-A
variable MP
variable MLEN
s" MLEN" s" -- ptr n" TRUST

: M-ALLOC-BUF ( -- n )
   0 MSIZE 3 M-MAP-PRIVATE-ANON -1 0 mmap
   dup 0 < if s" image-bytes: mmap failed" 74 die then ;

: M-ENSURE-BUF ( -- )
   MBUF-A @ 0= if M-ALLOC-BUF MBUF-A ! then ;

: MBUF ( -- ptr u8 )
   M-ENSURE-BUF
   MBUF-A @ ;
s" MBUF" s" -- ptr u8" TRUST

: MP@ ( -- ptr u8 ) MP @ ;
s" MP@" s" -- ptr u8" TRUST

: M-RESET ( -- )
   MBUF MP ! ;

: M-HERE ( -- n )
   MP@ MBUF - ;

: M-FAIL ( ptr u8 n -- )
   2drop
   M-BOUNDS-RC throw ;

: M-CHECK-N ( n -- )
   dup 0 < if s" image-bytes: negative span" M-FAIL then
   dup MSIZE > if s" image-bytes: span exceeds buffer" M-FAIL then
   drop ;

: M-LEN ( n -- len )
   dup M-CHECK-N
   >LEN ;

: M-OFF ( n -- off )
   dup M-CHECK-N
   >OFF ;

: M-ROOM1 ( -- )
   M-HERE MSIZE >= if s" image-bytes: cursor exceeds buffer" M-FAIL then ;

: M-CHECK-ROOM ( len -- ) {: u :}
   M-HERE u LEN>N + MSIZE > if s" image-bytes: write exceeds buffer" M-FAIL then ;

: M8 ( n -- ) {: b :}
   M-ROOM1
   b MP@ c!
   MP@ 1 + MP ! ;

: M16 ( n -- ) {: h :}
   h M8
   h 8 rshift M8 ;

: M32 ( n -- ) {: w :}
   w M16
   w 16 rshift M16 ;

: M64 ( n -- ) {: x :}
   x M32
   x 32 rshift M32 ;

: M-ZEROS-LEN ( len -- ) {: u :}
   u M-CHECK-ROOM
   u LEN>N begin dup 0 > while 0 M8 1 - repeat drop ;

: M-ZEROS ( n -- )
   M-LEN M-ZEROS-LEN ;

: M-BYTES-LEN ( ptr u8 len -- ) {: a:ptr u :}
   u M-CHECK-ROOM
   0 begin dup u LEN>N < while
      dup a + c@ M8
      1 +
   repeat drop ;

: M-BYTES ( ptr u8 n -- )
   M-LEN M-BYTES-LEN ;

: M-NAME16-LEN ( ptr u8 len -- ) {: a:ptr u :}
   u LEN>N 16 > if s" image-bytes: name exceeds 16 bytes" M-FAIL then
   a u M-BYTES-LEN
   16 u LEN>N - M-ZEROS ;

: M-NAME16 ( ptr u8 n -- )
   M-LEN M-NAME16-LEN ;

: M-PAD-OFF ( off -- ) {: off :}
   off OFF>N M-HERE - M-ZEROS ;

: M-PAD ( n -- )
   M-OFF M-PAD-OFF ;

: M-LE32@ ( off -- n ) {: off :}
   MBUF off OFF>N + {: a:ptr :}
   a c@
   a 1 + c@ 8 lshift or
   a 2 + c@ 16 lshift or
   a 3 + c@ 24 lshift or ;

: M-LE32! ( n off -- ) {: w off :}
   MBUF off OFF>N + {: a:ptr :}
   w $FF and a c!
   w 8 rshift $FF and a 1 + c!
   w 16 rshift $FF and a 2 + c!
   w 24 rshift $FF and a 3 + c! ;

: M-LE64! ( n off -- )
   {: x off :}
   x off M-LE32!
   x 32 rshift off OFF>N 4 + M-OFF M-LE32! ;

variable MBC

: M-BE-RESET ( off -- )
   OFF>N MBC ! ;

: M-BE-HERE ( -- n )
   MBC @ ;

: M-BE-PTR ( -- ptr u8 )
   MBUF MBC @ + ;

: M-BE-SKIP ( len -- )
   LEN>N MBC @ + MBC ! ;

: M-BE8 ( n -- ) {: c :}
   c M-BE-PTR c!
   1 M-LEN M-BE-SKIP ;

: M-BE32 ( n -- ) {: w :}
   w 24 rshift $FF and M-BE8
   w 16 rshift $FF and M-BE8
   w 8 rshift $FF and M-BE8
   w $FF and M-BE8 ;

: M-BE64 ( n -- ) {: x :}
   x 32 rshift M-BE32
   x $FFFFFFFF and M-BE32 ;

: M-BE-BYTES-LEN ( ptr u8 len -- ) {: a:ptr u :}
   0 begin dup u LEN>N < while
      dup a + c@ M-BE8
      1 +
   repeat drop ;

: M-BE-BYTES ( ptr u8 n -- )
   M-LEN M-BE-BYTES-LEN ;
