\ image-bytes.f - shared executable image byte cursor and patch helpers.

$90000 constant MSIZE
$1002 constant M-MAP-PRIVATE-ANON
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

: M8 ( n -- ) {: b :}
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

: M-HERE ( -- n )
   MP@ MBUF - ;

: M-ZEROS ( n -- ) {: n :}
   n 0 > if
      n begin dup 0 > while 0 M8 1 - repeat drop
   then ;

: M-BYTES ( ptr u8 n -- ) {: a:ptr u :}
   0 begin dup u < while
      dup a + c@ M8
      1 +
   repeat drop ;

: M-NAME16 ( ptr u8 n -- ) {: a:ptr u :}
   a u M-BYTES
   16 u - M-ZEROS ;

: M-PAD ( n -- ) {: off :}
   off M-HERE - M-ZEROS ;

: M-LE32@ ( n -- n ) {: off :}
   MBUF off + {: a:ptr :}
   a c@
   a 1 + c@ 8 lshift or
   a 2 + c@ 16 lshift or
   a 3 + c@ 24 lshift or ;

: M-LE32! ( n n -- ) {: w off :}
   MBUF off + {: a:ptr :}
   w $FF and a c!
   w 8 rshift $FF and a 1 + c!
   w 16 rshift $FF and a 2 + c!
   w 24 rshift $FF and a 3 + c! ;

: M-LE64! ( n n -- )
   {: x off :}
   x off M-LE32!
   x 32 rshift off 4 + M-LE32! ;

variable MBC

: M-BE-RESET ( n -- )
   MBC ! ;

: M-BE-HERE ( -- n )
   MBC @ ;

: M-BE-PTR ( -- ptr u8 )
   MBUF MBC @ + ;

: M-BE-SKIP ( n -- )
   MBC @ + MBC ! ;

: M-BE8 ( n -- ) {: c :}
   c M-BE-PTR c!
   1 M-BE-SKIP ;

: M-BE32 ( n -- ) {: w :}
   w 24 rshift $FF and M-BE8
   w 16 rshift $FF and M-BE8
   w 8 rshift $FF and M-BE8
   w $FF and M-BE8 ;

: M-BE64 ( n -- ) {: x :}
   x 32 rshift M-BE32
   x $FFFFFFFF and M-BE32 ;

: M-BE-BYTES ( ptr u8 n -- ) {: a:ptr u :}
   0 begin dup u < while
      dup a + c@ M-BE8
      1 +
   repeat drop ;
