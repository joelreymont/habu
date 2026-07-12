\ image-bytes.f - shared executable image byte cursor and patch helpers.

\ MSIZE must cover the largest image any writer can emit: __text up to MPAGE
\ ($200000, both targets' loud 73-die code-window guard) plus the largest
\ non-text tail (Mach-O: DATA-CONST-SIZE + MACHO-FIXUPS-SIZE + the ad-hoc
\ signature; ELF: ELF-RW-SZ). The writers assert this bound at load time
\ (MACHO-MSIZE-CHECK / ELF-MSIZE-CHECK) so the code-window guard is always the
\ binding constraint - an M-BOUNDS-RC throw here means a writer bug, not a
\ program-size limit. $220000 exceeds the derived $210000 minimum with margin.
$220000 constant MSIZE
$1002 constant M-MAP-PRIVATE-ANON
75 constant M-BOUNDS-RC
variable MBUF-A
variable MP
variable MLEN
variable M-A
variable M-SRC
variable M-N
variable M-O

: M-ALLOC-BUF ( -- n )
   0 MSIZE 3 M-MAP-PRIVATE-ANON -1 0 mmap
   dup 0 < if s" image-bytes: mmap failed" 74 die then ;

TRUSTED: MBUF-RC>PTR ( n -- ptr u8 ) ;

: MBUF-A-FIELD ( -- ptr ptr u8 )
   MBUF-A 0 ptr-field ;

: MBUF-A@ ( -- ptr u8 )
   MBUF-A-FIELD @ ;

: MBUF-A! ( ptr u8 -- )
   MBUF-A-FIELD ! ;

: M-ENSURE-BUF ( -- )
   MBUF-A@ 0= if M-ALLOC-BUF MBUF-RC>PTR MBUF-A! then ;

: MBUF ( -- ptr u8 )
   M-ENSURE-BUF
   MBUF-A@ ;

: MP@ ( -- ptr u8 ) MP @ ;

: MLEN@ ( -- n )
   MLEN @ ;

: MLEN! ( n -- )
   MLEN ! ;

: M-A-FIELD ( -- ptr ptr u8 )
   M-A 0 ptr-field ;

: M-SRC-FIELD ( -- ptr ptr u8 )
   M-SRC 0 ptr-field ;

: M-A@ ( -- ptr u8 )
   M-A-FIELD @ ;

: M-SRC@ ( -- ptr u8 )
   M-SRC-FIELD @ ;

: M-A! ( ptr u8 -- )
   M-A-FIELD ! ;

: M-SRC! ( ptr u8 -- )
   M-SRC-FIELD ! ;

: M-O@ ( -- off )
   M-O @ >OFF ;

: M-BYTE+ ( ptr u8 n -- ptr u8 )
   + ;

: M-BYTE@ ( ptr u8 n -- n )
   M-BYTE+ c@ ;

: M-RESET ( -- )
   MBUF MP ! ;

: M-HERE ( -- n )
   MP@ MBUF - ;

create M-FAIL-NL 1 allot

\ Report before throwing: the stage/maker drivers exit with the raw throw
\ code, so a swallowed message here becomes a silent build failure (proven:
\ the 104-byte MSIZE overrun exited 75 with no output). The write rc has
\ nowhere further to report on this diagnostic path.
: M-FAIL ( ptr u8 n -- ) {: a:ptr u:n :}
   2 a u write drop
   10 M-FAIL-NL c!
   2 M-FAIL-NL 1 write drop
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

: M-CHECK-ROOM ( len -- )
   LEN>N M-HERE + MSIZE > if s" image-bytes: write exceeds buffer" M-FAIL then ;

: IMG-M8 ( n -- )
   M-ROOM1
   MP@ c!
   MP@ 1 M-BYTE+ MP ! ;

: IMG-M16 ( n -- )
   dup IMG-M8
   8 rshift IMG-M8 ;

: IMG-M32 ( n -- )
   dup IMG-M16
   16 rshift IMG-M16 ;

: IMG-M64 ( n -- )
   dup IMG-M32
   32 rshift IMG-M32 ;

: M-ZEROS-LEN ( len -- )
   dup M-CHECK-ROOM
   LEN>N begin dup 0 > while 0 IMG-M8 1 - repeat drop ;

: M-ZEROS ( n -- )
   M-LEN M-ZEROS-LEN ;

: M-BYTES-LEN ( ptr u8 len -- )
   dup M-CHECK-ROOM
   LEN>N M-N ! M-SRC!
   0 begin dup M-N @ < while
      dup M-SRC@ swap M-BYTE+ c@ IMG-M8
      1 +
   repeat drop ;

: M-BYTES ( ptr u8 n -- )
   M-LEN M-BYTES-LEN ;

: M-NAME16-LEN ( ptr u8 len -- )
   dup LEN>N dup M-N ! $10 > if s" image-bytes: name exceeds 16 bytes" M-FAIL then
   2dup M-BYTES-LEN
   2drop $10 M-N @ - M-ZEROS ;

: M-NAME16 ( ptr u8 n -- )
   M-LEN M-NAME16-LEN ;

: M-PAD-OFF ( off -- )
   OFF>N M-HERE - M-ZEROS ;

: M-PAD ( n -- )
   M-OFF M-PAD-OFF ;

: M-ADDR ( off -- ptr u8 )
   OFF>N MBUF swap M-BYTE+ ;

: M-LE32@ ( off -- n )
   M-ADDR
   dup c@
   over 1 M-BYTE@ 8 lshift or
   over 2 M-BYTE@ 16 lshift or
   swap 3 M-BYTE@ 24 lshift or ;

: M-LE32! ( n off -- )
   M-ADDR M-A! M-N !
   M-N @ $FF and M-A@ c!
   M-N @ 8 rshift $FF and M-A@ 1 M-BYTE+ c!
   M-N @ 16 rshift $FF and M-A@ 2 M-BYTE+ c!
   M-N @ 24 rshift $FF and M-A@ 3 M-BYTE+ c! ;

: M-LE64! ( n off -- )
   M-O !
   dup M-O@ M-LE32!
   32 rshift M-O@ OFF>N $4 + M-OFF M-LE32! ;

variable MBC

: M-BE-RESET ( off -- )
   OFF>N MBC ! ;

: M-BE-HERE ( -- n )
   MBC @ ;

: M-BE-PTR ( -- ptr u8 )
   MBUF MBC @ M-BYTE+ ;

: M-BE-SKIP ( len -- )
   LEN>N MBC @ + MBC ! ;

: M-BE8 ( n -- )
   M-BE-PTR c!
   1 M-LEN M-BE-SKIP ;

: M-BE32 ( n -- )
   dup 24 rshift $FF and M-BE8
   dup 16 rshift $FF and M-BE8
   dup 8 rshift $FF and M-BE8
   $FF and M-BE8 ;

: M-BE64 ( n -- )
   dup 32 rshift M-BE32
   $FFFFFFFF and M-BE32 ;

: M-BE-BYTES-LEN ( ptr u8 len -- )
   LEN>N M-N ! M-SRC!
   0 begin dup M-N @ < while
      dup M-SRC@ swap M-BYTE+ c@ M-BE8
      1 +
   repeat drop ;

: M-BE-BYTES ( ptr u8 n -- )
   M-LEN M-BE-BYTES-LEN ;
