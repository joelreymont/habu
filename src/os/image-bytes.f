\ image-bytes.f - shared executable image byte cursor and patch helpers.
\ It names no machine's code package, so it compiles the same under every
\ target writer and links as any file does; the writer sizes the buffer.

package IMAGE-BYTES
\ The buffer's bytes: the largest image a loaded target writer names (MSIZE!).
variable MSIZE-N
public
: MSIZE ( -- n ) MSIZE-N @ ;
75 constant M-BOUNDS-RC
\ The image cursors hold addresses in the creating process's mapped buffer, so
\ they are pointer cells; a snapshot builder clears them in a named reset.
PTR-VARIABLE MBUF-A
PTR-VARIABLE MP
variable MLEN
private
PTR-VARIABLE M-A
PTR-VARIABLE M-SRC
variable M-N
variable M-O
public
\ The code/payload boundary macho.f reads to put payload bytes in __DATA_CONST;
\ only IMG-PAYLOAD-CLEAR stores it, as zero, so no image carries a payload.
PTR-VARIABLE IMG-PAYLOAD-A
variable IMG-PAYLOAD-N
variable IMG-CODE-N

: IMG-PAYLOAD-CLEAR ( -- )
   0 IMG-PAYLOAD-N !  0 IMG-CODE-N ! ;

private
: M-ALLOC-BUF ( -- ptr u8 )
   MSIZE map-anon 0 <> if drop s" image-bytes: mmap failed" 74 die then ;

: MBUF-A@ ( -- ptr u8 )
   MBUF-A @ ;

: MBUF-A! ( ptr u8 -- )
   MBUF-A ! ;

: M-ENSURE-BUF ( -- )
   MBUF-A@ 0= if M-ALLOC-BUF MBUF-A! then ;

public
\ A target writer names its largest image at load, rounded up to 64 KiB; the
\ largest named wins, whatever the load order. The buffer maps on first use,
\ so a size named after that is refused.
: MSIZE! ( n -- )
   MBUF-A@ 0= 0= if s" image-bytes: a size named after the buffer mapped" 73 die then
   $FFFF + $10000 / $10000 *  MSIZE-N @ max  MSIZE-N ! ;

: MBUF ( -- ptr u8 )
   M-ENSURE-BUF
   MBUF-A@ ;

private
: MP@ ( -- ptr u8 ) MP @ ;

public
: MLEN@ ( -- n )
   MLEN @ ;

: MLEN! ( n -- )
   MLEN ! ;

private
: M-A@ ( -- ptr u8 )
   M-A @ ;

: M-SRC@ ( -- ptr u8 )
   M-SRC @ ;

: M-A! ( ptr u8 -- )
   M-A ! ;

: M-SRC! ( ptr u8 -- )
   M-SRC ! ;

: M-O@ ( -- off )
   M-O @ >OFF ;

public
: M-BYTE+ ( ptr u8 n -- ptr u8 )
   + ;

: M-BYTE@ ( ptr u8 n -- n )
   M-BYTE+ c@ ;

: M-RESET ( -- )
   MBUF MP ! ;

: M-HERE ( -- n )
   MP@ MBUF - ;

private
create M-FAIL-NL 1 allot

\ Reports before throwing: the drivers exit with the raw throw code, so an
\ unreported overrun exits 75 silently. The write's rc has nowhere to go.
: M-FAIL ( ptr u8 n -- ) {: a:ptr u:n :}
   2 a u write drop
   10 M-FAIL-NL c!
   2 M-FAIL-NL 1 write drop
   M-BOUNDS-RC throw ;

: M-CHECK-N ( n -- )
   dup 0 < if s" image-bytes: negative span" M-FAIL then
   dup MSIZE > if s" image-bytes: span exceeds buffer" M-FAIL then
   drop ;

public
: M-LEN ( n -- len )
   dup M-CHECK-N
   >LEN ;

: M-OFF ( n -- off )
   dup M-CHECK-N
   >OFF ;

private
: M-ROOM1 ( -- )
   M-HERE MSIZE >= if s" image-bytes: cursor exceeds buffer" M-FAIL then ;

\ `>LEN` is a cast, not a validator, so the length is compared with the room
\ left: in a sum, a length near the maximum cell wraps back under MSIZE.
: M-CHECK-ROOM ( len -- )
   LEN>N {: u:n :}
   u 0 < u MSIZE M-HERE - > or if s" image-bytes: write exceeds buffer" M-FAIL then ;

public
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

private
: M-NAME16 ( ptr u8 n -- )
   M-LEN M-NAME16-LEN ;

public
: M-PAD-OFF ( off -- )
   OFF>N M-HERE - M-ZEROS ;

private
: M-PAD ( n -- )
   M-OFF M-PAD-OFF ;

: M-ADDR ( off -- ptr u8 )
   OFF>N MBUF swap M-BYTE+ ;

public
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

private
variable MBC

public
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

private
: M-BE-BYTES ( ptr u8 n -- )
   M-LEN M-BE-BYTES-LEN ;
;package
