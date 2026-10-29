\ File identity follows symlinks and preserves stat failures other than ENOENT.
\ SAMEFILE is true when both paths exist and name one device and inode; an
\ ENOENT path is simply not the same file, any other stat failure E-FS-STAT.
\ SAME-OPEN-FILE? compares two open descriptors, untouched by later path or
\ symlink changes. PATH-IDENTITY answers the device and inode a path names,
\ throwing E-FS-STAT when stat fails, ENOENT included.
\
\ STORAGE CLASS. CALLER-OWNED: each call allocates and releases its own MEM
\ mapping; no module state.
require src/lib/fs.f
require src/lib/le.f
require src/lib/memory.f
require src/lib/ffi-abi.f
require src/lib/span.f

package FS
using MEM

private
8 constant INODE-OFFSET
STAT-CAP PATHZ-CAP + constant WORK-BYTES

PROCESS-SYMBOLS
FUNCTION: STAT-CALL stat ( ptr u8 ptr u8 -- i32 )
   1 STAT-CAP WRITES-BYTES
;FUNCTION

: DEVICE-INODE ( ptr u8 -- n n ) {: buffer :}
   HB-TARGET-LINUX? HB-TARGET-LINUX-X86-64? or if buffer LE:U64@ else
      HB-TARGET-MACOS? if buffer LE:U32@ else E-FS-STAT throw then
   then
   buffer INODE-OFFSET + LE:U64@ ;

: FD-IDENTITY ( fd ptr u8 -- n n ) {: file buffer :}
   file FD>N buffer FSTAT-CALL 0<> if E-FS-STAT throw then
   buffer DEVICE-INODE ;

: COMPARE-FDS ( fd fd ptr u8 NUM:alloc-byte-len -- bool )
   drop {: left right buffer :}
   left buffer FD-IDENTITY {: device:n inode:n :}
   right buffer FD-IDENTITY inode = swap device = and ;


: CHECK-PATH ( ptr u8 n -- )
   {: text bytes:n :}
   bytes 0 <= bytes PATH-CAP > or if E-FS-PATH throw then ;


: STAT-EXISTS? ( ptr u8 SPAN:span<u8> -- bool )
   {: path output :}
   path output 0 SPAN:AT STAT-CALL 0= if true exit then
   FFI:ERRNO ENOENT = if false exit then
   E-FS-STAT throw ;


\ The work buffer is one span split in two: the stat block the foreign call
\ writes and the NUL-padded path behind it. Both halves are narrowings, so the
\ split cannot address past what SAMEFILE allocated.
: IDENTITY ( ptr u8 n SPAN:span<u8> -- n n bool )
   {: text bytes:n work :}
   text bytes CHECK-PATH
   text bytes  work STAT-CAP PATHZ-CAP SPAN:SUB  PATHZ-INTO
   work 0 STAT-CAP SPAN:SUB STAT-EXISTS? if
      work 0 SPAN:AT DEVICE-INODE true
   else 0 0 false then ;


: COMPARE-IDENTITIES ( ptr u8 n ptr u8 n ptr u8 NUM:alloc-byte-len -- bool )
   ALLOCATION>SPAN {: left left-bytes:n right right-bytes:n work :}
   left left-bytes work IDENTITY {: left-device:n left-inode:n left-exists:bool :}
   right right-bytes work IDENTITY {: right-device:n right-inode:n right-exists:bool :}
   left-exists right-exists and left-device right-device = and left-inode right-inode = and ;


: LOOKUP-IDENTITY ( ptr u8 n ptr u8 NUM:alloc-byte-len -- n n )
   ALLOCATION>SPAN {: text bytes:n work :}
   text bytes work IDENTITY 0= if E-FS-STAT throw then ;

\ Compare the actual open files, independent of later path or symlink changes.
: SAME-OPEN-FILE? ( fd fd -- bool )
   STAT-CAP BYTES-ALLOC-LEN [: COMPARE-FDS ;] WITH-BYTES ;

public
\ WITH-BYTES hands the quotation exactly the WORK-BYTES mapping it allocated
\ here; ALLOCATION>SPAN preserves that producer's base and reach together.
: SAMEFILE ( ptr u8 n ptr u8 n -- bool )
   WORK-BYTES BYTES-ALLOC-LEN [: COMPARE-IDENTITIES ;] WITH-BYTES ;

\ The device and inode the path names, through any symlinks.
: PATH-IDENTITY ( ptr u8 n -- n n )
   WORK-BYTES BYTES-ALLOC-LEN [: LOOKUP-IDENTITY ;] WITH-BYTES ;

;using
;package
