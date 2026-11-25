\ image-bytes-test.f - shared executable image byte writer regression.
\ Run: bin/hb --load src/lib/error.f src/lib/string.f src/lib/fs.f tools/image-bytes-test.f

require src/lib/error.f
require src/lib/string.f
require src/lib/test/assert.f
require src/lib/fs.f
require src/arch/arm64/icode.f
require src/os/image-bytes.f
using A64ICODE
using IMAGE-BYTES

: IBT-CHECK-REJECTS ( str -- )
   CHECK-QUIET-CANDIDATE! 0 T= ;

\ A size rounds up to 64 KiB. A larger one maps a fresh, zeroed buffer at the
\ next use; an equal or smaller one keeps the mapped buffer and its bytes.
: IBT-TEST-MSIZE ( -- )
   MSIZE $10000 + {: grown:n :}
   grown 1 - MSIZE!  MSIZE grown T=
   M-RESET  7 IMG-M8  MBUF c@ 7 T=
   grown MSIZE!  MSIZE grown T=  MBUF c@ 7 T=
   1 MSIZE!  MSIZE grown T=  MBUF c@ 7 T=
   grown 1 + MSIZE!  MSIZE grown $10000 + T=  MBUF c@ 0 T=
   M-RESET  MSIZE 1- M-OFF M-PAD-OFF  1 IMG-M8  M-HERE MSIZE T= ;

: IBT-TEST-LITTLE-ENDIAN ( -- )
   M-RESET
   $12 IMG-M8
   $3456 IMG-M16
   $789ABCDE IMG-M32
   $1122334455667788 IMG-M64
   M-HERE 15 T=
   3 M-OFF M-LE32@ $789ABCDE T=
   $AABBCCDD 2 M-OFF M-LE32!
   2 M-OFF M-LE32@ $AABBCCDD T=
   $1122334455667788 8 M-OFF M-LE64!
   8 M-OFF M-LE32@ $55667788 T=
   12 M-OFF M-LE32@ $11223344 T= ;

: IBT-TEST-COPY-PAD ( -- )
   M-RESET
   s" habu" M-BYTES
   M-HERE 4 T=
   8 M-OFF M-PAD-OFF
   M-HERE 8 T=
   s" xy" M-NAME16
   M-HERE 24 T=
   MBUF 8 + c@ 120 T=
   MBUF 9 + c@ 121 T=
   MBUF 23 + c@ 0 T= ;

: IBT-TEST-BIG-ENDIAN ( -- )
   M-RESET
   16 M-OFF M-BE-RESET
   $01020304 M-BE32
   $05060708090A0B0C M-BE64
   s" Z" M-BE-BYTES
   M-BE-HERE 29 T=
   MBUF 16 + c@ 1 T=
   MBUF 17 + c@ 2 T=
   MBUF 18 + c@ 3 T=
   MBUF 19 + c@ 4 T=
   MBUF 27 + c@ 12 T=
   MBUF 28 + c@ 90 T= ;

: IBT-LEN-NEG ( -- )
   -1 M-LEN drop ;

: IBT-OFF-NEG ( -- )
   -1 M-OFF drop ;

: IBT-PAD-BACK ( -- )
   M-RESET
   4 IMG-M8
   0 M-OFF M-PAD-OFF ;

: IBT-NAME-LONG ( -- )
   M-RESET
   s" 12345678901234567" M-NAME16 ;

: IBT-FILL-OVER ( -- )
   s" xy" M-BYTES ;

: IBT-FILL-EXACT ( -- )
   s" x" M-BYTES ;

: IBT-CURSOR-OVER ( -- )
   1 IMG-M8 ;

: IBT-WRITE-NEG ( -- )
   M-RESET 1 IMG-M8
   s" x" STR:DATA -1 >str M-BYTES ;

: IBT-WRITE-MAX ( -- )
   M-RESET 1 IMG-M8
   s" x" STR:DATA -1 1 rshift >str M-BYTES ;

\ One pad to the last byte serves the three room boundaries in turn. A write is
\ measured against the room left: `>LEN` is a cast, not a validator, and a length
\ near the maximum cell would wrap the sum back under MSIZE. A rejected write
\ leaves the cursor in place, so the exact write after it fills the buffer; a
\ cursor at MSIZE throws 88.
: IBT-TEST-ROOM ( -- )
   M-RESET  MSIZE 1- M-OFF M-PAD-OFF
   [: IBT-FILL-OVER ;] EXIT-STATUS:IMAGE-BOUNDS TTHROWSQ
   IBT-FILL-EXACT M-HERE MSIZE T=
   [: IBT-CURSOR-OVER ;] EXIT-STATUS:IMAGE-BOUNDS TTHROWSQ
   [: IBT-WRITE-NEG ;] EXIT-STATUS:IMAGE-BOUNDS TTHROWSQ
   M-HERE 1 T=
   [: IBT-WRITE-MAX ;] EXIT-STATUS:IMAGE-BOUNDS TTHROWSQ
   M-HERE 1 T= ;

: IBT-TEST-REFINE-ERRORS ( -- )
   [: IBT-LEN-NEG ;] EXIT-STATUS:IMAGE-BOUNDS TTHROWSQ
   [: IBT-OFF-NEG ;] EXIT-STATUS:IMAGE-BOUNDS TTHROWSQ
   [: IBT-PAD-BACK ;] EXIT-STATUS:IMAGE-BOUNDS TTHROWSQ
   [: IBT-NAME-LONG ;] EXIT-STATUS:IMAGE-BOUNDS TTHROWSQ
   s" BAD-M-LE32 ( len -- n ) M-LE32@" IBT-CHECK-REJECTS
   s" BAD-M-PAD ( len -- ) M-PAD-OFF" IBT-CHECK-REJECTS ;

package IMAGE-BYTES-TEST

public

: RUN ( -- )
   T-RESET
   IBT-TEST-MSIZE
   IBT-TEST-LITTLE-ENDIAN
   IBT-TEST-COPY-PAD
   IBT-TEST-BIG-ENDIAN
   IBT-TEST-REFINE-ERRORS
   IBT-TEST-ROOM
   T-REPORT
   s" image-bytes-test: ok" type cr ;

;package

IMAGE-BYTES-TEST:RUN
;using
;using
