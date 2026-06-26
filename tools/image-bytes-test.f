\ image-bytes-test.f - shared executable image byte writer regression.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f src/os/image-bytes.f tools/image-bytes-test.f

$20000 constant IBT-CAP
create IBT-BUF IBT-CAP allot
variable IBT-LEN

TRUSTED: IBT-CHECK-REJECTS ( ptr u8 n -- )
   DIAGXT @ >r
   0 DIAGXT !
   CHECK! 0 T=
   r> DIAGXT ! ;

: IBT-SOURCE ( -- ptr u8 n )
   IBT-BUF IBT-LEN @ ;

: IBT-LOAD ( ptr u8 n -- )
   IBT-BUF IBT-CAP READ-ALL IBT-LEN ! ;

: IBT-HAS? ( ptr u8 n -- bool )
   IBT-SOURCE 2swap CONTAINS? ;

: IBT-MUST-HAVE ( ptr u8 n -- )
   IBT-HAS? TTRUE ;

: IBT-MUST-LACK ( ptr u8 n -- )
   IBT-HAS? 0= TTRUE ;

: IBT-TEST-LITTLE-ENDIAN ( -- )
   M-RESET
   $12 M8
   $3456 M16
   $789ABCDE M32
   $1122334455667788 M64
   M-HERE 15 T=
   3 M-OFF M-LE32@ $789ABCDE T=
   $AABBCCDD 2 M-OFF M-LE32!
   2 M-OFF M-LE32@ $AABBCCDD T=
   $1122334455667788 8 M-OFF M-LE64!
   8 M-OFF M-LE32@ $55667788 T=
   12 M-OFF M-LE32@ $11223344 T= ;

: IBT-TEST-COPY-PAD ( -- )
   M-RESET
   s" habu" M-LEN M-BYTES-LEN
   M-HERE 4 T=
   8 M-OFF M-PAD-OFF
   M-HERE 8 T=
   s" xy" M-LEN M-NAME16-LEN
   M-HERE 24 T=
   MBUF 8 + c@ 120 T=
   MBUF 9 + c@ 121 T=
   MBUF 23 + c@ 0 T= ;

: IBT-TEST-BIG-ENDIAN ( -- )
   M-RESET
   16 M-OFF M-BE-RESET
   $01020304 M-BE32
   $05060708090A0B0C M-BE64
   s" Z" M-LEN M-BE-BYTES-LEN
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
   4 M8
   0 M-OFF M-PAD-OFF ;

: IBT-NAME-LONG ( -- )
   M-RESET
   s" 12345678901234567" M-LEN M-NAME16-LEN ;

: IBT-TEST-REFINE-ERRORS ( -- )
   [: IBT-LEN-NEG ;] M-BOUNDS-RC TTHROWSQ
   [: IBT-OFF-NEG ;] M-BOUNDS-RC TTHROWSQ
   [: IBT-PAD-BACK ;] M-BOUNDS-RC TTHROWSQ
   [: IBT-NAME-LONG ;] M-BOUNDS-RC TTHROWSQ
   s" BAD-M-LE32 ( len -- n ) M-LE32@" IBT-CHECK-REJECTS
   s" BAD-M-PAD ( len -- ) M-PAD-OFF" IBT-CHECK-REJECTS ;

: IBT-TEST-SOURCE-SHAPE ( -- )
   s" src/os/image-bytes.f" IBT-LOAD
   s" create MBUF MSIZE allot" IBT-MUST-LACK
   s" : MBUF ( -- ptr u8 )" IBT-MUST-HAVE
   s" image-bytes: mmap failed" IBT-MUST-HAVE
   s" : M-LE32@ ( off -- n )" IBT-MUST-HAVE
   s" : M-LE64! ( n off -- )" IBT-MUST-HAVE
   s" : M-BE32 ( n -- )" IBT-MUST-HAVE
   s" src/os/linux/elf.f" IBT-LOAD
   s" create MBUF MSIZE allot" IBT-MUST-LACK
   s" variable MP" IBT-MUST-LACK
   s" : M8" IBT-MUST-LACK
   s" src/os/macos/macho.f" IBT-LOAD
   s" create MBUF MSIZE allot" IBT-MUST-LACK
   s" variable MP" IBT-MUST-LACK
   s" : M8" IBT-MUST-LACK
   s" variable PHP" IBT-MUST-LACK
   s" : PL!" IBT-MUST-LACK
   s" src/os/macos/sign2.f" IBT-LOAD
   s" variable HLP" IBT-MUST-LACK
   s" : HL@" IBT-MUST-LACK
   s" : B32" IBT-MUST-LACK
   s" : BSTR" IBT-MUST-LACK
   s" M-BE32" IBT-MUST-HAVE
   s" tools/build-fixpoint.f" IBT-LOAD
   s" src/os/image-bytes.f" IBT-MUST-HAVE
   s" tools/srclist.f" IBT-LOAD
   s" src/os/image-bytes.f " IBT-MUST-HAVE ;

: IBT-MAIN ( -- )
   T-RESET
   IBT-TEST-LITTLE-ENDIAN
   IBT-TEST-COPY-PAD
   IBT-TEST-BIG-ENDIAN
   IBT-TEST-REFINE-ERRORS
   IBT-TEST-SOURCE-SHAPE
   T-REPORT
   s" image-bytes-test: ok" type cr ;

IBT-MAIN
