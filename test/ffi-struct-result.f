\ ffi-struct-result.f - C structs returned by value through FUNCTION:.
\ Loaded by src/lib/ffi-test.f on macOS arm64, inside package FFI-TEST.
\
\ System functions return integer records in each AAPCS64 convention:
\ inet_makeaddr's 4-byte struct in_addr and div's 8-byte div_t in x0,
\ lldiv's 16-byte lldiv_t in x0:x1, and CoreMedia's 24-byte CMTime
\ { int64 value; int32 timescale; uint32 flags; int64 epoch } through the x8
\ destination. CoreMedia has no scalar-only function returning a larger record
\ (its CMTimeRange makers take CMTime by value), so CMTime stands for every
\ indirect width. No system function returns a 12-byte integer record; every
\ 9..16-byte integer record arrives in x0:x1 alike, so lldiv declared at 12
\ bytes is the same call and shows the second register stored at its exact
\ width. Each result lands between canaries.

LIBRARY /usr/lib/libSystem.B.dylib
FUNCTION: FFI-T-MAKEADDR inet_makeaddr ( n n ptr u8 -- ) 4 STRUCT-RESULT ;FUNCTION
FUNCTION: FFI-T-DIV div ( n n ptr u8 -- ) 8 STRUCT-RESULT ;FUNCTION
FUNCTION: FFI-T-LLDIV12 lldiv ( n n ptr u8 -- ) 12 STRUCT-RESULT ;FUNCTION
FUNCTION: FFI-T-LLDIV lldiv ( n n ptr u8 -- ) 16 STRUCT-RESULT ;FUNCTION

LIBRARY /System/Library/Frameworks/CoreMedia.framework/CoreMedia
FUNCTION: FFI-T-CMTIME CMTimeMake ( n n ptr u8 -- ) 24 STRUCT-RESULT ;FUNCTION
FUNCTION: FFI-T-CMTIME-EPOCH CMTimeMakeWithEpoch ( n n n ptr u8 -- )
   24 STRUCT-RESULT
;FUNCTION
FUNCTION: FFI-T-CMTIME-SECONDS CMTimeMakeWithSeconds ( r n ptr u8 -- )
   24 STRUCT-RESULT
;FUNCTION

\ Eight C arguments fill x0-x7 and the destination rides x8, so this shape is
\ admitted; it is declared only, never called.
FUNCTION: FFI-T-SR-FULL habu-no-such-struct ( n n n n n n n n ptr u8 -- )
   32 STRUCT-RESULT
;FUNCTION

$A5 constant FFI-T-SR-FILL
create FFI-T-SR-BUF 64 allot

\ The destination is the buffer's first 16-byte-aligned address past its
\ first cell, leaving a leading canary of 8 or 16 bytes.
: FFI-T-SR-OFF ( -- n ) 16 FFI-T-SR-BUF FFI:>CELL 15 and - ;
: FFI-T-SR-DST ( -- ptr u8 ) FFI-T-SR-BUF FFI-T-SR-OFF + ;

: FFI-T-SR-RESET ( -- )
   64 0 do FFI-T-SR-FILL FFI-T-SR-BUF i + c! loop ;

: FFI-T-SR-UNTOUCHED ( -- )
   64 0 do FFI-T-SR-BUF i + c@ FFI-T-SR-FILL T= loop ;

\ The bytes before and the cell after a size-byte result keep their fill.
: FFI-T-SR-CANARIES ( n -- ) {: size:n :}
   FFI-T-SR-OFF 0 do FFI-T-SR-BUF i + c@ FFI-T-SR-FILL T= loop
   8 0 do FFI-T-SR-DST size + i + c@ FFI-T-SR-FILL T= loop ;

: FFI-T-SR-CMTIME ( n n n n -- ) {: value:n scale:n flags:n epoch:n :}
   FFI-T-SR-DST LE:U64@ value T=
   FFI-T-SR-DST 8 + LE:S32@ scale T=
   FFI-T-SR-DST 12 + LE:U32@ flags T=
   FFI-T-SR-DST 16 + LE:U64@ epoch T=
   24 FFI-T-SR-CANARIES ;

\ Declarations the declarer must refuse, each through the loader's evaluate.
: FFI-T-SR-ZERO ( -- )
   s" FUNCTION: FFI-T-SRX2 div ( n n ptr u8 -- ) 0 STRUCT-RESULT ;FUNCTION" INCLUDE-EVALUATE ;
: FFI-T-SR-OUTPUT ( -- )
   s" FUNCTION: FFI-T-SRX3 div ( n n ptr u8 -- n ) 8 STRUCT-RESULT ;FUNCTION" INCLUDE-EVALUATE ;
: FFI-T-SR-NOT-LAST ( -- )
   s" FUNCTION: FFI-T-SRX4 div ( n ptr u8 n -- ) 8 STRUCT-RESULT ;FUNCTION" INCLUDE-EVALUATE ;
: FFI-T-SR-NO-INPUT ( -- )
   s" FUNCTION: FFI-T-SRX5 div ( -- ) 8 STRUCT-RESULT ;FUNCTION" INCLUDE-EVALUATE ;
: FFI-T-SR-TWICE ( -- )
   s" FUNCTION: FFI-T-SRX6 div ( n n ptr u8 -- ) 8 STRUCT-RESULT 8 STRUCT-RESULT ;FUNCTION"
   INCLUDE-EVALUATE ;
: FFI-T-SR-VARIADIC-BEFORE ( -- )
   s" FUNCTION: FFI-T-SRX7 div ( n n ptr u8 -- ) 1 VARIADIC 8 STRUCT-RESULT ;FUNCTION"
   INCLUDE-EVALUATE ;
: FFI-T-SR-VARIADIC-AFTER ( -- )
   s" FUNCTION: FFI-T-SRX8 div ( n n ptr u8 -- ) 8 STRUCT-RESULT 1 VARIADIC ;FUNCTION"
   INCLUDE-EVALUATE ;
: FFI-T-SR-WRITES-BEFORE ( -- )
   s" FUNCTION: FFI-T-SRX9 div ( n n ptr u8 -- ) 2 8 WRITES-BYTES 8 STRUCT-RESULT ;FUNCTION"
   INCLUDE-EVALUATE ;
: FFI-T-SR-WRITES-AFTER ( -- )
   s" FUNCTION: FFI-T-SRXA div ( n n ptr u8 -- ) 8 STRUCT-RESULT 2 8 WRITES-BYTES ;FUNCTION"
   INCLUDE-EVALUATE ;
: FFI-T-SR-OUTSIDE ( -- )
   s" 8 STRUCT-RESULT" INCLUDE-EVALUATE ;
\ Sixteen inputs are the limit, and the destination is one of them.
: FFI-T-SR-INPUTS ( -- )
   s" FUNCTION: FFI-T-SRXC div ( n n n n n n n n n n n n n n n n ptr u8 -- ) 32 STRUCT-RESULT ;FUNCTION"
   INCLUDE-EVALUATE ;

: FFI-T-STRUCT-RESULTS ( -- )
   s" a 4-byte struct result takes exactly four bytes of x0" T-LABEL
   FFI-T-SR-RESET
   10 $010203 FFI-T-SR-DST FFI-T-MAKEADDR
   FFI-T-SR-DST c@ 10 T=
   FFI-T-SR-DST 1 + c@ 1 T=
   FFI-T-SR-DST 2 + c@ 2 T=
   FFI-T-SR-DST 3 + c@ 3 T=
   4 FFI-T-SR-CANARIES

   s" an 8-byte struct result arrives whole from x0" T-LABEL
   FFI-T-SR-RESET
   -7 2 FFI-T-SR-DST FFI-T-DIV
   FFI-T-SR-DST LE:S32@ -3 T=
   FFI-T-SR-DST 4 + LE:S32@ -1 T=
   8 FFI-T-SR-CANARIES

   s" a 12-byte struct result takes x0 and exactly four bytes of x1" T-LABEL
   FFI-T-SR-RESET
   -7000000000000 3 FFI-T-SR-DST FFI-T-LLDIV12
   FFI-T-SR-DST LE:U64@ -2333333333333 T=
   FFI-T-SR-DST 8 + LE:U32@ $FFFFFFFF T=
   12 FFI-T-SR-CANARIES

   s" a 16-byte struct result arrives from x0 then x1" T-LABEL
   FFI-T-SR-RESET
   -7000000000000 3 FFI-T-SR-DST FFI-T-LLDIV
   FFI-T-SR-DST LE:U64@ -2333333333333 T=
   FFI-T-SR-DST 8 + LE:U64@ -1 T=
   16 FFI-T-SR-CANARIES

   s" a 24-byte struct result is written through x8" T-LABEL
   FFI-T-SR-RESET
   1001 30000 FFI-T-SR-DST FFI-T-CMTIME
   1001 30000 1 0 FFI-T-SR-CMTIME
   FFI-T-SR-RESET
   -5 600 7 FFI-T-SR-DST FFI-T-CMTIME-EPOCH
   -5 600 1 7 FFI-T-SR-CMTIME

   s" a double argument rides d0 beside an x8 struct result" T-LABEL
   FFI-T-SR-RESET
   2.5 600 FFI-T-SR-DST FFI-T-CMTIME-SECONDS
   1500 600 1 0 FFI-T-SR-CMTIME

   s" a misaligned result destination is refused before the call" T-LABEL
   FFI-T-SR-RESET
   [: 1 2 FFI-T-SR-DST 4 + FFI-T-DIV ;] E-FFI-SYNTAX TTHROWSQ
   FFI-T-SR-UNTOUCHED
   s" an x8 result destination needs 16-byte alignment" T-LABEL
   [: 1 2 FFI-T-SR-DST 8 + FFI-T-CMTIME ;] E-FFI-SYNTAX TTHROWSQ
   FFI-T-SR-UNTOUCHED

   s" malformed struct-result declarations are refused" T-LABEL
   [: FFI-T-SR-ZERO ;] E-FFI-SYNTAX TTHROWSQ
   [: FFI-T-SR-OUTPUT ;] E-FFI-SYNTAX TTHROWSQ
   [: FFI-T-SR-NOT-LAST ;] E-FFI-SYNTAX TTHROWSQ
   [: FFI-T-SR-NO-INPUT ;] E-FFI-SYNTAX TTHROWSQ
   [: FFI-T-SR-TWICE ;] E-FFI-SYNTAX TTHROWSQ
   [: FFI-T-SR-VARIADIC-BEFORE ;] E-FFI-SYNTAX TTHROWSQ
   [: FFI-T-SR-VARIADIC-AFTER ;] E-FFI-SYNTAX TTHROWSQ
   [: FFI-T-SR-WRITES-BEFORE ;] E-FFI-SYNTAX TTHROWSQ
   [: FFI-T-SR-WRITES-AFTER ;] E-FFI-SYNTAX TTHROWSQ
   [: FFI-T-SR-OUTSIDE ;] E-FFI-SYNTAX TTHROWSQ
   s" sixteen C arguments beside the destination exceed the inputs" T-LABEL
   [: FFI-T-SR-INPUTS ;] E-FFI-ARITY TTHROWSQ ;

FFI-T-STRUCT-RESULTS
