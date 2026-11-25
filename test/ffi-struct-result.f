\ ffi-struct-result.f - C structs returned by value through FUNCTION:.
\ Loaded by src/lib/ffi-test.f inside package FFI-TEST.
\
\ System functions return integer records in each AAPCS64 convention:
\ inet_makeaddr's 4-byte struct in_addr and div's 8-byte div_t in x0, and
\ lldiv's 16-byte lldiv_t in x0:x1, from libSystem on macOS and glibc on
\ Linux. The fixture library's (test/ffi-struct-fixture.f) 24-byte record
\ { int64 value; int32 timescale; uint32 flags; int64 epoch } arrives through
\ the x8 destination, as every larger integer record does, so it stands for
\ every indirect width. No system function returns a 12-byte integer record;
\ every 9..16-byte integer record arrives in x0:x1 alike, so lldiv declared
\ at 12 bytes is the same call and shows the second register stored at its
\ exact width.
\ SysV returns the same widths in rax and rdx and the larger record through
\ rdi.

: FFI-T-SR-LIBC ( -- )
   HB-TARGET-MACOS? if
      s" /usr/lib/libSystem.B.dylib"
   else
      s" libc.so.6"
   then FFI:SELECT-LIBRARY ;
FFI-T-SR-LIBC

FUNCTION: FFI-T-MAKEADDR inet_makeaddr ( n n ^u8 -- ) 4 STRUCT-RESULT ;FUNCTION
FUNCTION: FFI-T-DIV div ( n n ^u8 -- ) 8 STRUCT-RESULT ;FUNCTION
FUNCTION: FFI-T-LLDIV12 lldiv ( n n ^u8 -- ) 12 STRUCT-RESULT ;FUNCTION
FUNCTION: FFI-T-LLDIV lldiv ( n n ^u8 -- ) 16 STRUCT-RESULT ;FUNCTION

STRUCT-FIXTURE:SELECT
FUNCTION: FFI-T-TIME24 hb_time24 ( n n ^u8 -- ) 24 STRUCT-RESULT ;FUNCTION
FUNCTION: FFI-T-TIME24-EPOCH hb_time24_epoch ( n n n ^u8 -- )
   24 STRUCT-RESULT
;FUNCTION
FUNCTION: FFI-T-TIME24-SECONDS hb_time24_seconds ( r n ^u8 -- )
   24 STRUCT-RESULT
;FUNCTION

\ Eight C arguments fill x0-x7 and the destination rides x8, so this shape is
\ admitted; it is declared only, never called.
FUNCTION: FFI-T-SR-FULL habu-no-such-struct ( n n n n n n n n ^u8 -- )
   32 STRUCT-RESULT
;FUNCTION

$A5 constant FFI-T-SR-FILL
create FFI-T-SR-BUF 64 allot

\ The destination is the buffer's first 16-byte-aligned address past its
\ first cell, leaving a leading canary of 8 or 16 bytes.
: FFI-T-SR-OFF ( -- n ) 16 FFI-T-SR-BUF FFI:>CELL 15 and - ;
: FFI-T-SR-DST ( -- ^u8 ) FFI-T-SR-BUF FFI-T-SR-OFF + ;

: FFI-T-SR-RESET ( -- )
   64 0 do FFI-T-SR-FILL FFI-T-SR-BUF i + c! loop ;

: FFI-T-SR-CANARIES ( n -- ) {: size:n :}
   FFI-T-SR-OFF 0 do FFI-T-SR-BUF i + c@ FFI-T-SR-FILL T= loop
   8 0 do FFI-T-SR-DST size + i + c@ FFI-T-SR-FILL T= loop ;

: FFI-T-SR-TIME24 ( n n n n -- )
   {: value:n scale:n flags:n epoch:n :}
   FFI-T-SR-DST LE:U64@ value T=
   FFI-T-SR-DST 8 + LE:S32@ scale T=
   FFI-T-SR-DST 12 + LE:U32@ flags T=
   FFI-T-SR-DST 16 + LE:U64@ epoch T=
   24 FFI-T-SR-CANARIES ;

: FFI-T-SR-ZERO ( -- )
   s" FUNCTION: FFI-T-SRX2 div ( n n ^u8 -- ) 0 STRUCT-RESULT ;FUNCTION" evaluate-closed ;
: FFI-T-SR-OUTPUT ( -- )
   s" FUNCTION: FFI-T-SRX3 div ( n n ^u8 -- n ) 8 STRUCT-RESULT ;FUNCTION" evaluate-closed ;
: FFI-T-SR-NOT-LAST ( -- )
   s" FUNCTION: FFI-T-SRX4 div ( n ^u8 n -- ) 8 STRUCT-RESULT ;FUNCTION" evaluate-closed ;
: FFI-T-SR-NO-INPUT ( -- )
   s" FUNCTION: FFI-T-SRX5 div ( -- ) 8 STRUCT-RESULT ;FUNCTION" evaluate-closed ;
: FFI-T-SR-TWICE ( -- )
   s" FUNCTION: FFI-T-SRX6 div ( n n ^u8 -- ) 8 STRUCT-RESULT 8 STRUCT-RESULT ;FUNCTION"
   evaluate-closed ;
: FFI-T-SR-VARIADIC-BEFORE ( -- )
   s" FUNCTION: FFI-T-SRX7 div ( n n ^u8 -- ) 1 VARIADIC 8 STRUCT-RESULT ;FUNCTION"
   evaluate-closed ;
: FFI-T-SR-VARIADIC-AFTER ( -- )
   s" FUNCTION: FFI-T-SRX8 div ( n n ^u8 -- ) 8 STRUCT-RESULT 1 VARIADIC ;FUNCTION"
   evaluate-closed ;
: FFI-T-SR-OUTSIDE ( -- )
   s" 8 STRUCT-RESULT" evaluate-closed ;
: FFI-T-SR-INPUTS ( -- )
   s" FUNCTION: FFI-T-SRXC div ( n n n n n n n n n n n n n n n n ^u8 -- ) 32 STRUCT-RESULT ;FUNCTION"
   evaluate-closed ;

: FFI-T-STRUCT-RESULTS ( -- )
   s" a 4-byte struct result takes exactly four register bytes" T-LABEL
   FFI-T-SR-RESET
   10 $010203 FFI-T-SR-DST FFI-T-MAKEADDR
   FFI-T-SR-DST c@ 10 T=
   FFI-T-SR-DST 1 + c@ 1 T=
   FFI-T-SR-DST 2 + c@ 2 T=
   FFI-T-SR-DST 3 + c@ 3 T=
   4 FFI-T-SR-CANARIES

   s" an 8-byte struct result arrives whole from one register" T-LABEL
   FFI-T-SR-RESET
   -7 2 FFI-T-SR-DST FFI-T-DIV
   FFI-T-SR-DST LE:S32@ -3 T=
   FFI-T-SR-DST 4 + LE:S32@ -1 T=
   8 FFI-T-SR-CANARIES

   s" a 12-byte struct result takes exactly four bytes of its second register" T-LABEL
   FFI-T-SR-RESET
   -7000000000000 3 FFI-T-SR-DST FFI-T-LLDIV12
   FFI-T-SR-DST LE:U64@ -2333333333333 T=
   FFI-T-SR-DST 8 + LE:U32@ $FFFFFFFF T=
   12 FFI-T-SR-CANARIES

   s" a 16-byte struct result arrives from two registers" T-LABEL
   FFI-T-SR-RESET
   -7000000000000 3 FFI-T-SR-DST FFI-T-LLDIV
   FFI-T-SR-DST LE:U64@ -2333333333333 T=
   FFI-T-SR-DST 8 + LE:U64@ -1 T=
   16 FFI-T-SR-CANARIES

   s" a 24-byte struct result is written through the hidden pointer" T-LABEL
   FFI-T-SR-RESET
   1001 30000 FFI-T-SR-DST FFI-T-TIME24
   1001 30000 1 0 FFI-T-SR-TIME24
   FFI-T-SR-RESET
   -5 600 7 FFI-T-SR-DST FFI-T-TIME24-EPOCH
   -5 600 1 7 FFI-T-SR-TIME24

   s" a double argument beside a hidden struct result" T-LABEL
   FFI-T-SR-RESET
   2.5 600 FFI-T-SR-DST FFI-T-TIME24-SECONDS
   1500 600 1 0 FFI-T-SR-TIME24

   s" malformed struct-result declarations are refused" T-LABEL
   [: FFI-T-SR-ZERO ;]
   s" error[E-FFI-SYNTAX]: STRUCT-RESULT's byte count is not positive" FFI-T-RECORD
   [: FFI-T-SR-OUTPUT ;]
   s" error[E-FFI-SYNTAX]: STRUCT-RESULT with a declared output" FFI-T-RECORD
   [: FFI-T-SR-NOT-LAST ;]
   s" error[E-FFI-SYNTAX]: the clause names an input that is not ^u8" FFI-T-RECORD
   [: FFI-T-SR-NO-INPUT ;]
   s" error[E-FFI-SYNTAX]: STRUCT-RESULT with no input to write to" FFI-T-RECORD
   [: FFI-T-SR-TWICE ;]
   s" error[E-FFI-SYNTAX]: a second STRUCT-RESULT" FFI-T-RECORD
   [: FFI-T-SR-VARIADIC-BEFORE ;]
   s" error[E-FFI-SYNTAX]: a struct call with VARIADIC" FFI-T-RECORD
   [: FFI-T-SR-VARIADIC-AFTER ;]
   s" error[E-FFI-SYNTAX]: a struct call with VARIADIC" FFI-T-RECORD
   [: FFI-T-SR-OUTSIDE ;]
   s" error[E-FFI-SYNTAX]: a clause outside a FUNCTION: declaration" FFI-T-RECORD
   s" sixteen C arguments beside the destination exceed the inputs" T-LABEL
   [: FFI-T-SR-INPUTS ;]
   s" error[E-FFI-ARITY]: a FUNCTION: declaration has at most 16 inputs, a str counting two" FFI-T-RECORD ;

FFI-T-STRUCT-RESULTS
