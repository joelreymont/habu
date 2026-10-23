\ be-test.f - big-endian reads and stores through src/lib/be.f.
\ Run: bin/hb --load src/lib/be-test.f
\
\ Every vector is written from the byte order, not computed: the first byte
\ is the most significant. Stores land at odd offsets of a buffer filled with
\ AAh, so a wrong width or alignment shows in the neighbouring bytes.

require src/lib/string.f
require src/lib/test.f
require src/lib/be.f

package BE-TEST

16 constant SCRATCH-BYTES
create SCRATCH SCRATCH-BYTES allot

: FRESH ( -- ) SCRATCH-BYTES 0 do $AA SCRATCH i + c! loop ;

\ The scratch bytes from offset 0 through the stored field and one past it.
: SEEN$ ( n -- ptr u8 n ) SCRATCH swap ;

\ 1 shl 63, C2BC928F996E0000h and 0123456789ABCDEFh, composed so no literal
\ passes the signed cell.
: HIGH-BIT ( -- n ) 1 63 lshift ;
: UNSET-BITS ( -- n ) $C2BC928F 32 lshift $996E0000 or ;
: COUNTING ( -- n ) $01234567 32 lshift $89ABCDEF or ;

: READS ( -- )
   s" unsigned reads" T-LABEL
   s\" \x12\x34" drop BE:U16@ $1234 T=
   s\" \xFF\xFE" drop BE:U16@ $FFFE T=
   s\" \x12\x34\x56\x78" drop BE:U32@ $12345678 T=
   s\" \xFF\xFF\xFF\xFF" drop BE:U32@ $FFFFFFFF T=
   s\" \x01\x23\x45\x67\x89\xAB\xCD\xEF" drop BE:U64@ COUNTING T=
   s\" \x80\x00\x00\x00\x00\x00\x00\x00" drop BE:U64@ HIGH-BIT T=
   s\" \xC2\xBC\x92\x8F\x99\x6E\x00\x00" drop BE:U64@ UNSET-BITS T= ;

: SIGNED ( -- )
   s" signed reads at each boundary" T-LABEL
   s\" \x00\x00" drop BE:S16@ 0 T=
   s\" \x7F\xFF" drop BE:S16@ 32767 T=
   s\" \x80\x00" drop BE:S16@ -32768 T=
   s\" \xFF\xFF" drop BE:S16@ -1 T=
   s\" \x7F\xFF\xFF\xFF" drop BE:S32@ 2147483647 T=
   s\" \x80\x00\x00\x00" drop BE:S32@ -2147483648 T=
   s\" \xFF\xFF\xFF\xFE" drop BE:S32@ -2 T= ;

: STORES ( -- )
   s" stores at odd offsets keep their neighbours" T-LABEL
   FRESH $1234 SCRATCH 1 + BE:U16!
   4 SEEN$ s\" \xAA\x12\x34\xAA" T$=
   FRESH -2 SCRATCH 1 + BE:U16!
   4 SEEN$ s\" \xAA\xFF\xFE\xAA" T$=
   FRESH $12345678 SCRATCH 3 + BE:U32!
   8 SEEN$ s\" \xAA\xAA\xAA\x12\x34\x56\x78\xAA" T$=
   FRESH -1 SCRATCH 1 + BE:U32!
   6 SEEN$ s\" \xAA\xFF\xFF\xFF\xFF\xAA" T$=
   FRESH UNSET-BITS SCRATCH 1 + BE:U64!
   10 SEEN$ s\" \xAA\xC2\xBC\x92\x8F\x99\x6E\x00\x00\xAA" T$= ;

: ROUND-TRIPS ( -- )
   s" a store reads back at an odd offset" T-LABEL
   FRESH -32768 SCRATCH 1 + BE:U16!  SCRATCH 1 + BE:S16@ -32768 T=
   FRESH -2147483648 SCRATCH 3 + BE:U32!  SCRATCH 3 + BE:S32@ -2147483648 T=
   FRESH HIGH-BIT SCRATCH 5 + BE:U64!  SCRATCH 5 + BE:U64@ HIGH-BIT T=
   FRESH COUNTING SCRATCH 7 + BE:U64!  SCRATCH 7 + BE:U64@ COUNTING T= ;

: MAIN ( -- )
   T-RESET
   READS
   SIGNED
   STORES
   ROUND-TRIPS
   T-REPORT
   s" be-test: ok" type cr ;

MAIN

;package
