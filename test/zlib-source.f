\ ZLIB cases for one stripped application: literal RFC 1951 streams, CRC-32
\ vectors, refusals made before C is called, failures after libz has a live
\ stream (each followed by a successful operation), and a deterministic 10 MiB
\ payload written to the directory given as the first argument after `--`:
\ original.bin, compressed.bin (raw DEFLATE) and decoded.bin.
\ Built and run by test/zlib-e2e.f.
require src/lib/zlib.f
require src/lib/test.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/fs.f
require src/os/script-argv.f

package ZLIB-SOURCE
private

\ The observed SolidWorks workload's largest expanded member is about 10 MiB.
$A00000 constant PAYLOAD-BYTES
64 constant SMALL-CAP
create SMALL-IN SMALL-CAP allot
create SMALL-OUT SMALL-CAP allot
create PATH FS-PATH-CAP allot

: CHECK$ ( -- ptr u8 n ) s" 123456789" ;
: HELLO$ ( -- ptr u8 n ) s" hello" ;
: EMPTY-STREAM$ ( -- ptr u8 n ) s\" \x03\x00" ;
: STORED-HELLO$ ( -- ptr u8 n ) s\" \x01\x05\x00\xFA\xFFhello" ;
: FIXED-HELLO$ ( -- ptr u8 n ) s\" \xCB\x48\xCD\xC9\xC9\x07\x00" ;

\ Resolved while the builder loads this file; MAIN calls it again in the saved
\ application, whose FFI cache capture has cleared.
CHECK$ ZLIB:CRC32 constant BUILD-CRC

: OUT$ ( n -- ptr u8 n ) SMALL-OUT swap ;

\ Decode into SMALL-OUT with the given capacity.
: DECODE ( ptr u8 n n -- n ) {: src:ptr srcu:n cap:n :}
   src srcu SMALL-OUT cap ZLIB:INFLATE-RAW ;

: DECODES ( ptr u8 n n ptr u8 n -- )
   {: src:ptr srcu:n cap:n want:ptr wantu:n :}
   src srcu cap DECODE OUT$ want wantu T$= ;

\ A live stream was abandoned before this call: the next one still works.
: STILL-WORKS ( -- )
   STORED-HELLO$ SMALL-CAP HELLO$ DECODES ;

: CRC-CASES ( -- )
   s" CRC-32 of 123456789 is CBF43926, also after capture" T-LABEL
   CHECK$ ZLIB:CRC32 $CBF43926 T=
   BUILD-CRC $CBF43926 T=
   s" CRC-32 of nothing is zero" T-LABEL
   SMALL-IN 0 ZLIB:CRC32 0 T= ;

: LITERAL-CASES ( -- )
   s" a fixed-Huffman empty stream decodes to nothing" T-LABEL
   EMPTY-STREAM$ SMALL-CAP DECODE 0 T=
   s" an empty stream decodes into zero capacity" T-LABEL
   EMPTY-STREAM$ 0 DECODE 0 T=
   s" a stored block decodes" T-LABEL
   STORED-HELLO$ SMALL-CAP HELLO$ DECODES
   s" a fixed-Huffman block decodes" T-LABEL
   FIXED-HELLO$ SMALL-CAP HELLO$ DECODES
   s" exact destination capacity completes" T-LABEL
   STORED-HELLO$ 5 HELLO$ DECODES
   FIXED-HELLO$ 5 HELLO$ DECODES ;

: STORED-SHORT ( -- ) STORED-HELLO$ 4 DECODE drop ;
: FIXED-SHORT ( -- ) FIXED-HELLO$ 4 DECODE drop ;
: EMPTY-ZERO ( -- ) HELLO$ drop 0 SMALL-CAP DECODE drop ;
: FIXED-CUT ( -- ) FIXED-HELLO$ 1- SMALL-CAP DECODE drop ;
: STORED-CUT ( -- ) STORED-HELLO$ 3 - SMALL-CAP DECODE drop ;
: BAD-COMPLEMENT ( -- )
   s\" \x01\x05\x00\xFA\xFEhello" SMALL-CAP DECODE drop ;
: RESERVED-TYPE ( -- ) s\" \x07\x00" SMALL-CAP DECODE drop ;
: EMPTY-TRAILING ( -- ) s\" \x03\x00\x00" SMALL-CAP DECODE drop ;
: STORED-TRAILING ( -- ) s\" \x01\x05\x00\xFA\xFFhello\x00" SMALL-CAP DECODE drop ;
: EXACT-TRAILING ( -- ) s\" \x01\x05\x00\xFA\xFFhello\x00" 5 DECODE drop ;

: STREAM-FAILURES ( -- )
   s" an understated expanded size is a capacity error" T-LABEL
   [: STORED-SHORT ;] ZLIB:E-CAPACITY TTHROWSQ STILL-WORKS
   [: FIXED-SHORT ;] ZLIB:E-CAPACITY TTHROWSQ STILL-WORKS
   s" zero compressed bytes are truncated input" T-LABEL
   [: EMPTY-ZERO ;] ZLIB:E-DATA TTHROWSQ STILL-WORKS
   s" a truncated final block is refused" T-LABEL
   [: FIXED-CUT ;] ZLIB:E-DATA TTHROWSQ STILL-WORKS
   [: STORED-CUT ;] ZLIB:E-DATA TTHROWSQ STILL-WORKS
   s" an invalid stored-block complement is refused" T-LABEL
   [: BAD-COMPLEMENT ;] ZLIB:E-DATA TTHROWSQ STILL-WORKS
   s" the reserved block type is refused" T-LABEL
   [: RESERVED-TYPE ;] ZLIB:E-DATA TTHROWSQ STILL-WORKS
   s" a byte after the final block is refused" T-LABEL
   [: EMPTY-TRAILING ;] ZLIB:E-DATA TTHROWSQ STILL-WORKS
   [: STORED-TRAILING ;] ZLIB:E-DATA TTHROWSQ STILL-WORKS
   [: EXACT-TRAILING ;] ZLIB:E-DATA TTHROWSQ STILL-WORKS ;

\ Each span is a real 64-byte buffer with a length that, if dereferenced,
\ would read or write far past it: only a refusal made before C passes.
$100000000 constant U32-OVER
: CRC-NEGATIVE ( -- ) SMALL-IN -1 ZLIB:CRC32 drop ;
: CRC-WIDE ( -- ) SMALL-IN U32-OVER ZLIB:CRC32 drop ;
: CRC-NULL ( -- ) NULL-PTR BYTE-VIEW 1 ZLIB:CRC32 drop ;
: IN-NEGATIVE ( -- ) SMALL-IN -1 SMALL-OUT SMALL-CAP ZLIB:INFLATE-RAW drop ;
: IN-WIDE ( -- ) SMALL-IN U32-OVER SMALL-OUT SMALL-CAP ZLIB:INFLATE-RAW drop ;
: OUT-NEGATIVE ( -- ) SMALL-IN 2 SMALL-OUT -1 ZLIB:INFLATE-RAW drop ;
: OUT-WIDE ( -- ) SMALL-IN 2 SMALL-OUT U32-OVER ZLIB:INFLATE-RAW drop ;
: OUT-NULL ( -- ) SMALL-IN 2 NULL-PTR BYTE-VIEW 1 ZLIB:INFLATE-RAW drop ;
: DEFLATE-WIDE ( -- ) SMALL-IN U32-OVER SMALL-OUT SMALL-CAP ZLIB:DEFLATE-RAW drop ;
: DEFLATE-OUT-WIDE ( -- ) SMALL-IN 2 SMALL-OUT U32-OVER ZLIB:DEFLATE-RAW drop ;
: INFLATE-OVERLAP ( -- ) SMALL-IN 8 SMALL-IN 4 + 8 ZLIB:INFLATE-RAW drop ;
: DEFLATE-OVERLAP ( -- ) SMALL-IN 4 + 8 SMALL-IN 8 ZLIB:DEFLATE-RAW drop ;
: BOUND-NEGATIVE ( -- ) -1 ZLIB:RAW-BOUND drop ;
: BOUND-WIDE ( -- ) $FFFFFFFF ZLIB:RAW-BOUND drop ;

: SPAN-REFUSALS ( -- )
   s" negative, wide and null spans are refused before C" T-LABEL
   [: CRC-NEGATIVE ;] ZLIB:E-SIZE TTHROWSQ
   [: CRC-WIDE ;] ZLIB:E-SIZE TTHROWSQ
   [: CRC-NULL ;] ZLIB:E-SIZE TTHROWSQ
   [: IN-NEGATIVE ;] ZLIB:E-SIZE TTHROWSQ
   [: IN-WIDE ;] ZLIB:E-SIZE TTHROWSQ
   [: OUT-NEGATIVE ;] ZLIB:E-SIZE TTHROWSQ
   [: OUT-WIDE ;] ZLIB:E-SIZE TTHROWSQ
   [: OUT-NULL ;] ZLIB:E-SIZE TTHROWSQ
   [: DEFLATE-WIDE ;] ZLIB:E-SIZE TTHROWSQ
   [: DEFLATE-OUT-WIDE ;] ZLIB:E-SIZE TTHROWSQ
   s" overlapping source and destination are refused" T-LABEL
   [: INFLATE-OVERLAP ;] ZLIB:E-SIZE TTHROWSQ
   [: DEFLATE-OVERLAP ;] ZLIB:E-SIZE TTHROWSQ
   s" a bound past 32 bits is refused" T-LABEL
   [: BOUND-NEGATIVE ;] ZLIB:E-SIZE TTHROWSQ
   [: BOUND-WIDE ;] ZLIB:E-SIZE TTHROWSQ
   STILL-WORKS ;

PTR-VARIABLE FIT-SRC variable FIT-SRC-U
PTR-VARIABLE FIT-DST variable FIT-CAP

: FIT ( n -- n ) {: cap:n :}
   FIT-SRC @ FIT-SRC-U @ FIT-DST @ cap ZLIB:DEFLATE-RAW ;
: FIT-SHORT ( -- ) FIT-CAP @ 1- FIT drop ;

\ The stream src compresses to, packed bytes long, fits a destination of
\ exactly that size, which it then holds, and not one byte less.
: EXACT-FIT ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu:n dst:ptr packed:n :}
   src FIT-SRC ! srcu FIT-SRC-U ! dst FIT-DST ! packed FIT-CAP !
   [: FIT-SHORT ;] ZLIB:E-CAPACITY TTHROWSQ
   packed FIT packed T= ;

\ Compress SMALL-IN's first n bytes into SMALL-OUT, again at exactly their
\ compressed size, then decode them back into SMALL-IN's upper half.
: SMALL-ROUND ( n -- ) {: n:n :}
   SMALL-IN n SMALL-OUT SMALL-CAP ZLIB:DEFLATE-RAW {: packed:n :}
   SMALL-IN n SMALL-OUT packed EXACT-FIT
   SMALL-OUT packed SMALL-IN 32 + 32 ZLIB:INFLATE-RAW n T=
   SMALL-IN 32 + n SMALL-IN n T$= ;

: DEFLATE-ZERO ( -- ) HELLO$ SMALL-OUT 0 ZLIB:DEFLATE-RAW drop ;
: DEFLATE-SHORT ( -- ) HELLO$ SMALL-OUT 2 ZLIB:DEFLATE-RAW drop ;

: SMALL-DEFLATE ( -- )
   32 0 ?do 255 i 37 * - $FF and SMALL-IN i + c! loop
   s" empty plaintext round-trips, exactly in two bytes" T-LABEL
   0 SMALL-ROUND
   SMALL-OUT 2 EMPTY-STREAM$ T$=
   s" binary plaintext round-trips" T-LABEL
   32 SMALL-ROUND
   s" text round-trips at its exact compressed size" T-LABEL
   HELLO$ SMALL-IN swap BYTE-COPY 5 SMALL-ROUND
   s" a full destination is a capacity error" T-LABEL
   [: DEFLATE-ZERO ;] ZLIB:E-CAPACITY TTHROWSQ STILL-WORKS
   [: DEFLATE-SHORT ;] ZLIB:E-CAPACITY TTHROWSQ STILL-WORKS
   s" the empty bound holds the empty stream" T-LABEL
   0 ZLIB:RAW-BOUND 2 >= TTRUE ;

\ A deterministic mix: runs of one byte, a repeating text and an LCG's bytes,
\ so both literal and match coding are exercised.
: FILL ( ptr u8 n -- ) {: p:ptr u:n :}
   $2545F491 u 0 ?do
      $41C64E6D * $3039 + $FFFFFFFF and
      i $10000 / 3 mod
      dup 0= if drop i $4000 / $FF and else
      1 = if i 26 mod [char] a + else dup 16 rshift $FF and then then
      p i + c!
   loop drop ;

: ARTIFACT$ ( ptr u8 n -- ptr u8 n ) {: name:ptr nameu:n :}
   0 SCRIPT-ARGV$ name nameu PATH JOIN-PATH PATH swap ;

PTR-VARIABLE PLAIN
PTR-VARIABLE PACKED
PTR-VARIABLE BACK
variable PACKED-BOUND
variable PACKED-SIZE

: PLAIN$ ( -- ptr u8 n ) PLAIN @ PAYLOAD-BYTES ;
: PACKED$ ( -- ptr u8 n ) PACKED @ PACKED-SIZE @ ;
: BACK$ ( -- ptr u8 n ) BACK @ PAYLOAD-BYTES ;
: RELEASE ( ptr u8 n -- ) MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES ;

: DECODE-SHORT ( -- )
   PACKED$ BACK @ PAYLOAD-BYTES 1- ZLIB:INFLATE-RAW drop ;

: ALLOCATE ( -- )
   PAYLOAD-BYTES MEM-ALLOC-BYTES drop PLAIN !
   PAYLOAD-BYTES ZLIB:RAW-BOUND PACKED-BOUND !
   PACKED-BOUND @ MEM-ALLOC-BYTES drop PACKED !
   PAYLOAD-BYTES MEM-ALLOC-BYTES drop BACK ! ;

: LARGE ( -- )
   ALLOCATE
   PLAIN$ FILL
   s" a 10 MiB payload round-trips through the bound" T-LABEL
   PLAIN$ PACKED @ PACKED-BOUND @ ZLIB:DEFLATE-RAW PACKED-SIZE !
   PACKED-SIZE @ PACKED-BOUND @ <= TTRUE
   s" a 10 MiB payload compresses at exactly its size, not one byte less" T-LABEL
   PLAIN$ PACKED$ EXACT-FIT
   PACKED$ BACK$ ZLIB:INFLATE-RAW PAYLOAD-BYTES T=
   BACK$ PLAIN$ STR= TTRUE
   BACK$ ZLIB:CRC32 PLAIN$ ZLIB:CRC32 T=
   s" a 10 MiB payload one byte over its declared size is refused" T-LABEL
   [: DECODE-SHORT ;] ZLIB:E-CAPACITY TTHROWSQ
   s" original.bin" ARTIFACT$ PLAIN$ WRITE-ALL
   s" compressed.bin" ARTIFACT$ PACKED$ WRITE-ALL
   s" decoded.bin" ARTIFACT$ BACK$ WRITE-ALL
   PLAIN$ RELEASE PACKED @ PACKED-BOUND @ RELEASE BACK$ RELEASE ;

public
: MAIN ( -- )
   T-RESET
   CRC-CASES LITERAL-CASES STREAM-FAILURES SPAN-REFUSALS
   SMALL-DEFLATE LARGE
   T-REPORT
   s" zlib-source: ok" type cr ;
;package

: MAIN ( -- ) ZLIB-SOURCE:MAIN ;
