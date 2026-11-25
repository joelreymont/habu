\ ZLIB cases. Refusals are made before C is called; each failure after libz has
\ a live stream is followed by a successful operation. The 10 MiB original.bin,
\ compressed.bin (raw DEFLATE) and decoded.bin go in a temporary directory
\ the process removes at exit.
require src/lib/zlib.f
require src/lib/test.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f

package ZLIB-SOURCE
private

\ The observed SolidWorks workload's largest expanded member is about 10 MiB.
$A00000 constant PAYLOAD-BYTES
64 constant SMALL-CAP
create SMALL-IN SMALL-CAP allot
create SMALL-OUT SMALL-CAP allot
create ROOT PATH-CAP allot variable ROOT-U
create PATH PATH-CAP allot

: CHECK$ ( -- str ) s" 123456789" ;
: HELLO$ ( -- str ) s" hello" ;
: EMPTY-STREAM$ ( -- str ) s\" \x03\x00" ;
: STORED-HELLO$ ( -- str ) s\" \x01\x05\x00\xFA\xFFhello" ;
: FIXED-HELLO$ ( -- str ) s\" \xCB\x48\xCD\xC9\xC9\x07\x00" ;
\ FIXED-HELLO$ in the zlib wrapper: header 78 9C, then the big-endian Adler-32
\ of "hello", 062C0215.
: FRAMED-HELLO$ ( -- str ) s\" \x78\x9C\xCB\x48\xCD\xC9\xC9\x07\x00\x06\x2C\x02\x15" ;
: EMPTY-FRAMED$ ( -- str ) s\" \x78\x9C\x03\x00\x00\x00\x00\x01" ;

: OUT$ ( n -- str ) SMALL-OUT swap >str ;

: DECODE ( str n -- n ) {: src:str cap:n :}
   src SMALL-OUT cap >str ZLIB:INFLATE-RAW ;

: DECODES ( str n str -- )
   {: src:str cap:n want:str :}
   src cap DECODE OUT$ want T$= ;

\ A live stream was abandoned before this call: the next one still works.
: STILL-WORKS ( -- )
   STORED-HELLO$ SMALL-CAP HELLO$ DECODES ;

: CRC-CASES ( -- )
   s" CRC-32 of 123456789 is CBF43926" T-LABEL
   CHECK$ ZLIB:CRC32 $CBF43926 T=
   s" CRC-32 of nothing is zero" T-LABEL
   SMALL-IN 0 >str ZLIB:CRC32 0 T= ;

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
: EMPTY-ZERO ( -- ) HELLO$ STR:DATA 0 >str SMALL-CAP DECODE drop ;
: FIXED-CUT ( -- )
   FIXED-HELLO$ dup STR:DATA swap STR:LEN 1- >str SMALL-CAP DECODE drop ;
: STORED-CUT ( -- )
   STORED-HELLO$ dup STR:DATA swap STR:LEN 3 - >str SMALL-CAP DECODE drop ;
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
: CRC-NEGATIVE ( -- ) SMALL-IN -1 >str ZLIB:CRC32 drop ;
: CRC-WIDE ( -- ) SMALL-IN U32-OVER >str ZLIB:CRC32 drop ;
: CRC-NULL ( -- ) NULL-PTR BYTE-VIEW 1 >str ZLIB:CRC32 drop ;
: IN-NEGATIVE ( -- )
   SMALL-IN -1 >str SMALL-OUT SMALL-CAP >str ZLIB:INFLATE-RAW drop ;
: IN-WIDE ( -- )
   SMALL-IN U32-OVER >str SMALL-OUT SMALL-CAP >str ZLIB:INFLATE-RAW drop ;
: OUT-NEGATIVE ( -- )
   SMALL-IN 2 >str SMALL-OUT -1 >str ZLIB:INFLATE-RAW drop ;
: OUT-WIDE ( -- )
   SMALL-IN 2 >str SMALL-OUT U32-OVER >str ZLIB:INFLATE-RAW drop ;
: OUT-NULL ( -- )
   SMALL-IN 2 >str NULL-PTR BYTE-VIEW 1 >str ZLIB:INFLATE-RAW drop ;
: DEFLATE-WIDE ( -- )
   SMALL-IN U32-OVER >str SMALL-OUT SMALL-CAP >str ZLIB:DEFLATE-RAW drop ;
: DEFLATE-OUT-WIDE ( -- )
   SMALL-IN 2 >str SMALL-OUT U32-OVER >str ZLIB:DEFLATE-RAW drop ;
: INFLATE-OVERLAP ( -- )
   SMALL-IN 8 >str SMALL-IN 4 + 8 >str ZLIB:INFLATE-RAW drop ;
: DEFLATE-OVERLAP ( -- )
   SMALL-IN 4 + 8 >str SMALL-IN 8 >str ZLIB:DEFLATE-RAW drop ;
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

defer FIT-CODEC ( str str -- n )

: FIT ( n -- n ) {: cap:n :}
   FIT-SRC @ FIT-SRC-U @ >str FIT-DST @ cap >str FIT-CODEC ;
: FIT-SHORT ( -- ) FIT-CAP @ 1- FIT drop ;

\ The stream the codec compresses src to, packed bytes long, fits a
\ destination of exactly that size, which it then holds, and not one byte less.
\ The destination is zeroed before the exact call, so the caller's decode
\ checks bytes that call wrote, not an earlier call's.
: EXACT-FIT ( str str [ str str -- n ] -- )
   is FIT-CODEC {: src:str dst:str :}
   src STR:DATA FIT-SRC !  src STR:LEN FIT-SRC-U !
   dst STR:DATA FIT-DST !  dst STR:LEN FIT-CAP !
   [: FIT-SHORT ;] ZLIB:E-CAPACITY TTHROWSQ
   dst STR:LEN 0 ?do 0 dst STR:DATA i + c! loop
   dst STR:LEN FIT dst STR:LEN T= ;

\ Compress SMALL-IN's first n bytes into SMALL-OUT, again at exactly their
\ compressed size, then decode them back into SMALL-IN's upper half.
: SMALL-ROUND ( n -- ) {: n:n :}
   SMALL-IN n >str SMALL-OUT SMALL-CAP >str ZLIB:DEFLATE-RAW {: packed:n :}
   SMALL-IN n >str SMALL-OUT packed >str [: ZLIB:DEFLATE-RAW ;] EXACT-FIT
   SMALL-OUT packed >str SMALL-IN 32 + 32 >str ZLIB:INFLATE-RAW n T=
   SMALL-IN 32 + n >str SMALL-IN n >str T$= ;

: DEFLATE-ZERO ( -- ) HELLO$ SMALL-OUT 0 >str ZLIB:DEFLATE-RAW drop ;
: DEFLATE-SHORT ( -- ) HELLO$ SMALL-OUT 2 >str ZLIB:DEFLATE-RAW drop ;

: SMALL-DEFLATE ( -- )
   32 0 ?do 255 i 37 * - $FF and SMALL-IN i + c! loop
   s" empty plaintext round-trips, exactly in two bytes" T-LABEL
   0 SMALL-ROUND
   SMALL-OUT 2 >str EMPTY-STREAM$ T$=
   s" binary plaintext round-trips" T-LABEL
   32 SMALL-ROUND
   s" text round-trips at its exact compressed size" T-LABEL
   HELLO$ STR:DATA SMALL-IN HELLO$ STR:LEN BYTE-COPY 5 SMALL-ROUND
   s" a full destination is a capacity error" T-LABEL
   [: DEFLATE-ZERO ;] ZLIB:E-CAPACITY TTHROWSQ STILL-WORKS
   [: DEFLATE-SHORT ;] ZLIB:E-CAPACITY TTHROWSQ STILL-WORKS
   s" the empty bound holds the empty stream" T-LABEL
   0 ZLIB:RAW-BOUND 2 >= TTRUE ;

: FRAMED-DECODE ( str n -- n ) {: src:str cap:n :}
   src SMALL-OUT cap >str ZLIB:INFLATE-FRAMED ;

\ Compress text, then compress it again at exactly its compressed size, then
\ decode it back; lit is its literal framed stream, whose trailer it shares.
: FRAMED-ROUND ( str str -- ) {: src:str lit:str :}
   src SMALL-OUT src STR:LEN ZLIB:FRAMED-BOUND >str ZLIB:DEFLATE-FRAMED
   {: packed:n :}
   SMALL-OUT c@ $78 T=
   SMALL-OUT packed + 4 - 4 >str lit STR:DATA lit STR:LEN + 4 - 4 >str T$=
   src SMALL-OUT packed >str [: ZLIB:DEFLATE-FRAMED ;] EXACT-FIT
   SMALL-OUT packed >str SMALL-IN SMALL-CAP >str ZLIB:INFLATE-FRAMED
   src STR:LEN T=
   src STR:LEN 0<> if SMALL-IN src STR:LEN >str src T$= then ;

: BAD-ADLER ( -- )
   s\" \x78\x9C\xCB\x48\xCD\xC9\xC9\x07\x00\x06\x2C\x02\x16" SMALL-CAP FRAMED-DECODE drop ;
: BAD-FCHECK ( -- )
   s\" \x78\x9D\xCB\x48\xCD\xC9\xC9\x07\x00\x06\x2C\x02\x15" SMALL-CAP FRAMED-DECODE drop ;
\ FDICT set, FCHECK valid (0x78BB is 31 * 997), dictionary id 1, an empty body.
: PRESET-DICT ( -- )
   s\" \x78\xBB\x00\x00\x00\x01\x03\x00\x00\x00\x00\x01" SMALL-CAP FRAMED-DECODE drop ;
: RAW-AS-FRAMED ( -- ) FIXED-HELLO$ SMALL-CAP FRAMED-DECODE drop ;
: FRAMED-AS-RAW ( -- ) FRAMED-HELLO$ SMALL-CAP DECODE drop ;
: FRAMED-TRAILING ( -- )
   s\" \x78\x9C\xCB\x48\xCD\xC9\xC9\x07\x00\x06\x2C\x02\x15\x00" SMALL-CAP FRAMED-DECODE drop ;
\ Every plaintext byte fits, so the missing trailer is found past a full
\ destination.
: FRAMED-CUT ( -- )
   FRAMED-HELLO$ dup STR:DATA swap STR:LEN 2 - >str 5 FRAMED-DECODE drop ;
: FRAMED-SHORT ( -- ) FRAMED-HELLO$ 4 FRAMED-DECODE drop ;
: FRAMED-OVERLAP ( -- )
   SMALL-IN 8 >str SMALL-IN 4 + 8 >str ZLIB:INFLATE-FRAMED drop ;
: FRAMED-BOUND-WIDE ( -- ) $FFFFFFFF ZLIB:FRAMED-BOUND drop ;

: FRAMED-CASES ( -- )
   s" a zlib-framed fixed-Huffman stream decodes" T-LABEL
   FRAMED-HELLO$ SMALL-CAP FRAMED-DECODE OUT$ HELLO$ T$=
   s" an empty zlib-framed stream decodes into zero capacity" T-LABEL
   EMPTY-FRAMED$ 0 FRAMED-DECODE 0 T=
   s" a corrupt Adler-32 trailer is refused" T-LABEL
   [: BAD-ADLER ;] ZLIB:E-DATA TTHROWSQ STILL-WORKS
   s" a header that fails its check is refused" T-LABEL
   [: BAD-FCHECK ;] ZLIB:E-DATA TTHROWSQ STILL-WORKS
   s" a stream that asks for a preset dictionary is refused" T-LABEL
   [: PRESET-DICT ;] ZLIB:E-DATA TTHROWSQ STILL-WORKS
   s" raw and framed streams are not interchangeable" T-LABEL
   [: RAW-AS-FRAMED ;] ZLIB:E-DATA TTHROWSQ STILL-WORKS
   [: FRAMED-AS-RAW ;] ZLIB:E-DATA TTHROWSQ STILL-WORKS
   s" a byte after the trailer is refused" T-LABEL
   [: FRAMED-TRAILING ;] ZLIB:E-DATA TTHROWSQ STILL-WORKS
   s" a stream cut inside its trailer is refused" T-LABEL
   [: FRAMED-CUT ;] ZLIB:E-DATA TTHROWSQ STILL-WORKS
   s" an understated framed expanded size is a capacity error" T-LABEL
   [: FRAMED-SHORT ;] ZLIB:E-CAPACITY TTHROWSQ STILL-WORKS
   s" framed overlap and a framed bound past 32 bits are refused" T-LABEL
   [: FRAMED-OVERLAP ;] ZLIB:E-SIZE TTHROWSQ
   [: FRAMED-BOUND-WIDE ;] ZLIB:E-SIZE TTHROWSQ
   STILL-WORKS
   s" framed text round-trips at its exact size, with header and Adler-32" T-LABEL
   HELLO$ FRAMED-HELLO$ FRAMED-ROUND
   s" framed empty input round-trips at its exact size" T-LABEL
   HELLO$ STR:DATA 0 >str EMPTY-FRAMED$ FRAMED-ROUND ;

\ A deterministic mix: runs of one byte, a repeating text and an LCG's bytes,
\ so both literal and match coding are exercised.
: FILL ( ^u8 n -- ) {: p:^ u:n :}
   $2545F491 u 0 ?do
      $41C64E6D * $3039 + $FFFFFFFF and
      i $10000 / 3 mod
      dup 0= if drop i $4000 / $FF and else
      1 = if i 26 mod [char] a + else dup 16 rshift $FF and then then
      p i + c!
   loop drop ;

: ARTIFACT$ ( str -- str ) {: name:str :}
   ROOT ROOT-U @ >str name PATH FS:JOIN-PATH PATH swap >str ;

PTR-VARIABLE PLAIN
PTR-VARIABLE PACKED
PTR-VARIABLE BACK
variable PACKED-BOUND
variable PACKED-CAP
variable PACKED-SIZE

: PLAIN$ ( -- str ) PLAIN @ PAYLOAD-BYTES >str ;
: PACKED$ ( -- str ) PACKED @ PACKED-SIZE @ >str ;
: BACK$ ( -- str ) BACK @ PAYLOAD-BYTES >str ;
: RELEASE ( ^u8 n -- ) MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES ;

: BACK-CLEAR ( -- )
   PAYLOAD-BYTES 0 ?do 0 BACK @ i + c! loop ;

: DECODE-SHORT ( -- )
   PACKED$ BACK @ PAYLOAD-BYTES 1- >str ZLIB:INFLATE-RAW drop ;

: ALLOCATE ( -- )
   PAYLOAD-BYTES MEM-ALLOC-BYTES drop PLAIN !
   PAYLOAD-BYTES ZLIB:RAW-BOUND PACKED-BOUND !
   PAYLOAD-BYTES ZLIB:FRAMED-BOUND PACKED-CAP !
   PACKED-CAP @ MEM-ALLOC-BYTES drop PACKED !
   PAYLOAD-BYTES MEM-ALLOC-BYTES drop BACK ! ;

: LARGE ( -- )
   ALLOCATE
   PLAIN @ PAYLOAD-BYTES FILL
   s" a 10 MiB payload round-trips through the bound" T-LABEL
   PLAIN$ PACKED @ PACKED-BOUND @ >str ZLIB:DEFLATE-RAW PACKED-SIZE !
   PACKED-SIZE @ PACKED-BOUND @ <= TTRUE
   s" a 10 MiB payload compresses at exactly its size, not one byte less" T-LABEL
   PLAIN$ PACKED$ [: ZLIB:DEFLATE-RAW ;] EXACT-FIT
   PACKED$ BACK$ ZLIB:INFLATE-RAW PAYLOAD-BYTES T=
   BACK$ PLAIN$ STR:= TTRUE
   BACK$ ZLIB:CRC32 PLAIN$ ZLIB:CRC32 T=
   s" a 10 MiB payload one byte over its declared size is refused" T-LABEL
   [: DECODE-SHORT ;] ZLIB:E-CAPACITY TTHROWSQ
   s" original.bin" ARTIFACT$ PLAIN$ FS:WRITE-ALL
   s" compressed.bin" ARTIFACT$ PACKED$ FS:WRITE-ALL
   s" decoded.bin" ARTIFACT$ BACK$ FS:WRITE-ALL
   s" a 10 MiB payload round-trips framed through the framed bound" T-LABEL
   BACK-CLEAR
   PLAIN$ PACKED @ PACKED-CAP @ >str ZLIB:DEFLATE-FRAMED PACKED-SIZE !
   PACKED$ BACK$ ZLIB:INFLATE-FRAMED PAYLOAD-BYTES T=
   BACK$ PLAIN$ STR:= TTRUE
   PLAIN @ PAYLOAD-BYTES RELEASE PACKED @ PACKED-CAP @ RELEASE
   BACK @ PAYLOAD-BYTES RELEASE ;

public
: MAIN ( -- )
   T-RESET
   s" zlib" FS:MKTEMP-DIR {: root:str :}
   root FS:CLEANUP-TREE+
   root STR:DATA ROOT root STR:LEN BYTE-COPY root STR:LEN ROOT-U !

   CRC-CASES LITERAL-CASES STREAM-FAILURES SPAN-REFUSALS
   SMALL-DEFLATE FRAMED-CASES LARGE
   T-REPORT
   s" zlib-source: ok" type cr ;
;package

ZLIB-SOURCE:MAIN
