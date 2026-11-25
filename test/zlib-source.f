\ ZLIB cases. Rejections are made before C is called; each failure after libz has
\ a live stream is followed by a successful operation. Each codec case appends
\ to a vector it makes on the heap allocator and deletes. The 10 MiB
\ original.bin, compressed.bin (raw DEFLATE) and decoded.bin go in a temporary
\ directory the process removes at exit.
require src/lib/zlib.f
require src/lib/test.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/heap.f
require src/lib/vector.f
require src/lib/fs.f
require src/lib/fs-mutate.f

package ZLIB-SOURCE
private

\ The observed SolidWorks workload's largest expanded member is about 10 MiB.
$A00000 constant PAYLOAD-BYTES
64 constant SMALL-CAP
create SMALL-IN SMALL-CAP allot
create ROOT PATH-CAP allot variable ROOT-U
create PATH PATH-CAP allot

: CHECK$ ( -- str ) s" 123456789" ;
: HELLO$ ( -- str ) s" hello" ;
: NOTHING$ ( -- str ) HELLO$ STR:DATA 0 >str ;
: EMPTY-STREAM$ ( -- str ) s\" \x03\x00" ;
: STORED-HELLO$ ( -- str ) s\" \x01\x05\x00\xFA\xFFhello" ;
: FIXED-HELLO$ ( -- str ) s\" \xCB\x48\xCD\xC9\xC9\x07\x00" ;
\ FIXED-HELLO$ in the zlib wrapper: header 78 9C, then the big-endian Adler-32
\ of "hello", 062C0215.
: FRAMED-HELLO$ ( -- str ) s\" \x78\x9C\xCB\x48\xCD\xC9\xC9\x07\x00\x06\x2C\x02\x15" ;
: EMPTY-FRAMED$ ( -- str ) s\" \x78\x9C\x03\x00\x00\x00\x00\x01" ;

: FRESH ( -- ^VEC:vector<u8> ) HEAP:ALLOCATOR 0 [type] u8 VEC:NEW ;

\ The codec appends at most n bytes of src to a fresh vector, which then
\ holds exactly want.
: INFLATES ( str n str [ str ^VEC:vector<u8> n -- ] -- )
   {: src:str most:n want:str codec :}
   FRESH {: v:^ :}
   src v most codec execute
   v VEC:BYTES want T$=
   v VEC:DELETE ;

\ A decode for its throw: the fresh vector is deleted on every path.
: INFLATE-CASE ( str n [ str ^VEC:vector<u8> n -- ] -- )
   {: src:str most:n codec :}
   FRESH {: v:^ :}
   [: src v most codec execute ;] [: v VEC:DELETE ;] finally ;

: DECODES ( str n str -- ) [: ZLIB:INFLATE-RAW ;] INFLATES ;
: DECODE ( str n -- ) [: ZLIB:INFLATE-RAW ;] INFLATE-CASE ;
: FRAMED-DECODES ( str n str -- ) [: ZLIB:INFLATE-FRAMED ;] INFLATES ;
: FRAMED-DECODE ( str n -- ) [: ZLIB:INFLATE-FRAMED ;] INFLATE-CASE ;

\ A deflate for its throw, on a fresh vector deleted on every path.
: DEFLATE-CASE ( str -- ) {: src:str :}
   FRESH {: v:^ :}
   [: src v ZLIB:DEFLATE-RAW ;] [: v VEC:DELETE ;] finally ;

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
   EMPTY-STREAM$ SMALL-CAP NOTHING$ DECODES
   s" an empty stream decodes into zero capacity" T-LABEL
   EMPTY-STREAM$ 0 NOTHING$ DECODES
   s" a stored block decodes" T-LABEL
   STORED-HELLO$ SMALL-CAP HELLO$ DECODES
   s" a fixed-Huffman block decodes" T-LABEL
   FIXED-HELLO$ SMALL-CAP HELLO$ DECODES
   s" exact destination capacity completes" T-LABEL
   STORED-HELLO$ 5 HELLO$ DECODES
   FIXED-HELLO$ 5 HELLO$ DECODES ;

: STORED-SHORT ( -- ) STORED-HELLO$ 4 DECODE ;
: FIXED-SHORT ( -- ) FIXED-HELLO$ 4 DECODE ;
: EMPTY-ZERO ( -- ) NOTHING$ SMALL-CAP DECODE ;
: FIXED-CUT ( -- )
   FIXED-HELLO$ dup STR:DATA swap STR:LEN 1- >str SMALL-CAP DECODE ;
: STORED-CUT ( -- )
   STORED-HELLO$ dup STR:DATA swap STR:LEN 3 - >str SMALL-CAP DECODE ;
: BAD-COMPLEMENT ( -- )
   s\" \x01\x05\x00\xFA\xFEhello" SMALL-CAP DECODE ;
: RESERVED-TYPE ( -- ) s\" \x07\x00" SMALL-CAP DECODE ;
: EMPTY-TRAILING ( -- ) s\" \x03\x00\x00" SMALL-CAP DECODE ;
: STORED-TRAILING$ ( -- str ) s\" \x01\x05\x00\xFA\xFFhello\x00" ;
: STORED-TRAILING ( -- ) STORED-TRAILING$ SMALL-CAP DECODE ;
: EXACT-TRAILING ( -- ) STORED-TRAILING$ 5 DECODE ;

: STREAM-FAILURES ( -- )
   s" an understated expanded size is a capacity error" T-LABEL
   [: STORED-SHORT ;] ZLIB:E-CAPACITY TTHROWSQ STILL-WORKS
   [: FIXED-SHORT ;] ZLIB:E-CAPACITY TTHROWSQ STILL-WORKS
   s" zero compressed bytes are truncated input" T-LABEL
   [: EMPTY-ZERO ;] ZLIB:E-DATA TTHROWSQ STILL-WORKS
   s" a truncated final block is rejected" T-LABEL
   [: FIXED-CUT ;] ZLIB:E-DATA TTHROWSQ STILL-WORKS
   [: STORED-CUT ;] ZLIB:E-DATA TTHROWSQ STILL-WORKS
   s" an invalid stored-block complement is rejected" T-LABEL
   [: BAD-COMPLEMENT ;] ZLIB:E-DATA TTHROWSQ STILL-WORKS
   s" the reserved block type is rejected" T-LABEL
   [: RESERVED-TYPE ;] ZLIB:E-DATA TTHROWSQ STILL-WORKS
   s" a byte after the final block is rejected" T-LABEL
   [: EMPTY-TRAILING ;] ZLIB:E-DATA TTHROWSQ STILL-WORKS
   [: STORED-TRAILING ;] ZLIB:E-DATA TTHROWSQ STILL-WORKS
   [: EXACT-TRAILING ;] ZLIB:E-DATA TTHROWSQ STILL-WORKS ;

\ A vector's earlier bytes stay through an append and through a failure
\ after the stream wrote into the room.
: KEPT ( ^VEC:vector<u8> -- ) VEC:BYTES s" 123456789hello" T$= ;

: KEEPS-INFLATE ( ^VEC:vector<u8> -- ) {: v:^ :}
   STORED-HELLO$ v SMALL-CAP ZLIB:INFLATE-RAW  v KEPT
   [: STORED-HELLO$ v 4 ZLIB:INFLATE-RAW ;] ZLIB:E-CAPACITY TTHROWSQ
   v KEPT
   [: STORED-TRAILING$ v SMALL-CAP ZLIB:INFLATE-RAW ;] ZLIB:E-DATA TTHROWSQ
   v KEPT ;

\ The appended stream starts past the 14 bytes KEPT reads.
: KEEPS-DEFLATE ( ^VEC:vector<u8> -- ) {: v:^ :}
   HELLO$ v ZLIB:DEFLATE-RAW
   v VEC:COUNT 14 - {: k:n :}
   v 14 VEC:AT k >str SMALL-CAP HELLO$ DECODES
   v 14 VEC:RESIZE  v KEPT ;

: KEEPS ( -- )
   s" an append keeps the vector's earlier bytes, on success and failure" T-LABEL
   FRESH {: v:^ :}
   CHECK$ v VEC:APPEND
   v KEEPS-INFLATE v KEEPS-DEFLATE
   v VEC:DELETE ;

\ Each span is a real 64-byte buffer with a length that, if dereferenced,
\ would read or write far past it: only a rejection made before C passes.
$100000000 constant U32-OVER
: CRC-NEGATIVE ( -- ) SMALL-IN -1 >str ZLIB:CRC32 drop ;
: CRC-WIDE ( -- ) SMALL-IN U32-OVER >str ZLIB:CRC32 drop ;
: CRC-NULL ( -- ) NULL-PTR BYTE-VIEW 1 >str ZLIB:CRC32 drop ;
: IN-NEGATIVE ( -- ) SMALL-IN -1 >str SMALL-CAP DECODE ;
: IN-WIDE ( -- ) SMALL-IN U32-OVER >str SMALL-CAP DECODE ;
: MOST-NEGATIVE ( -- ) SMALL-IN 2 >str -1 DECODE ;
: MOST-WIDE ( -- ) SMALL-IN 2 >str U32-OVER DECODE ;
: DEFLATE-WIDE ( -- ) SMALL-IN U32-OVER >str DEFLATE-CASE ;

: SPAN-REJECTIONS ( -- )
   s" negative, wide and null spans are rejected before C" T-LABEL
   [: CRC-NEGATIVE ;] ZLIB:E-SIZE TTHROWSQ
   [: CRC-WIDE ;] ZLIB:E-SIZE TTHROWSQ
   [: CRC-NULL ;] ZLIB:E-SIZE TTHROWSQ
   [: IN-NEGATIVE ;] ZLIB:E-SIZE TTHROWSQ
   [: IN-WIDE ;] ZLIB:E-SIZE TTHROWSQ
   [: DEFLATE-WIDE ;] ZLIB:E-SIZE TTHROWSQ
   s" a negative or past-32-bit most is rejected before C" T-LABEL
   [: MOST-NEGATIVE ;] ZLIB:E-SIZE TTHROWSQ
   [: MOST-WIDE ;] ZLIB:E-SIZE TTHROWSQ
   STILL-WORKS ;

\ Compresses SMALL-IN's first n bytes and decodes them back; answers the
\ stream, which the caller deletes.
: SMALL-ROUND ( n -- ^VEC:vector<u8> ) {: n:n :}
   FRESH {: packed:^ :}
   SMALL-IN n >str packed ZLIB:DEFLATE-RAW
   packed VEC:BYTES SMALL-CAP SMALL-IN n >str DECODES
   packed ;

: SMALL-DEFLATE ( -- )
   32 0 ?do 255 i 37 * - $FF and SMALL-IN i + c! loop
   s" empty plaintext round-trips, exactly in two bytes" T-LABEL
   0 SMALL-ROUND {: empty:^ :}
   empty VEC:BYTES EMPTY-STREAM$ T$=
   empty VEC:DELETE
   s" binary plaintext round-trips" T-LABEL
   32 SMALL-ROUND VEC:DELETE
   s" text round-trips" T-LABEL
   HELLO$ STR:DATA SMALL-IN HELLO$ STR:LEN BYTE-COPY
   5 SMALL-ROUND VEC:DELETE ;

: TRAILER ( str -- str ) {: s:str :} s STR:DATA s STR:LEN + 4 - 4 >str ;

\ Compress text, then decode it back; lit is its literal framed stream, whose
\ trailer it shares.
: FRAMED-ROUND ( str str -- ) {: src:str lit:str :}
   FRESH {: packed:^ :}
   src packed ZLIB:DEFLATE-FRAMED
   packed VEC:BYTES {: s:str :}
   s STR:DATA c@ $78 T=
   s TRAILER lit TRAILER T$=
   s SMALL-CAP src FRAMED-DECODES
   packed VEC:DELETE ;

: BAD-ADLER ( -- )
   s\" \x78\x9C\xCB\x48\xCD\xC9\xC9\x07\x00\x06\x2C\x02\x16" SMALL-CAP FRAMED-DECODE ;
: BAD-FCHECK ( -- )
   s\" \x78\x9D\xCB\x48\xCD\xC9\xC9\x07\x00\x06\x2C\x02\x15" SMALL-CAP FRAMED-DECODE ;
\ FDICT set, FCHECK valid (0x78BB is 31 * 997), dictionary id 1, an empty body.
: PRESET-DICT ( -- )
   s\" \x78\xBB\x00\x00\x00\x01\x03\x00\x00\x00\x00\x01" SMALL-CAP FRAMED-DECODE ;
: RAW-AS-FRAMED ( -- ) FIXED-HELLO$ SMALL-CAP FRAMED-DECODE ;
: FRAMED-AS-RAW ( -- ) FRAMED-HELLO$ SMALL-CAP DECODE ;
: FRAMED-TRAILING ( -- )
   s\" \x78\x9C\xCB\x48\xCD\xC9\xC9\x07\x00\x06\x2C\x02\x15\x00" SMALL-CAP FRAMED-DECODE ;
\ Every plaintext byte fits, so the missing trailer is found past a full
\ destination.
: FRAMED-CUT ( -- )
   FRAMED-HELLO$ dup STR:DATA swap STR:LEN 2 - >str 5 FRAMED-DECODE ;
: FRAMED-SHORT ( -- ) FRAMED-HELLO$ 4 FRAMED-DECODE ;

: FRAMED-CASES ( -- )
   s" a zlib-framed fixed-Huffman stream decodes" T-LABEL
   FRAMED-HELLO$ SMALL-CAP HELLO$ FRAMED-DECODES
   s" an empty zlib-framed stream decodes into zero capacity" T-LABEL
   EMPTY-FRAMED$ 0 NOTHING$ FRAMED-DECODES
   s" a corrupt Adler-32 trailer is rejected" T-LABEL
   [: BAD-ADLER ;] ZLIB:E-DATA TTHROWSQ STILL-WORKS
   s" a header that fails its check is rejected" T-LABEL
   [: BAD-FCHECK ;] ZLIB:E-DATA TTHROWSQ STILL-WORKS
   s" a stream that asks for a preset dictionary is rejected" T-LABEL
   [: PRESET-DICT ;] ZLIB:E-DATA TTHROWSQ STILL-WORKS
   s" raw and framed streams are not interchangeable" T-LABEL
   [: RAW-AS-FRAMED ;] ZLIB:E-DATA TTHROWSQ STILL-WORKS
   [: FRAMED-AS-RAW ;] ZLIB:E-DATA TTHROWSQ STILL-WORKS
   s" a byte after the trailer is rejected" T-LABEL
   [: FRAMED-TRAILING ;] ZLIB:E-DATA TTHROWSQ STILL-WORKS
   s" a stream cut inside its trailer is rejected" T-LABEL
   [: FRAMED-CUT ;] ZLIB:E-DATA TTHROWSQ STILL-WORKS
   s" an understated framed expanded size is a capacity error" T-LABEL
   [: FRAMED-SHORT ;] ZLIB:E-CAPACITY TTHROWSQ STILL-WORKS
   s" framed text round-trips with header and Adler-32" T-LABEL
   HELLO$ FRAMED-HELLO$ FRAMED-ROUND
   s" framed empty input round-trips" T-LABEL
   NOTHING$ EMPTY-FRAMED$ FRAMED-ROUND ;

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

: PLAIN$ ( -- str ) PLAIN @ PAYLOAD-BYTES >str ;
: RELEASE ( ^u8 n -- ) MEM:BYTES-ALLOC-LEN MEM:RELEASE-BYTES ;

\ The stream decodes to the payload at exactly its size, and not one byte
\ less; the decoded vector, which the caller deletes, is answered.
: LARGE-BACK ( str [ str ^VEC:vector<u8> n -- ] -- ^VEC:vector<u8> )
   {: packed:str codec :}
   FRESH {: back:^ :}
   packed back PAYLOAD-BYTES codec execute
   back VEC:COUNT PAYLOAD-BYTES T=
   back VEC:BYTES PLAIN$ STR:= TTRUE
   [: packed PAYLOAD-BYTES 1- codec INFLATE-CASE ;] ZLIB:E-CAPACITY TTHROWSQ
   back ;

: LARGE-RAW ( -- )
   s" a 10 MiB payload round-trips" T-LABEL
   FRESH {: packed:^ :}
   PLAIN$ packed ZLIB:DEFLATE-RAW
   packed VEC:BYTES [: ZLIB:INFLATE-RAW ;] LARGE-BACK {: back:^ :}
   back VEC:BYTES ZLIB:CRC32 PLAIN$ ZLIB:CRC32 T=
   s" original.bin" ARTIFACT$ PLAIN$ FS:WRITE-ALL
   s" compressed.bin" ARTIFACT$ packed VEC:BYTES FS:WRITE-ALL
   s" decoded.bin" ARTIFACT$ back VEC:BYTES FS:WRITE-ALL
   back VEC:DELETE packed VEC:DELETE ;

: LARGE-FRAMED ( -- )
   s" a 10 MiB payload round-trips framed" T-LABEL
   FRESH {: packed:^ :}
   PLAIN$ packed ZLIB:DEFLATE-FRAMED
   packed VEC:BYTES [: ZLIB:INFLATE-FRAMED ;] LARGE-BACK
   VEC:DELETE packed VEC:DELETE ;

: LARGE ( -- )
   PAYLOAD-BYTES MEM-ALLOC-BYTES drop PLAIN !
   PLAIN @ PAYLOAD-BYTES FILL
   LARGE-RAW LARGE-FRAMED
   PLAIN @ PAYLOAD-BYTES RELEASE ;

public
: MAIN ( -- )
   T-RESET
   s" zlib" FS:MKTEMP-DIR {: root:str :}
   root FS:CLEANUP-TREE+
   root STR:DATA ROOT root STR:LEN BYTE-COPY root STR:LEN ROOT-U !

   CRC-CASES LITERAL-CASES STREAM-FAILURES KEEPS SPAN-REJECTIONS
   SMALL-DEFLATE FRAMED-CASES LARGE
   T-REPORT
   s" zlib-source: ok" type cr ;
;package

ZLIB-SOURCE:MAIN
