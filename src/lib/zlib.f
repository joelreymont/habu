split-str
\ zlib.f - raw DEFLATE (RFC 1951), zlib-framed DEFLATE (RFC 1950) and CRC-32
\ by libz. A codec word borrows both buffers, keeps nothing and answers the
\ bytes it wrote; libz allocates from init to the matching *End, which every
\ path reaches. A failure rethrows its code and may leave a destination prefix.
\ STORAGE CLASS. TASK-LOCAL: the z_stream and the overflow byte are TASK:+USER
\ rows, cleared after every call, so no address survives into a saved image.

require src/lib/error.f
require src/lib/le.f
require src/lib/ffi-abi.f
require src/lib/task.f

package ZLIB
public

-9520 constant E-FIRST
-9529 constant E-LAST
-9520 s" an invalid span, an overlap, a negative length, or a length or bound past 32 bits"
   ERROR:CODE E-SIZE
-9521 s" the destination cannot hold the result: pass a larger destination" ERROR:CODE E-CAPACITY
-9522 s" invalid, truncated or trailing compressed data" ERROR:CODE E-DATA
-9523 s" libz reports Z_MEM_ERROR" ERROR:FAULT E-MEMORY
-9524 s" a libz version, layout or state error, or an unexpected libz status" ERROR:FAULT E-ABI

private

$FFFFFFFF constant U32-MAX

\ z_stream on the supported LP64 hosts (Darwin and Linux): pointers and uLong
\ are eight bytes, uInt and int four; avail-in, avail-out and data-type are
\ each a four-byte C field at the start of its cell, the rest padding.
STRUCTURE zstream DERIVE addr
   FIELD next-in ^u8
   FIELD avail-in n
   FIELD total-in n
   FIELD next-out ^u8
   FIELD avail-out n
   FIELD total-out n
   FIELD msg ^u8
   FIELD state ^u8
   FIELD zalloc ^u8
   FIELD zfree ^u8
   FIELD opaque ^u8
   FIELD data-type n
   FIELD adler n
   FIELD reserved n
;STRUCTURE

0 constant Z-OK
1 constant Z-STREAM-END
2 constant Z-NEED-DICT
-3 constant Z-DATA-ERROR
-4 constant Z-MEM-ERROR
-5 constant Z-BUF-ERROR
4 constant Z-FINISH
-1 constant Z-DEFAULT-COMPRESSION
8 constant Z-DEFLATED
-15 constant RAW-WINDOW     \ negative window bits select raw framing
15 constant FRAMED-WINDOW   \ positive window bits select the RFC 1950 header and Adler-32 trailer
8 constant MEM-LEVEL
0 constant Z-DEFAULT-STRATEGY

TASK:+USER STREAM zstream
TASK:+USER PROBE u8
: NEXT-IN ( -- ^^u8 ) STREAM ZSTREAM-NEXT-IN ;
: NEXT-OUT ( -- ^^u8 ) STREAM ZSTREAM-NEXT-OUT ;
: AVAIL-IN@ ( -- n ) STREAM ZSTREAM-AVAIL-IN BYTE-VIEW LE:U32@ ;
: AVAIL-OUT@ ( -- n ) STREAM ZSTREAM-AVAIL-OUT BYTE-VIEW LE:U32@ ;
: AVAIL-OUT! ( n -- ) STREAM ZSTREAM-AVAIL-OUT BYTE-VIEW LE:U32! ;
: TOTAL-OUT@ ( -- n ) STREAM ZSTREAM-TOTAL-OUT @ ;

\ The only asserted effects are exact C entry points; Habu owns every check.
VERSIONED-LIBRARY z 1
FUNCTION: C-VERSION zlibVersion ( -- ^u8 ) ;FUNCTION
FUNCTION: C-CRC32 crc32 ( n ^u8 n -- n ) ;FUNCTION
FUNCTION: C-INFLATE-INIT inflateInit2_ ( ^u8 n ^u8 n -- i32 ) ;FUNCTION
FUNCTION: C-INFLATE inflate ( ^u8 n -- i32 ) ;FUNCTION
FUNCTION: C-INFLATE-END inflateEnd ( ^u8 -- i32 ) ;FUNCTION
FUNCTION: C-DEFLATE-INIT deflateInit2_ ( ^u8 n n n n n ^u8 n -- i32 ) ;FUNCTION
FUNCTION: C-DEFLATE deflate ( ^u8 n -- i32 ) ;FUNCTION
FUNCTION: C-DEFLATE-END deflateEnd ( ^u8 -- i32 ) ;FUNCTION
FUNCTION: C-BOUND deflateBound ( ^u8 n -- n ) ;FUNCTION

\ Null allocator callbacks select libz's own malloc and free.
: CLEAR ( -- )
   [type] zstream width>n 0 ?do 0 STREAM BYTE-VIEW i + c! loop
   0 PROBE c! ;

: ADDRESS ( ^u8 -- n ) NULL-PTR - ;

\ libz counts lengths in 32 bits.
: SPAN-CHECK ( str -- ) {: p:^ u:n :}
   u 0 < if E-SIZE throw then
   u U32-MAX > if E-SIZE throw then
   u 0<> p NULL-PTR = and if E-SIZE throw then ;

: DISJOINT-CHECK ( str str -- )
   {: a:^ au:n b:^ bu:n :}
   au 0= bu 0= or if exit then
   a ADDRESS b ADDRESS bu + < b ADDRESS a ADDRESS au + < and if E-SIZE throw then ;

: PAIR-CHECK ( str str -- )
   {: src:^ srcu:n dst:^ cap:n :}
   src srcu SPAN-CHECK dst cap SPAN-CHECK
   src srcu dst cap DISJOINT-CHECK ;

\ A framed stream that asks for a preset dictionary is data no caller can
\ supply one for.
: FAIL ( n -- )
   dup Z-DATA-ERROR = if drop E-DATA throw then
   dup Z-NEED-DICT = if drop E-DATA throw then
   Z-MEM-ERROR = if E-MEMORY throw then
   E-ABI throw ;

\ A failed init owns nothing, so no *End follows it.
: INIT-CHECK ( n -- )
   dup Z-OK = if drop exit then
   FAIL ;

: STAGE ( ^u8 n ^u8 n -- )
   {: src:^ srcu:n dst:^ cap:n :}
   CLEAR
   src NEXT-IN ! srcu STREAM ZSTREAM-AVAIL-IN BYTE-VIEW LE:U32!
   dst NEXT-OUT ! cap AVAIL-OUT! ;

\ The *End status of an operation that succeeded. After a failure the caught
\ code is the report: abandoning an unfinished deflater ends with Z_DATA_ERROR.
: END-CHECK ( n -- ) Z-OK <> if E-ABI throw then ;

: DEFLATE-OPEN ( n -- ) {: window:n :}
   STREAM BYTE-VIEW Z-DEFAULT-COMPRESSION Z-DEFLATED window MEM-LEVEL
   Z-DEFAULT-STRATEGY C-VERSION [type] zstream width>n C-DEFLATE-INIT
   INIT-CHECK ;

: INFLATE-OPEN ( n -- ) {: window:n :}
   STREAM BYTE-VIEW window C-VERSION [type] zstream width>n C-INFLATE-INIT
   INIT-CHECK ;

: BOUND ( n -- n ) STREAM BYTE-VIEW swap C-BOUND ;

: STALLED? ( n -- bool ) dup Z-OK = swap Z-BUF-ERROR = or ;

: ENDED ( -- n )
   AVAIL-IN@ 0<> if E-DATA throw then
   TOTAL-OUT@ ;

\ The destination is full, or had no capacity, and the stream has not ended.
\ One more byte of room tells completion from an understated size without
\ exposing anything past the caller's capacity.
: PROBE-STEP ( -- n )
   PROBE NEXT-OUT ! 1 AVAIL-OUT!
   STREAM BYTE-VIEW Z-FINISH C-INFLATE {: status:n :}
   status Z-STREAM-END = if
      AVAIL-OUT@ 0= if E-CAPACITY throw then
      ENDED exit
   then
   status STALLED? 0= if status FAIL then
   AVAIL-OUT@ 0= if E-CAPACITY throw then
   E-DATA throw ;

\ Z_FINISH lets a stream that completes in this call skip libz's window. A
\ stall with room left means the input ran out first: truncated.
: INFLATE-RUN ( -- n )
   AVAIL-OUT@ 0= if PROBE-STEP exit then
   STREAM BYTE-VIEW Z-FINISH C-INFLATE {: status:n :}
   status Z-STREAM-END = if ENDED exit then
   status STALLED? 0= if status FAIL then
   AVAIL-OUT@ 0<> if E-DATA throw then
   PROBE-STEP ;

\ Deflate may withhold the end when the last byte exactly fills the
\ destination. One more byte of room tells an exactly sized destination from a
\ short one without exposing anything past the caller's capacity.
: DEFLATE-PROBE ( -- n )
   PROBE NEXT-OUT ! 1 AVAIL-OUT!
   STREAM BYTE-VIEW Z-FINISH C-DEFLATE {: status:n :}
   status Z-STREAM-END = status STALLED? or 0= if status FAIL then
   AVAIL-OUT@ 0= if E-CAPACITY throw then
   status Z-STREAM-END <> if E-ABI throw then
   TOTAL-OUT@ ;

\ With Z_FINISH and all input supplied, a stop short of the end with room left
\ is a libz contract breach.
: DEFLATE-RUN ( -- n )
   STREAM BYTE-VIEW Z-FINISH C-DEFLATE {: status:n :}
   status Z-STREAM-END = if TOTAL-OUT@ exit then
   status STALLED? 0= if status FAIL then
   AVAIL-OUT@ 0<> if E-ABI throw then
   DEFLATE-PROBE ;

: BOUND-SESSION ( n n -- n )
   CLEAR DEFLATE-OPEN
   [: BOUND ;] catch {: rc:n :}
   STREAM BYTE-VIEW C-DEFLATE-END {: status:n :}
   rc 0<> if rc throw then
   status END-CHECK ;

: DEFLATE-SESSION ( n -- n )
   DEFLATE-OPEN
   0 [: drop DEFLATE-RUN ;] catch {: rc:n :}
   STREAM BYTE-VIEW C-DEFLATE-END {: status:n :}
   rc 0<> if rc throw then
   status END-CHECK ;

: INFLATE-SESSION ( n -- n )
   INFLATE-OPEN
   0 [: drop INFLATE-RUN ;] catch {: rc:n :}
   STREAM BYTE-VIEW C-INFLATE-END {: status:n :}
   rc 0<> if rc throw then
   status END-CHECK ;

: BOUND-FOR ( n n -- n ) {: n:n window:n :}
   n 0 < if E-SIZE throw then
   n U32-MAX > if E-SIZE throw then
   n window [: BOUND-SESSION ;] [: CLEAR ;] finally
   dup U32-MAX > if E-SIZE throw then ;

\ Every stream is at least one byte, so zero capacity cannot hold one.
: DEFLATE-FOR ( ^u8 n ^u8 n n -- n )
   {: src:^ srcu:n dst:^ cap:n window:n :}
   src srcu dst cap PAIR-CHECK
   cap 0= if E-CAPACITY throw then
   window src srcu dst cap [: STAGE DEFLATE-SESSION ;] [: CLEAR ;] finally ;

: INFLATE-FOR ( ^u8 n ^u8 n n -- n )
   {: src:^ srcu:n dst:^ cap:n window:n :}
   src srcu dst cap PAIR-CHECK
   window src srcu dst cap [: STAGE INFLATE-SESSION ;] [: CLEAR ;] finally ;

public

\ The finalized IEEE CRC-32; zero for no bytes.
: CRC32 ( ^u8 n -- u32 ) {: p:^ u:n :}
   p u SPAN-CHECK
   0 p u C-CRC32 U32-MAX and ;

\ A capacity DEFLATE-RAW always completes in, for n source bytes.
: RAW-BOUND ( n -- n ) RAW-WINDOW BOUND-FOR ;

\ Default compression, method 8, window bits -15, memory level 8, default
\ strategy.
: DEFLATE-RAW ( str str -- n ) RAW-WINDOW DEFLATE-FOR ;

\ Exactly one complete raw stream that uses the whole source.
: INFLATE-RAW ( str str -- n ) RAW-WINDOW INFLATE-FOR ;

\ A capacity DEFLATE-FRAMED always completes in, for n source bytes.
: FRAMED-BOUND ( n -- n ) FRAMED-WINDOW BOUND-FOR ;

\ DEFLATE-RAW's settings in the zlib wrapper, window bits +15: a 78 header,
\ no dictionary and a big-endian Adler-32 trailer.
: DEFLATE-FRAMED ( str str -- n ) FRAMED-WINDOW DEFLATE-FOR ;

\ Exactly one complete zlib stream that uses the whole source; libz checks
\ its method, window, header check and Adler-32. A preset dictionary is
\ refused.
: INFLATE-FRAMED ( str str -- n ) FRAMED-WINDOW INFLATE-FOR ;

;package
