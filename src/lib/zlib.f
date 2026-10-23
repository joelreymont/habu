\ zlib.f - raw DEFLATE (RFC 1951), zlib-framed DEFLATE (RFC 1950) and CRC-32
\ over caller-owned buffers, by libz.
\
\ Each codec word takes source, source bytes, destination and destination
\ capacity, and answers the bytes it wrote. It borrows both buffers and keeps
\ nothing: libz's own allocations live from its init call to the matching
\ *End, which every path reaches. A failed call may leave a destination prefix
\ written and answers no length, and rethrows its original code.
\
\ STORAGE CLASS. TASK-LOCAL scratch: one z_stream and an overflow byte per task,
\ cleared after every call, so no address survives into a saved image.

require src/lib/errors.f
require src/lib/le.f
require src/lib/ffi-abi.f
require src/lib/task.f

package ZLIB
public

-9520 constant E-FIRST
-9529 constant E-LAST
-9520 constant E-SIZE       \ invalid span, overlap, negative length, or a length or bound past 32 bits
-9521 constant E-CAPACITY   \ the destination cannot hold the result
-9522 constant E-DATA       \ invalid, truncated or trailing compressed data
-9523 constant E-MEMORY     \ libz reports Z_MEM_ERROR
-9524 constant E-ABI        \ a version, layout or state error, or an unexpected libz status

private

$FFFFFFFF constant U32-MAX

\ z_stream on the supported LP64 hosts (Darwin and Linux): pointers and uLong
\ are eight bytes, uInt four, with the padding that gives these offsets.
112 constant STREAM-BYTES
8 constant AVAIL-IN-OFF
32 constant AVAIL-OUT-OFF
40 constant TOTAL-OUT-OFF

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

TASK:#USER 7 + $FFFFFFFFFFFFFFF8 and STREAM-BYTES 8 + TASK:+USER SCRATCH drop
: STREAM ( -- ptr u8 ) SCRATCH BYTE-VIEW ;
: PROBE ( -- ptr u8 ) STREAM STREAM-BYTES + ;
: NEXT-IN ( -- ptr ptr u8 ) SCRATCH 0 ptr-field ;
: NEXT-OUT ( -- ptr ptr u8 ) SCRATCH 3 ptr-field ;
: AVAIL-IN@ ( -- n ) STREAM AVAIL-IN-OFF + LE:U32@ ;
: AVAIL-OUT@ ( -- n ) STREAM AVAIL-OUT-OFF + LE:U32@ ;
: AVAIL-OUT! ( n -- ) STREAM AVAIL-OUT-OFF + LE:U32! ;
: TOTAL-OUT@ ( -- n ) STREAM TOTAL-OUT-OFF + LE:U64@ ;

\ The only asserted effects are exact C entry points; Habu owns every check.
VERSIONED-LIBRARY z 1
FUNCTION: C-VERSION zlibVersion ( -- ptr u8 ) ;FUNCTION
FUNCTION: C-CRC32 crc32 ( n ptr u8 n -- n ) ;FUNCTION
FUNCTION: C-INFLATE-INIT inflateInit2_ ( ptr u8 n ptr u8 n -- i32 )
   0 STREAM-BYTES WRITES-BYTES
;FUNCTION
FUNCTION: C-INFLATE inflate ( ptr u8 n -- i32 )
   0 STREAM-BYTES WRITES-BYTES
;FUNCTION
FUNCTION: C-INFLATE-END inflateEnd ( ptr u8 -- i32 )
   0 STREAM-BYTES WRITES-BYTES
;FUNCTION
FUNCTION: C-DEFLATE-INIT deflateInit2_ ( ptr u8 n n n n n ptr u8 n -- i32 )
   0 STREAM-BYTES WRITES-BYTES
;FUNCTION
FUNCTION: C-DEFLATE deflate ( ptr u8 n -- i32 )
   0 STREAM-BYTES WRITES-BYTES
;FUNCTION
FUNCTION: C-DEFLATE-END deflateEnd ( ptr u8 -- i32 )
   0 STREAM-BYTES WRITES-BYTES
;FUNCTION
FUNCTION: C-BOUND deflateBound ( ptr u8 n -- n ) ;FUNCTION

\ Null allocator callbacks select libz's own malloc and free.
: CLEAR ( -- )
   STREAM-BYTES 1+ 0 ?do 0 STREAM i + c! loop ;

: ADDRESS ( ptr u8 -- n ) NULL-PTR - ;

\ A length libz's 32-bit counters hold, and an address for every nonempty span.
: SPAN-CHECK ( ptr u8 n -- ) {: p:ptr u:n :}
   u 0 < if E-SIZE throw then
   u U32-MAX > if E-SIZE throw then
   u 0<> p ADDRESS 0= and if E-SIZE throw then ;

: DISJOINT-CHECK ( ptr u8 n ptr u8 n -- )
   {: a:ptr au:n b:ptr bu:n :}
   au 0= bu 0= or if exit then
   a ADDRESS b ADDRESS bu + < b ADDRESS a ADDRESS au + < and if E-SIZE throw then ;

: PAIR-CHECK ( ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n dst:ptr cap:n :}
   src srcu SPAN-CHECK dst cap SPAN-CHECK
   src srcu dst cap DISJOINT-CHECK ;

\ A status no successful path accepts. A framed stream that asks for a preset
\ dictionary is data no caller can supply one for.
: FAIL ( n -- )
   dup Z-DATA-ERROR = if drop E-DATA throw then
   dup Z-NEED-DICT = if drop E-DATA throw then
   Z-MEM-ERROR = if E-MEMORY throw then
   E-ABI throw ;

\ A failed init owns nothing, so no *End follows it.
: INIT-CHECK ( n -- )
   dup Z-OK = if drop exit then
   FAIL ;

: STAGE ( ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n dst:ptr cap:n :}
   CLEAR
   src NEXT-IN ! srcu STREAM AVAIL-IN-OFF + LE:U32!
   dst NEXT-OUT ! cap AVAIL-OUT! ;

\ The *End status of an operation that succeeded. After a failure the caught
\ code is the report: abandoning an unfinished deflater ends with Z_DATA_ERROR.
: END-CHECK ( n -- ) Z-OK <> if E-ABI throw then ;

: DEFLATE-OPEN ( n -- ) {: window:n :}
   STREAM Z-DEFAULT-COMPRESSION Z-DEFLATED window MEM-LEVEL
   Z-DEFAULT-STRATEGY C-VERSION STREAM-BYTES C-DEFLATE-INIT INIT-CHECK ;

: INFLATE-OPEN ( n -- ) {: window:n :}
   STREAM window C-VERSION STREAM-BYTES C-INFLATE-INIT INIT-CHECK ;

: BOUND ( n -- n ) STREAM swap C-BOUND ;

\ Output stopped before the stream ended: Z_OK or Z_BUF_ERROR, no progress.
: STALLED? ( n -- bool ) dup Z-OK = swap Z-BUF-ERROR = or ;

\ The stream ended; any input left after it is trailing data.
: ENDED ( -- n )
   AVAIL-IN@ 0<> if E-DATA throw then
   TOTAL-OUT@ ;

\ The destination is full, or had no capacity, and the stream has not ended.
\ One more byte of room tells completion from an understated size without
\ exposing anything past the caller's capacity.
: PROBE-STEP ( -- n )
   PROBE NEXT-OUT ! 1 AVAIL-OUT!
   STREAM Z-FINISH C-INFLATE {: status:n :}
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
   STREAM Z-FINISH C-INFLATE {: status:n :}
   status Z-STREAM-END = if ENDED exit then
   status STALLED? 0= if status FAIL then
   AVAIL-OUT@ 0<> if E-DATA throw then
   PROBE-STEP ;

\ The destination is full and deflate has not reported the end, which it may
\ withhold when the last byte exactly fills it. One more byte of room tells an
\ exactly sized destination from a short one without exposing anything past
\ the caller's capacity.
: DEFLATE-PROBE ( -- n )
   PROBE NEXT-OUT ! 1 AVAIL-OUT!
   STREAM Z-FINISH C-DEFLATE {: status:n :}
   status Z-STREAM-END = status STALLED? or 0= if status FAIL then
   AVAIL-OUT@ 0= if E-CAPACITY throw then
   status Z-STREAM-END <> if E-ABI throw then
   TOTAL-OUT@ ;

\ With Z_FINISH and all input supplied, a stop short of the end with room left
\ is a libz contract breach.
: DEFLATE-RUN ( -- n )
   STREAM Z-FINISH C-DEFLATE {: status:n :}
   status Z-STREAM-END = if TOTAL-OUT@ exit then
   status STALLED? 0= if status FAIL then
   AVAIL-OUT@ 0<> if E-ABI throw then
   DEFLATE-PROBE ;

\ Each session runs on a staged stream: init, the work under catch, End once
\ init has succeeded, then the work's code before End's status. The public
\ word clears the scratch after it on every exit. Each takes the window bits
\ that select the framing.
: BOUND-SESSION ( n n -- n )
   CLEAR DEFLATE-OPEN
   [: BOUND ;] catch {: rc:n :}
   STREAM C-DEFLATE-END {: status:n :}
   rc 0<> if rc throw then
   status END-CHECK ;

: DEFLATE-SESSION ( n -- n )
   DEFLATE-OPEN
   0 [: drop DEFLATE-RUN ;] catch {: rc:n :}
   STREAM C-DEFLATE-END {: status:n :}
   rc 0<> if rc throw then
   status END-CHECK ;

: INFLATE-SESSION ( n -- n )
   INFLATE-OPEN
   0 [: drop INFLATE-RUN ;] catch {: rc:n :}
   STREAM C-INFLATE-END {: status:n :}
   rc 0<> if rc throw then
   status END-CHECK ;

\ The checks and sessions every public codec word shares, for window bits.
: BOUND-FOR ( n n -- n ) {: n:n window:n :}
   n 0 < if E-SIZE throw then
   n U32-MAX > if E-SIZE throw then
   n window [: BOUND-SESSION ;] [: CLEAR ;] finally
   dup U32-MAX > if E-SIZE throw then ;

\ Every stream is at least one byte, so zero capacity cannot hold one.
: DEFLATE-FOR ( ptr u8 n ptr u8 n n -- n )
   {: src:ptr srcu:n dst:ptr cap:n window:n :}
   src srcu dst cap PAIR-CHECK
   cap 0= if E-CAPACITY throw then
   window src srcu dst cap [: STAGE DEFLATE-SESSION ;] [: CLEAR ;] finally ;

: INFLATE-FOR ( ptr u8 n ptr u8 n n -- n )
   {: src:ptr srcu:n dst:ptr cap:n window:n :}
   src srcu dst cap PAIR-CHECK
   window src srcu dst cap [: STAGE INFLATE-SESSION ;] [: CLEAR ;] finally ;

public

\ The finalized IEEE CRC-32; zero for no bytes.
: CRC32 ( ptr u8 n -- u32 ) {: p:ptr u:n :}
   p u SPAN-CHECK
   0 p u C-CRC32 U32-MAX and ;

\ A capacity DEFLATE-RAW always completes in, for n source bytes.
: RAW-BOUND ( n -- n ) RAW-WINDOW BOUND-FOR ;

\ Default compression, method 8, window bits -15, memory level 8, default
\ strategy.
: DEFLATE-RAW ( ptr u8 n ptr u8 n -- n ) RAW-WINDOW DEFLATE-FOR ;

\ Exactly one complete raw stream that uses the whole source.
: INFLATE-RAW ( ptr u8 n ptr u8 n -- n ) RAW-WINDOW INFLATE-FOR ;

\ A capacity DEFLATE-FRAMED always completes in, for n source bytes.
: FRAMED-BOUND ( n -- n ) FRAMED-WINDOW BOUND-FOR ;

\ DEFLATE-RAW's settings in the zlib wrapper, window bits +15: a 78 header,
\ no dictionary and a big-endian Adler-32 trailer.
: DEFLATE-FRAMED ( ptr u8 n ptr u8 n -- n ) FRAMED-WINDOW DEFLATE-FOR ;

\ Exactly one complete zlib stream that uses the whole source; libz checks
\ its method, window, header check and Adler-32. A preset dictionary is
\ refused.
: INFLATE-FRAMED ( ptr u8 n ptr u8 n -- n ) FRAMED-WINDOW INFLATE-FOR ;

;package
