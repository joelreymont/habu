\ zlib.f - raw DEFLATE (RFC 1951), zlib-framed DEFLATE (RFC 1950) and CRC-32
\ by libz. A codec word reads its source and appends to the caller's vector:
\ deflate the whole stream, inflate at most the count it is given. A failure
\ rethrows its code and leaves COUNT as it was, earlier bytes intact. The
\ source must not lie in the vector's block: growing it may move the block.
\ libz allocates from init to the matching *End, which every path reaches.
\ STORAGE CLASS. TASK-LOCAL: the z_stream and the overflow byte are TASK:+USER
\ rows, cleared after every call, so no address survives into a saved image.

require src/lib/error.f
require src/lib/le.f
require src/lib/ffi-abi.f
require src/lib/task.f
require src/lib/vector.f

package ZLIB
public

-9520 constant E-FIRST
-9529 constant E-LAST
-9520 s" an invalid span, a negative length, or a length or bound past 32 bits"
   ERROR:CODE E-SIZE
-9521 s" the expanded stream is longer than the most the caller allows"
   ERROR:CODE E-CAPACITY
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
FUNCTION: C-CRC32 crc32 ( n str -- n ) ;FUNCTION
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


\ libz counts lengths in 32 bits.
: SPAN-CHECK ( str -- ) {: s:str :}
   s STR:LEN 0 < if E-SIZE throw then
   s STR:LEN U32-MAX > if E-SIZE throw then
   s STR:LEN 0<> s STR:DATA NULL-PTR = and if E-SIZE throw then ;

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

: STAGE ( str str -- )
   {: src:str dst:str :}
   CLEAR
   src STR:DATA NEXT-IN ! src STR:LEN STREAM ZSTREAM-AVAIL-IN BYTE-VIEW LE:U32!
   dst STR:DATA NEXT-OUT ! dst STR:LEN AVAIL-OUT! ;

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

\ The room is full, or there was none, and the stream has not ended. One
\ more byte of room tells completion from an overrun without exposing
\ anything past the most the caller allows.
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

\ With Z_FINISH, all input supplied and libz's bound for room, deflate ends
\ in this call; a stall short of the end is a libz contract breach.
: DEFLATE-RUN ( -- n )
   STREAM BYTE-VIEW Z-FINISH C-DEFLATE {: status:n :}
   status Z-STREAM-END = if TOTAL-OUT@ exit then
   status STALLED? 0= if status FAIL then
   E-ABI throw ;

: BOUND-SESSION ( n n -- n )
   CLEAR DEFLATE-OPEN
   [: BOUND ;] catch {: rc:n :}
   STREAM BYTE-VIEW C-DEFLATE-END {: status:n :}
   rc 0<> if rc throw then
   status END-CHECK ;

: DEFLATE-SESSION ( n -- n )
   DEFLATE-OPEN
   [: DEFLATE-RUN ;] catch {: rc:n :}
   STREAM BYTE-VIEW C-DEFLATE-END {: status:n :}
   rc 0<> if rc throw then
   status END-CHECK ;

: INFLATE-SESSION ( n -- n )
   INFLATE-OPEN
   [: INFLATE-RUN ;] catch {: rc:n :}
   STREAM BYTE-VIEW C-INFLATE-END {: status:n :}
   rc 0<> if rc throw then
   status END-CHECK ;

\ libz counts the room in 32 bits too.
: BOUND-FOR ( n n -- n ) {: n:n window:n :}
   n window [: BOUND-SESSION ;] [: CLEAR ;] finally
   dup U32-MAX > if E-SIZE throw then ;

: DEFLATE-INTO ( str str n -- n )
   {: src:str dst:str window:n :}
   window src dst [: STAGE DEFLATE-SESSION ;] [: CLEAR ;] finally ;

: INFLATE-INTO ( str str n -- n )
   {: src:str dst:str window:n :}
   window src dst [: STAGE INFLATE-SESSION ;] [: CLEAR ;] finally ;

: DEFLATE-FOR ( str ^VEC:vector<u8> n -- )
   {: src:str v:^ window:n :}
   src SPAN-CHECK
   src STR:LEN window BOUND-FOR {: room:n :}
   v VEC:COUNT {: old:n :}
   [: src v room VEC:EXTEND SPAN:$ window DEFLATE-INTO ;] catch
   dup 0<> if nip v old VEC:RESIZE throw then
   drop old + v swap VEC:RESIZE ;

: INFLATE-FOR ( str ^VEC:vector<u8> n n -- )
   {: src:str v:^ most:n window:n :}
   src SPAN-CHECK
   most 0 < most U32-MAX > or if E-SIZE throw then
   v VEC:COUNT {: old:n :}
   [: src v most VEC:EXTEND SPAN:$ window INFLATE-INTO ;] catch
   dup 0<> if nip v old VEC:RESIZE throw then
   drop old + v swap VEC:RESIZE ;

public

\ The finalized IEEE CRC-32; zero for no bytes.
: CRC32 ( str -- u32 ) {: s:str :}
   s SPAN-CHECK
   0 s C-CRC32 U32-MAX and ;

\ Default compression, method 8, window bits -15, memory level 8, default
\ strategy.
: DEFLATE-RAW ( str ^VEC:vector<u8> -- ) RAW-WINDOW DEFLATE-FOR ;

\ Exactly one complete raw stream that uses the whole source, at most n
\ bytes expanded.
: INFLATE-RAW ( str ^VEC:vector<u8> n -- ) RAW-WINDOW INFLATE-FOR ;

\ DEFLATE-RAW's settings in the zlib wrapper, window bits +15: a 78 header,
\ no dictionary and a big-endian Adler-32 trailer.
: DEFLATE-FRAMED ( str ^VEC:vector<u8> -- ) FRAMED-WINDOW DEFLATE-FOR ;

\ Exactly one complete zlib stream that uses the whole source; libz checks
\ its method, window, header check and Adler-32, at most n bytes expanded. A
\ preset dictionary is refused.
: INFLATE-FRAMED ( str ^VEC:vector<u8> n -- ) FRAMED-WINDOW INFLATE-FOR ;

;package
