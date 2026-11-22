split-str
\ event-stream-test.f - checked native SSE framing cases.
\ Run: bin/hb --load src/lib/net/event-stream-test.f

require src/lib/test.f
require src/lib/heap.f
require src/lib/span.f
require src/lib/vector.f
require src/lib/net/event-stream.f

package SSE-TEST

TYPED-VARIABLE DEC-A SSE:state
TYPED-VARIABLE DEC-B SSE:state
TYPED-VARIABLE GOT ^VEC:vector<u8>
TYPED-VARIABLE WANT ^VEC:vector<u8>
TYPED-VARIABLE BIG ^VEC:vector<u8>
TYPED-VARIABLE ACTIVE SSE:decoder
variable CALLS

: APPEND ( str ^^VEC:vector<u8> -- ) @ VEC:APPEND ;

: FRESH ( ^^VEC:vector<u8> -- )
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW swap ! ;

: RECORD ( ^a str str -- )
   {: ctx:^ kind:^ ku:n data:^ du:n :}
   ctx drop
   kind ku GOT APPEND 0 GOT @ VEC:PUSH c!
   data du GOT APPEND 0 GOT @ VEC:PUSH c!
   1 CALLS +! ;

: EXPECT ( str str -- )
   {: kind:^ ku:n data:^ du:n :}
   kind ku WANT APPEND 0 WANT @ VEC:PUSH c!
   data du WANT APPEND 0 WANT @ VEC:PUSH c! ;

: RESET ( -- )
   GOT @ 0 VEC:RESIZE WANT @ 0 VEC:RESIZE 0 CALLS ! ;

: CHECK ( n -- )
   CALLS @ T=
   GOT @ VEC:SPAN SPAN:$ WANT @ VEC:SPAN SPAN:$ T$= ;

: A-OPEN ( -- SSE:decoder ) DEC-A SSE:OPEN ;
: B-OPEN ( -- SSE:decoder ) DEC-B SSE:OPEN ;

: SEND ( SSE:decoder str -- )
   NULL-PTR [: RECORD ;] SSE:FEED ;

: BASIC ( -- )
   RESET
   s" custom" s\" A\né" EXPECT
   s" message" s" " EXPECT
   s" message" s" z" EXPECT
   A-OPEN {: d:SSE:decoder :}
   d s\" \xEF\xBB\xBF:ignored\r\nevent: custom\r\ndata: A\rdata: é\n\r\ndata:\n\nid: ignored\n\nevent:\ndata: z\n\n" SEND
   d SSE:FINISH d SSE:CLOSE
   3 CHECK ;

: SPLIT ( n -- )
   {: cut:n :}
   RESET
   s" custom" s\" A\né" EXPECT
   s" message" s" " EXPECT
   s" message" s" z" EXPECT
   A-OPEN {: d:SSE:decoder :}
   s\" \xEF\xBB\xBF:ignored\r\nevent: custom\r\ndata: A\rdata: é\n\r\ndata:\n\nid: ignored\n\nevent:\ndata: z\n\n"
   {: src:^ u:n :}
   d src cut SEND
   d src cut + u cut - SEND
   d SSE:FINISH d SSE:CLOSE
   3 CHECK ;

: EVERY-SPLIT ( -- )
   s\" \xEF\xBB\xBF:ignored\r\nevent: custom\r\ndata: A\rdata: é\n\r\ndata:\n\nid: ignored\n\nevent:\ndata: z\n\n" nip
   1+ 0 ?do i SPLIT loop ;

: TWO-DECODERS ( -- )
   RESET
   s" one" s" A" EXPECT
   s" message" s" B" EXPECT
   A-OPEN {: a:SSE:decoder :}
   B-OPEN {: b:SSE:decoder :}
   a s\" event: one\ndata:" SEND
   b s\" data: B\n" SEND
   a s\"  A\n\n" SEND
   b s\" \n" SEND
   a SSE:CLOSE b SSE:CLOSE
   2 CHECK ;

: EOF-DISCARD ( -- )
   RESET
   A-OPEN {: d:SSE:decoder :}
   d s\" data: lost\n" SEND
   d SSE:FINISH d SSE:CLOSE
   0 CHECK
   A-OPEN {: e:SSE:decoder :}
   e s\" data: lost\r" SEND
   e SSE:FINISH e SSE:CLOSE
   0 CHECK ;

: FIELDS ( -- )
   RESET
   s" later" s"   value" EXPECT
   s" message" s\" a\x00b" EXPECT
   A-OPEN {: d:SSE:decoder :}
   d s\" event: ghost\n\nid: ignored\nretry: 100\n\n" SEND
   d s\" event: first\nevent: later\ndata:   value\n\n" SEND
   d s\" data:a\x00b\n\n" SEND
   d SSE:CLOSE
   2 CHECK ;

: NO-COLON ( -- )
   RESET
   s" message" s" " EXPECT
   A-OPEN {: d:SSE:decoder :}
   d s\" data\n\n" SEND
   d SSE:CLOSE
   1 CHECK ;

: BOM-CUT ( str n -- )
   {: src:^ u:n cut:n :}
   A-OPEN {: d:SSE:decoder :}
   d src cut SEND
   d src cut + u cut - SEND
   d SSE:FINISH d SSE:CLOSE ;

\ A leading BOM split anywhere is dropped; a leading EF or EF BB that does not
\ finish a BOM is replayed into the line, so its field name is unknown.
: REAL-BOM ( n -- )
   RESET s" message" s" x" EXPECT
   s\" \xEF\xBB\xBFdata: x\n\n" rot BOM-CUT 1 CHECK ;

: EF-ONLY ( n -- )
   RESET s\" \xEFdata: x\n\n" rot BOM-CUT 0 CHECK ;

: EF-BB ( n -- )
   RESET s\" \xEF\xBBdata: x\n\n" rot BOM-CUT 0 CHECK ;

: BOM-SPLITS ( -- )
   s\" \xEF\xBB\xBFdata: x\n\n" nip 1+ 0 ?do i REAL-BOM loop
   s\" \xEFdata: x\n\n" nip 1+ 0 ?do i EF-ONLY loop
   s\" \xEF\xBBdata: x\n\n" nip 1+ 0 ?do i EF-BB loop ;

: MIDSTREAM-BOM ( -- )
   RESET
   s" message" s\" \xEF\xBB\xBFx" EXPECT
   A-OPEN {: d:SSE:decoder :}
   d s\" data:\xEF\xBB\xBFx\n\n" SEND
   d SSE:CLOSE
   1 CHECK ;

: LARGE-EVENT ( -- )
   RESET BIG @ 0 VEC:RESIZE
   s" data:" BIG APPEND
   70000 0 ?do [char] x BIG @ VEC:PUSH c! loop
   s\" \n\n" BIG APPEND
   A-OPEN {: d:SSE:decoder :}
   BIG @ VEC:SPAN SPAN:$ d -rot SEND
   d SSE:CLOSE
   1 CALLS @ T=
   GOT @ VEC:COUNT 70009 T= ;

: INVALID-UTF8 ( -- )
   RESET
   A-OPEN ACTIVE !
   [: ACTIVE @ s\" data:\xFF\n\n" SEND ;] SSE:E-UTF8 TTHROWSQ
   ACTIVE @ SSE:CLOSE
   0 CHECK ;

: BOOM ( ^a str str -- )
   2drop 2drop drop -12345 throw ;

: CALLBACK-FAILURE ( -- )
   RESET
   A-OPEN ACTIVE !
   [: ACTIVE @ s\" data: x\n\n" NULL-PTR [: BOOM ;] SSE:FEED ;]
      -12345 TTHROWSQ
   [: ACTIVE @ s\" data: x\n\n" SEND ;] SSE:E-STATE TTHROWSQ
   ACTIVE @ SSE:CLOSE
   0 CHECK ;

: MISUSE ( -- )
   A-OPEN ACTIVE !
   [: A-OPEN drop ;] SSE:E-STATE TTHROWSQ
   ACTIVE @ SSE:FINISH
   [: ACTIVE @ s\" data: x\n\n" SEND ;] SSE:E-STATE TTHROWSQ
   ACTIVE @ SSE:CLOSE
   [: ACTIVE @ s\" data: x\n\n" SEND ;] SSE:E-STATE TTHROWSQ
   [: ACTIVE @ SSE:CLOSE ;] SSE:E-STATE TTHROWSQ
   A-OPEN SSE:CLOSE ;

: BAD-SOURCE ( -- )
   A-OPEN ACTIVE !
   [: ACTIVE @ NULL-PTR -1 SEND ;] SSE:E-SOURCE TTHROWSQ
   [: ACTIVE @ NULL-PTR 1 SEND ;] SSE:E-SOURCE TTHROWSQ
   ACTIVE @ SSE:CLOSE ;

: RUN ( -- )
   T-RESET
   GOT FRESH WANT FRESH BIG FRESH
   BASIC EVERY-SPLIT TWO-DECODERS EOF-DISCARD FIELDS NO-COLON
   BOM-SPLITS MIDSTREAM-BOM LARGE-EVENT INVALID-UTF8 CALLBACK-FAILURE
   MISUSE BAD-SOURCE
   BIG @ VEC:DELETE WANT @ VEC:DELETE GOT @ VEC:DELETE
   T-REPORT
   s" event-stream-test: ok" type cr ;

RUN
;package
