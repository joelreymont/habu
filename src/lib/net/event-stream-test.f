\ event-stream-test.f - checked native SSE framing cases.
\ Run: bin/hb --load src/lib/net/event-stream-test.f

require src/lib/test.f
require src/lib/byte-buffer.f
require src/lib/net/event-stream.f

package SSE-TEST

SSE:STORAGE-BYTES constant DEC-BYTES
create DEC-A DEC-BYTES allot
create DEC-B DEC-BYTES allot
create GOT BUF:HDR-BYTES allot
create WANT BUF:HDR-BYTES allot
create BIG BUF:HDR-BYTES allot
TYPED-VARIABLE ACTIVE SSE:decoder
variable CALLS

: APPEND ( ptr u8 n ptr a -- )
   {: src:ptr u:n buf:ptr :}
   src u BUF:N>BLEN buf BUF:APPEND-SPAN ;

: RECORD ( ptr a ptr u8 n ptr u8 n -- )
   {: ctx:ptr kind:ptr ku:n data:ptr du:n :}
   ctx drop
   kind ku GOT APPEND 0 GOT BUF:APPEND-BYTE
   data du GOT APPEND 0 GOT BUF:APPEND-BYTE
   1 CALLS +! ;

: EXPECT ( ptr u8 n ptr u8 n -- )
   {: kind:ptr ku:n data:ptr du:n :}
   kind ku WANT APPEND 0 WANT BUF:APPEND-BYTE
   data du WANT APPEND 0 WANT BUF:APPEND-BYTE ;

: RESET ( -- )
   GOT BUF:CLEAR WANT BUF:CLEAR 0 CALLS ! ;

: CHECK ( n -- )
   CALLS @ T=
   GOT BUF:SPAN$ BUF:BLEN>N WANT BUF:SPAN$ BUF:BLEN>N T$= ;

: A-OPEN ( -- SSE:decoder ) DEC-A DEC-BYTES SSE:OPEN ;
: B-OPEN ( -- SSE:decoder ) DEC-B DEC-BYTES SSE:OPEN ;

: SEND ( SSE:decoder ptr u8 n -- )
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
   {: src:ptr u:n :}
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

: MIDSTREAM-BOM ( -- )
   RESET
   s" message" s\" \xEF\xBB\xBFx" EXPECT
   A-OPEN {: d:SSE:decoder :}
   d s\" data:\xEF\xBB\xBFx\n\n" SEND
   d SSE:CLOSE
   1 CHECK ;

: LARGE-EVENT ( -- )
   RESET BIG BUF:CLEAR
   s" data:" BIG APPEND
   70000 0 ?do [char] x BIG BUF:APPEND-BYTE loop
   s\" \n\n" BIG APPEND
   A-OPEN {: d:SSE:decoder :}
   BIG BUF:SPAN$ BUF:BLEN>N d -rot SEND
   d SSE:CLOSE
   1 CALLS @ T=
   GOT BUF:LEN@ BUF:BLEN>N 70009 T= ;

: INVALID-UTF8 ( -- )
   RESET
   A-OPEN ACTIVE !
   [: ACTIVE @ s\" data:\xFF\n\n" SEND ;] SSE:E-UTF8 TTHROWSQ
   ACTIVE @ SSE:CLOSE
   0 CHECK ;

: BOOM ( ptr a ptr u8 n ptr u8 n -- )
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
   ACTIVE @ SSE:CLOSE
   [: DEC-A 1 SSE:OPEN drop ;] SSE:E-STORAGE TTHROWSQ ;

: RUN ( -- )
   GOT 16 BUF:N>BLEN BUF:INIT
   WANT 16 BUF:N>BLEN BUF:INIT
   BIG 16 BUF:N>BLEN BUF:INIT
   BASIC EVERY-SPLIT TWO-DECODERS EOF-DISCARD FIELDS NO-COLON
   MIDSTREAM-BOM LARGE-EVENT INVALID-UTF8 CALLBACK-FAILURE
   MISUSE BAD-SOURCE
   BIG BUF:DISPOSE WANT BUF:DISPOSE GOT BUF:DISPOSE
   s" event-stream-test: ok" type cr ;

RUN
;package
