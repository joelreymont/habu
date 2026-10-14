\ jsonl-test.f - file and memory JSONL framing through the public API.
\ Run: bin/hb --load src/lib/jsonl-test.f

require src/lib/test.f
require src/lib/fs.f
require src/lib/byte-buffer.f
require src/lib/jsonl.f

package JSONL-TEST
private

TYPED-VARIABLE A JSONL:cursor
TYPED-VARIABLE B JSONL:cursor

16399 constant LONG-N
LONG-N 2 + BUFFER: FIXTURE

: PATH$ ( -- ptr u8 n )
   s" TMPDIR" GETENV dup 0= if 2drop s" /tmp" then
   SB-RESET SB-APPEND s" /habu-jsonl-e2e.jsonl" SB-APPEND SB$ ;

: MAKE-FIXTURE ( -- )
   LONG-N 0 ?do [char] a FIXTURE i + c! loop
   10 FIXTURE LONG-N + c!
   [char] b FIXTURE LONG-N 1+ + c!
   PATH$ FIXTURE LONG-N 2 + WRITE-ALL ;

: TAKE ( option<JSONL:line> -- ptr u8 n n )
   MATCH option
      none OF JSONL:E-STATE throw ENDOF
      some OF
         JSONL-LINE:UNMAKE {: a:ptr u:NUM:byte-len number:n :}
         a u BUF:BLEN>N number
      ENDOF
   ;MATCH ;

: CHECK-LINE ( option<JSONL:line> ptr u8 n n -- )
   {: row:option<JSONL:line> want:ptr wu:n number:n :}
   row TAKE {: got:ptr gu:n line:n :}
   got gu want wu T$=
   line number T= ;

: CHECK-END ( option<JSONL:line> -- )
   MATCH option
      none OF ENDOF
      some OF JSONL-LINE:UNMAKE drop 2drop JSONL:E-STATE throw ENDOF
   ;MATCH ;

: BEGIN-A ( ptr u8 n n -- )
   {: src:ptr u:n max:n :}
   A src u max BUF:N>BLEN JSONL:START ;

: MEMORY ( -- )
   s" " 0 BEGIN-A
   A JSONL:NEXT-LINE CHECK-END
   A JSONL:CLOSE A JSONL:CLOSE
   s\" \n" 0 BEGIN-A
   A JSONL:NEXT-LINE s" " 1 CHECK-LINE
   A JSONL:NEXT-LINE CHECK-END
   A JSONL:CLOSE
   s\" a\n" 1 BEGIN-A
   A JSONL:NEXT-LINE s" a" 1 CHECK-LINE
   A JSONL:NEXT-LINE CHECK-END
   A JSONL:CLOSE
   s\" a\r\n\nlast" 4 BEGIN-A
   A JSONL:NEXT-LINE s\" a\r" 1 CHECK-LINE
   A JSONL:NEXT-LINE s" " 2 CHECK-LINE
   A JSONL:NEXT-LINE s" last" 3 CHECK-LINE
   A JSONL:NEXT-LINE CHECK-END
   A JSONL:CLOSE ;

: NEXT-SKIPS ( -- )
   s\" \n1\n\n2" 1 BEGIN-A
   A JSONL:NEXT s" 1" 2 CHECK-LINE
   A JSONL:NEXT s" 2" 4 CHECK-LINE
   A JSONL:NEXT CHECK-END
   A JSONL:CLOSE ;

: INTERLEAVE ( -- )
   A s\" a\nb" 2 BUF:N>BLEN JSONL:START
   B s\" x\ny" 2 BUF:N>BLEN JSONL:START
   A JSONL:NEXT-LINE s" a" 1 CHECK-LINE
   B JSONL:NEXT-LINE s" x" 1 CHECK-LINE
   A JSONL:NEXT-LINE s" b" 2 CHECK-LINE
   B JSONL:NEXT-LINE s" y" 2 CHECK-LINE
   A JSONL:CLOSE B JSONL:CLOSE ;

: READ-ONE ( ptr JSONL:cursor -- )
   JSONL:READER
   JR:NEXT JR:T-OBJ T=
   s" x" JR:FIND-KEY TTRUE
   JR:TOKEN JR:T-INT T=
   JR:INT 7 T=
   JR:CLOSE ;

: READER ( -- )
   s\" {\qx\q:7}\n0" 16 BEGIN-A
   A JSONL:NEXT-LINE s\" {\qx\q:7}" 1 CHECK-LINE
   A READ-ONE
   A JSONL:NEXT-LINE s" 0" 2 CHECK-LINE
   A JSONL:CLOSE ;

: CR-LIMIT ( -- )
   s\" a\r\n" 1 BEGIN-A
   A JSONL:NEXT-LINE drop A JSONL:CLOSE ;

: TOO-LONG-MEM ( -- )
   s" ab" 1 BEGIN-A A JSONL:NEXT-LINE drop A JSONL:CLOSE ;

: TOO-LONG-FILE ( -- )
   A PATH$ 1 BUF:N>BLEN JSONL:OPEN
   A JSONL:NEXT-LINE drop A JSONL:CLOSE ;

: FILE ( -- )
   MAKE-FIXTURE
   PATH$ FILE-SIZE LONG-N 2 + T=
   A PATH$ LONG-N BUF:N>BLEN JSONL:OPEN
   A JSONL:NEXT-LINE TAKE {: first:ptr u:n line:n :}
   u LONG-N T= line 1 T=
   first c@ [char] a T=
   first LONG-N 1- + c@ [char] a T=
   A JSONL:NEXT-LINE s" b" 2 CHECK-LINE
   A JSONL:NEXT-LINE CHECK-END
   A JSONL:NEXT-LINE CHECK-END
   A JSONL:CLOSE ;

: FILE-ZERO ( -- )
   PATH$ s\" \n" WRITE-ALL
   A PATH$ 0 BUF:N>BLEN JSONL:OPEN
   A JSONL:NEXT-LINE s" " 1 CHECK-LINE
   A JSONL:NEXT-LINE CHECK-END
   A JSONL:CLOSE
   MAKE-FIXTURE ;

: OPEN-MISSING ( -- )
   A s" no-such-habu-jsonl-file" 1 BUF:N>BLEN JSONL:OPEN ;

: THROW-BODY ( n ptr JSONL:cursor -- n ptr JSONL:cursor )
   dup JSONL:NEXT-LINE drop
   dup JSONL:READER JR:NEXT drop JR:CLOSE ;

: THROW-WITH ( -- )
   3 A PATH$ LONG-N BUF:N>BLEN [: THROW-BODY ;] JSONL:WITH-FILE drop ;

: SUCCESS-BODY ( n ptr JSONL:cursor -- n ptr JSONL:cursor )
   dup JSONL:NEXT-LINE TAKE {: first:ptr u:n number:n :}
   first c@ [char] a T=
   u LONG-N T=
   number 1 T=
   swap 1+ swap ;

: WITH-CLEANUP ( -- )
   3 A PATH$ LONG-N BUF:N>BLEN [: SUCCESS-BODY ;] JSONL:WITH-FILE 4 T=
   [: THROW-WITH ;] E-JR-MALFORMED TTHROWSQ
   A s" 1" 1 BUF:N>BLEN JSONL:START
   A JSONL:NEXT-LINE s" 1" 1 CHECK-LINE
   A JSONL:CLOSE ;

public
: ARTIFACT$ ( -- ptr u8 n ) PATH$ ;
: RUN ( -- )
   MEMORY NEXT-SKIPS INTERLEAVE READER
   FILE FILE-ZERO
   [: TOO-LONG-MEM ;] JSONL:E-LIMIT TTHROWSQ
   A s" 1" 1 BUF:N>BLEN JSONL:START A JSONL:CLOSE
   [: CR-LIMIT ;] JSONL:E-LIMIT TTHROWSQ
   A s" 1" 1 BUF:N>BLEN JSONL:START A JSONL:CLOSE
   [: TOO-LONG-FILE ;] JSONL:E-LIMIT TTHROWSQ
   A s" 1" 1 BUF:N>BLEN JSONL:START A JSONL:CLOSE
   [: OPEN-MISSING ;] E-FS-OPEN TTHROWSQ
   A s" 1" 1 BUF:N>BLEN JSONL:START A JSONL:CLOSE
   WITH-CLEANUP ;
;package

T-RESET
JSONL-TEST:RUN
T-REPORT
s" jsonl-test: ok; fixture: " type
JSONL-TEST:ARTIFACT$ type cr
