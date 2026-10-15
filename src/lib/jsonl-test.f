\ jsonl-test.f - the JSONL file cursor and memory splitter through the public API.
\ Run: bin/hb --load src/lib/jsonl-test.f

require src/lib/test.f
require src/lib/fs.f
require src/lib/byte-buffer.f
require src/lib/json-read.f
require src/lib/jsonl.f
require src/lib/test/outcome.f
require src/lib/test/subject.f

package JSONL-TEST
private

TYPED-VARIABLE A JSONL:cursor
TYPED-VARIABLE B JSONL:cursor

8192 constant BLOCK-N                  \ the cursor's read block
16399 constant LONG-N                  \ a JSON string line spanning three blocks
LONG-N 2 + BUFFER: FIXTURE
LONG-N 1+ BUFFER: STR-OUT

$47 constant FATAL-RC                  \ the close-invariant exit status
$100 constant CAP
1000 constant TIMEOUT-MS
CAP BUFFER: CAP-OUT
CAP BUFFER: CAP-ERR

: TMP$ ( -- ptr u8 n )
   s" TMPDIR" GETENV dup 0= if 2drop s" /tmp" then ;

: PATH$ ( -- ptr u8 n )
   TMP$ SB-RESET SB-APPEND s" /habu-jsonl-e2e.jsonl" SB-APPEND SB$ ;

: PATH-B$ ( -- ptr u8 n )
   TMP$ SB-RESET SB-APPEND s" /habu-jsonl-e2e-b.jsonl" SB-APPEND SB$ ;

\ Write a JSON string line of n bytes, "a...a", followed by the tail bytes.
: STRING-FIXTURE ( n ptr u8 n -- )
   {: n:n tail:ptr tu:n :}
   [char] " FIXTURE c!
   n 1- 1 ?do [char] a FIXTURE i + c! loop
   [char] " FIXTURE n 1- + c!
   tail FIXTURE n + tu BYTE-COPY
   PATH$ FIXTURE n tu + WRITE-ALL ;

: LONG-FIXTURE ( -- )
   LONG-N s\" \n7" STRING-FIXTURE ;

\ POSIX open-rd takes the lowest free descriptor. With no other opens between
\ the probe and JSONL:OPEN, the next probe verifies the cursor owns that fd.
: FREE-FD ( -- n )
   s" /dev/null" FS-PATHZ open-rd {: fd:n :}
   fd 0 >= TTRUE
   fd close-rc 0 T=
   fd ;

\ Open A on PATH$ and prove it holds the probed descriptor; answer that fd.
: OPEN-KNOWN ( n -- n )
   {: max:n :}
   FREE-FD {: fd:n :}
   A PATH$ max BUF:N>BLEN JSONL:OPEN
   FREE-FD fd 1+ T=
   fd ;

\ The line at pos is want, and the position after it is next.
: SPLIT= ( ptr u8 n n ptr u8 n n -- )
   {: a:ptr u:n pos:n want:ptr wu:n next:n :}
   a u pos JSONL:SPLIT-LINE TTRUE {: la:ptr lu:n got:n :}
   la lu want wu T$=
   got next T= ;

\ No line starts at pos: the splitter answers an empty span and pos.
: SPLIT-END ( ptr u8 n n -- )
   {: a:ptr u:n pos:n :}
   a u pos JSONL:SPLIT-LINE TFALSE {: la:ptr lu:n got:n :}
   lu 0 T=
   got pos T= ;

: SPLIT ( -- )
   s" " 0 SPLIT-END
   s\" \n" 0 s" " 1 SPLIT=
   s\" \n" 1 SPLIT-END
   s\" a\n" 0 s" a" 2 SPLIT=
   s\" a\n" 2 SPLIT-END
   s\" a\n" 5 SPLIT-END
   s\" a\r\n\nlast" 0 s\" a\r" 3 SPLIT=
   s\" a\r\n\nlast" 3 s" " 4 SPLIT=
   s\" a\r\n\nlast" 4 s" last" 8 SPLIT=
   s\" a\r\n\nlast" 8 SPLIT-END
   s\" \n1\n\n2" 0 s" " 1 SPLIT=
   s\" \n1\n\n2" 1 s" 1" 3 SPLIT=
   s\" \n1\n\n2" 3 s" " 4 SPLIT=
   s\" \n1\n\n2" 4 s" 2" 5 SPLIT=
   s\" \n1\n\n2" 5 SPLIT-END
   [: s" a" drop -1 0 JSONL:SPLIT-LINE drop 2drop drop ;] JSONL:E-SOURCE TTHROWSQ
   [: NULL-PTR 1 0 JSONL:SPLIT-LINE drop 2drop drop ;] JSONL:E-SOURCE TTHROWSQ
   [: s" a" -1 JSONL:SPLIT-LINE drop 2drop drop ;] JSONL:E-SOURCE TTHROWSQ ;

\ Advance cur to a line holding a JSON string of n bytes, all a's inside.
: STRING-LINE ( ptr JSONL:cursor n -- )
   {: cur:ptr n:n :}
   cur JSONL:NEXT TTRUE
   cur JSONL:READER
   JR:NEXT JR:T-STR T=
   STR-OUT LONG-N 1+ JR:STR n 2 - T=
   JR:CLOSE
   STR-OUT c@ [char] a T=
   STR-OUT n 3 - + c@ [char] a T= ;

: READ-INT ( ptr JSONL:cursor n -- )
   {: cur:ptr want:n :}
   cur JSONL:READER
   JR:NEXT JR:T-INT T=
   JR:INT want T=
   JR:CLOSE ;

: INT-LINE ( ptr JSONL:cursor n -- )
   {: cur:ptr want:n :}
   cur JSONL:NEXT TTRUE
   cur want READ-INT ;

: READ-ONE ( ptr JSONL:cursor -- )
   JSONL:READER
   JR:NEXT JR:T-OBJ T=
   s" x" JR:FIND-KEY TTRUE
   JR:TOKEN JR:T-INT T=
   JR:INT 7 T=
   JR:CLOSE ;

: FILE ( -- )
   LONG-FIXTURE
   PATH$ FILE-SIZE LONG-N 2 + T=
   LONG-N OPEN-KNOWN {: fd:n :}
   A LONG-N STRING-LINE
   A 7 INT-LINE
   A JSONL:NEXT TFALSE
   FREE-FD fd T=
   [: A JSONL:NEXT drop ;] JSONL:E-STATE TTHROWSQ ;

: BLOCK-EDGE ( -- )
   BLOCK-N 1- s\" \n8" STRING-FIXTURE       \ the LF is the block's last byte
   BLOCK-N 1- OPEN-KNOWN {: fd:n :}
   A BLOCK-N 1- STRING-LINE
   A 8 INT-LINE
   A JSONL:NEXT TFALSE
   FREE-FD fd T=
   BLOCK-N s" " STRING-FIXTURE              \ one full block, no LF, then EOF
   BLOCK-N OPEN-KNOWN {: fd2:n :}
   A BLOCK-N STRING-LINE
   A JSONL:NEXT TFALSE
   FREE-FD fd2 T= ;

: EMPTY-LINES ( -- )
   PATH$ s\" \n\n{\qx\q:7}\n\n\n9" WRITE-ALL
   16 OPEN-KNOWN {: fd:n :}
   A JSONL:NEXT TTRUE
   A READ-ONE
   A READ-ONE                                 \ a second reader over the same line
   A 9 INT-LINE
   A JSONL:NEXT TFALSE
   FREE-FD fd T=
   PATH$ s\" \n\n" WRITE-ALL
   16 OPEN-KNOWN drop
   A JSONL:NEXT TFALSE
   PATH$ s" " WRITE-ALL
   16 OPEN-KNOWN {: fd2:n :}
   A JSONL:NEXT TFALSE
   FREE-FD fd2 T= ;

: CR-LIMIT ( -- )
   PATH$ s\" 7\r\n" WRITE-ALL
   FREE-FD {: fd:n :}
   [: A PATH$ 1 BUF:N>BLEN JSONL:OPEN A JSONL:NEXT drop ;] JSONL:E-LIMIT TTHROWSQ
   FREE-FD fd T=
   2 OPEN-KNOWN drop
   A 7 INT-LINE
   A JSONL:NEXT TFALSE ;

: LIMIT ( -- )
   LONG-FIXTURE
   FREE-FD {: fd:n :}
   [: A PATH$ LONG-N 1- BUF:N>BLEN JSONL:OPEN A JSONL:NEXT drop ;] JSONL:E-LIMIT TTHROWSQ
   FREE-FD fd T=
   [: A PATH$ 1 BUF:N>BLEN JSONL:OPEN A JSONL:NEXT drop ;] JSONL:E-LIMIT TTHROWSQ
   FREE-FD fd T=
   LONG-N OPEN-KNOWN drop
   A LONG-N STRING-LINE
   A JSONL:CLOSE
   FREE-FD fd T= ;

: READ-FAULT ( -- )
   FREE-FD {: fd:n :}
   A TMP$ 16 BUF:N>BLEN JSONL:OPEN           \ a directory opens; its read fails
   FREE-FD fd 1+ T=
   [: A JSONL:NEXT drop ;] E-FS-IO TTHROWSQ
   FREE-FD fd T=
   [: A JSONL:NEXT drop ;] JSONL:E-STATE TTHROWSQ ;

: OPEN-MISSING ( -- )
   FREE-FD {: fd:n :}
   [: A s" no-such-habu-jsonl-file" 1 BUF:N>BLEN JSONL:OPEN ;] E-FS-OPEN TTHROWSQ
   FREE-FD fd T=
   [: A JSONL:NEXT drop ;] JSONL:E-STATE TTHROWSQ ;

: OPEN-TWICE ( -- )
   PATH$ s\" 1\n2" WRITE-ALL
   16 OPEN-KNOWN {: fd:n :}
   [: A PATH$ 16 BUF:N>BLEN JSONL:OPEN ;] JSONL:E-STATE TTHROWSQ
   FREE-FD fd 1+ T=
   A 1 INT-LINE
   A JSONL:CLOSE
   FREE-FD fd T= ;

: READER-EARLY ( -- )
   PATH$ s\" 1\n" WRITE-ALL
   16 OPEN-KNOWN drop
   [: A JSONL:READER JR:CLOSE ;] JSONL:E-STATE TTHROWSQ
   A 1 INT-LINE
   A JSONL:NEXT TFALSE
   [: A JSONL:READER JR:CLOSE ;] JSONL:E-STATE TTHROWSQ ;

: CLOSE-EARLY ( -- )
   PATH$ s\" 1\n2" WRITE-ALL
   16 OPEN-KNOWN {: fd:n :}
   A JSONL:NEXT TTRUE
   A JSONL:CLOSE
   FREE-FD fd T=
   A JSONL:CLOSE
   [: A JSONL:NEXT drop ;] JSONL:E-STATE TTHROWSQ ;

\ Both cursors hold a live reader at once, so shared backing would corrupt one.
: INTERLEAVE ( -- )
   PATH$ s\" 1\n2" WRITE-ALL
   PATH-B$ s\" 3\n{\qx\q:7}" WRITE-ALL
   A PATH$ 16 BUF:N>BLEN JSONL:OPEN
   B PATH-B$ 16 BUF:N>BLEN JSONL:OPEN
   A JSONL:NEXT TTRUE
   B JSONL:NEXT TTRUE
   A JSONL:READER B JSONL:READER
   JR:NEXT JR:T-INT T= JR:INT 3 T= JR:CLOSE
   JR:NEXT JR:T-INT T= JR:INT 1 T= JR:CLOSE
   B JSONL:NEXT TTRUE
   A 2 INT-LINE
   B READ-ONE
   A JSONL:NEXT TFALSE
   B JSONL:NEXT TFALSE ;

public

\ The child subject of CLOSE-FAULT: an external close makes the read fail
\ EBADF, and the cursor's cleanup close then fails too.
: SABOTAGE ( -- )
   16 OPEN-KNOWN {: fd:n :}
   fd close-rc 0 T=
   A JSONL:NEXT drop ;

private

: CLOSE-FAULT ( -- )
   s" ' JSONL-TEST:SABOTAGE catch $53 emit" {: src:ptr srcu:n :}
   src srcu CAP-OUT CAP >LEN CAP-ERR CAP >LEN TIMEOUT-MS >MS SUBJECT:RUN
   {: outu:len erru:len oc :}
   src srcu CAP-OUT outu LEN>N CAP-ERR erru LEN>N oc FATAL-RC T-OUTCOME-EXITED=
   outu LEN>N 0 T=
   CAP-ERR erru LEN>N s\" jsonl: close failed\n" T$= ;

public
: ARTIFACT$ ( -- ptr u8 n ) PATH$ ;
: RUN ( -- )
   SPLIT
   FILE BLOCK-EDGE EMPTY-LINES
   CR-LIMIT LIMIT
   READ-FAULT OPEN-MISSING OPEN-TWICE READER-EARLY CLOSE-EARLY
   INTERLEAVE
   CLOSE-FAULT
   LONG-FIXTURE ;
;package

T-RESET
JSONL-TEST:RUN
T-REPORT
s" jsonl-test: ok; fixture: " type
JSONL-TEST:ARTIFACT$ type cr
