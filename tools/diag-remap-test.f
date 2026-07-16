\ diag-remap-test.f - authenticated byte-exact text and JSON remapping.

require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/content-key.f
require tools/diag-remap.f

package DIAG-REMAP-TEST
private

FS-PATH-CAP constant PATH-CAP
4096 constant MAP-CAP
32 constant SRC-CAP
32 constant DIGEST-U

create SRC PATH-CAP allot
create MAP-BUF MAP-CAP allot
create SRC-BUF SRC-CAP allot
create SRC-DG DIGEST-U allot
create MAP-DG DIGEST-U allot
variable SRC-U
variable MAP-BUF-U
variable SRC-BUF-U

: COPY$ ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr up:ptr :}
   a dst u BYTE-COPY
   u up ! ;

: SRC$ ( -- ptr u8 n )
   SRC SRC-U @ ;

: MAP-BYTES$ ( -- ptr u8 n )
   MAP-BUF MAP-BUF-U @ ;

: SRC-BYTES$ ( -- ptr u8 n )
   SRC-BUF SRC-BUF-U @ ;

: HEX+ ( ptr u8 n -- ) {: a:ptr u:n :}
   0 begin dup u < while
      a over + c@ {: c:n :}
      c 4 rshift $F and dup 10 < if $30 + else $37 + then SB-APPEND-C
      c $F and dup 10 < if $30 + else $37 + then SB-APPEND-C
      1+
   repeat drop ;

: VALID-MAP$ ( -- ptr u8 n )
   SB-RESET
   S\" HABUMAP2\t7\t2\t1\t1\t" SB-APPEND
   SRC-DG DIGEST-U HEX+
   S\" \nF\t0\t656E7472792E66\nF\t1\t6F726967696E2E66\nC\t0\t2\t0\t1\nM\t0\t7\t0\t1\t1\t1\t0\n" SB-APPEND
   SB$ ;

: PREP ( -- )
   s" composed.f" SRC SRC-U COPY$
   S\" abc\ndef" {: src:ptr srcu:n :}
   src SRC-BUF srcu BYTE-COPY
   srcu SRC-BUF-U !
   SRC-BYTES$ SRC-DG SHA256
   VALID-MAP$ {: a:ptr u:n :}
   a MAP-BUF u BYTE-COPY
   u MAP-BUF-U !
   MAP-BYTES$ MAP-DG SHA256 ;

: REMAP$ ( ptr u8 n -- ptr u8 n ) {: diag:ptr diagu:n :}
   MAP-BYTES$ SRC-BYTES$ SRC$ SRC-DG DIGEST-U MAP-DG DIGEST-U
   diag diagu DIAG-REMAP:REMAP$ ;

: JSON-DIAG$ ( -- ptr u8 n )
   SB-RESET
   S\"  \t{  \qco\\u0064e\q : \qE-X\q , \qfile\q : \q" SB-APPEND
   SRC$ SB-APPEND
   S\" \q , \qline\q : 1 , \qcolumn\q : 1 , \qbyte_start\q : 0 , \qbyte_end\q : 3 , \qmeta\q : { \qescaped\q : \qA\\\\u0042\q }  }\t" SB-APPEND
   SB$ ;

: JSON-WANT$ ( -- ptr u8 n )
   S\"  \t{  \qco\\u0064e\q : \qE-X\q , \qfile\q : \qorigin.f\q , \qline\q : 1 , \qcolumn\q : 1 , \qbyte_start\q : 0 , \qbyte_end\q : 3 , \qmeta\q : { \qescaped\q : \qA\\\\u0042\q }  ,\qinclude_chain\q:[\qentry.f\q,\qorigin.f\q]}\t" ;

: JSON-LINE-COL$ ( -- ptr u8 n )
   SB-RESET
   S\" {\qfile\q:\q" SB-APPEND
   SRC$ SB-APPEND
   S\" \q,\qline\q:1,\qcolumn\q:1}" SB-APPEND
   SB$ ;

: JSON-LINE-COL-WANT$ ( -- ptr u8 n )
   S\" {\qfile\q:\qorigin.f\q,\qline\q:1,\qcolumn\q:1,\qbyte_start\q:0,\qinclude_chain\q:[\qentry.f\q,\qorigin.f\q]}" ;

: JSON-CHAIN$ ( -- ptr u8 n )
   S\" {\qfile\q:\qcomposed.f\q,\qline\q:1,\qcolumn\q:1,\qbyte_start\q:0,\qinclude_chain\q:[\qentry.f\q,\qorigin.f\q]}" ;

: JSON-BAD-CHAIN$ ( -- ptr u8 n )
   S\" {\qfile\q:\qcomposed.f\q,\qbyte_start\q:0,\qinclude_chain\q:[\qwrong.f\q]}" ;

: JSON-BAD-CHAIN-SHAPE$ ( -- ptr u8 n )
   S\" {\qfile\q:\qcomposed.f\q,\qbyte_start\q:0,\qinclude_chain\q:{}}" ;

: JSON-BAD-CROSSCHECK$ ( -- ptr u8 n )
   S\" {\qfile\q:\qcomposed.f\q,\qline\q:2,\qcolumn\q:1,\qbyte_start\q:0}" ;

: ESC-JSON+ ( ptr u8 n -- ) {: a:ptr u:n :}
   0 begin dup u < while
      $5C SB-APPEND-C $75 SB-APPEND-C $30 SB-APPEND-C $30 SB-APPEND-C
      a over + 1 HEX+
      1+
   repeat drop ;

: JSON-ESCAPED-LEAK$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   SB-RESET
   S\" {\qfile\q:\qother.f\q,\qmeta\q:{\qnote\q:\q" SB-APPEND
   a u ESC-JSON+
   S\" \q}}" SB-APPEND
   SB$ ;

: JSON-ESCAPED-KEY-LEAK$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   SB-RESET
   S\" {\qfile\q:\qother.f\q,\q" SB-APPEND
   a u ESC-JSON+
   S\" \q:\qnote\q}" SB-APPEND
   SB$ ;

: JSON-NO-LOC$ ( -- ptr u8 n )
   SB-RESET
   S\" {\qfile\q:\q" SB-APPEND
   SRC$ SB-APPEND
   S\" \q,\qcode\q:\qE-X\q}" SB-APPEND
   SB$ ;

: JSON-LEAK$ ( -- ptr u8 n )
   SB-RESET
   S\" {\qfile\q:\q<habu-composed>\q,\qline\q:1,\qcolumn\q:1,\qnote\q:\q" SB-APPEND
   SRC$ SB-APPEND
   S\" \q}" SB-APPEND
   SB$ ;

: TEXT-DIAG$ ( -- ptr u8 n )
   SB-RESET
   SRC$ SB-APPEND
   s" :2:1: bad dependency" SB-APPEND
   SB$ ;

: TEXT-SENTINEL$ ( -- ptr u8 n )
   s" E-X <habu-composed>:2:1: top-level" ;

: TEXT-NO-LOC$ ( -- ptr u8 n )
   SB-RESET
   s" bad path " SB-APPEND
   SRC$ SB-APPEND
   SB$ ;

: TEXT-LEAK$ ( -- ptr u8 n )
   SB-RESET
   s" <habu-composed>:1:1: leaked " SB-APPEND
   SRC$ SB-APPEND
   SB$ ;

: TEST-JSON ( -- )
   JSON-DIAG$ REMAP$ JSON-WANT$ T$=
   JSON-LINE-COL$ REMAP$ JSON-LINE-COL-WANT$ T$=
   JSON-CHAIN$ REMAP$ s" origin.f" CONTAINS? TTRUE ;

: TEST-TEXT ( -- )
   TEXT-DIAG$ REMAP$ {: a:ptr u:n :}
   a u SRC$ CONTAINS? TFALSE
   a u S\" \qorigin.f\q:2:1 byte 4: bad dependency" CONTAINS? TTRUE
   a u S\" include chain: \qentry.f\q -> \qorigin.f\q" CONTAINS? TTRUE ;

: TEST-SENTINEL ( -- )
   TEXT-SENTINEL$ REMAP$ {: a:ptr u:n :}
   a u S\" E-X \qorigin.f\q:2:1 byte 4: top-level" CONTAINS? TTRUE
   a u s" <habu-composed>" CONTAINS? TFALSE ;

: TEST-RAW ( -- )
   s" other.f:1:1: note" REMAP$ s" other.f:1:1: note" T$= ;

: TEST-FAIL-CLOSED ( -- )
   [: JSON-BAD-CHAIN$ REMAP$ 2drop ;] E-DIAG-ORIGIN TTHROWSQ
   [: JSON-BAD-CHAIN-SHAPE$ REMAP$ 2drop ;] E-DIAG-SCHEMA TTHROWSQ
   [: JSON-BAD-CROSSCHECK$ REMAP$ 2drop ;] E-DIAG-ORIGIN TTHROWSQ
   [: JSON-NO-LOC$ REMAP$ 2drop ;] E-DIAG-ORIGIN TTHROWSQ
   [: TEXT-NO-LOC$ REMAP$ 2drop ;] E-DIAG-ORIGIN TTHROWSQ
   [: JSON-LEAK$ REMAP$ 2drop ;] E-DIAG-ORIGIN TTHROWSQ
   [: TEXT-LEAK$ REMAP$ 2drop ;] E-DIAG-ORIGIN TTHROWSQ
   [: SRC$ JSON-ESCAPED-LEAK$ REMAP$ 2drop ;] E-DIAG-ORIGIN TTHROWSQ
   [: s" <habu-composed>" JSON-ESCAPED-LEAK$ REMAP$ 2drop ;]
      E-DIAG-ORIGIN TTHROWSQ
   [: SRC$ JSON-ESCAPED-KEY-LEAK$ REMAP$ 2drop ;] E-DIAG-ORIGIN TTHROWSQ
   [: s" <habu-composed>" JSON-ESCAPED-KEY-LEAK$ REMAP$ 2drop ;]
      E-DIAG-ORIGIN TTHROWSQ ;

public

: RUN ( -- )
   T-RESET
   PREP
   TEST-JSON
   TEST-TEXT
   TEST-SENTINEL
   TEST-RAW
   TEST-FAIL-CLOSED
   T-REPORT
   s" diag-remap-test: ok" type cr ;

;package

DIAG-REMAP-TEST:RUN
