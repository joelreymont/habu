\ source-map-test.f - authenticated canonical HABUMAP2 origin fixtures.

require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/content-key.f
require tools/source-map.f

package SOURCE-MAP-TEST
private

FS-PATH-CAP constant PATH-CAP
32 constant DIGEST-U

create MAP PATH-CAP allot
create SRC PATH-CAP allot
create SRC-DG DIGEST-U allot
create MAP-DG DIGEST-U allot
variable MAP-U
variable SRC-U

: MAP$ ( -- ptr u8 n )
   MAP MAP-U @ ;

: SRC$ ( -- ptr u8 n )
   SRC SRC-U @ ;

: HEX+ ( ptr u8 n -- ) {: a:ptr u:n :}
   0 begin dup u < while
      a over + c@ {: c:n :}
      c 4 rshift $F and dup 10 < if $30 + else $37 + then SB-APPEND-C
      c $F and dup 10 < if $30 + else $37 + then SB-APPEND-C
      1+
   repeat drop ;

: HEADER+ ( n n n n -- ) {: srcu:n files:n chains:n rows:n :}
   s" HABUMAP2" SB-APPEND 9 SB-APPEND-C
   srcu FS-MUT-SB-U 9 SB-APPEND-C
   files FS-MUT-SB-U 9 SB-APPEND-C
   chains FS-MUT-SB-U 9 SB-APPEND-C
   rows FS-MUT-SB-U 9 SB-APPEND-C
   SRC-DG DIGEST-U HEX+
   10 SB-APPEND-C ;

: VALID-MAP$ ( -- ptr u8 n )
   SB-RESET
   7 2 1 1 HEADER+
   S\" F\t0\t656E7472792E66\nF\t1\t6F726967696E2E66\nC\t0\t2\t0\t1\nM\t0\t7\t0\t1\t1\t1\t0\n" SB-APPEND
   SB$ ;

: SPAN-MAP$ ( n n -- ptr u8 n ) {: src:n chain:n :}
   SB-RESET
   7 3 chain 0= if 1 else 2 then 2 HEADER+
   S\" F\t0\t656E7472792E66\nF\t1\t6F726967696E2E66\nF\t2\t6F746865722E66\nC\t0\t2\t0\t1\n" SB-APPEND
   chain 0= 0= if S\" C\t1\t2\t0\t2\n" SB-APPEND then
   S\" M\t0\t3\t0\t1\t1\t1\t0\nM\t3\t4\t" SB-APPEND
   src FS-MUT-SB-U
   S\" \t1\t" SB-APPEND
   chain 0= if 4 else 1 then FS-MUT-SB-U
   9 SB-APPEND-C
   chain 0= if 1 else 2 then FS-MUT-SB-U
   9 SB-APPEND-C
   chain FS-MUT-SB-U
   10 SB-APPEND-C
   SB$ ;

: WRITE-MAP ( ptr u8 n -- ) {: a:ptr u:n :}
   u PATH-CAP > if E-DIAG-CAPACITY throw then
   a MAP u BYTE-COPY
   u MAP-U !
   MAP$ MAP-DG SHA256 ;

: WRITE-VALID ( -- )
   VALID-MAP$ WRITE-MAP ;

: PREP ( -- )
   S\" abc\ndef" {: a:ptr u:n :}
   a SRC u BYTE-COPY
   u SRC-U !
   SRC$ SRC-DG SHA256
   WRITE-VALID ;

: OPEN ( -- )
   MAP$ SRC$ SRC-DG DIGEST-U MAP-DG DIGEST-U SOURCE-MAP:OPEN ;

: TEST-FIRST ( -- )
   OPEN
   0 SOURCE-MAP:ORIGIN!
   SOURCE-MAP:ORIGIN-OUTPUT-BYTE 0 T=
   SOURCE-MAP:ORIGIN-FILE-ID 1 T=
   SOURCE-MAP:ORIGIN-CHAIN-ID 0 T=
   SOURCE-MAP:ORIGIN-FILE$ s" origin.f" T$=
   SOURCE-MAP:ORIGIN-LINE 1 T=
   SOURCE-MAP:ORIGIN-COLUMN 1 T=
   SOURCE-MAP:ORIGIN-BYTE 0 T=
   SOURCE-MAP:ORIGIN-CHAIN-N 2 T=
   0 SOURCE-MAP:ORIGIN-CHAIN-FILE$ s" entry.f" T$=
   1 SOURCE-MAP:ORIGIN-CHAIN-FILE$ s" origin.f" T$= ;

: TEST-ADVANCE ( -- )
   OPEN
   4 SOURCE-MAP:ORIGIN!
   SOURCE-MAP:ORIGIN-OUTPUT-BYTE 4 T=
   SOURCE-MAP:ORIGIN-LINE 2 T=
   SOURCE-MAP:ORIGIN-COLUMN 1 T=
   SOURCE-MAP:ORIGIN-BYTE 4 T= ;

: TEST-LINE-COLUMN ( -- )
   OPEN
   2 1 SOURCE-MAP:ORIGIN-LINE-COLUMN!
   SOURCE-MAP:ORIGIN-OUTPUT-BYTE 4 T=
   SOURCE-MAP:ORIGIN-LINE 2 T=
   SOURCE-MAP:ORIGIN-COLUMN 1 T=
   SOURCE-MAP:ORIGIN-BYTE 4 T= ;

: TEST-AUTH ( -- )
   SRC c@ {: old:n :}
   $58 SRC c!
   [: OPEN ;] E-DIAG-SCHEMA TTHROWSQ
   old SRC c!
   $58 MAP MAP-U @ + c!
   MAP-U @ 1+ MAP-U !
   [: OPEN ;] E-DIAG-SCHEMA TTHROWSQ
   MAP-U @ 1- MAP-U !
   WRITE-VALID ;

: EXPECT-SCHEMA ( ptr u8 n -- )
   WRITE-MAP
   [: OPEN ;] E-DIAG-SCHEMA TTHROWSQ ;

: BAD-BEGIN ( n n n -- ) {: files:n chains:n rows:n :}
   SB-RESET
   7 files chains rows HEADER+ ;

: ROW+ ( ptr u8 n -- )
   SB-APPEND
   10 SB-APPEND-C ;

: ORIGIN-FILE+ ( -- )
   S\" F\t0\t656E7472792E66" ROW+ ;

: ENTRY-FILE+ ( -- )
   S\" F\t1\t6F726967696E2E66" ROW+ ;

: ORIGIN-CHAIN+ ( -- )
   S\" C\t0\t1\t0" ROW+ ;

: VALID-ROW+ ( -- )
   S\" M\t0\t7\t0\t1\t1\t0\t0" ROW+ ;

: EXPECT-BAD ( -- )
   SB$ EXPECT-SCHEMA ;

: BAD-FILE-ID ( ptr u8 n -- ) {: id:ptr idu:n :}
   1 1 1 BAD-BEGIN
   S\" F\t" SB-APPEND id idu SB-APPEND
   S\" \t6F726967696E2E66" ROW+
   ORIGIN-CHAIN+
   VALID-ROW+
   EXPECT-BAD ;

: BAD-LEADING-ZERO ( -- )
   s" 00" BAD-FILE-ID ;

: BAD-PLUS ( -- )
   s" +0" BAD-FILE-ID ;

: BAD-NONDIGIT ( -- )
   s" 0A" BAD-FILE-ID ;

: BAD-LOWER-HEX ( -- )
   1 1 1 BAD-BEGIN
   S\" F\t0\t6f726967696E2E66" ROW+
   ORIGIN-CHAIN+
   VALID-ROW+
   EXPECT-BAD ;

: BAD-ORDER ( -- )
   1 1 1 BAD-BEGIN
   ORIGIN-CHAIN+
   ORIGIN-FILE+
   VALID-ROW+
   EXPECT-BAD ;

: BAD-RANGE ( ptr u8 n -- ) {: row:ptr rowu:n :}
   1 1 1 BAD-BEGIN
   ORIGIN-FILE+
   ORIGIN-CHAIN+
   row rowu ROW+
   EXPECT-BAD ;

: BAD-GAP ( -- )
   S\" M\t1\t6\t10\t2\t3\t0\t0" BAD-RANGE ;

: BAD-PARTIAL ( -- )
   S\" M\t0\t6\t10\t2\t3\t0\t0" BAD-RANGE ;

: BAD-PATH ( ptr u8 n -- ) {: path:ptr pathu:n :}
   1 1 1 BAD-BEGIN
   S\" F\t0\t" SB-APPEND path pathu ROW+
   ORIGIN-CHAIN+
   VALID-ROW+
   EXPECT-BAD ;

: BAD-EMPTY-PATH ( -- )
   s" " BAD-PATH ;

: BAD-NUL-PATH ( -- )
   s" 00" BAD-PATH ;

: BAD-DUP-FILE ( -- )
   2 1 1 BAD-BEGIN
   ORIGIN-FILE+
   S\" F\t1\t656E7472792E66" ROW+
   ORIGIN-CHAIN+
   VALID-ROW+
   EXPECT-BAD ;

: BAD-DUP-CHAIN ( -- )
   1 2 1 BAD-BEGIN
   ORIGIN-FILE+
   ORIGIN-CHAIN+
   S\" C\t1\t1\t0" ROW+
   VALID-ROW+
   EXPECT-BAD ;

: BAD-CHAIN-ORIGIN ( -- )
   2 1 1 BAD-BEGIN
   ORIGIN-FILE+
   ENTRY-FILE+
   S\" C\t0\t1\t1" ROW+
   VALID-ROW+
   EXPECT-BAD ;

: BAD-CHAIN-REPEAT ( -- )
   1 1 1 BAD-BEGIN
   ORIGIN-FILE+
   S\" C\t0\t2\t0\t0" ROW+
   VALID-ROW+
   EXPECT-BAD ;

: BAD-LINE-ZERO ( -- )
   S\" M\t0\t7\t10\t0\t3\t0\t0" BAD-RANGE ;

: BAD-COLUMN-ZERO ( -- )
   S\" M\t0\t7\t10\t2\t0\t0\t0" BAD-RANGE ;

: TEST-CANONICAL ( -- )
   BAD-LEADING-ZERO
   BAD-PLUS
   BAD-NONDIGIT
   BAD-LOWER-HEX
   BAD-ORDER
   BAD-GAP
   BAD-PARTIAL
   BAD-EMPTY-PATH
   BAD-NUL-PATH
   BAD-DUP-FILE
   BAD-DUP-CHAIN
   BAD-CHAIN-ORIGIN
   BAD-CHAIN-REPEAT
   BAD-LINE-ZERO
   BAD-COLUMN-ZERO
   WRITE-VALID ;

: TEST-SPANS ( -- )
   3 0 SPAN-MAP$ WRITE-MAP
   OPEN
   1 6 SOURCE-MAP:ORIGIN-SPAN!
   SOURCE-MAP:ORIGIN-FILE$ s" origin.f" T$=
   SOURCE-MAP:ORIGIN-CHAIN-ID 0 T=
   SOURCE-MAP:ORIGIN-BYTE 1 T=
   4 0 SPAN-MAP$ EXPECT-SCHEMA
   0 1 SPAN-MAP$ WRITE-MAP
   OPEN
   [: 1 6 SOURCE-MAP:ORIGIN-SPAN! ;] E-DIAG-ORIGIN TTHROWSQ
   WRITE-VALID ;

: TEST-EOF-SPAN ( -- )
   OPEN
   7 7 SOURCE-MAP:ORIGIN-SPAN!
   SOURCE-MAP:ORIGIN-OUTPUT-BYTE 7 T=
   SOURCE-MAP:ORIGIN-FILE$ s" origin.f" T$=
   SOURCE-MAP:ORIGIN-LINE 2 T=
   SOURCE-MAP:ORIGIN-COLUMN 4 T=
   SOURCE-MAP:ORIGIN-BYTE 7 T=
   [: 8 8 SOURCE-MAP:ORIGIN-SPAN! ;] E-DIAG-ORIGIN TTHROWSQ ;

: TEST-UNMAPPED ( -- )
   OPEN
   [: 7 SOURCE-MAP:ORIGIN! ;] E-DIAG-ORIGIN TTHROWSQ
   [: 3 1 SOURCE-MAP:ORIGIN-LINE-COLUMN! ;] E-DIAG-ORIGIN TTHROWSQ ;

public

: RUN ( -- )
   T-RESET
   PREP
   TEST-FIRST
   TEST-ADVANCE
   TEST-LINE-COLUMN
   TEST-AUTH
   TEST-CANONICAL
   TEST-SPANS
   TEST-EOF-SPAN
   TEST-UNMAPPED
   T-REPORT
   s" source-map-test: ok" type cr ;

;package

SOURCE-MAP-TEST:RUN
