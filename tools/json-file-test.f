\ json-file-test.f - focused tests for the dynamic JSONL file cursor (package JSONF).
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f lib/fs-mutate.f tools/json.f tools/json-file.f tools/json-file-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require tools/json.f
require tools/json-file.f

using JSON

package JSONF-TEST

5000 constant LONG-N
5120 constant LONG-CAP

variable ROOT-U
variable IN-U
variable LONG-U

create ROOT-BUF FS-PATH-CAP allot
create IN-BUF FS-PATH-CAP allot
create LONG-BUF LONG-CAP allot

: COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: ROOT$ ( -- ptr u8 n )
   ROOT-BUF ROOT-U @ ;

: IN$ ( -- ptr u8 n )
   IN-BUF IN-U @ ;

: PREPARE ( -- )
   CLEANUP-RESET
   s" habu-json-file" TMPDIR-MKDIR {: a:ptr u:n :}
   a u ROOT-BUF ROOT-U COPY!
   ROOT$ CLEANUP-TREE+
   ROOT$ s" rows.jsonl" IN-BUF JOIN-PATH IN-U !
   IN$ CLEANUP+ ;

: DQ ( -- )
   34 SB-APPEND-C ;

: NL ( -- )
   10 SB-APPEND-C ;

: OBJ-A ( -- )
   123 SB-APPEND-C
   DQ s" a" SB-APPEND DQ
   s" :1}" SB-APPEND ;

: OBJ-B ( -- )
   123 SB-APPEND-C
   DQ s" b" SB-APPEND DQ
   s" :true}" SB-APPEND ;

: MIXED$ ( -- ptr u8 n )
   SB-RESET
   OBJ-A NL
   NL
   s" bad" SB-APPEND NL
   OBJ-B
   SB$ ;

: LONG+C ( n -- ) {: c:n :}
   LONG-U @ 1+ LONG-CAP > IF s" json-file-test: long buffer full" 1 die THEN
   c LONG-BUF LONG-U @ + c!
   LONG-U @ 1+ LONG-U ! ;

: LONG+ ( ptr u8 n -- ) {: a:ptr u:n :}
   LONG-U @ u + LONG-CAP > IF s" json-file-test: long buffer full" 1 die THEN
   a LONG-BUF LONG-U @ + u BYTE-COPY
   LONG-U @ u + LONG-U ! ;

: LONG$ ( -- ptr u8 n )
   LONG-BUF LONG-U @ ;

: LONG-DQ ( -- )
   34 LONG+C ;

: BUILD-LONG ( -- ptr u8 n )
   0 LONG-U !
   123 LONG+C
   LONG-DQ s" big" LONG+ LONG-DQ
   58 LONG+C
   LONG-DQ
   0 begin dup LONG-N < while
      97 LONG+C
      1+
   repeat drop
   LONG-DQ
   125 LONG+C
   LONG$ ;

: WRITE-MIXED ( -- )
   IN$ MIXED$ WRITE-ALL ;

: WRITE-LONG ( -- )
   IN$ BUILD-LONG WRITE-ALL ;

: OPEN-IN ( -- )
   IN$ JSONF:OPEN ;

: LINE-RT ( ptr u8 n -- ptr u8 n )
   JSONF-LINE:MAKE JSONF-LINE:UNMAKE ;

: ROW-RT ( n n n -- n n n )
   JSONF-ROW:MAKE JSONF-ROW:UNMAKE ;

: TEST-GENERATED ( -- )
   s" line-layout" LINE-RT s" line-layout" T$=
   11 22 33 ROW-RT 33 T= 22 T= 11 T= ;

\ EXPECT-ROW: the next row MUST be present; leaves ( node kind code ). A NONE here
\ is a test failure and dummy cells keep the two MATCH arms stack-balanced.
: EXPECT-ROW ( -- n n n )
   JSONF:NEXT-ROW MATCH option
     none OF 0 0= 0= TTRUE   -1 -1 -1 ENDOF
     some OF JSONF-ROW:UNMAKE ENDOF
   ;MATCH ;

\ EXPECT-EOF: the next row MUST be NONE (end of stream). A SOME here fails.
: EXPECT-EOF ( -- )
   JSONF:NEXT-ROW MATCH option
     none OF ENDOF
     some OF JSONF-ROW:UNMAKE 2drop drop   0 0= 0= TTRUE ENDOF
   ;MATCH ;

: CHECK-JSON-A ( -- )                            \ data row {"a":1}
   EXPECT-ROW {: node:n kind:n code:n :}
   code 0 T=
   kind JSONL-ROW-JSON T=
   node s" a" JSON-GET JSON-NUMBER$ s" 1" T$= ;

: CHECK-BLANK ( -- )                             \ blank row
   EXPECT-ROW {: node:n kind:n code:n :}
   code 0 T=
   kind JSONL-ROW-BLANK T=
   node -1 T= ;

: CHECK-ERROR ( -- )                             \ malformed row "bad"
   EXPECT-ROW {: node:n kind:n code:n :}
   code E-JSON-SYNTAX T=
   kind JSONL-ROW-ERROR T=
   node -1 T=
   JSONL-LINE$ s" bad" T$= ;

: CHECK-JSON-B ( -- )                            \ partial line at EOF {"b":true}
   EXPECT-ROW {: node:n kind:n code:n :}
   code 0 T=
   kind JSONL-ROW-JSON T=
   node s" b" JSON-GET JSON-BOOL@ TTRUE ;

: TEST-MIXED ( -- )
   WRITE-MIXED
   OPEN-IN
   CHECK-JSON-A   JSONF:LINE# 1 T=
   CHECK-BLANK    JSONF:LINE# 2 T=
   CHECK-ERROR    JSONF:LINE# 3 T=
   CHECK-JSON-B   JSONF:LINE# 4 T=
   EXPECT-EOF ;

: TEST-LONG ( -- )
   WRITE-LONG
   OPEN-IN
   EXPECT-ROW {: node:n kind:n code:n :}
   code 0 T=
   kind JSONL-ROW-JSON T=
   node s" big" JSON-GET JSON-STRING$ LONG-N T=
   drop
   JSONF:LINE-CAP JSONF:LINE-BOOT-CAP > TTRUE
   JSONF:LINE# 1 T=
   EXPECT-EOF ;

public
: MAIN ( -- )
   T-RESET
   PREPARE
   TEST-GENERATED
   TEST-MIXED
   TEST-LONG
   CLEANUP-RUN
   ROOT$ EXISTS? TFALSE
   T-REPORT
   s" json-file-test: ok" type cr ;

;package

JSONF-TEST:MAIN

;using
