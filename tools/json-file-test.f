\ json-file-test.f - focused tests for dynamic JSONL file cursor.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f lib/fs-mutate.f tools/json.f tools/json-file.f tools/json-file-test.f

5000 constant JFT-LONG-N
5120 constant JFT-LONG-CAP

variable JFT-ROOT-U
variable JFT-IN-U
variable JFT-LONG-U
variable JFT-ROOT

create JFT-ROOT-BUF FS-PATH-CAP allot
create JFT-IN-BUF FS-PATH-CAP allot
create JFT-LONG-BUF JFT-LONG-CAP allot

: JFT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: JFT-ROOT$ ( -- ptr u8 n )
   JFT-ROOT-BUF JFT-ROOT-U @ ;

: JFT-IN$ ( -- ptr u8 n )
   JFT-IN-BUF JFT-IN-U @ ;

: JFT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-json-file" TMPDIR-MKDIR {: a:ptr u :}
   a u JFT-ROOT-BUF JFT-ROOT-U JFT-COPY!
   JFT-ROOT$ CLEANUP-TREE+
   JFT-ROOT$ s" rows.jsonl" JFT-IN-BUF JOIN-PATH JFT-IN-U !
   JFT-IN$ CLEANUP+ ;

: JFT-DQ ( -- )
   34 SB-APPEND-C ;

: JFT-LF ( -- )
   10 SB-APPEND-C ;

: JFT-OBJ-A ( -- )
   123 SB-APPEND-C
   JFT-DQ s" a" SB-APPEND JFT-DQ
   s" :1}" SB-APPEND ;

: JFT-OBJ-B ( -- )
   123 SB-APPEND-C
   JFT-DQ s" b" SB-APPEND JFT-DQ
   s" :true}" SB-APPEND ;

: JFT-MIXED$ ( -- ptr u8 n )
   SB-RESET
   JFT-OBJ-A JFT-LF
   JFT-LF
   s" bad" SB-APPEND JFT-LF
   JFT-OBJ-B
   SB$ ;

: JFT-LONG+C ( n -- ) {: c :}
   JFT-LONG-U @ 1+ JFT-LONG-CAP > IF s" json-file-test: long buffer full" 1 die THEN
   c JFT-LONG-BUF JFT-LONG-U @ + c!
   JFT-LONG-U @ 1+ JFT-LONG-U ! ;

: JFT-LONG+ ( ptr u8 n -- ) {: a:ptr u :}
   JFT-LONG-U @ u + JFT-LONG-CAP > IF s" json-file-test: long buffer full" 1 die THEN
   a JFT-LONG-BUF JFT-LONG-U @ + u BYTE-COPY
   JFT-LONG-U @ u + JFT-LONG-U ! ;

: JFT-LONG$ ( -- ptr u8 n )
   JFT-LONG-BUF JFT-LONG-U @ ;

: JFT-LONG-DQ ( -- )
   34 JFT-LONG+C ;

: JFT-BUILD-LONG ( -- ptr u8 n )
   0 JFT-LONG-U !
   123 JFT-LONG+C
   JFT-LONG-DQ s" big" JFT-LONG+ JFT-LONG-DQ
   58 JFT-LONG+C
   JFT-LONG-DQ
   0 begin dup JFT-LONG-N < while
      97 JFT-LONG+C
      1+
   repeat drop
   JFT-LONG-DQ
   125 JFT-LONG+C
   JFT-LONG$ ;

: JFT-WRITE-MIXED ( -- )
   JFT-IN$ JFT-MIXED$ WRITE-ALL ;

: JFT-WRITE-LONG ( -- )
   JFT-IN$ JFT-BUILD-LONG WRITE-ALL ;

: JFT-OPEN ( -- )
   JFT-IN$ JSONLF-OPEN ;

: JFT-ASSERT-EOF ( -- )
   JSONLF-NEXT-ROW 0= TTRUE
   0 T=
   JSONL-ROW-EOF T=
   -1 T= ;

: TEST-JSONLF-MIXED ( -- )
   JFT-WRITE-MIXED
   JFT-OPEN
   JSONLF-NEXT-ROW TTRUE
   0 T=
   JSONL-ROW-JSON T=
   dup s" a" JSON-GET JSON-NUMBER$ s" 1" T$=
   drop
   JSONLF-LINE# 1 T=
   JSONLF-NEXT-ROW TTRUE
   0 T=
   JSONL-ROW-BLANK T=
   -1 T=
   JSONLF-LINE# 2 T=
   JSONLF-NEXT-ROW TTRUE
   E-JSON-SYNTAX T=
   JSONL-ROW-ERROR T=
   -1 T=
   JSONL-LINE$ s" bad" T$=
   JSONLF-LINE# 3 T=
   JSONLF-NEXT-ROW TTRUE
   0 T=
   JSONL-ROW-JSON T=
   dup s" b" JSON-GET JSON-BOOL@ TTRUE
   drop
   JSONLF-LINE# 4 T=
   JFT-ASSERT-EOF ;

: TEST-JSONLF-LONG ( -- )
   JFT-WRITE-LONG
   JFT-OPEN
   JSONLF-NEXT-ROW TTRUE
   0 T=
   JSONL-ROW-JSON T=
   s" big" JSON-GET JSON-STRING$ JFT-LONG-N T=
   drop
   JSONLF-LINE-CAP JSONLF-LINE-BOOT-CAP > TTRUE
   JSONLF-LINE# 1 T=
   JFT-ASSERT-EOF ;

: JSON-FILE-TEST ( -- )
   T-RESET
   JFT-PREPARE
   TEST-JSONLF-MIXED
   TEST-JSONLF-LONG
   CLEANUP-RUN
   JFT-ROOT$ EXISTS? TFALSE
   T-REPORT
   s" json-file-test: ok" type cr ;

JSON-FILE-TEST
