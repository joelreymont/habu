\ check-all-errors.f - run the native checker over each top-level definition.
\ Load after tools/lint/lib.f, tools/lint/source-lex.f, and tools/argv.f.

0 set-check

$10000 constant CA-FILE-CAP
$20000 constant CA-PROG-CAP
$10000 constant CA-ERR-CAP
$400 constant CA-OUT-CAP
512 constant CA-DEF-MAX
32 constant CA-NUM-CAP

10 constant CA-LF
32 constant CA-SP
58 constant CA-COLON-C
123 constant CA-LBRACE
1 constant POLLIN
2 constant F-SETFD
1 constant FD-CLOEXEC

create CA-FILE-BUF CA-FILE-CAP allot
create CA-PROG-BUF CA-PROG-CAP allot
create CA-ERR-BUF CA-ERR-CAP allot
create CA-OUT-BUF CA-OUT-CAP allot
create CA-NUM-BUF CA-NUM-CAP allot
create CA-PFD 8 allot
create CA-LF-BUF 1 allot

create CA-DEF-START CA-DEF-MAX cells allot
create CA-DEF-END CA-DEF-MAX cells allot
create CA-DEF-LINE CA-DEF-MAX cells allot
create CA-DEF-COL CA-DEF-MAX cells allot
create CA-DEF-BYTE CA-DEF-MAX cells allot
create CA-DEF-OK CA-DEF-MAX cells allot

variable CA-DEF#
variable CA-I
variable CA-J
variable CA-K
variable CA-NUM-I
variable CA-PID
variable CA-RC
variable CA-FAILED
variable CA-RAW-FAILURE
variable CA-JSON-FOUND
variable CA-PROG-LEN

variable CA-IN-R
variable CA-IN-W
variable CA-OUT-R
variable CA-OUT-W
variable CA-ERR-R
variable CA-ERR-W
variable CA-ERR-LEN
variable CA-OUT-LEN
variable CA-GOT
variable CA-LS
variable CA-LE

variable CA-FILE-A
variable CA-FILE-U

: CA-CELL@ ( base k -- n )
   cells + @ ;

: CA-CELL! ( n base k -- )
   cells + ! ;

: CA-START@ ( k -- n ) CA-DEF-START swap CA-CELL@ ;
: CA-END@ ( k -- n ) CA-DEF-END swap CA-CELL@ ;
: CA-LINE@ ( k -- n ) CA-DEF-LINE swap CA-CELL@ ;
: CA-COL@ ( k -- n ) CA-DEF-COL swap CA-CELL@ ;
: CA-BYTE@ ( k -- n ) CA-DEF-BYTE swap CA-CELL@ ;
: CA-OK@ ( k -- n ) CA-DEF-OK swap CA-CELL@ ;

: CA-START! ( n k -- ) CA-DEF-START swap CA-CELL! ;
: CA-END! ( n k -- ) CA-DEF-END swap CA-CELL! ;
: CA-LINE! ( n k -- ) CA-DEF-LINE swap CA-CELL! ;
: CA-COL! ( n k -- ) CA-DEF-COL swap CA-CELL! ;
: CA-BYTE! ( n k -- ) CA-DEF-BYTE swap CA-CELL! ;
: CA-OK! ( n k -- ) CA-DEF-OK swap CA-CELL! ;

: CA-WRITE {: fd a u :} ( fd a u -- )
   u 0= IF exit THEN
   fd a u write u <> IF s" check-all-errors: write failed" 74 die THEN ;

: CA-ERR ( a u -- )
   2 -rot CA-WRITE ;

: CA-LF$ ( -- a u )
   CA-LF CA-LF-BUF c!
   CA-LF-BUF 1 ;

: CA-U$ {: u :} ( u -- a u )
   CA-NUM-CAP CA-NUM-I !
   u 0= IF
      CA-NUM-I @ 1- CA-NUM-I !
      48 CA-NUM-BUF CA-NUM-I @ + c!
      CA-NUM-BUF CA-NUM-I @ + 1
      exit
   THEN
   u begin dup 0 > while
      dup 10 mod 48 +
      CA-NUM-I @ 1- CA-NUM-I !
      CA-NUM-BUF CA-NUM-I @ + c!
      10 /
   repeat drop
   CA-NUM-BUF CA-NUM-I @ + CA-NUM-CAP CA-NUM-I @ - ;

: CA-PROG+ {: a u :} ( a u -- )
   CA-PROG-LEN @ u + CA-PROG-CAP > IF s" check-all-errors: generated program too large" 76 die THEN
   a CA-PROG-BUF CA-PROG-LEN @ + u BMOVE
   CA-PROG-LEN @ u + CA-PROG-LEN ! ;

: CA-PROG-C ( c -- )
   CA-LF-BUF c!
   CA-LF-BUF 1 CA-PROG+ ;

: CA-PROG-LN ( a u -- )
   CA-PROG+
   CA-LF CA-PROG-C ;

: CA-PROG-U ( u -- )
   CA-U$ CA-PROG+ ;

: CA-PFD! {: fd events :} ( fd events -- )
   events 32 lshift fd $FFFFFFFF and or CA-PFD ! ;

: CA-POLL-IN {: fd ms :} ( fd ms -- rc )
   fd POLLIN CA-PFD!
   CA-PFD 1 ms poll ;

: CA-CLOEXEC {: fd :} ( fd -- )
   fd F-SETFD FD-CLOEXEC fcntl drop ;

: CA-MKPIPE {: rvar wvar :} ( rvar wvar -- )
   pipe 0 <> IF s" check-all-errors: pipe failed" 74 die THEN
   wvar !
   rvar ! ;

: CA-DRAIN-FD {: fd buf cap lenp :} ( fd buf cap lenp -- )
   0 lenp !
   begin fd 0 CA-POLL-IN 0 > while
      fd buf lenp @ + cap lenp @ - read CA-GOT !
      CA-GOT @ 0 > IF
         lenp @ CA-GOT @ + lenp !
      ELSE
         exit
      THEN
   repeat ;

: CA-TOK-WORD? {: k :} ( k -- f )
   k L# @ >= IF 0 exit THEN
   k LK@ L-WORD = ;

: CA-TOK= {: k a u :} ( k a u -- f )
   k CA-TOK-WORD? 0= IF 0 exit THEN
   k LTOK a u STR= ;

: CA-PARSE-NEXT? {: k :} ( k -- f )
   k s" char" CA-TOK= IF -1 exit THEN
   k s" [char]" CA-TOK= ;

: CA-ORIGIN! {: src dst :} ( src dst -- )
   src 1+ CA-TOK-WORD? IF
      src 1+ LL@ dst CA-LINE!
      src 1+ LC@ dst CA-COL!
      src 1+ LB@ dst CA-BYTE!
   ELSE
      src LL@ dst CA-LINE!
      src LC@ dst CA-COL!
      src LB@ dst CA-BYTE!
   THEN ;

: CA-ADD-DEF {: start end tok :} ( start end tok -- )
   CA-DEF# @ CA-DEF-MAX >= IF s" check-all-errors: too many definitions" 76 die THEN
   start CA-DEF# @ CA-START!
   end CA-DEF# @ CA-END!
   tok CA-DEF# @ CA-ORIGIN!
   0 CA-DEF# @ CA-OK!
   CA-DEF# @ 1+ CA-DEF# ! ;

: CA-COLLECT-DEFS ( -- )
   0 CA-DEF# !
   0 CA-I !
   begin CA-I @ L# @ < while
      CA-I @ s" :" CA-TOK= IF
         CA-I @ 1+ CA-J !
         begin CA-J @ L# @ < while
            CA-J @ CA-PARSE-NEXT? IF
               CA-J @ 2 + CA-J !
            ELSE CA-J @ s" ;" CA-TOK= IF
               CA-I @ LB@
               CA-J @ LB@ CA-J @ LTOK nip +
               CA-I @ CA-ADD-DEF
               CA-J @ CA-I !
               L# @ CA-J !
            ELSE
               CA-J @ 1+ CA-J !
            THEN THEN
         repeat
      THEN
      CA-I @ 1+ CA-I !
   repeat ;

: CA-SLICE$ {: start end :} ( start end -- a u )
   CA-FILE-BUF start + end start - ;

: CA-PROG-SLICE ( start end -- )
   CA-SLICE$ CA-PROG+ ;

: CA-PROG-PREFIX ( -- )
   s" 0 set-check" CA-PROG-LN
   s" s" CA-PROG+
   34 CA-PROG-C
   CA-SP CA-PROG-C
   CA-FILE-A @ CA-FILE-U @ CA-PROG+
   34 CA-PROG-C
   s"  DIAG-FILE!" CA-PROG-LN
   ARGV-JSON? IF s" -1 JSON-DIAGS !" CA-PROG-LN THEN
   s" : CHECK-SH-HOOK ( n n -- n )" CA-PROG-LN
   s"    CHECK! dup -1 <> IF 70 throw THEN ;" CA-PROG-LN
   s" ' CHECK-SH-HOOK set-check" CA-PROG-LN ;

: CA-PROG-ACCEPTED {: upto :} ( upto -- )
   0 begin dup upto < while
      dup CA-OK@ IF
         dup CA-START@ over CA-END@ CA-PROG-SLICE
         CA-LF CA-PROG-C
      THEN
      1+
   repeat drop ;

: CA-PROG-ORIGIN {: k :} ( k -- )
   k CA-LINE@ CA-PROG-U  CA-SP CA-PROG-C
   k CA-COL@ CA-PROG-U   CA-SP CA-PROG-C
   k CA-BYTE@ CA-PROG-U
   s"  DIAG-ORIGIN!" CA-PROG-LN ;

: CA-BUILD-PROGRAM {: k :} ( k -- )
   0 CA-PROG-LEN !
   CA-PROG-PREFIX
   k CA-PROG-ACCEPTED
   k CA-PROG-ORIGIN
   k CA-START@ k CA-END@ CA-PROG-SLICE ;

: CA-SPAWN-HB {: k :} ( k -- rc )
   CA-IN-R CA-IN-W CA-MKPIPE
   CA-OUT-R CA-OUT-W CA-MKPIPE
   CA-ERR-R CA-ERR-W CA-MKPIPE
   CA-IN-W @ CA-CLOEXEC
   CA-OUT-R @ CA-CLOEXEC
   CA-ERR-R @ CA-CLOEXEC
   s" bin/hb" PATHZ PATHBUF CA-IN-R @ CA-OUT-W @ CA-ERR-W @ spawn-io CA-PID !
   CA-IN-R @ close
   CA-OUT-W @ close
   CA-ERR-W @ close
   k CA-BUILD-PROGRAM
   CA-IN-W @ CA-PROG-BUF CA-PROG-LEN @ CA-WRITE
   CA-IN-W @ close
   CA-PID @ wait-rc CA-RC !
   CA-OUT-R @ CA-OUT-BUF CA-OUT-CAP CA-OUT-LEN CA-DRAIN-FD
   CA-ERR-R @ CA-ERR-BUF CA-ERR-CAP CA-ERR-LEN CA-DRAIN-FD
   CA-OUT-R @ close
   CA-ERR-R @ close
   CA-RC @ ;

: CA-JSON-LINE? ( a u -- f )
   TRIM dup 0= IF 2drop 0 exit THEN
   over c@ CA-LBRACE = ;

: CA-ERR-LINE {: start end :} ( start end -- a u )
   CA-ERR-BUF start + end start - ;

: CA-EMIT-ERR-LINE {: start end :} ( start end -- )
   start end CA-ERR-LINE TRIM CA-ERR
   CA-LF$ CA-ERR ;

: CA-FILTER-JSON ( -- )
   0 CA-JSON-FOUND !
   0 CA-LS !
   0 CA-LE !
   begin CA-LE @ CA-ERR-LEN @ < while
      CA-ERR-BUF CA-LE @ + c@ CA-LF = IF
         CA-LS @ CA-LE @ CA-ERR-LINE CA-JSON-LINE? IF
            CA-LS @ CA-LE @ CA-EMIT-ERR-LINE
            -1 CA-JSON-FOUND !
         THEN
         CA-LE @ 1+ CA-LS !
      THEN
      CA-LE @ 1+ CA-LE !
   repeat
   CA-LS @ CA-ERR-LEN @ < IF
      CA-LS @ CA-ERR-LEN @ CA-ERR-LINE CA-JSON-LINE? IF
         CA-LS @ CA-ERR-LEN @ CA-EMIT-ERR-LINE
         -1 CA-JSON-FOUND !
      THEN
   THEN ;

: CA-HANDLE-FAIL {: rc :} ( rc -- )
   -1 CA-FAILED !
   ARGV-JSON? IF
      CA-FILTER-JSON
      CA-JSON-FOUND @ 0= IF
         CA-ERR-BUF CA-ERR-LEN @ CA-ERR
         rc CA-RAW-FAILURE !
      THEN
   ELSE
      CA-ERR-BUF CA-ERR-LEN @ CA-ERR
   THEN ;

: CA-RUN-DEFS ( -- )
   0 CA-FAILED !
   0 CA-RAW-FAILURE !
   0 CA-K !
   begin CA-K @ CA-DEF# @ < while
      CA-K @ CA-SPAWN-HB dup 0= IF
         drop -1 CA-K @ CA-OK!
      ELSE
         CA-HANDLE-FAIL
      THEN
      CA-K @ 1+ CA-K !
   repeat ;

: CHECK-ALL-ERRORS ( -- )
   s" tools/check-all-errors.f [--json-errors] --label name source" ARGV-USAGE!
   ARGV-PARSE
   ARGV-REQUIRE-LABEL
   1 ARGV-EXPECT-POS-EXACT
   ARGV-LABEL$ CA-FILE-U ! CA-FILE-A !
   0 ARGV-POS$ CA-FILE-BUF CA-FILE-CAP READ-FILE LEX-SOURCE
   CA-COLLECT-DEFS
   CA-RUN-DEFS
   CA-RAW-FAILURE @ IF CA-RAW-FAILURE @ throw THEN
   CA-FAILED @ IF 70 throw THEN ;

CHECK-ALL-ERRORS
