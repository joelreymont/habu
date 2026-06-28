\ check-all-errors-core.f - reusable all-errors checker core.
\ Load after lib/string.f, lib/memory.f, lib/vector.f, lib/fs.f,
\ lib/process.f, lib/process-argv.f,
\ tools/lint/text.f, tools/lint/token.f, tools/lint/lib.f,
\ tools/lint/json-writer.f, and tools/lint/source-lex.f.

$10000 constant CA-PROG-EXTRA
$10000 constant CA-DEFAULT-ERR-CAP
$400 constant CA-DEFAULT-OUT-CAP
512 constant CA-INIT-CAP
32 constant CA-NUM-CAP
128 constant CA-RUN-NAME-CAP
256 constant CA-RUN-PATH-CAP
120000 constant CA-TIMEOUT-MS

10 constant CA-LF
32 constant CA-SP
58 constant CA-COLON-C
123 constant CA-LBRACE

create CA-NUM-BUF CA-NUM-CAP allot
create CA-LF-BUF 1 allot
create CA-RUN-NAME CA-RUN-NAME-CAP allot
create CA-RUN-PATH-BUF CA-RUN-PATH-CAP allot

create CA-DEF-START VEC-HEADER-CELLS cells allot
create CA-DEF-END VEC-HEADER-CELLS cells allot
create CA-DEF-TOK VEC-HEADER-CELLS cells allot
create CA-DEF-LINE VEC-HEADER-CELLS cells allot
create CA-DEF-COL VEC-HEADER-CELLS cells allot
create CA-DEF-BYTE VEC-HEADER-CELLS cells allot
create CA-DEF-OK VEC-HEADER-CELLS cells allot
create CA-SUP-START VEC-HEADER-CELLS cells allot
create CA-SUP-END VEC-HEADER-CELLS cells allot

variable CA-DEF#
variable CA-SUP#
variable CA-I
variable CA-J
variable CA-K
variable CA-NUM-I
variable CA-RC
variable CA-FAILED
variable CA-RAW-FAILURE
variable CA-JSON-FOUND
variable CA-PROG-LEN
variable CA-PROG-A
variable CA-PROG-CAP
variable CA-SRC-A
variable CA-SRC-U
variable CA-SRC-CAP
variable CA-RAW-A
variable CA-RAW-U
variable CA-MATCH-TOK
variable CA-MATCH-ORD
variable CA-ORD
variable CA-ALL-DEFS
variable CA-ERR-LEN
variable CA-OUT-LEN
variable CA-ERR-A
variable CA-ERR-CAP
variable CA-OUT-A
variable CA-OUT-CAP
variable CA-RUN-NAME-U
variable CA-RUN-PATH-U
variable CA-LS
variable CA-LE
variable CA-NEXT-D
variable CA-NEXT-S

variable CA-FILE-A
variable CA-FILE-U
variable CA-JSON

: CA-TRUE ( -- bool )
   0 0= ;

: CA-FALSE ( -- bool )
   CA-TRUE 0= ;

: CA-NOT ( bool -- bool )
   IF CA-FALSE ELSE CA-TRUE THEN ;

: CA-PROG-A-FIELD ( -- ptr ptr u8 )
   CA-PROG-A 0 ptr-field ;

: CA-PROG-A@ ( -- ptr u8 )
   CA-PROG-A-FIELD @ ;

: CA-PROG-A! ( ptr u8 -- )
   CA-PROG-A-FIELD ! ;

: CA-SRC-A-FIELD ( -- ptr ptr u8 )
   CA-SRC-A 0 ptr-field ;

: CA-SRC-A@ ( -- ptr u8 )
   CA-SRC-A-FIELD @ ;

: CA-SRC-A! ( ptr u8 -- )
   CA-SRC-A-FIELD ! ;

: CA-RAW-A-FIELD ( -- ptr ptr u8 )
   CA-RAW-A 0 ptr-field ;

: CA-RAW-A@ ( -- ptr u8 )
   CA-RAW-A-FIELD @ ;

: CA-RAW-A! ( ptr u8 -- )
   CA-RAW-A-FIELD ! ;

: CA-FILE-A-FIELD ( -- ptr ptr u8 )
   CA-FILE-A 0 ptr-field ;

: CA-FILE-A@ ( -- ptr u8 )
   CA-FILE-A-FIELD @ ;

: CA-FILE-A! ( ptr u8 -- )
   CA-FILE-A-FIELD ! ;

: CA-ERR-A-FIELD ( -- ptr ptr u8 )
   CA-ERR-A 0 ptr-field ;

: CA-ERR-A@ ( -- ptr u8 )
   CA-ERR-A-FIELD @ ;

: CA-ERR-A! ( ptr u8 -- )
   CA-ERR-A-FIELD ! ;

: CA-OUT-A-FIELD ( -- ptr ptr u8 )
   CA-OUT-A 0 ptr-field ;

: CA-OUT-A@ ( -- ptr u8 )
   CA-OUT-A-FIELD @ ;

: CA-OUT-A! ( ptr u8 -- )
   CA-OUT-A-FIELD ! ;

: CHECK-ALL-ERRORS-BUFFERS! ( ptr u8 n ptr u8 n -- ) {: outa:ptr outcap erra:ptr errcap :}
   outcap CA-OUT-CAP !
   outa CA-OUT-A!
   errcap CA-ERR-CAP !
   erra CA-ERR-A! ;

: CHECK-ALL-ERRORS-JSON! ( bool -- )
   CA-JSON ! ;

: CA-JSON? ( -- bool )
   CA-JSON @ 0 <> ;

: CA-RUN-PATH ( -- ptr u8 n )
   CA-RUN-PATH-BUF CA-RUN-PATH-U @ ;

: CA-FAIL ( ptr u8 n n -- )
   die ;

: CA-CELL@ ( ptr a n -- n )
   >IDX VEC-N@ ;

: CA-CELL! ( n ptr a n -- )
   >IDX VEC-N! ;

: CA-DEF-INIT ( -- )
   CA-DEF-START CA-INIT-CAP >COUNT VEC-INIT
   CA-DEF-END CA-INIT-CAP >COUNT VEC-INIT
   CA-DEF-TOK CA-INIT-CAP >COUNT VEC-INIT
   CA-DEF-LINE CA-INIT-CAP >COUNT VEC-INIT
   CA-DEF-COL CA-INIT-CAP >COUNT VEC-INIT
   CA-DEF-BYTE CA-INIT-CAP >COUNT VEC-INIT
   CA-DEF-OK CA-INIT-CAP >COUNT VEC-INIT ;

: CA-SUP-INIT ( -- )
   CA-SUP-START CA-INIT-CAP >COUNT VEC-INIT
   CA-SUP-END CA-INIT-CAP >COUNT VEC-INIT ;

: CA-STORE-INIT ( -- )
   CA-DEF-INIT
   CA-SUP-INIT ;

: CA-DEF-CLEAR ( -- )
   CA-DEF-START VEC-CLEAR
   CA-DEF-END VEC-CLEAR
   CA-DEF-TOK VEC-CLEAR
   CA-DEF-LINE VEC-CLEAR
   CA-DEF-COL VEC-CLEAR
   CA-DEF-BYTE VEC-CLEAR
   CA-DEF-OK VEC-CLEAR ;

: CA-SUP-CLEAR ( -- )
   CA-SUP-START VEC-CLEAR
   CA-SUP-END VEC-CLEAR ;

: CA-STORE-CLEAR ( -- )
   CA-DEF-CLEAR
   CA-SUP-CLEAR ;

: CA-DEF-ENSURE ( n -- ) {: count :}
   CA-DEF-START count >COUNT VEC-ENSURE
   CA-DEF-END count >COUNT VEC-ENSURE
   CA-DEF-TOK count >COUNT VEC-ENSURE
   CA-DEF-LINE count >COUNT VEC-ENSURE
   CA-DEF-COL count >COUNT VEC-ENSURE
   CA-DEF-BYTE count >COUNT VEC-ENSURE
   CA-DEF-OK count >COUNT VEC-ENSURE ;

: CA-SUP-ENSURE ( n -- ) {: count :}
   CA-SUP-START count >COUNT VEC-ENSURE
   CA-SUP-END count >COUNT VEC-ENSURE ;

: CA-DEF-LEN! ( n -- ) {: count :}
   count >LEN CA-DEF-START VEC-LEN!
   count >LEN CA-DEF-END VEC-LEN!
   count >LEN CA-DEF-TOK VEC-LEN!
   count >LEN CA-DEF-LINE VEC-LEN!
   count >LEN CA-DEF-COL VEC-LEN!
   count >LEN CA-DEF-BYTE VEC-LEN!
   count >LEN CA-DEF-OK VEC-LEN! ;

: CA-SUP-LEN! ( n -- ) {: count :}
   count >LEN CA-SUP-START VEC-LEN!
   count >LEN CA-SUP-END VEC-LEN! ;

: CA-DEF-ROOM ( n -- )
   1+ dup CA-DEF-ENSURE CA-DEF-LEN! ;

: CA-SUP-ROOM ( n -- )
   1+ dup CA-SUP-ENSURE CA-SUP-LEN! ;

: CA-START@ ( k -- n ) CA-DEF-START swap CA-CELL@ ;
: CA-END@ ( k -- n ) CA-DEF-END swap CA-CELL@ ;
: CA-DEFTOK@ ( k -- n ) CA-DEF-TOK swap CA-CELL@ ;
: CA-LINE@ ( k -- n ) CA-DEF-LINE swap CA-CELL@ ;
: CA-COL@ ( k -- n ) CA-DEF-COL swap CA-CELL@ ;
: CA-BYTE@ ( k -- n ) CA-DEF-BYTE swap CA-CELL@ ;
: CA-OK@ ( k -- n ) CA-DEF-OK swap CA-CELL@ ;
: CA-SUP-START@ ( k -- n ) CA-SUP-START swap CA-CELL@ ;
: CA-SUP-END@ ( k -- n ) CA-SUP-END swap CA-CELL@ ;

: CA-START! ( n k -- ) CA-DEF-START swap CA-CELL! ;
: CA-END! ( n k -- ) CA-DEF-END swap CA-CELL! ;
: CA-DEFTOK! ( n k -- ) CA-DEF-TOK swap CA-CELL! ;
: CA-LINE! ( n k -- ) CA-DEF-LINE swap CA-CELL! ;
: CA-COL! ( n k -- ) CA-DEF-COL swap CA-CELL! ;
: CA-BYTE! ( n k -- ) CA-DEF-BYTE swap CA-CELL! ;
: CA-OK! ( n k -- ) CA-DEF-OK swap CA-CELL! ;
: CA-SUP-START! ( n k -- ) CA-SUP-START swap CA-CELL! ;
: CA-SUP-END! ( n k -- ) CA-SUP-END swap CA-CELL! ;

: CA-WRITE ( n ptr u8 n -- ) {: fd a:ptr u :}
   u 0= IF exit THEN
   fd a u write u <> IF s" check-all-errors: write failed" 74 CA-FAIL THEN ;

: CA-ERR ( ptr u8 n -- )
   2 -rot CA-WRITE ;

: CA-LF$ ( -- ptr u8 n )
   CA-LF CA-LF-BUF c!
   CA-LF-BUF 1 ;

: CA-U$ ( n -- ptr u8 n ) {: u :}
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

: CA-PROG+ ( ptr u8 n -- ) {: a:ptr u :}
   CA-PROG-LEN @ u + CA-PROG-CAP @ > IF s" check-all-errors: generated program too large" 76 CA-FAIL THEN
   a CA-PROG-A@ CA-PROG-LEN @ + u BYTE-COPY
   CA-PROG-LEN @ u + CA-PROG-LEN ! ;

: CA-PROG-C ( c -- )
   CA-LF-BUF c!
   CA-LF-BUF 1 CA-PROG+ ;

: CA-PROG-LN ( a u -- )
   CA-PROG+
   CA-LF CA-PROG-C ;

: CA-PROG-U ( u -- )
   CA-U$ CA-PROG+ ;

: CA-NAME+ ( ptr u8 n -- ) {: a:ptr u :}
   CA-RUN-NAME-U @ u + CA-RUN-NAME-CAP > IF s" check-all-errors: run name too large" 76 CA-FAIL THEN
   a CA-RUN-NAME CA-RUN-NAME-U @ + u BYTE-COPY
   CA-RUN-NAME-U @ u + CA-RUN-NAME-U ! ;

: CA-COPY-RUN-PATH! ( ptr u8 n -- ) {: a:ptr u :}
   u CA-RUN-PATH-CAP > IF s" check-all-errors: run path too large" 76 CA-FAIL THEN
   a CA-RUN-PATH-BUF u BYTE-COPY
   u CA-RUN-PATH-U ! ;

: CA-MAKE-RUN-PATH ( -- )
   0 CA-RUN-NAME-U !
   s" habu-check-all-" CA-NAME+
   mono-ns CA-U$ CA-NAME+
   s" .f" CA-NAME+
   CA-RUN-NAME CA-RUN-NAME-U @ TMP-PATH CA-COPY-RUN-PATH! ;

: CA-ARGV+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: CA-TOK-WORD? ( n -- bool ) {: k :}
   k L# @ >= IF CA-FALSE exit THEN
   k LK@ L-WORD = ;

: CA-TOK= ( n ptr u8 n -- bool ) {: k a:ptr u :}
   k CA-TOK-WORD? CA-NOT IF CA-FALSE exit THEN
   k LEX-TOK a u LINT-STR= ;

: CA-TOK-CI= ( n ptr u8 n -- bool ) {: k a:ptr u :}
   k CA-TOK-WORD? CA-NOT IF CA-FALSE exit THEN
   k LEX-TOK a u LINT-STR=CI ;

: CA-PARSE-NEXT? ( n -- bool ) {: k :}
   k s" char" CA-TOK= IF CA-TRUE exit THEN
   k s" [char]" CA-TOK= ;

: CA-SRC-C@ ( n -- c )
   CA-SRC-A@ + c@ ;

: CA-TOK-END-BYTE {: k :} ( k -- n )
   k LB@ k LEX-TOK nip + ;

: CA-LINE-START-BYTE ( n -- n )
   begin dup 0 > while
      dup 1- CA-SRC-C@ CA-LF = IF exit THEN
      1-
   repeat ;

: CA-LINE-END-BYTE ( n -- n )
   begin dup CA-SRC-U @ < while
      dup CA-SRC-C@ CA-LF = IF exit THEN
      1+
   repeat ;

: CA-LINE-SEG-START ( n -- n ) {: k :}
   k LB@ CA-LINE-START-BYTE CA-LS !
   k 1- CA-J !
   begin CA-J @ 0 >= while
      CA-J @ LL@ k LL@ <> IF CA-LS @ exit THEN
      CA-J @ s" ;" CA-TOK= IF
         CA-J @ CA-TOK-END-BYTE CA-LS !
         CA-LS @ exit
      THEN
      CA-J @ 1- CA-J !
   repeat
   CA-LS @ ;

: CA-LINE-SAFE-END ( n -- n ) {: k :}
   k LB@ CA-LINE-END-BYTE CA-LE !
   k 1+ CA-J !
   begin CA-J @ L# @ < while
      CA-J @ LL@ k LL@ <> IF CA-LE @ exit THEN
      CA-J @ s" :" CA-TOK= IF CA-J @ LB@ exit THEN
      CA-J @ 1+ CA-J !
   repeat
   CA-LE @ ;

: CA-LAST-TOK-BEFORE ( n n -- n ) {: k end :}
   k CA-J !
   begin CA-J @ 1+ L# @ < while
      CA-J @ 1+ LB@ end < IF
         CA-J @ 1+ CA-J !
      ELSE
         CA-J @ exit
      THEN
   repeat
   CA-J @ ;

: CA-ADD-SUPPORT ( n n -- ) {: start end :}
   end start <= IF exit THEN
   CA-SUP# @ CA-SUP-ROOM
   start CA-SUP# @ CA-SUP-START!
   end CA-SUP# @ CA-SUP-END!
   CA-SUP# @ 1+ CA-SUP# ! ;

: CA-ADD-SUPPORT-LINE ( n -- ) {: k :}
   k CA-LINE-SEG-START
   k CA-LINE-SAFE-END
   CA-ADD-SUPPORT ;

: CA-ADD-SUPPORT-PAIR ( n -- ) {: k :}
   k 1+ L# @ >= IF exit THEN
   k LB@
   k 1+ CA-TOK-END-BYTE
   CA-ADD-SUPPORT
   k 1+ CA-I ! ;

: CA-ADD-SUPPORT-CONSTANT ( n -- ) {: k :}
   k 1+ L# @ >= IF exit THEN
   k CA-LINE-SEG-START
   k 1+ CA-TOK-END-BYTE
   CA-ADD-SUPPORT
   k 1+ CA-I ! ;

: CA-FIND-SEMI ( n -- n ) {: k :}
   k 1+ CA-J !
   begin CA-J @ L# @ < while
      CA-J @ CA-PARSE-NEXT? IF
         CA-J @ 2 + CA-J !
      ELSE CA-J @ s" ;" CA-TOK= IF
         CA-J @ exit
      ELSE
         CA-J @ 1+ CA-J !
      THEN THEN
   repeat
   L# @ ;

: CA-ADD-SUPPORT-TRUSTED ( n -- ) {: k :}
   k CA-FIND-SEMI dup L# @ >= IF drop exit THEN
   k LB@ swap CA-TOK-END-BYTE CA-ADD-SUPPORT
   CA-J @ CA-I ! ;

: CA-ADD-SUPPORT-TRUST ( n -- ) {: k :}
   k CA-LINE-SEG-START
   k CA-TOK-END-BYTE
   CA-ADD-SUPPORT ;

: CA-ORIGIN! ( n n -- ) {: src dst :}
   src 1+ CA-TOK-WORD? IF
      src 1+ LL@ dst CA-LINE!
      src 1+ LC@ dst CA-COL!
      src 1+ LB@ dst CA-BYTE!
   ELSE
      src LL@ dst CA-LINE!
      src LC@ dst CA-COL!
      src LB@ dst CA-BYTE!
   THEN ;

: CA-ADD-DEF ( n n n -- ) {: start end tok :}
   CA-DEF# @ CA-DEF-ROOM
   start CA-DEF# @ CA-START!
   end CA-DEF# @ CA-END!
   tok CA-DEF# @ CA-DEFTOK!
   tok CA-DEF# @ CA-ORIGIN!
   0 CA-DEF# @ CA-OK!
   CA-DEF# @ 1+ CA-DEF# ! ;

: CA-ADD-DEF-RANGE ( n n -- ) {: k semi :}
   k LB@ semi CA-TOK-END-BYTE k CA-ADD-DEF ;

: CA-COLLECT-DEF ( n -- ) {: k :}
   k CA-FIND-SEMI dup L# @ >= IF drop exit THEN
   dup k swap CA-ADD-DEF-RANGE
   CA-I ! ;

: CA-COLLECT-CREATE ( n -- ) {: k:n :}
   k CA-ADD-SUPPORT-LINE
   k k CA-LINE-SAFE-END CA-LAST-TOK-BEFORE CA-I ! ;

: CA-COLLECT-DEFER ( n -- ) {: k:n :}
   k CA-ADD-SUPPORT-LINE
   k k CA-LINE-SAFE-END CA-LAST-TOK-BEFORE CA-I ! ;

: CA-COLLECT-UNDEFINE ( n -- ) {: k:n :}
   k CA-ADD-SUPPORT-LINE
   k k CA-LINE-SAFE-END CA-LAST-TOK-BEFORE CA-I ! ;

: CA-COLLECT-SUPPORT ( n -- ) {: k:n :}
   k s" TRUSTED:" CA-TOK-CI= IF k CA-ADD-SUPPORT-TRUSTED exit THEN
   k s" defer" CA-TOK-CI= IF k CA-COLLECT-DEFER exit THEN
   k s" undefine" CA-TOK-CI= IF k CA-COLLECT-UNDEFINE exit THEN
   k s" create" CA-TOK-CI= IF k CA-COLLECT-CREATE exit THEN
   k s" variable" CA-TOK-CI= IF k CA-ADD-SUPPORT-PAIR exit THEN
   k s" constant" CA-TOK-CI= IF k CA-ADD-SUPPORT-CONSTANT exit THEN
   k s" TRUST" CA-TOK-CI= IF k CA-ADD-SUPPORT-TRUST exit THEN ;

: CA-COLLECT-ONE ( -- )
   CA-I @ s" :" CA-TOK= IF
      CA-I @ CA-COLLECT-DEF
   ELSE
      CA-I @ CA-COLLECT-SUPPORT
   THEN ;

: CA-COLLECT-RESET ( -- )
   0 CA-DEF# !
   0 CA-SUP# !
   CA-STORE-CLEAR
   0 CA-I ! ;

: CA-COLLECT-DEFS ( -- )
   CA-COLLECT-RESET
   begin CA-I @ L# @ < while
      CA-COLLECT-ONE
      CA-I @ 1+ CA-I !
   repeat ;

: CA-SLICE$ ( n n -- ptr u8 n ) {: start end :}
   CA-SRC-A@ start + end start - ;

: CA-PROG-SLICE ( n n -- )
   CA-SLICE$ CA-PROG+ ;

: CA-PROG-PREFIX ( -- )
   s" 0 set-check" CA-PROG-LN
   s" s" CA-PROG+
   34 CA-PROG-C
   CA-SP CA-PROG-C
   CA-FILE-A@ CA-FILE-U @ CA-PROG+
   34 CA-PROG-C
   s"  DIAG-FILE!" CA-PROG-LN
   CA-JSON? IF s" -1 JSON-DIAGS !" CA-PROG-LN THEN
   s" : CHECK-SH-HOOK ( ptr u8 n -- n )" CA-PROG-LN
   s"    CHECK! dup -1 <> IF 70 throw THEN ;" CA-PROG-LN
   s" ' CHECK-SH-HOOK set-check" CA-PROG-LN ;

: CA-INF ( -- n )
   CA-SRC-U @ 1+ ;

: CA-NEXT-DEF ( n -- n ) {: limit :}
   CA-I @ CA-DEF# @ < IF
      CA-I @ CA-START@ limit < IF CA-I @ CA-START@ exit THEN
   THEN
   CA-INF ;

: CA-NEXT-SUP ( n -- n ) {: limit :}
   CA-J @ CA-SUP# @ < IF
      CA-J @ CA-SUP-START@ limit < IF CA-J @ CA-SUP-START@ exit THEN
   THEN
   CA-INF ;

: CA-PROG-DEF-I ( -- )
   CA-ALL-DEFS @ 0 <> CA-I @ CA-OK@ 0 <> or IF
      CA-I @ CA-START@ CA-I @ CA-END@ CA-PROG-SLICE
      CA-LF CA-PROG-C
   THEN
   CA-I @ 1+ CA-I ! ;

: CA-PROG-SUP-J ( -- )
   CA-J @ CA-SUP-START@ CA-J @ CA-SUP-END@ CA-PROG-SLICE
   CA-LF CA-PROG-C
   CA-J @ 1+ CA-J ! ;

: CA-PROG-CONTEXT-LIMIT ( n -- ) {: limit :}
   0 CA-I !
   0 CA-J !
   begin
      limit CA-NEXT-DEF CA-NEXT-D !
      limit CA-NEXT-SUP CA-NEXT-S !
      CA-NEXT-D @ CA-INF < CA-NEXT-S @ CA-INF < or
   while
      CA-NEXT-D @ CA-NEXT-S @ <= IF
         CA-PROG-DEF-I
      ELSE
         CA-PROG-SUP-J
      THEN
   repeat ;

: CA-PROG-CONTEXT ( n -- ) {: k :}
   k CA-START@ CA-PROG-CONTEXT-LIMIT ;

: CA-PROG-ORIGIN ( n -- ) {: k :}
   k CA-LINE@ CA-PROG-U  CA-SP CA-PROG-C
   k CA-COL@ CA-PROG-U   CA-SP CA-PROG-C
   k CA-BYTE@ CA-PROG-U
   s"  DIAG-ORIGIN!" CA-PROG-LN ;

: CA-BUILD-PROGRAM ( n -- ) {: k :}
   0 CA-PROG-LEN !
   CA-PROG-PREFIX
   k CA-PROG-CONTEXT
   k CA-PROG-ORIGIN
   k CA-START@ k CA-END@ CA-PROG-SLICE ;

: CA-BUILD-FULL-PROGRAM ( -- )
   0 CA-PROG-LEN !
   CA-PROG-PREFIX
   CA-TRUE CA-ALL-DEFS !
   CA-INF CA-PROG-CONTEXT-LIMIT
   CA-FALSE CA-ALL-DEFS ! ;

: CA-RUN-PROGRAM ( -- n )
   CA-MAKE-RUN-PATH
   CA-RUN-PATH CA-PROG-A@ CA-PROG-LEN @ WRITE-ALL
   PROC-ARGV-RESET
   s" --load" CA-ARGV+
   CA-RUN-PATH CA-ARGV+
   s" bin/hb" >LEN CA-OUT-A@ CA-OUT-CAP @ >LEN
   CA-ERR-A@ CA-ERR-CAP @ >LEN CA-TIMEOUT-MS >MS
   RUN-ARGV-CAPTURE {: outu erru rc :}
   outu LEN>N CA-OUT-LEN !
   erru LEN>N CA-ERR-LEN !
   rc RC>N CA-RC !
   CA-RUN-PATH FS-PATHZ unlink drop
   CA-RC @ ;

: CA-SPAWN-HB ( n -- n )
   CA-BUILD-PROGRAM
   CA-RUN-PROGRAM ;

: CA-SPAWN-HB-FULL ( -- n )
   CA-BUILD-FULL-PROGRAM
   CA-RUN-PROGRAM ;

: CA-JSON-LINE? ( ptr u8 n -- bool )
   LINT-TRIM dup 0= IF 2drop CA-FALSE exit THEN
   drop c@ CA-LBRACE = ;

: CA-ERR-LINE ( n n -- ptr u8 n ) {: start end :}
   CA-ERR-A@ start + end start - ;

: CA-EMIT-ERR-LINE ( n n -- ) {: start end :}
   start end CA-ERR-LINE LINT-TRIM CA-ERR
   CA-LF$ CA-ERR ;

: CA-WORD$ ( n -- ptr u8 n ) {: k :}
   k CA-DEFTOK@ 1+ LEX-TOK ;

: CA-DEF-SOURCE$ ( n -- ptr u8 n ) {: k :}
   CA-SRC-A@ k CA-DEFTOK@ 1+ LB@ +
   k CA-END@ 1- k CA-DEFTOK@ 1+ LB@ - ;

: CA-DECLARED$ ( n -- ptr u8 n bool ) {: k :}
   k CA-DEFTOK@ 2 + dup L# @ >= IF drop s" " CA-FALSE exit THEN
   dup LK@ L-COMMENT <> IF drop s" " CA-FALSE exit THEN
   LCONTENT LINT-TRIM CA-TRUE ;

: CA-BODY-START ( n -- n ) {: k :}
   k CA-DEFTOK@ 2 +
   begin dup L# @ < while
      dup LK@ L-COMMENT = IF 1+ ELSE exit THEN
   repeat ;

: CA-FIND-BODY-TOKEN ( n ptr u8 n -- n n bool ) {: k a:ptr u :}
   0 CA-ORD !
   k CA-BODY-START CA-J !
   begin CA-J @ L# @ < while
      CA-J @ LB@ k CA-END@ >= IF 0 0 CA-FALSE exit THEN
      CA-J @ CA-TOK-WORD? IF
         CA-J @ s" ;" CA-TOK= IF 0 0 CA-FALSE exit THEN
         CA-ORD @ 1+ CA-ORD !
         CA-J @ LEX-TOK a u LINT-STR= IF CA-J @ CA-ORD @ CA-TRUE exit THEN
      THEN
      CA-J @ 1+ CA-J !
   repeat
   0 0 CA-FALSE ;

: CA-JSON-EMPTY-FIELD ( ptr u8 n -- )
   LJW-KEY s" " LJW-STRING ;

: CA-JSON-UNDEF ( n n n -- ) {: k tok ord :}
   LJW-RESET
   LJW-OBJECT-START
   s" schema_version" LJW-KEY 1 LJW-U LJW-COMMA
   s" code" LJW-KEY s" E-UNDEFINED" LJW-STRING LJW-COMMA
   s" repair_class" LJW-KEY s" unknown_rejection" LJW-STRING LJW-COMMA
   s" verdict" LJW-KEY s" rejected" LJW-STRING LJW-COMMA
   s" word" LJW-KEY k CA-WORD$ LJW-STRING LJW-COMMA
   s" token" LJW-KEY tok LEX-TOK LJW-STRING LJW-COMMA
   s" token_index" LJW-KEY ord LJW-U LJW-COMMA
   s" file" LJW-KEY CA-FILE-A@ CA-FILE-U @ LJW-STRING LJW-COMMA
   s" line" LJW-KEY tok LL@ LJW-U LJW-COMMA
   s" column" LJW-KEY tok LC@ LJW-U LJW-COMMA
   s" byte_start" LJW-KEY tok LB@ LJW-U LJW-COMMA
   s" byte_end" LJW-KEY tok LB@ tok LEX-TOK nip + LJW-U LJW-COMMA
   s" definition_source" LJW-KEY k CA-DEF-SOURCE$ LJW-STRING LJW-COMMA
   k CA-DECLARED$ IF
      2dup s" declared_effect" LJW-KEY LJW-STRING LJW-COMMA
      s" declared_effect_source" LJW-KEY LJW-STRING LJW-COMMA
   ELSE
      2drop
   THEN
   s" inferred_effect" LJW-KEY s" unknown " LJW-STRING LJW-COMMA
   s" return_stack" LJW-KEY
   LJW-OBJECT-START
   s" expected" CA-JSON-EMPTY-FIELD LJW-COMMA
   s" actual" CA-JSON-EMPTY-FIELD
   LJW-OBJECT-END LJW-COMMA
   s" suggestion" LJW-KEY s" Inspect the token, signature, and raw stack evidence." LJW-STRING
   LJW-OBJECT-END
   LJW$ CA-ERR
   CA-LF$ CA-ERR ;

: CA-JSON-LEX-UNTERM ( -- )
   LJW-RESET
   LJW-OBJECT-START
   s" schema_version" LJW-KEY 1 LJW-U LJW-COMMA
   s" code" LJW-KEY s" E-UNTERMINATED-STRING" LJW-STRING LJW-COMMA
   s" repair_class" LJW-KEY s" fix_source" LJW-STRING LJW-COMMA
   s" verdict" LJW-KEY s" rejected" LJW-STRING LJW-COMMA
   s" token" LJW-KEY CA-SRC-A@ LEX-UNTERM-BYTE @ + 2 LJW-STRING LJW-COMMA
   s" file" LJW-KEY CA-FILE-A@ CA-FILE-U @ LJW-STRING LJW-COMMA
   s" line" LJW-KEY LEX-UNTERM-LINE @ LJW-U LJW-COMMA
   s" column" LJW-KEY LEX-UNTERM-COL @ LJW-U LJW-COMMA
   s" byte_start" LJW-KEY LEX-UNTERM-BYTE @ LJW-U LJW-COMMA
   s" byte_end" LJW-KEY LEX-UNTERM-BYTE @ 2 + LJW-U LJW-COMMA
   s" suggestion" LJW-KEY s" Close the string literal before the definition ends." LJW-STRING
   LJW-OBJECT-END
   LJW$ CA-ERR
   CA-LF$ CA-ERR ;

: CA-HANDLE-LEX-UNTERM ( -- )
   LEX-UNTERM-QUOTE? 0= IF exit THEN
   CA-JSON? IF
      CA-JSON-LEX-UNTERM
   ELSE
      s" E-UNTERMINATED-STRING" CA-ERR
      CA-LF$ CA-ERR
   THEN
   70 throw ;

: CA-TRY-RAW-JSON ( n -- bool ) {: k :}
   CA-ERR-A@ CA-ERR-LEN @ LINT-TRIM CA-RAW-U ! CA-RAW-A!
   CA-RAW-U @ 0= IF CA-FALSE exit THEN
   k CA-RAW-A@ CA-RAW-U @ CA-FIND-BODY-TOKEN IF
      CA-MATCH-ORD ! CA-MATCH-TOK !
      k CA-MATCH-TOK @ CA-MATCH-ORD @ CA-JSON-UNDEF
      CA-TRUE
   ELSE
      2drop
      CA-FALSE
   THEN ;

: CA-FILTER-JSON ( -- )
   CA-FALSE CA-JSON-FOUND !
   0 CA-LS !
   0 CA-LE !
   begin CA-LE @ CA-ERR-LEN @ < while
      CA-ERR-A@ CA-LE @ + c@ CA-LF = IF
         CA-LS @ CA-LE @ CA-ERR-LINE CA-JSON-LINE? IF
            CA-LS @ CA-LE @ CA-EMIT-ERR-LINE
            CA-TRUE CA-JSON-FOUND !
         THEN
         CA-LE @ 1+ CA-LS !
      THEN
      CA-LE @ 1+ CA-LE !
   repeat
   CA-LS @ CA-ERR-LEN @ < IF
      CA-LS @ CA-ERR-LEN @ CA-ERR-LINE CA-JSON-LINE? IF
         CA-LS @ CA-ERR-LEN @ CA-EMIT-ERR-LINE
         CA-TRUE CA-JSON-FOUND !
      THEN
   THEN ;

: CA-HANDLE-FAIL {: k rc :} ( k rc -- )
   CA-TRUE CA-FAILED !
   CA-JSON? IF
      CA-FILTER-JSON
      CA-JSON-FOUND @ 0= IF
         k CA-TRY-RAW-JSON CA-NOT IF
            CA-ERR-A@ CA-ERR-LEN @ CA-ERR
            rc CA-RAW-FAILURE !
         THEN
      THEN
   ELSE
      CA-ERR-A@ CA-ERR-LEN @ CA-ERR
   THEN ;

: CA-RUN-DEFS ( -- )
   CA-FALSE CA-FAILED !
   0 CA-RAW-FAILURE !
   CA-SPAWN-HB-FULL 0= IF exit THEN
   0 CA-K !
   begin CA-K @ CA-DEF# @ < while
      CA-K @ CA-SPAWN-HB dup 0= IF
         drop -1 CA-K @ CA-OK!
      ELSE
         CA-K @ swap CA-HANDLE-FAIL
      THEN
      CA-K @ 1+ CA-K !
   repeat ;

: CA-CHECK-PROG-NEED ( n -- n )
   CA-PROG-EXTRA + dup 0 <= IF s" check-all-errors: source too large" 76 CA-FAIL THEN ;

: CA-ALLOC-SOURCE ( n -- )
   dup MEM-ALLOC-64K-SPAN CA-SRC-CAP ! CA-SRC-A!
   CA-CHECK-PROG-NEED MEM-ALLOC-64K-SPAN CA-PROG-CAP ! CA-PROG-A! ;

: CA-READ-SOURCE ( ptr u8 n -- ) {: path:ptr pu :}
   path pu FILE-SIZE CA-ALLOC-SOURCE
   path pu CA-SRC-A@ CA-SRC-CAP @ READ-ALL CA-SRC-U ! ;

: CHECK-ALL-ERRORS-FILE ( ptr u8 n ptr u8 n -- ) {: labela:ptr labelu patha:ptr pathu :}
   CA-STORE-INIT
   labelu CA-FILE-U !
   labela CA-FILE-A!
   patha pathu CA-READ-SOURCE
   CA-SRC-A@ CA-SRC-U @ LEX-SOURCE
   CA-HANDLE-LEX-UNTERM
   CA-COLLECT-DEFS
   CA-RUN-DEFS
   CA-RAW-FAILURE @ 0 <> IF CA-RAW-FAILURE @ throw THEN
   CA-FAILED @ 0 <> IF 70 throw THEN ;
