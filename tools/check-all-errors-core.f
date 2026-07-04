\ check-all-errors-core.f - reusable all-errors checker core.
\ Load after lib/string.f, lib/memory.f, lib/vector.f, lib/fs.f,
\ lib/process.f, lib/process-argv.f,
\ tools/lint/text.f, tools/lint/token.f, tools/lint/lib.f,
\ tools/lint/json-writer.f, and tools/lint/source-lex.f.

: CA-MAYBE-VERIFY-SOURCE ( -- )
   s" VERIFY:SOURCE-BUF" XREF-FIND 0= if s" src/habu/verify-source.f" required then ;

CA-MAYBE-VERIFY-SOURCE

$10000 constant CA-DEFAULT-ERR-CAP
$10000 constant CA-DEFAULT-OUT-CAP
512 constant CA-INIT-CAP

10 constant CA-LF
58 constant CA-COLON-C
123 constant CA-LBRACE
$4E constant CA-DUP-RC

create CA-LF-BUF 1 allot

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
variable CA-CHECK-I
variable CA-CHECK-R
variable CA-FULL-R
variable CA-SUP-I
variable CA-FAILED
variable CA-RAW-FAILURE
variable CA-JSON-FOUND
variable CA-SRC-A
variable CA-SRC-U
variable CA-SRC-CAP
variable CA-RAW-A
variable CA-RAW-U
variable CA-MATCH-TOK
variable CA-MATCH-ORD
variable CA-ORD
variable CA-ERR-LEN
variable CA-ERR-A
variable CA-ERR-CAP
variable CA-OUT-LEN
variable CA-OUT-A
variable CA-OUT-CAP
variable CA-LS
variable CA-LE
variable CA-RAW-OFF
variable CA-RAW-Q0
variable CA-RAW-Q1

variable CA-FILE-A
variable CA-FILE-U
variable CA-JSON

: CA-TRUE ( -- bool )
   0 0= ;

: CA-FALSE ( -- bool )
   CA-TRUE 0= ;

: CA-NOT ( bool -- bool )
   IF CA-FALSE ELSE CA-TRUE THEN ;

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

: CHECK-ALL-ERRORS-BUFFERS! ( ptr u8 n ptr u8 n -- ) {: outa:ptr outcap:n erra:ptr errcap:n :}
   outcap CA-OUT-CAP !
   outa CA-OUT-A!
   errcap CA-ERR-CAP !
   erra CA-ERR-A!
   0 CA-OUT-LEN ! ;

: CHECK-ALL-ERRORS-OUT$ ( -- ptr u8 n )
   CA-OUT-A@ CA-OUT-LEN @ ;

: CHECK-ALL-ERRORS-JSON! ( bool -- )
   CA-JSON ! ;

: CA-JSON? ( -- bool )
   CA-JSON @ 0 <> ;

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

: CA-DEF-ENSURE ( n -- ) {: count:n :}
   CA-DEF-START count >COUNT VEC-ENSURE
   CA-DEF-END count >COUNT VEC-ENSURE
   CA-DEF-TOK count >COUNT VEC-ENSURE
   CA-DEF-LINE count >COUNT VEC-ENSURE
   CA-DEF-COL count >COUNT VEC-ENSURE
   CA-DEF-BYTE count >COUNT VEC-ENSURE
   CA-DEF-OK count >COUNT VEC-ENSURE ;

: CA-SUP-ENSURE ( n -- ) {: count:n :}
   CA-SUP-START count >COUNT VEC-ENSURE
   CA-SUP-END count >COUNT VEC-ENSURE ;

: CA-DEF-LEN! ( n -- ) {: count:n :}
   count >LEN CA-DEF-START VEC-LEN!
   count >LEN CA-DEF-END VEC-LEN!
   count >LEN CA-DEF-TOK VEC-LEN!
   count >LEN CA-DEF-LINE VEC-LEN!
   count >LEN CA-DEF-COL VEC-LEN!
   count >LEN CA-DEF-BYTE VEC-LEN!
   count >LEN CA-DEF-OK VEC-LEN! ;

: CA-SUP-LEN! ( n -- ) {: count:n :}
   count >LEN CA-SUP-START VEC-LEN!
   count >LEN CA-SUP-END VEC-LEN! ;

: CA-DEF-ROOM ( n -- )
   1+ dup CA-DEF-ENSURE CA-DEF-LEN! ;

: CA-SUP-ROOM ( n -- )
   1+ dup CA-SUP-ENSURE CA-SUP-LEN! ;

: CA-START@ ( n -- n ) CA-DEF-START swap CA-CELL@ ;
: CA-END@ ( n -- n ) CA-DEF-END swap CA-CELL@ ;
: CA-DEFTOK@ ( n -- n ) CA-DEF-TOK swap CA-CELL@ ;
: CA-LINE@ ( n -- n ) CA-DEF-LINE swap CA-CELL@ ;
: CA-COL@ ( n -- n ) CA-DEF-COL swap CA-CELL@ ;
: CA-BYTE@ ( n -- n ) CA-DEF-BYTE swap CA-CELL@ ;
: CA-SUP-START@ ( n -- n ) CA-SUP-START swap CA-CELL@ ;
: CA-SUP-END@ ( n -- n ) CA-SUP-END swap CA-CELL@ ;

: CA-START! ( n n -- ) CA-DEF-START swap CA-CELL! ;
: CA-END! ( n n -- ) CA-DEF-END swap CA-CELL! ;
: CA-DEFTOK! ( n n -- ) CA-DEF-TOK swap CA-CELL! ;
: CA-LINE! ( n n -- ) CA-DEF-LINE swap CA-CELL! ;
: CA-COL! ( n n -- ) CA-DEF-COL swap CA-CELL! ;
: CA-BYTE! ( n n -- ) CA-DEF-BYTE swap CA-CELL! ;
: CA-OK! ( n n -- ) CA-DEF-OK swap CA-CELL! ;
: CA-SUP-START! ( n n -- ) CA-SUP-START swap CA-CELL! ;
: CA-SUP-END! ( n n -- ) CA-SUP-END swap CA-CELL! ;

: CA-WRITE ( n ptr u8 n -- ) {: fd:n a:ptr u:n :}
   u 0= IF exit THEN
   fd a u write u <> IF s" check-all-errors: write failed" 74 CA-FAIL THEN ;

: CA-OUT-ROOM ( n -- )
   CA-OUT-LEN @ + CA-OUT-CAP @ > IF s" check-all-errors: output buffer full" 76 CA-FAIL THEN ;

: CA-ERR ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= IF exit THEN
   u CA-OUT-ROOM
   a CA-OUT-A@ CA-OUT-LEN @ + u BYTE-COPY
   CA-OUT-LEN @ u + CA-OUT-LEN ! ;

: CA-LF$ ( -- ptr u8 n )
   CA-LF CA-LF-BUF c!
   CA-LF-BUF 1 ;

: CA-TOK-WORD? ( n -- bool ) {: k:n :}
   k L# @ >= IF CA-FALSE exit THEN
   k LK@ L-WORD = ;

: CA-TOK= ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k CA-TOK-WORD? CA-NOT IF CA-FALSE exit THEN
   k LEX-TOK a u LINT-STR= ;

: CA-TOK-CI= ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k CA-TOK-WORD? CA-NOT IF CA-FALSE exit THEN
   k LEX-TOK a u LINT-STR=CI ;

: CA-PARSE-NEXT? ( n -- bool ) {: k:n :}
   k s" char" CA-TOK= IF CA-TRUE exit THEN
   k s" [char]" CA-TOK= ;

: CA-SRC-C@ ( n -- u8 )
   CA-SRC-A@ + c@ ;

: CA-RAW-C@ ( n -- u8 )
   CA-RAW-A@ + c@ ;

: CA-SCAN-RAW-CHAR ( n n -- n ) {: start:n c:n :}
   start begin dup CA-RAW-U @ < while
      dup CA-RAW-C@ c = IF exit THEN
      1+
   repeat drop -1 ;

: CA-RAW-TOKEN-FIELD? ( -- ptr u8 n bool )
   CA-RAW-A@ CA-RAW-U @ s" token" LINT-FIND-SUB CA-RAW-OFF !
   CA-RAW-OFF @ 0< IF s" " CA-FALSE exit THEN
   CA-RAW-OFF @ 58 CA-SCAN-RAW-CHAR CA-RAW-OFF !
   CA-RAW-OFF @ 0< IF s" " CA-FALSE exit THEN
   CA-RAW-OFF @ 1+ DQUOTE CA-SCAN-RAW-CHAR CA-RAW-Q0 !
   CA-RAW-Q0 @ 0< IF s" " CA-FALSE exit THEN
   CA-RAW-Q0 @ 1+ DQUOTE CA-SCAN-RAW-CHAR CA-RAW-Q1 !
   CA-RAW-Q1 @ 0< IF s" " CA-FALSE exit THEN
   CA-RAW-A@ CA-RAW-Q0 @ 1+ + CA-RAW-Q1 @ CA-RAW-Q0 @ 1+ - CA-TRUE ;

: CA-TOK-END-BYTE ( n -- n ) {: k:n :}
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

: CA-LINE-SEG-START ( n -- n ) {: k:n :}
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

: CA-LINE-SAFE-END ( n -- n ) {: k:n :}
   k LB@ CA-LINE-END-BYTE CA-LE !
   k 1+ CA-J !
   begin CA-J @ L# @ < while
      CA-J @ LL@ k LL@ <> IF CA-LE @ exit THEN
      CA-J @ s" :" CA-TOK= IF CA-J @ LB@ exit THEN
      CA-J @ 1+ CA-J !
   repeat
   CA-LE @ ;

: CA-LAST-TOK-BEFORE ( n n -- n ) {: k:n end:n :}
   k CA-J !
   begin CA-J @ 1+ L# @ < while
      CA-J @ 1+ LB@ end < IF
         CA-J @ 1+ CA-J !
      ELSE
         CA-J @ exit
      THEN
   repeat
   CA-J @ ;

: CA-ADD-SUPPORT ( n n -- ) {: start:n end:n :}
   end start <= IF exit THEN
   CA-SUP# @ CA-SUP-ROOM
   start CA-SUP# @ CA-SUP-START!
   end CA-SUP# @ CA-SUP-END!
   CA-SUP# @ 1+ CA-SUP# ! ;

: CA-ADD-SUPPORT-LINE ( n -- ) {: k:n :}
   k CA-LINE-SEG-START
   k CA-LINE-SAFE-END
   CA-ADD-SUPPORT ;

: CA-ADD-SUPPORT-PAIR ( n -- ) {: k:n :}
   k 1+ L# @ >= IF exit THEN
   k LB@
   k 1+ CA-TOK-END-BYTE
   CA-ADD-SUPPORT
   k 1+ CA-I ! ;

: CA-ADD-SUPPORT-CONSTANT ( n -- ) {: k:n :}
   k 1+ L# @ >= IF exit THEN
   k CA-LINE-SEG-START
   k 1+ CA-TOK-END-BYTE
   CA-ADD-SUPPORT
   k 1+ CA-I ! ;

: CA-FIND-SEMI ( n -- n ) {: k:n :}
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

: CA-ADD-SUPPORT-TRUSTED ( n -- ) {: k:n :}
   k CA-FIND-SEMI dup L# @ >= IF drop exit THEN
   k LB@ swap CA-TOK-END-BYTE CA-ADD-SUPPORT
   CA-J @ CA-I ! ;

: CA-ADD-SUPPORT-TRUST ( n -- ) {: k:n :}
   k CA-LINE-SEG-START
   k CA-TOK-END-BYTE
   CA-ADD-SUPPORT ;

: CA-ADD-SUPPORT-TOKEN ( n -- ) {: k:n :}
   k LB@ k CA-TOK-END-BYTE CA-ADD-SUPPORT ;

: CA-VREC-END ( n -- n ) {: k:n :}
   k 1+ CA-J !
   begin CA-J @ L# @ < while
      CA-J @ s" END-VALUE-RECORD" CA-TOK-CI= IF CA-J @ exit THEN
      CA-J @ 1+ CA-J !
   repeat
   L# @ ;

: CA-ADD-SUPPORT-VREC ( n -- ) {: k:n :}
   k CA-VREC-END {: endk:n :}
   endk L# @ >= IF exit THEN
   k LB@ endk CA-TOK-END-BYTE CA-ADD-SUPPORT
   endk CA-I ! ;

: CA-ORIGIN! ( n n -- ) {: src:n dst:n :}
   src 1+ CA-TOK-WORD? IF
      src 1+ LL@ dst CA-LINE!
      src 1+ LC@ dst CA-COL!
      src 1+ LB@ dst CA-BYTE!
   ELSE
      src LL@ dst CA-LINE!
      src LC@ dst CA-COL!
      src LB@ dst CA-BYTE!
   THEN ;

: CA-ADD-DEF ( n n n -- ) {: start:n end:n tok:n :}
   CA-DEF# @ CA-DEF-ROOM
   start CA-DEF# @ CA-START!
   end CA-DEF# @ CA-END!
   tok CA-DEF# @ CA-DEFTOK!
   tok CA-DEF# @ CA-ORIGIN!
   0 CA-DEF# @ CA-OK!
   CA-DEF# @ 1+ CA-DEF# ! ;

: CA-ADD-DEF-RANGE ( n n -- ) {: k:n semi:n :}
   k LB@ semi CA-TOK-END-BYTE k CA-ADD-DEF ;

: CA-COLLECT-DEF ( n -- ) {: k:n :}
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
   k s" package" CA-TOK-CI= IF k CA-ADD-SUPPORT-LINE exit THEN
   k s" public" CA-TOK-CI= IF k CA-ADD-SUPPORT-LINE exit THEN
   k s" private" CA-TOK-CI= IF k CA-ADD-SUPPORT-LINE exit THEN
   k s" end-package" CA-TOK-CI= IF k CA-ADD-SUPPORT-LINE exit THEN
   k s" TRUSTED:" CA-TOK-CI= IF k CA-ADD-SUPPORT-TRUSTED exit THEN
   k s" defer" CA-TOK-CI= IF k CA-COLLECT-DEFER exit THEN
   k s" undefine" CA-TOK-CI= IF k CA-COLLECT-UNDEFINE exit THEN
   k s" create" CA-TOK-CI= IF k CA-COLLECT-CREATE exit THEN
   k s" variable" CA-TOK-CI= IF k CA-ADD-SUPPORT-PAIR exit THEN
   k s" constant" CA-TOK-CI= IF k CA-ADD-SUPPORT-CONSTANT exit THEN
   k s" TRUST" CA-TOK-CI= IF k CA-ADD-SUPPORT-TRUST exit THEN
   k s" deftype" CA-TOK-CI= IF k CA-ADD-SUPPORT-PAIR exit THEN
   k s" deflinear" CA-TOK-CI= IF k CA-ADD-SUPPORT-PAIR exit THEN
   k s" value-record" CA-TOK-CI= IF k CA-ADD-SUPPORT-VREC exit THEN
   k s" immediate" CA-TOK-CI= IF k CA-ADD-SUPPORT-TOKEN exit THEN
   k s" EXPORT" CA-TOK-CI= IF k CA-ADD-SUPPORT-PAIR exit THEN ;

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

: CA-SLICE$ ( n n -- ptr u8 n ) {: start:n end:n :}
   CA-SRC-A@ start + end start - ;

: CA-JSON-LINE? ( ptr u8 n -- bool )
   LINT-TRIM dup 0= IF 2drop CA-FALSE exit THEN
   drop c@ CA-LBRACE = ;

: CA-ERR-LINE ( n n -- ptr u8 n ) {: start:n end:n :}
   CA-ERR-A@ start + end start - ;

: CA-EMIT-ERR-LINE ( n n -- ) {: start:n end:n :}
   start end CA-ERR-LINE LINT-TRIM CA-ERR
   CA-LF$ CA-ERR ;

: CA-WORD$ ( n -- ptr u8 n ) {: k:n :}
   k CA-DEFTOK@ 1+ LEX-TOK ;

: CA-COLON-LINE@ ( n -- n )
   CA-DEFTOK@ LL@ ;

: CA-COLON-COL@ ( n -- n )
   CA-DEFTOK@ LC@ ;

: CA-COLON-BYTE@ ( n -- n )
   CA-DEFTOK@ LB@ ;

: CA-DEF-FULL$ ( n -- ptr u8 n ) {: k:n :}
   k CA-START@ k CA-END@ CA-SLICE$ ;

: CA-DEF-SOURCE$ ( n -- ptr u8 n ) {: k:n :}
   CA-SRC-A@ k CA-DEFTOK@ 1+ LB@ +
   k CA-END@ 1- k CA-DEFTOK@ 1+ LB@ - ;

: CA-DECLARED$ ( n -- ptr u8 n bool ) {: k:n :}
   k CA-DEFTOK@ 2 + dup L# @ >= IF drop s" " CA-FALSE exit THEN
   dup LK@ L-COMMENT <> IF drop s" " CA-FALSE exit THEN
   LCONTENT LINT-TRIM CA-TRUE ;

: CA-BODY-START ( n -- n ) {: k:n :}
   k CA-DEFTOK@ 2 +
   begin dup L# @ < while
      dup LK@ L-COMMENT = IF 1+ ELSE exit THEN
   repeat ;

: CA-FIND-BODY-TOKEN ( n ptr u8 n -- n n bool ) {: k:n a:ptr u:n :}
   0 CA-ORD !
   k CA-BODY-START CA-J !
   begin CA-J @ L# @ < while
      CA-J @ LB@ k CA-END@ >= IF 0 0 CA-FALSE exit THEN
      CA-J @ CA-TOK-WORD? IF
         CA-J @ s" ;" CA-TOK= IF 0 0 CA-FALSE exit THEN
         CA-ORD @ 1+ CA-ORD !
         CA-J @ LEX-TOK a u LINT-STR=CI IF CA-J @ CA-ORD @ CA-TRUE exit THEN
      THEN
      CA-J @ 1+ CA-J !
   repeat
   0 0 CA-FALSE ;

: CA-CONTROL-TOK? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" [:" LINT-STR=CI IF CA-TRUE exit THEN
   a u s" ;]" LINT-STR=CI IF CA-TRUE exit THEN
   a u s" if" LINT-STR=CI IF CA-TRUE exit THEN
   a u s" else" LINT-STR=CI IF CA-TRUE exit THEN
   a u s" then" LINT-STR=CI IF CA-TRUE exit THEN
   a u s" begin" LINT-STR=CI IF CA-TRUE exit THEN
   a u s" until" LINT-STR=CI IF CA-TRUE exit THEN
   a u s" again" LINT-STR=CI IF CA-TRUE exit THEN
   a u s" while" LINT-STR=CI IF CA-TRUE exit THEN
   a u s" repeat" LINT-STR=CI IF CA-TRUE exit THEN
   a u s" do" LINT-STR=CI IF CA-TRUE exit THEN
   a u s" ?do" LINT-STR=CI IF CA-TRUE exit THEN
   a u s" loop" LINT-STR=CI IF CA-TRUE exit THEN
   a u s" +loop" LINT-STR=CI IF CA-TRUE exit THEN
   a u s" i" LINT-STR=CI IF CA-TRUE exit THEN
   a u s" j" LINT-STR=CI IF CA-TRUE exit THEN
   a u s" exit" LINT-STR=CI IF CA-TRUE exit THEN
   a u s" leave" LINT-STR=CI IF CA-TRUE exit THEN
   a u s" unloop" LINT-STR=CI IF CA-TRUE exit THEN
   a u s" recurse" LINT-STR=CI ;

: CA-JSON-EMPTY-FIELD ( ptr u8 n -- )
   LJW-KEY s" " LJW-STRING ;

: CA-JSON-UNDEF ( n n n -- ) {: k:n tok:n ord:n :}
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

: CA-DUP-WORD$ ( -- ptr u8 n )
   s" duplicate-definition" ;

: CA-JSON-DUP ( -- )
   LJW-RESET
   LJW-OBJECT-START
   s" schema_version" LJW-KEY 1 LJW-U LJW-COMMA
   s" code" LJW-KEY s" E-DUPLICATE-DEFINITION" LJW-STRING LJW-COMMA
   s" repair_class" LJW-KEY s" fix_source" LJW-STRING LJW-COMMA
   s" verdict" LJW-KEY s" rejected" LJW-STRING LJW-COMMA
   s" word" LJW-KEY CA-DUP-WORD$ LJW-STRING LJW-COMMA
   s" token" LJW-KEY CA-DUP-WORD$ LJW-STRING LJW-COMMA
   s" token_index" LJW-KEY 1 LJW-U LJW-COMMA
   s" file" LJW-KEY CA-FILE-A@ CA-FILE-U @ LJW-STRING LJW-COMMA
   s" line" LJW-KEY 1 LJW-U LJW-COMMA
   s" column" LJW-KEY 1 LJW-U LJW-COMMA
   s" byte_start" LJW-KEY 0 LJW-U LJW-COMMA
   s" byte_end" LJW-KEY CA-DUP-WORD$ nip LJW-U LJW-COMMA
   s" definition_source" LJW-KEY CA-DUP-WORD$ LJW-STRING LJW-COMMA
   s" declared_effect" LJW-KEY s" unknown" LJW-STRING LJW-COMMA
   s" declared_effect_source" LJW-KEY s" unknown" LJW-STRING LJW-COMMA
   s" inferred_effect" LJW-KEY s" unknown" LJW-STRING LJW-COMMA
   s" return_stack" LJW-KEY
   LJW-OBJECT-START
   s" expected" CA-JSON-EMPTY-FIELD LJW-COMMA
   s" actual" CA-JSON-EMPTY-FIELD
   LJW-OBJECT-END LJW-COMMA
   s" suggestion" LJW-KEY s" Rename the word or undefine the old definition before redefining it." LJW-STRING
   LJW-OBJECT-END
   LJW$ CA-ERR
   CA-LF$ CA-ERR ;

: CA-PROSE-DUP ( -- )
   s" checker: duplicate definition" CA-ERR
   CA-LF$ CA-ERR ;

: CA-HANDLE-DUP ( -- )
   CA-TRUE CA-FAILED !
   CA-JSON? IF CA-JSON-DUP ELSE CA-PROSE-DUP THEN
   CA-DUP-RC CA-RAW-FAILURE ! ;

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

: CA-TRY-RAW-JSON ( n -- bool ) {: k:n :}
   CA-ERR-A@ CA-ERR-LEN @ LINT-TRIM CA-RAW-U ! CA-RAW-A!
   CA-RAW-U @ 0= IF CA-FALSE exit THEN
   CA-RAW-TOKEN-FIELD? 0= IF 2drop CA-FALSE exit THEN
   CA-RAW-U ! CA-RAW-A!
   k CA-RAW-A@ CA-RAW-U @ CA-FIND-BODY-TOKEN IF
      CA-MATCH-ORD ! CA-MATCH-TOK !
      CA-MATCH-TOK @ LEX-TOK CA-CONTROL-TOK? IF CA-FALSE exit THEN
      k CA-MATCH-TOK @ CA-MATCH-ORD @ CA-JSON-UNDEF
      CA-TRUE
   ELSE
      2drop
      CA-FALSE
   THEN ;

: CA-UNCHECKABLE-JSON? ( -- bool )
   CA-ERR-A@ CA-ERR-LEN @ s" E-UNCHECKABLE" LINT-CONTAINS? ;

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

: CA-HANDLE-FAIL ( n n -- ) {: k:n rc:n :}
   CA-TRUE CA-FAILED !
   CA-JSON? IF
      CA-UNCHECKABLE-JSON? IF
         k CA-TRY-RAW-JSON IF exit THEN
      THEN
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

: CA-RESET-CAPTURE ( -- )
   0 CA-ERR-LEN ! ;

: CA-CHECK-RC ( n -- n )
   -1 = IF 0 ELSE 70 THEN ;

: CA-DIAG-START ( n -- )
   {: k:n :}
   CA-FILE-A@ CA-FILE-U @ DIAG-FILE!
   CA-JSON? DIAG-JSON!
   k CA-LINE@ k CA-COL@ k CA-BYTE@ DIAG-ORIGIN!
   CA-ERR-A@ CA-ERR-CAP @ DIAG-BUFFER! ;

: CA-DIAG-FINISH ( -- )
   DIAG-BUFFER$ nip CA-ERR-LEN !
   DIAG-BUFFER-OFF ;

: CA-DIAG-FULL-START ( -- )
   CA-FILE-A@ CA-FILE-U @ DIAG-FILE!
   CA-JSON? DIAG-JSON!
   1 1 0 DIAG-ORIGIN!
   CA-ERR-A@ CA-ERR-CAP @ DIAG-BUFFER! ;

: CA-CHECK-FULL-ACT ( -- )
   CA-SRC-A@ CA-SRC-U @ VERIFY:SOURCE-BUF-IN-SCOPE ;

: CA-CHECK-FULL ( -- n )
   CA-RESET-CAPTURE
   CA-DIAG-FULL-START
   [: CA-CHECK-FULL-ACT ;] catch
   CA-DIAG-FINISH ;

: CA-CHECK-FULL-SCOPE ( -- n )
   0 CA-FULL-R !
   CHECKER-SCOPE-START
   [: CA-CHECK-FULL CA-FULL-R ! ;] catch
   CHECKER-SCOPE-DONE
   dup 0= IF drop CA-FULL-R @ exit THEN ;

: CA-CHECK-ACT ( -- )
   -1 CA-CHECK-R !
   CA-CHECK-I @ CA-DEF-FULL$
   CA-CHECK-I @ CA-COLON-LINE@
   CA-CHECK-I @ CA-COLON-COL@
   CA-CHECK-I @ CA-COLON-BYTE@
   VERIFY:SOURCE-BUF-AT-IN-SCOPE ;

: CA-CHECK-DEF ( n -- n )
   {: k:n :}
   CA-RESET-CAPTURE
   0 CA-CHECK-R !
   k CA-DIAG-START
   k CA-CHECK-I !
   [: CA-CHECK-ACT ;] catch
   CA-DIAG-FINISH
   dup 0= IF drop CA-CHECK-R @ CA-CHECK-RC exit THEN ;

: CA-SUPPORT$ ( n -- ptr u8 n )
   {: k:n :}
   k CA-SUP-START@ k CA-SUP-END@ CA-SLICE$ ;

: CA-VERIFY-SUPPORT ( n -- )
   CA-SUPPORT$ VERIFY:SOURCE-BUF-IN-SCOPE ;

: CA-SUPPORT-BEFORE ( n -- )
   {: limit:n :}
   begin CA-SUP-I @ CA-SUP# @ < while
      CA-SUP-I @ CA-SUP-START@ limit >= IF exit THEN
      CA-SUP-I @ CA-VERIFY-SUPPORT
      CA-SUP-I @ 1 + CA-SUP-I !
   repeat ;

: CA-RESET-RESULTS ( -- )
   CA-FALSE CA-FAILED !
   0 CA-RAW-FAILURE ! ;

: CA-RUN-DEFS-SCOPE ( -- )
   CA-RESET-RESULTS
   0 CA-SUP-I !
   0 CA-K !
   begin CA-K @ CA-DEF# @ < while
      CA-K @ CA-START@ CA-SUPPORT-BEFORE
      CA-K @ CA-CHECK-DEF dup 0= IF
         drop -1 CA-K @ CA-OK!
      ELSE
         CA-K @ swap CA-HANDLE-FAIL
      THEN
      CA-K @ 1+ CA-K !
   repeat ;

: CA-RUN-DEFS ( -- )
   CA-RESET-RESULTS
   CA-CHECK-FULL-SCOPE dup 0= IF drop exit THEN
   dup CA-DUP-RC = IF drop CA-HANDLE-DUP exit THEN
   drop
   CHECKER-SCOPE-START
   [: CA-RUN-DEFS-SCOPE ;] catch
   CHECKER-SCOPE-DONE
   dup 0= IF drop exit THEN
   throw ;

: CA-ALLOC-SOURCE ( n -- )
   MEM-ALLOC-64K-SPAN CA-SRC-CAP ! CA-SRC-A! ;

: CA-READ-SOURCE ( ptr u8 n -- ) {: path:ptr pu:n :}
   path pu FILE-SIZE CA-ALLOC-SOURCE
   path pu CA-SRC-A@ CA-SRC-CAP @ READ-ALL CA-SRC-U ! ;

: CA-SOURCE-BUF! ( ptr u8 n -- ) {: a:ptr u:n :}
   u CA-SRC-CAP !
   u CA-SRC-U !
   a CA-SRC-A! ;

: CA-RUN-SOURCE ( -- )
   CA-SRC-A@ CA-SRC-U @ LEX-SOURCE
   CA-HANDLE-LEX-UNTERM
   CA-COLLECT-DEFS
   CA-RUN-DEFS
   CA-RAW-FAILURE @ 0 <> IF CA-RAW-FAILURE @ throw THEN
   CA-FAILED @ 0 <> IF 70 throw THEN ;

: CHECK-ALL-ERRORS-FILE ( ptr u8 n ptr u8 n -- ) {: labela:ptr labelu:n patha:ptr pathu:n :}
   CA-STORE-INIT
   labelu CA-FILE-U !
   labela CA-FILE-A!
   patha pathu CA-READ-SOURCE
   CA-RUN-SOURCE ;

: CHECK-ALL-ERRORS-BUF ( ptr u8 n ptr u8 n -- ) {: labela:ptr labelu:n srca:ptr srcu:n :}
   CA-STORE-INIT
   labelu CA-FILE-U !
   labela CA-FILE-A!
   srca srcu CA-SOURCE-BUF!
   CA-RUN-SOURCE ;
