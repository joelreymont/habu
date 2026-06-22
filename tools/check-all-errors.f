\ check-all-errors.f - run the native checker over each top-level definition.
\ Load after lib/string.f, lib/memory.f, lib/vector.f, lib/fs.f,
\ tools/lint/text.f, tools/lint/token.f, tools/lint/lib.f,
\ tools/lint/json-writer.f, tools/lint/source-lex.f, and tools/argv.f.

$10000 constant CA-PROG-EXTRA
$10000 constant CA-ERR-CAP
$400 constant CA-OUT-CAP
512 constant CA-DEF-MAX
512 constant CA-SUP-MAX
32 constant CA-NUM-CAP

10 constant CA-LF
32 constant CA-SP
58 constant CA-COLON-C
123 constant CA-LBRACE
1 constant POLLIN
2 constant F-SETFD
1 constant FD-CLOEXEC

create CA-ERR-BUF CA-ERR-CAP allot
create CA-OUT-BUF CA-OUT-CAP allot
create CA-NUM-BUF CA-NUM-CAP allot
create CA-PFD 8 allot
create CA-LF-BUF 1 allot

create CA-DEF-START CA-DEF-MAX cells allot
create CA-DEF-END CA-DEF-MAX cells allot
create CA-DEF-TOK CA-DEF-MAX cells allot
create CA-DEF-LINE CA-DEF-MAX cells allot
create CA-DEF-COL CA-DEF-MAX cells allot
create CA-DEF-BYTE CA-DEF-MAX cells allot
create CA-DEF-OK CA-DEF-MAX cells allot
create CA-SUP-START CA-SUP-MAX cells allot
create CA-SUP-END CA-SUP-MAX cells allot

variable CA-DEF#
variable CA-SUP#
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
variable CA-NEXT-D
variable CA-NEXT-S

variable CA-FILE-A
variable CA-FILE-U

: CA-TRUE ( -- bool )
   0 0= ;

: CA-FALSE ( -- bool )
   CA-TRUE 0= ;

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

: CA-FAIL ( ptr u8 n n -- )
   die ;

: CA-CELL@ ( ptr a n -- n )
   cells + @ ;

: CA-CELL! ( n ptr a n -- )
   cells + ! ;

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

: CA-PFD! ( n n -- ) {: fd events :}
   events 32 lshift fd $FFFFFFFF and or CA-PFD ! ;

: CA-POLL-IN ( n n -- n ) {: fd ms :}
   fd POLLIN CA-PFD!
   CA-PFD 1 ms poll ;

: CA-CLOEXEC ( n -- ) {: fd :}
   fd F-SETFD FD-CLOEXEC fcntl drop ;

: CA-MKPIPE ( ptr n ptr n -- ) {: rvar:ptr wvar:ptr :}
   pipe 0 <> IF s" check-all-errors: pipe failed" 74 CA-FAIL THEN
   wvar !
   rvar ! ;

: CA-DRAIN-FD ( n ptr u8 n ptr n -- ) {: fd buf:ptr cap lenp:ptr :}
   0 lenp !
   begin fd 0 CA-POLL-IN 0 > while
      lenp @ cap >= IF s" check-all-errors: child output too large" 76 CA-FAIL THEN
      fd buf lenp @ + cap lenp @ - read CA-GOT !
      CA-GOT @ 0 > IF
         lenp @ CA-GOT @ + lenp !
      ELSE
         exit
      THEN
   repeat ;

: CA-TOK-WORD? ( n -- bool ) {: k :}
   k L# @ >= IF CA-FALSE exit THEN
   k LK@ L-WORD = ;

: CA-TOK= ( n ptr u8 n -- bool ) {: k a:ptr u :}
   k CA-TOK-WORD? 0= IF CA-FALSE exit THEN
   k LTOK a u STR= ;

: CA-TOK-CI= ( n ptr u8 n -- bool ) {: k a:ptr u :}
   k CA-TOK-WORD? 0= IF CA-FALSE exit THEN
   k LTOK a u STR=CI ;

: CA-PARSE-NEXT? ( n -- bool ) {: k :}
   k s" char" CA-TOK= IF CA-TRUE exit THEN
   k s" [char]" CA-TOK= ;

: CA-SRC-C@ ( n -- c )
   CA-SRC-A@ + c@ ;

: CA-TOK-END-BYTE {: k :} ( k -- n )
   k LB@ k LTOK nip + ;

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
   CA-SUP# @ CA-SUP-MAX >= IF s" check-all-errors: too many support slices" 76 CA-FAIL THEN
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
   CA-DEF# @ CA-DEF-MAX >= IF s" check-all-errors: too many definitions" 76 CA-FAIL THEN
   start CA-DEF# @ CA-START!
   end CA-DEF# @ CA-END!
   tok CA-DEF# @ CA-DEFTOK!
   tok CA-DEF# @ CA-ORIGIN!
   0 CA-DEF# @ CA-OK!
   CA-DEF# @ 1+ CA-DEF# ! ;

: CA-COLLECT-DEFS ( -- )
   0 CA-DEF# !
   0 CA-SUP# !
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
      ELSE CA-I @ s" TRUSTED:" CA-TOK-CI= IF
         CA-I @ CA-ADD-SUPPORT-TRUSTED
      ELSE CA-I @ s" create" CA-TOK-CI= IF
         CA-I @ CA-ADD-SUPPORT-LINE
         CA-I @ CA-I @ CA-LINE-SAFE-END CA-LAST-TOK-BEFORE CA-I !
      ELSE CA-I @ s" variable" CA-TOK-CI= IF
         CA-I @ CA-ADD-SUPPORT-PAIR
      ELSE CA-I @ s" constant" CA-TOK-CI= IF
         CA-I @ CA-ADD-SUPPORT-CONSTANT
      ELSE CA-I @ s" TRUST" CA-TOK-CI= IF
         CA-I @ CA-ADD-SUPPORT-TRUST
      THEN THEN THEN THEN THEN
      THEN
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
   ARGV-JSON? IF s" -1 JSON-DIAGS !" CA-PROG-LN THEN
   s" : CHECK-SH-HOOK ( n n -- n )" CA-PROG-LN
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
   CA-I @ CA-OK@ IF
      CA-I @ CA-START@ CA-I @ CA-END@ CA-PROG-SLICE
      CA-LF CA-PROG-C
   THEN
   CA-I @ 1+ CA-I ! ;

: CA-PROG-SUP-J ( -- )
   CA-J @ CA-SUP-START@ CA-J @ CA-SUP-END@ CA-PROG-SLICE
   CA-LF CA-PROG-C
   CA-J @ 1+ CA-J ! ;

: CA-PROG-CONTEXT ( n -- ) {: k :}
   0 CA-I !
   0 CA-J !
   begin
      k CA-START@ CA-NEXT-DEF CA-NEXT-D !
      k CA-START@ CA-NEXT-SUP CA-NEXT-S !
      CA-NEXT-D @ CA-INF < CA-NEXT-S @ CA-INF < or
   while
      CA-NEXT-D @ CA-NEXT-S @ <= IF
         CA-PROG-DEF-I
      ELSE
         CA-PROG-SUP-J
      THEN
   repeat ;

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

: CA-SPAWN-HB ( n -- n ) {: k :}
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
   CA-IN-W @ CA-PROG-A@ CA-PROG-LEN @ CA-WRITE
   CA-IN-W @ close
   CA-PID @ wait-rc CA-RC !
   CA-OUT-R @ CA-OUT-BUF CA-OUT-CAP CA-OUT-LEN CA-DRAIN-FD
   CA-ERR-R @ CA-ERR-BUF CA-ERR-CAP CA-ERR-LEN CA-DRAIN-FD
   CA-OUT-R @ close
   CA-ERR-R @ close
   CA-RC @ ;

: CA-JSON-LINE? ( ptr u8 n -- bool )
   TRIM dup 0= IF 2drop CA-FALSE exit THEN
   over c@ CA-LBRACE = ;

: CA-ERR-LINE ( n n -- ptr u8 n ) {: start end :}
   CA-ERR-BUF start + end start - ;

: CA-EMIT-ERR-LINE ( n n -- ) {: start end :}
   start end CA-ERR-LINE TRIM CA-ERR
   CA-LF$ CA-ERR ;

: CA-WORD$ ( n -- ptr u8 n ) {: k :}
   k CA-DEFTOK@ 1+ LTOK ;

: CA-DEF-SOURCE$ ( n -- ptr u8 n ) {: k :}
   CA-SRC-A@ k CA-DEFTOK@ 1+ LB@ +
   k CA-END@ 1- k CA-DEFTOK@ 1+ LB@ - ;

: CA-DECLARED$ ( n -- ptr u8 n bool ) {: k :}
   k CA-DEFTOK@ 2 + dup L# @ >= IF drop s" " CA-FALSE exit THEN
   dup LK@ L-COMMENT <> IF drop s" " CA-FALSE exit THEN
   LCONTENT TRIM CA-TRUE ;

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
         CA-J @ LTOK a u STR= IF CA-J @ CA-ORD @ CA-TRUE exit THEN
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
   s" token" LJW-KEY tok LTOK LJW-STRING LJW-COMMA
   s" token_index" LJW-KEY ord LJW-U LJW-COMMA
   s" file" LJW-KEY CA-FILE-A@ CA-FILE-U @ LJW-STRING LJW-COMMA
   s" line" LJW-KEY tok LL@ LJW-U LJW-COMMA
   s" column" LJW-KEY tok LC@ LJW-U LJW-COMMA
   s" byte_start" LJW-KEY tok LB@ LJW-U LJW-COMMA
   s" byte_end" LJW-KEY tok LB@ tok LTOK nip + LJW-U LJW-COMMA
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

: CA-TRY-RAW-JSON ( n -- bool ) {: k :}
   CA-ERR-BUF CA-ERR-LEN @ TRIM CA-RAW-U ! CA-RAW-A!
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

: CA-HANDLE-FAIL {: k rc :} ( k rc -- )
   -1 CA-FAILED !
   ARGV-JSON? IF
      CA-FILTER-JSON
      CA-JSON-FOUND @ 0= IF
         k CA-TRY-RAW-JSON 0= IF
            CA-ERR-BUF CA-ERR-LEN @ CA-ERR
            rc CA-RAW-FAILURE !
         THEN
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
   path pu FILE-SIZE dup CA-ALLOC-SOURCE
   path pu CA-SRC-A@ CA-SRC-CAP @ READ-ALL CA-SRC-U ! ;

: CHECK-ALL-ERRORS ( -- )
   s" tools/check-all-errors.f [--json-errors] --label name source" ARGV-USAGE!
   ARGV-PARSE
   ARGV-REQUIRE-LABEL
   1 ARGV-EXPECT-POS-EXACT
   ARGV-LABEL$ CA-FILE-U ! CA-FILE-A!
   0 ARGV-POS$ CA-READ-SOURCE
   CA-SRC-A@ CA-SRC-U @ LEX-SOURCE
   CA-COLLECT-DEFS
   CA-RUN-DEFS
   CA-RAW-FAILURE @ IF CA-RAW-FAILURE @ throw THEN
   CA-FAILED @ IF 70 throw THEN ;

CHECK-ALL-ERRORS
