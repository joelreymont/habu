\ public-signatures.f - emit a JSON manifest of typed public Forth definitions.
\ Load after tools/lint/text.f and tools/lint/lib.f.

0 set-check

$10000 constant PS-FILE-CAP
256 constant PS-WORD-CAP
1024 constant PS-SIG-CAP
32 constant PS-NUM-CAP

8 constant PS-BS
9 constant PS-TAB
10 constant PS-LF-C
13 constant PS-CR
12 constant PS-FF
34 constant PS-DQ
44 constant PS-COMMA-C
48 constant PS-ZERO
58 constant PS-COLON-C
91 constant PS-LBRACK-C
92 constant PS-BACKSLASH-C
93 constant PS-RBRACK-C
123 constant PS-LBRACE-C
125 constant PS-RBRACE-C

1 constant PS-WORD
2 constant PS-COMMENT

create PS-FILE-BUF PS-FILE-CAP allot
create PS-WORD-BUF PS-WORD-CAP allot
create PS-SIG-BUF PS-SIG-CAP allot
create PS-NUM-BUF PS-NUM-CAP allot
create PS-ONE 1 allot

variable PS-I
variable PS-NUM-I
variable PS-FIRST?
variable PS-TA
variable PS-TU
variable PS-HAS-ALPHA?

variable PS-SRC-A
variable PS-SRC-U
variable PS-X
variable PS-OFF
variable PS-LINE
variable PS-COL
variable PS-START
variable PS-START-OFF
variable PS-START-LINE
variable PS-START-COL

variable PS-TOK-K
variable PS-TOK-A
variable PS-TOK-U
variable PS-TOK-BYTE
variable PS-TOK-LINE
variable PS-TOK-COL
variable PS-CONT-A
variable PS-CONT-U

variable PS-NAME-A
variable PS-NAME-U
variable PS-NAME-BYTE
variable PS-NAME-LINE
variable PS-NAME-COL
variable PS-SIG-A
variable PS-SIG-U

: PS-DIE ( a u -- )  76 die ;

: PS-WRITE {: fd a u :} ( fd a u -- )
   u 0= IF exit THEN
   fd a u write u <> IF s" public-signatures: write failed" PS-DIE THEN ;

: PS-OUT {: a u :} ( a u -- )
   1 a u PS-WRITE ;

: PS-C! ( c -- )
   PS-ONE c! ;

: PS-C ( c -- )
   PS-C!
   PS-ONE 1 PS-OUT ;

: PS-ERR-C ( c -- )
   PS-C!
   2 PS-ONE 1 PS-WRITE ;

: PS-ERRLN {: a u :} ( -- )
   2 a u PS-WRITE
   PS-LF-C PS-ERR-C ;

: PS-USAGE ( -- )
   s" usage: tools/public-signatures.f file ..." PS-ERRLN
   64 throw ;

: PS-U$ {: u :} ( -- a u )
   PS-NUM-CAP PS-NUM-I !
   u 0= IF
      PS-NUM-I @ 1- PS-NUM-I !
      PS-ZERO PS-NUM-BUF PS-NUM-I @ + c!
      PS-NUM-BUF PS-NUM-I @ + 1
      exit
   THEN
   u begin dup 0 > while
      dup 10 mod PS-ZERO +
      PS-NUM-I @ 1- PS-NUM-I !
      PS-NUM-BUF PS-NUM-I @ + c!
      10 /
   repeat drop
   PS-NUM-BUF PS-NUM-I @ +  PS-NUM-CAP PS-NUM-I @ - ;

: PS-JSON-U ( u -- )
   PS-U$ PS-OUT ;

: PS-JSON-BOOL ( f -- )
   IF s" true" ELSE s" false" THEN PS-OUT ;

: PS-JSON-NIBBLE ( n -- c )
   dup 10 < IF PS-ZERO + ELSE 55 + THEN ;

: PS-JSON-U00 ( c -- )
   PS-BACKSLASH-C PS-C
   117 PS-C
   PS-ZERO PS-C
   PS-ZERO PS-C
   dup 4 rshift PS-JSON-NIBBLE PS-C
   $F and PS-JSON-NIBBLE PS-C ;

: PS-JSON-ESC-C {: c :} ( c -- )
   c PS-DQ = IF PS-BACKSLASH-C PS-C PS-DQ PS-C exit THEN
   c PS-BACKSLASH-C = IF PS-BACKSLASH-C PS-C PS-BACKSLASH-C PS-C exit THEN
   c PS-BS = IF PS-BACKSLASH-C PS-C 98 PS-C exit THEN
   c PS-FF = IF PS-BACKSLASH-C PS-C 102 PS-C exit THEN
   c PS-LF-C = IF PS-BACKSLASH-C PS-C 110 PS-C exit THEN
   c PS-CR = IF PS-BACKSLASH-C PS-C 114 PS-C exit THEN
   c PS-TAB = IF PS-BACKSLASH-C PS-C 116 PS-C exit THEN
   c 32 < IF c PS-JSON-U00 exit THEN
   c PS-C ;

: PS-JSON-STRING {: a u :} ( a u -- )
   PS-DQ PS-C
   0 begin dup u < while
      dup a + c@ PS-JSON-ESC-C
      1+
   repeat drop
   PS-DQ PS-C ;

: PS-JSON-KEY ( a u -- )
   PS-JSON-STRING
   PS-COLON-C PS-C ;

: PS-JSON-COMMA ( -- ) PS-COMMA-C PS-C ;
: PS-JSON-OBJECT-START ( -- ) PS-LBRACE-C PS-C ;
: PS-JSON-OBJECT-END ( -- ) PS-RBRACE-C PS-C ;
: PS-JSON-ARRAY-START ( -- ) PS-LBRACK-C PS-C ;
: PS-JSON-ARRAY-END ( -- ) PS-RBRACK-C PS-C ;

: PS-LOWER? ( c -- f )
   dup 96 > swap 123 < and ;

: PS-UPPER? ( c -- f )
   dup 64 > swap 91 < and ;

: PS-ALPHA? ( c -- f )
   dup PS-UPPER? swap PS-LOWER? or ;

: PS-PROJECT-WORD? {: a u :} ( a u -- f )
   0 PS-HAS-ALPHA? !
   0 begin dup u < while
      a over + c@ dup PS-LOWER? IF drop 0 exit THEN
      PS-ALPHA? IF -1 PS-HAS-ALPHA? ! THEN
      1+
   repeat drop
   PS-HAS-ALPHA? @ ;

: PS-UPPER$ {: a u :} ( a u -- a' u )
   u PS-WORD-CAP > IF s" public-signatures: word too long" PS-DIE THEN
   a u PS-WORD-BUF COPY-UPPER
   PS-WORD-BUF u ;

: PS-SIGNATURE$ ( a u -- a' u' )
   TRIM PS-TU ! PS-TA !
   PS-TU @ 2 + PS-SIG-CAP > IF s" public-signatures: signature too long" PS-DIE THEN
   40 PS-SIG-BUF c!
   PS-TA @ PS-SIG-BUF 1+ PS-TU @ BMOVE
   41 PS-SIG-BUF PS-TU @ 1+ + c!
   PS-SIG-BUF PS-TU @ 2 + ;

: PS-WS? ( c -- f )
   dup 32 = over 9 = or over PS-LF-C = or swap PS-CR = or ;

: PS-END? ( -- f )
   PS-X @ PS-SRC-U @ >= ;

: PS-C@ ( -- c )
   PS-SRC-A @ PS-X @ + c@ ;

: PS-ADV ( -- c )
   PS-C@
   PS-X @ 1+ PS-X !
   dup PS-LF-C = IF
      PS-OFF @ 1+ PS-OFF !
      PS-LINE @ 1+ PS-LINE !
      1 PS-COL !
   ELSE
      dup 128 >= over 192 < and 0= IF
         PS-OFF @ 1+ PS-OFF !
         PS-COL @ 1+ PS-COL !
      THEN
   THEN ;

: PS-SKIP-LINE ( -- )
   begin PS-END? 0= while
      PS-C@ PS-LF-C = IF exit THEN
      PS-ADV drop
   repeat ;

: PS-SKIP-WS ( -- )
   begin PS-END? 0= while
      PS-C@ PS-WS? IF
         PS-ADV drop
      ELSE
         PS-C@ 92 = IF PS-SKIP-LINE ELSE exit THEN
      THEN
   repeat ;

: PS-LEX-START {: a u :} ( a u -- )
   a PS-SRC-A !
   u PS-SRC-U !
   0 PS-X !
   0 PS-OFF !
   1 PS-LINE !
   1 PS-COL ! ;

: PS-MARK-START ( -- )
   PS-X @ PS-START !
   PS-OFF @ PS-START-OFF !
   PS-LINE @ PS-START-LINE !
   PS-COL @ PS-START-COL ! ;

: PS-SAVE-TOKEN ( a u ca cu kind -- )
   {: a u ca cu kind :}
   kind PS-TOK-K !
   a PS-TOK-A !
   u PS-TOK-U !
   ca PS-CONT-A !
   cu PS-CONT-U !
   PS-START-OFF @ PS-TOK-BYTE !
   PS-START-LINE @ PS-TOK-LINE !
   PS-START-COL @ PS-TOK-COL ! ;

: PS-STRING-OPENER? {: a u :} ( a u -- f )
   u 2 <> IF 0 exit THEN
   a 1+ c@ PS-DQ <> IF 0 exit THEN
   a c@ FOLD dup 115 = swap 99 = or
   a c@ DOT = or ;

: PS-SKIP-QUOTE ( -- )
   begin PS-END? 0= while
      PS-ADV PS-DQ = IF exit THEN
   repeat ;

: PS-LEX-COMMENT ( -- )
   PS-ADV drop
   PS-SRC-A @ PS-X @ + PS-TA !
   PS-X @ PS-I !
   begin PS-END? 0= while
      PS-C@ 41 = IF
         PS-SRC-A @ PS-START @ +  PS-X @ PS-START @ -  PS-TA @  PS-X @ PS-I @ -  PS-COMMENT PS-SAVE-TOKEN
         PS-ADV drop
         exit
      THEN
      PS-ADV drop
   repeat
   PS-SRC-A @ PS-START @ +  PS-X @ PS-START @ -  PS-TA @  PS-X @ PS-I @ -  PS-COMMENT PS-SAVE-TOKEN ;

: PS-LEX-WORD ( -- )
   begin PS-END? 0= PS-C@ PS-WS? 0= and while
      PS-ADV drop
   repeat
   PS-SRC-A @ PS-START @ +  PS-X @ PS-START @ -  0 0  PS-WORD PS-SAVE-TOKEN
   PS-TOK-A @ PS-TOK-U @ PS-STRING-OPENER? IF PS-SKIP-QUOTE THEN ;

: PS-NEXT-TOK ( -- f )
   PS-SKIP-WS
   PS-END? IF 0 exit THEN
   PS-MARK-START
   PS-C@ 40 = IF PS-LEX-COMMENT ELSE PS-LEX-WORD THEN
   -1 ;

: PS-WORD? ( -- f ) PS-TOK-K @ PS-WORD = ;
: PS-COMMENT? ( -- f ) PS-TOK-K @ PS-COMMENT = ;
: PS-TOK$ ( -- a u ) PS-TOK-A @ PS-TOK-U @ ;
: PS-CONTENT$ ( -- a u ) PS-CONT-A @ PS-CONT-U @ ;

: PS-SAVE-NAME ( -- )
   PS-TOK-A @ PS-NAME-A !
   PS-TOK-U @ PS-NAME-U !
   PS-TOK-BYTE @ PS-NAME-BYTE !
   PS-TOK-LINE @ PS-NAME-LINE !
   PS-TOK-COL @ PS-NAME-COL ! ;

: PS-SAVE-SIG ( -- )
   PS-CONT-A @ PS-SIG-A !
   PS-CONT-U @ PS-SIG-U ! ;

: PS-COLLECT-EXPORTS ( -- )
   INTERN-RESET
   PS-FILE-BUF PS-TU @ PS-LEX-START
   begin PS-NEXT-TOK while
      PS-WORD? IF
         PS-TOK$ s" EXPORT" STR=CI IF
            PS-NEXT-TOK IF PS-WORD? IF PS-TOK$ INTERN-FOLD drop THEN THEN
         THEN
      THEN
   repeat ;

: PS-DEF-START ( -- )
   PS-FIRST? @ IF
      0 PS-FIRST? !
   ELSE
      PS-JSON-COMMA
   THEN
   PS-JSON-OBJECT-START ;

: PS-DEF-END ( -- )
   PS-JSON-OBJECT-END ;

: PS-PAIR-COMMA ( -- )
   PS-JSON-COMMA ;

: PS-EMIT-DEF {: exported? file-a file-u :} ( exported? file-a file-u -- )
   PS-DEF-START
   s" schema_version" PS-JSON-KEY 1 PS-JSON-U PS-PAIR-COMMA
   s" word" PS-JSON-KEY PS-NAME-A @ PS-NAME-U @ PS-UPPER$ PS-JSON-STRING PS-PAIR-COMMA
   s" file" PS-JSON-KEY file-a file-u PS-JSON-STRING PS-PAIR-COMMA
   s" line" PS-JSON-KEY PS-NAME-LINE @ PS-JSON-U PS-PAIR-COMMA
   s" column" PS-JSON-KEY PS-NAME-COL @ PS-JSON-U PS-PAIR-COMMA
   s" byte_start" PS-JSON-KEY PS-NAME-BYTE @ PS-JSON-U PS-PAIR-COMMA
   s" signature" PS-JSON-KEY PS-SIG-A @ PS-SIG-U @ PS-SIGNATURE$ PS-JSON-STRING PS-PAIR-COMMA
   s" exported" PS-JSON-KEY exported? PS-JSON-BOOL
   PS-DEF-END ;

: PS-EXPORTED? ( -- f )
   PS-NAME-A @ PS-NAME-U @ INTERN-FOLD? ;

: PS-PUBLIC? ( -- f )
   PS-EXPORTED? IF -1 exit THEN
   PS-NAME-A @ PS-NAME-U @ PS-PROJECT-WORD? ;

: PS-MAYBE-DEF {: file-a file-u :} ( file-a file-u -- )
   PS-NEXT-TOK 0= IF exit THEN
   PS-WORD? 0= IF exit THEN
   PS-SAVE-NAME
   PS-NEXT-TOK 0= IF exit THEN
   PS-COMMENT? 0= IF exit THEN
   PS-CONTENT$ s" --" CONTAINS? 0= IF exit THEN
   PS-SAVE-SIG
   PS-PUBLIC? IF PS-EXPORTED? file-a file-u PS-EMIT-DEF THEN ;

: PS-SCAN-DEFS {: file-a file-u :} ( file-a file-u -- )
   PS-FILE-BUF PS-TU @ PS-LEX-START
   begin PS-NEXT-TOK while
      PS-WORD? IF
         PS-TOK$ s" :" STR= IF file-a file-u PS-MAYBE-DEF THEN
      THEN
   repeat ;

: PS-SCAN-FILE {: file-a file-u :} ( file-a file-u -- )
   file-a file-u PS-FILE-BUF PS-FILE-CAP READ-FILE nip PS-TU !
   PS-COLLECT-EXPORTS
   file-a file-u PS-SCAN-DEFS ;

: PS-START ( -- )
   -1 PS-FIRST? !
   PS-JSON-OBJECT-START
   s" schema_version" PS-JSON-KEY 1 PS-JSON-U PS-JSON-COMMA
   s" definitions" PS-JSON-KEY PS-JSON-ARRAY-START ;

: PS-END ( -- )
   PS-JSON-ARRAY-END
   PS-JSON-OBJECT-END
   PS-LF-C PS-C ;

: PS-MAIN ( -- )
   SCRIPT-ARGC 0= IF PS-USAGE THEN
   PS-START
   0 begin dup SCRIPT-ARGC < while
      dup SCRIPT-ARGV$ PS-SCAN-FILE
      1+
   repeat drop
   PS-END ;

PS-MAIN
