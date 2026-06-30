\ public-signatures-core.f - emit typed public-word manifests.
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/vector.f, tools/lint/text.f,
\ tools/lint/intern.f, tools/lint/token.f, and tools/lint/lib.f.

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
variable PS-TRUST
variable PS-ARG-I
variable PS-OUT-A
variable PS-OUT-CAP
variable PS-OUT-U
variable PS-ERR-A
variable PS-ERR-CAP
variable PS-ERR-U
variable PS-PKG-A
variable PS-PKG-U
variable PS-IN-PKG
variable PS-PKG-PUBLIC

: PS-TA-FIELD ( -- ptr ptr u8 )
   PS-TA 0 ptr-field ;

: PS-SRC-A-FIELD ( -- ptr ptr u8 )
   PS-SRC-A 0 ptr-field ;

: PS-TOK-A-FIELD ( -- ptr ptr u8 )
   PS-TOK-A 0 ptr-field ;

: PS-CONT-A-FIELD ( -- ptr ptr u8 )
   PS-CONT-A 0 ptr-field ;

: PS-NAME-A-FIELD ( -- ptr ptr u8 )
   PS-NAME-A 0 ptr-field ;

: PS-SIG-A-FIELD ( -- ptr ptr u8 )
   PS-SIG-A 0 ptr-field ;

: PS-OUT-A-FIELD ( -- ptr ptr u8 )
   PS-OUT-A 0 ptr-field ;

: PS-ERR-A-FIELD ( -- ptr ptr u8 )
   PS-ERR-A 0 ptr-field ;

: PS-PKG-A-FIELD ( -- ptr ptr u8 )
   PS-PKG-A 0 ptr-field ;

: PS-TA@ ( -- ptr u8 )
   PS-TA-FIELD @ ;

: PS-SRC-A@ ( -- ptr u8 )
   PS-SRC-A-FIELD @ ;

: PS-TOK-A@ ( -- ptr u8 )
   PS-TOK-A-FIELD @ ;

: PS-CONT-A@ ( -- ptr u8 )
   PS-CONT-A-FIELD @ ;

: PS-NAME-A@ ( -- ptr u8 )
   PS-NAME-A-FIELD @ ;

: PS-SIG-A@ ( -- ptr u8 )
   PS-SIG-A-FIELD @ ;

: PS-OUT-A@ ( -- ptr u8 )
   PS-OUT-A-FIELD @ ;

: PS-ERR-A@ ( -- ptr u8 )
   PS-ERR-A-FIELD @ ;

: PS-PKG-A@ ( -- ptr u8 )
   PS-PKG-A-FIELD @ ;

: PS-TA! ( ptr u8 -- )
   PS-TA-FIELD ! ;

: PS-SRC-A! ( ptr u8 -- )
   PS-SRC-A-FIELD ! ;

: PS-TOK-A! ( ptr u8 -- )
   PS-TOK-A-FIELD ! ;

: PS-CONT-A! ( ptr u8 -- )
   PS-CONT-A-FIELD ! ;

: PS-NAME-A! ( ptr u8 -- )
   PS-NAME-A-FIELD ! ;

: PS-SIG-A! ( ptr u8 -- )
   PS-SIG-A-FIELD ! ;

: PS-OUT-A! ( ptr u8 -- )
   PS-OUT-A-FIELD ! ;

: PS-ERR-A! ( ptr u8 -- )
   PS-ERR-A-FIELD ! ;

: PS-PKG-A! ( ptr u8 -- )
   PS-PKG-A-FIELD ! ;

: PS-TRUE ( -- bool )
   0 0= ;

: PS-FALSE ( -- bool )
   PS-TRUE 0= ;

: PS-DIE ( ptr u8 n -- )  76 die ;

: PS-OUT-BUFFER! ( ptr u8 n -- ) {: a:ptr cap:n :}
   a PS-OUT-A!
   cap PS-OUT-CAP !
   0 PS-OUT-U ! ;

: PS-ERR-BUFFER! ( ptr u8 n -- ) {: a:ptr cap:n :}
   a PS-ERR-A!
   cap PS-ERR-CAP !
   0 PS-ERR-U ! ;

: PS-BUFFERS-OFF ( -- )
   NULL$ drop PS-OUT-A!
   NULL$ drop PS-ERR-A!
   0 PS-OUT-CAP !
   0 PS-ERR-CAP !
   0 PS-OUT-U !
   0 PS-ERR-U ! ;

: PS-OUT$ ( -- ptr u8 n )
   PS-OUT-A@ PS-OUT-U @ ;

: PS-ERR$ ( -- ptr u8 n )
   PS-ERR-A@ PS-ERR-U @ ;

: PS-WRITE-BUFFERED ( n ptr u8 n -- bool ) {: fd:n a:ptr u:n :}
   fd 1 = PS-OUT-A@ 0= 0= and IF
      a u PS-OUT-A@ PS-OUT-CAP @ PS-OUT-U BUF-APPEND
      PS-TRUE exit
   THEN
   fd 2 = PS-ERR-A@ 0= 0= and IF
      a u PS-ERR-A@ PS-ERR-CAP @ PS-ERR-U BUF-APPEND
      PS-TRUE exit
   THEN
   PS-FALSE ;

: PS-WRITE ( n ptr u8 n -- ) {: fd:n a:ptr u:n :}
   u 0= IF exit THEN
   fd a u PS-WRITE-BUFFERED IF exit THEN
   fd a u write u <> IF s" public-signatures: write failed" PS-DIE THEN ;

: PS-OUT ( ptr u8 n -- ) {: a:ptr u:n :}
   1 a u PS-WRITE ;

: PS-C! ( c -- )
   PS-ONE c! ;

: PS-C ( c -- )
   PS-C!
   PS-ONE 1 PS-OUT ;

: PS-ERR-C ( c -- )
   PS-C!
   2 PS-ONE 1 PS-WRITE ;

: PS-ERRLN ( ptr u8 n -- ) {: a:ptr u:n :}
   2 a u PS-WRITE
   PS-LF-C PS-ERR-C ;

: PS-USAGE ( -- )
   s" usage: tools/public-signatures.f [--trust] file ..." PS-ERRLN
   64 throw ;

: PS-U$ ( n -- ptr u8 n ) {: u:n :}
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

: PS-JSON-ESC-C ( n -- ) {: c:n :}
   c PS-DQ = IF PS-BACKSLASH-C PS-C PS-DQ PS-C exit THEN
   c PS-BACKSLASH-C = IF PS-BACKSLASH-C PS-C PS-BACKSLASH-C PS-C exit THEN
   c PS-BS = IF PS-BACKSLASH-C PS-C 98 PS-C exit THEN
   c PS-FF = IF PS-BACKSLASH-C PS-C 102 PS-C exit THEN
   c PS-LF-C = IF PS-BACKSLASH-C PS-C 110 PS-C exit THEN
   c PS-CR = IF PS-BACKSLASH-C PS-C 114 PS-C exit THEN
   c PS-TAB = IF PS-BACKSLASH-C PS-C 116 PS-C exit THEN
   c 32 < IF c PS-JSON-U00 exit THEN
   c PS-C ;

: PS-JSON-STRING ( ptr u8 n -- ) {: a:ptr u:n :}
   PS-DQ PS-C
   0 begin dup u < while
      dup a + c@ PS-JSON-ESC-C
      1+
   repeat drop
   PS-DQ PS-C ;

: PS-JSON-KEY ( ptr u8 n -- )
   PS-JSON-STRING
   PS-COLON-C PS-C ;

: PS-JSON-COMMA ( -- ) PS-COMMA-C PS-C ;
: PS-JSON-OBJECT-START ( -- ) PS-LBRACE-C PS-C ;
: PS-JSON-OBJECT-END ( -- ) PS-RBRACE-C PS-C ;
: PS-JSON-ARRAY-START ( -- ) PS-LBRACK-C PS-C ;
: PS-JSON-ARRAY-END ( -- ) PS-RBRACK-C PS-C ;

: PS-TRUST-Q ( ptr u8 n -- ) {: a:ptr u:n :}
   115 PS-C
   PS-DQ PS-C
   32 PS-C
   a u PS-OUT
   PS-DQ PS-C ;

: PS-LOWER? ( n -- bool )
   dup 96 > swap 123 < and ;

: PS-UPPER? ( n -- bool )
   dup 64 > swap 91 < and ;

: PS-ALPHA? ( n -- bool )
   dup PS-UPPER? swap PS-LOWER? or ;

: PS-PROJECT-WORD? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   PS-FALSE PS-HAS-ALPHA? !
   0 begin dup u < while
      a over + c@ dup PS-LOWER? IF drop drop PS-FALSE exit THEN
      PS-ALPHA? IF PS-TRUE PS-HAS-ALPHA? ! THEN
      1+
   repeat drop
   PS-HAS-ALPHA? @ IF PS-TRUE ELSE PS-FALSE THEN ;

: PS-UPPER$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u PS-WORD-CAP > IF s" public-signatures: word too long" PS-DIE THEN
   a u PS-WORD-BUF COPY-UPPER
   PS-WORD-BUF u ;

: PS-PKG-PUBLIC? ( -- bool )
   PS-IN-PKG @ if PS-PKG-PUBLIC @ else PS-FALSE then ;

: PS-QUAL-WORD$ ( -- ptr u8 n )
   PS-PKG-U @ PS-NAME-U @ + 1+ {: u:n :}
   u PS-WORD-CAP > IF s" public-signatures: word too long" PS-DIE THEN
   PS-PKG-A@ PS-PKG-U @ PS-WORD-BUF COPY-UPPER
   PS-COLON-C PS-WORD-BUF PS-PKG-U @ + c!
   PS-NAME-A@ PS-NAME-U @ PS-WORD-BUF PS-PKG-U @ + 1+ COPY-UPPER
   PS-WORD-BUF u ;

: PS-WORD$ ( -- ptr u8 n )
   PS-PKG-PUBLIC? if PS-QUAL-WORD$ exit then
   PS-NAME-A@ PS-NAME-U @ PS-UPPER$ ;

: PS-TRUST-ENTRY ( ptr u8 n -- ) {: sig:ptr sigu:n :}
   PS-WORD$ PS-TRUST-Q
   32 PS-C
   sig sigu LINT-TRIM PS-TRUST-Q
   s"  TRUST" PS-OUT
   PS-LF-C PS-C ;

: PS-SIGNATURE$ ( ptr u8 n -- ptr u8 n )
   LINT-TRIM PS-TU ! PS-TA!
   PS-TU @ 2 + PS-SIG-CAP > IF s" public-signatures: signature too long" PS-DIE THEN
   40 PS-SIG-BUF c!
   PS-TA@ PS-SIG-BUF 1+ PS-TU @ LINT-BMOVE
   41 PS-SIG-BUF PS-TU @ 1+ + c!
   PS-SIG-BUF PS-TU @ 2 + ;

: PS-WS? ( n -- bool )
   dup 32 = over 9 = or over PS-LF-C = or swap PS-CR = or ;

: PS-END? ( -- bool )
   PS-X @ PS-SRC-U @ >= ;

: PS-C@ ( -- n )
   PS-SRC-A@ PS-X @ + c@ ;

: PS-ADV ( -- n )
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

: PS-LEX-START ( ptr u8 n -- ) {: a:ptr u:n :}
   a PS-SRC-A!
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

: PS-SAVE-TOKEN ( ptr u8 n ptr u8 n n -- )
   {: a:ptr u:n ca:ptr cu:n kind:n :}
   kind PS-TOK-K !
   a PS-TOK-A!
   u PS-TOK-U !
   ca PS-CONT-A!
   cu PS-CONT-U !
   PS-START-OFF @ PS-TOK-BYTE !
   PS-START-LINE @ PS-TOK-LINE !
   PS-START-COL @ PS-TOK-COL ! ;

: PS-STRING-OPENER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 2 <> IF PS-FALSE exit THEN
   a 1+ c@ PS-DQ <> IF PS-FALSE exit THEN
   a c@ LINT-FOLD dup 115 = swap 99 = or
   a c@ DOT = or ;

: PS-SKIP-QUOTE ( -- )
   begin PS-END? 0= while
      PS-ADV PS-DQ = IF exit THEN
   repeat ;

: PS-LEX-COMMENT ( -- )
   PS-ADV drop
   PS-SRC-A@ PS-X @ + PS-TA!
   PS-X @ PS-I !
   begin PS-END? 0= while
      PS-C@ 41 = IF
         PS-SRC-A@ PS-START @ +  PS-X @ PS-START @ -  PS-TA@  PS-X @ PS-I @ -  PS-COMMENT PS-SAVE-TOKEN
         PS-ADV drop
         exit
      THEN
      PS-ADV drop
   repeat
   PS-SRC-A@ PS-START @ +  PS-X @ PS-START @ -  PS-TA@  PS-X @ PS-I @ -  PS-COMMENT PS-SAVE-TOKEN ;

: PS-LEX-WORD ( -- )
   begin PS-END? 0= PS-C@ PS-WS? 0= and while
      PS-ADV drop
   repeat
   PS-SRC-A@ PS-START @ +  PS-X @ PS-START @ -  PS-ONE 0  PS-WORD PS-SAVE-TOKEN
   PS-TOK-A@ PS-TOK-U @ PS-STRING-OPENER? IF PS-SKIP-QUOTE THEN ;

: PS-NEXT-TOK ( -- bool )
   PS-SKIP-WS
   PS-END? IF PS-FALSE exit THEN
   PS-MARK-START
   PS-C@ 40 = IF PS-LEX-COMMENT ELSE PS-LEX-WORD THEN
   PS-TRUE ;

: PS-WORD? ( -- bool ) PS-TOK-K @ PS-WORD = ;
: PS-COMMENT? ( -- bool ) PS-TOK-K @ PS-COMMENT = ;
: PS-TOK$ ( -- ptr u8 n ) PS-TOK-A@ PS-TOK-U @ ;
: PS-CONTENT$ ( -- ptr u8 n ) PS-CONT-A@ PS-CONT-U @ ;

: PS-SAVE-NAME ( -- )
   PS-TOK-A@ PS-NAME-A!
   PS-TOK-U @ PS-NAME-U !
   PS-TOK-BYTE @ PS-NAME-BYTE !
   PS-TOK-LINE @ PS-NAME-LINE !
   PS-TOK-COL @ PS-NAME-COL ! ;

: PS-SAVE-SIG ( -- )
   PS-CONT-A@ PS-SIG-A!
   PS-CONT-U @ PS-SIG-U ! ;

: PS-COLLECT-EXPORTS ( -- )
   INTERN-RESET
   PS-FILE-BUF PS-TU @ PS-LEX-START
   begin PS-NEXT-TOK while
      PS-WORD? IF
         PS-TOK$ s" EXPORT" LINT-STR=CI IF
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

: PS-EMIT-DEF ( bool ptr u8 n -- ) {: exported?:bool file-a:ptr file-u:n :}
   PS-DEF-START
   s" schema_version" PS-JSON-KEY 1 PS-JSON-U PS-PAIR-COMMA
   s" word" PS-JSON-KEY PS-WORD$ PS-JSON-STRING PS-PAIR-COMMA
   s" file" PS-JSON-KEY file-a file-u PS-JSON-STRING PS-PAIR-COMMA
   s" line" PS-JSON-KEY PS-NAME-LINE @ PS-JSON-U PS-PAIR-COMMA
   s" column" PS-JSON-KEY PS-NAME-COL @ PS-JSON-U PS-PAIR-COMMA
   s" byte_start" PS-JSON-KEY PS-NAME-BYTE @ PS-JSON-U PS-PAIR-COMMA
   s" signature" PS-JSON-KEY PS-SIG-A@ PS-SIG-U @ PS-SIGNATURE$ PS-JSON-STRING PS-PAIR-COMMA
   s" exported" PS-JSON-KEY exported? PS-JSON-BOOL
   PS-DEF-END ;

: PS-EMIT-PUBLIC ( bool ptr u8 n -- ) {: exported?:bool file-a:ptr file-u:n :}
   PS-TRUST @ IF
      exported? drop file-a drop file-u drop
      PS-SIG-A@ PS-SIG-U @ PS-TRUST-ENTRY
   ELSE
      exported? file-a file-u PS-EMIT-DEF
   THEN ;

: PS-EXPORTED? ( -- bool )
   PS-NAME-A@ PS-NAME-U @ INTERN-FOLD? ;

: PS-PUBLIC? ( -- bool )
   PS-PKG-PUBLIC? IF PS-TRUE exit THEN
   PS-IN-PKG @ IF PS-FALSE exit THEN
   PS-EXPORTED? IF PS-TRUE exit THEN
   PS-NAME-A@ PS-NAME-U @ PS-PROJECT-WORD? ;

: PS-EXPORTED-FLAG ( -- bool )
   PS-PKG-PUBLIC? IF PS-TRUE exit THEN
   PS-EXPORTED? ;

: PS-MAYBE-DEF ( ptr u8 n -- ) {: file-a:ptr file-u:n :}
   PS-NEXT-TOK 0= IF exit THEN
   PS-WORD? 0= IF exit THEN
   PS-SAVE-NAME
   PS-NEXT-TOK 0= IF exit THEN
   PS-COMMENT? 0= IF exit THEN
   PS-CONTENT$ s" --" LINT-CONTAINS? 0= IF exit THEN
   PS-SAVE-SIG
   PS-PUBLIC? IF PS-EXPORTED-FLAG file-a file-u PS-EMIT-PUBLIC THEN ;

: PS-TRUST-DEFINER ( ptr u8 n -- ) {: sig:ptr sigu:n :}
   PS-NEXT-TOK 0= IF exit THEN
   PS-WORD? 0= IF exit THEN
   PS-SAVE-NAME
   PS-PUBLIC? IF sig sigu PS-TRUST-ENTRY THEN ;

: PS-MAYBE-TRUST-DEFINER ( ptr u8 n -- bool ) {: a:ptr u:n :}
   PS-TRUST @ 0= IF PS-FALSE exit THEN
   a u s" constant" LINT-STR=CI IF s" -- a" PS-TRUST-DEFINER PS-TRUE exit THEN
   a u s" create" LINT-STR=CI IF s" -- ptr a" PS-TRUST-DEFINER PS-TRUE exit THEN
   a u s" variable" LINT-STR=CI IF s" -- ptr a" PS-TRUST-DEFINER PS-TRUE exit THEN
   PS-FALSE ;

: PS-PACKAGE-NAME! ( -- )
   PS-NEXT-TOK 0= IF exit THEN
   PS-WORD? 0= IF exit THEN
   PS-TOK-A@ PS-PKG-A!
   PS-TOK-U @ PS-PKG-U !
   PS-TRUE PS-IN-PKG !
   PS-FALSE PS-PKG-PUBLIC ! ;

: PS-SCOPE-TOKEN? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" package" LINT-STR=CI IF PS-PACKAGE-NAME! PS-TRUE exit THEN
   a u s" public" LINT-STR=CI IF PS-IN-PKG @ IF PS-TRUE PS-PKG-PUBLIC ! THEN PS-TRUE exit THEN
   a u s" private" LINT-STR=CI IF PS-IN-PKG @ IF PS-FALSE PS-PKG-PUBLIC ! THEN PS-TRUE exit THEN
   a u s" end-package" LINT-STR=CI IF PS-FALSE PS-IN-PKG ! PS-FALSE PS-PKG-PUBLIC ! PS-TRUE exit THEN
   PS-FALSE ;

: PS-RESET-SCOPE ( -- )
   PS-FALSE PS-IN-PKG !
   PS-FALSE PS-PKG-PUBLIC !
   0 PS-PKG-U ! ;

: PS-SCAN-DEFS ( ptr u8 n -- ) {: file-a:ptr file-u:n :}
   PS-RESET-SCOPE
   PS-FILE-BUF PS-TU @ PS-LEX-START
   begin PS-NEXT-TOK while
      PS-WORD? IF
         PS-TOK$ PS-SCOPE-TOKEN? IF
         ELSE PS-TOK$ s" :" LINT-STR= IF
            file-a file-u PS-MAYBE-DEF
         ELSE
            PS-TOK$ PS-MAYBE-TRUST-DEFINER drop
         THEN THEN
      THEN
   repeat ;

: PS-SCAN-FILE ( ptr u8 n -- ) {: file-a:ptr file-u:n :}
   file-a file-u PS-FILE-BUF PS-FILE-CAP READ-FILE nip PS-TU !
   PS-COLLECT-EXPORTS
   file-a file-u PS-SCAN-DEFS ;

: PS-JSON-DOC-START ( -- )
   -1 PS-FIRST? !
   PS-TRUST @ IF exit THEN
   PS-JSON-OBJECT-START
   s" schema_version" PS-JSON-KEY 1 PS-JSON-U PS-JSON-COMMA
   s" definitions" PS-JSON-KEY PS-JSON-ARRAY-START ;

: PS-JSON-DOC-END ( -- )
   PS-TRUST @ IF exit THEN
   PS-JSON-ARRAY-END
   PS-JSON-OBJECT-END
   PS-LF-C PS-C ;

: PS-PARSE-ARGS ( -- )
   0 PS-TRUST !
   0 PS-ARG-I !
   SCRIPT-ARGC 0= IF PS-USAGE THEN
   0 SCRIPT-ARGV$ s" --trust" LINT-STR= IF
      -1 PS-TRUST !
      1 PS-ARG-I !
   THEN
   PS-ARG-I @ SCRIPT-ARGC >= IF PS-USAGE THEN ;

: PS-MAIN ( -- )
   PS-PARSE-ARGS
   PS-JSON-DOC-START
   begin PS-ARG-I @ SCRIPT-ARGC < while
      PS-ARG-I @ SCRIPT-ARGV$ PS-SCAN-FILE
      PS-ARG-I @ 1+ PS-ARG-I !
   repeat
   PS-JSON-DOC-END ;
