\ aot-lint-core.f - reject source forms unsupported by stripped AOT.
\ Load after lib/memory.f, lib/vector.f, tools/lint/text.f,
\ tools/lint/token.f, tools/lint/lib.f, tools/lint/json-writer.f, and
\ tools/lint/source-lex.f.

$10000 constant AL-FILE-CAP
32 constant AL-NUM-CAP

create AL-FILE-BUF AL-FILE-CAP allot
create AL-NUM-BUF AL-NUM-CAP allot
create AL-C-BUF 1 allot

variable AL-BAD
variable AL-I
variable AL-EXPECT-NAME
variable AL-NUM-I
variable AL-JSON
variable AL-OUT-FD

variable AL-FILE-A
variable AL-FILE-U
variable AL-CURRENT-A
variable AL-CURRENT-U

: AL-FILE-A-FIELD ( -- ptr ptr u8 )
   AL-FILE-A 0 ptr-field ;

: AL-CURRENT-A-FIELD ( -- ptr ptr u8 )
   AL-CURRENT-A 0 ptr-field ;

: AL-FILE-A@ ( -- ptr u8 )
   AL-FILE-A-FIELD @ ;

: AL-FILE-A! ( ptr u8 -- )
   AL-FILE-A-FIELD ! ;

: AL-CURRENT-A@ ( -- ptr u8 )
   AL-CURRENT-A-FIELD @ ;

: AL-CURRENT-A! ( ptr u8 -- )
   AL-CURRENT-A-FIELD ! ;

: AL-JSON! ( bool -- )
   AL-JSON ! ;

: AL-OUT-FD! ( fd -- )
   AL-OUT-FD ! ;

: AL-OUT ( ptr u8 n -- )
   AL-OUT-FD @ -rot LINT-OUT-WRITE ;

: AL-C ( n -- )
   AL-C-BUF c!
   AL-C-BUF 1 AL-OUT ;

: AL-NL ( -- )
   10 AL-C ;

: AL-U$ ( n -- ptr u8 n ) {: u:n :}
   AL-NUM-CAP AL-NUM-I !
   u 0= IF
      AL-NUM-I @ 1- AL-NUM-I !
      48 AL-NUM-BUF AL-NUM-I @ + c!
      AL-NUM-BUF AL-NUM-I @ + 1
      exit
   THEN
   u begin dup 0 > while
      dup 10 mod 48 +
      AL-NUM-I @ 1- AL-NUM-I !
      AL-NUM-BUF AL-NUM-I @ + c!
      10 /
   repeat drop
   AL-NUM-BUF AL-NUM-I @ + AL-NUM-CAP AL-NUM-I @ - ;

: AL-CURRENT! ( ptr u8 n -- ) {: a:ptr u:n :}
   a AL-CURRENT-A!
   u AL-CURRENT-U ! ;

\ Static token lint: it cannot tell a compile-time buffer definition
\ (`create FOO 8 allot`) from a runtime call, so it only flags tokens that are
\ never a legitimate compile-time form and always unsupported by stripped AOT.
\ Data-space words (@ ! c@ c! here allot , c, create) are now handled: the AOT
\ entry maps a persistent DATA region (aot-lib.f), and runtime create is caught
\ precisely by the closure check (aot-closure.f AOT-UNSAFE?).
: AL-UNSAFE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" compile," LINT-STR=CI IF LINT-TRUE exit THEN
   a u s" patch32" LINT-STR=CI ;

: AL-WORD-TOK? ( n -- bool ) {: k:n :}
   k L# @ >= IF LINT-FALSE exit THEN
   k LK@ L-WORD = ;

: AL-TOK-END ( n -- n ) {: k:n :}
   k LB@ k LEX-TOK nip + ;

: AL-JSON-FINDING ( n -- ) {: k:n :}
   LJW-RESET
   LJW-OBJECT-START
   s" schema_version" LJW-KEY 1 LJW-U LJW-COMMA
   s" code" LJW-KEY s" E-AOT-UNSUPPORTED" LJW-STRING LJW-COMMA
   s" file" LJW-KEY AL-FILE-A@ AL-FILE-U @ LJW-STRING LJW-COMMA
   s" line" LJW-KEY k LL@ LJW-U LJW-COMMA
   s" column" LJW-KEY k LC@ LJW-U LJW-COMMA
   s" byte_start" LJW-KEY k LB@ LJW-U LJW-COMMA
   s" byte_end" LJW-KEY k AL-TOK-END LJW-U LJW-COMMA
   s" word" LJW-KEY AL-CURRENT-A@ AL-CURRENT-U @ LJW-STRING LJW-COMMA
   s" token" LJW-KEY k LEX-TOK LJW-STRING LJW-COMMA
   s" reason" LJW-KEY s" stripped AOT has no runtime compiler or writable code" LJW-STRING LJW-COMMA
   s" suggestion" LJW-KEY
   s" stripped AOT cannot run compile,/patch32 at runtime; use --repl or remove the word" LJW-STRING
   LJW-OBJECT-END
   LJW$ AL-OUT AL-NL ;

: AL-TEXT-FINDING ( n -- ) {: k:n :}
   s" E-AOT-UNSUPPORTED " AL-OUT
   AL-FILE-A@ AL-FILE-U @ AL-OUT
   58 AL-C k LL@ AL-U$ AL-OUT
   58 AL-C k LC@ AL-U$ AL-OUT
   s" : `" AL-OUT
   k LEX-TOK AL-OUT
   s" ` is not supported by stripped AOT" AL-OUT
   AL-NL ;

: AL-REPORT ( n -- ) {: k:n :}
   AL-BAD @ 1+ AL-BAD !
   AL-JSON @ IF k AL-JSON-FINDING ELSE k AL-TEXT-FINDING THEN ;

: AL-HANDLE-WORD ( n -- ) {: k:n :}
   AL-EXPECT-NAME @ IF
      k LEX-TOK AL-CURRENT!
      0 AL-EXPECT-NAME !
      exit
   THEN
   k LEX-TOK s" :" LINT-STR= IF
      -1 AL-EXPECT-NAME !
      s" " AL-CURRENT!
      exit
   THEN
   k LEX-TOK s" ;" LINT-STR= IF
      s" " AL-CURRENT!
      exit
   THEN
   k LEX-TOK AL-UNSAFE? IF k AL-REPORT THEN ;

: AL-SCAN-TOKENS ( -- )
   0 AL-I !
   0 AL-EXPECT-NAME !
   s" " AL-CURRENT!
   begin AL-I @ L# @ < while
      AL-I @ AL-WORD-TOK? IF AL-I @ AL-HANDLE-WORD THEN
      AL-I @ 1+ AL-I !
   repeat ;

: AOT-LINT-RESET ( -- )
   0 AL-BAD !
   LINT-FALSE AL-JSON!
   1 >FD AL-OUT-FD! ;

: AOT-LINT-FILE-AS ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu:n label:ptr labelu:n :}
   label AL-FILE-A!
   labelu AL-FILE-U !
   path pathu AL-FILE-BUF AL-FILE-CAP READ-FILE LEX-SOURCE
   AL-SCAN-TOKENS ;

: AOT-LINT-FILE ( ptr u8 n -- )
   2dup AOT-LINT-FILE-AS ;

: AL-SUMMARY ( -- )
   s" aot-lint: " AL-OUT
   AL-BAD @ AL-U$ AL-OUT
   s"  finding(s)" AL-OUT AL-NL ;

: AOT-LINT-FINISH ( -- )
   AL-JSON @ LINT-NOT AL-BAD @ 0 > and IF AL-SUMMARY THEN
   AL-BAD @ 0 > IF 1 throw THEN ;
