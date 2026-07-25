\ aot-lint-core.f - reject source forms unsupported by stripped AOT.
\ Load after lib/memory.f, lib/vector.f, tools/lint/text.f,
\ tools/lint/token.f, tools/lint/lib.f, tools/lint/json-writer.f, and
\ tools/lint/source-lex.f.

package AOT-LINT

$10000 constant FILE-CAP
32 constant NUM-CAP

create FILE-BUF FILE-CAP allot
create NUM-BUF NUM-CAP allot
create C-BUF 1 allot

variable BAD-N
variable SCAN-I
variable EXPECT-NAME
variable NUM-I
variable JSON
variable OUT-FD

variable FILE-A
variable FILE-U
variable CUR-A
variable CUR-U

: FILE-A-FIELD ( -- ptr ptr u8 )
   FILE-A 0 ptr-field ;

: CUR-A-FIELD ( -- ptr ptr u8 )
   CUR-A 0 ptr-field ;

: FILE-A@ ( -- ptr u8 )
   FILE-A-FIELD @ ;

: FILE-A! ( ptr u8 -- )
   FILE-A-FIELD ! ;

: CUR-A@ ( -- ptr u8 )
   CUR-A-FIELD @ ;

: CUR-A! ( ptr u8 -- )
   CUR-A-FIELD ! ;

public

: JSON! ( bool -- )
   JSON ! ;

: OUT-FD! ( fd -- )
   OUT-FD ! ;

private

: OUT ( ptr u8 n -- )
   OUT-FD @ -rot LINT-OUT-WRITE ;

: C ( n -- )
   C-BUF c!
   C-BUF 1 OUT ;

: NL ( -- )
   10 C ;

: U$ ( n -- ptr u8 n ) {: u:n :}
   NUM-CAP NUM-I !
   u 0= IF
      NUM-I @ 1- NUM-I !
      48 NUM-BUF NUM-I @ + c!
      NUM-BUF NUM-I @ + 1
      exit
   THEN
   u begin dup 0 > while
      dup 10 mod 48 +
      NUM-I @ 1- NUM-I !
      NUM-BUF NUM-I @ + c!
      10 /
   repeat drop
   NUM-BUF NUM-I @ + NUM-CAP NUM-I @ - ;

: CUR! ( ptr u8 n -- ) {: a:ptr u:n :}
   a CUR-A!
   u CUR-U ! ;

\ Static token lint: it cannot tell a compile-time buffer definition
\ (`create FOO 8 allot`) from a runtime call, so it only flags tokens that are
\ never a legitimate compile-time form and always unsupported by stripped AOT.
\ Data-space words (@ ! c@ c! here allot , c, create) are now handled: the AOT
\ entry maps a persistent DATA region (aot-lib.f), and runtime create is caught
\ precisely by the closure check (aot-closure.f AOT-UNSAFE?).
: UNSAFE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" compile," LINT-STR=CI IF LINT-TRUE exit THEN
   a u s" patch32" LINT-STR=CI ;

: WORD-TOK? ( n -- bool ) {: k:n :}
   k LINT-LEX:COUNT >= IF LINT-FALSE exit THEN
   k LINT-LEX:KIND@ LINT-LEX:WORD = ;

: TOK-END ( n -- n ) {: k:n :}
   k LINT-LEX:BYTE@ k LINT-LEX:TOKEN nip + ;

: JSON-FINDING ( n -- ) {: k:n :}
   LJW-RESET
   LJW-OBJECT-START
   s" schema_version" LJW-KEY 1 LJW-U LJW-COMMA
   s" code" LJW-KEY s" E-AOT-UNSUPPORTED" LJW-STRING LJW-COMMA
   s" file" LJW-KEY FILE-A@ FILE-U @ LJW-STRING LJW-COMMA
   s" line" LJW-KEY k LINT-LEX:LINE@ LJW-U LJW-COMMA
   s" column" LJW-KEY k LINT-LEX:COL@ LJW-U LJW-COMMA
   s" byte_start" LJW-KEY k LINT-LEX:BYTE@ LJW-U LJW-COMMA
   s" byte_end" LJW-KEY k TOK-END LJW-U LJW-COMMA
   s" word" LJW-KEY CUR-A@ CUR-U @ LJW-STRING LJW-COMMA
   s" token" LJW-KEY k LINT-LEX:TOKEN LJW-STRING LJW-COMMA
   s" reason" LJW-KEY s" stripped AOT has no runtime compiler or writable code" LJW-STRING LJW-COMMA
   s" suggestion" LJW-KEY
   s" stripped AOT cannot run compile,/patch32 at runtime; use --repl or remove the word" LJW-STRING
   LJW-OBJECT-END
   LJW$ OUT NL ;

: TEXT-FINDING ( n -- ) {: k:n :}
   s" E-AOT-UNSUPPORTED " OUT
   FILE-A@ FILE-U @ OUT
   58 C k LINT-LEX:LINE@ U$ OUT
   58 C k LINT-LEX:COL@ U$ OUT
   s" : `" OUT
   k LINT-LEX:TOKEN OUT
   s" ` is not supported by stripped AOT" OUT
   NL ;

: REPORT ( n -- ) {: k:n :}
   BAD-N @ 1+ BAD-N !
   JSON @ IF k JSON-FINDING ELSE k TEXT-FINDING THEN ;

: HANDLE-WORD ( n -- ) {: k:n :}
   EXPECT-NAME @ IF
      k LINT-LEX:TOKEN CUR!
      0 EXPECT-NAME !
      exit
   THEN
   k LINT-LEX:TOKEN s" :" LINT-STR= IF
      -1 EXPECT-NAME !
      s" " CUR!
      exit
   THEN
   k LINT-LEX:TOKEN s" ;" LINT-STR= IF
      s" " CUR!
      exit
   THEN
   k LINT-LEX:TOKEN UNSAFE? IF k REPORT THEN ;

: SCAN-TOKENS ( -- )
   0 SCAN-I !
   0 EXPECT-NAME !
   s" " CUR!
   begin SCAN-I @ LINT-LEX:COUNT < while
      SCAN-I @ WORD-TOK? IF SCAN-I @ HANDLE-WORD THEN
      SCAN-I @ 1+ SCAN-I !
   repeat ;

public

: RESET ( -- )
   0 BAD-N !
   LINT-FALSE JSON!
   1 >FD OUT-FD! ;

: FILE-AS ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu:n label:ptr labelu:n :}
   label FILE-A!
   labelu FILE-U !
   path pathu FILE-BUF FILE-CAP READ-FILE LINT-LEX:SOURCE
   SCAN-TOKENS ;

: FILE ( ptr u8 n -- )
   2dup FILE-AS ;

private

: SUMMARY ( -- )
   s" aot-lint: " OUT
   BAD-N @ U$ OUT
   s"  finding(s)" OUT NL ;

public

: FINISH ( -- )
   JSON @ LINT-NOT BAD-N @ 0 > and IF SUMMARY THEN
   BAD-N @ 0 > IF 1 throw THEN ;

;package
