\ signature-lint-core.f - strict typed-signature lint core.
\ Load after lib/memory.f, lib/vector.f, tools/lint/text.f,
\ tools/lint/token.f, tools/lint/lib.f, tools/lint/json-writer.f, and
\ tools/lint/source-lex.f.

package SIGNATURE-LINT
private

$10000 constant SL-FILE-CAP
32 constant SL-NUM-CAP

10 constant SL-LF
32 constant SL-SP
58 constant SL-COLON-C

create SL-NUM-BUF SL-NUM-CAP allot
create SL-LF-BUF 1 allot

variable SL-BUF-A
variable SL-BAD
variable SL-I
variable SL-KIND
variable SL-NUM-I
variable SL-JSON
variable SL-OUT-FD

variable SL-FILE-A
variable SL-FILE-U
variable SL-CODE-A
variable SL-CODE-U
variable SL-LINE
variable SL-COL
variable SL-BYTE
variable SL-END
variable SL-HAS-END
variable SL-WORD-A
variable SL-WORD-U
variable SL-SUG-A
variable SL-SUG-U

: SL-BUF-A-FIELD ( -- ptr ptr u8 )
   SL-BUF-A 0 ptr-field ;

: SL-FILE-A-FIELD ( -- ptr ptr u8 )
   SL-FILE-A 0 ptr-field ;

: SL-CODE-A-FIELD ( -- ptr ptr u8 )
   SL-CODE-A 0 ptr-field ;

: SL-WORD-A-FIELD ( -- ptr ptr u8 )
   SL-WORD-A 0 ptr-field ;

: SL-SUG-A-FIELD ( -- ptr ptr u8 )
   SL-SUG-A 0 ptr-field ;

: SL-BUF-A@ ( -- ptr u8 )
   SL-BUF-A-FIELD @ ;

: SL-FILE-A@ ( -- ptr u8 )
   SL-FILE-A-FIELD @ ;

: SL-CODE-A@ ( -- ptr u8 )
   SL-CODE-A-FIELD @ ;

: SL-WORD-A@ ( -- ptr u8 )
   SL-WORD-A-FIELD @ ;

: SL-SUG-A@ ( -- ptr u8 )
   SL-SUG-A-FIELD @ ;

: SL-BUF-A! ( ptr u8 -- )
   SL-BUF-A-FIELD ! ;

: SL-FILE-A! ( ptr u8 -- )
   SL-FILE-A-FIELD ! ;

: SL-CODE-A! ( ptr u8 -- )
   SL-CODE-A-FIELD ! ;

: SL-WORD-A! ( ptr u8 -- )
   SL-WORD-A-FIELD ! ;

: SL-SUG-A! ( ptr u8 -- )
   SL-SUG-A-FIELD ! ;

public

: JSON! ( bool -- )
   SL-JSON ! ;

: OUT-FD! ( fd -- )
   SL-OUT-FD ! ;

private

: SL-ALLOC-FILE-BUF ( -- )
   SL-BUF-A @ 0= if
      SL-FILE-CAP MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop SL-BUF-A!
   then ;

: SL-FILE-BUF ( -- ptr u8 )
   SL-ALLOC-FILE-BUF
   SL-BUF-A@ ;

: SL-WRITE ( n ptr u8 n -- ) {: fd a:ptr u :}
   fd a u LINT-OUT-WRITE ;

: SL-OUT ( ptr u8 n -- )
   SL-OUT-FD @ -rot SL-WRITE ;

: SL-C ( n -- )
   SL-LF-BUF c!
   SL-LF-BUF 1 SL-OUT ;

: SL-NL ( -- )
   SL-LF SL-C ;

: SL-U$ ( n -- ptr u8 n ) {: u:n :}
   SL-NUM-CAP SL-NUM-I !
   u 0= IF
      SL-NUM-I @ 1- SL-NUM-I !
      48 SL-NUM-BUF SL-NUM-I @ + c!
      SL-NUM-BUF SL-NUM-I @ + 1
      exit
   THEN
   u begin dup 0 > while
      dup 10 mod 48 +
      SL-NUM-I @ 1- SL-NUM-I !
      SL-NUM-BUF SL-NUM-I @ + c!
      10 /
   repeat drop
   SL-NUM-BUF SL-NUM-I @ + SL-NUM-CAP SL-NUM-I @ - ;

: SL-CODE! ( ptr u8 n -- ) {: a:ptr u :}
   a SL-CODE-A!  u SL-CODE-U ! ;

: SL-WORD! ( ptr u8 n -- ) {: a:ptr u :}
   a SL-WORD-A!  u SL-WORD-U ! ;

: SL-SUG! ( ptr u8 n -- ) {: a:ptr u :}
   a SL-SUG-A!  u SL-SUG-U ! ;

: SL-ORIGIN! ( n -- ) {: k :}
   k LINT-LEX:LINE@ SL-LINE !
   k LINT-LEX:COL@ SL-COL !
   k LINT-LEX:BYTE@ SL-BYTE ! ;

: SL-TOK-END ( n -- n ) {: k :}
   k LINT-LEX:BYTE@ k LINT-LEX:TOKEN nip + ;

: SL-END! ( n -- )
   SL-END !
   LINT-TRUE SL-HAS-END ! ;

: SL-NO-END ( -- )
   LINT-FALSE SL-HAS-END ! ;

: SL-JSON-FINDING ( -- )
   JSON-WRITE:RESET
   JSON-WRITE:OBJECT-START
   s" schema_version" JSON-WRITE:KEY 1 JSON-WRITE:U JSON-WRITE:COMMA
   s" code" JSON-WRITE:KEY SL-CODE-A@ SL-CODE-U @ JSON-WRITE:STRING JSON-WRITE:COMMA
   s" file" JSON-WRITE:KEY SL-FILE-A@ SL-FILE-U @ JSON-WRITE:STRING JSON-WRITE:COMMA
   s" line" JSON-WRITE:KEY SL-LINE @ JSON-WRITE:U JSON-WRITE:COMMA
   s" column" JSON-WRITE:KEY SL-COL @ JSON-WRITE:U JSON-WRITE:COMMA
   s" byte_start" JSON-WRITE:KEY SL-BYTE @ JSON-WRITE:U JSON-WRITE:COMMA
   SL-HAS-END @ IF s" byte_end" JSON-WRITE:KEY SL-END @ JSON-WRITE:U JSON-WRITE:COMMA THEN
   s" word" JSON-WRITE:KEY SL-WORD-A@ SL-WORD-U @ JSON-WRITE:STRING JSON-WRITE:COMMA
   s" suggestion" JSON-WRITE:KEY SL-SUG-A@ SL-SUG-U @ JSON-WRITE:STRING
   JSON-WRITE:OBJECT-END
   JSON-WRITE:$ SL-OUT SL-NL ;

: SL-MISSING-SIG? ( -- f )
   SL-CODE-A@ SL-CODE-U @ s" E-MISSING-SIGNATURE" LINT-STR= ;

: SL-TEXT-FINDING ( -- )
   SL-CODE-A@ SL-CODE-U @ SL-OUT
   SL-SP SL-C
   SL-FILE-A@ SL-FILE-U @ SL-OUT
   SL-COLON-C SL-C SL-LINE @ SL-U$ SL-OUT
   SL-COLON-C SL-C SL-COL @ SL-U$ SL-OUT
   s" : `" SL-OUT
   SL-WORD-A@ SL-WORD-U @ SL-OUT
   s" ` " SL-OUT
   SL-MISSING-SIG? IF
      s" needs a typed `( in -- out )` signature" SL-OUT
   ELSE
      s" must use a typed `( in -- out )` signature" SL-OUT
   THEN
   SL-NL ;

: SL-REPORT ( -- )
   SL-BAD @ 1+ SL-BAD !
   SL-JSON @ IF SL-JSON-FINDING ELSE SL-TEXT-FINDING THEN ;

: SL-MISSING-NAME ( -- )
   s" E-MISSING-NAME" SL-CODE!
   SL-I @ SL-ORIGIN!
   SL-NO-END
   s" " SL-WORD!
   s" add a word name after ':'" SL-SUG!
   SL-REPORT ;

: SL-MISSING-SIGNATURE ( n -- ) {: name :}
   s" E-MISSING-SIGNATURE" SL-CODE!
   name SL-ORIGIN!
   name SL-TOK-END SL-END!
   name LINT-LEX:TOKEN SL-WORD!
   s" add a typed `( in -- out )` signature immediately after the word name" SL-SUG!
   SL-REPORT ;

: SL-UNVERIFIED-SIGNATURE ( n n -- ) {: name sig :}
   s" E-UNVERIFIED-SIGNATURE" SL-CODE!
   name SL-ORIGIN!
   sig SL-TOK-END SL-END!
   name LINT-LEX:TOKEN SL-WORD!
   s" agent-facing strict mode requires a typed `( in -- out )` signature" SL-SUG!
   SL-REPORT ;

: SL-WORD-TOK? ( n -- bool ) {: k :}
   k LINT-LEX:COUNT >= IF LINT-FALSE exit THEN
   k LINT-LEX:KIND@ LINT-LEX:WORD = ;

: SL-COMMENT-TOK? ( n -- bool ) {: k :}
   k LINT-LEX:COUNT >= IF LINT-FALSE exit THEN
   k LINT-LEX:KIND@ LINT-LEX:COMMENT = ;

: SL-COLON? ( n -- bool ) {: k :}
   k SL-WORD-TOK? LINT-NOT IF LINT-FALSE exit THEN
   k LINT-LEX:TOKEN s" :" LINT-STR= ;

: SL-SIG-KIND ( n -- n ) {: k :}
   k SL-COMMENT-TOK? LINT-NOT IF SIG-MISSING exit THEN
   k LINT-LEX:CONTENT SIG-KIND ;

: SL-HANDLE-COLON ( -- )
   SL-I @ 1+ SL-WORD-TOK? 0= IF
      SL-MISSING-NAME
      SL-I @ 1+ SL-I !
      exit
   THEN
   SL-I @ 2 + SL-SIG-KIND SL-KIND !
   SL-KIND @ SIG-OPTOUT = IF
      SL-I @ 1+ SL-I @ 2 + SL-UNVERIFIED-SIGNATURE
   ELSE SL-KIND @ SIG-MISSING = IF
      SL-I @ 1+ SL-MISSING-SIGNATURE
   THEN THEN
   SL-I @ 2 + SL-I ! ;

: SL-SCAN-TOKENS ( -- )
   0 SL-I !
   begin SL-I @ LINT-LEX:COUNT < while
      SL-I @ SL-COLON? IF
         SL-HANDLE-COLON
      ELSE
         SL-I @ 1+ SL-I !
      THEN
   repeat ;

public

: RESET ( -- )
   0 SL-BAD !
   LINT-FALSE SL-JSON !
   1 >FD OUT-FD! ;

: FILE-AS ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu:n label:ptr labelu:n :}
   label SL-FILE-A! labelu SL-FILE-U !
   path pathu SL-FILE-BUF SL-FILE-CAP READ-FILE LINT-LEX:SOURCE
   SL-SCAN-TOKENS ;

: FILE ( ptr u8 n -- )
   2dup FILE-AS ;

private

: SL-SUMMARY ( -- )
   s" signature-lint: " SL-OUT
   SL-BAD @ SL-U$ SL-OUT
   s"  finding(s)" SL-OUT SL-NL ;

public

: FINISH ( -- )
   SL-JSON @ LINT-NOT SL-BAD @ 0 > and IF SL-SUMMARY THEN
   SL-BAD @ 0 > IF 1 throw THEN ;

;package
