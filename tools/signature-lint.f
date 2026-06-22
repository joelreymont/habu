\ signature-lint.f - strict typed-signature lint.
\ Load after lib/memory.f, lib/vector.f, tools/lint/text.f, tools/lint/token.f, tools/lint/lib.f,
\ tools/lint/source-lex.f,
\ and tools/argv.f.

0 set-check

$10000 constant SL-FILE-CAP
32 constant SL-NUM-CAP

10 constant SL-LF
32 constant SL-SP

create SL-FILE-BUF SL-FILE-CAP allot
create SL-NUM-BUF SL-NUM-CAP allot

variable SL-BAD
variable SL-I
variable SL-KIND
variable SL-NUM-I

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

: SL-CHECK-HOOK ( -- )
   CHECK! ;
' SL-CHECK-HOOK set-check

: SL-OUT ( ptr u8 n -- ) type ;
: SL-NL ( -- ) 10 emit ;

: SL-U$ ( n -- ptr u8 n ) {: u :}
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
   a SL-CODE-A !  u SL-CODE-U ! ;

: SL-WORD! ( ptr u8 n -- ) {: a:ptr u :}
   a SL-WORD-A !  u SL-WORD-U ! ;

: SL-SUG! ( ptr u8 n -- ) {: a:ptr u :}
   a SL-SUG-A !  u SL-SUG-U ! ;

: SL-ORIGIN! ( n -- ) {: k :}
   k LL@ SL-LINE !
   k LC@ SL-COL !
   k LB@ SL-BYTE ! ;

: SL-TOK-END ( n -- n ) {: k :}
   k LB@ k LTOK nip + ;

: SL-END! ( n -- )
   SL-END !
   -1 SL-HAS-END ! ;

: SL-NO-END ( -- )
   0 SL-HAS-END ! ;

: SL-JSON-FINDING ( -- )
   LJW-RESET
   LJW-OBJECT-START
   s" schema_version" LJW-KEY 1 LJW-U LJW-COMMA
   s" code" LJW-KEY SL-CODE-A @ SL-CODE-U @ LJW-STRING LJW-COMMA
   s" file" LJW-KEY SL-FILE-A @ SL-FILE-U @ LJW-STRING LJW-COMMA
   s" line" LJW-KEY SL-LINE @ LJW-U LJW-COMMA
   s" column" LJW-KEY SL-COL @ LJW-U LJW-COMMA
   s" byte_start" LJW-KEY SL-BYTE @ LJW-U LJW-COMMA
   SL-HAS-END @ IF s" byte_end" LJW-KEY SL-END @ LJW-U LJW-COMMA THEN
   s" word" LJW-KEY SL-WORD-A @ SL-WORD-U @ LJW-STRING LJW-COMMA
   s" suggestion" LJW-KEY SL-SUG-A @ SL-SUG-U @ LJW-STRING
   LJW-OBJECT-END
   LJW$ SL-OUT SL-NL ;

: SL-MISSING-SIG? ( -- f )
   SL-CODE-A @ SL-CODE-U @ s" E-MISSING-SIGNATURE" STR= ;

: SL-TEXT-FINDING ( -- )
   SL-CODE-A @ SL-CODE-U @ SL-OUT
   32 emit
   SL-FILE-A @ SL-FILE-U @ SL-OUT
   58 emit SL-LINE @ SL-U$ SL-OUT
   58 emit SL-COL @ SL-U$ SL-OUT
   s" : `" SL-OUT
   SL-WORD-A @ SL-WORD-U @ SL-OUT
   s" ` " SL-OUT
   SL-MISSING-SIG? IF
      s" needs a typed `( in -- out )` signature" SL-OUT
   ELSE
      s" must use a typed `( in -- out )` signature" SL-OUT
   THEN
   SL-NL ;

: SL-REPORT ( -- )
   SL-BAD @ 1+ SL-BAD !
   ARGV-JSON? IF SL-JSON-FINDING ELSE SL-TEXT-FINDING THEN ;

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
   name LTOK SL-WORD!
   s" add a typed `( in -- out )` signature immediately after the word name" SL-SUG!
   SL-REPORT ;

: SL-UNVERIFIED-SIGNATURE ( n n -- ) {: name sig :}
   s" E-UNVERIFIED-SIGNATURE" SL-CODE!
   name SL-ORIGIN!
   sig SL-TOK-END SL-END!
   name LTOK SL-WORD!
   s" agent-facing strict mode requires a typed `( in -- out )` signature" SL-SUG!
   SL-REPORT ;

: SL-WORD-TOK? ( n -- bool ) {: k :}
   k L# @ >= IF 0 exit THEN
   k LK@ L-WORD = ;

: SL-COMMENT-TOK? ( n -- bool ) {: k :}
   k L# @ >= IF 0 exit THEN
   k LK@ L-COMMENT = ;

: SL-COLON? ( n -- bool ) {: k :}
   k SL-WORD-TOK? 0= IF 0 exit THEN
   k LTOK s" :" STR= ;

: SL-SIG-KIND ( n -- n ) {: k :}
   k SL-COMMENT-TOK? 0= IF SIG-MISSING exit THEN
   k LCONTENT SIG-KIND ;

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
   begin SL-I @ L# @ < while
      SL-I @ SL-COLON? IF
         SL-HANDLE-COLON
      ELSE
         SL-I @ 1+ SL-I !
      THEN
   repeat ;

: SL-LABEL$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   ARGV-LABEL? IF ARGV-LABEL$ ELSE a u THEN ;

: SL-SCAN-FILE ( ptr u8 n -- ) {: a:ptr u :}
   a u SL-LABEL$ SL-FILE-U ! SL-FILE-A !
   a u SL-FILE-BUF SL-FILE-CAP READ-FILE LEX-SOURCE
   SL-SCAN-TOKENS ;

: SL-SUMMARY ( -- )
   s" signature-lint: " SL-OUT
   SL-BAD @ SL-U$ SL-OUT
   s"  finding(s)" SL-OUT SL-NL ;

: SIGNATURE-LINT ( -- )
   s" tools/signature-lint.f [--json] [--label name] file ..." ARGV-USAGE!
   ARGV-PARSE
   1 -1 ARGV-EXPECT-POS
   0 SL-BAD !
   0 begin dup ARGV-POS# < while
      dup ARGV-POS$ SL-SCAN-FILE
      1+
   repeat drop
   ARGV-JSON? 0= SL-BAD @ 0 > and IF SL-SUMMARY THEN
   SL-BAD @ 0 > IF 1 throw THEN ;

SIGNATURE-LINT
