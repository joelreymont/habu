\ signature-lint.f - strict typed-signature lint.
\ Load after tools/lint/lib.f, tools/lint/source-lex.f, and tools/argv.f.

0 set-check

$10000 constant SL-FILE-CAP
$4000 constant SL-JSON-CAP
32 constant SL-NUM-CAP

8 constant SL-BS
9 constant SL-TAB
10 constant SL-LF
12 constant SL-FF
13 constant SL-CR
32 constant SL-SP
34 constant SL-DQ
44 constant SL-COMMA
48 constant SL-ZERO
58 constant SL-COLON
92 constant SL-BACKSLASH

create SL-FILE-BUF SL-FILE-CAP allot
create SL-JSON-BUF SL-JSON-CAP allot
create SL-NUM-BUF SL-NUM-CAP allot

variable SL-BAD
variable SL-I
variable SL-KIND
variable SL-NUM-I
variable SL-JSON-LEN

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

: SL-OUT ( a u -- ) type ;
: SL-NL ( -- ) 10 emit ;

: SL-U$ {: u :} ( u -- a u )
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

: SLJ-RESET ( -- )
   0 SL-JSON-LEN ! ;

: SLJ-C {: c :} ( c -- )
   SL-JSON-LEN @ 1+ SL-JSON-CAP > IF s" signature-lint: json buffer overflow" 76 die THEN
   c SL-JSON-BUF SL-JSON-LEN @ + c!
   SL-JSON-LEN @ 1+ SL-JSON-LEN ! ;

: SLJ-RAW {: a u :} ( a u -- )
   SL-JSON-LEN @ u + SL-JSON-CAP > IF s" signature-lint: json buffer overflow" 76 die THEN
   a SL-JSON-BUF SL-JSON-LEN @ + u BMOVE
   SL-JSON-LEN @ u + SL-JSON-LEN ! ;

: SLJ-HEX ( n -- c )
   dup 10 < IF SL-ZERO + ELSE 55 + THEN ;

: SLJ-U00 ( c -- )
   SL-BACKSLASH SLJ-C
   117 SLJ-C
   SL-ZERO SLJ-C
   SL-ZERO SLJ-C
   dup 4 rshift SLJ-HEX SLJ-C
   $F and SLJ-HEX SLJ-C ;

: SLJ-ESC-C {: c :} ( c -- )
   c SL-DQ = IF SL-BACKSLASH SLJ-C SL-DQ SLJ-C exit THEN
   c SL-BACKSLASH = IF SL-BACKSLASH SLJ-C SL-BACKSLASH SLJ-C exit THEN
   c SL-BS = IF SL-BACKSLASH SLJ-C 98 SLJ-C exit THEN
   c SL-FF = IF SL-BACKSLASH SLJ-C 102 SLJ-C exit THEN
   c SL-LF = IF SL-BACKSLASH SLJ-C 110 SLJ-C exit THEN
   c SL-CR = IF SL-BACKSLASH SLJ-C 114 SLJ-C exit THEN
   c SL-TAB = IF SL-BACKSLASH SLJ-C 116 SLJ-C exit THEN
   c SL-SP < IF c SLJ-U00 exit THEN
   c SLJ-C ;

: SLJ-STRING {: a u :} ( a u -- )
   SL-DQ SLJ-C
   0 begin dup u < while
      dup a + c@ SLJ-ESC-C
      1+
   repeat drop
   SL-DQ SLJ-C ;

: SLJ-KEY ( a u -- )
   SLJ-STRING
   SL-COLON SLJ-C ;

: SL-JSON-U ( u -- )
   SL-U$ SLJ-RAW ;

: SL-CODE! {: a u :} ( a u -- )
   a SL-CODE-A !  u SL-CODE-U ! ;

: SL-WORD! {: a u :} ( a u -- )
   a SL-WORD-A !  u SL-WORD-U ! ;

: SL-SUG! {: a u :} ( a u -- )
   a SL-SUG-A !  u SL-SUG-U ! ;

: SL-ORIGIN! {: k :} ( k -- )
   k LL@ SL-LINE !
   k LC@ SL-COL !
   k LB@ SL-BYTE ! ;

: SL-TOK-END {: k :} ( k -- n )
   k LB@ k LTOK nip + ;

: SL-END! ( n -- )
   SL-END !
   -1 SL-HAS-END ! ;

: SL-NO-END ( -- )
   0 SL-HAS-END ! ;

: SL-JSON-FINDING ( -- )
   SLJ-RESET
   123 SLJ-C
   s" schema_version" SLJ-KEY 1 SL-JSON-U SL-COMMA SLJ-C
   s" code" SLJ-KEY SL-CODE-A @ SL-CODE-U @ SLJ-STRING SL-COMMA SLJ-C
   s" file" SLJ-KEY SL-FILE-A @ SL-FILE-U @ SLJ-STRING SL-COMMA SLJ-C
   s" line" SLJ-KEY SL-LINE @ SL-JSON-U SL-COMMA SLJ-C
   s" column" SLJ-KEY SL-COL @ SL-JSON-U SL-COMMA SLJ-C
   s" byte_start" SLJ-KEY SL-BYTE @ SL-JSON-U SL-COMMA SLJ-C
   SL-HAS-END @ IF s" byte_end" SLJ-KEY SL-END @ SL-JSON-U SL-COMMA SLJ-C THEN
   s" word" SLJ-KEY SL-WORD-A @ SL-WORD-U @ SLJ-STRING SL-COMMA SLJ-C
   s" suggestion" SLJ-KEY SL-SUG-A @ SL-SUG-U @ SLJ-STRING
   125 SLJ-C
   SL-JSON-BUF SL-JSON-LEN @ SL-OUT SL-NL ;

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

: SL-MISSING-SIGNATURE {: name :} ( name -- )
   s" E-MISSING-SIGNATURE" SL-CODE!
   name SL-ORIGIN!
   name SL-TOK-END SL-END!
   name LTOK SL-WORD!
   s" add a typed `( in -- out )` signature immediately after the word name" SL-SUG!
   SL-REPORT ;

: SL-UNVERIFIED-SIGNATURE {: name sig :} ( name sig -- )
   s" E-UNVERIFIED-SIGNATURE" SL-CODE!
   name SL-ORIGIN!
   sig SL-TOK-END SL-END!
   name LTOK SL-WORD!
   s" agent-facing strict mode requires a typed `( in -- out )` signature" SL-SUG!
   SL-REPORT ;

: SL-WORD-TOK? {: k :} ( k -- f )
   k L# @ >= IF 0 exit THEN
   k LK@ L-WORD = ;

: SL-COMMENT-TOK? {: k :} ( k -- f )
   k L# @ >= IF 0 exit THEN
   k LK@ L-COMMENT = ;

: SL-COLON? {: k :} ( k -- f )
   k SL-WORD-TOK? 0= IF 0 exit THEN
   k LTOK s" :" STR= ;

: SL-SIG-KIND {: k :} ( k -- kind )
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

: SL-LABEL$ {: a u :} ( a u -- la lu )
   ARGV-LABEL? IF 2drop ARGV-LABEL$ ELSE a u THEN ;

: SL-SCAN-FILE {: a u :} ( a u -- )
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
