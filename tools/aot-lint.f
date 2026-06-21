\ aot-lint.f - reject source forms unsupported by stripped AOT.
\ Load after lib/memory.f, tools/lint/lib.f, tools/lint/json-writer.f,
\ tools/lint/source-lex.f, and tools/argv.f.

0 set-check

$10000 constant AL-FILE-CAP
32 constant AL-NUM-CAP

create AL-FILE-BUF AL-FILE-CAP allot
create AL-NUM-BUF AL-NUM-CAP allot

variable AL-BAD
variable AL-I
variable AL-EXPECT-NAME
variable AL-NUM-I

variable AL-FILE-A
variable AL-FILE-U
variable AL-CURRENT-A
variable AL-CURRENT-U

: AL-OUT ( a u -- ) type ;
: AL-NL ( -- ) 10 emit ;

: AL-U$ {: u :} ( u -- a u )
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

: AL-CURRENT! {: a u :} ( a u -- )
   a AL-CURRENT-A !  u AL-CURRENT-U ! ;

: AL-LABEL$ {: a u :} ( a u -- la lu )
   ARGV-LABEL? IF 2drop ARGV-LABEL$ ELSE a u THEN ;

: AL-UNSAFE? {: a u :} ( a u -- f )
   a u s" @" STR=CI IF -1 exit THEN
   a u s" !" STR=CI IF -1 exit THEN
   a u s" c@" STR=CI IF -1 exit THEN
   a u s" c!" STR=CI IF -1 exit THEN
   a u s" here" STR=CI IF -1 exit THEN
   a u s" allot" STR=CI IF -1 exit THEN
   a u s" ," STR=CI IF -1 exit THEN
   a u s" c," STR=CI IF -1 exit THEN
   a u s" create" STR=CI IF -1 exit THEN
   a u s" compile," STR=CI IF -1 exit THEN
   a u s" patch32" STR=CI ;

: AL-WORD-TOK? {: k :} ( k -- f )
   k L# @ >= IF 0 exit THEN
   k LK@ L-WORD = ;

: AL-TOK-END {: k :} ( k -- n )
   k LB@ k LTOK nip + ;

: AL-JSON-FINDING {: k :} ( k -- )
   LJW-RESET
   LJW-OBJECT-START
   s" schema_version" LJW-KEY 1 LJW-U LJW-COMMA
   s" code" LJW-KEY s" E-AOT-UNSUPPORTED" LJW-STRING LJW-COMMA
   s" file" LJW-KEY AL-FILE-A @ AL-FILE-U @ LJW-STRING LJW-COMMA
   s" line" LJW-KEY k LL@ LJW-U LJW-COMMA
   s" column" LJW-KEY k LC@ LJW-U LJW-COMMA
   s" byte_start" LJW-KEY k LB@ LJW-U LJW-COMMA
   s" byte_end" LJW-KEY k AL-TOK-END LJW-U LJW-COMMA
   s" word" LJW-KEY AL-CURRENT-A @ AL-CURRENT-U @ LJW-STRING LJW-COMMA
   s" token" LJW-KEY k LTOK LJW-STRING LJW-COMMA
   s" reason" LJW-KEY s" stripped AOT has no persistent data region" LJW-STRING LJW-COMMA
   s" suggestion" LJW-KEY
   s" stripped AOT has no persistent data region; use --repl/snapshot for data-space words or remove the runtime data access" LJW-STRING
   LJW-OBJECT-END
   LJW$ AL-OUT AL-NL ;

: AL-TEXT-FINDING {: k :} ( k -- )
   s" E-AOT-UNSUPPORTED " AL-OUT
   AL-FILE-A @ AL-FILE-U @ AL-OUT
   58 emit k LL@ AL-U$ AL-OUT
   58 emit k LC@ AL-U$ AL-OUT
   s" : `" AL-OUT
   k LTOK AL-OUT
   s" ` is not supported by stripped AOT" AL-OUT
   AL-NL ;

: AL-REPORT {: k :} ( k -- )
   AL-BAD @ 1+ AL-BAD !
   ARGV-JSON? IF k AL-JSON-FINDING ELSE k AL-TEXT-FINDING THEN ;

: AL-HANDLE-WORD {: k :} ( k -- )
   AL-EXPECT-NAME @ IF
      k LTOK AL-CURRENT!
      0 AL-EXPECT-NAME !
      exit
   THEN
   k LTOK s" :" STR= IF
      -1 AL-EXPECT-NAME !
      s" " AL-CURRENT!
      exit
   THEN
   k LTOK s" ;" STR= IF
      s" " AL-CURRENT!
      exit
   THEN
   k LTOK AL-UNSAFE? IF k AL-REPORT THEN ;

: AL-SCAN-TOKENS ( -- )
   0 AL-I !
   0 AL-EXPECT-NAME !
   s" " AL-CURRENT!
   begin AL-I @ L# @ < while
      AL-I @ AL-WORD-TOK? IF AL-I @ AL-HANDLE-WORD THEN
      AL-I @ 1+ AL-I !
   repeat ;

: AL-SCAN-FILE {: a u :} ( a u -- )
   a u AL-LABEL$ AL-FILE-U ! AL-FILE-A !
   a u AL-FILE-BUF AL-FILE-CAP READ-FILE LEX-SOURCE
   AL-SCAN-TOKENS ;

: AL-SUMMARY ( -- )
   s" aot-lint: " AL-OUT
   AL-BAD @ AL-U$ AL-OUT
   s"  finding(s)" AL-OUT AL-NL ;

: AOT-LINT ( -- )
   s" tools/aot-lint.f [--json] [--label name] file ..." ARGV-USAGE!
   ARGV-PARSE
   1 -1 ARGV-EXPECT-POS
   0 AL-BAD !
   0 begin dup ARGV-POS# < while
      dup ARGV-POS$ AL-SCAN-FILE
      1+
   repeat drop
   ARGV-JSON? 0= AL-BAD @ 0 > and IF AL-SUMMARY THEN
   AL-BAD @ 0 > IF 1 throw THEN ;

AOT-LINT
