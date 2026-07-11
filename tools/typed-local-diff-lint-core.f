\ typed-local-diff-lint-core.f - reject newly added bare locals.
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/vector.f,
\ lib/fs.f, tools/lint/text.f, tools/lint/token.f, tools/lint/lib.f,
\ and tools/lint/source-lex.f.

32 constant TLD-NUM-CAP
43 constant TLD-PLUS-C
44 constant TLD-COMMA-C
45 constant TLD-MINUS-C
10 constant TLD-LF-C
13 constant TLD-CR-C
58 constant TLD-COLON-C

create TLD-NUM TLD-NUM-CAP allot
create TLD-ONE 1 allot

variable TLD-DIFF-A
variable TLD-DIFF-CAP
variable TLD-FILE-A
variable TLD-FILE-U
variable TLD-DIFF-U
variable TLD-BAD
variable TLD-NEW-LINE
variable TLD-IN-HUNK
variable TLD-IN-LOCALS
variable TLD-ALLOW-GROUP
variable TLD-NUM-I
variable TLD-HUNK-START
variable TLD-HUNK-END
variable TLD-SCAN-START

: TLD-FILE-A-FIELD ( -- ptr ptr u8 )
   TLD-FILE-A 0 ptr-field ;

: TLD-DIFF-A-FIELD ( -- ptr ptr u8 )
   TLD-DIFF-A 0 ptr-field ;

: TLD-FILE-A@ ( -- ptr u8 )
   TLD-FILE-A-FIELD @ ;

: TLD-DIFF-A@ ( -- ptr u8 )
   TLD-DIFF-A-FIELD @ ;

: TLD-FILE-A! ( ptr u8 -- )
   TLD-FILE-A-FIELD ! ;

: TLD-DIFF-A! ( ptr u8 -- )
   TLD-DIFF-A-FIELD ! ;

: TLD-TRUE ( -- bool )
   0 0= ;

: TLD-FALSE ( -- bool )
   TLD-TRUE 0= ;

: TLD-NOT ( bool -- bool )
   IF TLD-FALSE ELSE TLD-TRUE THEN ;

: TLD-FILE$ ( -- ptr u8 n )
   TLD-FILE-A@ TLD-FILE-U @ ;

: TLD-U$ ( n -- ptr u8 n ) {: u:n :}
   TLD-NUM-CAP TLD-NUM-I !
   u 0= IF
      TLD-NUM-I @ 1- TLD-NUM-I !
      48 TLD-NUM TLD-NUM-I @ + c!
      TLD-NUM TLD-NUM-I @ + 1
      exit
   THEN
   u begin dup 0 > while
      dup 10 mod 48 +
      TLD-NUM-I @ 1- TLD-NUM-I !
      TLD-NUM TLD-NUM-I @ + c!
      10 /
   repeat drop
   TLD-NUM TLD-NUM-I @ + TLD-NUM-CAP TLD-NUM-I @ - ;

: TLD-WRITE ( n ptr u8 n -- ) {: fd:n a:ptr u:n :}
   fd a u LINT-OUT-WRITE ;

: TLD-OUT ( ptr u8 n -- )
   1 -rot TLD-WRITE ;

: TLD-C ( n -- ) {: c:n :}
   c TLD-ONE c!
   TLD-ONE 1 TLD-OUT ;

: TLD-BAD+ ( -- )
   TLD-BAD @ 1+ TLD-BAD ! ;

: TLD-LINE-FIRST? ( ptr u8 n n -- bool ) {: a:ptr u:n c:n :}
   u 0= IF TLD-FALSE exit THEN
   a c@ c = ;

: TLD-STARTS? ( ptr u8 n ptr u8 n -- bool )
   LINT-STARTS-WITH? ;

: TLD-TOKEN= ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k LEX-TOK a u LINT-STR= ;

: TLD-TYPED-LOCAL? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u TLD-COLON-C LINT-INDEX-OF MATCH option
     none OF TLD-FALSE ENDOF
     some OF drop TLD-TRUE ENDOF
   ;MATCH ;

: TLD-FORTH-FILE? ( -- bool )
   TLD-FILE$ s" .f" LINT-ENDS-WITH? IF TLD-TRUE exit THEN
   TLD-FILE$ s" .fs" LINT-ENDS-WITH? ;

: TLD-ALLOW-LINE? ( ptr u8 n -- bool )
   s" typed-local-lint: allow-bare-local" LINT-CONTAINS? ;

: TLD-SOURCE-LINE ( n -- n ) {: k:n :}
   TLD-NEW-LINE @ k LL@ + 1- ;

: TLD-REPORT-LOCAL ( n -- ) {: k:n :}
   TLD-BAD+
   s" E-UNTYPED-LOCAL " TLD-OUT
   TLD-FILE$ dup 0= IF 2drop s" <unknown>" THEN TLD-OUT
   TLD-COLON-C TLD-C
   k TLD-SOURCE-LINE TLD-U$ TLD-OUT
   TLD-COLON-C TLD-C
   k LC@ TLD-U$ TLD-OUT
   s" : `" TLD-OUT
   k LEX-TOK TLD-OUT
   s" ` needs :type inside {: :}" TLD-OUT
   10 TLD-C ;

: TLD-SCAN-LOCAL-TOKEN ( n -- ) {: k:n :}
   k s" {:" TLD-TOKEN= IF TLD-TRUE TLD-IN-LOCALS ! exit THEN
   k s" :}" TLD-TOKEN= IF
      TLD-FALSE TLD-IN-LOCALS !
      TLD-FALSE TLD-ALLOW-GROUP !
      exit
   THEN
   TLD-IN-LOCALS @ TLD-NOT IF exit THEN
   TLD-ALLOW-GROUP @ IF exit THEN
   k LEX-TOK TLD-TYPED-LOCAL? TLD-NOT IF k TLD-REPORT-LOCAL THEN ;

: TLD-SCAN-ADDED-SOURCE ( ptr u8 n -- ) {: a:ptr u:n :}
   TLD-FORTH-FILE? TLD-NOT IF exit THEN
   a u TLD-ALLOW-LINE? IF TLD-TRUE TLD-ALLOW-GROUP ! THEN
   a u LEX-SOURCE
   0 begin dup L# @ < while
      dup TLD-SCAN-LOCAL-TOKEN
      1+
   repeat drop ;

: TLD-INC-LINE ( -- )
   TLD-NEW-LINE @ 1+ TLD-NEW-LINE ! ;

: TLD-SCAN-ADDED-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= IF TLD-INC-LINE exit THEN
   a 1+ u 1- TLD-SCAN-ADDED-SOURCE
   TLD-INC-LINE ;

: TLD-RESET-FILE ( -- )
   TLD-FALSE TLD-IN-HUNK !
   TLD-FALSE TLD-IN-LOCALS !
   TLD-FALSE TLD-ALLOW-GROUP ! ;

: TLD-SET-FILE$ ( ptr u8 n -- ) {: a:ptr u:n :}
   a TLD-FILE-A! u TLD-FILE-U !
   TLD-RESET-FILE ;

: TLD-SET-DIFF-FILE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u s" +++ b/" TLD-STARTS? IF a 6 + u 6 - TLD-SET-FILE$ exit THEN
   a u s" +++ " TLD-STARTS? IF a 4 + u 4 - TLD-SET-FILE$ exit THEN ;

: TLD-NUM-END ( ptr u8 n n -- n ) {: a:ptr u:n start:n :}
   start begin dup u < while
      dup a + c@ dup TLD-COMMA-C = over 32 = or swap 9 = or IF exit THEN
      1+
   repeat ;

: TLD-PARSE-HUNK ( ptr u8 n -- ) {: a:ptr u:n :}
   a u TLD-PLUS-C LINT-INDEX-OF MATCH option
     none OF 0 ENDOF
     some OF 1+ ENDOF
   ;MATCH TLD-HUNK-START !
   TLD-HUNK-START @ 0 <= IF exit THEN
   a u TLD-HUNK-START @ TLD-NUM-END TLD-HUNK-END !
   TLD-HUNK-END @ TLD-HUNK-START @ <= IF exit THEN
   a TLD-HUNK-START @ + TLD-HUNK-END @ TLD-HUNK-START @ - STR>NUMBER? IF
      TLD-NEW-LINE !
      TLD-TRUE TLD-IN-HUNK !
      TLD-FALSE TLD-IN-LOCALS !
      TLD-FALSE TLD-ALLOW-GROUP !
   ELSE
      drop
   THEN ;

: TLD-DIFF-FILE-LINE? ( ptr u8 n -- bool )
   s" +++ " TLD-STARTS? ;

: TLD-HUNK-LINE? ( ptr u8 n -- bool )
   s" @@" TLD-STARTS? ;

: TLD-PROCESS-HUNK-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u TLD-PLUS-C TLD-LINE-FIRST? IF a u TLD-SCAN-ADDED-LINE exit THEN
   a u TLD-MINUS-C TLD-LINE-FIRST? IF exit THEN
   a u 32 TLD-LINE-FIRST? IF TLD-INC-LINE THEN ;

: TLD-PROCESS-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u TLD-DIFF-FILE-LINE? IF a u TLD-SET-DIFF-FILE exit THEN
   a u TLD-HUNK-LINE? IF a u TLD-PARSE-HUNK exit THEN
   TLD-IN-HUNK @ IF a u TLD-PROCESS-HUNK-LINE THEN ;

: TYPED-LOCAL-DIFF-LINT-RESET ( -- )
   0 TLD-BAD !
   0 TLD-DIFF-U !
   0 TLD-FILE-U !
   0 TLD-NEW-LINE !
   TLD-FALSE TLD-IN-HUNK !
   TLD-FALSE TLD-IN-LOCALS !
   TLD-FALSE TLD-ALLOW-GROUP ! ;

: TLD-LINE-TRIM-CR ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u 0 > IF
      a u 1- + c@ TLD-CR-C = IF a u 1- exit THEN
   THEN
   a u ;

: TLD-PROCESS-LINE-SPAN ( ptr u8 n -- )
   TLD-LINE-TRIM-CR TLD-PROCESS-LINE ;

: TYPED-LOCAL-DIFF-LINT-SOURCE ( ptr u8 n -- ) {: a:ptr u:n :}
   0 TLD-SCAN-START !
   0 begin dup u < while
      dup a + c@ TLD-LF-C = IF
         a TLD-SCAN-START @ + over TLD-SCAN-START @ - TLD-PROCESS-LINE-SPAN
         dup 1+ TLD-SCAN-START !
      THEN
      1+
   repeat drop
   TLD-SCAN-START @ u < IF
      a TLD-SCAN-START @ + u TLD-SCAN-START @ - TLD-PROCESS-LINE-SPAN
   THEN ;

: TLD-ALLOC-DIFF ( n -- ) {: need:n :}
   need 1 < IF 1 ELSE need THEN
   MEM-ALLOC-64K-SPAN TLD-DIFF-CAP ! TLD-DIFF-A! ;

: TYPED-LOCAL-DIFF-LINT-FILE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu FILE-SIZE TLD-ALLOC-DIFF
   path pathu TLD-DIFF-A@ TLD-DIFF-CAP @ READ-ALL TLD-DIFF-U !
   TLD-DIFF-A@ TLD-DIFF-U @ TYPED-LOCAL-DIFF-LINT-SOURCE ;

: TYPED-LOCAL-DIFF-LINT-FINISH ( -- )
   TLD-BAD @ 0 > IF 1 throw THEN ;
