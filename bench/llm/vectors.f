\ vectors.f - checked vector parser and Habu snippet emitter.
\
\ Load after lib/errors.f, lib/string.f, and bench/llm/manifest.f.

59 constant BV-SEMI
32 constant BV-SPACE
45 constant BV-DASH
62 constant BV-GT
91 constant BV-LBRACK
93 constant BV-RBRACK
8192 constant BV-OUT-CAP

create BV-OUT BV-OUT-CAP allot

variable BV-OUT-LEN
variable BV-CASE-NEXT
variable BV-TOK-NEXT
variable BV-COUNT#
variable BV-IDX

: BV-TRUE ( -- bool )
   0 0= ;

: BV-FALSE ( -- bool )
   BV-TRUE 0= ;

: BV-RESET ( -- )
   0 BV-OUT-LEN ! ;

: BV$ ( -- ptr u8 n )
   BV-OUT BV-OUT-LEN @ ;

: BV-ROOM ( n -- ) {: add :}
   add 0 < if E-BM-FIELD throw then
   add BV-OUT-CAP BV-OUT-LEN @ - > if E-BM-FIELD throw then ;

: BV-APPEND ( ptr u8 n -- ) {: a:ptr u :}
   u BV-ROOM
   a BV-OUT BV-OUT-LEN @ + u BYTE-COPY
   BV-OUT-LEN @ u + BV-OUT-LEN ! ;

: BV-C ( n -- ) {: c :}
   1 BV-ROOM
   c BV-OUT BV-OUT-LEN @ + c!
   BV-OUT-LEN @ 1+ BV-OUT-LEN ! ;

: BV-NL ( -- )
   10 BV-C ;

: BV-U+ ( n -- ) {: n :}
   n 0 < if BV-DASH BV-C n negate recurse exit then
   n 10 >= if n 10 / recurse then
   n 10 mod 48 + BV-C ;

: BV-ARROW-POS ( ptr u8 n -- n )
   s" ->" FIND-SUB dup 0 < if E-BM-SCHEMA throw then ;

: BV-LHS$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   a u BV-ARROW-POS {: p :}
   a p TRIM ;

: BV-RHS$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   a u BV-ARROW-POS {: p :}
   a p 2 + + u p 2 + - TRIM ;

: BV-REQUIRE-NUM ( ptr u8 n -- )
   STR>NUMBER? 0= if E-BM-FIELD throw then drop ;

: BV-SCALAR$ ( ptr u8 n -- ptr u8 n )
   TRIM 2dup BV-REQUIRE-NUM ;

: BV-ARRAY-INNER$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   a u TRIM {: b:ptr v :}
   v 2 < if E-BM-SCHEMA throw then
   b c@ BV-LBRACK <> if E-BM-SCHEMA throw then
   b v 1- + c@ BV-RBRACK <> if E-BM-SCHEMA throw then
   b 1+ v 2 - TRIM ;

: BV-COUNT-ARRAY ( ptr u8 n -- n ) {: a:ptr u :}
   a u BV-ARRAY-INNER$ {: b:ptr v :}
   0 BV-COUNT# !
   0 BV-TOK-NEXT !
   begin
      b v BV-SPACE BV-TOK-NEXT @ SPLIT-NEXT
   while
      BV-TOK-NEXT !
      TRIM dup 0 > if
         2dup BV-REQUIRE-NUM
         2drop
         BV-COUNT# @ 1+ BV-COUNT# !
      else
         2drop
      then
   repeat
   drop 2drop
   BV-COUNT# @ ;

: BV-APPEND-ARRAY-BUILD ( ptr u8 n -- n ) {: a:ptr u :}
   a u BV-ARRAY-INNER$ {: b:ptr v :}
   s" here" BV-APPEND
   0 BV-COUNT# !
   0 BV-TOK-NEXT !
   begin
      b v BV-SPACE BV-TOK-NEXT @ SPLIT-NEXT
   while
      BV-TOK-NEXT !
      TRIM dup 0 > if
         2dup BV-REQUIRE-NUM
         BV-SPACE BV-C
         BV-APPEND
         s"  ," BV-APPEND
         BV-COUNT# @ 1+ BV-COUNT# !
      else
         2drop
      then
   repeat
   drop 2drop
   s"  AP !" BV-APPEND
   BV-COUNT# @ ;

: BV-APPEND-AA-CHECKS ( ptr u8 n -- n ) {: a:ptr u :}
   a u BV-ARRAY-INNER$ {: b:ptr v :}
   0 BV-IDX !
   0 BV-TOK-NEXT !
   begin
      b v BV-SPACE BV-TOK-NEXT @ SPLIT-NEXT
   while
      BV-TOK-NEXT !
      TRIM dup 0 > if
         2dup BV-REQUIRE-NUM
         s"  AP @ " BV-APPEND
         BV-IDX @ BV-U+
         s"  cells + @ " BV-APPEND
         BV-APPEND
         s"  G=" BV-APPEND
         BV-IDX @ 1+ BV-IDX !
      else
         2drop
      then
   repeat
   drop 2drop
   BV-IDX @ ;

: BV-EMIT-AS-CASE ( ptr u8 n ptr u8 n -- ) {: name:ptr nameu case:ptr caseu :}
   case caseu BV-LHS$ BV-APPEND-ARRAY-BUILD {: len :}
   s"   AP @ " BV-APPEND
   len BV-U+
   BV-SPACE BV-C
   name nameu BV-APPEND
   BV-SPACE BV-C
   case caseu BV-RHS$ BV-SCALAR$ BV-APPEND
   s"  G=" BV-APPEND
   BV-NL ;

: BV-EMIT-AA-CASE ( ptr u8 n ptr u8 n -- ) {: name:ptr nameu case:ptr caseu :}
   case caseu BV-LHS$ BV-APPEND-ARRAY-BUILD {: len :}
   s"   AP @ " BV-APPEND
   len BV-U+
   BV-SPACE BV-C
   name nameu BV-APPEND
   case caseu BV-RHS$ BV-APPEND-AA-CHECKS len <> if E-BM-SCHEMA throw then
   BV-NL ;

: BV-EMPTY? ( ptr u8 n -- bool )
   TRIM s" empty" STR= ;

: BV-APPEND-STACK-SIDE ( ptr u8 n -- ) {: a:ptr u :}
   a u BV-EMPTY? if exit then
   a u TRIM BV-APPEND ;

: BV-EMIT-STACK-CASE ( ptr u8 n ptr u8 n -- ) {: name:ptr nameu case:ptr caseu :}
   s" T{ " BV-APPEND
   case caseu BV-LHS$ BV-APPEND-STACK-SIDE
   BV-SPACE BV-C
   name nameu BV-APPEND
   s"  -> " BV-APPEND
   case caseu BV-RHS$ BV-APPEND-STACK-SIDE
   s"  }T" BV-APPEND
   BV-NL ;

: BV-CASE-COUNT ( ptr u8 n -- n ) {: a:ptr u :}
   0 BV-COUNT# !
   0 BV-CASE-NEXT !
   begin
      a u BV-SEMI BV-CASE-NEXT @ SPLIT-NEXT
   while
      BV-CASE-NEXT !
      TRIM dup 0 > if
         2dup BV-ARROW-POS drop
         2drop
         BV-COUNT# @ 1+ BV-COUNT# !
      else
         2drop
      then
   repeat
   drop 2drop
   BV-COUNT# @ ;

: BV-EMIT-AS ( ptr u8 n ptr u8 n -- ) {: name:ptr nameu vec:ptr vecu :}
   0 BV-CASE-NEXT !
   begin
      vec vecu BV-SEMI BV-CASE-NEXT @ SPLIT-NEXT
   while
      BV-CASE-NEXT !
      TRIM dup 0 > if
         name nameu 2swap BV-EMIT-AS-CASE
      else
         2drop
      then
   repeat
   drop 2drop ;

: BV-EMIT-AA ( ptr u8 n ptr u8 n -- ) {: name:ptr nameu vec:ptr vecu :}
   0 BV-CASE-NEXT !
   begin
      vec vecu BV-SEMI BV-CASE-NEXT @ SPLIT-NEXT
   while
      BV-CASE-NEXT !
      TRIM dup 0 > if
         name nameu 2swap BV-EMIT-AA-CASE
      else
         2drop
      then
   repeat
   drop 2drop ;

: BV-EMIT-STACK ( ptr u8 n ptr u8 n -- ) {: name:ptr nameu vec:ptr vecu :}
   0 BV-CASE-NEXT !
   begin
      vec vecu BV-SEMI BV-CASE-NEXT @ SPLIT-NEXT
   while
      BV-CASE-NEXT !
      TRIM dup 0 > if
         name nameu 2swap BV-EMIT-STACK-CASE
      else
         2drop
      then
   repeat
   drop 2drop ;

: BV-REQUIRE-CONV ( ptr u8 n -- ) {: conv:ptr convu :}
   conv convu s" as" STR= if exit then
   conv convu s" aa" STR= if exit then
   conv convu s" stack" STR= if exit then
   E-BM-FIELD throw ;

: BV-HABU-TESTS ( ptr u8 n ptr u8 n ptr u8 n -- ptr u8 n ) {: conv:ptr convu name:ptr nameu vec:ptr vecu :}
   BV-RESET
   conv convu BV-REQUIRE-CONV
   conv convu s" as" STR= if name nameu vec vecu BV-EMIT-AS BV$ exit then
   conv convu s" aa" STR= if name nameu vec vecu BV-EMIT-AA BV$ exit then
   name nameu vec vecu BV-EMIT-STACK
   BV$ ;
