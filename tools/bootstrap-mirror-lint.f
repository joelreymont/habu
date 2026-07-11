\ bootstrap-mirror-lint.f - tripwire for the gforth recovery corpus.
\
\ The no-binary recovery (tools/bootstrap.sh) compiles ONLY src/ sources
\ (SRC_COMMON: src/core + src/arch + src/os + src/habu) with the Gforth stage-0
\ emitter, which has NO width-aware pass-2 mirror (bootstrap/cg/forth.fs; dot
\ habu-bootstrap-mirror-pass-f1714953). That is sound exactly while no ADT
\ declaration (SUMTYPE / ENUM / PRODUCT / TYPEFAMILY) exists in src/ non-test
\ source: lib/'s wide families are compiled only by the recovered NATIVE engine
\ at the immediate fixpoint refresh. This lint turns that boundary into a red
\ gate: it fires the moment a declaration keyword appears as a live token in
\ src/, naming the mirror dot that must land first.
\
\ A live token is a lexed word (comments and strings skipped by the source
\ lexer) that is not a definition NAME (src/core/sumtype.f defines the words
\ SUMTYPE / ENUM / PRODUCT / TYPEFAMILY themselves) and not an escaped
\ reference (' / ['] / postpone / char / [char]).
\
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/fs.f,
\ tools/lint/text.f, tools/lint/token.f, tools/lint/lib.f, and
\ tools/lint/source-lex.f.

$20 constant BML-NUM-CAP
$0A constant BML-LF

create BML-NUM BML-NUM-CAP allot
create BML-LF-BUF 1 allot

variable BML-SRC-A
variable BML-SRC-U
variable BML-SRC-CAP
variable BML-FILE-A
variable BML-FILE-U
variable BML-BAD
variable BML-FILES
variable BML-I
variable BML-NUM-I

: BML-SRC-A-FIELD ( -- ptr ptr u8 )
   BML-SRC-A 0 ptr-field ;

: BML-FILE-A-FIELD ( -- ptr ptr u8 )
   BML-FILE-A 0 ptr-field ;

: BML-SRC-A@ ( -- ptr u8 )
   BML-SRC-A-FIELD @ ;

: BML-SRC-A! ( ptr u8 -- )
   BML-SRC-A-FIELD ! ;

: BML-FILE-A@ ( -- ptr u8 )
   BML-FILE-A-FIELD @ ;

: BML-FILE-A! ( ptr u8 -- )
   BML-FILE-A-FIELD ! ;

: BML-C ( n -- )
   BML-LF-BUF c!
   BML-LF-BUF 1 type ;

: BML-NL ( -- )
   BML-LF BML-C ;

: BML-U. ( n -- ) {: u:n :}
   BML-NUM-CAP BML-NUM-I !
   u 0= if
      BML-NUM-I @ 1- BML-NUM-I !
      $30 BML-NUM BML-NUM-I @ + c!
      BML-NUM BML-NUM-I @ + 1 type
      exit
   then
   u begin dup 0 > while
      dup 10 mod $30 +
      BML-NUM-I @ 1- BML-NUM-I !
      BML-NUM BML-NUM-I @ + c!
      10 /
   repeat drop
   BML-NUM BML-NUM-I @ + BML-NUM-CAP BML-NUM-I @ - type ;

: BML-KEYWORD? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" sumtype" LINT-STR=CI if LINT-TRUE exit then
   a u s" enum" LINT-STR=CI if LINT-TRUE exit then
   a u s" product" LINT-STR=CI if LINT-TRUE exit then
   a u s" typefamily" LINT-STR=CI ;

: BML-WORD? ( n -- bool ) {: k:n :}
   k L# @ >= if LINT-FALSE exit then
   k LK@ L-WORD = ;

: BML-NAME-POSITION? ( n -- bool ) {: k:n :}   \ token k is a definition NAME, not a declaration
   k 0 <= if LINT-FALSE exit then
   k 1- BML-WORD? 0= if LINT-FALSE exit then
   k 1- LEX-TOK s" :" LINT-STR= if LINT-TRUE exit then
   k 1- LEX-TOK s" +:" LINT-STR= if LINT-TRUE exit then
   k 1- LEX-TOK s" TRUSTED:" LINT-STR=CI if LINT-TRUE exit then
   k 1- LEX-TOK s" KERNEL:" LINT-STR=CI if LINT-TRUE exit then
   k 1- LEX-TOK s" create" LINT-STR=CI if LINT-TRUE exit then
   k 1- LEX-TOK s" variable" LINT-STR=CI if LINT-TRUE exit then
   k 1- LEX-TOK s" constant" LINT-STR=CI if LINT-TRUE exit then
   k 1- LEX-TOK s" defer" LINT-STR=CI ;

: BML-ESCAPED? ( n -- bool ) {: k:n :}         \ token k is a quoted/parsed reference
   k 0 <= if LINT-FALSE exit then
   k 1- BML-WORD? 0= if LINT-FALSE exit then
   k 1- LEX-TOK s" '" LINT-STR= if LINT-TRUE exit then
   k 1- LEX-TOK s" [']" LINT-STR= if LINT-TRUE exit then
   k 1- LEX-TOK s" postpone" LINT-STR=CI if LINT-TRUE exit then
   k 1- LEX-TOK s" char" LINT-STR=CI if LINT-TRUE exit then
   k 1- LEX-TOK s" [char]" LINT-STR=CI ;

: BML-REPORT ( n -- ) {: k:n :}
   BML-BAD @ 1+ BML-BAD !
   s" BOOTSTRAP-MIRROR " type
   BML-FILE-A@ BML-FILE-U @ type
   $3A BML-C k LL@ BML-U.
   s" : ADT declaration `" type k LEX-TOK type
   s" ` in the gforth-compiled recovery corpus; land the stage-0 pass-2 mirror first (dot habu-bootstrap-mirror-pass-f1714953)" type
   BML-NL ;

: BML-SCAN-TOKEN ( -- )
   BML-I @ BML-WORD? 0= if exit then
   BML-I @ LEX-TOK BML-KEYWORD? 0= if exit then
   BML-I @ BML-NAME-POSITION? if exit then
   BML-I @ BML-ESCAPED? if exit then
   BML-I @ BML-REPORT ;

: BML-SCAN ( -- )
   0 BML-I !
   begin BML-I @ L# @ < while
      BML-SCAN-TOKEN
      BML-I @ 1+ BML-I !
   repeat ;

: BML-ALLOC-NEED ( n -- n ) {: n:n :}
   n 0 <= if 1 exit then
   n ;

: BML-LOAD-SOURCE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu FILE-SIZE BML-ALLOC-NEED MEM-ALLOC-64K-SPAN BML-SRC-CAP ! BML-SRC-A!
   path pathu BML-SRC-A@ BML-SRC-CAP @ READ-ALL BML-SRC-U ! ;

: BOOTSTRAP-MIRROR-LINT-FILE-AS ( ptr u8 n ptr u8 n -- )
   {: path:ptr pathu:n label:ptr labelu:n :}
   label BML-FILE-A!  labelu BML-FILE-U !
   path pathu BML-LOAD-SOURCE
   BML-FILES @ 1+ BML-FILES !
   BML-SRC-A@ BML-SRC-U @ LEX-SOURCE
   BML-SCAN ;

: BOOTSTRAP-MIRROR-LINT-FILE ( ptr u8 n -- )
   2dup BOOTSTRAP-MIRROR-LINT-FILE-AS ;

: BML-FORTH-FILE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" .f" HAS-EXT? if LINT-TRUE exit then
   a u s" .fs" HAS-EXT? ;

: BML-TEST-FILE? ( ptr u8 n -- bool )          \ src/ carries no test sources today; skip any that appear
   s" test" LINT-CONTAINS? ;

: BML-WALK-FILE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u BML-FORTH-FILE? 0= if exit then
   a u BML-TEST-FILE? if exit then
   a u BOOTSTRAP-MIRROR-LINT-FILE ;

: BOOTSTRAP-MIRROR-LINT-RESET ( -- )
   0 BML-BAD !
   0 BML-FILES ! ;

: BOOTSTRAP-MIRROR-LINT-FINISH ( -- )
   s" bootstrap-mirror-lint: " type
   BML-FILES @ BML-U. s"  file(s), " type
   BML-BAD @ BML-U. s"  finding(s)" type BML-NL
   BML-BAD @ 0 > if 1 throw then ;

: BOOTSTRAP-MIRROR-LINT ( -- )
   BOOTSTRAP-MIRROR-LINT-RESET
   s" src" [: BML-WALK-FILE ;] WALK-FILES
   BOOTSTRAP-MIRROR-LINT-FINISH ;
