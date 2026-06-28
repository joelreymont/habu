\ duplicate-definition-lint-core.f - report duplicate flat source definitions.
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/fs.f,
\ tools/lint/text.f, tools/lint/token.f, tools/lint/lib.f,
\ tools/lint/json-writer.f, and tools/lint/source-lex.f.

$1000 constant DDL-DEF-CAP
$2000 constant DDL-HASH-CAP
$1FFF constant DDL-HASH-MASK
$20 constant DDL-NUM-CAP
$0A constant DDL-LF
$3A constant DDL-COLON-C

create DDL-NUM DDL-NUM-CAP allot
create DDL-LF-BUF 1 allot

variable DDL-SRC-A
variable DDL-SRC-U
variable DDL-SRC-CAP
variable DDL-NAME-A
variable DDL-NAME-U
variable DDL-FILE-A
variable DDL-FILE-U
variable DDL-LINE
variable DDL-COL
variable DDL-HASH-TAB
variable DDL-FILE-LABEL-A
variable DDL-FILE-LABEL-U
variable DDL-DEF#
variable DDL-BAD
variable DDL-I
variable DDL-HASH-I
variable DDL-HASH-H
variable DDL-PROBE-I
variable DDL-CAND
variable DDL-IN-DEF
variable DDL-JSON
variable DDL-OUT-FD
variable DDL-NUM-I

: DDL-PTR-U8-FIELD ( ptr a -- ptr ptr u8 )
   0 ptr-field ;

: DDL-PTR-U8@ ( ptr a -- ptr u8 )
   DDL-PTR-U8-FIELD @ ;

: DDL-PTR-U8! ( ptr u8 ptr a -- )
   DDL-PTR-U8-FIELD ! ;

: DDL-PTR-A-FIELD ( ptr a -- ptr ptr a )
   0 ptr-field ;

: DDL-PTR-A@ ( ptr a -- ptr a )
   DDL-PTR-A-FIELD @ ;

: DDL-PTR-A! ( ptr a ptr a -- )
   DDL-PTR-A-FIELD ! ;

: DDL-TABLE-SLOT ( n ptr a -- ptr a ) {: idx table:ptr :}
   table DDL-PTR-A@ idx cells + ;

: DDL-PTR-U8-SLOT ( n ptr a -- ptr ptr u8 )
   DDL-TABLE-SLOT DDL-PTR-U8-FIELD ;

: DDL-PTR-U8-SLOT@ ( n ptr a -- ptr u8 )
   DDL-PTR-U8-SLOT @ ;

: DDL-PTR-U8-SLOT! ( ptr u8 n ptr a -- )
   DDL-PTR-U8-SLOT ! ;

: DDL-NAME-A! ( ptr u8 n -- )
   DDL-NAME-A DDL-PTR-U8-SLOT! ;

: DDL-NAME-A@ ( n -- ptr u8 )
   DDL-NAME-A DDL-PTR-U8-SLOT@ ;

: DDL-FILE-A! ( ptr u8 n -- )
   DDL-FILE-A DDL-PTR-U8-SLOT! ;

: DDL-FILE-A@ ( n -- ptr u8 )
   DDL-FILE-A DDL-PTR-U8-SLOT@ ;

: DDL-SRC-A@ ( -- ptr u8 )
   DDL-SRC-A DDL-PTR-U8@ ;

: DDL-SRC-A! ( ptr u8 -- )
   DDL-SRC-A DDL-PTR-U8! ;

: DDL-FILE-LABEL-A@ ( -- ptr u8 )
   DDL-FILE-LABEL-A DDL-PTR-U8@ ;

: DDL-FILE-LABEL-A! ( ptr u8 -- )
   DDL-FILE-LABEL-A DDL-PTR-U8! ;

: DDL-NAME-U! ( n n -- )
   {: u idx :}
   u idx DDL-NAME-U DDL-TABLE-SLOT ! ;

: DDL-NAME-U@ ( n -- n )
   DDL-NAME-U DDL-TABLE-SLOT @ ;

: DDL-FILE-U! ( n n -- )
   {: u idx :}
   u idx DDL-FILE-U DDL-TABLE-SLOT ! ;

: DDL-FILE-U@ ( n -- n )
   DDL-FILE-U DDL-TABLE-SLOT @ ;

: DDL-LINE! ( n n -- )
   {: line idx :}
   line idx DDL-LINE DDL-TABLE-SLOT ! ;

: DDL-LINE@ ( n -- n )
   DDL-LINE DDL-TABLE-SLOT @ ;

: DDL-COL! ( n n -- )
   {: col idx :}
   col idx DDL-COL DDL-TABLE-SLOT ! ;

: DDL-COL@ ( n -- n )
   DDL-COL DDL-TABLE-SLOT @ ;

: DDL-HASH@ ( n -- n )
   DDL-HASH-TAB DDL-TABLE-SLOT @ ;

: DDL-HASH! ( n n -- )
   {: value idx :}
   value idx DDL-HASH-TAB DDL-TABLE-SLOT ! ;

: DDL-NAME$ ( n -- ptr u8 n ) {: idx :}
   idx DDL-NAME-A@  idx DDL-NAME-U@ ;

: DDL-FILE$ ( n -- ptr u8 n ) {: idx :}
   idx DDL-FILE-A@  idx DDL-FILE-U@ ;

: DDL-JSON! ( bool -- )
   DDL-JSON ! ;

: DDL-OUT-FD! ( fd -- )
   DDL-OUT-FD ! ;

: DDL-WRITE ( n ptr u8 n -- ) {: fd a:ptr u :}
   u 0= if exit then
   fd a u write u <> if s" duplicate-definition-lint: write failed" 74 die then ;

: DDL-OUT ( ptr u8 n -- )
   DDL-OUT-FD @ -rot DDL-WRITE ;

: DDL-C ( n -- )
   DDL-LF-BUF c!
   DDL-LF-BUF 1 DDL-OUT ;

: DDL-NL ( -- )
   DDL-LF DDL-C ;

: DDL-U$ ( n -- ptr u8 n ) {: u:n :}
   DDL-NUM-CAP DDL-NUM-I !
   u 0= if
      DDL-NUM-I @ 1- DDL-NUM-I !
      $30 DDL-NUM DDL-NUM-I @ + c!
      DDL-NUM DDL-NUM-I @ + 1
      exit
   then
   u begin dup 0 > while
      dup 10 mod $30 +
      DDL-NUM-I @ 1- DDL-NUM-I !
      DDL-NUM DDL-NUM-I @ + c!
      10 /
   repeat drop
   DDL-NUM DDL-NUM-I @ + DDL-NUM-CAP DDL-NUM-I @ - ;

: DDL-WORD? ( n -- bool ) {: k :}
   k L# @ >= if LINT-FALSE exit then
   k LK@ L-WORD = ;

: DDL-TOK-END ( n -- n ) {: k :}
   k LB@ k LEX-TOK nip + ;

: DDL-PARSE-NEXT? ( n -- bool ) {: k:n :}
   k LEX-TOK s" char" LINT-STR=CI if LINT-TRUE exit then
   k LEX-TOK s" [char]" LINT-STR=CI if LINT-TRUE exit then
   k LEX-TOK s" '" LINT-STR= if LINT-TRUE exit then
   k LEX-TOK s" [']" LINT-STR= if LINT-TRUE exit then
   k LEX-TOK s" postpone" LINT-STR=CI ;

: DDL-COLON-DEFINER? ( n -- bool ) {: k:n :}
   k LEX-TOK s" :" LINT-STR= if LINT-TRUE exit then
   k LEX-TOK s" +:" LINT-STR= if LINT-TRUE exit then
   k LEX-TOK s" TRUSTED:" LINT-STR=CI if LINT-TRUE exit then
   k LEX-TOK s" KERNEL:" LINT-STR=CI ;

: DDL-DATA-DEFINER? ( n -- bool ) {: k:n :}
   k LEX-TOK s" create" LINT-STR=CI if LINT-TRUE exit then
   k LEX-TOK s" variable" LINT-STR=CI if LINT-TRUE exit then
   k LEX-TOK s" constant" LINT-STR=CI ;

: DDL-UNDEFINE? ( n -- bool ) {: k:n :}
   k LEX-TOK s" undefine" LINT-STR=CI ;

: DDL-MATCH? ( ptr u8 n n -- bool ) {: a:ptr u:n idx:n :}
   a u idx DDL-NAME$ LINT-STR=CI ;

: DDL-HASH-NAME ( ptr u8 n -- n ) {: a:ptr u :}
   $1505 DDL-HASH-H !
   0 DDL-HASH-I !
   begin DDL-HASH-I @ u < while
      DDL-HASH-H @ 5 lshift DDL-HASH-H @ +
      a DDL-HASH-I @ + c@ LINT-FOLD +
      DDL-HASH-MASK and DDL-HASH-H !
      DDL-HASH-I @ 1+ DDL-HASH-I !
   repeat
   DDL-HASH-H @ ;

: DDL-HASH-STEP ( -- )
   DDL-PROBE-I @ 1+ DDL-HASH-MASK and DDL-PROBE-I ! ;

: DDL-FIND ( ptr u8 n -- n ) {: a:ptr u :}
   a u DDL-HASH-NAME DDL-PROBE-I !
   begin
      DDL-PROBE-I @ DDL-HASH@ dup 0= if drop -1 exit then
      1- DDL-CAND !
      a u DDL-CAND @ DDL-MATCH? if DDL-CAND @ exit then
      DDL-HASH-STEP
   again ;

: DDL-HASH-ADD ( ptr u8 n n -- ) {: a:ptr u idx :}
   a u DDL-HASH-NAME DDL-PROBE-I !
   begin
      DDL-PROBE-I @ DDL-HASH@ 0= if
         idx 1+ DDL-PROBE-I @ DDL-HASH!
         exit
      then
      DDL-HASH-STEP
   again ;

: DDL-JSON-FINDING ( n n -- ) {: first k :}
   LJW-RESET
   LJW-OBJECT-START
   s" schema_version" LJW-KEY 1 LJW-U LJW-COMMA
   s" code" LJW-KEY s" E-DUPLICATE-DEFINITION" LJW-STRING LJW-COMMA
   s" file" LJW-KEY DDL-FILE-LABEL-A@ DDL-FILE-LABEL-U @ LJW-STRING LJW-COMMA
   s" line" LJW-KEY k LL@ LJW-U LJW-COMMA
   s" column" LJW-KEY k LC@ LJW-U LJW-COMMA
   s" byte_start" LJW-KEY k LB@ LJW-U LJW-COMMA
   s" byte_end" LJW-KEY k DDL-TOK-END LJW-U LJW-COMMA
   s" word" LJW-KEY k LEX-TOK LJW-STRING LJW-COMMA
   s" first_file" LJW-KEY first DDL-FILE$ LJW-STRING LJW-COMMA
   s" first_line" LJW-KEY first DDL-LINE@ LJW-U LJW-COMMA
   s" first_column" LJW-KEY first DDL-COL@ LJW-U
   LJW-OBJECT-END
   LJW$ DDL-OUT DDL-NL ;

: DDL-TEXT-FINDING ( n n -- ) {: first k :}
   s" E-DUPLICATE-DEFINITION " DDL-OUT
   DDL-FILE-LABEL-A@ DDL-FILE-LABEL-U @ DDL-OUT
   DDL-COLON-C DDL-C k LL@ DDL-U$ DDL-OUT
   DDL-COLON-C DDL-C k LC@ DDL-U$ DDL-OUT
   s" : `" DDL-OUT
   k LEX-TOK DDL-OUT
   s" ` already defined at " DDL-OUT
   first DDL-FILE$ DDL-OUT
   DDL-COLON-C DDL-C first DDL-LINE@ DDL-U$ DDL-OUT
   DDL-COLON-C DDL-C first DDL-COL@ DDL-U$ DDL-OUT
   DDL-NL ;

: DDL-REPORT ( n n -- ) {: first k :}
   DDL-BAD @ 1+ DDL-BAD !
   DDL-JSON @ if first k DDL-JSON-FINDING exit then
   first k DDL-TEXT-FINDING ;

: DDL-ADD-DEF$ ( ptr u8 n n -- ) {: a:ptr u k :}
   DDL-DEF# @ DDL-DEF-CAP >= if s" duplicate-definition-lint: too many definitions" 77 die then
   a DDL-DEF# @ DDL-NAME-A!
   u DDL-DEF# @ DDL-NAME-U!
   DDL-FILE-LABEL-A@ DDL-DEF# @ DDL-FILE-A!
   DDL-FILE-LABEL-U @ DDL-DEF# @ DDL-FILE-U!
   k LL@ DDL-DEF# @ DDL-LINE!
   k LC@ DDL-DEF# @ DDL-COL!
   a u DDL-DEF# @ DDL-HASH-ADD
   DDL-DEF# @ 1+ DDL-DEF# ! ;

: DDL-ADD-DEF ( n -- )
   dup LEX-TOK rot DDL-ADD-DEF$ ;

: DDL-DELETE-DEF ( n -- ) {: k:n :}
   k DDL-WORD? 0= if exit then
   k LEX-TOK DDL-FIND dup 0 >= if 0 swap DDL-NAME-U! else drop then ;

: DDL-CHECK-NAME ( n -- ) {: k:n :}
   k DDL-WORD? 0= if exit then
   k LEX-TOK DDL-FIND dup 0 >= if k DDL-REPORT else drop k DDL-ADD-DEF then ;

: DDL-HANDLE-IN-DEF ( -- )
   DDL-I @ DDL-PARSE-NEXT? if DDL-I @ 1+ DDL-I ! exit then
   DDL-I @ LEX-TOK s" ;" LINT-STR= if 0 DDL-IN-DEF ! then ;

: DDL-HANDLE-TOP ( -- )
   DDL-I @ DDL-COLON-DEFINER? if
      DDL-I @ 1+ DDL-CHECK-NAME
      -1 DDL-IN-DEF !
      exit
   then
   DDL-I @ DDL-UNDEFINE? if
      DDL-I @ 1+ DDL-DELETE-DEF
      DDL-I @ 1+ DDL-I !
      exit
   then
   DDL-I @ DDL-DATA-DEFINER? if DDL-I @ 1+ DDL-CHECK-NAME then ;

: DDL-SCAN-TOKEN ( -- )
   DDL-I @ DDL-WORD? 0= if exit then
   DDL-IN-DEF @ if DDL-HANDLE-IN-DEF else DDL-HANDLE-TOP then ;

: DDL-SCAN ( -- )
   0 DDL-I !
   0 DDL-IN-DEF !
   begin DDL-I @ L# @ < while
      DDL-SCAN-TOKEN
      DDL-I @ 1+ DDL-I !
   repeat ;

: DDL-ALLOC-NEED ( n -- n ) {: n :}
   n 0 <= if 1 exit then
   n ;

: DDL-ALLOC-SOURCE ( n -- )
   DDL-ALLOC-NEED MEM-ALLOC-64K-SPAN DDL-SRC-CAP ! DDL-SRC-A! ;

: DDL-LOAD-SOURCE ( ptr u8 n -- ) {: path:ptr pathu :}
   path pathu FILE-SIZE DDL-ALLOC-SOURCE
   path pathu DDL-SRC-A@ DDL-SRC-CAP @ READ-ALL DDL-SRC-U ! ;

: DDL-ALLOC-TABLE ( n ptr a -- ) {: cap slot:ptr :}
   slot DDL-PTR-A@ 0= if cap >COUNT MEM-ALLOC-CELLS slot DDL-PTR-A! then ;

: DDL-ENSURE-TABLES ( -- )
   DDL-DEF-CAP DDL-NAME-A DDL-ALLOC-TABLE
   DDL-DEF-CAP DDL-NAME-U DDL-ALLOC-TABLE
   DDL-DEF-CAP DDL-FILE-A DDL-ALLOC-TABLE
   DDL-DEF-CAP DDL-FILE-U DDL-ALLOC-TABLE
   DDL-DEF-CAP DDL-LINE DDL-ALLOC-TABLE
   DDL-DEF-CAP DDL-COL DDL-ALLOC-TABLE
   DDL-HASH-CAP DDL-HASH-TAB DDL-ALLOC-TABLE ;

: DUPLICATE-DEFINITION-LINT-RESET ( -- )
   DDL-ENSURE-TABLES
   0 DDL-BAD !
   0 DDL-DEF# !
   0 begin dup DDL-HASH-CAP < while
      0 over DDL-HASH!
      1+
   repeat drop
   0 DDL-JSON !
   1 >FD DDL-OUT-FD! ;

: DUPLICATE-DEFINITION-LINT-FILE-AS ( ptr u8 n ptr u8 n -- )
   {: path:ptr pathu label:ptr labelu :}
   label DDL-FILE-LABEL-A! labelu DDL-FILE-LABEL-U !
   path pathu DDL-LOAD-SOURCE
   DDL-SRC-A@ DDL-SRC-U @ LEX-SOURCE
   DDL-SCAN ;

: DUPLICATE-DEFINITION-LINT-FILE ( ptr u8 n -- )
   2dup DUPLICATE-DEFINITION-LINT-FILE-AS ;

: DUPLICATE-DEFINITION-LINT-FINISH ( -- )
   DDL-BAD @ 0 > if 1 throw then ;
