\ duplicate-definition-lint-core.f - report duplicate flat source definitions.
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/fs.f,
\ tools/lint/text.f, tools/lint/token.f, tools/lint/lib.f,
\ and tools/lint/json-writer.f.

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
variable DDL-HASH-I
variable DDL-HASH-H
variable DDL-PROBE-I
variable DDL-CAND
variable DDL-IN-DEF
variable DDL-JSON
variable DDL-OUT-FD
variable DDL-NUM-I
variable DDL-SCAN-I
variable DDL-SCAN-LINE
variable DDL-SCAN-COL
variable DDL-TOK-A
variable DDL-TOK-U
variable DDL-TOK-BYTE
variable DDL-TOK-LINE
variable DDL-TOK-COL

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
   fd a u LINT-OUT-WRITE ;

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

: DDL-TOK-A@ ( -- ptr u8 )
   DDL-TOK-A DDL-PTR-U8@ ;

: DDL-TOK-A! ( ptr u8 -- )
   DDL-TOK-A DDL-PTR-U8! ;

: DDL-TOK$ ( -- ptr u8 n )
   DDL-TOK-A@ DDL-TOK-U @ ;

: DDL-TOK-END ( -- n )
   DDL-TOK-BYTE @ DDL-TOK-U @ + ;

: DDL-SCAN-END? ( -- bool )
   DDL-SCAN-I @ DDL-SRC-U @ >= ;

: DDL-SCAN-C@ ( -- n )
   DDL-SRC-A@ DDL-SCAN-I @ + c@ ;

: DDL-SCAN-ADV ( -- n )
   DDL-SCAN-C@
   DDL-SCAN-I @ 1+ DDL-SCAN-I !
   dup DDL-LF = if
      DDL-SCAN-LINE @ 1+ DDL-SCAN-LINE !
      1 DDL-SCAN-COL !
   else
      DDL-SCAN-COL @ 1+ DDL-SCAN-COL !
   then ;

: DDL-SKIP-LINE-COMMENT ( -- )
   begin DDL-SCAN-END? 0= DDL-SCAN-C@ DDL-LF <> and while
      DDL-SCAN-ADV drop
   repeat ;

: DDL-SKIP-PAREN-COMMENT ( -- )
   DDL-SCAN-ADV drop
   begin DDL-SCAN-END? 0= DDL-SCAN-C@ 41 <> and while
      DDL-SCAN-ADV drop
   repeat
   DDL-SCAN-END? 0= if DDL-SCAN-ADV drop then ;

: DDL-STRING-OPENER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 2 <> if LINT-FALSE exit then
   a 1+ c@ DQUOTE <> if LINT-FALSE exit then
   a c@ LINT-FOLD 115 = if LINT-TRUE exit then
   a c@ DOT = if LINT-TRUE exit then
   a c@ LINT-FOLD 99 = ;

: DDL-SKIP-QUOTE ( -- )
   begin DDL-SCAN-END? 0= while
      DDL-SCAN-ADV DQUOTE = if exit then
   repeat ;

: DDL-MARK-WORD ( -- )
   DDL-SRC-A@ DDL-SCAN-I @ + DDL-TOK-A!
   DDL-SCAN-I @ DDL-TOK-BYTE !
   DDL-SCAN-LINE @ DDL-TOK-LINE !
   DDL-SCAN-COL @ DDL-TOK-COL ! ;

: DDL-SCAN-RAW-WORD ( -- )
   DDL-MARK-WORD
   begin DDL-SCAN-END? 0= DDL-SCAN-C@ LINT-WS? 0= and while
      DDL-SCAN-ADV drop
   repeat
   DDL-SCAN-I @ DDL-TOK-BYTE @ - DDL-TOK-U ! ;

: DDL-SCAN-WORD ( -- )
   DDL-SCAN-RAW-WORD
   DDL-TOK$ DDL-STRING-OPENER? if DDL-SKIP-QUOTE then ;

: DDL-SKIP-WS ( -- )
   begin DDL-SCAN-END? 0= DDL-SCAN-C@ LINT-WS? and while
      DDL-SCAN-ADV drop
   repeat ;

: DDL-NEXT-RAW-WORD? ( -- bool )
   DDL-SKIP-WS
   DDL-SCAN-END? if LINT-FALSE exit then
   DDL-SCAN-RAW-WORD
   LINT-TRUE ;

: DDL-NEXT-TOKEN? ( -- bool )
   begin DDL-SCAN-END? 0= while
      DDL-SCAN-C@ LINT-WS? if DDL-SCAN-ADV drop
      else DDL-SCAN-C@ 92 = if DDL-SKIP-LINE-COMMENT
      else DDL-SCAN-C@ 40 = if DDL-SKIP-PAREN-COMMENT
      else DDL-SCAN-WORD LINT-TRUE exit then then then
   repeat
   LINT-FALSE ;

: DDL-PARSE-NEXT? ( -- bool )
   DDL-TOK$ s" char" LINT-STR=CI if LINT-TRUE exit then
   DDL-TOK$ s" [char]" LINT-STR=CI if LINT-TRUE exit then
   DDL-TOK$ s" '" LINT-STR= if LINT-TRUE exit then
   DDL-TOK$ s" [']" LINT-STR= if LINT-TRUE exit then
   DDL-TOK$ s" postpone" LINT-STR=CI ;

: DDL-COLON-DEFINER? ( -- bool )
   DDL-TOK$ s" :" LINT-STR= if LINT-TRUE exit then
   DDL-TOK$ s" +:" LINT-STR= if LINT-TRUE exit then
   DDL-TOK$ s" TRUSTED:" LINT-STR=CI if LINT-TRUE exit then
   DDL-TOK$ s" KERNEL:" LINT-STR=CI ;

: DDL-DATA-DEFINER? ( -- bool )
   DDL-TOK$ s" create" LINT-STR=CI if LINT-TRUE exit then
   DDL-TOK$ s" variable" LINT-STR=CI if LINT-TRUE exit then
   DDL-TOK$ s" constant" LINT-STR=CI ;

: DDL-UNDEFINE? ( -- bool )
   DDL-TOK$ s" undefine" LINT-STR=CI ;

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

: DDL-JSON-FINDING ( n -- ) {: first:n :}
   LJW-RESET
   LJW-OBJECT-START
   s" schema_version" LJW-KEY 1 LJW-U LJW-COMMA
   s" code" LJW-KEY s" E-DUPLICATE-DEFINITION" LJW-STRING LJW-COMMA
   s" file" LJW-KEY DDL-FILE-LABEL-A@ DDL-FILE-LABEL-U @ LJW-STRING LJW-COMMA
   s" line" LJW-KEY DDL-TOK-LINE @ LJW-U LJW-COMMA
   s" column" LJW-KEY DDL-TOK-COL @ LJW-U LJW-COMMA
   s" byte_start" LJW-KEY DDL-TOK-BYTE @ LJW-U LJW-COMMA
   s" byte_end" LJW-KEY DDL-TOK-END LJW-U LJW-COMMA
   s" word" LJW-KEY DDL-TOK$ LJW-STRING LJW-COMMA
   s" first_file" LJW-KEY first DDL-FILE$ LJW-STRING LJW-COMMA
   s" first_line" LJW-KEY first DDL-LINE@ LJW-U LJW-COMMA
   s" first_column" LJW-KEY first DDL-COL@ LJW-U
   LJW-OBJECT-END
   LJW$ DDL-OUT DDL-NL ;

: DDL-TEXT-FINDING ( n -- ) {: first:n :}
   s" E-DUPLICATE-DEFINITION " DDL-OUT
   DDL-FILE-LABEL-A@ DDL-FILE-LABEL-U @ DDL-OUT
   DDL-COLON-C DDL-C DDL-TOK-LINE @ DDL-U$ DDL-OUT
   DDL-COLON-C DDL-C DDL-TOK-COL @ DDL-U$ DDL-OUT
   s" : `" DDL-OUT
   DDL-TOK$ DDL-OUT
   s" ` already defined at " DDL-OUT
   first DDL-FILE$ DDL-OUT
   DDL-COLON-C DDL-C first DDL-LINE@ DDL-U$ DDL-OUT
   DDL-COLON-C DDL-C first DDL-COL@ DDL-U$ DDL-OUT
   DDL-NL ;

: DDL-REPORT ( n -- ) {: first:n :}
   DDL-BAD @ 1+ DDL-BAD !
   DDL-JSON @ if first DDL-JSON-FINDING exit then
   first DDL-TEXT-FINDING ;

: DDL-ADD-DEF$ ( ptr u8 n -- ) {: a:ptr u:n :}
   DDL-DEF# @ DDL-DEF-CAP >= if s" duplicate-definition-lint: too many definitions" 77 die then
   a DDL-DEF# @ DDL-NAME-A!
   u DDL-DEF# @ DDL-NAME-U!
   DDL-FILE-LABEL-A@ DDL-DEF# @ DDL-FILE-A!
   DDL-FILE-LABEL-U @ DDL-DEF# @ DDL-FILE-U!
   DDL-TOK-LINE @ DDL-DEF# @ DDL-LINE!
   DDL-TOK-COL @ DDL-DEF# @ DDL-COL!
   a u DDL-DEF# @ DDL-HASH-ADD
   DDL-DEF# @ 1+ DDL-DEF# ! ;

: DDL-ADD-DEF ( -- )
   DDL-TOK$ DDL-ADD-DEF$ ;

: DDL-DELETE-DEF ( -- )
   DDL-TOK$ DDL-FIND dup 0 >= if 0 swap DDL-NAME-U! else drop then ;

: DDL-CHECK-NAME ( -- )
   DDL-TOK$ DDL-FIND dup 0 >= if DDL-REPORT else drop DDL-ADD-DEF then ;

: DDL-HANDLE-IN-DEF ( -- )
   DDL-PARSE-NEXT? if DDL-NEXT-RAW-WORD? drop exit then
   DDL-TOK$ s" ;" LINT-STR= if 0 DDL-IN-DEF ! then ;

: DDL-HANDLE-TOP ( -- )
   DDL-COLON-DEFINER? if
      DDL-NEXT-RAW-WORD? if DDL-CHECK-NAME then
      -1 DDL-IN-DEF !
      exit
   then
   DDL-UNDEFINE? if
      DDL-NEXT-RAW-WORD? if DDL-DELETE-DEF then
      exit
   then
   DDL-DATA-DEFINER? if DDL-NEXT-RAW-WORD? if DDL-CHECK-NAME then then ;

: DDL-SCAN-TOKEN ( -- )
   DDL-IN-DEF @ if DDL-HANDLE-IN-DEF else DDL-HANDLE-TOP then ;

: DDL-SCAN ( -- )
   0 DDL-SCAN-I !
   1 DDL-SCAN-LINE !
   1 DDL-SCAN-COL !
   0 DDL-IN-DEF !
   begin DDL-NEXT-TOKEN? while
      DDL-SCAN-TOKEN
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
   DDL-SCAN ;

: DUPLICATE-DEFINITION-LINT-FILE ( ptr u8 n -- )
   2dup DUPLICATE-DEFINITION-LINT-FILE-AS ;

: DUPLICATE-DEFINITION-LINT-FINISH ( -- )
   DDL-BAD @ 0 > if 1 throw then ;
