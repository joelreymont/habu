\ stdlib-manifest-test.f - validate lib/std.manifest without host text tools.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/memory.f lib/fs.f
\ lib/process.f lib/process-argv.f tools/lint/text.f tools/lint/token.f
\ tools/lint/lib.f tools/stdlib-manifest-test.f

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/process.f
require lib/process-argv.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f

$40000 constant SMT-MAN-CAP
$20000 constant SMT-DOC-CAP
$80000 constant SMT-PUB-CAP                 \ public-signatures JSON for the whole stdlib
$4000 constant SMT-ERR-CAP
$80000 constant SMT-STR-CAP
1536 constant SMT-WORD-MAX
64 constant SMT-MOD-MAX
64 constant SMT-LIB-MAX
11 constant SMT-FIELDS
$7530 constant SMT-TIMEOUT-MS

9 constant SMT-TAB
10 constant SMT-LF
45 constant SMT-DASH
48 constant SMT-ZERO
47 constant SMT-SLASH
58 constant SMT-COLON
96 constant SMT-BEFORE-A
123 constant SMT-AFTER-Z
34 constant SMT-DQ

variable SMT-MAN-BUF-A
variable SMT-DOC-BUF-A
variable SMT-PUB-BUF-A
variable SMT-ERR-BUF-A
variable SMT-STR-BUF-A

: SMT-PTR-U8-FIELD ( ptr a -- ptr ptr u8 )
   0 ptr-field ;

: SMT-PTR-U8@ ( ptr a -- ptr u8 )
   SMT-PTR-U8-FIELD @ ;

: SMT-PTR-U8! ( ptr u8 ptr a -- )
   SMT-PTR-U8-FIELD ! ;

: SMT-ALLOC-BUF ( n -- ptr u8 )
   MEM-ALLOC-BYTES drop ;

: SMT-BUF ( ptr a n -- ptr u8 ) {: slot:ptr cap :}
   slot @ 0= if cap SMT-ALLOC-BUF slot SMT-PTR-U8! then
   slot SMT-PTR-U8@ ;

: SMT-MAN-BUF ( -- ptr u8 )
   SMT-MAN-BUF-A SMT-MAN-CAP SMT-BUF ;

: SMT-DOC-BUF ( -- ptr u8 )
   SMT-DOC-BUF-A SMT-DOC-CAP SMT-BUF ;

: SMT-PUB-BUF ( -- ptr u8 )
   SMT-PUB-BUF-A SMT-PUB-CAP SMT-BUF ;

: SMT-ERR-BUF ( -- ptr u8 )
   SMT-ERR-BUF-A SMT-ERR-CAP SMT-BUF ;

: SMT-STR-BUF ( -- ptr u8 )
   SMT-STR-BUF-A SMT-STR-CAP SMT-BUF ;

create SMT-ONE 1 allot
create SMT-NUM 32 allot
create SMT-FIELD-O SMT-FIELDS cells allot
create SMT-FIELD-L SMT-FIELDS cells allot

create SMT-MF-O SMT-WORD-MAX cells allot
create SMT-MF-L SMT-WORD-MAX cells allot
create SMT-MW-O SMT-WORD-MAX cells allot
create SMT-MW-L SMT-WORD-MAX cells allot
create SMT-ME-O SMT-WORD-MAX cells allot
create SMT-ME-L SMT-WORD-MAX cells allot

create SMT-PF-O SMT-WORD-MAX cells allot
create SMT-PF-L SMT-WORD-MAX cells allot
create SMT-PW-O SMT-WORD-MAX cells allot
create SMT-PW-L SMT-WORD-MAX cells allot
create SMT-PE-O SMT-WORD-MAX cells allot
create SMT-PE-L SMT-WORD-MAX cells allot

create SMT-MOD-F-O SMT-MOD-MAX cells allot
create SMT-MOD-F-L SMT-MOD-MAX cells allot
create SMT-LIB-F-O SMT-LIB-MAX cells allot
create SMT-LIB-F-L SMT-LIB-MAX cells allot

variable SMT-MAN-U
variable SMT-DOC-U
variable SMT-PUB-U
variable SMT-STR-U
variable SMT-FIELD-N
variable SMT-BAD
variable SMT-NUM-U
variable SMT-MAN-N
variable SMT-PUB-N
variable SMT-MOD-N
variable SMT-LIB-N
variable SMT-FIRST
variable SMT-END
variable SMT-JSON-A
variable SMT-JSON-U
variable SMT-JSON-NEXT
variable SMT-JSON-BODY
variable SMT-JSON-END
variable SMT-I
variable SMT-J

: SMT-C! ( u8 -- )
   SMT-ONE c! ;

: SMT-ERR ( ptr u8 n -- )
   dup 0= IF 2drop exit THEN
   2 -rot write drop ;

: SMT-OUT ( ptr u8 n -- )
   dup 0= IF 2drop exit THEN
   1 -rot write drop ;

: SMT-ERR-C ( u8 -- )
   SMT-C!
   SMT-ONE 1 SMT-ERR ;

: SMT-NL ( -- )
   SMT-LF SMT-ERR-C ;

: SMT-BAD+ ( -- )
   SMT-BAD @ 1+ SMT-BAD ! ;

: SMT-UERR ( n -- )
   0 SMT-NUM-U !
   dup 0= IF drop SMT-ZERO SMT-ERR-C exit THEN
   begin dup 0 > while
      dup 10 mod SMT-ZERO + SMT-NUM SMT-NUM-U @ + c!
      10 /
      SMT-NUM-U @ 1+ SMT-NUM-U !
   repeat drop
   begin SMT-NUM-U @ 0 > while
      SMT-NUM-U @ 1- SMT-NUM-U !
      SMT-NUM SMT-NUM-U @ + c@ SMT-ERR-C
   repeat ;

: SMT-FINDING ( ptr u8 n -- )
   s" stdlib-manifest-test: " SMT-ERR
   SMT-ERR
   SMT-NL
   SMT-BAD+ ;

: SMT-ROW-FINDING ( n ptr u8 n -- ) {: line msg:ptr mu :}
   s" lib/std.manifest:" SMT-ERR
   line SMT-UERR
   s" : " SMT-ERR
   msg mu SMT-ERR
   SMT-NL
   SMT-BAD+ ;

: SMT-ROW-FINDING$ ( n ptr u8 n ptr u8 n -- )
   {: line msg:ptr mu val:ptr vu :}
   s" lib/std.manifest:" SMT-ERR
   line SMT-UERR
   s" : " SMT-ERR
   msg mu SMT-ERR
   s" `" SMT-ERR
   val vu SMT-ERR
   s" `" SMT-ERR
   SMT-NL
   SMT-BAD+ ;

: SMT-DIE ( -- )
   SMT-BAD @ 0 > IF s" stdlib-manifest-test: failed" 76 die THEN ;

: SMT-A! ( n ptr a n -- ) {: x arr:ptr idx :}
   x arr idx cells + ! ;

: SMT-A@ ( ptr a n -- n ) {: arr:ptr idx :}
   arr idx cells + @ ;

: SMT-STORE$ ( ptr u8 n -- n n ) {: a:ptr u :}
   SMT-STR-U @ u + SMT-STR-CAP > IF s" string store overflow" SMT-FINDING SMT-DIE THEN
   a SMT-STR-BUF SMT-STR-U @ + u BYTE-COPY
   SMT-STR-U @ u
   SMT-STR-U @ u + SMT-STR-U ! ;

: SMT-STR$ ( n n -- ptr u8 n ) {: off len :}
   SMT-STR-BUF off + len ;

: SMT-FIELD-O! ( n n -- ) {: off idx :}
   off SMT-FIELD-O idx SMT-A! ;

: SMT-FIELD-L! ( n n -- ) {: len idx :}
   len SMT-FIELD-L idx SMT-A! ;

: SMT-FIELD$ ( ptr u8 n -- ptr u8 n ) {: line:ptr idx :}
   line SMT-FIELD-O idx SMT-A@ +
   SMT-FIELD-L idx SMT-A@ ;

: SMT-SAVE-FIELD ( n n -- ) {: start pos :}
   SMT-FIELD-N @ SMT-FIELDS >= IF exit THEN
   start SMT-FIELD-N @ SMT-FIELD-O!
   pos start - SMT-FIELD-N @ SMT-FIELD-L!
   SMT-FIELD-N @ 1+ SMT-FIELD-N ! ;

: SMT-SPLIT-FIELDS ( ptr u8 n -- ) {: a:ptr u :}
   0 SMT-FIELD-N !
   0 SMT-I !
   0 SMT-J !
   begin SMT-I @ u < while
      a SMT-I @ + c@ SMT-TAB = IF
         SMT-J @ SMT-I @ SMT-SAVE-FIELD
         SMT-I @ 1+ SMT-J !
      THEN
      SMT-I @ 1+ SMT-I !
   repeat
   SMT-J @ u SMT-SAVE-FIELD ;

: SMT-LINE-LEN ( ptr u8 n -- ptr u8 n )
   dup 0 > IF
      2dup + 1- c@ 13 = IF 1- THEN
   THEN ;

: SMT-LINE-END ( ptr u8 n n -- n ) {: a:ptr u start :}
   start begin dup u < while
      dup a + c@ SMT-LF = IF exit THEN
      1+
   repeat ;

: SMT-LOWER? ( n -- bool )
   dup SMT-BEFORE-A > swap SMT-AFTER-Z < and ;

: SMT-DIGIT? ( n -- bool )
   dup 47 > swap 58 < and ;

: SMT-MODULE-TAIL? ( n -- bool )
   dup SMT-LOWER? swap dup SMT-DIGIT? swap SMT-DASH = or or ;

: SMT-MODULE-NAME? ( ptr u8 n -- bool ) {: a:ptr u :}
   u 0= IF 0 0= 0= exit THEN
   a c@ SMT-LOWER? 0= IF 0 0= 0= exit THEN
   1 begin dup u < while
      dup a + c@ SMT-MODULE-TAIL? 0= IF drop 0 0= 0= exit THEN
      1+
   repeat drop 0 0= ;

: SMT-CORE-FILE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 11 < IF 0 0= 0= exit THEN
   a u s" src/core/" LINT-STARTS-WITH? 0= IF 0 0= 0= exit THEN
   a u s" .f" LINT-ENDS-WITH? 0= IF 0 0= 0= exit THEN
   a 9 + u 11 - SMT-MODULE-NAME? ;

: SMT-PATH-SEP? ( ptr u8 n n -- bool ) {: a:ptr u:n i:n :}
   i 0 = IF 0 0= 0= exit THEN
   i u 1- = IF 0 0= 0= exit THEN
   a i 1- + c@ SMT-SLASH = IF 0 0= 0= exit THEN
   0 0= ;

: SMT-MODULE-PATH-CHAR? ( ptr u8 n n -- bool ) {: a:ptr u:n i:n :}
   a i + c@ {: c:n :}
   c SMT-SLASH = IF a u i SMT-PATH-SEP? exit THEN
   i 0 = IF c SMT-LOWER? exit THEN
   a i 1- + c@ SMT-SLASH = IF c SMT-LOWER? exit THEN
   c SMT-MODULE-TAIL? ;

: SMT-MODULE-PATH? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0= IF 0 0= 0= exit THEN
   0 begin dup u < while
      dup a u rot SMT-MODULE-PATH-CHAR? 0= IF drop 0 0= 0= exit THEN
      1+
   repeat drop 0 0= ;

: SMT-LIB-FILE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u SMT-CORE-FILE? IF 0 0= exit THEN
   u 7 < IF 0 0= 0= exit THEN
   a u s" lib/" LINT-STARTS-WITH? 0= IF 0 0= 0= exit THEN
   a u s" .f" LINT-ENDS-WITH? 0= IF 0 0= 0= exit THEN
   a 4 + u 6 - SMT-MODULE-PATH? ;

: SMT-KIND? ( ptr u8 n -- bool )
   2dup s" module" LINT-STR= IF 2drop 0 0= exit THEN
   s" word" LINT-STR= ;

: SMT-STATUS? ( ptr u8 n -- bool )
   2dup s" planned" LINT-STR= IF 2drop 0 0= exit THEN
   2dup s" active" LINT-STR= IF 2drop 0 0= exit THEN
   s" published" LINT-STR= ;

: SMT-NOT-PLANNED? ( ptr u8 n -- bool )
   s" planned" LINT-STR= 0= ;

: SMT-MAN-F$ ( n -- ptr u8 n ) {: k :}
   SMT-MF-O k SMT-A@ SMT-MF-L k SMT-A@ SMT-STR$ ;

: SMT-MAN-W$ ( n -- ptr u8 n ) {: k :}
   SMT-MW-O k SMT-A@ SMT-MW-L k SMT-A@ SMT-STR$ ;

: SMT-MAN-E$ ( n -- ptr u8 n ) {: k :}
   SMT-ME-O k SMT-A@ SMT-ME-L k SMT-A@ SMT-STR$ ;

: SMT-PUB-F$ ( n -- ptr u8 n ) {: k :}
   SMT-PF-O k SMT-A@ SMT-PF-L k SMT-A@ SMT-STR$ ;

: SMT-PUB-W$ ( n -- ptr u8 n ) {: k :}
   SMT-PW-O k SMT-A@ SMT-PW-L k SMT-A@ SMT-STR$ ;

: SMT-PUB-E$ ( n -- ptr u8 n ) {: k :}
   SMT-PE-O k SMT-A@ SMT-PE-L k SMT-A@ SMT-STR$ ;

: SMT-MOD-F$ ( n -- ptr u8 n ) {: k :}
   SMT-MOD-F-O k SMT-A@ SMT-MOD-F-L k SMT-A@ SMT-STR$ ;

: SMT-LIB-F$ ( n -- ptr u8 n ) {: k :}
   SMT-LIB-F-O k SMT-A@ SMT-LIB-F-L k SMT-A@ SMT-STR$ ;

: SMT-MAN-WORD= ( n ptr u8 n ptr u8 n ptr u8 n -- bool )
   {: k fa:ptr fu wa:ptr wu ea:ptr eu :}
   k SMT-MAN-F$ fa fu LINT-STR= 0= IF 0 0= 0= exit THEN
   k SMT-MAN-W$ wa wu LINT-STR= 0= IF 0 0= 0= exit THEN
   k SMT-MAN-E$ ea eu LINT-STR= ;

: SMT-PUB-WORD= ( n ptr u8 n ptr u8 n ptr u8 n -- bool )
   {: k fa:ptr fu wa:ptr wu ea:ptr eu :}
   k SMT-PUB-F$ fa fu LINT-STR= 0= IF 0 0= 0= exit THEN
   k SMT-PUB-W$ wa wu LINT-STR= 0= IF 0 0= 0= exit THEN
   k SMT-PUB-E$ ea eu LINT-STR= ;

: SMT-FIND-MAN-WORD ( ptr u8 n ptr u8 n ptr u8 n -- n )
   {: fa:ptr fu wa:ptr wu ea:ptr eu :}
   0 begin dup SMT-MAN-N @ < while
      dup fa fu wa wu ea eu SMT-MAN-WORD= IF exit THEN
      1+
   repeat drop -1 ;

: SMT-FIND-PUB-WORD ( ptr u8 n ptr u8 n ptr u8 n -- n )
   {: fa:ptr fu wa:ptr wu ea:ptr eu :}
   0 begin dup SMT-PUB-N @ < while
      dup fa fu wa wu ea eu SMT-PUB-WORD= IF exit THEN
      1+
   repeat drop -1 ;

: SMT-FIND-MOD-FILE ( ptr u8 n -- n ) {: fa:ptr fu :}
   0 begin dup SMT-MOD-N @ < while
      dup SMT-MOD-F$ fa fu LINT-STR= IF exit THEN
      1+
   repeat drop -1 ;

: SMT-FIND-LIB-FILE ( ptr u8 n -- n ) {: fa:ptr fu :}
   0 begin dup SMT-LIB-N @ < while
      dup SMT-LIB-F$ fa fu LINT-STR= IF exit THEN
      1+
   repeat drop -1 ;

: SMT-ADD-MAN-WORD ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: fa:ptr fu wa:ptr wu ea:ptr eu :}
   fa fu wa wu ea eu SMT-FIND-MAN-WORD 0 >= IF
      s" duplicate manifest word row" SMT-FINDING
   THEN
   SMT-MAN-N @ SMT-WORD-MAX >= IF s" too many manifest word rows" SMT-FINDING SMT-DIE THEN
   fa fu SMT-STORE$ SMT-MF-L SMT-MAN-N @ SMT-A! SMT-MF-O SMT-MAN-N @ SMT-A!
   wa wu SMT-STORE$ SMT-MW-L SMT-MAN-N @ SMT-A! SMT-MW-O SMT-MAN-N @ SMT-A!
   ea eu SMT-STORE$ SMT-ME-L SMT-MAN-N @ SMT-A! SMT-ME-O SMT-MAN-N @ SMT-A!
   SMT-MAN-N @ 1+ SMT-MAN-N ! ;

: SMT-ADD-PUB-WORD ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: fa:ptr fu wa:ptr wu ea:ptr eu :}
   SMT-PUB-N @ SMT-WORD-MAX >= IF s" too many public word rows" SMT-FINDING SMT-DIE THEN
   fa fu SMT-STORE$ SMT-PF-L SMT-PUB-N @ SMT-A! SMT-PF-O SMT-PUB-N @ SMT-A!
   wa wu SMT-STORE$ SMT-PW-L SMT-PUB-N @ SMT-A! SMT-PW-O SMT-PUB-N @ SMT-A!
   ea eu SMT-STORE$ SMT-PE-L SMT-PUB-N @ SMT-A! SMT-PE-O SMT-PUB-N @ SMT-A!
   SMT-PUB-N @ 1+ SMT-PUB-N ! ;

: SMT-ADD-MOD-FILE ( ptr u8 n -- ) {: fa:ptr fu :}
   fa fu SMT-FIND-MOD-FILE 0 >= IF s" duplicate module row" SMT-FINDING THEN
   SMT-MOD-N @ SMT-MOD-MAX >= IF s" too many module rows" SMT-FINDING SMT-DIE THEN
   fa fu SMT-STORE$ SMT-MOD-F-L SMT-MOD-N @ SMT-A! SMT-MOD-F-O SMT-MOD-N @ SMT-A!
   SMT-MOD-N @ 1+ SMT-MOD-N ! ;

: SMT-ADD-LIB-FILE ( ptr u8 n -- ) {: fa:ptr fu :}
   fa fu SMT-FIND-LIB-FILE 0 >= IF exit THEN
   SMT-LIB-N @ SMT-LIB-MAX >= IF s" too many lib files" SMT-FINDING SMT-DIE THEN
   fa fu SMT-STORE$ SMT-LIB-F-L SMT-LIB-N @ SMT-A! SMT-LIB-F-O SMT-LIB-N @ SMT-A!
   SMT-LIB-N @ 1+ SMT-LIB-N ! ;

: SMT-CHECK-DOC-ROW ( ptr u8 n -- ) {: a:ptr u :}
   SMT-DOC-BUF SMT-DOC-U @ a u LINT-CONTAINS? 0= IF
      s" missing stdlib doc contract: " SMT-ERR
      a u SMT-ERR
      SMT-NL
      SMT-BAD+
   THEN ;

: SMT-DOC-HANDLE-ROWS ( -- )
   s" ## Handle Representation" SMT-CHECK-DOC-ROW
   s" v1 memory-backed handles use `ptr u8 n`" SMT-CHECK-DOC-ROW
   s" fixed-capacity map slot storage uses" SMT-CHECK-DOC-ROW
   s" Opaque `addr` values are boundary-only" SMT-CHECK-DOC-ROW ;

: SMT-DOC-STRING-ROWS ( -- )
   s" ## LLM Surface" SMT-CHECK-DOC-ROW
   s" ## Core Bytes" SMT-CHECK-DOC-ROW
   s" `src/core/bytes.f`" SMT-CHECK-DOC-ROW
   s" ## String" SMT-CHECK-DOC-ROW
   s" BYTE-COPY       ( ptr u8 ptr u8 n -- )" SMT-CHECK-DOC-ROW
   s" SB-APPEND       ( ptr u8 n -- )" SMT-CHECK-DOC-ROW ;

: SMT-DOC-REGEX-ROWS ( -- )
   s" ## Regex" SMT-CHECK-DOC-ROW
   s" RX-COMPILE            ( ptr u8 len ptr u8 len -- len )" SMT-CHECK-DOC-ROW
   s" RX-MATCH?             ( ptr u8 len ptr u8 len -- bool )" SMT-CHECK-DOC-ROW
   s" RX-FIND               ( ptr u8 len ptr u8 len -- off len bool )" SMT-CHECK-DOC-ROW
   s" RX-COUNT              ( ptr u8 len ptr u8 len -- count )" SMT-CHECK-DOC-ROW ;

: SMT-DOC-MAP-ROWS ( -- )
   s" ## Map" SMT-CHECK-DOC-ROW
   s" MAP-CELLS           ( count -- count )" SMT-CHECK-DOC-ROW
   s" MAP-SET     ( n ptr a count ptr u8 len -- )" SMT-CHECK-DOC-ROW ;

: SMT-DOC-SOURCE-ROWS ( -- )
   s" ## Source Materialization" SMT-CHECK-DOC-ROW
   s" READ-STDIN-ALL                 ( ptr u8 len -- len )" SMT-CHECK-DOC-ROW
   s" CONCAT-FILES                   ( ptr a ptr a count ptr u8 len -- len )" SMT-CHECK-DOC-ROW
   s" COMMENT-EXPORTS                ( ptr u8 len ptr u8 len -- len )" SMT-CHECK-DOC-ROW ;

: SMT-DOC-FS-ROWS ( -- )
   s" ## Files" SMT-CHECK-DOC-ROW
   s" WALK-FILES   ( ptr u8 n [ ptr u8 n -- ] -- )" SMT-CHECK-DOC-ROW
   s" depth-first" SMT-CHECK-DOC-ROW
   s" per-depth recursion buffers" SMT-CHECK-DOC-ROW ;

: SMT-DOC-PROC-CAPTURE-ROWS ( -- )
   s" ## Processes" SMT-CHECK-DOC-ROW
   s" RUN-CAPTURE  ( ptr u8 len ptr u8 len ptr u8 len ms -- len len rc )" SMT-CHECK-DOC-ROW
   s" counted paths/commands" SMT-CHECK-DOC-ROW
   s" FD_CLOEXEC" SMT-CHECK-DOC-ROW ;

: SMT-DOC-PROC-ERROR-ROWS ( -- )
   s" rc in that order" SMT-CHECK-DOC-ROW
   s" E-PROC-TRUNCATED" SMT-CHECK-DOC-ROW
   s" E-PROC-TIMEOUT" SMT-CHECK-DOC-ROW ;

: SMT-DOC-PROC-ROWS ( -- )
   SMT-DOC-PROC-CAPTURE-ROWS
   SMT-DOC-PROC-ERROR-ROWS ;

: SMT-DOC-ARGV-CORE-ROWS ( -- )
   s" ## Argv" SMT-CHECK-DOC-ROW
   s" ARGV-PARSE              ( -- )" SMT-CHECK-DOC-ROW
   s" ARGV-EXPECT-POS-EXACT   ( n -- )" SMT-CHECK-DOC-ROW
   s" ARGV-OUT$               ( -- ptr u8 n )" SMT-CHECK-DOC-ROW ;

: SMT-DOC-ARGV-BOUNDARY-ROWS ( -- )
   s" `--`" SMT-CHECK-DOC-ROW
   s" ARGV-E-USAGE" SMT-CHECK-DOC-ROW ;

: SMT-DOC-ARGV-ROWS ( -- )
   SMT-DOC-ARGV-CORE-ROWS
   SMT-DOC-ARGV-BOUNDARY-ROWS ;

: SMT-DOC-TEST-ASSERT-ROWS ( -- )
   s" ## Test Property And Build Helpers" SMT-CHECK-DOC-ROW
   s" T-RESET         ( -- )" SMT-CHECK-DOC-ROW
   s" T=              ( n n -- )" SMT-CHECK-DOC-ROW
   s" T-REPORT        ( -- )" SMT-CHECK-DOC-ROW ;

: SMT-DOC-TEST-THROW-ROWS ( -- )
   s" TTHROWS         ( a n -- )" SMT-CHECK-DOC-ROW
   s" TTHROWSQ        ( [ -- ] n -- )" SMT-CHECK-DOC-ROW ;

: SMT-DOC-TEST-ROWS ( -- )
   SMT-DOC-TEST-ASSERT-ROWS
   SMT-DOC-TEST-THROW-ROWS ;

: SMT-DOC-BUILD-ROWS ( -- )
   s" PROP-RUN-RESET  ( n n -- )" SMT-CHECK-DOC-ROW
   s" PROP-SHRINK     ( [ -- bool ] -- )" SMT-CHECK-DOC-ROW
   s" BUILD-STEP      ( ptr u8 n [ -- n ] -- )" SMT-CHECK-DOC-ROW
   s" `evaluate`" SMT-CHECK-DOC-ROW
   s" source-string generation" SMT-CHECK-DOC-ROW ;

: SMT-DOC-SHELL-ROWS ( -- )
   s" ## Build Shell Boundary" SMT-CHECK-DOC-ROW
   s" Shell wrappers may only" SMT-CHECK-DOC-ROW
   s" `HB_TMP`" SMT-CHECK-DOC-ROW
   s" Habu build helpers are responsible" SMT-CHECK-DOC-ROW ;

: SMT-DOC-FOUNDATION-ROWS ( -- )
   SMT-DOC-HANDLE-ROWS
   SMT-DOC-STRING-ROWS
   SMT-DOC-REGEX-ROWS
   SMT-DOC-MAP-ROWS ;

: SMT-DOC-IO-ROWS ( -- )
   SMT-DOC-SOURCE-ROWS
   SMT-DOC-FS-ROWS
   SMT-DOC-PROC-ROWS
   SMT-DOC-ARGV-ROWS ;

: SMT-DOC-HARNESS-ROWS ( -- )
   SMT-DOC-TEST-ROWS
   SMT-DOC-BUILD-ROWS
   SMT-DOC-SHELL-ROWS ;

: SMT-CHECK-DOC-ROWS ( -- )
   SMT-DOC-FOUNDATION-ROWS
   SMT-DOC-IO-ROWS
   SMT-DOC-HARNESS-ROWS ;

: SMT-CHECK-DOCS ( -- )
   s" docs/stdlib.md" SMT-DOC-BUF SMT-DOC-CAP READ-FILE nip SMT-DOC-U !
   SMT-CHECK-DOC-ROWS ;

: SMT-EMPTY? ( ptr u8 n -- bool )
   nip 0= ;

: SMT-NONEMPTY? ( ptr u8 n -- bool )
   nip 0 > ;

: SMT-CHECK-MODULE-NOTE-ROW ( n ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: line module:ptr mu notes:ptr nu target:ptr tu need:ptr needu msg:ptr msgu :}
   module mu target tu LINT-STR= IF
      notes nu need needu LINT-CONTAINS? 0= IF line msg msgu SMT-ROW-FINDING THEN
   THEN ;

: SMT-NOTE-REGEX-ROW ( n ptr u8 n ptr u8 n -- )
   s" regex"
   s" ptr u8 len"
   s" regex module notes must specify ptr u8 len handle representation"
   SMT-CHECK-MODULE-NOTE-ROW ;

: SMT-NOTE-MAP-STORAGE-ROW ( n ptr u8 n ptr u8 n -- )
   s" map"
   s" ptr a n"
   s" map module notes must specify ptr a n storage"
   SMT-CHECK-MODULE-NOTE-ROW ;

: SMT-NOTE-MAP-KEY-ROW ( n ptr u8 n ptr u8 n -- )
   s" map"
   s" ptr u8 n"
   s" map module notes must specify ptr u8 n key representation"
   SMT-CHECK-MODULE-NOTE-ROW ;

: SMT-NOTE-FS-WALK-ROW ( n ptr u8 n ptr u8 n -- )
   s" fs"
   s" WALK-FILES"
   s" fs module notes must mention WALK-FILES contract"
   SMT-CHECK-MODULE-NOTE-ROW ;

: SMT-NOTE-PROC-RUN-ROW ( n ptr u8 n ptr u8 n -- )
   s" process"
   s" RUN-CAPTURE"
   s" process module notes must mention RUN-CAPTURE"
   SMT-CHECK-MODULE-NOTE-ROW ;

: SMT-NOTE-PROC-FD-ROW ( n ptr u8 n ptr u8 n -- )
   s" process"
   s" FD_CLOEXEC"
   s" process module notes must mention FD_CLOEXEC"
   SMT-CHECK-MODULE-NOTE-ROW ;

: SMT-NOTE-TEST-ASSERT-ROW ( n ptr u8 n ptr u8 n -- )
   s" test"
   s" checked assertions"
   s" test module notes must mention checked assertions"
   SMT-CHECK-MODULE-NOTE-ROW ;

: SMT-NOTE-PROP-PRNG-ROW ( n ptr u8 n ptr u8 n -- )
   s" property"
   s" PRNG"
   s" property module notes must mention PRNG"
   SMT-CHECK-MODULE-NOTE-ROW ;

: SMT-NOTE-PROP-EVAL-ROW ( n ptr u8 n ptr u8 n -- )
   s" property"
   s" evaluate boundary"
   s" property module notes must mention evaluate boundary"
   SMT-CHECK-MODULE-NOTE-ROW ;

: SMT-NOTE-BUILD-SHELL-ROW ( n ptr u8 n ptr u8 n -- )
   s" build"
   s" shell boundary"
   s" build module notes must mention shell boundary"
   SMT-CHECK-MODULE-NOTE-ROW ;

: SMT-NOTE-BUILD-HABU-ROW ( n ptr u8 n ptr u8 n -- )
   s" build"
   s" Habu"
   s" build module notes must mention Habu ownership"
   SMT-CHECK-MODULE-NOTE-ROW ;

: SMT-CHECK-MODULE-NOTES-REP ( n ptr u8 n ptr u8 n -- )
   {: line module:ptr mu notes:ptr nu :}
   line module mu notes nu SMT-NOTE-REGEX-ROW
   line module mu notes nu SMT-NOTE-MAP-STORAGE-ROW
   line module mu notes nu SMT-NOTE-MAP-KEY-ROW
   line module mu notes nu SMT-NOTE-FS-WALK-ROW ;

: SMT-CHECK-MODULE-NOTES-PROC ( n ptr u8 n ptr u8 n -- )
   {: line module:ptr mu notes:ptr nu :}
   line module mu notes nu SMT-NOTE-PROC-RUN-ROW
   line module mu notes nu SMT-NOTE-PROC-FD-ROW
   line module mu notes nu SMT-NOTE-TEST-ASSERT-ROW ;

: SMT-CHECK-MODULE-NOTES-HARNESS ( n ptr u8 n ptr u8 n -- )
   {: line module:ptr mu notes:ptr nu :}
   line module mu notes nu SMT-NOTE-PROP-PRNG-ROW
   line module mu notes nu SMT-NOTE-PROP-EVAL-ROW
   line module mu notes nu SMT-NOTE-BUILD-SHELL-ROW
   line module mu notes nu SMT-NOTE-BUILD-HABU-ROW ;

: SMT-CHECK-MODULE-NOTES ( n ptr u8 n ptr u8 n -- )
   {: line module:ptr mu notes:ptr nu :}
   line module mu notes nu SMT-CHECK-MODULE-NOTES-REP
   line module mu notes nu SMT-CHECK-MODULE-NOTES-PROC
   line module mu notes nu SMT-CHECK-MODULE-NOTES-HARNESS ;

: SMT-CHECK-ROW ( ptr u8 n n -- ) {: a:ptr u line :}
   a u SMT-SPLIT-FIELDS
   SMT-FIELD-N @ SMT-FIELDS <> IF line s" expected 11 tab-separated columns" SMT-ROW-FINDING exit THEN
   a 0 SMT-FIELD$ s" 1" LINT-STR= 0= IF line s" schema_version must be 1" SMT-ROW-FINDING THEN
   a 1 SMT-FIELD$ SMT-MODULE-NAME? 0= IF line s" invalid module name " a 1 SMT-FIELD$ SMT-ROW-FINDING$ THEN
   a 2 SMT-FIELD$ SMT-LIB-FILE? 0= IF line s" file must be a stable lib/*.f or src/core/*.f path" SMT-ROW-FINDING THEN
   a 3 SMT-FIELD$ SMT-KIND? 0= IF line s" kind must be module or word" SMT-ROW-FINDING THEN
   a 7 SMT-FIELD$ SMT-NONEMPTY? 0= IF line s" doc is required" SMT-ROW-FINDING THEN
   a 8 SMT-FIELD$ SMT-NONEMPTY? 0= IF line s" owner is required" SMT-ROW-FINDING THEN
   a 10 SMT-FIELD$ SMT-NONEMPTY? 0= IF line s" notes are required" SMT-ROW-FINDING THEN
   a 9 SMT-FIELD$ SMT-STATUS? 0= IF line s" status must be planned, active, or published" SMT-ROW-FINDING THEN
   a 7 SMT-FIELD$ FILE? 0= IF line s" doc path missing: " a 7 SMT-FIELD$ SMT-ROW-FINDING$ THEN
   a 3 SMT-FIELD$ s" module" LINT-STR= IF
      a 4 SMT-FIELD$ SMT-EMPTY? 0= IF line s" module rows must leave word empty" SMT-ROW-FINDING THEN
      a 5 SMT-FIELD$ SMT-EMPTY? 0= IF line s" module rows must leave effect empty" SMT-ROW-FINDING THEN
      a 2 SMT-FIELD$ SMT-ADD-MOD-FILE
      line a 1 SMT-FIELD$ a 10 SMT-FIELD$ SMT-CHECK-MODULE-NOTES
      exit
   THEN
   a 3 SMT-FIELD$ s" word" LINT-STR= IF
      a 4 SMT-FIELD$ SMT-NONEMPTY? 0= IF line s" word rows require word" SMT-ROW-FINDING THEN
      a 5 SMT-FIELD$ SMT-NONEMPTY? 0= IF line s" word rows require effect" SMT-ROW-FINDING THEN
      a 9 SMT-FIELD$ SMT-NOT-PLANNED? 0= IF line s" word rows must be source-backed, not planned" SMT-ROW-FINDING THEN
      a 2 SMT-FIELD$ a 4 SMT-FIELD$ a 5 SMT-FIELD$ SMT-ADD-MAN-WORD
   THEN ;

: SMT-HEADER$ ( -- ptr u8 n )
   s" schema_version	module	file	kind	word	effect	test	doc	owner	status	notes" ;

: SMT-PARSE-MANIFEST ( -- )
   s" lib/std.manifest" SMT-MAN-BUF SMT-MAN-CAP READ-FILE nip SMT-MAN-U !
   SMT-MAN-BUF SMT-MAN-U @ 0 SMT-LINE-END SMT-FIRST !
   SMT-MAN-BUF SMT-FIRST @ SMT-LINE-LEN SMT-HEADER$ LINT-STR= 0= IF s" unexpected manifest header" SMT-FINDING THEN
   SMT-FIRST @ 1+ SMT-I !
   2 SMT-J !
   begin SMT-I @ SMT-MAN-U @ < while
      SMT-MAN-BUF SMT-MAN-U @ SMT-I @ SMT-LINE-END SMT-END !
      SMT-MAN-BUF SMT-I @ + SMT-END @ SMT-I @ - SMT-LINE-LEN SMT-J @ SMT-CHECK-ROW
      SMT-END @ 1+ SMT-I !
      SMT-J @ 1+ SMT-J !
   repeat
   SMT-MOD-N @ 0= IF s" expected at least one module row" SMT-FINDING THEN ;

: SMT-LIB-SOURCE? ( ptr u8 n -- bool )
   2dup s" .f" LINT-ENDS-WITH? 0= IF 2drop 0 0= 0= exit THEN
   s" -test.f" LINT-ENDS-WITH? 0= ;

\ Coverage tracks stdlib modules: flat lib/<module>.f plus the TEST framework
\ subpackage. Curated core rows and research sub-libraries such as lib/ptx/
\ are manifest-checked when listed, but not discovered by this stdlib walk.
: SMT-FLAT-LIB-FILE? ( ptr u8 n -- bool )
   SMT-SLASH LINT-COUNT-CHAR 1 = ;

: SMT-TEST-LIB-FILE? ( ptr u8 n -- bool )
   s" lib/test/" LINT-STARTS-WITH? ;

: SMT-STDLIB-COVERAGE-FILE? ( ptr u8 n -- bool )
   2dup SMT-FLAT-LIB-FILE? IF 2drop 0 0= exit THEN
   SMT-TEST-LIB-FILE? ;

: SMT-COLLECT-LIB-FILE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u SMT-LIB-SOURCE? a u SMT-LIB-FILE? and a u SMT-STDLIB-COVERAGE-FILE? and IF
      a u SMT-ADD-LIB-FILE
   THEN ;

: SMT-CHECK-LIB-COVERAGE ( -- )
   s" lib" [: SMT-COLLECT-LIB-FILE ;] WALK-FILES
   0 begin dup SMT-LIB-N @ < while
      dup SMT-LIB-F$ 2dup SMT-FIND-MOD-FILE 0 < IF
         s" missing module row for " SMT-ERR
         SMT-ERR
         SMT-NL
         SMT-BAD+
      ELSE
         2drop
      THEN
      1+
   repeat drop ;

: SMT-ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: SMT-ARGV-LOAD-PUBLIC ( -- )
   PROC-ARGV-RESET
   s" --load" SMT-ARG+
   s" lib/errors.f" SMT-ARG+
   s" lib/string.f" SMT-ARG+
   s" lib/memory.f" SMT-ARG+
   s" lib/vector.f" SMT-ARG+
   s" tools/lint/text.f" SMT-ARG+
   s" tools/lint/intern.f" SMT-ARG+
   s" tools/lint/token.f" SMT-ARG+
   s" tools/lint/lib.f" SMT-ARG+
   s" tools/public-signatures-core.f" SMT-ARG+
   s" tools/public-signatures.f" SMT-ARG+
   s" --" SMT-ARG+
   0 begin dup SMT-MOD-N @ < while
      dup SMT-MOD-F$ 2dup FILE? IF SMT-ARG+ ELSE 2drop THEN
      1+
   repeat drop ;

: SMT-RUN-PUBLIC ( -- )
   SMT-ARGV-LOAD-PUBLIC
   s" bin/hb" >LEN SMT-PUB-BUF SMT-PUB-CAP >LEN SMT-ERR-BUF SMT-ERR-CAP >LEN SMT-TIMEOUT-MS >MS RUN-ARGV-CAPTURE
   {: outu erru rc :}
   outu LEN>N SMT-PUB-U !
   rc RC>N 0 <> IF s" public-signatures run failed" SMT-FINDING THEN
   erru LEN>N 0 <> IF s" public-signatures wrote stderr" SMT-FINDING THEN ;

: SMT-FIND-FROM ( ptr u8 n ptr u8 n n -- option<n> )
   {: a:ptr u needle:ptr nu start :}
   start 0 < IF OPTION:NONE exit THEN
   u start < IF OPTION:NONE exit THEN
   a start + u start - needle nu LINT-FIND-SUB MATCH option
     none OF OPTION:NONE ENDOF
     some OF start + OPTION:SOME ENDOF
   ;MATCH ;

: SMT-JSON-SLICE ( ptr u8 n n -- ptr u8 n n bool ) {: a:ptr u:n start:n :}   \ body located: slice to the closing quote
   a SMT-JSON-BODY @ + u SMT-JSON-BODY @ - SMT-DQ LINT-INDEX-OF MATCH option
     none OF a 0 start 0 0= 0= ENDOF
     some OF SMT-JSON-END !
        a SMT-JSON-BODY @ + SMT-JSON-END @
        SMT-JSON-BODY @ SMT-JSON-END @ + 1+
        0 0= ENDOF
   ;MATCH ;

: SMT-CAPTURE-AFTER ( ptr u8 n ptr u8 n n -- ptr u8 n n bool )
   {: a:ptr u key:ptr ku start :}
   a u key ku start SMT-FIND-FROM MATCH option
     none OF a 0 start 0 0= 0= ENDOF
     some OF ku + SMT-JSON-BODY !
        a u start SMT-JSON-SLICE ENDOF
   ;MATCH ;

create SMT-JSON-WORD-KEY SMT-DQ c, 119 c, 111 c, 114 c, 100 c, SMT-DQ c, SMT-COLON c, SMT-DQ c,
create SMT-JSON-FILE-KEY SMT-DQ c, 102 c, 105 c, 108 c, 101 c, SMT-DQ c, SMT-COLON c, SMT-DQ c,
create SMT-JSON-SIG-KEY SMT-DQ c, 115 c, 105 c, 103 c, 110 c, 97 c, 116 c, 117 c, 114 c, 101 c, SMT-DQ c, SMT-COLON c, SMT-DQ c,

: SMT-JSON-WORD-KEY$ ( -- ptr u8 n ) SMT-JSON-WORD-KEY 8 ;
: SMT-JSON-FILE-KEY$ ( -- ptr u8 n ) SMT-JSON-FILE-KEY 8 ;
: SMT-JSON-SIG-KEY$ ( -- ptr u8 n ) SMT-JSON-SIG-KEY 13 ;

: SMT-CAPTURE! ( ptr u8 n n -- ) {: a:ptr u next :}
   a SMT-JSON-A !
   u SMT-JSON-U !
   next SMT-JSON-NEXT ! ;

: SMT-CAPTURE-DROP ( ptr u8 n n -- )
   drop 2drop ;

: SMT-CAPTURE-KEY! ( ptr u8 n n -- bool ) {: key:ptr ku start :}
   SMT-PUB-BUF SMT-PUB-U @ key ku start SMT-CAPTURE-AFTER IF
      SMT-CAPTURE!
      0 0=
   ELSE
      SMT-CAPTURE-DROP
      0 0= 0=
   THEN ;

: SMT-CAPTURE-WORD! ( n -- bool )
   SMT-JSON-WORD-KEY$ rot SMT-CAPTURE-KEY! ;

: SMT-CAPTURE-FILE! ( -- bool )
   SMT-JSON-FILE-KEY$ SMT-JSON-NEXT @ SMT-CAPTURE-KEY! ;

: SMT-CAPTURE-SIG! ( -- bool )
   SMT-JSON-SIG-KEY$ SMT-JSON-NEXT @ SMT-CAPTURE-KEY! ;

: SMT-PARSE-PUBLIC-ONE ( n -- n bool ) {: start :}
   start SMT-CAPTURE-WORD! 0= IF start 0 0= 0= exit THEN
   SMT-PUB-N @ SMT-WORD-MAX >= IF s" too many public word rows" SMT-FINDING SMT-DIE THEN
   SMT-JSON-A @ SMT-JSON-U @ SMT-STORE$ SMT-PW-L SMT-PUB-N @ SMT-A! SMT-PW-O SMT-PUB-N @ SMT-A!
   SMT-CAPTURE-FILE! 0= IF start 0 0= 0= exit THEN
   SMT-JSON-A @ SMT-JSON-U @ SMT-STORE$ SMT-PF-L SMT-PUB-N @ SMT-A! SMT-PF-O SMT-PUB-N @ SMT-A!
   SMT-CAPTURE-SIG! 0= IF start 0 0= 0= exit THEN
   SMT-JSON-A @ SMT-JSON-U @ SMT-STORE$ SMT-PE-L SMT-PUB-N @ SMT-A! SMT-PE-O SMT-PUB-N @ SMT-A!
   SMT-PUB-N @ 1+ SMT-PUB-N !
   SMT-JSON-NEXT @ 0 0= ;

: SMT-PARSE-PUBLIC ( -- )
   0 begin SMT-PARSE-PUBLIC-ONE while repeat drop ;

: SMT-PUB-IN-MAN? ( n -- bool ) {: k :}
   k SMT-PUB-F$ k SMT-PUB-W$ k SMT-PUB-E$ SMT-FIND-MAN-WORD 0 >= ;

: SMT-MAN-IN-PUB? ( n -- bool ) {: k :}
   k SMT-MAN-F$ k SMT-MAN-W$ k SMT-MAN-E$ SMT-FIND-PUB-WORD 0 >= ;

: SMT-PRINT-PUB-ROW ( n -- ) {: k :}
   k SMT-PUB-F$ SMT-ERR
   s"  " SMT-ERR
   k SMT-PUB-W$ SMT-ERR
   s"  " SMT-ERR
   k SMT-PUB-E$ SMT-ERR ;

: SMT-PRINT-MAN-ROW ( n -- ) {: k :}
   k SMT-MAN-F$ SMT-ERR
   s"  " SMT-ERR
   k SMT-MAN-W$ SMT-ERR
   s"  " SMT-ERR
   k SMT-MAN-E$ SMT-ERR ;

: SMT-CHECK-PUBLIC-IN-MANIFEST ( -- )
   0 begin dup SMT-PUB-N @ < while
      dup SMT-PUB-IN-MAN? 0= IF
         s" public word missing from manifest: " SMT-ERR
         dup SMT-PRINT-PUB-ROW
         SMT-NL
         SMT-BAD+
      THEN
      1+
   repeat drop ;

: SMT-CHECK-MANIFEST-IN-PUBLIC ( -- )
   0 begin dup SMT-MAN-N @ < while
      dup SMT-MAN-IN-PUB? 0= IF
         s" manifest word missing from public signatures: " SMT-ERR
         dup SMT-PRINT-MAN-ROW
         SMT-NL
         SMT-BAD+
      THEN
      1+
   repeat drop ;

: SMT-CHECK-DRIFT ( -- )
   SMT-RUN-PUBLIC
   SMT-PARSE-PUBLIC
   SMT-CHECK-PUBLIC-IN-MANIFEST
   SMT-CHECK-MANIFEST-IN-PUBLIC ;

: SMT-RESET ( -- )
   0 SMT-BAD !
   0 SMT-STR-U !
   0 SMT-MAN-N !
   0 SMT-PUB-N !
   0 SMT-MOD-N !
   0 SMT-LIB-N ! ;

: SMT-MAIN ( -- )
   SMT-RESET
   s" lib/std.manifest" FILE? 0= IF s" missing lib/std.manifest" SMT-FINDING THEN
   s" docs/stdlib.md" FILE? 0= IF s" missing docs/stdlib.md" SMT-FINDING THEN
   s" bin/hb" FILE? 0= IF s" missing executable bin/hb" SMT-FINDING THEN
   SMT-CHECK-DOCS
   SMT-PARSE-MANIFEST
   SMT-CHECK-LIB-COVERAGE
   SMT-CHECK-DRIFT
   SMT-DIE
   s" stdlib-manifest-test: ok" SMT-OUT
   SMT-LF SMT-C!
   SMT-ONE 1 SMT-OUT ;

SMT-MAIN
