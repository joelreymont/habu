\ checked-boundary-lint-core.f - forbid broad unchecked definitions.
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/fs.f, and
\ tools/lint/json-writer.f.

32 constant UB-NUM-CAP

10 constant UB-LF
13 constant UB-CR
32 constant UB-SP
34 constant UB-DQ
40 constant UB-LPAREN
41 constant UB-RPAREN
9 constant UB-TAB
58 constant UB-COLON-C
92 constant UB-BSLASH

create UB-NUM UB-NUM-CAP allot
create UB-LF-BUF 1 allot

variable UB-FILE-A
variable UB-FILE-U
variable UB-SRC-A
variable UB-SRC-U
variable UB-SRC-CAP
variable UB-I
variable UB-LINE
variable UB-COL
variable UB-TOK-A
variable UB-TOK-U
variable UB-TOK-BYTE
variable UB-TOK-LINE
variable UB-TOK-COL
variable UB-PREV-A
variable UB-PREV-U
variable UB-BAD
variable UB-CHECK-OFF
variable UB-NUM-I
variable UB-TRUSTED
variable UB-CARRY-OFF
variable UB-JSON
variable UB-STRICT-BOUNDARY
variable UB-OUT-FD

: UB-FILE-A-FIELD ( -- ptr ptr u8 )
   UB-FILE-A 0 ptr-field ;

: UB-SRC-A-FIELD ( -- ptr ptr u8 )
   UB-SRC-A 0 ptr-field ;

: UB-TOK-A-FIELD ( -- ptr ptr u8 )
   UB-TOK-A 0 ptr-field ;

: UB-PREV-A-FIELD ( -- ptr ptr u8 )
   UB-PREV-A 0 ptr-field ;

: UB-FILE-A@ ( -- ptr u8 )
   UB-FILE-A-FIELD @ ;

: UB-SRC-A@ ( -- ptr u8 )
   UB-SRC-A-FIELD @ ;

: UB-TOK-A@ ( -- ptr u8 )
   UB-TOK-A-FIELD @ ;

: UB-PREV-A@ ( -- ptr u8 )
   UB-PREV-A-FIELD @ ;

: UB-FILE-A! ( ptr u8 -- )
   UB-FILE-A-FIELD ! ;

: UB-SRC-A! ( ptr u8 -- )
   UB-SRC-A-FIELD ! ;

: UB-TOK-A! ( ptr u8 -- )
   UB-TOK-A-FIELD ! ;

: UB-PREV-A! ( ptr u8 -- )
   UB-PREV-A-FIELD ! ;

: UB-TRUE ( -- bool )
   0 0= ;

: UB-FALSE ( -- bool )
   UB-TRUE 0= ;

: UB-NOT ( bool -- bool )
   IF UB-FALSE ELSE UB-TRUE THEN ;

: UB-JSON! ( bool -- )
   UB-JSON ! ;

: UB-STRICT-BOUNDARY! ( bool -- )
   UB-STRICT-BOUNDARY ! ;

: UB-OUT-FD! ( fd -- )
   UB-OUT-FD ! ;

: UB-CHECK-OFF! ( bool -- )
   dup UB-CHECK-OFF !
   UB-CARRY-OFF ! ;

: UB-WRITE ( n ptr u8 n -- ) {: fd a:ptr u :}
   u 0= IF exit THEN
   fd a u write u <> IF s" checked-boundary-lint: write failed" 74 die THEN ;

: UB-OUT ( ptr u8 n -- )
   UB-OUT-FD @ -rot UB-WRITE ;

: UB-C ( n -- )
   UB-LF-BUF c!
   UB-LF-BUF 1 UB-OUT ;

: UB-NL ( -- )
   UB-LF UB-C ;

: UB-U$ ( n -- ptr u8 n )
   {: u:n :}
   UB-NUM-CAP UB-NUM-I !
   u 0= if
      UB-NUM-I @ 1- UB-NUM-I !
      48 UB-NUM UB-NUM-I @ + c!
      UB-NUM UB-NUM-I @ + 1
      exit
   then
   u begin dup 0 > while
      dup 10 mod 48 +
      UB-NUM-I @ 1- UB-NUM-I !
      UB-NUM UB-NUM-I @ + c!
      10 /
   repeat drop
   UB-NUM UB-NUM-I @ + UB-NUM-CAP UB-NUM-I @ - ;

: UB-END? ( -- bool )
   UB-I @ UB-SRC-U @ >= ;

: UB-C@ ( -- n )
   UB-SRC-A@ UB-I @ + c@ ;

: UB-ADV ( -- n )
   UB-C@
   UB-I @ 1+ UB-I !
   dup UB-LF = if
      UB-LINE @ 1+ UB-LINE !
      1 UB-COL !
   else
      UB-COL @ 1+ UB-COL !
   then ;

: UB-WS? ( n -- bool )
   dup UB-SP = over UB-LF = or over UB-TAB = or swap UB-CR = or ;

: UB-TOK-MORE? ( -- bool )
   UB-END? if UB-FALSE exit then
   UB-C@ UB-WS? UB-NOT ;

: UB-SKIP-LINE-COMMENT ( -- )
   begin UB-END? UB-NOT while
      UB-C@ UB-LF = if exit then
      UB-ADV drop
   repeat ;

: UB-SKIP-PAREN-COMMENT ( -- )
   begin UB-END? UB-NOT while
      UB-ADV UB-RPAREN = if exit then
   repeat ;

: UB-SKIP-STRING ( -- )
   begin UB-END? UB-NOT while
      UB-ADV UB-DQ = if exit then
   repeat ;

: UB-STRING-OPENER? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   u 2 <> if UB-FALSE exit then
   a 1+ c@ UB-DQ <> if UB-FALSE exit then
   a c@ ASCII-LOWER dup 115 = over 46 = or swap 99 = or ;

: UB-SKIP-IGNORED ( -- )
   begin UB-END? UB-NOT while
      UB-C@ dup UB-WS? if drop UB-ADV drop
      else dup UB-BSLASH = if drop UB-SKIP-LINE-COMMENT
      else dup UB-LPAREN = if drop UB-ADV drop UB-SKIP-PAREN-COMMENT
      else drop exit then then then
   repeat ;

: UB-TOK$ ( -- ptr u8 n )
   UB-TOK-A@ UB-TOK-U @ ;

: UB-PREV$ ( -- ptr u8 n )
   UB-PREV-A@ UB-PREV-U @ ;

: UB-TOK= ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   UB-TOK$ a u STR= ;

: UB-PREV= ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   UB-PREV$ a u STR= ;

: UB-TOK=CI ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   UB-TOK$ a u STR=CI ;

: UB-PREV=CI ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   UB-PREV$ a u STR=CI ;

: UB-SAVE-PREV ( -- )
   UB-TOK-A@ UB-PREV-A!
   UB-TOK-U @ UB-PREV-U ! ;

: UB-NEXT-TOK ( -- bool )
   UB-SKIP-IGNORED
   UB-END? if UB-FALSE exit then
   UB-SRC-A@ UB-I @ + UB-TOK-A!
   UB-I @ UB-TOK-BYTE !
   UB-LINE @ UB-TOK-LINE !
   UB-COL @ UB-TOK-COL !
   0 UB-TOK-U !
   begin UB-TOK-MORE? while
      UB-ADV drop
      UB-TOK-U @ 1+ UB-TOK-U !
   repeat
   UB-TOK$ UB-STRING-OPENER? if UB-SKIP-STRING then
   UB-TRUE ;

: UB-SET-CHECK-OFF? ( -- bool )
   s" set-check" UB-TOK=CI s" 0" UB-PREV=CI and ;

: UB-SET-CHECK-ON? ( -- bool )
   s" set-check" UB-TOK=CI s" 0" UB-PREV=CI UB-NOT and ;

: UB-CHECKER-MUTATION? ( -- bool )
   s" set-check" UB-TOK=CI ;

: UB-COLON? ( -- bool )
   s" :" UB-TOK= ;

: UB-TRUSTED? ( -- bool )
   s" TRUSTED:" UB-TOK=CI ;

: UB-SEMI? ( -- bool )
   s" ;" UB-TOK= ;

: UB-HOOK-NAME? ( ptr u8 n -- bool )
   s" CHECK-HOOK" ENDS-WITH? ;

: UB-JSON-BASE ( ptr u8 n -- )
   {: code:ptr codeu:n :}
   LJW-RESET
   LJW-OBJECT-START
   s" schema_version" LJW-KEY 1 LJW-U LJW-COMMA
   s" code" LJW-KEY code codeu LJW-STRING LJW-COMMA
   s" repair_class" LJW-KEY s" trusted_boundary_required" LJW-STRING LJW-COMMA
   s" verdict" LJW-KEY s" rejected" LJW-STRING LJW-COMMA ;

: UB-JSON-ORIGIN ( -- )
   s" token" LJW-KEY UB-TOK$ LJW-STRING LJW-COMMA
   s" token_index" LJW-KEY 0 LJW-U LJW-COMMA
   s" file" LJW-KEY UB-FILE-A@ UB-FILE-U @ LJW-STRING LJW-COMMA
   s" line" LJW-KEY UB-TOK-LINE @ LJW-U LJW-COMMA
   s" column" LJW-KEY UB-TOK-COL @ LJW-U LJW-COMMA
   s" byte_start" LJW-KEY UB-TOK-BYTE @ LJW-U LJW-COMMA
   s" byte_end" LJW-KEY UB-TOK-BYTE @ UB-TOK-U @ + LJW-U LJW-COMMA ;

: UB-JSON-FINISH ( ptr u8 n ptr u8 n -- )
   {: expected:ptr expectedu:n actual:ptr actualu:n :}
   s" expected" LJW-KEY expected expectedu LJW-STRING LJW-COMMA
   s" actual" LJW-KEY actual actualu LJW-STRING LJW-COMMA
   s" suggestion" LJW-KEY
   s" Keep generated/user code checked; move checker mutations behind an audited boundary." LJW-STRING
   LJW-OBJECT-END
   LJW$ UB-OUT UB-NL ;

: UB-JSON-DEFINITION ( ptr u8 n -- )
   {: name:ptr nu:n :}
   s" E-UNCHECKED-DEFINITION" UB-JSON-BASE
   s" word" LJW-KEY name nu LJW-STRING LJW-COMMA
   UB-JSON-ORIGIN
   s" checked definition" s" checker disabled" UB-JSON-FINISH ;

: UB-JSON-MUTATION ( -- )
   s" E-CHECKER-MUTATION" UB-JSON-BASE
   s" word" LJW-KEY s" " LJW-STRING LJW-COMMA
   UB-JSON-ORIGIN
   s" checker hook remains installed" s" set-check" UB-JSON-FINISH ;

: UB-REPORT-DEFINITION ( ptr u8 n -- )
   {: name:ptr nu:n :}
   UB-BAD @ 1+ UB-BAD !
   UB-JSON @ if name nu UB-JSON-DEFINITION exit then
   s" UNCHECKED-DEFINITION " UB-OUT
   UB-FILE-A@ UB-FILE-U @ UB-OUT
   UB-COLON-C UB-C UB-TOK-LINE @ UB-U$ UB-OUT
   UB-COLON-C UB-C UB-TOK-COL @ UB-U$ UB-OUT
   s" : `" UB-OUT name nu UB-OUT
   s" ` defined while checker disabled" UB-OUT UB-NL ;

: UB-REPORT-MUTATION ( -- )
   UB-BAD @ 1+ UB-BAD !
   UB-JSON @ if UB-JSON-MUTATION exit then
   s" CHECKER-MUTATION " UB-OUT
   UB-FILE-A@ UB-FILE-U @ UB-OUT
   UB-COLON-C UB-C UB-TOK-LINE @ UB-U$ UB-OUT
   UB-COLON-C UB-C UB-TOK-COL @ UB-U$ UB-OUT
   s" : `" UB-OUT UB-TOK$ UB-OUT
   s" ` mutates checker state in strict boundary mode" UB-OUT UB-NL ;

: UB-HANDLE-COLON ( -- )
   UB-CHECK-OFF @ UB-NOT if exit then
   UB-NEXT-TOK UB-NOT if exit then
   UB-TOK$ 2dup UB-HOOK-NAME? if 2drop exit then
   UB-REPORT-DEFINITION ;

: UB-RESET-FILE-SCAN ( -- )
   UB-NUM UB-PREV-A! 0 UB-PREV-U !
   0 UB-I ! 1 UB-LINE ! 1 UB-COL !
   UB-CARRY-OFF @ UB-CHECK-OFF !
   UB-FALSE UB-TRUSTED ! ;

: UB-SCAN ( -- )
   UB-RESET-FILE-SCAN
   begin UB-NEXT-TOK while
      UB-TRUSTED? if UB-TRUE UB-TRUSTED ! then
      UB-STRICT-BOUNDARY @ if
         UB-CHECKER-MUTATION? UB-TRUSTED @ UB-NOT and if UB-REPORT-MUTATION then
      then
      UB-SET-CHECK-OFF? if UB-TRUE UB-CHECK-OFF! then
      UB-COLON? if UB-HANDLE-COLON then
      UB-SET-CHECK-ON? if UB-FALSE UB-CHECK-OFF! then
      UB-SEMI? if UB-FALSE UB-TRUSTED ! then
      UB-SAVE-PREV
   repeat ;

: CHECKED-BOUNDARY-LINT-RESET ( -- )
   0 UB-BAD !
   UB-FALSE UB-CHECK-OFF!
   UB-FALSE UB-JSON!
   UB-FALSE UB-STRICT-BOUNDARY!
   1 >FD UB-OUT-FD! ;

: CHECKED-BOUNDARY-LINT-FILE ( ptr u8 n -- )
   {: path:ptr pu:n :}
   path UB-FILE-A! pu UB-FILE-U !
   path pu FILE-SIZE MEM-ALLOC-64K-SPAN
   UB-SRC-CAP ! UB-SRC-A!
   path pu UB-SRC-A@ UB-SRC-CAP @ READ-ALL UB-SRC-U !
   UB-SCAN ;

: CHECKED-BOUNDARY-LINT-FINISH ( -- )
   UB-BAD @ 0 > if 1 throw then ;
