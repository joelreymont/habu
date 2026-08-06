\ checked-boundary-lint-core.f - forbid broad unchecked definitions.
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/fs.f, and
\ tools/lint/json-writer.f.

require tools/hook-sites.f                     \ single registry of audited hook identities

package CHECKED-BOUNDARY-LINT

private

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
variable UB-PREV2-A
variable UB-PREV2-U
variable UB-BAD
variable UB-CHECK-OFF
variable UB-NUM-I
variable UB-TRUSTED
variable UB-IN-DEF
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

: UB-PREV2-A-FIELD ( -- ptr ptr u8 )
   UB-PREV2-A 0 ptr-field ;

: UB-FILE-A@ ( -- ptr u8 )
   UB-FILE-A-FIELD @ ;

: UB-FILE$ ( -- ptr u8 n )
   UB-FILE-A@ UB-FILE-U @ ;

: UB-SRC-A@ ( -- ptr u8 )
   UB-SRC-A-FIELD @ ;

: UB-TOK-A@ ( -- ptr u8 )
   UB-TOK-A-FIELD @ ;

: UB-PREV-A@ ( -- ptr u8 )
   UB-PREV-A-FIELD @ ;

: UB-PREV2-A@ ( -- ptr u8 )
   UB-PREV2-A-FIELD @ ;

: UB-FILE-A! ( ptr u8 -- )
   UB-FILE-A-FIELD ! ;

: UB-SRC-A! ( ptr u8 -- )
   UB-SRC-A-FIELD ! ;

: UB-TOK-A! ( ptr u8 -- )
   UB-TOK-A-FIELD ! ;

: UB-PREV-A! ( ptr u8 -- )
   UB-PREV-A-FIELD ! ;

: UB-PREV2-A! ( ptr u8 -- )
   UB-PREV2-A-FIELD ! ;

: UB-TRUE ( -- bool )
   0 0= ;

: UB-FALSE ( -- bool )
   UB-TRUE 0= ;

: UB-NOT ( bool -- bool )
   IF UB-FALSE ELSE UB-TRUE THEN ;

public

: JSON! ( bool -- )
   UB-JSON ! ;

: STRICT! ( bool -- )
   UB-STRICT-BOUNDARY ! ;

: OUT-FD! ( fd -- )
   UB-OUT-FD ! ;

private

: UB-CHECK-OFF! ( bool -- )
   dup UB-CHECK-OFF !
   UB-CARRY-OFF ! ;

: UB-WRITE ( n ptr u8 n -- ) {: fd a:ptr u :}
   fd a u LINT-OUT-WRITE ;

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

: UB-SKIP-ESC-STRING ( -- )
   begin UB-END? UB-NOT while
      UB-ADV dup UB-BSLASH = if
         drop UB-END? UB-NOT if UB-ADV drop then
      else
         UB-DQ = if exit then
      then
   repeat ;

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

: UB-PREV2$ ( -- ptr u8 n )
   UB-PREV2-A@ UB-PREV2-U @ ;

: UB-TOK= ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   UB-TOK$ a u LINT-STR= ;

: UB-PREV= ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   UB-PREV$ a u LINT-STR= ;

: UB-TOK=CI ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   UB-TOK$ a u LINT-STR=CI ;

: UB-PREV=CI ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   UB-PREV$ a u LINT-STR=CI ;

: UB-PREV2= ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   UB-PREV2$ a u LINT-STR= ;

: UB-SAVE-PREV ( -- )
   UB-PREV-A@ UB-PREV2-A!
   UB-PREV-U @ UB-PREV2-U !
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
   UB-TOK$ LINT-ESC-STRING-OPENER? if UB-SKIP-ESC-STRING else
   UB-TOK$ LINT-NORMAL-STRING-OPENER? if UB-SKIP-STRING then then
   UB-TRUE ;

: UB-SET-CHECK-OFF? ( -- bool )
   s" set-check" UB-TOK=CI s" 0" UB-PREV=CI and ;

: UB-SET-CHECK-ON? ( -- bool )
   s" set-check" UB-TOK=CI s" 0" UB-PREV=CI UB-NOT and ;

: UB-PREFLIGHT-INSTALL? ( -- bool )
   s" LOWER-CERT-HOOK:INSTALL" UB-TOK=CI ;

\ A hook install is `' NAME set-check` / `['] NAME set-check`: current token
\ `set-check`, two-back token the tick, one-back token the installed hook name.
\ Requiring the tick two-back also excludes the `' set-check` name reference
\ (there the tick is one-back and the name slot is `set-check` itself).
: UB-TICK-PREV2? ( -- bool )
   s" '" UB-PREV2= s" [']" UB-PREV2= or ;

: UB-SET-CHECK-INSTALL? ( -- bool )
   s" set-check" UB-TOK=CI UB-TICK-PREV2? and ;

\ The tier-2 top-level reject enforcer is installed through `set-top-check`
\ (src/core/top-row.f TR-INSTALL, dot habu-typed-top-tier-589c550f). It is the
\ audited escape window for the whole top-row tracker: any name installed there
\ diverts the pre-execution reject enforcement unless the registry authorizes it
\ for this exact file. Same `' NAME set-top-check` / `['] NAME set-top-check`
\ shape as the set-check installs above.
: UB-SET-TOP-CHECK-INSTALL? ( -- bool )
   s" set-top-check" UB-TOK=CI UB-TICK-PREV2? and ;

\ Audited checker hooks. tools/hook-sites.f is the single registry of legitimate
\ hook identities, and a row names a file as well as a word. The installed name
\ alone proves nothing - any file may define a word called CHECK-HOOK - so an
\ install is audited only when the file being scanned and the installed name
\ together match a registry row. Registry paths are the committed repo-relative
\ spellings and are compared exactly, so `./path`, an absolute path, or a
\ workspace-prefixed path fails closed.
: UB-HOOK-ALLOWED? ( -- bool )
   UB-FILE$ UB-PREV$ HOOK-SITES:CHECK-MATCH? ;

\ Same registry, top-row rows: `set-top-check` installs are authorized by the
\ (file, installed name) pair the registry records for that enforcer.
: UB-TOP-HOOK-ALLOWED? ( -- bool )
   UB-FILE$ UB-PREV$ HOOK-SITES:TOP-MATCH? ;

: UB-CHECKER-MUTATION? ( -- bool )
   s" set-check" UB-TOK=CI ;

: UB-COLON? ( -- bool )
   s" :" UB-TOK= ;

: UB-TRUSTED? ( -- bool )
   s" TRUSTED:" UB-TOK=CI ;

: UB-DEF-OPEN? ( -- bool )
   UB-COLON? if UB-TRUE exit then
   s" +:" UB-TOK=CI if UB-TRUE exit then
   s" CHECKED:" UB-TOK=CI if UB-TRUE exit then
   UB-TRUSTED? if UB-TRUE exit then
   s" KERNEL:" UB-TOK=CI if UB-TRUE exit then
   s" :noname" UB-TOK=CI ;

: UB-TOP? ( -- bool )
   UB-IN-DEF @ UB-NOT ;

: UB-SEMI? ( -- bool )
   s" ;" UB-TOK= ;

: UB-HOOK-NAME? ( ptr u8 n -- bool )
   s" CHECK-HOOK" LINT-ENDS-WITH? ;

: UB-JSON-BASE ( ptr u8 n -- )
   {: code:ptr codeu:n :}
   JSON-WRITE:RESET
   JSON-WRITE:OBJECT-START
   s" schema_version" JSON-WRITE:KEY 1 JSON-WRITE:U JSON-WRITE:COMMA
   s" code" JSON-WRITE:KEY code codeu JSON-WRITE:STRING JSON-WRITE:COMMA
   s" repair_class" JSON-WRITE:KEY s" trusted_boundary_required" JSON-WRITE:STRING JSON-WRITE:COMMA
   s" verdict" JSON-WRITE:KEY s" rejected" JSON-WRITE:STRING JSON-WRITE:COMMA ;

: UB-JSON-ORIGIN ( -- )
   s" token" JSON-WRITE:KEY UB-TOK$ JSON-WRITE:STRING JSON-WRITE:COMMA
   s" token_index" JSON-WRITE:KEY 0 JSON-WRITE:U JSON-WRITE:COMMA
   s" file" JSON-WRITE:KEY UB-FILE$ JSON-WRITE:STRING JSON-WRITE:COMMA
   s" line" JSON-WRITE:KEY UB-TOK-LINE @ JSON-WRITE:U JSON-WRITE:COMMA
   s" column" JSON-WRITE:KEY UB-TOK-COL @ JSON-WRITE:U JSON-WRITE:COMMA
   s" byte_start" JSON-WRITE:KEY UB-TOK-BYTE @ JSON-WRITE:U JSON-WRITE:COMMA
   s" byte_end" JSON-WRITE:KEY UB-TOK-BYTE @ UB-TOK-U @ + JSON-WRITE:U JSON-WRITE:COMMA ;

: UB-JSON-FINISH ( ptr u8 n ptr u8 n -- )
   {: expected:ptr expectedu:n actual:ptr actualu:n :}
   s" expected" JSON-WRITE:KEY expected expectedu JSON-WRITE:STRING JSON-WRITE:COMMA
   s" actual" JSON-WRITE:KEY actual actualu JSON-WRITE:STRING JSON-WRITE:COMMA
   s" suggestion" JSON-WRITE:KEY
   s" Keep generated/user code checked; move checker mutations behind an audited boundary." JSON-WRITE:STRING
   JSON-WRITE:OBJECT-END
   JSON-WRITE:$ UB-OUT UB-NL ;

: UB-JSON-DEFINITION ( ptr u8 n -- )
   {: name:ptr nu:n :}
   s" E-UNCHECKED-DEFINITION" UB-JSON-BASE
   s" word" JSON-WRITE:KEY name nu JSON-WRITE:STRING JSON-WRITE:COMMA
   UB-JSON-ORIGIN
   s" checked definition" s" checker disabled" UB-JSON-FINISH ;

: UB-JSON-MUTATION ( -- )
   s" E-CHECKER-MUTATION" UB-JSON-BASE
   s" word" JSON-WRITE:KEY s" " JSON-WRITE:STRING JSON-WRITE:COMMA
   UB-JSON-ORIGIN
   s" checker hook remains installed" s" set-check" UB-JSON-FINISH ;

: UB-JSON-PREFLIGHT ( -- )
   s" E-CHECKER-MUTATION" UB-JSON-BASE
   s" word" JSON-WRITE:KEY s" set-check" JSON-WRITE:STRING JSON-WRITE:COMMA
   UB-JSON-ORIGIN
   s" LOWER-CERT-HOOK:INSTALL" s" set-check" UB-JSON-FINISH ;

: UB-JSON-ROGUE-HOOK ( -- )
   s" E-UNAUDITED-HOOK" UB-JSON-BASE
   s" word" JSON-WRITE:KEY UB-PREV$ JSON-WRITE:STRING JSON-WRITE:COMMA
   UB-JSON-ORIGIN
   s" audited checker hook" UB-PREV$ UB-JSON-FINISH ;

: UB-JSON-ROGUE-TOP-HOOK ( -- )
   s" E-UNAUDITED-TOP-HOOK" UB-JSON-BASE
   s" word" JSON-WRITE:KEY UB-PREV$ JSON-WRITE:STRING JSON-WRITE:COMMA
   UB-JSON-ORIGIN
   s" audited top-check hook" UB-PREV$ UB-JSON-FINISH ;

: UB-REPORT-DEFINITION ( ptr u8 n -- )
   {: name:ptr nu:n :}
   UB-BAD @ 1+ UB-BAD !
   UB-JSON @ if name nu UB-JSON-DEFINITION exit then
   s" UNCHECKED-DEFINITION " UB-OUT
   UB-FILE$ UB-OUT
   UB-COLON-C UB-C UB-TOK-LINE @ UB-U$ UB-OUT
   UB-COLON-C UB-C UB-TOK-COL @ UB-U$ UB-OUT
   s" : `" UB-OUT name nu UB-OUT
   s" ` defined while checker disabled" UB-OUT UB-NL ;

: UB-REPORT-MUTATION ( -- )
   UB-BAD @ 1+ UB-BAD !
   UB-JSON @ if UB-JSON-MUTATION exit then
   s" CHECKER-MUTATION " UB-OUT
   UB-FILE$ UB-OUT
   UB-COLON-C UB-C UB-TOK-LINE @ UB-U$ UB-OUT
   UB-COLON-C UB-C UB-TOK-COL @ UB-U$ UB-OUT
   s" : `" UB-OUT UB-TOK$ UB-OUT
   s" ` mutates checker state in strict boundary mode" UB-OUT UB-NL ;

: UB-REPORT-PREFLIGHT ( -- )
   UB-BAD @ 1+ UB-BAD !
   UB-JSON @ if UB-JSON-PREFLIGHT exit then
   s" MISSING-PREFLIGHT-REARM " UB-OUT
   UB-FILE$ UB-OUT
   UB-COLON-C UB-C UB-TOK-LINE @ UB-U$ UB-OUT
   UB-COLON-C UB-C UB-TOK-COL @ UB-U$ UB-OUT
   s" : use `LOWER-CERT-HOOK:INSTALL` before replacing the checker hook" UB-OUT UB-NL ;

: UB-REPORT-ROGUE-HOOK ( -- )
   UB-BAD @ 1+ UB-BAD !
   UB-JSON @ if UB-JSON-ROGUE-HOOK exit then
   s" UNAUDITED-HOOK " UB-OUT
   UB-FILE$ UB-OUT
   UB-COLON-C UB-C UB-TOK-LINE @ UB-U$ UB-OUT
   UB-COLON-C UB-C UB-TOK-COL @ UB-U$ UB-OUT
   s" : `" UB-OUT UB-PREV$ UB-OUT
   s" ` is not an audited checker hook" UB-OUT UB-NL ;

: UB-REPORT-ROGUE-TOP-HOOK ( -- )
   UB-BAD @ 1+ UB-BAD !
   UB-JSON @ if UB-JSON-ROGUE-TOP-HOOK exit then
   s" UNAUDITED-TOP-HOOK " UB-OUT
   UB-FILE$ UB-OUT
   UB-COLON-C UB-C UB-TOK-LINE @ UB-U$ UB-OUT
   UB-COLON-C UB-C UB-TOK-COL @ UB-U$ UB-OUT
   s" : `" UB-OUT UB-PREV$ UB-OUT
   s" ` is not an audited top-check hook" UB-OUT UB-NL ;

: UB-HANDLE-INSTALL ( -- )
   UB-SET-CHECK-INSTALL? UB-NOT if exit then
   UB-HOOK-ALLOWED? if exit then
   UB-REPORT-ROGUE-HOOK ;

: UB-HANDLE-CHECK-ON ( -- )
   UB-SET-CHECK-ON? UB-NOT if exit then
   UB-CHECK-OFF @ if UB-REPORT-PREFLIGHT then ;

: UB-HANDLE-TOP-INSTALL ( -- )
   UB-SET-TOP-CHECK-INSTALL? UB-NOT if exit then
   UB-TOP-HOOK-ALLOWED? if exit then
   UB-REPORT-ROGUE-TOP-HOOK ;

: UB-HANDLE-COLON ( -- )
   UB-CHECK-OFF @ UB-NOT if exit then
   UB-NEXT-TOK UB-NOT if exit then
   UB-TOK$ 2dup UB-HOOK-NAME? if 2drop exit then
   UB-REPORT-DEFINITION ;

: UB-RESET-FILE-SCAN ( -- )
   UB-NUM UB-PREV-A! 0 UB-PREV-U !
   UB-NUM UB-PREV2-A! 0 UB-PREV2-U !
   0 UB-I ! 1 UB-LINE ! 1 UB-COL !
   UB-CARRY-OFF @ UB-CHECK-OFF !
   UB-FALSE UB-TRUSTED !
   UB-FALSE UB-IN-DEF ! ;

: UB-CLEAR-MAPPED-SPANS ( -- )
   NULL$ drop UB-SRC-A!   0 UB-SRC-U !  0 UB-SRC-CAP !
   NULL$ drop UB-TOK-A!   0 UB-TOK-U !
   NULL$ drop UB-PREV-A!  0 UB-PREV-U !
   NULL$ drop UB-PREV2-A! 0 UB-PREV2-U ! ;

: UB-CLEAR-SPANS ( -- )
   NULL$ drop UB-FILE-A! 0 UB-FILE-U !
   UB-CLEAR-MAPPED-SPANS ;

: UB-MAPPED-SPANS-CLEAR? ( -- bool )
   UB-SRC-A@ NULL$ drop =
   UB-SRC-U @ 0= and
   UB-SRC-CAP @ 0= and
   UB-TOK-A@ NULL$ drop = and
   UB-TOK-U @ 0= and
   UB-PREV-A@ NULL$ drop = and
   UB-PREV-U @ 0= and
   UB-PREV2-A@ NULL$ drop = and
   UB-PREV2-U @ 0= and ;

: UB-SPANS-CLEAR? ( -- bool )
   UB-FILE-A@ NULL$ drop =
   UB-FILE-U @ 0= and
   UB-MAPPED-SPANS-CLEAR? and ;

: UB-REQUIRE-MAPPED-CLEAR ( -- )
   UB-MAPPED-SPANS-CLEAR? 0= if E-BUF-STATE throw then ;

: UB-REQUIRE-IDLE ( -- )
   UB-SPANS-CLEAR? 0= if E-BUF-STATE throw then ;

: UB-SCAN ( -- )
   UB-RESET-FILE-SCAN
   begin UB-NEXT-TOK while
      UB-TRUSTED? if
         UB-TRUE UB-TRUSTED !
         UB-TRUE UB-IN-DEF !
      then
      UB-STRICT-BOUNDARY @ if
         UB-CHECKER-MUTATION? UB-TRUSTED @ UB-NOT and if UB-REPORT-MUTATION then
      then
      UB-SET-CHECK-OFF? UB-TOP? and if UB-TRUE UB-CHECK-OFF! then
      UB-DEF-OPEN? if
         UB-COLON? if UB-HANDLE-COLON then
         UB-TRUE UB-IN-DEF !
      then
      UB-PREFLIGHT-INSTALL? UB-TOP? and if UB-FALSE UB-CHECK-OFF! then
      UB-TOP? if UB-HANDLE-CHECK-ON then
      UB-HANDLE-INSTALL
      UB-HANDLE-TOP-INSTALL
      UB-SEMI? if
         UB-FALSE UB-TRUSTED !
         UB-FALSE UB-IN-DEF !
      then
      UB-SAVE-PREV
   repeat ;

: UB-MAPPED-SCAN ( -- )
   UB-FILE$ UB-SRC-A@ UB-SRC-CAP @ READ-ALL {: used:n :}
   used UB-SRC-U !
   UB-SCAN ;

: UB-MAPPED-FILE ( n ptr u8 CAD-NUM:alloc-byte-len -- )
   {: size:n src:ptr extent:CAD-NUM:alloc-byte-len :}
   src UB-SRC-A!
   size UB-SRC-CAP !
   [: UB-MAPPED-SCAN ;] catch {: rc:n :}
   UB-CLEAR-MAPPED-SPANS
   rc 0 <> if rc throw then ;

: UB-FILE-ACT ( -- )
   UB-FILE$ FILE-SIZE {: bytes:n :}
   bytes 0= if
      UB-NUM UB-SRC-A!
      UB-SCAN
      exit
   then
   bytes bytes MEM:BYTES-ALLOC-LEN [: UB-MAPPED-FILE ;] MEM:WITH-BYTES
   UB-REQUIRE-MAPPED-CLEAR ;

public

: RESET ( -- )
   UB-CLEAR-SPANS
   0 UB-BAD !
   UB-FALSE UB-CHECK-OFF!
   UB-FALSE JSON!
   UB-FALSE STRICT!
   1 >FD OUT-FD! ;

: FILE ( ptr u8 n -- )
   {: path:ptr pu:n :}
   UB-REQUIRE-IDLE
   path UB-FILE-A! pu UB-FILE-U !
   [: UB-FILE-ACT ;] catch {: rc:n :}
   UB-CLEAR-SPANS
   rc 0 <> if rc throw then ;

: FINISH ( -- )
   UB-BAD @ 0 > if 1 throw then ;

;package
