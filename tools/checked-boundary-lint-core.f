\ checked-boundary-lint-core.f - forbid unaudited top-check hook installs.

require src/lib/error.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/source.f
require tools/hook-sites.f                     \ single registry of audited hook identities

package CHECKED-BOUNDARY-LINT

private

10 constant UB-LF
13 constant UB-CR
32 constant UB-SP
34 constant UB-DQ
40 constant UB-LPAREN
41 constant UB-RPAREN
9 constant UB-TAB
92 constant UB-BSLASH

TYPED-VARIABLE UB-FILE-A ptr u8
variable UB-FILE-U
TYPED-VARIABLE UB-LABEL-A ptr u8
variable UB-LABEL-U
TYPED-VARIABLE UB-SRC-A ptr u8
DYNAMIC-BUFFER UB-SRC-MEM u8   \ the file being scanned, read whole; released after its scan
variable UB-SRC-U
variable UB-I
TYPED-VARIABLE UB-TOK-A ptr u8
variable UB-TOK-U
TYPED-VARIABLE UB-PREV-A ptr u8
variable UB-PREV-U
TYPED-VARIABLE UB-PREV2-A ptr u8
variable UB-PREV2-U
variable UB-BAD
variable UB-STRICT-BOUNDARY

: UB-FILE-A@ ( -- ptr u8 )
   UB-FILE-A @ ;

: UB-FILE$ ( -- ptr u8 n )
   UB-FILE-A@ UB-FILE-U @ ;

: UB-LABEL$ ( -- ptr u8 n )
   UB-LABEL-A @ UB-LABEL-U @ ;

: UB-SRC-A@ ( -- ptr u8 )
   UB-SRC-A @ ;

: UB-TOK-A@ ( -- ptr u8 )
   UB-TOK-A @ ;

: UB-PREV-A@ ( -- ptr u8 )
   UB-PREV-A @ ;

: UB-PREV2-A@ ( -- ptr u8 )
   UB-PREV2-A @ ;

: UB-FILE-A! ( ptr u8 -- )
   UB-FILE-A ! ;

: UB-LABEL-A! ( ptr u8 -- )
   UB-LABEL-A ! ;

: UB-SRC-A! ( ptr u8 -- )
   UB-SRC-A ! ;

: UB-TOK-A! ( ptr u8 -- )
   UB-TOK-A ! ;

: UB-PREV-A! ( ptr u8 -- )
   UB-PREV-A ! ;

: UB-PREV2-A! ( ptr u8 -- )
   UB-PREV2-A ! ;

: UB-TRUE ( -- bool )
   0 0= ;

: UB-FALSE ( -- bool )
   UB-TRUE 0= ;

: UB-NOT ( bool -- bool )
   IF UB-FALSE ELSE UB-TRUE THEN ;

public

: STRICT! ( bool -- )
   UB-STRICT-BOUNDARY ! ;

private

: UB-END? ( -- bool )
   UB-I @ UB-SRC-U @ >= ;

: UB-C@ ( -- n )
   UB-SRC-A@ UB-I @ + c@ ;

: UB-ADV ( -- n )
   UB-C@
   UB-I @ 1+ UB-I ! ;

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

: UB-TOK=CI ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   UB-TOK$ a u LINT-STR=CI ;

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
   0 UB-TOK-U !
   begin UB-TOK-MORE? while
      UB-ADV drop
      UB-TOK-U @ 1+ UB-TOK-U !
   repeat
   UB-TOK$ SOURCE:ESC-STRING-OPENER? if UB-SKIP-ESC-STRING else
   UB-TOK$ SOURCE:NORMAL-STRING-OPENER? if UB-SKIP-STRING then then
   UB-TRUE ;

\ A hook install is `' NAME set-top-check` / `['] NAME set-top-check`: current
\ token `set-top-check`, two-back token the tick, one-back token the installed
\ hook name. Requiring the tick two-back also excludes the `' set-top-check`
\ name reference (there the tick is one-back and the name slot is
\ `set-top-check` itself).
: UB-TICK-PREV2? ( -- bool )
   s" '" UB-PREV2= s" [']" UB-PREV2= or ;

\ The tier-2 top-level reject enforcer installs through `set-top-check`
\ (src/core/top-row.f TR-INSTALL): a name installed there diverts pre-execution
\ reject enforcement unless the registry authorizes it for this exact file.
: UB-SET-TOP-CHECK-INSTALL? ( -- bool )
   s" set-top-check" UB-TOK=CI UB-TICK-PREV2? and ;

\ tools/hook-sites.f rows name a file and a word, so an install is audited only
\ when the scanned file and installed name match a row. Paths compare exactly as
\ committed, so `./path`, an absolute or a workspace-prefixed path fails closed.
: UB-TOP-HOOK-ALLOWED? ( -- bool )
   UB-FILE$ UB-PREV$ HOOK-SITES:TOP-MATCH? ;

\ A finding is one rejection record, kept so the scan goes on: UB-OPEN opens it
\ with its code and its message follows; UB-PLACE places it at the token, whose
\ label is given, with the expected and found notes; UB-FINDING ends it with its
\ help.
: UB-OPEN ( ptr u8 n -- ) {: code:ptr cu:n :}
   UB-BAD @ 1+ UB-BAD !
   code cu REJECTION:OPEN ;

: UB-PLACE ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: t:ptr tu:n label:ptr lu:n e:ptr eu:n f:ptr fu:n :}
   t tu REJECTION:AT  label lu REJECTION:EMIT-STR
   s" expected" REJECTION:NOTE  e eu REJECTION:EMIT-STR
   s" found" REJECTION:NOTE  f fu REJECTION:EMIT-STR ;

: UB-FINDING ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   UB-PLACE
   REJECTION:HELP
   s" keep generated and user code checked; move checker mutations behind an audited boundary"
   REJECTION:EMIT-STR
   REJECTION:KEEP ;

: UB-REPORT-ROGUE-TOP-HOOK ( -- )
   s" E-UNAUDITED-TOP-HOOK" UB-OPEN
   s" `" REJECTION:EMIT-STR  UB-PREV$ REJECTION:EMIT-STR
   s" ` is not an audited top-check hook" REJECTION:EMIT-STR
   UB-PREV$ s" no row of tools/hook-sites.f names it in this file"
   s" audited top-check hook" UB-PREV$ UB-FINDING ;

: UB-HANDLE-TOP-INSTALL ( -- )
   UB-SET-TOP-CHECK-INSTALL? UB-NOT if exit then
   UB-TOP-HOOK-ALLOWED? if exit then
   UB-REPORT-ROGUE-TOP-HOOK ;

: UB-RESET-FILE-SCAN ( -- )
   NULL$ drop UB-PREV-A! 0 UB-PREV-U !
   NULL$ drop UB-PREV2-A! 0 UB-PREV2-U !
   0 UB-I ! ;

: UB-CLEAR-SRC-SPANS ( -- )
   NULL$ drop UB-SRC-A!   0 UB-SRC-U !
   NULL$ drop UB-TOK-A!   0 UB-TOK-U !
   NULL$ drop UB-PREV-A!  0 UB-PREV-U !
   NULL$ drop UB-PREV2-A! 0 UB-PREV2-U ! ;

: UB-CLEAR-SPANS ( -- )
   NULL$ drop UB-FILE-A! 0 UB-FILE-U !
   NULL$ drop UB-LABEL-A! 0 UB-LABEL-U !
   UB-CLEAR-SRC-SPANS ;

: UB-SRC-SPANS-CLEAR? ( -- bool )
   UB-SRC-A@ NULL$ drop =
   UB-SRC-U @ 0= and
   UB-TOK-A@ NULL$ drop = and
   UB-TOK-U @ 0= and
   UB-PREV-A@ NULL$ drop = and
   UB-PREV-U @ 0= and
   UB-PREV2-A@ NULL$ drop = and
   UB-PREV2-U @ 0= and ;

: UB-SPANS-CLEAR? ( -- bool )
   UB-FILE-A@ NULL$ drop =
   UB-FILE-U @ 0= and
   UB-SRC-SPANS-CLEAR? and ;

: UB-REQUIRE-SRC-CLEAR ( -- )
   UB-SRC-SPANS-CLEAR? 0= if E-BUF-STATE throw then ;

: UB-REQUIRE-IDLE ( -- )
   UB-SPANS-CLEAR? 0= if E-BUF-STATE throw then ;

: UB-SCAN ( -- )
   UB-RESET-FILE-SCAN
   begin UB-NEXT-TOK while
      UB-HANDLE-TOP-INSTALL
      UB-SAVE-PREV
   repeat ;

: UB-SRC-ROOM ( n -- ptr u8 )
   UB-SRC-MEM-RESERVE 0 UB-SRC-MEM ;

\ The file is read to its end however it grows while it is read: its size,
\ whose refusal (E-FS-STAT) stays a missing or irregular file's, is only the
\ first room (src/lib/source.f READ-WHOLE-SAMPLED). The scan takes the bytes after
\ the read, which may move the storage.
: UB-READ-SCAN ( -- )
   UB-FILE$  UB-FILE$ FS:FILE-SIZE  [: UB-SRC-ROOM ;] SOURCE:READ-WHOLE-SAMPLED UB-SRC-U !
   0 UB-SRC-MEM UB-SRC-A!
   UB-LABEL$ REJECTION:FILE!
   UB-SRC-A@ UB-SRC-U @ 1 1 0 REJECTION:SOURCE!
   UB-SCAN ;

\ The storage lives for one file's scan, armed as the rejection source: it is
\ disarmed and the spans into it are cleared, then it is released, on return
\ or throw.
: UB-FILE-ACT ( -- )
   [: UB-READ-SCAN ;] catch {: rc:n :}
   REJECTION:SOURCE-OFF
   UB-CLEAR-SRC-SPANS
   UB-SRC-MEM-RELEASE
   rc 0 <> if rc throw then
   UB-REQUIRE-SRC-CLEAR ;

public

: RESET ( -- )
   UB-CLEAR-SPANS
   0 UB-BAD !
   UB-FALSE STRICT! ;

\ PATH is read and matched against the hook registry as spelled; LABEL names it
\ in every report.
: FILE-AS ( ptr u8 n ptr u8 n -- )
   {: path:ptr pu:n label:ptr lu:n :}
   UB-REQUIRE-IDLE
   path UB-FILE-A! pu UB-FILE-U !
   label UB-LABEL-A! lu UB-LABEL-U !
   [: UB-FILE-ACT ;] catch {: rc:n :}
   UB-CLEAR-SPANS
   rc 0 <> if rc throw then ;

: FILE ( ptr u8 n -- )
   2dup FILE-AS ;

: FINISH ( -- )
   UB-BAD @ 0 > if CHECKER-REJECT-RC throw then ;

;package
