\ gate-diagnostics.f - checked runner for checker diagnostic contracts.
\
\ Load after test/gate-common.f.

64 constant GDX-USAGE-RC

create GDX-PATH-BUF FS-PATH-CAP allot
create GDX-TRUST-ROOT-BUF FS-PATH-CAP allot
create GDX-TRUST-SRC-DIR-BUF FS-PATH-CAP allot
create GDX-TRUST-SRC-BUF FS-PATH-CAP allot
create GDX-TRUST-MAN-BUF FS-PATH-CAP allot
variable GDX-PATH-U
variable GDX-TRUST-ROOT-U
variable GDX-TRUST-SRC-DIR-U
variable GDX-TRUST-SRC-U
variable GDX-TRUST-MAN-U

: GDX-USAGE ( -- )
   s" usage: test/gate-diagnostics.f [warm|diag-repair|diag-undef-primary|diag-all-strict|diag-file-unsafe]" GDX-USAGE-RC die ;

: GDX-ARG0= ( ptr u8 n -- bool )
   0 SCRIPT-ARGV$ STR= ;

: GDX-J-DQ ( -- )
   GE-DQ SB-APPEND-C ;

: GDX-J-COLON ( -- )
   s" :" SB-APPEND ;

: GDX-JKEY ( ptr u8 n -- )
   GDX-J-DQ
   SB-APPEND
   GDX-J-DQ ;

: GDX-EXPECT-ERR-JKEY ( ptr u8 n ptr u8 n -- ) {: key:ptr keyu label:ptr labelu :}
   SB-RESET
   key keyu GDX-JKEY
   GDX-J-COLON
   SB$ label labelu GE-EXPECT-ERR-HAS ;

: GDX-EXPECT-ERR-JRAW ( ptr u8 n ptr u8 n ptr u8 n -- ) {: key:ptr keyu raw:ptr rawu label:ptr labelu :}
   SB-RESET
   key keyu GDX-JKEY
   GDX-J-COLON
   raw rawu SB-APPEND
   SB$ label labelu GE-EXPECT-ERR-HAS ;

: GDX-EXPECT-ERR-JSTR ( ptr u8 n ptr u8 n ptr u8 n -- ) {: key:ptr keyu val:ptr valu label:ptr labelu :}
   SB-RESET
   key keyu GDX-JKEY
   GDX-J-COLON
   GDX-J-DQ
   val valu SB-APPEND
   SB$ label labelu GE-EXPECT-ERR-HAS ;

: GDX-PATH! ( ptr u8 n -- ) {: name:ptr nameu :}
   name nameu GDX-PATH-BUF GT-PATH GDX-PATH-U ! ;

: GDX-PATH$ ( -- ptr u8 n )
   GDX-PATH-BUF GDX-PATH-U @ ;

: GDX-TRUST-ROOT$ ( -- ptr u8 n )
   GDX-TRUST-ROOT-BUF GDX-TRUST-ROOT-U @ ;

: GDX-TRUST-SRC-DIR$ ( -- ptr u8 n )
   GDX-TRUST-SRC-DIR-BUF GDX-TRUST-SRC-DIR-U @ ;

: GDX-TRUST-SRC$ ( -- ptr u8 n )
   GDX-TRUST-SRC-BUF GDX-TRUST-SRC-U @ ;

: GDX-TRUST-MAN$ ( -- ptr u8 n )
   GDX-TRUST-MAN-BUF GDX-TRUST-MAN-U @ ;

: GDX-TRUST-PATHS ( -- )
   GT-ROOT s" trust-stale" GDX-TRUST-ROOT-BUF JOIN-PATH GDX-TRUST-ROOT-U !
   GDX-TRUST-ROOT$ s" src" GDX-TRUST-SRC-DIR-BUF JOIN-PATH GDX-TRUST-SRC-DIR-U !
   GDX-TRUST-SRC-DIR$ s" trust.f" GDX-TRUST-SRC-BUF JOIN-PATH GDX-TRUST-SRC-U !
   GDX-TRUST-ROOT$ s" TRUSTED.md" GDX-TRUST-MAN-BUF JOIN-PATH GDX-TRUST-MAN-U ! ;

: GDX-TRUST-SRC-TEXT$ ( -- ptr u8 n )
   SB-RESET
   s" TRUSTED: foo ( n -- n )" SB-APPEND GE-SB-LF
   s"    dup ;" SB-APPEND GE-SB-LF
   SB$ ;

: GDX-TRUST-MAN-TEXT$ ( -- ptr u8 n )
   SB-RESET
   s" | Word | Effect | Reason | Tests | Site | Last audited |" SB-APPEND GE-SB-LF
   s" |------|--------|--------|-------|------|--------------|" SB-APPEND GE-SB-LF
   s" | foo | `n -- n` | fixture | `test/t-fixture.fs` | src/trust.f:1 | 2026-06-13 |" SB-APPEND GE-SB-LF
   SB$ ;

: GDX-TRUST-FIXTURE ( -- )
   GDX-TRUST-PATHS
   GDX-TRUST-SRC-DIR$ MAKE-DIRS
   GDX-TRUST-SRC$ GDX-TRUST-SRC-TEXT$ WRITE-ALL
   GDX-TRUST-MAN$ GDX-TRUST-MAN-TEXT$ WRITE-ALL ;

: GDX-ARG+ ( ptr u8 n -- )
   GE-ARG+ ;

: GDX-PATH-ARGV+ ( ptr u8 n -- )
   GDX-PATH!
   GDX-PATH$ GDX-ARG+ ;

: GDX-WRITE-ERR ( ptr u8 n -- )
   GDX-PATH!
   GDX-PATH$ GT-ERR$ WRITE-ALL ;

: GDX-WRITE-OUT ( ptr u8 n -- )
   GDX-PATH!
   GDX-PATH$ GT-OUT$ WRITE-ALL ;

: GDX-WRITE-SRC ( ptr u8 n -- )
   GDX-PATH!
   GDX-PATH$ GE-SRC-BUF GE-SRC-U @ WRITE-ALL ;

: GDX-GJA-RESET ( -- )
   GE-HB-RESET
   s" --load" GDX-ARG+
   s" lib/errors.f" GDX-ARG+
   s" lib/memory.f" GDX-ARG+
   s" tools/json.f" GDX-ARG+
   s" tools/gate-json-assert-core.f" GDX-ARG+
   s" tools/gate-json-assert.f" GDX-ARG+
   s" --" GDX-ARG+ ;

: GDX-GJA-RUN ( ptr u8 n -- ) {: label:ptr labelu :}
   s" bin/hb" GE-TIMEOUT-MS GE-RUN-ENV
   label labelu GE-EXPECT-OK ;

: GDX-GJA1 ( ptr u8 n ptr u8 n ptr u8 n -- ) {: mode:ptr modeu file:ptr fileu label:ptr labelu :}
   GDX-GJA-RESET
   mode modeu GDX-ARG+
   file fileu GDX-PATH-ARGV+
   label labelu GDX-GJA-RUN ;

: GDX-GJA2S ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- ) {: mode:ptr modeu file:ptr fileu arg:ptr argu label:ptr labelu :}
   GDX-GJA-RESET
   mode modeu GDX-ARG+
   file fileu GDX-PATH-ARGV+
   arg argu GDX-ARG+
   label labelu GDX-GJA-RUN ;

: GDX-GJA2P ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- ) {: mode:ptr modeu file:ptr fileu arg:ptr argu label:ptr labelu :}
   GDX-GJA-RESET
   mode modeu GDX-ARG+
   file fileu GDX-PATH-ARGV+
   arg argu GDX-PATH-ARGV+
   label labelu GDX-GJA-RUN ;

: GDX-CHECK-JSON ( ptr u8 n -- ) {: label:ptr labelu :}
   GE-CHECK-ARGV
   s" --json-errors" GDX-ARG+
   GE-CHECK-EXE GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   label labelu GE-EXPECT-NONZERO ;

: GDX-CHECK-JSON-ALL ( ptr u8 n -- ) {: label:ptr labelu :}
   GE-CHECK-ARGV
   s" --json-errors" GDX-ARG+
   s" --all-errors" GDX-ARG+
   GE-CHECK-EXE GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   label labelu GE-EXPECT-NONZERO ;

: GDX-CHECK-FILE-JSON ( ptr u8 n ptr u8 n -- ) {: file:ptr fileu label:ptr labelu :}
   GE-CHECK-ARGV
   s" --json-errors" GDX-ARG+
   file fileu GDX-PATH-ARGV+
   GE-CHECK-EXE GE-TIMEOUT-MS GE-RUN-ENV
   label labelu GE-EXPECT-NONZERO ;

: GDX-CHECK-FILE-JSON-ALL ( ptr u8 n ptr u8 n -- ) {: file:ptr fileu label:ptr labelu :}
   GE-CHECK-ARGV
   s" --json-errors" GDX-ARG+
   s" --all-errors" GDX-ARG+
   file fileu GDX-PATH-ARGV+
   GE-CHECK-EXE GE-TIMEOUT-MS GE-RUN-ENV
   label labelu GE-EXPECT-NONZERO ;

: GDX-CHECK-FILE-STRICT-JSON ( ptr u8 n ptr u8 n -- ) {: file:ptr fileu label:ptr labelu :}
   GE-CHECK-ARGV
   s" --strict-signatures" GDX-ARG+
   s" --json-errors" GDX-ARG+
   file fileu GDX-PATH-ARGV+
   GE-CHECK-EXE GE-TIMEOUT-MS GE-RUN-ENV
   label labelu GE-EXPECT-NONZERO ;

: GDX-CHECK-STRICT ( ptr u8 n -- ) {: label:ptr labelu :}
   GE-CHECK-ARGV
   s" --strict-signatures" GDX-ARG+
   GE-CHECK-EXE GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   label labelu GE-EXPECT-NONZERO ;

: GDX-CHECK-STRICT-JSON ( ptr u8 n -- ) {: label:ptr labelu :}
   GE-CHECK-ARGV
   s" --strict-signatures" GDX-ARG+
   s" --json-errors" GDX-ARG+
   GE-CHECK-EXE GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   label labelu GE-EXPECT-NONZERO ;

: GDX-CHECK-JSON-FIELDS ( -- )
   s" verdict" s" rejected" s" --json-errors verdict" GDX-EXPECT-ERR-JSTR
   s" declared_effect" s" i64 -- i64 " s" --json-errors declared effect" GDX-EXPECT-ERR-JSTR
   s" inferred_effect" s" i64 -- i64 i64 " s" --json-errors inferred effect" GDX-EXPECT-ERR-JSTR
   s" token_index" s" 1" s" --json-errors token index" GDX-EXPECT-ERR-JRAW
   s" file" s" <stdin>" s" --json-errors file" GDX-EXPECT-ERR-JSTR
   s" line" s" 1" s" --json-errors line" GDX-EXPECT-ERR-JRAW
   s" column" s" --json-errors column" GDX-EXPECT-ERR-JKEY
   s" byte_start" s" --json-errors byte_start" GDX-EXPECT-ERR-JKEY
   s" byte_end" s" --json-errors byte_end" GDX-EXPECT-ERR-JKEY
   s" definition_source" s" --json-errors definition source" GDX-EXPECT-ERR-JKEY
   s" json-lines-schema" s" habu-json.err" s" json lines schema" GDX-GJA1
   s" diag-repair-class" s" habu-json.err" s" remove_producer" s" remove producer class" GDX-GJA2S ;

: GDX-PRIMARY-JSON ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : JBAD ( i64 -- i64 ) dup ;" GE-SRC-LINE
   s" tools/check.f --json-errors accepted bad def" GDX-CHECK-JSON
   s" habu-json.err" GDX-WRITE-ERR
   GDX-CHECK-JSON-FIELDS ;

: GDX-UNKNOWN-SIGNATURE ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : JSIG ( got expected -- bool ) <= ;" GE-SRC-LINE
   s" tools/check.f --json-errors accepted unknown signature type" GDX-CHECK-JSON
   s" code" s" E-UNKNOWN-SIGNATURE-TYPE" s" unknown signature type code" GDX-EXPECT-ERR-JSTR
   s" token" s" got" s" unknown signature type token" GDX-EXPECT-ERR-JSTR
   s" repair_class" s" fix_signature_type" s" unknown signature repair class" GDX-EXPECT-ERR-JSTR
   s" suggestion" s" Use a known stack-signature type or a single-letter type variable." s" unknown signature suggestion" GDX-EXPECT-ERR-JSTR ;

: GDX-BARE-PTR-SIGNATURE ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : JBPTR ( ptr -- ) drop ;" GE-SRC-LINE
   s" tools/check.f --json-errors accepted bare ptr signature" GDX-CHECK-JSON
   s" code" s" E-BARE-PTR-SIGNATURE" s" bare ptr signature code" GDX-EXPECT-ERR-JSTR
   s" repair_class" s" fix_bare_ptr_element" s" bare ptr repair class" GDX-EXPECT-ERR-JSTR
   s" suggestion" s" Give 'ptr' an element type, e.g. 'ptr u8' or 'ptr a'." s" bare ptr suggestion" GDX-EXPECT-ERR-JSTR ;

: GDX-REPAIR-CLASSES ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : JMISS ( i64 -- i64 ) drop ;" GE-SRC-LINE
   s" tools/check.f --json-errors accepted missing producer" GDX-CHECK-JSON
   s" habu-json-miss.err" GDX-WRITE-ERR
   s" diag-repair-class" s" habu-json-miss.err" s" add_producer" s" missing producer class" GDX-GJA2S
   GE-HB-RESET
   GE-SRC-RESET
   s" : JTYPE ( i64 -- i64 ) 0= ;" GE-SRC-LINE
   s" tools/check.f --json-errors accepted type mismatch" GDX-CHECK-JSON
   s" habu-json-type.err" GDX-WRITE-ERR
   s" diag-repair-class" s" habu-json-type.err" s" fix_type" s" type mismatch class" GDX-GJA2S
   GE-HB-RESET
   GE-SRC-RESET
   s" : JRET ( i64 -- ) >r ;" GE-SRC-LINE
   s" tools/check.f --json-errors accepted return-stack imbalance" GDX-CHECK-JSON
   s" habu-json-ret.err" GDX-WRITE-ERR
   s" diag-repair-class" s" habu-json-ret.err" s" fix_return_stack" s" return stack class" GDX-GJA2S
   GE-HB-RESET
   GE-SRC-RESET
   s" : JDEAD ( i64 -- i64 ) dup 0 < if 1 throw 0 then 1 + ;" GE-SRC-LINE
   s" tools/check.f --json-errors accepted dead-code padding" GDX-CHECK-JSON
   s" code" s" E-DEAD-CODE" s" dead-code diagnostic code" GDX-EXPECT-ERR-JSTR
   s" dead_owner" s" throw" s" dead-code owner" GDX-EXPECT-ERR-JSTR
   s" habu-json-dead.err" GDX-WRITE-ERR
   s" diag-repair-class" s" habu-json-dead.err" s" remove_dead_code" s" dead-code class" GDX-GJA2S
   GE-HB-RESET
   GE-SRC-RESET
   s" : JDIE ( i64 -- i64 ) dup 0 < if here 0 1 die 0 then 1 + ;" GE-SRC-LINE
   s" tools/check.f --json-errors accepted die dead-code padding" GDX-CHECK-JSON
   s" code" s" E-DEAD-CODE" s" die dead-code diagnostic code" GDX-EXPECT-ERR-JSTR
   s" dead_owner" s" die" s" die dead-code owner" GDX-EXPECT-ERR-JSTR ;

: GDX-FILE-ORIGIN ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" \ prelude" GE-SRC-LINE
   s" " GE-SRC-LINE
   s" : JBAD ( i64 -- i64 ) dup ;" GE-SRC-LINE
   s" habu-json-file.f" GDX-WRITE-SRC
   s" habu-json-file.f" s" tools/check.f --json-errors accepted file bad def" GDX-CHECK-FILE-JSON
   s" habu-json-file.err" GDX-WRITE-ERR
   s" diag-file-origin" s" habu-json-file.err" s" habu-json-file.f" s" file origin" GDX-GJA2P ;

: GDX-STRICT-SIGNATURES ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : GDX-NOSIG dup ;" GE-SRC-LINE
   s" tools/check.f --strict-signatures accepted nosig" GDX-CHECK-STRICT
   s" E-MISSING-SIGNATURE" s" strict-signatures text diagnostic" GE-EXPECT-ERR-HAS
   GE-HB-RESET
   GE-SRC-RESET
   s" : GDX-NOSIG dup ;" GE-SRC-LINE
   s" tools/check.f --strict-signatures --json-errors accepted nosig" GDX-CHECK-STRICT-JSON
   s" code" s" E-MISSING-SIGNATURE" s" strict-signatures JSON diagnostic" GDX-EXPECT-ERR-JSTR
   GE-HB-RESET
   GE-SRC-RESET
   s" : X ( infer ) dup ;" GE-SRC-LINE
   s" tools/check.f --strict-signatures accepted infer opt-out" GDX-CHECK-STRICT-JSON
   s" code" s" E-UNVERIFIED-SIGNATURE" s" strict-signatures opt-out diagnostic" GDX-EXPECT-ERR-JSTR ;

: GDX-UNSAFE-CHECK-SOURCE ( -- )
   GE-SRC-RESET
   s" EV ( -- n ) evaluate" GE-SRC-CHECK-LINE
   s" PO ( -- ) postpone dup" GE-SRC-CHECK-LINE
   s" CO ( -- ) compile," GE-SRC-CHECK-LINE
   s" IM ( -- ) immediate" GE-SRC-CHECK-LINE
   s" LB ( -- ) [" GE-SRC-CHECK-LINE
   s" RB ( -- ) ]" GE-SRC-CHECK-LINE ;

: GDX-UNSAFE-CHECKS ( -- )
   GE-HB-RESET
   GDX-UNSAFE-CHECK-SOURCE
   s" unsafe compiler words verdicts" GE-HB-RUN-STDIN
   SB-RESET
   s" 0" GE-OUT-LINE s" 0" GE-OUT-LINE s" 0" GE-OUT-LINE
   s" 0" GE-OUT-LINE s" 0" GE-OUT-LINE s" 0" GE-OUT-LINE
   SB$ s" unsafe compiler words verdict output" GE-EXPECT-OUT
   GE-HB-RESET
   GE-SRC-RESET
   s" : EV ( -- n ) evaluate ;" GE-SRC-LINE
   s" tools/check.f accepted unsafe evaluate" GDX-CHECK-JSON
   s" habu-unsafe.err" GDX-WRITE-ERR
   s" code" s" E-UNSAFE" s" unsafe checker E-UNSAFE" GDX-EXPECT-ERR-JSTR
   s" token" s" evaluate" s" unsafe checker token" GDX-EXPECT-ERR-JSTR
   s" diag-repair-class" s" habu-unsafe.err" s" trusted_boundary_required" s" unsafe repair class" GDX-GJA2S
   GE-HB-RESET
   GE-SRC-RESET
   s" : EV ( -- n ) evaluate ;" GE-SRC-LINE
   s" EV ." GE-SRC-LINE
   s" hb published unsafe evaluate definition" GE-HB-RUN-STDIN-NZ ;

: GDX-LOAD-FAIL-CLOSED ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : LOAD-BAD ( -- ) 1 ;" GE-SRC-LINE
   s" load-bad.f" GDX-WRITE-SRC
   s" --load" GDX-ARG+
   s" load-bad.f" GDX-PATH-ARGV+
   s" bin/hb" GE-TIMEOUT-MS GE-RUN-ENV
   s" checked load accepted bad declared effect" GE-EXPECT-NONZERO ;

: GDX-ALL-ERRORS-SOURCE ( -- )
   GE-SRC-RESET
   s" : GDX-AE-OK ( i64 -- i64 ) dup * ;" GE-SRC-LINE
   s" : GDX-AE-SEMI ( -- i64 ) [char] ; ;" GE-SRC-LINE
   s" : GDX-AE-BAD1 ( i64 -- i64 ) dup ;" GE-SRC-LINE
   s" : GDX-AE-BAD2 ( i64 -- ) >r ;" GE-SRC-LINE ;

: GDX-ALL-ERRORS ( -- )
   GE-HB-RESET
   GDX-ALL-ERRORS-SOURCE
   s" habu-all-errors.f" GDX-WRITE-SRC
   s" habu-all-errors.f" s" tools/check.f --all-errors accepted bad defs" GDX-CHECK-FILE-JSON-ALL
   s" habu-all-errors.err" GDX-WRITE-ERR
   s" all-errors" s" habu-all-errors.err" s" all-errors diagnostics" GDX-GJA1 ;

: GDX-UNDEFINED-RECURSIVE ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : UDEF ( i64 -- i64 ) dup NOPE ;" GE-SRC-LINE
   s" tools/check.f --all-errors accepted undefined word" GDX-CHECK-JSON-ALL
   s" habu-undef.err" GDX-WRITE-ERR
   s" code" s" E-UNDEFINED" s" undefined diagnostic code" GDX-EXPECT-ERR-JSTR
   s" token" s" NOPE" s" undefined diagnostic token" GDX-EXPECT-ERR-JSTR
   s" json-one-schema" s" habu-undef.err" s" undefined schema" GDX-GJA1
   s" diag-repair-class" s" habu-undef.err" s" unknown_rejection" s" undefined repair class" GDX-GJA2S
   GE-HB-RESET
   GE-SRC-RESET
   s" : POW ( i64 -- i64 ) dup POW ;" GE-SRC-LINE
   s" tools/check.f --all-errors accepted recursive self-call" GDX-CHECK-JSON-ALL
   s" habu-recursive.err" GDX-WRITE-ERR
   s" token" s" POW" s" recursive diagnostic token" GDX-EXPECT-ERR-JSTR
   s" json-one-schema" s" habu-recursive.err" s" recursive schema" GDX-GJA1 ;

: GDX-SARIF ( -- )
   GE-HB-RESET
   s" --load" GDX-ARG+
   s" lib/errors.f" GDX-ARG+
   s" lib/memory.f" GDX-ARG+
   s" tools/json.f" GDX-ARG+
   s" tools/diag-to-sarif.f" GDX-ARG+
   s" --" GDX-ARG+
   s" habu-all-errors.err" GDX-PATH-ARGV+
   s" bin/hb" GE-TIMEOUT-MS GE-RUN-ENV
   s" diag-to-sarif" GE-EXPECT-OK
   s" habu-all-errors.sarif" GDX-WRITE-OUT
   s" sarif" s" habu-all-errors.sarif" s" sarif output" GDX-GJA1 ;

: GDX-PUBLIC-SIGNATURES ( -- )
   GE-HB-RESET
   s" --load" GDX-ARG+
   s" lib/errors.f" GDX-ARG+
   s" lib/string.f" GDX-ARG+
   s" lib/memory.f" GDX-ARG+
   s" lib/vector.f" GDX-ARG+
   s" tools/lint/text.f" GDX-ARG+
   s" tools/lint/intern.f" GDX-ARG+
   s" tools/lint/token.f" GDX-ARG+
   s" tools/lint/lib.f" GDX-ARG+
   s" tools/public-signatures.f" GDX-ARG+
   s" --" GDX-ARG+
   s" examples/llm/good.f" GDX-ARG+
   s" bin/hb" GE-TIMEOUT-MS GE-RUN-ENV
   s" public-signatures" GE-EXPECT-OK
   s" public-signatures.json" GDX-WRITE-OUT
   s" public-signatures" s" public-signatures.json" s" public signatures output" GDX-GJA1 ;

: GDX-TRUST-LINT-STALE ( -- )
   GDX-TRUST-FIXTURE
   GE-HB-RESET
   s" --load" GDX-ARG+
   s" tools/date.f" GDX-ARG+
   s" lib/errors.f" GDX-ARG+
   s" lib/string.f" GDX-ARG+
   s" lib/memory.f" GDX-ARG+
   s" lib/fs.f" GDX-ARG+
   s" tools/lint/text.f" GDX-ARG+
   s" tools/lint/token.f" GDX-ARG+
   s" tools/lint/lib.f" GDX-ARG+
   s" tools/trust-lint-core.f" GDX-ARG+
   s" tools/argv.f" GDX-ARG+
   s" tools/trust-lint.f" GDX-ARG+
   s" --" GDX-ARG+
   GDX-TRUST-ROOT$ GDX-ARG+
   s" 2026-10-01" GDX-ARG+
   s" bin/hb" GE-TIMEOUT-MS GE-RUN-ENV
   s" trust-lint accepted stale audit dates" GE-EXPECT-NONZERO
   s" STALE-AUDIT" s" trust-lint stale audit diagnostic" GE-EXPECT-OUT-HAS ;

: GDX-WARM ( -- )
   s" hb-gate-diagnostics-warm" GT-START
   GE-CHECK-WARM
   GT-CLEANUP
   s" PASS: native checker warm image gate phase" type cr ;

: GDX-REPAIR-SLICE ( -- )
   s" hb-gate-diagnostics-repair" GT-START
   GDX-REPAIR-CLASSES
   GDX-PUBLIC-SIGNATURES
   GDX-TRUST-LINT-STALE
   GT-CLEANUP
   s" PASS: native checker diagnostics repair slice" type cr ;

: GDX-UNDEF-PRIMARY-SLICE ( -- )
   s" hb-gate-diagnostics-undef-primary" GT-START
   GDX-UNDEFINED-RECURSIVE
   GDX-PRIMARY-JSON
   GDX-UNKNOWN-SIGNATURE
   GT-CLEANUP
   s" PASS: native checker diagnostics undef-primary slice" type cr ;

: GDX-ALL-STRICT-SLICE ( -- )
   s" hb-gate-diagnostics-all-strict" GT-START
   GDX-ALL-ERRORS
   GDX-SARIF
   GDX-STRICT-SIGNATURES
   GDX-BARE-PTR-SIGNATURE
   GDX-LOAD-FAIL-CLOSED
   GT-CLEANUP
   s" PASS: native checker diagnostics all-strict slice" type cr ;

: GDX-FILE-UNSAFE-SLICE ( -- )
   s" hb-gate-diagnostics-file-unsafe" GT-START
   GDX-FILE-ORIGIN
   GDX-UNSAFE-CHECKS
   GT-CLEANUP
   s" PASS: native checker diagnostics file-unsafe slice" type cr ;

: GDX-SERIAL ( -- )
   s" hb-gate-diagnostics" GT-START
   GDX-PRIMARY-JSON
   GDX-UNKNOWN-SIGNATURE
   GDX-BARE-PTR-SIGNATURE
   GDX-REPAIR-CLASSES
   GDX-FILE-ORIGIN
   GDX-STRICT-SIGNATURES
   GDX-UNSAFE-CHECKS
   GDX-LOAD-FAIL-CLOSED
   GDX-ALL-ERRORS
   GDX-UNDEFINED-RECURSIVE
   GDX-SARIF
   GDX-PUBLIC-SIGNATURES
   GDX-TRUST-LINT-STALE
   GT-CLEANUP
   s" PASS: native checker diagnostics gate phase" type cr ;

: GDX-DISPATCH ( -- )
   SCRIPT-ARGC 0= if GDX-SERIAL exit then
   SCRIPT-ARGC 1 <> if GDX-USAGE then
   s" warm" GDX-ARG0= if GDX-WARM exit then
   s" diag-repair" GDX-ARG0= if GDX-REPAIR-SLICE exit then
   s" diag-undef-primary" GDX-ARG0= if GDX-UNDEF-PRIMARY-SLICE exit then
   s" diag-all-strict" GDX-ARG0= if GDX-ALL-STRICT-SLICE exit then
   s" diag-file-unsafe" GDX-ARG0= if GDX-FILE-UNSAFE-SLICE exit then
   GDX-USAGE ;

GDX-DISPATCH
