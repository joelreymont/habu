\ gate-diagnostics.f - checked runner for checker diagnostic contracts.
\
\ Load after test/gate-common.f.

64 constant GDX-USAGE-RC

create GDX-PATH-BUF FS-PATH-CAP allot
variable GDX-PATH-U

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

: GDX-PATH-ARGV+ ( ptr u8 n -- )
   GDX-PATH!
   GDX-PATH$  >LEN PROC-ARGV+ ;

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
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/memory.f"  >LEN PROC-ARGV+
   s" tools/json.f"  >LEN PROC-ARGV+
   s" tools/gate-json-assert.f"  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+ ;

: GDX-GJA-RUN ( ptr u8 n -- ) {: label:ptr labelu :}
   s" bin/hb" GE-TIMEOUT-MS GE-RUN-ENV
   label labelu GE-EXPECT-OK ;

: GDX-GJA1 ( ptr u8 n ptr u8 n ptr u8 n -- ) {: mode:ptr modeu file:ptr fileu label:ptr labelu :}
   GDX-GJA-RESET
   mode modeu  >LEN PROC-ARGV+
   file fileu GDX-PATH-ARGV+
   label labelu GDX-GJA-RUN ;

: GDX-GJA2S ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- ) {: mode:ptr modeu file:ptr fileu arg:ptr argu label:ptr labelu :}
   GDX-GJA-RESET
   mode modeu  >LEN PROC-ARGV+
   file fileu GDX-PATH-ARGV+
   arg argu  >LEN PROC-ARGV+
   label labelu GDX-GJA-RUN ;

: GDX-GJA2P ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- ) {: mode:ptr modeu file:ptr fileu arg:ptr argu label:ptr labelu :}
   GDX-GJA-RESET
   mode modeu  >LEN PROC-ARGV+
   file fileu GDX-PATH-ARGV+
   arg argu GDX-PATH-ARGV+
   label labelu GDX-GJA-RUN ;

: GDX-CHECK-JSON ( ptr u8 n -- ) {: label:ptr labelu :}
   GE-CHECK-ARGV
   s" --json-errors"  >LEN PROC-ARGV+
   GE-CHECK-EXE GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   label labelu GE-EXPECT-NONZERO ;

: GDX-CHECK-JSON-ALL ( ptr u8 n -- ) {: label:ptr labelu :}
   GE-CHECK-ARGV
   s" --json-errors"  >LEN PROC-ARGV+
   s" --all-errors"  >LEN PROC-ARGV+
   GE-CHECK-EXE GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   label labelu GE-EXPECT-NONZERO ;

: GDX-CHECK-FILE-JSON ( ptr u8 n ptr u8 n -- ) {: file:ptr fileu label:ptr labelu :}
   GE-CHECK-ARGV
   s" --json-errors"  >LEN PROC-ARGV+
   file fileu GDX-PATH-ARGV+
   GE-CHECK-EXE GE-TIMEOUT-MS GE-RUN-ENV
   label labelu GE-EXPECT-NONZERO ;

: GDX-CHECK-FILE-JSON-ALL ( ptr u8 n ptr u8 n -- ) {: file:ptr fileu label:ptr labelu :}
   GE-CHECK-ARGV
   s" --json-errors"  >LEN PROC-ARGV+
   s" --all-errors"  >LEN PROC-ARGV+
   file fileu GDX-PATH-ARGV+
   GE-CHECK-EXE GE-TIMEOUT-MS GE-RUN-ENV
   label labelu GE-EXPECT-NONZERO ;

: GDX-CHECK-FILE-STRICT-JSON ( ptr u8 n ptr u8 n -- ) {: file:ptr fileu label:ptr labelu :}
   GE-CHECK-ARGV
   s" --strict-signatures"  >LEN PROC-ARGV+
   s" --json-errors"  >LEN PROC-ARGV+
   file fileu GDX-PATH-ARGV+
   GE-CHECK-EXE GE-TIMEOUT-MS GE-RUN-ENV
   label labelu GE-EXPECT-NONZERO ;

: GDX-CHECK-STRICT ( ptr u8 n -- ) {: label:ptr labelu :}
   GE-CHECK-ARGV
   s" --strict-signatures"  >LEN PROC-ARGV+
   GE-CHECK-EXE GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   label labelu GE-EXPECT-NONZERO ;

: GDX-CHECK-STRICT-JSON ( ptr u8 n -- ) {: label:ptr labelu :}
   GE-CHECK-ARGV
   s" --strict-signatures"  >LEN PROC-ARGV+
   s" --json-errors"  >LEN PROC-ARGV+
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
   s" : NOSIG dup ;" GE-SRC-LINE
   s" tools/check.f --strict-signatures accepted nosig" GDX-CHECK-STRICT
   s" E-MISSING-SIGNATURE" s" strict-signatures text diagnostic" GE-EXPECT-ERR-HAS
   GE-HB-RESET
   GE-SRC-RESET
   s" : NOSIG dup ;" GE-SRC-LINE
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
   s" --load"  >LEN PROC-ARGV+
   s" load-bad.f" GDX-PATH-ARGV+
   s" bin/hb" GE-TIMEOUT-MS GE-RUN-ENV
   s" checked load accepted bad declared effect" GE-EXPECT-NONZERO ;

: GDX-ALL-ERRORS-SOURCE ( -- )
   GE-SRC-RESET
   s" : OK ( i64 -- i64 ) dup * ;" GE-SRC-LINE
   s" : SEMI ( -- i64 ) [char] ; ;" GE-SRC-LINE
   s" : BAD1 ( i64 -- i64 ) dup ;" GE-SRC-LINE
   s" : BAD2 ( i64 -- ) >r ;" GE-SRC-LINE ;

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
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/memory.f"  >LEN PROC-ARGV+
   s" tools/json.f"  >LEN PROC-ARGV+
   s" tools/diag-to-sarif.f"  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+
   s" habu-all-errors.err" GDX-PATH-ARGV+
   s" bin/hb" GE-TIMEOUT-MS GE-RUN-ENV
   s" diag-to-sarif" GE-EXPECT-OK
   s" habu-all-errors.sarif" GDX-WRITE-OUT
   s" sarif" s" habu-all-errors.sarif" s" sarif output" GDX-GJA1 ;

: GDX-PUBLIC-SIGNATURES ( -- )
   GE-HB-RESET
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/memory.f"  >LEN PROC-ARGV+
   s" lib/vector.f"  >LEN PROC-ARGV+
   s" tools/lint/text.f"  >LEN PROC-ARGV+
   s" tools/lint/intern.f"  >LEN PROC-ARGV+
   s" tools/lint/token.f"  >LEN PROC-ARGV+
   s" tools/lint/lib.f"  >LEN PROC-ARGV+
   s" tools/public-signatures.f"  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+
   s" examples/llm/good.f"  >LEN PROC-ARGV+
   s" bin/hb" GE-TIMEOUT-MS GE-RUN-ENV
   s" public-signatures" GE-EXPECT-OK
   s" public-signatures.json" GDX-WRITE-OUT
   s" public-signatures" s" public-signatures.json" s" public signatures output" GDX-GJA1 ;

: GDX-TRUST-LINT-STALE ( -- )
   GE-HB-RESET
   s" --load"  >LEN PROC-ARGV+
   s" tools/date.f"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/memory.f"  >LEN PROC-ARGV+
   s" lib/fs.f"  >LEN PROC-ARGV+
   s" tools/lint/text.f"  >LEN PROC-ARGV+ s" tools/lint/token.f" >LEN PROC-ARGV+ s" tools/lint/lib.f" >LEN PROC-ARGV+
   s" tools/trust-lint-core.f"  >LEN PROC-ARGV+
   s" tools/argv.f"  >LEN PROC-ARGV+
   s" tools/trust-lint.f"  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+
   s" ."  >LEN PROC-ARGV+
   s" 2026-10-01"  >LEN PROC-ARGV+
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
   GT-CLEANUP
   s" PASS: native checker diagnostics undef-primary slice" type cr ;

: GDX-ALL-STRICT-SLICE ( -- )
   s" hb-gate-diagnostics-all-strict" GT-START
   GDX-ALL-ERRORS
   GDX-SARIF
   GDX-STRICT-SIGNATURES
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
