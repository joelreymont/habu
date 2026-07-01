\ gate-diagnostics.f - checked runner for checker diagnostic contracts.
\
\ Load after test/gate-common.f, tools/json.f, and tools/gate-json-assert-core.f.

64 constant GDX-USAGE-RC

create GDX-PATH-BUF FS-PATH-CAP allot
create GDX-PATH2-BUF FS-PATH-CAP allot
create GDX-TRUST-ROOT-BUF FS-PATH-CAP allot
create GDX-TRUST-SRC-DIR-BUF FS-PATH-CAP allot
create GDX-TRUST-SRC-BUF FS-PATH-CAP allot
create GDX-TRUST-MAN-BUF FS-PATH-CAP allot
variable GDX-PATH-U
variable GDX-PATH2-U
variable GDX-TRUST-ROOT-U
variable GDX-TRUST-SRC-DIR-U
variable GDX-TRUST-SRC-U
variable GDX-TRUST-MAN-U

: GDX-USAGE ( -- )
   s" usage: test/gate-diagnostics.f [diag-repair|diag-undef-primary|diag-all-strict|diag-file-unsafe]" GDX-USAGE-RC die ;

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

: GDX-EXPECT-ERR-JKEY ( ptr u8 n ptr u8 n -- ) {: key:ptr keyu:n label:ptr labelu:n :}
   SB-RESET
   key keyu GDX-JKEY
   GDX-J-COLON
   SB$ label labelu GE-EXPECT-ERR-HAS ;

: GDX-EXPECT-ERR-JRAW ( ptr u8 n ptr u8 n ptr u8 n -- ) {: key:ptr keyu:n raw:ptr rawu:n label:ptr labelu:n :}
   SB-RESET
   key keyu GDX-JKEY
   GDX-J-COLON
   raw rawu SB-APPEND
   SB$ label labelu GE-EXPECT-ERR-HAS ;

: GDX-EXPECT-ERR-JSTR ( ptr u8 n ptr u8 n ptr u8 n -- ) {: key:ptr keyu:n val:ptr valu:n label:ptr labelu:n :}
   SB-RESET
   key keyu GDX-JKEY
   GDX-J-COLON
   GDX-J-DQ
   val valu SB-APPEND
   SB$ label labelu GE-EXPECT-ERR-HAS ;

: GDX-PATH! ( ptr u8 n -- ) {: name:ptr nameu:n :}
   name nameu GDX-PATH-BUF GT-PATH GDX-PATH-U ! ;

: GDX-PATH2! ( ptr u8 n -- ) {: name:ptr nameu:n :}
   name nameu GDX-PATH2-BUF GT-PATH GDX-PATH2-U ! ;

: GDX-PATH$ ( -- ptr u8 n )
   GDX-PATH-BUF GDX-PATH-U @ ;

: GDX-PATH2$ ( -- ptr u8 n )
   GDX-PATH2-BUF GDX-PATH2-U @ ;

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

: GDX-CHECK-DIRECT-START ( -- )
   GE-HB-RESET
   CHK-RESET-CFG
   CHK-CAPTURE-OFF
   LINT-OUT-BUFFER-OFF ;

: GDX-CHECK-DIRECT-OPT ( ptr u8 n -- ) {: a:ptr u:n :}
   a u GE-ARGV+
   a u CHK-PARSE-ONE ;

: GDX-CHECK-DIRECT-FILE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu GE-ARGV+
   path pathu CHK-ADD-POS ;

: GDX-CHECK-DIRECT-RUN-THROW ( -- )
   CHK-DIRECT-RUN throw ;

\ typed-local-lint: allow-bare-local - q keeps the quotation effect from the stack signature.
: GDX-CHECK-DIRECT-CAPTURE ( [ -- ] -- ) {: q :}
   q GE-CAPTURE-ACTION {: rc:n :}
   PROC-OUTCOME-EXIT GT-OUTCOME-KIND !
   rc GT-OUTCOME-CODE ! ;

: GDX-CHECK-DIRECT-STDIN-ACT ( -- )
   GE-SRC-BUF GE-SRC-U @ s" <stdin>" CHK-MATERIALIZE-BUF-AS
   GDX-CHECK-DIRECT-RUN-THROW ;

: GDX-CHECK-DIRECT-STDIN ( -- )
   [: GDX-CHECK-DIRECT-STDIN-ACT ;] GDX-CHECK-DIRECT-CAPTURE ;

: GDX-CHECK-DIRECT-PATH-ACT ( -- )
   CHK-MATERIALIZE
   GDX-CHECK-DIRECT-RUN-THROW ;

: GDX-CHECK-DIRECT-PATH ( -- )
   [: GDX-CHECK-DIRECT-PATH-ACT ;] GDX-CHECK-DIRECT-CAPTURE ;

: GDX-WRITE-ERR ( ptr u8 n -- )
   GDX-PATH!
   GDX-PATH$ GT-ERR$ WRITE-ALL ;

: GDX-WRITE-OUT ( ptr u8 n -- )
   GDX-PATH!
   GDX-PATH$ GT-OUT$ WRITE-ALL ;

: GDX-WRITE-SRC ( ptr u8 n -- )
   GDX-PATH!
   GDX-PATH$ GE-SRC-BUF GE-SRC-U @ WRITE-ALL ;

: GDX-GJA1-DISPATCH ( ptr u8 n ptr u8 n -- ) {: mode:ptr modeu:n file:ptr fileu:n :}
   file fileu GDX-PATH!
   mode modeu s" json-lines-schema" STR= if GDX-PATH$ GJA-JSON-LINES-SCHEMA exit then
   mode modeu s" json-one-schema" STR= if GDX-PATH$ GJA-JSON-ONE-SCHEMA exit then
   mode modeu s" all-errors" STR= if GDX-PATH$ GJA-ALL-ERRORS exit then
   mode modeu s" diag-contract" STR= if GDX-PATH$ GJA-DIAG-CONTRACT exit then
   mode modeu s" sarif" STR= if GDX-PATH$ GJA-SARIF exit then
   mode modeu s" public-signatures" STR= if GDX-PATH$ GJA-PUBLIC-SIGNATURES exit then
   GDX-USAGE ;

: GDX-GJA2S-DISPATCH ( ptr u8 n ptr u8 n ptr u8 n -- ) {: mode:ptr modeu:n file:ptr fileu:n arg:ptr argu:n :}
   file fileu GDX-PATH!
   mode modeu s" diag-repair-class" STR= if GDX-PATH$ arg argu GJA-DIAG-REPAIR-CLASS exit then
   GDX-USAGE ;

: GDX-GJA2P-DISPATCH ( ptr u8 n ptr u8 n ptr u8 n -- ) {: mode:ptr modeu:n file:ptr fileu:n arg:ptr argu:n :}
   file fileu GDX-PATH!
   arg argu GDX-PATH2!
   mode modeu s" diag-file-origin" STR= if GDX-PATH$ GDX-PATH2$ GJA-DIAG-FILE-ORIGIN exit then
   GDX-USAGE ;

: GDX-GJA-PROGRESS ( ptr u8 n -- )
   GT-PROGRESS-RUN ;

: GDX-GJA-PASS ( ptr u8 n -- )
   GT-PROGRESS-PASS ;

: GDX-GJA1 ( ptr u8 n ptr u8 n ptr u8 n -- ) {: mode:ptr modeu:n file:ptr fileu:n label:ptr labelu:n :}
   label labelu GDX-GJA-PROGRESS
   mode modeu file fileu GDX-GJA1-DISPATCH
   label labelu GDX-GJA-PASS ;

: GDX-GJA2S ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- ) {: mode:ptr modeu:n file:ptr fileu:n arg:ptr argu:n label:ptr labelu:n :}
   label labelu GDX-GJA-PROGRESS
   mode modeu file fileu arg argu GDX-GJA2S-DISPATCH
   label labelu GDX-GJA-PASS ;

: GDX-GJA2P ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- ) {: mode:ptr modeu:n file:ptr fileu:n arg:ptr argu:n label:ptr labelu:n :}
   label labelu GDX-GJA-PROGRESS
   mode modeu file fileu arg argu GDX-GJA2P-DISPATCH
   label labelu GDX-GJA-PASS ;

: GDX-DIAG-WORD-CLASS ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- ) {: file:ptr fileu:n word:ptr wordu:n class:ptr classu:n label:ptr labelu:n :}
   label labelu GDX-GJA-PROGRESS
   file fileu GDX-PATH!
   GDX-PATH$ word wordu class classu GJA-DIAG-WORD-REPAIR-CLASS
   label labelu GDX-GJA-PASS ;

: GDX-DIAG-CONTRACT ( ptr u8 n ptr u8 n -- ) {: file:ptr fileu:n label:ptr labelu:n :}
   s" diag-contract" file fileu label labelu GDX-GJA1 ;

: GDX-CHECK-JSON ( ptr u8 n -- ) {: label:ptr labelu:n :}
   GDX-CHECK-DIRECT-START
   s" --json-errors" GDX-CHECK-DIRECT-OPT
   GDX-CHECK-DIRECT-STDIN
   label labelu GE-EXPECT-NONZERO ;

: GDX-CHECK-JSON-OK ( ptr u8 n -- ) {: label:ptr labelu:n :}
   GDX-CHECK-DIRECT-START
   s" --json-errors" GDX-CHECK-DIRECT-OPT
   GDX-CHECK-DIRECT-STDIN
   label labelu GE-EXPECT-OK ;

: GDX-CHECK-JSON-ALL ( ptr u8 n -- ) {: label:ptr labelu:n :}
   GDX-CHECK-DIRECT-START
   s" --json-errors" GDX-CHECK-DIRECT-OPT
   s" --all-errors" GDX-CHECK-DIRECT-OPT
   GDX-CHECK-DIRECT-STDIN
   label labelu GE-EXPECT-NONZERO ;

: GDX-CHECK-FILE-JSON ( ptr u8 n ptr u8 n -- ) {: file:ptr fileu:n label:ptr labelu:n :}
   file fileu GDX-PATH!
   GDX-CHECK-DIRECT-START
   s" --json-errors" GDX-CHECK-DIRECT-OPT
   GDX-PATH$ GDX-CHECK-DIRECT-FILE
   GDX-CHECK-DIRECT-PATH
   label labelu GE-EXPECT-NONZERO ;

: GDX-CHECK-FILE-JSON-ALL ( ptr u8 n ptr u8 n -- ) {: file:ptr fileu:n label:ptr labelu:n :}
   file fileu GDX-PATH!
   GDX-CHECK-DIRECT-START
   s" --json-errors" GDX-CHECK-DIRECT-OPT
   s" --all-errors" GDX-CHECK-DIRECT-OPT
   GDX-PATH$ GDX-CHECK-DIRECT-FILE
   GDX-CHECK-DIRECT-PATH
   label labelu GE-EXPECT-NONZERO ;

: GDX-CHECK-FILE-STRICT-JSON ( ptr u8 n ptr u8 n -- ) {: file:ptr fileu:n label:ptr labelu:n :}
   file fileu GDX-PATH!
   GDX-CHECK-DIRECT-START
   s" --strict-signatures" GDX-CHECK-DIRECT-OPT
   s" --json-errors" GDX-CHECK-DIRECT-OPT
   GDX-PATH$ GDX-CHECK-DIRECT-FILE
   GDX-CHECK-DIRECT-PATH
   label labelu GE-EXPECT-NONZERO ;

: GDX-CHECK-STRICT ( ptr u8 n -- ) {: label:ptr labelu:n :}
   GDX-CHECK-DIRECT-START
   s" --strict-signatures" GDX-CHECK-DIRECT-OPT
   GDX-CHECK-DIRECT-STDIN
   label labelu GE-EXPECT-NONZERO ;

: GDX-CHECK-STRICT-JSON ( ptr u8 n -- ) {: label:ptr labelu:n :}
   GDX-CHECK-DIRECT-START
   s" --strict-signatures" GDX-CHECK-DIRECT-OPT
   s" --json-errors" GDX-CHECK-DIRECT-OPT
   GDX-CHECK-DIRECT-STDIN
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
   s" habu-json.err" s" primary diagnostic contract" GDX-DIAG-CONTRACT
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

: GDX-MALFORMED-QUOTATION-SIGNATURE ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : JQUOT ( [ n -- n -- ) drop ;" GE-SRC-LINE
   s" tools/check.f --json-errors accepted malformed quotation signature" GDX-CHECK-JSON
   s" code" s" E-BAD-SIGNATURE" s" malformed quotation signature code" GDX-EXPECT-ERR-JSTR
   s" token" s" --" s" malformed quotation token" GDX-EXPECT-ERR-JSTR
   s" repair_class" s" fix_signature_syntax" s" malformed quotation repair class" GDX-EXPECT-ERR-JSTR
   s" line" s" 1" s" malformed quotation line" GDX-EXPECT-ERR-JRAW
   s" byte_start" s" 19" s" malformed quotation byte_start" GDX-EXPECT-ERR-JRAW
   s" byte_end" s" 21" s" malformed quotation byte_end" GDX-EXPECT-ERR-JRAW ;

: GDX-BAD-PARAM-SIGNATURE ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : JPARAM ( span<space-global f32> -- ) drop ;" GE-SRC-LINE
   s" tools/check.f --json-errors accepted bad parametric signature" GDX-CHECK-JSON
   s" code" s" E-BAD-SIGNATURE" s" bad parametric signature code" GDX-EXPECT-ERR-JSTR
   s" token" s" f32" s" bad parametric token" GDX-EXPECT-ERR-JSTR
   s" repair_class" s" fix_signature_syntax" s" bad parametric repair class" GDX-EXPECT-ERR-JSTR
   s" suggestion" s" Repair the stack-effect comment syntax, including --." s" bad parametric suggestion" GDX-EXPECT-ERR-JSTR ;

: GDX-BAD-NOMINAL-DECL ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" deftype ptr" GE-SRC-LINE
   s" tools/check.f --json-errors accepted bad nominal declaration" GDX-CHECK-JSON
   s" code" s" E-BAD-NOMINAL-TYPE" s" bad nominal type code" GDX-EXPECT-ERR-JSTR
   s" token" s" ptr" s" bad nominal token" GDX-EXPECT-ERR-JSTR
   s" repair_class" s" fix_nominal_type" s" bad nominal repair class" GDX-EXPECT-ERR-JSTR
   s" suggestion" s" Choose a unique non-reserved nominal type name." s" bad nominal suggestion" GDX-EXPECT-ERR-JSTR
   s" line" s" 1" s" bad nominal line" GDX-EXPECT-ERR-JRAW
   s" column" s" 9" s" bad nominal column" GDX-EXPECT-ERR-JRAW
   s" byte_start" s" 8" s" bad nominal byte_start" GDX-EXPECT-ERR-JRAW
   s" byte_end" s" 11" s" bad nominal byte_end" GDX-EXPECT-ERR-JRAW ;

: GDX-SOURCE-LOCAL-NOMINAL ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" deftype widget" GE-SRC-LINE
   s" : JWIDGET ( widget -- ) drop ;" GE-SRC-LINE
   s" tools/check.f --json-errors rejected source-local nominal declaration" GDX-CHECK-JSON-OK ;

: GDX-REPAIR-CLASSES ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : JMISS ( i64 -- i64 ) drop ;" GE-SRC-LINE
   s" : JTYPE ( i64 -- i64 ) 0= ;" GE-SRC-LINE
   s" : JRET ( i64 -- ) >r ;" GE-SRC-LINE
   s" : JDEAD ( i64 -- i64 ) dup 0 < if 1 throw 0 then 1 + ;" GE-SRC-LINE
   s" : JDIE ( i64 -- i64 ) dup 0 < if here 0 1 die 0 then 1 + ;" GE-SRC-LINE
   s" tools/check.f --json-errors --all-errors accepted repair class batch" GDX-CHECK-JSON-ALL
   s" code" s" E-DEAD-CODE" s" dead-code diagnostic code" GDX-EXPECT-ERR-JSTR
   s" dead_owner" s" throw" s" dead-code owner" GDX-EXPECT-ERR-JSTR
   s" dead_owner" s" die" s" die dead-code owner" GDX-EXPECT-ERR-JSTR
   s" habu-json-repair.err" GDX-WRITE-ERR
   s" habu-json-repair.err" s" repair batch diagnostic contract" GDX-DIAG-CONTRACT
   s" habu-json-repair.err" s" jmiss" s" add_producer" s" missing producer class" GDX-DIAG-WORD-CLASS
   s" habu-json-repair.err" s" jtype" s" fix_type" s" type mismatch class" GDX-DIAG-WORD-CLASS
   s" habu-json-repair.err" s" jret" s" fix_return_stack" s" return stack class" GDX-DIAG-WORD-CLASS
   s" habu-json-repair.err" s" jdead" s" remove_dead_code" s" dead-code class" GDX-DIAG-WORD-CLASS
   s" habu-json-repair.err" s" jdie" s" remove_dead_code" s" die dead-code class" GDX-DIAG-WORD-CLASS ;

: GDX-FILE-ORIGIN ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" \ prelude" GE-SRC-LINE
   s" " GE-SRC-LINE
   s" : JBAD ( i64 -- i64 ) dup ;" GE-SRC-LINE
   s" habu-json-file.f" GDX-WRITE-SRC
   s" habu-json-file.f" s" tools/check.f --json-errors accepted file bad def" GDX-CHECK-FILE-JSON
   s" habu-json-file.err" GDX-WRITE-ERR
   s" habu-json-file.err" s" file-origin diagnostic contract" GDX-DIAG-CONTRACT
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
   s" unsafe compiler words verdicts" GE-EVAL-RUN-STDIN
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
   s" habu-unsafe.err" s" unsafe diagnostic contract" GDX-DIAG-CONTRACT
   s" diag-repair-class" s" habu-unsafe.err" s" trusted_boundary_required" s" unsafe repair class" GDX-GJA2S
   GE-HB-RESET
   GE-SRC-RESET
   s" : EV ( -- n ) evaluate ;" GE-SRC-LINE
   s" EV ." GE-SRC-LINE
   s" hb published unsafe evaluate definition" GE-HB-RUN-STDIN-NZ ;

: GDX-LOCAL-IN-LOOP ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : OKL ( -- ) 3 0 do i {: x:n :} x . loop ;" GE-SRC-LINE
   s" OKL" GE-SRC-LINE
   s" local in loop compiles" GE-EVAL-RUN-STDIN
   SB-RESET
   s" 0" GE-OUT-LINE  s" 1" GE-OUT-LINE  s" 2" GE-OUT-LINE
   SB$ s" local in loop output" GE-EXPECT-OUT
   GE-HB-RESET
   GE-SRC-RESET
   s" 0 set-check : BAD ( -- n ) [: 1 {: x:n :} x ;] execute ;" GE-SRC-LINE
   s" bin/hb" GE-SRC-BUF GE-SRC-U @ GE-TIMEOUT-MS GE-RUN-STDIN
   $4B s" B2 local-in-quote exits 75" GE-EXPECT-RC
   s" inside quotation" s" B2 local-in-quote diagnostic" GE-EXPECT-ERR-HAS
   GE-HB-RESET  GE-SRC-RESET
   s" : OKL ( n -- n ) {: a:n :} a ;" GE-SRC-LINE
   s" 5 OKL ." GE-SRC-LINE
   s" B2 word-top local still compiles" GE-EVAL-RUN-STDIN
   GE-HB-RESET
   GE-SRC-RESET
   s" GDX-SHOW ( i64 -- i64 ) {: x:? :} x" GE-SRC-CHECK-LINE
   s" show-inferred local prints type" GE-EVAL-RUN-STDIN
   s" inferred x: i64" s" show-inferred type output" GE-EXPECT-OUT-HAS
   s" -1" s" show-inferred verdict output" GE-EXPECT-OUT-HAS
   GE-HB-RESET
   GE-SRC-RESET
   s" GDX-SHOW-BAD ( i64 -- ) {: x:? :} x x" GE-SRC-CHECK-LINE
   s" show-inferred downstream mismatch rejects" GE-EVAL-RUN-STDIN
   s" inferred x: i64" s" show-inferred bad type output" GE-EXPECT-OUT-HAS
   s" 0" s" show-inferred bad verdict output" GE-EXPECT-OUT-HAS ;

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
   s" habu-all-errors.err" s" all-errors diagnostic contract" GDX-DIAG-CONTRACT
   s" all-errors" s" habu-all-errors.err" s" all-errors diagnostics" GDX-GJA1 ;

: GDX-UNDEFINED-RECURSIVE ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : UDEF ( i64 -- i64 ) dup NOPE ;" GE-SRC-LINE
   s" tools/check.f --all-errors accepted undefined word" GDX-CHECK-JSON-ALL
   s" habu-undef.err" GDX-WRITE-ERR
   s" code" s" E-UNDEFINED" s" undefined diagnostic code" GDX-EXPECT-ERR-JSTR
   s" token" s" NOPE" s" undefined diagnostic token" GDX-EXPECT-ERR-JSTR
   s" habu-undef.err" s" undefined diagnostic contract" GDX-DIAG-CONTRACT
   s" json-one-schema" s" habu-undef.err" s" undefined schema" GDX-GJA1
   s" diag-repair-class" s" habu-undef.err" s" unknown_rejection" s" undefined repair class" GDX-GJA2S
   GE-HB-RESET
   GE-SRC-RESET
   s" : POW ( i64 -- i64 ) dup POW ;" GE-SRC-LINE
   s" tools/check.f --all-errors accepted recursive self-call" GDX-CHECK-JSON-ALL
   s" habu-recursive.err" GDX-WRITE-ERR
   s" token" s" POW" s" recursive diagnostic token" GDX-EXPECT-ERR-JSTR
   s" habu-recursive.err" s" recursive diagnostic contract" GDX-DIAG-CONTRACT
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
   s" tools/public-signatures-core.f" GDX-ARG+
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
   s" tools/trust-lint.f" GDX-ARG+
   s" --" GDX-ARG+
   GDX-TRUST-ROOT$ GDX-ARG+
   s" 2026-10-01" GDX-ARG+
   s" bin/hb" GE-TIMEOUT-MS GE-RUN-ENV
   s" trust-lint accepted stale audit dates" GE-EXPECT-NONZERO
   s" STALE-AUDIT" s" trust-lint stale audit diagnostic" GE-EXPECT-OUT-HAS ;

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
   GDX-MALFORMED-QUOTATION-SIGNATURE
   GDX-BAD-PARAM-SIGNATURE
   GT-CLEANUP
   s" PASS: native checker diagnostics undef-primary slice" type cr ;

: GDX-ALL-STRICT-SLICE ( -- )
   s" hb-gate-diagnostics-all-strict" GT-START
   GDX-ALL-ERRORS
   GDX-SARIF
   GDX-STRICT-SIGNATURES
   GDX-BARE-PTR-SIGNATURE
   GDX-BAD-NOMINAL-DECL
   GDX-SOURCE-LOCAL-NOMINAL
   GDX-LOAD-FAIL-CLOSED
   GT-CLEANUP
   s" PASS: native checker diagnostics all-strict slice" type cr ;

: GDX-FILE-UNSAFE-SLICE ( -- )
   s" hb-gate-diagnostics-file-unsafe" GT-START
   GDX-FILE-ORIGIN
   GDX-UNSAFE-CHECKS
   GDX-LOCAL-IN-LOOP
   GT-CLEANUP
   s" PASS: native checker diagnostics file-unsafe slice" type cr ;

: GDX-SERIAL ( -- )
   s" hb-gate-diagnostics" GT-START
   GDX-PRIMARY-JSON
   GDX-UNKNOWN-SIGNATURE
   GDX-BARE-PTR-SIGNATURE
   GDX-MALFORMED-QUOTATION-SIGNATURE
   GDX-BAD-PARAM-SIGNATURE
   GDX-BAD-NOMINAL-DECL
   GDX-SOURCE-LOCAL-NOMINAL
   GDX-REPAIR-CLASSES
   GDX-FILE-ORIGIN
   GDX-STRICT-SIGNATURES
   GDX-UNSAFE-CHECKS
   GDX-LOCAL-IN-LOOP
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
   s" diag-repair" GDX-ARG0= if GDX-REPAIR-SLICE exit then
   s" diag-undef-primary" GDX-ARG0= if GDX-UNDEF-PRIMARY-SLICE exit then
   s" diag-all-strict" GDX-ARG0= if GDX-ALL-STRICT-SLICE exit then
   s" diag-file-unsafe" GDX-ARG0= if GDX-FILE-UNSAFE-SLICE exit then
   GDX-USAGE ;
