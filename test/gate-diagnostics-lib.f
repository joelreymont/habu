\ gate-diagnostics.f - checked runner for checker diagnostic contracts.
\
\ Load after test/gate-common.f, tools/json.f, and tools/gate-json-assert-core.f.

require test/golden.f

64 constant GDX-USAGE-RC

create GDX-PATH-BUF FS-PATH-CAP allot
create GDX-PATH2-BUF FS-PATH-CAP allot
create GDX-TRUST-ROOT-BUF FS-PATH-CAP allot
create GDX-TRUST-SRC-DIR-BUF FS-PATH-CAP allot
create GDX-TRUST-SRC-BUF FS-PATH-CAP allot
create GDX-TRUST-MAN-BUF FS-PATH-CAP allot
$40000 constant GDX-TL-STR-CAP
$30000 constant GDX-TL-FILE-CAP
create GDX-TL-STR-BUF GDX-TL-STR-CAP allot
create GDX-TL-FILE-BUF GDX-TL-FILE-CAP allot
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

\ Byte-exact golden assertion: compare captured diagnostic text against a
\ committed golden under test/golden/; GOLD:CHECK handles --update-golden and
\ prints the delta on mismatch. Diagnostics with volatile temp paths must set
\ GOLD:REDACT! to GT-ROOT before calling so paths normalize to <root>.
: GDX-EXPECT-GOLDEN ( ptr u8 n ptr u8 n ptr u8 n -- ) {: cap:ptr capu:n name:ptr nameu:n label:ptr labelu:n :}
   cap capu name nameu GOLD:CHECK 0= if label labelu GE-FAIL then ;

: GDX-EXPECT-ERR-GOLDEN ( ptr u8 n ptr u8 n -- ) {: name:ptr nameu:n label:ptr labelu:n :}
   GOLD:REDACT-CLEAR
   GT-ERR$ name nameu label labelu GDX-EXPECT-GOLDEN ;

\ File-origin diagnostics embed the temp source path; redact GT-ROOT so the
\ golden stays stable across runs.
: GDX-EXPECT-ERR-GOLDEN-R ( ptr u8 n ptr u8 n -- ) {: name:ptr nameu:n label:ptr labelu:n :}
   GT-ROOT GOLD:REDACT!
   GT-ERR$ name nameu label labelu GDX-EXPECT-GOLDEN ;

: GDX-EXPECT-OUT-GOLDEN-R ( ptr u8 n ptr u8 n -- ) {: name:ptr nameu:n label:ptr labelu:n :}
   GT-ROOT GOLD:REDACT!
   GT-OUT$ name nameu label labelu GDX-EXPECT-GOLDEN ;

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

: GDX-EXPECT-ERR-NO-JKEY ( ptr u8 n ptr u8 n -- ) {: key:ptr keyu:n label:ptr labelu:n :}
   SB-RESET
   key keyu GDX-JKEY
   GDX-J-COLON
   SB$ label labelu GE-EXPECT-ERR-LACKS ;

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
   q GE-CAPTURE-ACTION OUTCOME:EXITED GT-OUTCOME! ;

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
   s" diag-primary-json.err" s" primary json golden" GDX-EXPECT-ERR-GOLDEN
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
   s" diag-repair-classes.err" s" repair classes golden" GDX-EXPECT-ERR-GOLDEN
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

\ F3: patch32 is a TRUSTED-only capability prim (machine-code sink). A CHECKED
\ definition that calls it must be rejected with the named E-CAP-TRUSTED code and
\ routed to the trusted_boundary_required repair class; exact private or
\ test-local TRUSTED: builders are the audited certified path.
: GDX-CAP-TRUSTED ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : GDX-FORGE ( -- ) 0 0 patch32 ;" GE-SRC-LINE
   s" tools/check.f accepted checked patch32 (F3)" GDX-CHECK-JSON
   s" habu-cap-trusted.err" GDX-WRITE-ERR
   s" code" s" E-CAP-TRUSTED" s" capability checker E-CAP-TRUSTED" GDX-EXPECT-ERR-JSTR
   s" token" s" patch32" s" capability checker token" GDX-EXPECT-ERR-JSTR
   s" habu-cap-trusted.err" s" capability diagnostic contract" GDX-DIAG-CONTRACT
   s" diag-repair-class" s" habu-cap-trusted.err" s" trusted_boundary_required" s" capability repair class" GDX-GJA2S ;

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

\ A reject diagnostic whose expected/actual row is wider than RBUF-CAP (64) values
\ must fail closed: the renderer caps the row instead of overflowing RBUF into
\ adjacent DATA (which fed garbage type pointers to REND-TYPE -> SIGSEGV). 70
\ layout values in the actual row exceed the cap; the child must still exit with
\ the checker-reject rc and print the mismatch, never crash. (Red pre-fix: rc 134.)
: GDX-RENDER-CAP-FAIL-CLOSED ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" SUMTYPE zrc 0" GE-SRC-LINE
   s"   VARIANT keep n ;VARIANT" GE-SRC-LINE
   s" ;SUMTYPE" GE-SRC-LINE
   s" : ZRCK ( n -- zrc ) ZRC:KEEP ;" GE-SRC-LINE
   s" : RENDER-CAP-MISUSE ( -- n ) " GE-SRC+
   70 0 ?do s" 1 ZRCK " GE-SRC+ loop
   s" ;" GE-SRC-LINE
   s" render-cap.f" GDX-WRITE-SRC
   s" --load" GDX-ARG+
   s" render-cap.f" GDX-PATH-ARGV+
   s" bin/hb" GE-TIMEOUT-MS GE-RUN-ENV
   70 s" wide diagnostic row must fail closed with rc 70, not crash" GE-EXPECT-RC
   s" actual: " s" wide diagnostic row must still render the mismatch" GE-EXPECT-ERR-HAS ;

\ Item 13 repair-packet family field: a layout (ADT) type mismatch must carry the
\ involved type-family name as `"family"`; a pure-scalar mismatch must not.
: GDX-ADT-FAMILY ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" SUMTYPE zrc 0 VARIANT keep n ;VARIANT ;SUMTYPE" GE-SRC-LINE
   s" : ZBAD ( n -- zrc ) ;" GE-SRC-LINE
   s" tools/check.f accepted ADT layout mismatch" GDX-CHECK-JSON
   s" habu-adt-family.err" GDX-WRITE-ERR
   s" code" s" E-MISMATCH" s" ADT mismatch code" GDX-EXPECT-ERR-JSTR
   s" family" s" zrc" s" ADT mismatch carries family field" GDX-EXPECT-ERR-JSTR
   s" repair_class" s" fix_type" s" ADT mismatch repair class" GDX-EXPECT-ERR-JSTR
   s" habu-adt-family.err" s" ADT family diagnostic contract" GDX-DIAG-CONTRACT
   s" json-one-schema" s" habu-adt-family.err" s" ADT family schema" GDX-GJA1
   GE-HB-RESET
   GE-SRC-RESET
   s" SUMTYPE famaa 0 VARIANT lefta ;VARIANT ;SUMTYPE" GE-SRC-LINE
   s" SUMTYPE fambb 0 VARIANT sameb ;VARIANT ;SUMTYPE" GE-SRC-LINE
   s" SUMTYPE famcc 0 VARIANT rightc ;VARIANT ;SUMTYPE" GE-SRC-LINE
   s" : FAM-MIS ( -- famaa fambb ) construct famcc rightc construct fambb sameb ;" GE-SRC-LINE
   s" tools/check.f accepted adjacent ADT mismatch" GDX-CHECK-JSON
   s" family" s" famaa" s" family names the exact expected mismatch" GDX-EXPECT-ERR-JSTR
   GE-HB-RESET
   GE-SRC-RESET
   s" SUMTYPE fambb 0 VARIANT sameb ;VARIANT ;SUMTYPE" GE-SRC-LINE
   s" : SBAD ( -- i64 fambb ) 0 >COUNT construct fambb sameb ;" GE-SRC-LINE
   s" tools/check.f accepted scalar mismatch" GDX-CHECK-JSON
   s" habu-scalar-nofam.err" GDX-WRITE-ERR
   s" code" s" E-MISMATCH" s" scalar mismatch code" GDX-EXPECT-ERR-JSTR
   s" family" s" scalar mismatch omits family field" GDX-EXPECT-ERR-NO-JKEY
   s" json-one-schema" s" habu-scalar-nofam.err" s" scalar mismatch schema" GDX-GJA1 ;

\ Item 13 repair-packet variant/tag field: a `construct family variant` payload
\ mismatch must carry the arm's sum-variant name (`variant`) and declaration-order
\ `tag`, plus the family it belongs to; a pure-scalar mismatch carries none. The
\ two-variant sum built on the SECOND variant proves `tag` tracks the actual arm
\ (toss = 1), not a hard-coded 0.
: GDX-ADT-VARIANT ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" SUMTYPE zrc 0 VARIANT keep n ;VARIANT VARIANT toss n ;VARIANT ;SUMTYPE" GE-SRC-LINE
   s" : CVBAD ( ptr u8 -- zrc ) construct zrc toss ;" GE-SRC-LINE
   s" tools/check.f accepted ADT variant payload mismatch" GDX-CHECK-JSON
   s" habu-adt-variant.err" GDX-WRITE-ERR
   s" code" s" E-MISMATCH" s" ADT variant mismatch code" GDX-EXPECT-ERR-JSTR
   s" family" s" zrc" s" ADT variant mismatch carries family" GDX-EXPECT-ERR-JSTR
   s" variant" s" toss" s" ADT variant mismatch carries variant name" GDX-EXPECT-ERR-JSTR
   s" tag" s" 1" s" ADT variant mismatch carries declaration-order tag" GDX-EXPECT-ERR-JRAW
   s" habu-adt-variant.err" s" ADT variant diagnostic contract" GDX-DIAG-CONTRACT
   s" json-one-schema" s" habu-adt-variant.err" s" ADT variant schema" GDX-GJA1
   GE-HB-RESET
   GE-SRC-RESET
   s" : SBAD ( i64 -- i64 ) dup ;" GE-SRC-LINE
   s" tools/check.f accepted scalar mismatch (variant slice)" GDX-CHECK-JSON
   s" habu-scalar-novar.err" GDX-WRITE-ERR
   s" variant" s" scalar mismatch omits variant field" GDX-EXPECT-ERR-NO-JKEY
   s" tag" s" scalar mismatch omits tag field" GDX-EXPECT-ERR-NO-JKEY
   s" json-one-schema" s" habu-scalar-novar.err" s" scalar novar schema" GDX-GJA1 ;

\ Item 13 repair-packet payload_pos field: a construct payload mismatch names the
\ 0-based declaration-order slot that failed. Both slots of the two-payload
\ variant expect the SAME type (n), so a type-matched capture cannot tell them
\ apart: the two arms prove the position is tracked structurally through the
\ unification spine (top slot 1, deep slot 0). A family-only layout mismatch
\ (no construct in scope) carries no payload_pos.
: GDX-ADT-PAYLOAD-POS ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" SUMTYPE zpp 0 VARIANT mix n n ;VARIANT ;SUMTYPE" GE-SRC-LINE
   s" : PBAD1 ( n ptr u8 -- zpp ) construct zpp mix ;" GE-SRC-LINE
   s" tools/check.f accepted slot-1 payload mismatch" GDX-CHECK-JSON
   s" habu-adt-pos1.err" GDX-WRITE-ERR
   s" code" s" E-MISMATCH" s" slot-1 payload mismatch code" GDX-EXPECT-ERR-JSTR
   s" variant" s" mix" s" slot-1 payload mismatch carries variant" GDX-EXPECT-ERR-JSTR
   s" payload_pos" s" 1" s" slot-1 payload mismatch names slot 1" GDX-EXPECT-ERR-JRAW
   s" habu-adt-pos1.err" s" payload_pos slot-1 diagnostic contract" GDX-DIAG-CONTRACT
   s" json-one-schema" s" habu-adt-pos1.err" s" payload_pos slot-1 schema" GDX-GJA1
   GE-HB-RESET
   GE-SRC-RESET
   s" SUMTYPE zpp 0 VARIANT mix n n ;VARIANT ;SUMTYPE" GE-SRC-LINE
   s" : PBAD0 ( ptr u8 n -- zpp ) construct zpp mix ;" GE-SRC-LINE
   s" tools/check.f accepted slot-0 payload mismatch" GDX-CHECK-JSON
   s" habu-adt-pos0.err" GDX-WRITE-ERR
   s" payload_pos" s" 0" s" slot-0 payload mismatch names slot 0" GDX-EXPECT-ERR-JRAW
   s" json-one-schema" s" habu-adt-pos0.err" s" payload_pos slot-0 schema" GDX-GJA1
   GE-HB-RESET
   GE-SRC-RESET
   s" SUMTYPE zpf 0 VARIANT keep n ;VARIANT ;SUMTYPE" GE-SRC-LINE
   s" : ZPBAD ( n -- zpf ) ;" GE-SRC-LINE
   s" tools/check.f accepted family-only mismatch (pos slice)" GDX-CHECK-JSON
   s" habu-adt-nopos.err" GDX-WRITE-ERR
   s" family" s" zpf" s" family-only mismatch still carries family" GDX-EXPECT-ERR-JSTR
   s" payload_pos" s" family-only mismatch omits payload_pos" GDX-EXPECT-ERR-NO-JKEY
   s" json-one-schema" s" habu-adt-nopos.err" s" family-only nopos schema" GDX-GJA1 ;

\ Item 13 repair-packet arity fields: a family applied with the wrong number of
\ signature arguments (E-WRONG-ARITY / fix_signature_arity) carries the family's
\ declared arity (`arity_expected`) and the written count (`arity_actual`). The
\ arity-2 arm proves the counts track the declaration, not a hard-coded pair; a
\ scalar mismatch carries neither.
: GDX-SIG-ARITY ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" SUMTYPE zar 0 VARIANT one ;VARIANT ;SUMTYPE" GE-SRC-LINE
   s" : ABAD ( zar<n> -- ) drop ;" GE-SRC-LINE
   s" tools/check.f accepted over-applied family signature" GDX-CHECK-JSON
   s" habu-sig-arity.err" GDX-WRITE-ERR
   s" code" s" E-WRONG-ARITY" s" wrong-arity code" GDX-EXPECT-ERR-JSTR
   s" repair_class" s" fix_signature_arity" s" wrong-arity repair class" GDX-EXPECT-ERR-JSTR
   s" arity_expected" s" 0" s" wrong-arity declared count" GDX-EXPECT-ERR-JRAW
   s" arity_actual" s" 1" s" wrong-arity written count" GDX-EXPECT-ERR-JRAW
   s" habu-sig-arity.err" s" wrong-arity diagnostic contract" GDX-DIAG-CONTRACT
   s" json-one-schema" s" habu-sig-arity.err" s" wrong-arity schema" GDX-GJA1
   GE-HB-RESET
   GE-SRC-RESET
   s" SUMTYPE zar2 2 VARIANT two a ;VARIANT ;SUMTYPE" GE-SRC-LINE
   s" : A2BAD ( zar2<n> -- ) drop ;" GE-SRC-LINE
   s" tools/check.f accepted under-applied family signature" GDX-CHECK-JSON
   s" habu-sig-arity2.err" GDX-WRITE-ERR
   s" arity_expected" s" 2" s" under-applied declared count" GDX-EXPECT-ERR-JRAW
   s" arity_actual" s" 1" s" under-applied written count" GDX-EXPECT-ERR-JRAW
   s" json-one-schema" s" habu-sig-arity2.err" s" under-applied schema" GDX-GJA1
   GE-HB-RESET
   GE-SRC-RESET
   s" : SBAD ( i64 -- i64 ) dup ;" GE-SRC-LINE
   s" tools/check.f accepted scalar mismatch (arity slice)" GDX-CHECK-JSON
   s" habu-scalar-noarity.err" GDX-WRITE-ERR
   s" arity_expected" s" scalar mismatch omits arity_expected" GDX-EXPECT-ERR-NO-JKEY
   s" arity_actual" s" scalar mismatch omits arity_actual" GDX-EXPECT-ERR-NO-JKEY
   s" json-one-schema" s" habu-scalar-noarity.err" s" scalar noarity schema" GDX-GJA1 ;

: GDX-TFAM-DECL ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" SUMTYPE badsum 0 VARIANT samev n ;VARIANT VARIANT samev n ;VARIANT ;SUMTYPE" GE-SRC-LINE
   s" tools/check.f accepted duplicate type-family variant" GDX-CHECK-JSON
   s" habu-tfam-decl.err" GDX-WRITE-ERR
   s" code" s" E-BAD-DECLARATION" s" declaration diagnostic code" GDX-EXPECT-ERR-JSTR
   s" repair_class" s" fix_family_declaration" s" declaration repair class" GDX-EXPECT-ERR-JSTR
   s" habu-tfam-decl.err" s" declaration diagnostic contract" GDX-DIAG-CONTRACT
   s" json-one-schema" s" habu-tfam-decl.err" s" declaration diagnostic schema" GDX-GJA1 ;

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
   s" diag-all-errors.err" s" all-errors golden" GDX-EXPECT-ERR-GOLDEN-R
   s" habu-all-errors.err" s" all-errors diagnostic contract" GDX-DIAG-CONTRACT
   s" all-errors" s" habu-all-errors.err" s" all-errors diagnostics" GDX-GJA1 ;

: GDX-UNDEFINED-RECURSIVE ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : UDEF ( i64 -- i64 ) dup NOPE ;" GE-SRC-LINE
   s" tools/check.f --all-errors accepted undefined word" GDX-CHECK-JSON-ALL
   s" habu-undef.err" GDX-WRITE-ERR
   s" diag-undefined.err" s" undefined golden" GDX-EXPECT-ERR-GOLDEN
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

: GDX-PUBLIC-SIGNATURES ( -- )
   GT-OUT-BUF GT-OUT-CAP PS-OUT-BUFFER!
   GT-ERR-BUF GT-ERR-CAP PS-ERR-BUFFER!
   0 PS-TRUST !
   PS-JSON-DOC-START
   s" examples/llm/good.f" PS-SCAN-FILE
   PS-JSON-DOC-END
   PS-ERR$ nip 0 <> if s" public-signatures stderr" GE-FAIL then
   s" public-signatures.json" GDX-PATH!
   GDX-PATH$ PS-OUT$ WRITE-ALL
   PS-BUFFERS-OFF
   s" public-signatures" s" public-signatures.json" s" public signatures output" GDX-GJA1 ;

: GDX-TRUST-LINT-TODAY ( -- )
   s" 2026-10-01" PARSE-YMD MATCH option
     none OF s" trust-lint fixture today" GE-FAIL ENDOF
     some OF ENDOF
   ;MATCH
   TRUST-LINT-TODAY! ;

: GDX-TRUST-LINT-STALE-ACT ( -- )
   GDX-TL-STR-BUF GDX-TL-STR-CAP
   GDX-TL-FILE-BUF GDX-TL-FILE-CAP TRUST-LINT-BUFFERS!
   TL-TRUE TL-REPORT-SUCCESS!
   GDX-TRUST-ROOT$ TRUST-LINT-ROOT!
   GDX-TRUST-LINT-TODAY
   TRUST-LINT ;

: GDX-TRUST-LINT-STALE ( -- )
   GDX-TRUST-FIXTURE
   [: GDX-TRUST-LINT-STALE-ACT ;] GE-CAPTURE-ACTION GE-EVAL-STORE-RC
   TL-BAD @ 0= if s" trust-lint accepted stale audit dates" GE-FAIL then
   s" STALE-AUDIT" s" trust-lint stale audit diagnostic" GE-EXPECT-ERR-HAS ;

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
   GDX-RENDER-CAP-FAIL-CLOSED
   GDX-ADT-FAMILY
   GDX-ADT-VARIANT
   GDX-ADT-PAYLOAD-POS
   GDX-SIG-ARITY
   GDX-TFAM-DECL
   GT-CLEANUP
   s" PASS: native checker diagnostics undef-primary slice" type cr ;

: GDX-FILE-UNSAFE-SLICE ( -- )
   s" hb-gate-diagnostics-file-unsafe" GT-START
   GDX-FILE-ORIGIN
   GDX-UNSAFE-CHECKS
   GDX-CAP-TRUSTED
   GDX-LOCAL-IN-LOOP
   GT-CLEANUP
   s" PASS: native checker diagnostics file-unsafe slice" type cr ;
