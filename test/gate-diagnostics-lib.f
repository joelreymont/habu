\ gate-diagnostics.f - checked runner for checker diagnostic contracts.
\
\ Load after test/gate-common.f, tools/json.f, and tools/gate-json-assert-core.f.

require test/golden.f

package GATE-DIAGNOSTICS

64 constant USAGE-RC

create PATH-BUF FS-PATH-CAP allot
create PATH2-BUF FS-PATH-CAP allot
create LABEL-BUF FS-PATH-CAP allot
variable PATH-U
variable PATH2-U
variable LABEL-U

: USAGE ( -- )
   s" usage: test/gate-diagnostics.f [diag-repair|diag-undef-primary|diag-all-strict|diag-file-unsafe|diag-label-copy]" USAGE-RC die ;

: ARG0= ( ptr u8 n -- bool )
   0 SCRIPT-ARGV$ STR= ;

: J-DQ ( -- )
   GE-DQ SB-APPEND-C ;

: J-COLON ( -- )
   s" :" SB-APPEND ;

: JKEY ( ptr u8 n -- )
   J-DQ
   SB-APPEND
   J-DQ ;

\ Byte-exact golden assertion: compare captured diagnostic text against a
\ committed golden under test/golden/; GOLD:CHECK handles --update-golden and
\ prints the delta on mismatch. Diagnostics with volatile temp paths must set
\ GOLD:REDACT! to GT-ROOT before calling so paths normalize to <root>.
: EXPECT-GOLDEN ( ptr u8 n ptr u8 n ptr u8 n -- ) {: cap:ptr capu:n name:ptr nameu:n label:ptr labelu:n :}
   cap capu name nameu GOLD:CHECK 0= if label labelu GE-FAIL then ;

: ERR-GOLDEN ( ptr u8 n ptr u8 n -- ) {: name:ptr nameu:n label:ptr labelu:n :}
   GOLD:REDACT-CLEAR
   GT-ERR$ name nameu label labelu EXPECT-GOLDEN ;

\ File-origin diagnostics embed the temp source path; redact GT-ROOT so the
\ golden stays stable across runs.
: ERR-GOLDEN-R ( ptr u8 n ptr u8 n -- ) {: name:ptr nameu:n label:ptr labelu:n :}
   GT-ROOT GOLD:REDACT!
   GT-ERR$ name nameu label labelu EXPECT-GOLDEN ;

: OUT-GOLDEN-R ( ptr u8 n ptr u8 n -- ) {: name:ptr nameu:n label:ptr labelu:n :}
   GT-ROOT GOLD:REDACT!
   GT-OUT$ name nameu label labelu EXPECT-GOLDEN ;

: ERR-JKEY ( ptr u8 n ptr u8 n -- ) {: key:ptr keyu:n label:ptr labelu:n :}
   SB-RESET
   key keyu JKEY
   J-COLON
   SB$ label labelu GE-EXPECT-ERR-HAS ;

: ERR-JRAW ( ptr u8 n ptr u8 n ptr u8 n -- ) {: key:ptr keyu:n raw:ptr rawu:n label:ptr labelu:n :}
   SB-RESET
   key keyu JKEY
   J-COLON
   raw rawu SB-APPEND
   SB$ label labelu GE-EXPECT-ERR-HAS ;

: ERR-JSTR ( ptr u8 n ptr u8 n ptr u8 n -- ) {: key:ptr keyu:n val:ptr valu:n label:ptr labelu:n :}
   SB-RESET
   key keyu JKEY
   J-COLON
   J-DQ
   val valu SB-APPEND
   SB$ label labelu GE-EXPECT-ERR-HAS ;

: ERR-NO-JKEY ( ptr u8 n ptr u8 n -- ) {: key:ptr keyu:n label:ptr labelu:n :}
   SB-RESET
   key keyu JKEY
   J-COLON
   SB$ label labelu GE-EXPECT-ERR-LACKS ;

: PATH! ( ptr u8 n -- ) {: name:ptr nameu:n :}
   name nameu PATH-BUF GT-PATH PATH-U ! ;

: PATH2! ( ptr u8 n -- ) {: name:ptr nameu:n :}
   name nameu PATH2-BUF GT-PATH PATH2-U ! ;

: PATH$ ( -- ptr u8 n )
   PATH-BUF PATH-U @ ;

: PATH2$ ( -- ptr u8 n )
   PATH2-BUF PATH2-U @ ;

: LABEL$ ( -- ptr u8 n )
   LABEL-BUF LABEL-U @ ;

: LABEL! ( ptr u8 n -- )
   LABEL-BUF LABEL-U GE-COPY! ;

: ARG+ ( ptr u8 n -- )
   GE-ARG+ ;

: PATH-ARGV+ ( ptr u8 n -- )
   PATH!
   PATH$ ARG+ ;

: CHECK-START ( -- )
   GE-HB-RESET
   CHECK:RESET
   LINT-OUT-BUFFER-OFF ;

: CHECK-OPT ( ptr u8 n -- ) {: a:ptr u:n :}
   a u GE-ARGV+
   a u CHECK:OPT ;

: CHECK-FILE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu GE-ARGV+
   path pathu CHECK:FILE ;

: CHECK-RUN ( -- )
   CHECK:RUN throw ;

\ typed-local-lint: allow-bare-local - q preserves the quotation effect.
: CHECK-CAPTURE ( [ -- ] -- ) {: q :}
   q GE-CAPTURE-ACTION OUTCOME:EXITED GT-OUTCOME! ;

: STDIN-ACT ( -- )
   GE-SRC-BUF GE-SRC-U @ s" <stdin>" CHECK:SOURCE
   CHECK-RUN ;

: CHECK-STDIN ( -- )
   [: STDIN-ACT ;] CHECK-CAPTURE ;

: PATH-ACT ( -- )
   CHECK-RUN ;

: CHECK-PATH ( -- )
   [: PATH-ACT ;] CHECK-CAPTURE ;

: LABEL-COPY-ACT ( -- )
   GE-SRC-BUF GE-SRC-U @ LABEL$ CHECK:SOURCE
   $58 LABEL-BUF c!
   CHECK-RUN ;

: LABEL-COPY ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : LABEL-BAD ( i64 -- i64 ) dup ;" GE-SRC-LINE
   CHECK-START
   s" owned-label.f" LABEL!
   [: LABEL-COPY-ACT ;] CHECK-CAPTURE
   s" public CHECK:SOURCE accepted bad label-copy fixture" GE-EXPECT-NONZERO
   s" owned-label.f" s" copied source label appears in diagnostic" GE-EXPECT-ERR-HAS
   s" Xwned-label.f" s" mutated source label absent from diagnostic" GE-EXPECT-ERR-LACKS
   CHECK:RESET ;

: WRITE-ERR ( ptr u8 n -- )
   PATH!
   PATH$ GT-ERR$ WRITE-ALL ;

: WRITE-OUT ( ptr u8 n -- )
   PATH!
   PATH$ GT-OUT$ WRITE-ALL ;

: WRITE-SRC ( ptr u8 n -- )
   PATH!
   PATH$ GE-SRC-BUF GE-SRC-U @ WRITE-ALL ;

: GJA1-RUN ( ptr u8 n ptr u8 n -- ) {: mode:ptr modeu:n file:ptr fileu:n :}
   file fileu PATH!
   mode modeu s" json-lines-schema" STR= if PATH$ GJA-JSON-LINES-SCHEMA exit then
   mode modeu s" json-one-schema" STR= if PATH$ GJA-JSON-ONE-SCHEMA exit then
   mode modeu s" all-errors" STR= if PATH$ GJA-ALL-ERRORS exit then
   mode modeu s" diag-contract" STR= if PATH$ GJA-DIAG-CONTRACT exit then
   mode modeu s" sarif" STR= if PATH$ GJA-SARIF exit then
   mode modeu s" public-signatures" STR= if PATH$ GJA-PUBLIC-SIGNATURES exit then
   USAGE ;

: GJA2S-RUN ( ptr u8 n ptr u8 n ptr u8 n -- ) {: mode:ptr modeu:n file:ptr fileu:n arg:ptr argu:n :}
   file fileu PATH!
   mode modeu s" diag-repair-class" STR= if PATH$ arg argu GJA-DIAG-REPAIR-CLASS exit then
   USAGE ;

: GJA2P-RUN ( ptr u8 n ptr u8 n ptr u8 n -- ) {: mode:ptr modeu:n file:ptr fileu:n arg:ptr argu:n :}
   file fileu PATH!
   arg argu PATH2!
   mode modeu s" diag-file-origin" STR= if PATH$ PATH2$ GJA-DIAG-FILE-ORIGIN exit then
   USAGE ;

: GJA-PROGRESS ( ptr u8 n -- )
   GT-PROGRESS-RUN ;

: GJA-PASS ( ptr u8 n -- )
   GT-PROGRESS-PASS ;

: GJA1 ( ptr u8 n ptr u8 n ptr u8 n -- ) {: mode:ptr modeu:n file:ptr fileu:n label:ptr labelu:n :}
   label labelu GJA-PROGRESS
   mode modeu file fileu GJA1-RUN
   label labelu GJA-PASS ;

: GJA2S ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- ) {: mode:ptr modeu:n file:ptr fileu:n arg:ptr argu:n label:ptr labelu:n :}
   label labelu GJA-PROGRESS
   mode modeu file fileu arg argu GJA2S-RUN
   label labelu GJA-PASS ;

: GJA2P ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- ) {: mode:ptr modeu:n file:ptr fileu:n arg:ptr argu:n label:ptr labelu:n :}
   label labelu GJA-PROGRESS
   mode modeu file fileu arg argu GJA2P-RUN
   label labelu GJA-PASS ;

: WORD-CLASS ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- ) {: file:ptr fileu:n word:ptr wordu:n class:ptr classu:n label:ptr labelu:n :}
   label labelu GJA-PROGRESS
   file fileu PATH!
   PATH$ word wordu class classu GJA-DIAG-WORD-REPAIR-CLASS
   label labelu GJA-PASS ;

: DIAG-CONTRACT ( ptr u8 n ptr u8 n -- ) {: file:ptr fileu:n label:ptr labelu:n :}
   s" diag-contract" file fileu label labelu GJA1 ;

: CHECK-JSON ( ptr u8 n -- ) {: label:ptr labelu:n :}
   CHECK-START
   s" json-errors" CHECK-OPT
   CHECK-STDIN
   label labelu GE-EXPECT-NONZERO ;

: CHECK-JSON-OK ( ptr u8 n -- ) {: label:ptr labelu:n :}
   CHECK-START
   s" json-errors" CHECK-OPT
   CHECK-STDIN
   label labelu GE-EXPECT-OK ;

: CHECK-JSON-ALL ( ptr u8 n -- ) {: label:ptr labelu:n :}
   CHECK-START
   s" json-errors" CHECK-OPT
   s" all-errors" CHECK-OPT
   CHECK-STDIN
   label labelu GE-EXPECT-NONZERO ;

: FILE-JSON ( ptr u8 n ptr u8 n -- ) {: file:ptr fileu:n label:ptr labelu:n :}
   file fileu PATH!
   CHECK-START
   s" json-errors" CHECK-OPT
   PATH$ CHECK-FILE
   CHECK-PATH
   label labelu GE-EXPECT-NONZERO ;

: FILE-JSON-ALL ( ptr u8 n ptr u8 n -- ) {: file:ptr fileu:n label:ptr labelu:n :}
   file fileu PATH!
   CHECK-START
   s" json-errors" CHECK-OPT
   s" all-errors" CHECK-OPT
   PATH$ CHECK-FILE
   CHECK-PATH
   label labelu GE-EXPECT-NONZERO ;

: FILE-STRICT-JSON ( ptr u8 n ptr u8 n -- ) {: file:ptr fileu:n label:ptr labelu:n :}
   file fileu PATH!
   CHECK-START
   s" strict-signatures" CHECK-OPT
   s" json-errors" CHECK-OPT
   PATH$ CHECK-FILE
   CHECK-PATH
   label labelu GE-EXPECT-NONZERO ;

: CHECK-STRICT ( ptr u8 n -- ) {: label:ptr labelu:n :}
   CHECK-START
   s" strict-signatures" CHECK-OPT
   CHECK-STDIN
   label labelu GE-EXPECT-NONZERO ;

: CHECK-STRICT-JSON ( ptr u8 n -- ) {: label:ptr labelu:n :}
   CHECK-START
   s" strict-signatures" CHECK-OPT
   s" json-errors" CHECK-OPT
   CHECK-STDIN
   label labelu GE-EXPECT-NONZERO ;

: CHECK-JSON-FIELDS ( -- )
   s" verdict" s" rejected" s" --json-errors verdict" ERR-JSTR
   s" declared_effect" s" i64 -- i64 " s" --json-errors declared effect" ERR-JSTR
   s" inferred_effect" s" i64 -- i64 i64 " s" --json-errors inferred effect" ERR-JSTR
   s" token_index" s" 1" s" --json-errors token index" ERR-JRAW
   s" file" s" <stdin>" s" --json-errors file" ERR-JSTR
   s" line" s" 1" s" --json-errors line" ERR-JRAW
   s" column" s" --json-errors column" ERR-JKEY
   s" byte_start" s" --json-errors byte_start" ERR-JKEY
   s" byte_end" s" --json-errors byte_end" ERR-JKEY
   s" definition_source" s" --json-errors definition source" ERR-JKEY
   s" json-lines-schema" s" habu-json.err" s" json lines schema" GJA1
   s" habu-json.err" s" primary diagnostic contract" DIAG-CONTRACT
   s" diag-repair-class" s" habu-json.err" s" remove_producer" s" remove producer class" GJA2S ;

: PRIMARY-JSON ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : JBAD ( i64 -- i64 ) dup ;" GE-SRC-LINE
   s" tools/check.f --json-errors accepted bad def" CHECK-JSON
   s" habu-json.err" WRITE-ERR
   s" diag-primary-json.err" s" primary json golden" ERR-GOLDEN
   CHECK-JSON-FIELDS ;

: UNKNOWN-SIGNATURE ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : JSIG ( got expected -- bool ) <= ;" GE-SRC-LINE
   s" tools/check.f --json-errors accepted unknown signature type" CHECK-JSON
   s" code" s" E-UNKNOWN-SIGNATURE-TYPE" s" unknown signature type code" ERR-JSTR
   s" token" s" got" s" unknown signature type token" ERR-JSTR
   s" repair_class" s" fix_signature_type" s" unknown signature repair class" ERR-JSTR
   s" suggestion" s" Use a known stack-signature type or a single-letter type variable." s" unknown signature suggestion" ERR-JSTR ;

: BARE-PTR-SIGNATURE ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : JBPTR ( ptr -- ) drop ;" GE-SRC-LINE
   s" tools/check.f --json-errors accepted bare ptr signature" CHECK-JSON
   s" code" s" E-BARE-PTR-SIGNATURE" s" bare ptr signature code" ERR-JSTR
   s" repair_class" s" fix_bare_ptr_element" s" bare ptr repair class" ERR-JSTR
   s" suggestion" s" Give 'ptr' an element type, e.g. 'ptr u8' or 'ptr a'." s" bare ptr suggestion" ERR-JSTR ;

: MALFORMED-QUOTATION-SIGNATURE ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : JQUOT ( [ n -- n -- ) drop ;" GE-SRC-LINE
   s" tools/check.f --json-errors accepted malformed quotation signature" CHECK-JSON
   s" code" s" E-BAD-SIGNATURE" s" malformed quotation signature code" ERR-JSTR
   s" token" s" --" s" malformed quotation token" ERR-JSTR
   s" repair_class" s" fix_signature_syntax" s" malformed quotation repair class" ERR-JSTR
   s" line" s" 1" s" malformed quotation line" ERR-JRAW
   s" byte_start" s" 19" s" malformed quotation byte_start" ERR-JRAW
   s" byte_end" s" 21" s" malformed quotation byte_end" ERR-JRAW ;

: BAD-PARAM-SIGNATURE ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : JPARAM ( span<space-global f32> -- ) drop ;" GE-SRC-LINE
   s" tools/check.f --json-errors accepted bad parametric signature" CHECK-JSON
   s" code" s" E-BAD-SIGNATURE" s" bad parametric signature code" ERR-JSTR
   s" token" s" f32" s" bad parametric token" ERR-JSTR
   s" repair_class" s" fix_signature_syntax" s" bad parametric repair class" ERR-JSTR
   s" suggestion" s" Repair the stack-effect comment syntax, including --." s" bad parametric suggestion" ERR-JSTR ;

: BAD-NOMINAL-DECL ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" DEFTYPE PTR" GE-SRC-LINE
   s" tools/check.f --json-errors accepted bad nominal declaration" CHECK-JSON
   s" code" s" E-BAD-NOMINAL-TYPE" s" bad nominal type code" ERR-JSTR
   s" token" s" PTR" s" bad nominal token" ERR-JSTR
   s" repair_class" s" fix_nominal_type" s" bad nominal repair class" ERR-JSTR
   s" suggestion" s" Choose a unique non-reserved nominal type name." s" bad nominal suggestion" ERR-JSTR
   s" line" s" 1" s" bad nominal line" ERR-JRAW
   s" column" s" 9" s" bad nominal column" ERR-JRAW
   s" byte_start" s" 8" s" bad nominal byte_start" ERR-JRAW
   s" byte_end" s" 11" s" bad nominal byte_end" ERR-JRAW ;

\ The check CLI accepts a source that declares a nominal locally and uses it in
\ a signature. The declarer is DEFLINEAR, whose interpret word survives and runs
\ in the child engine; a DEFLINEAR value moves exactly once, so JWIDGET passes it
\ through by identity rather than dropping it.
: SOURCE-LOCAL-NOMINAL ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" DEFLINEAR widget" GE-SRC-LINE
   s" : JWIDGET ( widget -- widget ) ;" GE-SRC-LINE
   s" tools/check.f --json-errors rejected source-local nominal declaration" CHECK-JSON-OK ;

: REPAIR-CLASSES ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : JMISS ( i64 -- i64 ) drop ;" GE-SRC-LINE
   s" : JTYPE ( i64 -- i64 ) 0= ;" GE-SRC-LINE
   s" : JRET ( i64 -- ) >r ;" GE-SRC-LINE
   s" : JDEAD ( i64 -- i64 ) dup 0 < if 1 throw 0 then 1 + ;" GE-SRC-LINE
   s" : JDIE ( i64 -- i64 ) dup 0 < if here 0 1 die 0 then 1 + ;" GE-SRC-LINE
   s" variable JBP" GE-SRC-LINE
   s" : JBPBASE ( -- ptr u8 ) JBP @ ;" GE-SRC-LINE
   s" : JBPS ( -- ) 0 JBPBASE ! ;" GE-SRC-LINE
   s" tools/check.f --json-errors --all-errors accepted repair class batch" CHECK-JSON-ALL
   s" code" s" E-DEAD-CODE" s" dead-code diagnostic code" ERR-JSTR
   s" dead_owner" s" throw" s" dead-code owner" ERR-JSTR
   s" dead_owner" s" die" s" die dead-code owner" ERR-JSTR
   s" habu-json-repair.err" WRITE-ERR
   s" diag-repair-classes.err" s" repair classes golden" ERR-GOLDEN
   s" habu-json-repair.err" s" repair batch diagnostic contract" DIAG-CONTRACT
   s" habu-json-repair.err" s" jmiss" s" add_producer" s" missing producer class" WORD-CLASS
   s" habu-json-repair.err" s" jtype" s" fix_type" s" type mismatch class" WORD-CLASS
   s" habu-json-repair.err" s" jret" s" fix_return_stack" s" return stack class" WORD-CLASS
   s" habu-json-repair.err" s" jdead" s" remove_dead_code" s" dead-code class" WORD-CLASS
   s" habu-json-repair.err" s" jdie" s" remove_dead_code" s" die dead-code class" WORD-CLASS
   s" habu-json-repair.err" s" jbps" s" fix_type" s" byte-ptr store-target class" WORD-CLASS ;

: FILE-ORIGIN ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" \ prelude" GE-SRC-LINE
   s" " GE-SRC-LINE
   s" : JBAD ( i64 -- i64 ) dup ;" GE-SRC-LINE
   s" habu-json-file.f" WRITE-SRC
   s" habu-json-file.f" s" tools/check.f --json-errors accepted file bad def" FILE-JSON
   s" habu-json-file.err" WRITE-ERR
   s" habu-json-file.err" s" file-origin diagnostic contract" DIAG-CONTRACT
   s" diag-file-origin" s" habu-json-file.err" s" habu-json-file.f" s" file origin" GJA2P ;

: STRICT-SIGNATURES ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : GDX-NOSIG dup ;" GE-SRC-LINE
   s" tools/check.f --strict-signatures accepted nosig" CHECK-STRICT
   s" E-MISSING-SIGNATURE" s" strict-signatures text diagnostic" GE-EXPECT-ERR-HAS
   GE-HB-RESET
   GE-SRC-RESET
   s" : GDX-NOSIG dup ;" GE-SRC-LINE
   s" tools/check.f --strict-signatures --json-errors accepted nosig" CHECK-STRICT-JSON
   s" code" s" E-MISSING-SIGNATURE" s" strict-signatures JSON diagnostic" ERR-JSTR
   GE-HB-RESET
   GE-SRC-RESET
   s" : X ( infer ) dup ;" GE-SRC-LINE
   s" tools/check.f --strict-signatures accepted infer opt-out" CHECK-STRICT-JSON
   s" code" s" E-UNVERIFIED-SIGNATURE" s" strict-signatures opt-out diagnostic" ERR-JSTR ;

: UNSAFE-CHECK-SOURCE ( -- )
   GE-SRC-RESET
   s" EV ( -- n ) evaluate" GE-SRC-CHECK-LINE
   s" PO ( -- ) postpone dup" GE-SRC-CHECK-LINE
   s" CO ( -- ) compile," GE-SRC-CHECK-LINE
   s" IM ( -- ) immediate" GE-SRC-CHECK-LINE
   s" LB ( -- ) [" GE-SRC-CHECK-LINE
   s" RB ( -- ) ]" GE-SRC-CHECK-LINE
   s" PF ( -- ) 0 set-preflight" GE-SRC-CHECK-LINE ;

: UNSAFE-CHECKS ( -- )
   GE-HB-RESET
   UNSAFE-CHECK-SOURCE
   s" unsafe compiler words verdicts" GE-EVAL-RUN-STDIN
   SB-RESET
   s" 0" GE-OUT-LINE s" 0" GE-OUT-LINE s" 0" GE-OUT-LINE
   s" 0" GE-OUT-LINE s" 0" GE-OUT-LINE s" 0" GE-OUT-LINE
   s" 0" GE-OUT-LINE
   SB$ s" unsafe compiler words verdict output" GE-EXPECT-OUT
   GE-HB-RESET
   GE-SRC-RESET
   s" : EV ( -- n ) evaluate ;" GE-SRC-LINE
   s" tools/check.f accepted unsafe evaluate" CHECK-JSON
   s" habu-unsafe.err" WRITE-ERR
   s" code" s" E-UNSAFE" s" unsafe checker E-UNSAFE" ERR-JSTR
   s" token" s" evaluate" s" unsafe checker token" ERR-JSTR
   s" habu-unsafe.err" s" unsafe diagnostic contract" DIAG-CONTRACT
   s" diag-repair-class" s" habu-unsafe.err" s" trusted_boundary_required" s" unsafe repair class" GJA2S
   GE-HB-RESET
   GE-SRC-RESET
   s" : PF ( -- ) 0 set-preflight ;" GE-SRC-LINE
   s" tools/check.f accepted unsafe set-preflight" CHECK-JSON
   s" code" s" E-UNSAFE" s" set-preflight checker E-UNSAFE" ERR-JSTR
   s" token" s" set-preflight" s" set-preflight checker token" ERR-JSTR
   GE-HB-RESET
   GE-SRC-RESET
   s" : EV ( -- n ) evaluate ;" GE-SRC-LINE
   s" EV ." GE-SRC-LINE
   s" hb published unsafe evaluate definition" GE-HB-RUN-STDIN-NZ ;

\ F3: patch32 is a TRUSTED-only capability prim (machine-code sink). A CHECKED
\ definition that calls it must be rejected with the named E-CAP-TRUSTED code and
\ routed to the trusted_boundary_required repair class; exact private or
\ test-local TRUSTED: builders are the audited certified path.
: CAP-TRUSTED ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : GDX-FORGE ( -- ) 0 0 patch32 ;" GE-SRC-LINE
   s" tools/check.f accepted checked patch32 (F3)" CHECK-JSON
   s" habu-cap-trusted.err" WRITE-ERR
   s" code" s" E-CAP-TRUSTED" s" capability checker E-CAP-TRUSTED" ERR-JSTR
   s" token" s" patch32" s" capability checker token" ERR-JSTR
   s" habu-cap-trusted.err" s" capability diagnostic contract" DIAG-CONTRACT
   s" diag-repair-class" s" habu-cap-trusted.err" s" trusted_boundary_required" s" capability repair class" GJA2S ;

\ F3-FFI: the raw AAPCS64 trampolines ffi-call / ffi-call-abi / ffi-call-abi-r
\ are TRUSTED-only foreign-call boundaries (a checked caller could otherwise
\ resolve any C symbol and hand it raw pointers). Each must be rejected from a
\ CHECKED body with the named E-CAP-TRUSTED code, naming the offending prim.
: FFI-RAW-REJECT ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu:n tok:ptr toku:n :}
   GE-HB-RESET
   GE-SRC-RESET
   src srcu GE-SRC-LINE
   s" tools/check.f accepted checked raw ffi-call" CHECK-JSON
   s" code" s" E-CAP-TRUSTED" s" raw ffi checker E-CAP-TRUSTED" ERR-JSTR
   s" token" tok toku s" raw ffi checker token" ERR-JSTR ;

: CAP-TRUSTED-FFI ( -- )
   s" : GDX-FF ( ptr a n n -- n ) ffi-call ;"
   s" ffi-call" FFI-RAW-REJECT
   s" : GDX-FFA ( ptr a ptr b ptr c n n n n -- n ) ffi-call-abi ;"
   s" ffi-call-abi" FFI-RAW-REJECT
   s" : GDX-FFAR ( ptr a ptr b ptr c n n n n -- r ) ffi-call-abi-r ;"
   s" ffi-call-abi-r" FFI-RAW-REJECT ;

: LOCAL-IN-LOOP ( -- )
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

: LOAD-CLOSED ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : LOAD-BAD ( -- ) 1 ;" GE-SRC-LINE
   s" load-bad.f" WRITE-SRC
   s" --load" ARG+
   s" load-bad.f" PATH-ARGV+
   s" bin/hb" GE-TIMEOUT-MS GE-RUN-ENV
   s" checked load accepted bad declared effect" GE-EXPECT-NONZERO ;

\ A reject diagnostic whose expected/actual row is wider than RBUF-CAP (64) values
\ must fail closed: the renderer caps the row instead of overflowing RBUF into
\ adjacent DATA (which fed garbage type pointers to REND-TYPE -> SIGSEGV). 70
\ layout values in the actual row exceed the cap; the child must still exit with
\ the checker-reject rc and print the mismatch, never crash. (Red pre-fix: rc 134.)
: RENDER-CAP-CLOSED ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" SUMTYPE zrc 0" GE-SRC-LINE
   s"   VARIANT keep n ;VARIANT" GE-SRC-LINE
   s" ;SUMTYPE" GE-SRC-LINE
   s" : ZRCK ( n -- zrc ) ZRC:KEEP ;" GE-SRC-LINE
   s" : RENDER-CAP-MISUSE ( -- n ) " GE-SRC+
   70 0 ?do s" 1 ZRCK " GE-SRC+ loop
   s" ;" GE-SRC-LINE
   s" render-cap.f" WRITE-SRC
   s" --load" ARG+
   s" render-cap.f" PATH-ARGV+
   s" bin/hb" GE-TIMEOUT-MS GE-RUN-ENV
   70 s" wide diagnostic row must fail closed with rc 70, not crash" GE-EXPECT-RC
   s" actual: " s" wide diagnostic row must still render the mismatch" GE-EXPECT-ERR-HAS ;

\ Item 13 repair-packet family field: a layout (ADT) type mismatch must carry the
\ involved type-family name as `"family"`; a pure-scalar mismatch must not. The
\ field carries the interned qualified spelling — bare tail for these global
\ families; the foreign-package pkg:tail form is pinned in test/type-decl-suite.f
\ (TDLRJ2), since tools/check.f's scanner has no package-block support yet.
: ADT-FAMILY ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" SUMTYPE zrc 0 VARIANT keep n ;VARIANT ;SUMTYPE" GE-SRC-LINE
   s" : ZBAD ( n -- zrc ) ;" GE-SRC-LINE
   s" tools/check.f accepted ADT layout mismatch" CHECK-JSON
   s" habu-adt-family.err" WRITE-ERR
   s" code" s" E-MISMATCH" s" ADT mismatch code" ERR-JSTR
   s" family" s" zrc" s" ADT mismatch carries family field" ERR-JSTR
   s" repair_class" s" fix_type" s" ADT mismatch repair class" ERR-JSTR
   s" habu-adt-family.err" s" ADT family diagnostic contract" DIAG-CONTRACT
   s" json-one-schema" s" habu-adt-family.err" s" ADT family schema" GJA1
   GE-HB-RESET
   GE-SRC-RESET
   s" SUMTYPE famaa 0 VARIANT lefta ;VARIANT ;SUMTYPE" GE-SRC-LINE
   s" SUMTYPE fambb 0 VARIANT sameb ;VARIANT ;SUMTYPE" GE-SRC-LINE
   s" SUMTYPE famcc 0 VARIANT rightc ;VARIANT ;SUMTYPE" GE-SRC-LINE
   s" : FAM-MIS ( -- famaa fambb ) construct famcc rightc construct fambb sameb ;" GE-SRC-LINE
   s" tools/check.f accepted adjacent ADT mismatch" CHECK-JSON
   s" family" s" famaa" s" family names the exact expected mismatch" ERR-JSTR
   GE-HB-RESET
   GE-SRC-RESET
   s" SUMTYPE fambb 0 VARIANT sameb ;VARIANT ;SUMTYPE" GE-SRC-LINE
   s" : SBAD ( -- i64 fambb ) 0 >COUNT construct fambb sameb ;" GE-SRC-LINE
   s" tools/check.f accepted scalar mismatch" CHECK-JSON
   s" habu-scalar-nofam.err" WRITE-ERR
   s" code" s" E-MISMATCH" s" scalar mismatch code" ERR-JSTR
   s" family" s" scalar mismatch omits family field" ERR-NO-JKEY
   s" json-one-schema" s" habu-scalar-nofam.err" s" scalar mismatch schema" GJA1 ;

\ Item 13 repair-packet variant/tag field: a `construct family variant` payload
\ mismatch must carry the arm's sum-variant name (`variant`) and declaration-order
\ `tag`, plus the family it belongs to; a pure-scalar mismatch carries none. The
\ two-variant sum built on the SECOND variant proves `tag` tracks the actual arm
\ (toss = 1), not a hard-coded 0.
: ADT-VARIANT ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" SUMTYPE zrc 0 VARIANT keep n ;VARIANT VARIANT toss n ;VARIANT ;SUMTYPE" GE-SRC-LINE
   s" : CVBAD ( ptr u8 -- zrc ) construct zrc toss ;" GE-SRC-LINE
   s" tools/check.f accepted ADT variant payload mismatch" CHECK-JSON
   s" habu-adt-variant.err" WRITE-ERR
   s" code" s" E-MISMATCH" s" ADT variant mismatch code" ERR-JSTR
   s" family" s" zrc" s" ADT variant mismatch carries family" ERR-JSTR
   s" variant" s" toss" s" ADT variant mismatch carries variant name" ERR-JSTR
   s" tag" s" 1" s" ADT variant mismatch carries declaration-order tag" ERR-JRAW
   s" habu-adt-variant.err" s" ADT variant diagnostic contract" DIAG-CONTRACT
   s" json-one-schema" s" habu-adt-variant.err" s" ADT variant schema" GJA1
   GE-HB-RESET
   GE-SRC-RESET
   s" : SBAD ( i64 -- i64 ) dup ;" GE-SRC-LINE
   s" tools/check.f accepted scalar mismatch (variant slice)" CHECK-JSON
   s" habu-scalar-novar.err" WRITE-ERR
   s" variant" s" scalar mismatch omits variant field" ERR-NO-JKEY
   s" tag" s" scalar mismatch omits tag field" ERR-NO-JKEY
   s" json-one-schema" s" habu-scalar-novar.err" s" scalar novar schema" GJA1 ;

\ Item 13 repair-packet payload_pos field: a construct payload mismatch names the
\ 0-based declaration-order slot that failed. Both slots of the two-payload
\ variant expect the SAME type (n), so a type-matched capture cannot tell them
\ apart: the two arms prove the position is tracked structurally through the
\ unification spine (top slot 1, deep slot 0). A family-only layout mismatch
\ (no construct in scope) carries no payload_pos.
: ADT-PAYLOAD-POS ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" SUMTYPE zpp 0 VARIANT mix n n ;VARIANT ;SUMTYPE" GE-SRC-LINE
   s" : PBAD1 ( n ptr u8 -- zpp ) construct zpp mix ;" GE-SRC-LINE
   s" tools/check.f accepted slot-1 payload mismatch" CHECK-JSON
   s" habu-adt-pos1.err" WRITE-ERR
   s" code" s" E-MISMATCH" s" slot-1 payload mismatch code" ERR-JSTR
   s" variant" s" mix" s" slot-1 payload mismatch carries variant" ERR-JSTR
   s" payload_pos" s" 1" s" slot-1 payload mismatch names slot 1" ERR-JRAW
   s" habu-adt-pos1.err" s" payload_pos slot-1 diagnostic contract" DIAG-CONTRACT
   s" json-one-schema" s" habu-adt-pos1.err" s" payload_pos slot-1 schema" GJA1
   GE-HB-RESET
   GE-SRC-RESET
   s" SUMTYPE zpp 0 VARIANT mix n n ;VARIANT ;SUMTYPE" GE-SRC-LINE
   s" : PBAD0 ( ptr u8 n -- zpp ) construct zpp mix ;" GE-SRC-LINE
   s" tools/check.f accepted slot-0 payload mismatch" CHECK-JSON
   s" habu-adt-pos0.err" WRITE-ERR
   s" payload_pos" s" 0" s" slot-0 payload mismatch names slot 0" ERR-JRAW
   s" json-one-schema" s" habu-adt-pos0.err" s" payload_pos slot-0 schema" GJA1
   GE-HB-RESET
   GE-SRC-RESET
   s" SUMTYPE zpf 0 VARIANT keep n ;VARIANT ;SUMTYPE" GE-SRC-LINE
   s" : ZPBAD ( n -- zpf ) ;" GE-SRC-LINE
   s" tools/check.f accepted family-only mismatch (pos slice)" CHECK-JSON
   s" habu-adt-nopos.err" WRITE-ERR
   s" family" s" zpf" s" family-only mismatch still carries family" ERR-JSTR
   s" payload_pos" s" family-only mismatch omits payload_pos" ERR-NO-JKEY
   s" json-one-schema" s" habu-adt-nopos.err" s" family-only nopos schema" GJA1 ;

\ Item 13 repair-packet arity fields: a family applied with the wrong number of
\ signature arguments (E-WRONG-ARITY / fix_signature_arity) carries the family's
\ declared arity (`arity_expected`) and the written count (`arity_actual`). The
\ arity-2 arm proves the counts track the declaration, not a hard-coded pair; a
\ scalar mismatch carries neither.
: SIG-ARITY ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" SUMTYPE zar 0 VARIANT one ;VARIANT ;SUMTYPE" GE-SRC-LINE
   s" : ABAD ( zar<n> -- ) drop ;" GE-SRC-LINE
   s" tools/check.f accepted over-applied family signature" CHECK-JSON
   s" habu-sig-arity.err" WRITE-ERR
   s" code" s" E-WRONG-ARITY" s" wrong-arity code" ERR-JSTR
   s" repair_class" s" fix_signature_arity" s" wrong-arity repair class" ERR-JSTR
   s" arity_expected" s" 0" s" wrong-arity declared count" ERR-JRAW
   s" arity_actual" s" 1" s" wrong-arity written count" ERR-JRAW
   s" habu-sig-arity.err" s" wrong-arity diagnostic contract" DIAG-CONTRACT
   s" json-one-schema" s" habu-sig-arity.err" s" wrong-arity schema" GJA1
   GE-HB-RESET
   GE-SRC-RESET
   s" SUMTYPE zar2 2 VARIANT two a ;VARIANT ;SUMTYPE" GE-SRC-LINE
   s" : A2BAD ( zar2<n> -- ) drop ;" GE-SRC-LINE
   s" tools/check.f accepted under-applied family signature" CHECK-JSON
   s" habu-sig-arity2.err" WRITE-ERR
   s" arity_expected" s" 2" s" under-applied declared count" ERR-JRAW
   s" arity_actual" s" 1" s" under-applied written count" ERR-JRAW
   s" json-one-schema" s" habu-sig-arity2.err" s" under-applied schema" GJA1
   GE-HB-RESET
   GE-SRC-RESET
   s" : SBAD ( i64 -- i64 ) dup ;" GE-SRC-LINE
   s" tools/check.f accepted scalar mismatch (arity slice)" CHECK-JSON
   s" habu-scalar-noarity.err" WRITE-ERR
   s" arity_expected" s" scalar mismatch omits arity_expected" ERR-NO-JKEY
   s" arity_actual" s" scalar mismatch omits arity_actual" ERR-NO-JKEY
   s" json-one-schema" s" habu-scalar-noarity.err" s" scalar noarity schema" GJA1 ;

: TFAM-DECL ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" SUMTYPE badsum 0 VARIANT samev n ;VARIANT VARIANT samev n ;VARIANT ;SUMTYPE" GE-SRC-LINE
   s" tools/check.f accepted duplicate type-family variant" CHECK-JSON
   s" habu-tfam-decl.err" WRITE-ERR
   s" code" s" E-BAD-DECLARATION" s" declaration diagnostic code" ERR-JSTR
   s" repair_class" s" fix_family_declaration" s" declaration repair class" ERR-JSTR
   s" habu-tfam-decl.err" s" declaration diagnostic contract" DIAG-CONTRACT
   s" json-one-schema" s" habu-tfam-decl.err" s" declaration diagnostic schema" GJA1 ;

: ERROR-SOURCE ( -- )
   GE-SRC-RESET
   s" : GDX-AE-OK ( i64 -- i64 ) dup * ;" GE-SRC-LINE
   s" : GDX-AE-SEMI ( -- i64 ) [char] ; ;" GE-SRC-LINE
   s" : GDX-AE-BAD1 ( i64 -- i64 ) dup ;" GE-SRC-LINE
   s" : GDX-AE-BAD2 ( i64 -- ) >r ;" GE-SRC-LINE ;

: ALL-ERRORS ( -- )
   GE-HB-RESET
   ERROR-SOURCE
   s" habu-all-errors.f" WRITE-SRC
   s" habu-all-errors.f" s" tools/check.f --all-errors accepted bad defs" FILE-JSON-ALL
   s" habu-all-errors.err" WRITE-ERR
   s" diag-all-errors.err" s" all-errors golden" ERR-GOLDEN-R
   s" habu-all-errors.err" s" all-errors diagnostic contract" DIAG-CONTRACT
   s" all-errors" s" habu-all-errors.err" s" all-errors diagnostics" GJA1 ;

: UNDEFINED-RECURSIVE ( -- )
   GE-HB-RESET
   GE-SRC-RESET
   s" : UDEF ( i64 -- i64 ) dup NOPE ;" GE-SRC-LINE
   s" tools/check.f --all-errors accepted undefined word" CHECK-JSON-ALL
   s" habu-undef.err" WRITE-ERR
   s" diag-undefined.err" s" undefined golden" ERR-GOLDEN
   s" code" s" E-UNDEFINED" s" undefined diagnostic code" ERR-JSTR
   s" token" s" NOPE" s" undefined diagnostic token" ERR-JSTR
   s" habu-undef.err" s" undefined diagnostic contract" DIAG-CONTRACT
   s" json-one-schema" s" habu-undef.err" s" undefined schema" GJA1
   s" diag-repair-class" s" habu-undef.err" s" unknown_rejection" s" undefined repair class" GJA2S
   GE-HB-RESET
   GE-SRC-RESET
   s" : POW ( i64 -- i64 ) dup POW ;" GE-SRC-LINE
   s" tools/check.f --all-errors accepted recursive self-call" CHECK-JSON-ALL
   s" habu-recursive.err" WRITE-ERR
   s" token" s" POW" s" recursive diagnostic token" ERR-JSTR
   s" habu-recursive.err" s" recursive diagnostic contract" DIAG-CONTRACT
   s" json-one-schema" s" habu-recursive.err" s" recursive schema" GJA1 ;

: PUBLIC-SIGNATURES ( -- )
   GT-OUT-BUF GT-OUT-CAP PS-OUT-BUFFER!
   GT-ERR-BUF GT-ERR-CAP PS-ERR-BUFFER!
   0 PS-TRUST !
   PS-JSON-DOC-START
   s" examples/llm/good.f" PS-SCAN-FILE
   PS-JSON-DOC-END
   PS-ERR$ nip 0 <> if s" public-signatures stderr" GE-FAIL then
   s" public-signatures.json" PATH!
   PATH$ PS-OUT$ WRITE-ALL
   PS-BUFFERS-OFF
   s" public-signatures" s" public-signatures.json" s" public signatures output" GJA1 ;

public

: LABEL-COPY-SLICE ( -- )
   s" hb-gate-diagnostics-label-copy" GT-START
   LABEL-COPY
   GT-CLEANUP
   s" PASS: native checker diagnostics label-copy slice" type cr ;

: REPAIR ( -- )
   s" hb-gate-diagnostics-repair" GT-START
   REPAIR-CLASSES
   PUBLIC-SIGNATURES
   GT-CLEANUP
   s" PASS: native checker diagnostics repair slice" type cr ;

: UNDEFINED-PRIMARY ( -- )
   s" hb-gate-diagnostics-undef-primary" GT-START
   UNDEFINED-RECURSIVE
   PRIMARY-JSON
   UNKNOWN-SIGNATURE
   MALFORMED-QUOTATION-SIGNATURE
   BAD-PARAM-SIGNATURE
   RENDER-CAP-CLOSED
   ADT-FAMILY
   ADT-VARIANT
   ADT-PAYLOAD-POS
   SIG-ARITY
   TFAM-DECL
   GT-CLEANUP
   s" PASS: native checker diagnostics undef-primary slice" type cr ;

: FILE-UNSAFE ( -- )
   s" hb-gate-diagnostics-file-unsafe" GT-START
   FILE-ORIGIN
   UNSAFE-CHECKS
   CAP-TRUSTED
   CAP-TRUSTED-FFI
   LOCAL-IN-LOOP
   GT-CLEANUP
   s" PASS: native checker diagnostics file-unsafe slice" type cr ;

;package
