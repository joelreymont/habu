\ gate-stdlib-inline-lib.f - in-process stdlib gate slices for resident runners.
\
\ Load after test/gate-stdlib-lib.f in the resident test runner.

variable GSI-TIMINGS
variable GSI-PATH-A
variable GSI-PATH-U
variable GSI-START-NS
variable GSI-RC
variable GSI-SETUP
variable GSI-TEST-READY
variable GSI-TOOL-BASE-READY

$10000 constant GSI-TL-STR-CAP
$20000 constant GSI-TL-FILE-CAP

create GSI-TL-STR-BUF GSI-TL-STR-CAP allot
create GSI-TL-FILE-BUF GSI-TL-FILE-CAP allot

0 constant GSI-GROUP-SEQ
1 constant GSI-GROUP-PAR

: GSI-PATH-A-FIELD ( -- ptr ptr u8 )
   GSI-PATH-A 0 ptr-field ;

: GSI-PATH-A@ ( -- ptr u8 )
   GSI-PATH-A-FIELD @ ;

: GSI-PATH-A! ( ptr u8 -- )
   GSI-PATH-A-FIELD ! ;

: GSI-PATH$ ( -- ptr u8 n )
   GSI-PATH-A@ GSI-PATH-U @ ;

: GSI-TIMINGS! ( -- )
   -1 GSI-TIMINGS ! ;

: GSI-TIMINGS? ( -- bool )
   GSI-TIMINGS @ 0 <> ;

: GSI-SETUP! ( -- )
   -1 GSI-SETUP ! ;

: GSI-TEST! ( -- )
   0 GSI-SETUP ! ;

: GSI-SETUP? ( -- bool )
   GSI-SETUP @ 0 <> ;

: GSI-TEST-READY! ( -- )
   -1 GSI-TEST-READY ! ;

: GSI-TEST-READY? ( -- bool )
   GSI-TEST-READY @ 0 <> ;

: GSI-TOOL-BASE-READY! ( -- )
   -1 GSI-TOOL-BASE-READY ! ;

: GSI-TOOL-BASE-READY? ( -- bool )
   GSI-TOOL-BASE-READY @ 0 <> ;

: GSI-GROUP-MODE. ( n -- ) {: mode:n :}
   mode case
      GSI-GROUP-SEQ of s" sequential" type endof
      GSI-GROUP-PAR of s" parallel" type endof
      E-TBL-FIELD throw
   endcase ;

: GSI-GROUP-HEADER ( ptr u8 n n -- ) {: name:ptr nameu:n mode:n :}
   s" GROUP: " type name nameu type
   s"  [" type mode GSI-GROUP-MODE. s" ]" type cr ;

: GSI-PASS ( ptr u8 n n -- ) {: path:ptr pathu:n ms:n :}
   GSI-SETUP? if exit then
   GSI-TIMINGS? 0= if exit then
   s" PASS: " type path pathu type
   s"  (" type ms GT-U-TYPE s" ms)" type cr ;

: GSI-FAIL ( ptr u8 n n -- ) {: path:ptr pathu:n ms:n :}
   s" FAIL: " type
   GSI-SETUP? if s" setup " type then
   path pathu type
   s"  (" type ms GT-U-TYPE s" ms)" type cr ;

: GSI-SPAN ( ptr u8 n n -- ) {: path:ptr pathu:n ms:n :}
   GSI-SETUP? if exit then
   path pathu ms GS-SPAN ;

: GSI-INCLUDE-ACT ( -- )
   GSI-PATH$ included ;

: GSI-REQUIRE-ACT ( -- )
   GSI-PATH$ required ;

: GSI-INCLUDE-MS ( -- n )
   mono-ns GSI-START-NS @ - PROC-NS-PER-MS / ;

: GSI-LOAD-START ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path GSI-PATH-A!
   pathu GSI-PATH-U !
   mono-ns GSI-START-NS ! ;

: GSI-LOAD-FINISH ( -- )
   GSI-INCLUDE-MS {: ms:n :}
   GSI-PATH$ ms GSI-SPAN
   GSI-RC @ 0= if GSI-PATH$ ms GSI-PASS exit then
   GSI-PATH$ ms GSI-FAIL
   GSI-RC @ throw ;

: GSI-INCLUDE ( ptr u8 n -- )
   GSI-LOAD-START
   [: GSI-INCLUDE-ACT ;] catch GSI-RC !
   GSI-LOAD-FINISH ;

: GSI-REQUIRE ( ptr u8 n -- )
   GSI-LOAD-START
   [: GSI-REQUIRE-ACT ;] catch GSI-RC !
   GSI-LOAD-FINISH ;

\ typed-local-lint: allow-bare-local - q keeps the action effect from the stack signature.
: GSI-RUN ( ptr u8 n [ -- ] -- ) {: label:ptr labelu:n q :}
   label GSI-PATH-A!
   labelu GSI-PATH-U !
   mono-ns GSI-START-NS !
   q catch GSI-RC !
   GSI-INCLUDE-MS {: ms:n :}
   GSI-PATH$ ms GSI-SPAN
   GSI-RC @ 0= if GSI-PATH$ ms GSI-PASS exit then
   GSI-PATH$ ms GSI-FAIL
   GSI-RC @ throw ;

: GSI-TOOL-BASE ( -- )
   GSI-TOOL-BASE-READY? if exit then
   s" tools/date.f" GSI-REQUIRE
   GSI-TEST-READY? 0= if
      s" lib/test.f" GSI-REQUIRE
      GSI-TEST-READY!
   then
   s" lib/source.f" GSI-REQUIRE
   s" tools/lint/text.f" GSI-REQUIRE
   s" tools/lint/intern.f" GSI-REQUIRE
   s" tools/lint/token.f" GSI-REQUIRE
   s" tools/lint/lib.f" GSI-REQUIRE
   s" tools/lint/json-writer.f" GSI-REQUIRE
   s" tools/lint/source-lex.f" GSI-REQUIRE
   s" tools/argv.f" GSI-REQUIRE
   s" tools/check-all-errors-core.f" GSI-REQUIRE
   s" tools/diag-origin-core.f" GSI-REQUIRE
   s" tools/json.f" GSI-REQUIRE
   s" tools/json-only-core.f" GSI-REQUIRE
   s" tools/aot-lint-core.f" GSI-REQUIRE
   s" tools/signature-lint-core.f" GSI-REQUIRE
   s" tools/checked-boundary-lint-core.f" GSI-REQUIRE
   s" tools/reserved-name-lint-core.f" GSI-REQUIRE
   s" tools/trust-lint-core.f" GSI-REQUIRE
   s" tools/duplicate-definition-lint-core.f" GSI-REQUIRE
   s" tools/bundle-lib-core.f" GSI-REQUIRE
   GSI-TOOL-BASE-READY! ;

: GSI-TOOL-SETUP ( -- )
   GSI-SETUP!
   GSI-TOOL-BASE
   GSI-TEST! ;

: GSI-TOOL-SETUP-FILE ( ptr u8 n -- )
   GSI-SETUP!
   GSI-REQUIRE
   GSI-TEST! ;

: GSI-TOOL-REPAIR-CHECK ( -- )
   s" stdlib/tool-repair/check-all-errors" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TOOL-SETUP
   s" tools/check-all-errors-test.f" GSI-INCLUDE ;

: GSI-TOOL-TRUST ( -- )
   s" stdlib/tool-trust" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TOOL-SETUP
   s" tools/trust-lint-test.f" GSI-INCLUDE
   s" tools/aot-call-report-test.f" GSI-INCLUDE ;

: GSI-CHECK-CLI ( -- )
   s" stdlib/check-cli" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TOOL-SETUP
   s" tools/check-core.f" GSI-TOOL-SETUP-FILE
   s" tools/check-test.f" GSI-INCLUDE ;

: GSI-TOOL-REPAIR-PACKET ( -- )
   s" stdlib/tool-repair/repair-packet" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TOOL-SETUP
   s" tools/repair-packet-core.f" GSI-TOOL-SETUP-FILE
   s" tools/repair-packet-test.f" GSI-INCLUDE ;

: GSI-TOOL-DOC-PUBLIC ( -- )
   s" stdlib/tool-doc/public-signatures" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TOOL-SETUP
   s" tools/public-signatures-core.f" GSI-TOOL-SETUP-FILE
   s" tools/public-signatures-test.f" GSI-INCLUDE ;

: GSI-TOOL-DOC-STATUS ( -- )
   s" stdlib/tool-doc/stale-status" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TOOL-SETUP
   s" tools/stale-status-lint-core.f" GSI-TOOL-SETUP-FILE
   s" tools/stale-status-lint-test.f" GSI-INCLUDE ;

: GSI-TOOL-DOC-SCHEMA ( -- )
   s" stdlib/tool-doc/schema-examples" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TOOL-SETUP
   s" tools/repair-schema-doc-test.f" GSI-INCLUDE
   s" tools/examples-test.f" GSI-INCLUDE ;

: GSI-TOOL-LINT-REPL ( -- )
   s" tools/repl-lint-test.f" GSI-INCLUDE
   s" tools/diag-origin-test.f" GSI-INCLUDE ;

: GSI-TOOL-LINT-AOT ( -- )
   s" stdlib/tool-lints/aot-signature" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TOOL-SETUP
   s" tools/aot-lint-test.f" GSI-INCLUDE
   s" tools/signature-lint-test.f" GSI-INCLUDE ;

: GSI-TOOL-LINT-NAMES ( -- )
   s" stdlib/tool-lints/names" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TOOL-SETUP
   s" tools/checked-boundary-lint-test.f" GSI-INCLUDE
   s" tools/reserved-name-lint-test.f" GSI-INCLUDE
   s" tools/duplicate-definition-lint-test.f" GSI-INCLUDE ;

: GSI-TOOL-LINT-BUNDLE ( -- )
   s" stdlib/tool-lints/bundle-json" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TOOL-SETUP
   s" tools/bundle-lib-test.f" GSI-INCLUDE
   s" tools/json-only-test.f" GSI-INCLUDE ;

: GSI-TOOL-LINT-REPL-PHASE ( -- )
   s" stdlib/tool-lints/repl" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TOOL-SETUP
   s" tools/repl-lint-core.f" GSI-TOOL-SETUP-FILE
   GSI-TOOL-LINT-REPL ;

: GSI-TOOL-REPAIR-SETUP ( -- )
   GSI-TOOL-SETUP
   s" tools/repair-packet-core.f" GSI-TOOL-SETUP-FILE ;

: GSI-TOOL-REPAIR-BODY ( -- )
   s" tools/check-all-errors-test.f" GSI-INCLUDE
   s" tools/repair-packet-test.f" GSI-INCLUDE ;

: GSI-TOOL-DOC-SETUP ( -- )
   GSI-TOOL-SETUP
   s" tools/public-signatures-core.f" GSI-TOOL-SETUP-FILE
   s" tools/stale-status-lint-core.f" GSI-TOOL-SETUP-FILE ;

: GSI-TOOL-DOC-BODY ( -- )
   s" tools/public-signatures-test.f" GSI-INCLUDE
   s" tools/stale-status-lint-test.f" GSI-INCLUDE
   s" tools/repair-schema-doc-test.f" GSI-INCLUDE
   s" tools/examples-test.f" GSI-INCLUDE ;

: GSI-TOOL-LINT-SETUP ( -- )
   GSI-TOOL-SETUP
   s" tools/repl-lint-core.f" GSI-TOOL-SETUP-FILE ;

: GSI-TOOL-LINT-BODY ( -- )
   GSI-TOOL-LINT-REPL
   s" tools/aot-lint-test.f" GSI-INCLUDE
   s" tools/signature-lint-test.f" GSI-INCLUDE
   s" tools/checked-boundary-lint-test.f" GSI-INCLUDE
   s" tools/reserved-name-lint-test.f" GSI-INCLUDE
   s" tools/duplicate-definition-lint-test.f" GSI-INCLUDE
   s" tools/bundle-lib-test.f" GSI-INCLUDE
   s" tools/json-only-test.f" GSI-INCLUDE ;

: GSI-TOOL-TYPED-SETUP ( -- )
   GSI-TOOL-SETUP
   s" tools/typed-local-diff-lint-core.f" GSI-TOOL-SETUP-FILE ;

: GSI-TOOL-TYPED-BODY ( -- )
   s" tools/typed-local-diff-lint-test.f" GSI-INCLUDE ;

: GSI-TOOL-REPAIR ( -- )
   s" stdlib/tool-repair" GSI-GROUP-PAR GSI-GROUP-HEADER
   GSI-TOOL-REPAIR-SETUP
   GSI-TOOL-REPAIR-BODY ;

: GSI-TOOL-DOC ( -- )
   s" stdlib/tool-doc" GSI-GROUP-PAR GSI-GROUP-HEADER
   GSI-TOOL-DOC-SETUP
   GSI-TOOL-DOC-BODY ;

: GSI-TOOL-LINT-PHASE ( -- )
   s" stdlib/tool-lints" GSI-GROUP-PAR GSI-GROUP-HEADER
   GSI-TOOL-LINT-SETUP
   GSI-TOOL-LINT-BODY ;

: GSI-TOOL-TYPED ( -- )
   s" stdlib/tool-typed-local" GSI-GROUP-PAR GSI-GROUP-HEADER
   GSI-TOOL-TYPED-SETUP
   GSI-TOOL-TYPED-BODY ;

: GSI-TOOL-SEMANTICS ( -- )
   s" stdlib/tool-semantics" GSI-GROUP-SEQ GSI-GROUP-HEADER
   s" stdlib/tool-repair" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TOOL-REPAIR-SETUP
   GSI-TOOL-REPAIR-BODY
   s" stdlib/tool-doc" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TOOL-DOC-SETUP
   GSI-TOOL-DOC-BODY
   s" stdlib/tool-lints" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TOOL-LINT-SETUP
   GSI-TOOL-LINT-BODY
   s" stdlib/tool-typed-local" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TOOL-TYPED-SETUP
   GSI-TOOL-TYPED-BODY ;

: GSI-LINT-TOOLS-SETUP ( -- )
   GSI-SETUP!
   GSI-TOOL-BASE
   s" tools/repl-lint-core.f" GSI-REQUIRE
   s" tools/trust-lint-core.f" GSI-REQUIRE
   s" tools/stale-status-lint-core.f" GSI-REQUIRE
   s" tools/dot-dep-lint-core.f" GSI-REQUIRE
   s" tools/maki-dep-lint-core.f" GSI-REQUIRE
   GSI-TEST! ;

: GSI-LINT-TOOLS ( -- )
   s" stdlib/lint-tools" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-LINT-TOOLS-SETUP
   s" test/gate-stdlib-lint-tools.f" included ;

: GSI-TEST-SETUP ( -- )
   GSI-SETUP!
   GSI-TEST-READY? 0= if
      s" lib/test.f" GSI-REQUIRE
      GSI-TEST-READY!
   then
   GSI-TEST! ;

: GSI-TAIL-FAST-SETUP ( -- )
   GSI-TEST-SETUP
   GSI-TOOL-BASE-READY? 0= if s" tools/date.f" GSI-REQUIRE then
   s" lib/property.f" GSI-REQUIRE
   GSI-TEST! ;

: GSI-TAIL-FAST ( -- )
   s" stdlib/tail-fast" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TAIL-FAST-SETUP
   s" lib/test/assert-test.f" GSI-INCLUDE
   s" lib/test/suite-test.f" GSI-INCLUDE
   s" lib/property-test.f" GSI-INCLUDE
   s" tools/date-test.f" GSI-INCLUDE
   s" tools/spawn-emitter-test.f" GSI-INCLUDE
   s" tools/c-call-emitter-test.f" GSI-INCLUDE
   s" tools/signature-scan-emitter-test.f" GSI-INCLUDE
   s" tools/compiler-dispatch-test.f" GSI-INCLUDE ;

: GSI-TAIL-PURE-SETUP ( -- )
   GSI-TEST-SETUP
   s" lib/json-write.f" GSI-REQUIRE
   GSI-TEST! ;

: GSI-TAIL-PURE ( -- )
   s" stdlib/tail-pure" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TAIL-PURE-SETUP
   s" lib/json-write-test.f" GSI-INCLUDE
   s" lib/memory-test.f" GSI-INCLUDE
   s" lib/vector-test.f" GSI-INCLUDE
   s" lib/fs-test.f" GSI-INCLUDE
   s" tools/bootstrap-codegen-test.f" GSI-INCLUDE
   s" tools/asm-src-test.f" GSI-INCLUDE
   s" tools/asm-checked-test.f" GSI-INCLUDE
   s" tools/image-bytes-test.f" GSI-INCLUDE ;

: GSI-TAIL-RUNNER ( -- )
   s" stdlib/tail-runner" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TEST-SETUP
   s" lib/test/runner-test.f" GSI-INCLUDE ;

: GSI-TAIL-BUILD ( -- )
   s" stdlib/tail-build" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TEST-SETUP
   s" lib/build-test.f" GSI-INCLUDE ;

: GSI-TAIL-WARM-SETUP ( -- )
   GSI-TEST-SETUP ;

: GSI-TAIL-WARM ( -- )
   s" stdlib/tail-warm-image" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TAIL-WARM-SETUP
   s" tools/warm-image-test.f" GSI-INCLUDE ;

: GSI-TAIL-PROCESS ( -- )
   s" stdlib/tail-process" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TEST-SETUP
   s" tools/hb-cli-contracts-test.f" GSI-INCLUDE
   s" lib/process-test.f" GSI-INCLUDE
   s" lib/process-command-test.f" GSI-INCLUDE ;

: GSI-LINT-LIBS-CORE ( -- )
   s" stdlib/lint-libs/core" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TEST-SETUP
   s" lib/string-test.f" GSI-INCLUDE
   s" lib/ffi-abi-test.f" GSI-INCLUDE
   s" lib/array-test.f" GSI-INCLUDE
   s" lib/table-test.f" GSI-INCLUDE
   s" lib/regex-test.f" GSI-INCLUDE
   s" lib/map-test.f" GSI-INCLUDE ;

: GSI-LINT-LIBS-PTX ( -- )
   s" stdlib/lint-libs/ptx" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TEST-SETUP
   s" lib/ptx/header-test.f" GSI-INCLUDE
   s" lib/ptx/launch-test.f" GSI-INCLUDE
   s" lib/ptx/tile-test.f" GSI-INCLUDE
   s" lib/ptx/tile-loop-test.f" GSI-INCLUDE
   s" lib/ptx/tile-smem-test.f" GSI-INCLUDE
   s" lib/ptx/tile-acc-test.f" GSI-INCLUDE
   s" lib/ptx/gemm-checked-test.f" GSI-INCLUDE
   s" lib/ptx/tile-v4-test.f" GSI-INCLUDE
   s" lib/ptx/collective-test.f" GSI-INCLUDE
   s" lib/ptx/autograd-test.f" GSI-INCLUDE
   s" lib/ptx/ir-test.f" GSI-INCLUDE
   s" lib/ptx/ad-test.f" GSI-INCLUDE
   s" lib/ptx/ad-dag-test.f" GSI-INCLUDE
   s" lib/ptx/ad-saved-test.f" GSI-INCLUDE ;

: GSI-LINT-LIBS-PTX-NEG ( -- )
   s" stdlib/lint-libs/ptx-neg" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TEST-SETUP
   s" lib/ptx/tile-loop-neg-test.f" GSI-INCLUDE
   s" lib/ptx/tile-smem-neg-test.f" GSI-INCLUDE
   s" lib/ptx/tile-acc-neg-test.f" GSI-INCLUDE
   s" lib/ptx/gemm-checked-neg-test.f" GSI-INCLUDE ;

: GSI-LINT-LIBS-PTX-TOOL ( -- )
   s" stdlib/lint-libs/ptx-toolchain" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TEST-SETUP
   s" tools/ptx/saxpy-test.f" GSI-INCLUDE ;

: GSI-LINT-MANIFEST ( -- )
   s" stdlib/lint-manifest" GSI-GROUP-SEQ GSI-GROUP-HEADER
   s" tools/stdlib-manifest-test.f" GSI-INCLUDE ;

: GSI-LINT-ARTIFACTS-FAST ( -- )
   s" stdlib/lint-artifacts/fast" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TEST-SETUP
   s" tools/lint/text-foundation-test.f" GSI-INCLUDE
   s" tools/json-file-test.f" GSI-INCLUDE
   s" tools/sha256-file-test.f" GSI-INCLUDE
   s" lib/content-key-test.f" GSI-INCLUDE ;
