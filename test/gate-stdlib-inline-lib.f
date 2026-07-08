\ gate-stdlib-inline-lib.f - in-process stdlib gate slices for resident runners.
\
\ Load after test/gate-stdlib-lib.f in the resident test runner.

require lib/memory.f

variable GSI-TIMINGS
variable GSI-PATH-A
variable GSI-PATH-U
variable GSI-START-NS
variable GSI-RC
variable GSI-SETUP
variable GSI-TEST-READY
variable GSI-TOOL-BASE-READY

\ Trust-lint scratch: the file buffer must hold the largest scanned source
\ (src/core/checker.f outgrew the old $20000 cap); runtime-sized buffers are
\ lib/memory allocations, not dictionary allot, and grow by constant only.
\ Reads through the buffer stay fail-closed: READ-FILE dies on overflow, so
\ outgrowing the cap is a loud gate failure, never truncation.
$80000 constant GSI-TL-STR-CAP
$40000 constant GSI-TL-FILE-CAP
600000 constant GSI-FORK-TIMEOUT-MS

variable GSI-TL-READY
variable GSI-TL-STR-A
variable GSI-TL-FILE-A

: GSI-TL-STR-A-FIELD ( -- ptr ptr u8 )
   GSI-TL-STR-A 0 ptr-field ;

: GSI-TL-FILE-A-FIELD ( -- ptr ptr u8 )
   GSI-TL-FILE-A 0 ptr-field ;

: GSI-TL-ALLOC ( -- )
   GSI-TL-READY @ 0 <> if exit then
   GSI-TL-STR-CAP MEM-ALLOC-BYTES drop GSI-TL-STR-A-FIELD !
   GSI-TL-FILE-CAP MEM-ALLOC-BYTES drop GSI-TL-FILE-A-FIELD !
   -1 GSI-TL-READY ! ;

: GSI-TL-STR-BUF ( -- ptr u8 )
   GSI-TL-ALLOC
   GSI-TL-STR-A-FIELD @ ;

: GSI-TL-FILE-BUF ( -- ptr u8 )
   GSI-TL-ALLOC
   GSI-TL-FILE-A-FIELD @ ;

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

: GSI-FORK-RESET ( -- )
   GT-POOL-RESET ;

: GSI-FORK-DRAIN ( -- )
   GT-POOL-DRAIN ;

: GSI-FORK-INCLUDE-ACT ( -- )
   GSI-PATH$ GSI-INCLUDE ;

: GSI-FORK-INCLUDE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path GSI-PATH-A!
   pathu GSI-PATH-U !
   path pathu GSI-FORK-TIMEOUT-MS [: GSI-FORK-INCLUDE-ACT ;] GT-POOL-START-FORK ;

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
   s" tools/maki-ns-lint-core.f" GSI-REQUIRE
   GSI-TEST! ;

: GSI-LINT-TOOLS ( -- )
   s" stdlib/lint-tools" GSI-GROUP-PAR GSI-GROUP-HEADER
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
   s" lib/test/snap-test.f" GSI-INCLUDE
   s" lib/test/record-test.f" GSI-INCLUDE
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
   s" stdlib/tail-pure" GSI-GROUP-PAR GSI-GROUP-HEADER
   GSI-TAIL-PURE-SETUP
   GSI-FORK-RESET
   s" lib/json-write-test.f" GSI-FORK-INCLUDE
   s" lib/memory-test.f" GSI-FORK-INCLUDE
   s" lib/vector-test.f" GSI-FORK-INCLUDE
   s" lib/fs-test.f" GSI-FORK-INCLUDE
   s" tools/bootstrap-codegen-test.f" GSI-FORK-INCLUDE
   s" tools/asm-src-test.f" GSI-FORK-INCLUDE
   s" tools/asm-checked-test.f" GSI-FORK-INCLUDE
   s" test/drec-shape-test.f" GSI-FORK-INCLUDE
   s" tools/image-bytes-test.f" GSI-FORK-INCLUDE
   s" tools/include-events-test.f" GSI-FORK-INCLUDE
   s" tools/source-discovery-test.f" GSI-FORK-INCLUDE
   s" tools/event-closure-test.f" GSI-FORK-INCLUDE
   GSI-FORK-DRAIN ;

: GSI-TAIL-RUNNER ( -- )
   s" stdlib/tail-runner" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TEST-SETUP
   s" lib/test/runner-test.f" GSI-INCLUDE ;

: GSI-TAIL-BUILD ( -- )
   s" stdlib/tail-build" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TEST-SETUP
   s" lib/build-test.f" GSI-INCLUDE ;

: GSI-TAIL-PROCESS ( -- )
   s" stdlib/tail-process" GSI-GROUP-PAR GSI-GROUP-HEADER
   GSI-TEST-SETUP
   GSI-FORK-RESET
   s" tools/hb-cli-contracts-test.f" GSI-FORK-INCLUDE
   s" test/seal.f" GSI-FORK-INCLUDE
   s" test/seal-absence.f" GSI-FORK-INCLUDE
   s" test/seal-package.f" GSI-FORK-INCLUDE
   s" test/gate-runner-entry-test.f" GSI-FORK-INCLUDE
   s" lib/process-test.f" GSI-FORK-INCLUDE
   s" lib/process-command-test.f" GSI-FORK-INCLUDE
   GSI-FORK-DRAIN ;

: GSI-LINT-LIBS-CORE ( -- )
   s" stdlib/lint-libs/core" GSI-GROUP-PAR GSI-GROUP-HEADER
   GSI-TEST-SETUP
   GSI-FORK-RESET
   s" lib/string-test.f" GSI-FORK-INCLUDE
   s" lib/ffi-abi-test.f" GSI-FORK-INCLUDE
   s" lib/array-test.f" GSI-FORK-INCLUDE
   s" lib/table-test.f" GSI-FORK-INCLUDE
   s" lib/regex-test.f" GSI-FORK-INCLUDE
   s" lib/map-test.f" GSI-FORK-INCLUDE
   GSI-FORK-DRAIN ;

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
   s" stdlib/lint-libs/ptx-toolchain" GSI-GROUP-SEQ GSI-GROUP-HEADER \ ( -- )
   GSI-TEST-SETUP \ ( -- )
   s" lib/ptx/toolchain-test.f" GSI-INCLUDE \ ( -- )
   s" tools/ptx/profile-test.f" GSI-INCLUDE \ ( -- )
   s" tools/ptx/bench-test.f" GSI-INCLUDE \ ( -- )
   s" tools/ptx/saxpy-test.f" GSI-INCLUDE ; \ ( -- )

: GSI-LINT-MANIFEST ( -- )
   s" stdlib/lint-manifest" GSI-GROUP-SEQ GSI-GROUP-HEADER
   s" tools/stdlib-manifest-test.f" GSI-INCLUDE ;

: GSI-LINT-ARTIFACTS-FAST ( -- )
   s" stdlib/lint-artifacts/fast" GSI-GROUP-SEQ GSI-GROUP-HEADER
   GSI-TEST-SETUP
   s" tools/lint/text-foundation-test.f" GSI-INCLUDE
   s" tools/json-file-test.f" GSI-INCLUDE
   s" tools/sha256-file-test.f" GSI-INCLUDE
   s" lib/content-key-test.f" GSI-INCLUDE
   s" test/run-result-cache-test.f" GSI-INCLUDE
   s" test/run-budget-cal-test.f" GSI-INCLUDE
   s" test/run-rerun-failed-test.f" GSI-INCLUDE
   s" test/golden-test.f" GSI-INCLUDE
   s" tools/diagnose-hb-test.f" GSI-INCLUDE
   s" lib/object-test.f" GSI-INCLUDE ;
