\ gate-stdlib-lint-tools.f - in-process lint-tools body.
\
\ Load after GSI-LINT-TOOLS-SETUP.

package GATE-LINT-TOOLS
private

: REPL ( -- )
   s" ." REPL-ROOT!
   REPL-LINT ;

: TRUST-SCAN ( -- )
   GSI-TL-STR-BUF GSI-TL-STR-CAP
   GSI-TL-FILE-BUF GSI-TL-FILE-CAP
   TRUST-LINT-BUFFERS!
   TL-TRUE TL-REPORT-SUCCESS!
   s" ." TRUST-LINT-ROOT!
   TRUST-LINT-TODAY-NOW
   TRUST-LINT ;

: STALE-STATUS ( -- )
   s" ." SS-ROOT!
   epoch-seconds DATE:SECONDS-DAY / SS-TODAY-DAYS !
   STALE-STATUS-LINT ;

: CLOBBER ( -- )
   s" tools/lint/clobber-lint.f" GSI-REQUIRE
   s" tools/lint/clobber-lint-test.f" GSI-INCLUDE ;

: REPOSITORY ( -- )
   s" tools/lint/shadow-lint.f" GSI-INCLUDE
   s" tools/lint/ptx-emitter-lint.f" GSI-INCLUDE
   s" tools/host-lint.f" GSI-REQUIRE
   s" tools/process-primitive-lint.f" GSI-INCLUDE
   s" tools/process-primitive-lint-test.f" GSI-INCLUDE
   s" tools/parallel-agent-lint.f" GSI-INCLUDE
   s" tools/filemap-lint.f" GSI-REQUIRE
   s" tools/filemap-lint-test.f" GSI-INCLUDE
   s" tools/stdin-closure-lint.f" GSI-REQUIRE ;

: STATUS ( -- )
   s" repl-lint" [: REPL ;] GSI-RUN
   s" trust-lint" [: TRUST-SCAN ;] GSI-RUN
   s" stale-status-lint" [: STALE-STATUS ;] GSI-RUN
   s" test/gate-stats-test.f" GSI-INCLUDE ;

\ One fork per sub-suite so GT-POOL-FAIL's `FAIL: <label>` line names the
\ failing sub-suite directly. The old lint-tools/dot-maki bundled dot, maki,
\ maki-ns, host, and trusted-inventory in a single fork; a test file's
\ T-REPORT `die` exits the fork and bypasses GSI-INCLUDE's per-file FAIL line,
\ so a trusted-inventory ratchet failure surfaced only under the misleading
\ dot-maki label. Setup is loaded once in the parent and inherited copy-on-
\ write by every fork, so the split adds no setup cost.
: DOT ( -- )
   s" dot-dep-lint" [: DOT-DEP-LINT ;] GSI-RUN
   s" tools/dot-dep-lint-test.f" GSI-INCLUDE ;

: NANOGPT ( -- )
   s" nanogpt-inventory-lint" [: NANOGPT-INVENTORY-LINT ;] GSI-RUN
   s" tools/nanogpt-inventory-lint-test.f" GSI-INCLUDE ;

: MAKI ( -- )
   s" maki-dep-lint" [: MAKI-DEP-LINT ;] GSI-RUN
   s" tools/maki-dep-lint-test.f" GSI-INCLUDE ;

: REFINE ( -- )
   s" refine-lint" [: REFINE-LINT ;] GSI-RUN
   s" tools/refine-lint-test.f" GSI-INCLUDE ;

: SUITE-COVERAGE ( -- )
   s" suite-coverage-lint" [: SUITE-COVERAGE-LINT ;] GSI-RUN
   s" tools/suite-coverage-lint-test.f" GSI-INCLUDE ;

: NAMESPACE ( -- )
   s" namespace-lint" [: NAMESPACE-LINT-STRICT ;] GSI-RUN
   s" tools/namespace-lint-test.f" GSI-INCLUDE ;

: PACKAGE-OWNERSHIP ( -- )
   s" tools/package-diff-lint-test.f" GSI-INCLUDE ;

: ERROR-CODE ( -- )
   s" error-code-lint" [: ERROR-CODE-LINT-STRICT ;] GSI-RUN
   s" tools/error-code-lint-test.f" GSI-INCLUDE ;

: HOST ( -- )
   s" tools/host-lint-test.f" GSI-INCLUDE ;

: TRUSTED-INVENTORY ( -- )
   s" tools/trusted-inventory-test.f" GSI-INCLUDE ;

: PRIMITIVE-EFFECT-INVENTORY ( -- )
   s" tools/primitive-effect-inventory-test.f" GSI-INCLUDE ;

: BOOTSTRAP-MIRROR ( -- )
   s" tools/bootstrap-mirror-lint-test.f" GSI-INCLUDE ;

: BOOTSTRAP-REFRESH ( -- )
   s" tools/bootstrap-refresh-doc-test.f" GSI-INCLUDE ;

: PACKAGE-INHERITANCE ( -- )
   s" test/gate-lint-tools-package-child.f" GSI-INCLUDE ;

public

: RUN ( -- )
   GSI-FORK-RESET
   s" lint-tools/package-inheritance" GSI-FORK-TIMEOUT-MS [: PACKAGE-INHERITANCE ;] GT-POOL-START-FORK
   s" lint-tools/clobber" GSI-FORK-TIMEOUT-MS [: CLOBBER ;] GT-POOL-START-FORK
   s" lint-tools/repo" GSI-FORK-TIMEOUT-MS [: REPOSITORY ;] GT-POOL-START-FORK
   s" lint-tools/status" GSI-FORK-TIMEOUT-MS [: STATUS ;] GT-POOL-START-FORK
   s" lint-tools/dot" GSI-FORK-TIMEOUT-MS [: DOT ;] GT-POOL-START-FORK
   s" lint-tools/nanogpt" GSI-FORK-TIMEOUT-MS [: NANOGPT ;] GT-POOL-START-FORK
   s" lint-tools/maki" GSI-FORK-TIMEOUT-MS [: MAKI ;] GT-POOL-START-FORK
   s" lint-tools/refine" GSI-FORK-TIMEOUT-MS [: REFINE ;] GT-POOL-START-FORK
   s" lint-tools/suite-coverage" GSI-FORK-TIMEOUT-MS [: SUITE-COVERAGE ;] GT-POOL-START-FORK
   s" lint-tools/namespace" GSI-FORK-TIMEOUT-MS [: NAMESPACE ;] GT-POOL-START-FORK
   s" lint-tools/package-diff" GSI-FORK-TIMEOUT-MS [: PACKAGE-OWNERSHIP ;] GT-POOL-START-FORK
   s" lint-tools/error-code" GSI-FORK-TIMEOUT-MS [: ERROR-CODE ;] GT-POOL-START-FORK
   s" lint-tools/host" GSI-FORK-TIMEOUT-MS [: HOST ;] GT-POOL-START-FORK
   s" lint-tools/trusted-inventory" GSI-FORK-TIMEOUT-MS [: TRUSTED-INVENTORY ;] GT-POOL-START-FORK
   s" lint-tools/primitive-effect-inventory" GSI-FORK-TIMEOUT-MS [: PRIMITIVE-EFFECT-INVENTORY ;] GT-POOL-START-FORK
   s" lint-tools/bootstrap-mirror" GSI-FORK-TIMEOUT-MS [: BOOTSTRAP-MIRROR ;] GT-POOL-START-FORK
   s" lint-tools/bootstrap-refresh" GSI-FORK-TIMEOUT-MS [: BOOTSTRAP-REFRESH ;] GT-POOL-START-FORK
   GSI-FORK-DRAIN ;

;package

GATE-LINT-TOOLS:RUN
