\ gate-stdlib-lint-tools.f - in-process lint-tools body.
\
\ Load after GSI-LINT-TOOLS-SETUP.

: GSI-REPL-LINT ( -- )
   s" ." REPL-ROOT!
   REPL-LINT ;

: GSI-TRUST-LINT ( -- )
   GSI-TL-STR-BUF GSI-TL-STR-CAP
   GSI-TL-FILE-BUF GSI-TL-FILE-CAP
   TRUST-LINT-BUFFERS!
   TL-TRUE TL-REPORT-SUCCESS!
   s" ." TRUST-LINT-ROOT!
   TRUST-LINT-TODAY-NOW
   TRUST-LINT ;

: GSI-STALE-STATUS-LINT ( -- )
   s" ." SS-ROOT!
   epoch-seconds DATE-SECONDS-DAY / SS-TODAY-DAYS !
   STALE-STATUS-LINT ;

: GSI-LINT-TOOLS-CLOBBER ( -- )
   s" tools/lint/clobber-lint.f" GSI-REQUIRE
   s" tools/lint/clobber-lint-test.f" GSI-INCLUDE ;

: GSI-LINT-TOOLS-REPO ( -- )
   s" tools/lint/shadow-lint.f" GSI-INCLUDE
   s" tools/host-lint.f" GSI-REQUIRE
   s" tools/parallel-agent-lint.f" GSI-INCLUDE
   s" tools/filemap-lint.f" GSI-REQUIRE
   s" tools/filemap-lint-test.f" GSI-INCLUDE
   s" tools/stdin-closure-lint.f" GSI-REQUIRE ;

: GSI-LINT-TOOLS-STATUS ( -- )
   s" repl-lint" [: GSI-REPL-LINT ;] GSI-RUN
   s" trust-lint" [: GSI-TRUST-LINT ;] GSI-RUN
   s" stale-status-lint" [: GSI-STALE-STATUS-LINT ;] GSI-RUN
   s" test/gate-stats-test.f" GSI-INCLUDE ;

\ One fork per sub-suite so GT-POOL-FAIL's `FAIL: <label>` line names the
\ failing sub-suite directly. The old lint-tools/dot-maki bundled dot, maki,
\ maki-ns, host, and trusted-inventory in a single fork; a test file's
\ T-REPORT `die` exits the fork and bypasses GSI-INCLUDE's per-file FAIL line,
\ so a trusted-inventory ratchet failure surfaced only under the misleading
\ dot-maki label. Setup is loaded once in the parent and inherited copy-on-
\ write by every fork, so the split adds no setup cost.
: GSI-LINT-TOOLS-DOT ( -- )
   s" dot-dep-lint" [: DOT-DEP-LINT ;] GSI-RUN
   s" tools/dot-dep-lint-test.f" GSI-INCLUDE ;

: GSI-LINT-TOOLS-MAKI ( -- )
   s" maki-dep-lint" [: MAKI-DEP-LINT ;] GSI-RUN
   s" tools/maki-dep-lint-test.f" GSI-INCLUDE ;

: GSI-LINT-TOOLS-HOST ( -- )
   s" tools/host-lint-test.f" GSI-INCLUDE ;

: GSI-LINT-TOOLS-TRUSTED-INVENTORY ( -- )
   s" tools/trusted-inventory-test.f" GSI-INCLUDE ;

: GSI-LINT-TOOLS-BOOTSTRAP-MIRROR ( -- )
   s" tools/bootstrap-mirror-lint-test.f" GSI-INCLUDE ;

: GSI-LINT-TOOLS-BODY ( -- )
   GSI-FORK-RESET
   s" lint-tools/clobber" GSI-FORK-TIMEOUT-MS [: GSI-LINT-TOOLS-CLOBBER ;] GT-POOL-START-FORK
   s" lint-tools/repo" GSI-FORK-TIMEOUT-MS [: GSI-LINT-TOOLS-REPO ;] GT-POOL-START-FORK
   s" lint-tools/status" GSI-FORK-TIMEOUT-MS [: GSI-LINT-TOOLS-STATUS ;] GT-POOL-START-FORK
   s" lint-tools/dot" GSI-FORK-TIMEOUT-MS [: GSI-LINT-TOOLS-DOT ;] GT-POOL-START-FORK
   s" lint-tools/maki" GSI-FORK-TIMEOUT-MS [: GSI-LINT-TOOLS-MAKI ;] GT-POOL-START-FORK
   s" lint-tools/host" GSI-FORK-TIMEOUT-MS [: GSI-LINT-TOOLS-HOST ;] GT-POOL-START-FORK
   s" lint-tools/trusted-inventory" GSI-FORK-TIMEOUT-MS [: GSI-LINT-TOOLS-TRUSTED-INVENTORY ;] GT-POOL-START-FORK
   s" lint-tools/bootstrap-mirror" GSI-FORK-TIMEOUT-MS [: GSI-LINT-TOOLS-BOOTSTRAP-MIRROR ;] GT-POOL-START-FORK
   GSI-FORK-DRAIN ;

GSI-LINT-TOOLS-BODY
