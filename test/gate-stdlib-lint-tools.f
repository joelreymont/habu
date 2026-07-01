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
   s" tools/filemap-lint.f" GSI-INCLUDE ;

: GSI-LINT-TOOLS-STATUS ( -- )
   s" repl-lint" [: GSI-REPL-LINT ;] GSI-RUN
   s" trust-lint" [: GSI-TRUST-LINT ;] GSI-RUN
   s" stale-status-lint" [: GSI-STALE-STATUS-LINT ;] GSI-RUN
   s" test/gate-stats-test.f" GSI-INCLUDE ;

: GSI-LINT-TOOLS-DOT-MAKI ( -- )
   s" dot-dep-lint" [: DOT-DEP-LINT ;] GSI-RUN
   s" tools/dot-dep-lint-test.f" GSI-INCLUDE
   s" maki-dep-lint" [: MAKI-DEP-LINT ;] GSI-RUN
   s" tools/maki-dep-lint-test.f" GSI-INCLUDE
   s" tools/host-lint-test.f" GSI-INCLUDE ;

: GSI-LINT-TOOLS-BODY ( -- )
   GSI-FORK-RESET
   s" lint-tools/clobber" GSI-FORK-TIMEOUT-MS [: GSI-LINT-TOOLS-CLOBBER ;] GT-POOL-START-FORK
   s" lint-tools/repo" GSI-FORK-TIMEOUT-MS [: GSI-LINT-TOOLS-REPO ;] GT-POOL-START-FORK
   s" lint-tools/status" GSI-FORK-TIMEOUT-MS [: GSI-LINT-TOOLS-STATUS ;] GT-POOL-START-FORK
   s" lint-tools/dot-maki" GSI-FORK-TIMEOUT-MS [: GSI-LINT-TOOLS-DOT-MAKI ;] GT-POOL-START-FORK
   GSI-FORK-DRAIN ;

GSI-LINT-TOOLS-BODY
