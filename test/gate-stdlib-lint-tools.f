\ gate-stdlib-lint-tools.f - in-process lint-tools body.
\
\ Load after GSI-LINT-TOOLS-SETUP.

require lib/vector.f
require lib/fs-mutate.f

package GATE-LINT-TOOLS
private

\ This file is an in-process gate BODY, not a standalone entry point. Its header says
\ "Load after GSI-LINT-TOOLS-SETUP", and GSI-LINT-TOOLS in
\ test/gate-stdlib-inline-lib.f is what runs that setup and then includes this file.
\ The setup is what supplies the lint cores and the GSI-* harness words the bodies
\ below compile against, so there is nothing this file could require to stand alone.
\
\ Loading it directly used to die with an opaque E-UNDEFINED naming whichever harness
\ word happened to be reached first, which reads like a missing require rather than a
\ misuse of the file. Refuse loudly instead, naming the harness that has to run first,
\ so the diagnostic points at the caller rather than at an innocent-looking word.
9002 constant E-GLT-NO-HARNESS          \ harness bound, not a product error code

: HARNESS-READY? ( -- bool )            \ has GSI-LINT-TOOLS-SETUP been loaded?
   s" GSI-LINT-TOOLS-SETUP" 0 search-wl 0 <> ;

: REQUIRE-HARNESS ( -- )
   HARNESS-READY? if exit then
   s" test/gate-stdlib-lint-tools.f is an in-process gate body, not a standalone entry."
      type cr
   s" Run it through GSI-LINT-TOOLS (test/gate-stdlib-inline-lib.f), which calls"
      type cr
   s" GSI-LINT-TOOLS-SETUP first and then includes this file."
      type cr
   E-GLT-NO-HARNESS throw ;

REQUIRE-HARNESS

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
   s" ." STALE-STATUS-LINT:ROOT!
   STALE-STATUS-LINT:RUN 0 > IF 1 throw THEN ;

: CLOBBER ( -- )
   s" tools/lint/clobber-lint.f" GSI-REQUIRE
   s" tools/lint/clobber-lint-test.f" GSI-INCLUDE ;

: REPOSITORY ( -- )
   s" tools/lint/shadow-lint.f" GSI-INCLUDE
   \ The tool is REQUIRED, not INCLUDED, because its test file requires it too:
   \ GSI-INCLUDE loads through `included`, which never registers the file, so an
   \ INCLUDE here would load the tool a second time from the test's own `require`
   \ and redefine PE-FB-CAP in the shared image. This matches CLOBBER above.
   s" tools/lint/ptx-emitter-lint.f" GSI-REQUIRE
   s" tools/lint/ptx-emitter-lint-test.f" GSI-INCLUDE
   s" tools/host-lint.f" GSI-REQUIRE
   s" tools/process-primitive-lint.f" GSI-INCLUDE
   s" tools/process-primitive-lint-test.f" GSI-INCLUDE
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
   s" refine-lint" [: RFL:RUN ;] GSI-RUN
   s" tools/refine-lint-test.f" GSI-INCLUDE ;

: LINT-DEF ( -- )
   s" tools/lint/def-test.f" GSI-INCLUDE ;

: SUITE-COVERAGE ( -- )
   s" suite-coverage-lint" [: SUITE-COVERAGE-LINT:RUN ;] GSI-RUN
   s" tools/suite-coverage-lint-test.f" GSI-INCLUDE ;

: NAMESPACE ( -- )
   s" namespace-lint" [: NAMESPACE-LINT-STRICT ;] GSI-RUN
   s" tools/namespace-lint-test.f" GSI-INCLUDE ;

: PACKAGE-OWNERSHIP ( -- )
   s" tools/package-diff-lint-test.f" GSI-INCLUDE ;

\ Design section 6.6's renderer-output fence. Loading the CLI file IS the
\ enforcing run over the live compiler tree - it throws on any finding - so this
\ group needs no entry in the shared lint-core preload, and the fixtures that
\ prove the fence bites follow it.
: RENDER-PARSE ( -- )
   s" tools/render-parse-lint.f" GSI-REQUIRE
   s" tools/render-parse-lint-test.f" GSI-INCLUDE ;

: ERROR-CODE ( -- )
   s" error-code-lint" [: ERROR-CODE-LINT:STRICT ;] GSI-RUN
   s" tools/error-code-lint-test.f" GSI-INCLUDE
   s" tools/error-code-region-test.f" GSI-INCLUDE ;

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
   s" lint-tools/def" GSI-FORK-TIMEOUT-MS [: LINT-DEF ;] GT-POOL-START-FORK
   s" lint-tools/suite-coverage" GSI-FORK-TIMEOUT-MS [: SUITE-COVERAGE ;] GT-POOL-START-FORK
   s" lint-tools/namespace" GSI-FORK-TIMEOUT-MS [: NAMESPACE ;] GT-POOL-START-FORK
   s" lint-tools/package-diff" GSI-FORK-TIMEOUT-MS [: PACKAGE-OWNERSHIP ;] GT-POOL-START-FORK
   s" lint-tools/error-code" GSI-FORK-TIMEOUT-MS [: ERROR-CODE ;] GT-POOL-START-FORK
   s" lint-tools/render-parse" GSI-FORK-TIMEOUT-MS [: RENDER-PARSE ;] GT-POOL-START-FORK
   s" lint-tools/host" GSI-FORK-TIMEOUT-MS [: HOST ;] GT-POOL-START-FORK
   s" lint-tools/trusted-inventory" GSI-FORK-TIMEOUT-MS [: TRUSTED-INVENTORY ;] GT-POOL-START-FORK
   s" lint-tools/primitive-effect-inventory" GSI-FORK-TIMEOUT-MS [: PRIMITIVE-EFFECT-INVENTORY ;] GT-POOL-START-FORK
   s" lint-tools/bootstrap-mirror" GSI-FORK-TIMEOUT-MS [: BOOTSTRAP-MIRROR ;] GT-POOL-START-FORK
   s" lint-tools/bootstrap-refresh" GSI-FORK-TIMEOUT-MS [: BOOTSTRAP-REFRESH ;] GT-POOL-START-FORK
   GSI-FORK-DRAIN ;

;package

GATE-LINT-TOOLS:RUN
