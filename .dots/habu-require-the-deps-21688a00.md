---
title: Require the dependencies the 29 remaining Load-after tools files name
status: open
priority: 3
issue-type: task
created-at: "2026-09-22T08:00:53.009891+03:00"
---

Problem (measured by the tools-requires lane, landed 2e7150d4): rg -n 'Load after' tools --glob '*.f' still finds 29 files: aot-lint-core.f, build-fixpoint.f (by design: caller-composed --load list), build-fixpoint-main.f, bundle-lib.f, check-all-errors.f, checked-boundary-lint.f, check.f, codegen-role.f, dot-dep-lint-core.f, dot-dep-lint.f, dot-dep-lint-test.f, error-code-lint.f, error-code-lint-test.f, error-code-region-test.f, event-closure-lib.f, gate-json-assert.f, hb-build-direct-lints.f, hb-build-report.f, json.f, json-file.f, lint/clobber-lint-test.f, manifest-lint.f, manifest-lint-test.f, public-signatures.f, repair-packet-core.f, repl-lint-core.f, repl-lint.f, seed.f, seed-main.f. A header naming a load list the file does not require makes every loader carry the preamble (the defect d1d54d51 fixed for eleven files). Acceptance: each file either requires what its header names, in the card's order, loads standalone rc 0 and loses the header, or keeps the header with a one-line by-design reason (an entry whose --load list a caller composes, like build-fixpoint.f); rg 'Load after' tools then lists only the by-design files; test/run.f green. Files: the 29. Depends: none. Ownership: hazel (tools load path). Claim: unassigned.
