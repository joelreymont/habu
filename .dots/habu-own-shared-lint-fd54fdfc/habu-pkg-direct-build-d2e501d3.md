---
title: Package direct build lint hooks
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T17:32:17.036300+02:00"
---

Files: tools/hb-build-direct-lints.f only. Add package HB-BUILD-DIRECT-LINTS, keep all state and helper words private, rename HBB-INSTALL-DIRECT-LINTS to INSTALL, and invoke INSTALL inside the package so the existing hook installation side effect is unchanged. Continue calling the still-global signature and AOT lint core APIs in this leaf. Acceptance: no HBB-* definition or storage remains global; both direct lint hooks install exactly once and propagate failures; no alias or swallowed error. Verify: the hb-build direct-lint focused test, signature-lint-test.f, aot-lint-test.f, typed-local-diff-lint, package-diff focused mutation, host-lint, filemap-lint.
