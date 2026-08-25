---
title: 143 error codes no test provokes
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:47:07.052893+02:00"
---

Problem: 1184 negative E- claims in lib/errors.f and owners; 143 (excluding -FIRST/-LAST sentinels) appear in no file under test/, maki/test* or *-test.f - 120 of them declared in lib/errors.f: E-FFI-DLSYM (:157), E-FFI-SYNTAX (:156), E-ENGINE-PATH (:179), E-DIAG-SCHEMA (:89), E-DIAG-ORIGIN (:91), E-CODEGEN-CLANG-TOOL (:712), and the E-A64RAV-*, E-A64SPILL-*, E-IR-VERIFY-*, E-JUDGE-*, E-NLOOP-*, E-NMIGRATE-*, E-TASK-*, E-ZED-* families; tools/error-code-lint-core.f checks uniqueness only. Acceptance: error-code-lint gains a reachability column (a claimed code must be provoked by at least one scheduled test, read through the lexer), with a documented retirement path for codes nothing can raise; the 143 either get a negative test or are retired. Files: tools/error-code-lint-core.f, lib/errors.f, tests. Verify: the lint 0 findings. Depends: habu-regalloc-verifier-refusals-73345b50 covers the A64RAV family. Ownership: error codes. Claim: unassigned.
