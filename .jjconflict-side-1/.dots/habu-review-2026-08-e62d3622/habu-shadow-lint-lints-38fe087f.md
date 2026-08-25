---
title: shadow-lint lints a hand list of 35 files
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:47:07.160283+02:00"
---

Problem: tools/lint/shadow-lint.f:160-193 lists 35 of 144 src/ files, omitting sumtype.f, enum-decl.f, structure-decl.f, decl-event.f, all of src/compiler/, src/habu/aot-*.f, driver-io.f, stdin.f; nothing asserts the list against tools/boot-pin.f BP-EACH or bootstrap.sh SRC_COMMON; the prim pass uses TOKENIZE + 'FPRIM' prefix (:59-67). Acceptance: the file set derived from BP-EACH plus SRC_COMMON (one authority); the prim pass through LINT-LEX string payloads (source-lex.f:484-522 CONTENT); a test that adds a file to the prefix and sees the lint follow. Files: tools/lint/shadow-lint.f, shadow-lint-test.f. Verify: the test. Depends: none. Ownership: lints. Claim: unassigned.
