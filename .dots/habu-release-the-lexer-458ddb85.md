---
title: "Release the lexer's tables before an image save"
status: active
priority: 1
issue-type: task
created-at: "2026-11-05T13:33:02.599922+02:00"
---

Problem (reported by tim from Maki's vocabulary lane): LINT-LEX (src/lib/source-lex.f) maps its 11 token-table VEC headers (:40-54) once (RESET-TABLES :111-114). A program that runs LINT-LEX:SOURCE and then saves an image (Maki's build runs WORD-DOCS:FILE, which lexes) keeps CAP, TOK-N and the headers' addresses but not the mappings: in the restarted image LINT-LEX:COUNT prints the stale count (105) and the next SOURCE dies SIGSEGV, rc 139. Cause: LINT-LEX never registers its tables with IMAGE-LIFECYCLE:REGISTER, the tree's rule for process-local state (src/lib/image-lifecycle.f :49-52; font.f and f64-text.f REGISTER-CLEANUP). Acceptance: INIT-VECTORS registers a one-shot hook that disposes the tables and clears the scan state; a new end-to-end test/source-lex-image.f (SUITE source-lex-image) saves an image after a SOURCE, then shows COUNT 0 and a working SOURCE in the restarted image and in the live process after the save. No new public word. Files: src/lib/source-lex.f, test/source-lex-image.f, test/gate-stdlib-cases.f. Verify: bin/hb --load test/run.f -- source-lex-image; src/lib/source-lex-test.f; src/lib/word-docs-test.f. Brief: .jj-ws.noindex/carl-dots/.out/carl-file/brief-lex-save.md. Depends: none. Ownership: carl. Claim: carl-lex. Tell tim the Habu revision when it lands.
