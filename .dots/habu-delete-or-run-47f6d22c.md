---
title: Delete or run the tools no test runs
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T20:15:59.605240+02:00"
---

Problem: no kept test loads these tool entries: tools/error-code-lint.f; tools/repl-lint.f; tools/json-only.f; tools/cli-run.f; tools/check-all-errors.f (test/gate-dictionary.f names it: check whether it runs it). Comments and docs name some of them as the check behind a rule (repl-lint: src/lib/line-editor.f:183; check-all-errors: src/core/render.f; error-code-lint: docs/forth-card.md, src/lib/error.f:3, src/lib/codegen.f, src/core/render.f, src/habu/arith-abi.f, src/habu/stack-abi.f, src/lib/source-lex-test.f). No row shares an entry with another row, so test/gate-entry-guard.f's allowance for rows sharing an entry (:8, ROW-B) serves none. The clobber lint stays: dot 0e9d32d8 runs it in a test. Acceptance: each lint is deleted with the files only it requires, or run by a test; each comment or doc line naming a deleted one drops the clause or keeps the rule with the fact that proves it; a tool that is not a lint is deleted when nothing uses it, or kept with the caller named; the shared-entry allowance goes unless a row needs it. Verify: rg for each path finds no remaining mention; the full suite runs. Depends: none. Claim: unassigned.
