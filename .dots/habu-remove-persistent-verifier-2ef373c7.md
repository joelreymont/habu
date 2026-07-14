---
title: Remove persistent verifier commit authority
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-14T19:12:02.074952+02:00\""
blocks:
  - habu-honor-set-curr-fbd17193
---

Context: src/habu/verify-source.f public SOURCE-BUF-IN-SCOPE/SOURCE-BUF-AT-IN-SCOPE commit checker mutations outside rollback. Exact exploit: s" TRUSTED: dup ( n -- ) drop ;" VERIFY:SOURCE-BUF-IN-SCOPE then : FORGE-DUP ( n -- ) dup ; certifies and runs with forged dup effect. Fix: remove public persistent entrypoints; make check/check-all-errors assemble support plus target inside one candidate transaction with unconditional rollback and non-persistent source-at diagnostics. Acceptance: exact stdin/load forged-dup negatives; candidate success and throw leave all checker/verifier state unchanged; check gates pass.
