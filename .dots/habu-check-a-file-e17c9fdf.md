---
title: Check a file whose root ROOT! sets
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T06:27:15.388423+01:00"
---

Problem: tools/check.f rejects a require that resolves under a root the file set with SOURCE-ROOT:ROOT!, which the loader accepts (docs/forth.md:296-300). Reproducer: /tmp/carl-spark/root-repro, where other/b.f defines RRB:B and lit.f is 's" other" SOURCE-ROOT:ROOT!' / 'require b.f' / 'RRB:B .'; 'hb --load lit.f' prints 2 rc 0, while 'hb --load tools/check.f -- --json-errors lit.f' gives E-MISSING-SOURCE at lit.f:2:1. word.f computes the same path in a word and fails alike. Zen's src files set ROOT! from SOURCE-ROOT:DIRNAME, and Tender's from ZEN-ROOT:ROOT$ (src/office-source.f:3), so check.f and the LSP reject most of both trees. Cause: check.f runs no top-level code before its run stage, so ROOT! never moves the current root: discovery's SD-STEP (tools/source-discovery.f:1274-1348) has no case for ROOT! or WITH, and the verifier's composition leaves top-level tokens unrun (src/habu/verify-source.f:163-166); both resolve through the loader's own SOURCE-ROOT resolution (src/core/include.f:1121-1160) under the file's directory. The LSP and tools/public-signatures.f share the miss; hb-build does not, since its loader runs the top level. A computed root needs code run: Tender's from an environment variable (src/zen-root.f:20-36), Zen's from ACTIVE-SOURCE and DIRNAME. Acceptance: check.f and the LSP resolve every require as the load does, under the root ROOT! or SOURCE-ROOT:WITH made current, for a literal and a computed path; Tender's src/admission.f and Zen's src/document/read.f reach their verdict; a test checks the reproducer. Depends: none. Claim: unassigned.
