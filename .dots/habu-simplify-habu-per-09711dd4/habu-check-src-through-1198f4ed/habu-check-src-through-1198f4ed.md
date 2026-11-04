---
title: Check source through the interpreter
status: open
priority: 2
issue-type: task
created-at: "2026-10-25T22:57:08.860079+02:00"
---

Rule: docs/architecture.md:271-273, a tool that reports on source runs a child hb that loads the subject under hooks and writes one JSON line per fact; :289-290, ENUM and the sum-type definers run evaluate and STRUCTURE reads ahead, so a scanner cannot see their effects. Problem: src/habu/verify-source.f (about 4700 lines, header line 3 'scanner') re-implements the front end: its own reader (SCAN-C@ ~255, SCAN-SIG ~583), a copy of the definer table (DEFINER-FIND..DEFINER-EFFECT ~1441-1531), copies of the definers (RECORD-SUMTYPE ~2592, RECORD-ENUM ~2628, RECORD-STRUCTURE ~3569, VERIFY-DEFINITION ~3857). Callers: tools/check-core.f, check-verify-core.f, check-all-errors-core.f, lsp-source-index.f, lsp-references.f. Acceptance: check and the LSP index load the subject in a check-mode child hb under hooks; verify-source.f's scanner is deleted; every refusal the scanner reported is still reported, located. Related: habu-delete-the-trusted-42b30edd and habu-sweep-trusted-out-41e973ce name verify-source words. Verify: check and LSP suites, test/run.f.
