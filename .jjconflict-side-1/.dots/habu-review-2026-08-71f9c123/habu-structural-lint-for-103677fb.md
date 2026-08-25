---
title: structural lint for unmirrored engine seams
status: open
priority: 1
issue-type: task
created-at: "2026-08-22T22:38:25.806298+02:00"
---

Problem: the mirror bootstrap/cg/forth.fs missed LASTC-TRUST:PUBLISH-PTR-A/PUBLISH-A (create/variable/constant) and published does> effects through trust-decl instead of trust-raw; tools/bootstrap-mirror-lint.f only polices ADT keywords in src/, tools/bootstrap-codegen-test.f only capacities. A native seam can land without its mirror half and nothing refuses it. Acceptance: a bootstrap-codegen-test invariant, read through the real lexer (tools/lint/source-lex.f), that every LASTC-TRUST:PUBLISH*, trust-raw, trust-decl, C-DEFHOOK and check-hook call site in src/habu/habu2.f has a named mirror counterpart in forth.fs (a table of pairs, not substrings); a hostile fixture with the native site only reds it; the two historical misses are its first two rows. Files: tools/bootstrap-codegen-test.f, tools/lint/source-lex.f. Verify: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f tools/bootstrap-codegen-test.f exit 0; the fixture reds. Depends: 9269e3a3 (so the table starts complete). Ownership: bootstrap tripwires. Claim: unassigned.
