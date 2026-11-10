---
title: Model a wordlist the source protects in the source verifier
status: closed
priority: 2
issue-type: task
created-at: "2026-11-01T04:49:53.104065+02:00"
closed-at: "2026-11-10T21:50:20.856837+02:00"
close-reason: merged into habu-check-src-through-1198f4ed
---

Problem: check.f --verify-only admits a definition into a wordlist the source protects as it loads, which --load refuses with exit 84. Probes /tmp/jerry-pkg2a-rr2-tmp/probe/: prot-def.f (`package PRB-P public : A ( -- n ) 1 ; get-current prot-wid-add ;package : PRB-P:B ( -- n ) 2 ;`: --load exits 84 'hb: cannot publish into protected word: PRB-P:B'; plain check and --all-errors the same; --verify-only exits 0, no output), prot-reopen.f (reopening a protected package), prot-cur.f (an unqualified definition after get-current prot-wid-add inside the package). Cause: src/habu/verify-source.f's scan runs no top-level code, so SEALED-NAME and SEALED-PACKAGE see only the engine's sealed wordlists. docs/forth.md 'Comments & hygiene' says --verify-only reports every refused definition. Acceptance: the scan models a top-level `get-current prot-wid-add` (or stops checking what follows it, as after a parsing word), and the three probes become rows in tools/check-verify-test-lib.f that fail before. Related: habu-check-top-level-045ef3a9 (--verify-only checks no top-level statement). Files: src/habu/verify-source.f, tools/check-verify-test-lib.f. Verify: rows check-verify and the package-seal rows. Depends: nested-package feature landed (habu-read-a-pkg-701d8b54). Claim: unassigned.

Census 2026-11-04: a disagreement between two implementations of one thing; it is closed by its parent removing the second implementation, not by patching that copy.
