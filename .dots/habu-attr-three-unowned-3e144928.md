---
title: Attribute three unowned gate-stdlib reds
status: open
priority: 2
issue-type: task
created-at: "\"2026-07-29T20:36:18.188769+02:00\""
---

Full context: measured 2026-07-29 on the proofs base, before and after an unrelated engine change, so none is owned by the snapshot incident: pre-trust-defer asserts exit 73 and gets 70 (matches the Gforth-mirror replay defect described in docs/debugging.md); aot-wid-restore asserts 0 and gets 67 three times; stdlib-process-fixtures fails a boolean assertion with no exit-code evidence captured. (A fourth, engine-error-package expecting 70 getting 67, is already owned by habu-restore-fail-closed-4f1d6375.) These were hiding behind the snapshot reds in the pool ordering. Root-cause and attribute each separately to an owner or a new dot; do not batch-fix.

Claim: agent=attrthree workspace=.jj-ws/habu-attr-three-unowned-3e144928 (RELEASED 2026-08-21: workspace gone, no live lane - gc)

Outcome 2026-07-29, all three attributed with reproduced evidence:

1. pre-trust-defer (asserts 73, gets 70) -> new dot habu-reprove-the-undrained-16d55d36. The Gforth mirror attribution is REFUTED: the whole failure reproduces under the native engine on the ordinary `bin/hb --load` child-boot path with no gforth and no tools/bootstrap.sh. With the drain blanked the first checked `is` on a pre-trust deferred word - `: INSTALL ( -- ) [: LIVE ;] is PKG-LIVE-XT ;` at src/habu/xref.f:207-209, whose deferred word is declared at src/core/checker.f:465 before `: TRUST` at src/core/checker.f:8671 - fails the check hook (src/core/check-hook.f:34) and exits 70, 283 lines before the baseline SEAL-CAPTURE token at src/habu/xref.f:492 can fire the exit-73 backstop. Blanking that one `is` as well restores exit 73 naming TFAM-RESOLVE-XT, so the backstop itself is intact. Introduced by commit e8c27f225303 "Harden package authority".

2. aot-wid-restore (asserts 0, gets 67 three times) -> existing dot habu-model-bare-wordlists-9e7c3521, evidence appended there rather than minting a duplicate. The three assertions are numbers 25, 27 and 29, all the `RC @ 0 T=` leg of ASSERT-OK; the escaping throw is pinned by mutation to src/core/checker.f:703 (was 634) in CHECKER-PKG-CONTEXT, reached because LIVE-PKG (src/habu/xref.f:171-199) rejects a bare user word-list as a package context.

3. stdlib-process-fixtures (boolean assertion, no exit-code evidence) -> new dot habu-budget-the-standalone-92d730f2. Evidence captured: the only red file of the six is test/lint-cli-standalone-load.f, assertion 18, label tools/refine-lint.f, which is the `EXITED @ TTRUE` leg at line 124 - the child hit the 20000 ms TIMEOUT-MS at line 41. `bin/hb --load tools/refine-lint.f` succeeds but took 65.7 s, 30.1 s and 16.5 s on three isolated runs; raising TIMEOUT-MS to 120000 in a scratch copy and changing nothing else makes the file green.
