---
title: Remove DRAIN-PRETRUST-COMPAT transition shim
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-19T16:58:11.841619+02:00\""
---

Discharge the last invalid-owner trusted-inventory row (strict mode FAILS: DRAIN-PRETRUST-COMPAT at src/core/checker.f:8060 is owned by closed dot habu-checker-exec-of-5923c543). The shim resolved the DRAIN-PRETRUST prim by runtime search-wl so PREVIOUS-fixpoint engines (lacking the prim) could still load the boot prefix; its removal condition — the first stage-2b conversion landing — is already met: commit 563b2540 added five real TFAM pre-trust defers, so an old engine loading this tree dies fail-closed at exit 73 via the BSEALCAP backstop regardless of the shim (test/pre-trust-defer.f pins exactly that), and every engine since carries the prim. The shim therefore serves no engine and its stale comment ('this tree declares no pre-trust defers') is false. Fix: replace the TRUSTED: DRAIN-PRETRUST-COMPAT shim with the bare DRAIN-PRETRUST token, delete the shim word, its TRUSTED.md manifest row (~line 272), and update the surrounding comment block; keep the PTD-REGRESSION-BLANK sentinels and adapt test/pre-trust-defer.f so the blank-region negative regression still proves the exit-73 undrained backstop with the bare token; trusted-inventory strict must exit 0 with zero invalid owners. Seed-affecting: fixpoint install --force x2 byte-identical plus the full battery. Verify with a previous-fixpoint engine if one is preserved, else document that old-engine loading is already impossible (exit 73) by construction.
Claim: agent=pretrust-shim workspace=.jj-ws/habu-remove-drain-pretrust-509d64a0
