---
title: Guard owner namespace writes
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T05:52:27.636663+02:00"
blocks:
  - habu-record-owner-namespace-8ca3b072
  - habu-reject-owner-pkg-75707e19
---

Problem: saved numeric wordlist identifiers can still target an owner package after it closes. Result: reopen package OWNER-GUARD and add `WID? ( -- )`, the exact native builder mirror of XREF-OWNER:WID? with input x9 and boolean output x13. C-PACKAGE calls CHECKER-PACKAGE, whose XREF composition rejects the name before checker or current-wordlist mutation. C-EXPORT uses OWNER-GUARD:WID? before CHECKER-EXPORT. The shared C-STORE-DEF-NAME sink uses it again before every definition and EXPORT record write, covering set-current, qualified, evaluated, JIT, AOT, and saved-WID paths. The original active package can publish before ;package appends the marker, and its already-published public API remains callable. Owner: OWNER-GUARD WID mirror, native EXPORT preflight, and shared name-publication sink only. Production red: save the package private WID, close a directly flagged owner package, set-current to the saved value, and publish a word successfully. Acceptance: XREF-OWNER:WID? and OWNER-GUARD:WID? agree on public, private, unrelated, and rolled-back controls; saved-public-WID, saved-private-WID, and EXPORT attempts reject before dictionary, checker, code, or current-wordlist mutation; every definer reaches the shared sink; unmarked packages and calls through owner public words remain unchanged; source, JIT, AOT, snapshot, and fixpoint tests pass. Forbidden: another classifier, unowned native global, C-PACKAGE name scan, PROT-WID enrollment, OWNER-WID table, copied WID roles, caller allowlist, raw-wordlist effect removal, source scan, runtime wrapper, friend exception, ABI/version change, or lint. Smallest owning check: saved-private-WID definition and EXPORT attempts both fail without mutation while an ordinary package still publishes and an owner public word still executes.
