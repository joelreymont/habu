---
title: "A using refusal's ncomp line names no token"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T10:40:13.002749+02:00"
---

Problem: a bare name two used packages export, or one a used public shadows a global for, is refused by the checker with its own line (`E-USING-AMBIGUOUS habu: bare 'W' is ambiguous under using…`) and a throw (USING-RAISE, src/core/checker.f:14698). The throw skips the hook line (src/core/check-hook.f:236); the native compiler's SETTLE-UNIT (src/compiler/native/definition.f:519) then sees a code other than RC-REJECT, and REPORT-REFUSAL (:492) prints the compiler's own empty token: `ncomp: cannot compile X` with no `at`. Cases /Users/joel/Work/reviews/habu-10x-2026-11-10/refusals/ncomp/b5.f and b6.f, rc 70. In the transcripts 132 refusal episodes show this bare line; the share that are using refusals is not measured. Source: the root-cause hunt of habu-checker-refusals-that-3152182a, 2026-11-10; Joel, 2026-11-10: "you must dot and launch a fix automatically for any issues you identify". Acceptance: for a code the checker reported itself (REPORTED-ERROR?, src/core/checker.f:26310) the ncomp line names the token the checker refused, in the line's existing form `ncomp: cannot compile X at TOKEN in FILE:LINE`. test/compiler/native-hookless-reject.f:52 pins the token; tools/load-argv-test.f:164 is unchanged. Files: src/compiler/native/definition.f, test/compiler/native-hookless-reject.f. Verify: b5.f and b6.f name the token; that test, on a private engine. Depends: none. Ownership: the native compiler's refusal report. Claim: agent=bob workspace=.jj-ws.noindex/habu-a-using-refusal-3abb8838
