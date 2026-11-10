---
title: Tokens refused as uncheckable with no reason
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T10:40:13.023499+02:00"
---

Problem: a token whose effect the checker does not know sets UNCK alone at src/core/checker.f:19877, :23567, :23673, :23729, :24028 and :21248, so the refusal is E-UNCHECKABLE with only `at 'TOKEN'` and the fixed suggestion. :22517 shows the form that carries a reason (an MD-* reason latched beside UNCK). Source: the root-cause hunt of habu-checker-refusals-that-3152182a, 2026-11-10; Joel, 2026-11-10: "you must dot and launch a fix automatically for any issues you identify". Acceptance: first a census, with no code change: for each site a smallest program that reaches it, the line it prints today, and the reason the checker holds there; sites no program reaches are named as such. Then each reachable site prints its reason. Files: src/core/checker.f, src/core/render.f. Verify: the census cases. Depends: none. Ownership: the checker's uncheckable refusals. Claim: agent=bob workspace=.jj-ws.noindex/habu-tokens-refused-as-e0ec22ab

Census, 2026-11-10 (cases: ~/Work/reviews/habu-10x-2026-11-10/refusals/unck/): two sites refuse with no reason, CF-PUSH (:21009, 33 nested control frames) and CF-RECURSE (:21248, RECURSE with no declared effect). :19850 and :23729 print E-UNDEFINED; :19877, :23567 and :23673 are reached only under tools/check-verify-child.f and defer; :24028 passes on another site's flag; :22517, :26166 and :26169 latch a reason. Fix: each of the two latches an MD-* reason beside UNCK: `control flow nested too deep to check (depth <n>, at most 32)` and `recursion needs a declared stack effect`.
