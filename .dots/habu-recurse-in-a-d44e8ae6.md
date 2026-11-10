---
title: Recurse in a does> clause asks for an effect it cannot declare
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T11:10:14.964879+02:00"
---

Problem: `recurse` inside a does> clause is refused as uncheckable with the reason `recursion needs a declared stack effect` (added by habu-tokens-refused-as-e0ec22ab), but a clause has no signature of its own to declare: CHECK-DOES-RUN never calls SIG-EFF-CACHE!. Case /Users/joel/Work/reviews/habu-10x-2026-11-10/refusals/unck/recurse-does.f. Not determined: what the engine compiles for RECURSE in a clause (the clause, or the defining word). Found by the uncheckable-reason worker. Source: Joel, 2026-11-10: "you must dot and launch a fix automatically for any issues you identify". Acceptance: a root-cause hunt states what RECURSE in a clause means at run time; then either the checker checks it against the clause's effect, or the refusal carries its own reason that tells the writer what to do instead. Files: src/core/checker.f (CF-RECURSE, CHECK-DOES-RUN), src/core/render.f. Verify: recurse-does.f on a private engine. Depends: habu-tokens-refused-as-e0ec22ab landed. Ownership: the checker's does> clause path. Claim: see the hunt result below.

Hunt result 2026-11-10 (bob): all three compilers compile RECURSE in a does> clause as a call to the defining word (src/compiler/t0-emit.f:175-178, src/compiler/emit.f:1474-1477, src/habu/habu2.f:3286-3289), and the checker has no effect cached there (CHECK-RESET, src/core/checker.f:25688). Settled: keep the refusal, with its own reason: "recurse in a does> clause would call the defining word; move the recursion into a named word with a declared effect and call it from the clause". The workaround loads: /Users/joel/Work/reviews/habu-10x-2026-11-10/recurse/t8.f. The hunt's workspace is retired; the fix waits for its Depends. Claim: unassigned.

Claim: agent=bob workspace=.jj-ws.noindex/habu-recurse-in-a-d44e8ae6
