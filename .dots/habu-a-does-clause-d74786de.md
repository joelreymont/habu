---
title: "A does> clause's packet renders the live rows"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T11:10:15.010107+02:00"
---

Problem: habu-render-the-declared-f4f40bb8 renders declared_effect from the record SIG-EFF-CACHE! builds, but a does> clause's packet (DOES-REPORT, src/core/checker.f near :27947) still renders the live rows, the record being gone by then; so a clause refusal can report the body's view of the stack as the declared effect. Not measured: no case run yet. Found by that lane's worker. Source: Joel, 2026-11-10: "you must dot and launch a fix automatically for any issues you identify". Acceptance: a refused does> clause whose body reaches under its inputs reports declared_effect as written; a case shows the defect on the base first. Files: src/core/checker.f (DOES-REPORT), src/core/render.f (DECL-EFFECT). Verify: the case's packet on a private engine; a row in test/compiler/input-underflow-refusals.f. Depends: habu-render-the-declared-f4f40bb8 landed. Ownership: the checker's diagnostics. Claim: unassigned.

Claim: agent=bob workspace=.jj-ws.noindex/habu-a-does-clause-d74786de
