---
title: "A load's prose refusal prints no suggestion"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T14:16:42.377615+02:00"
---

Problem: src/core/render.f DIAG-PROSE (:1670) and the other prose writers print no suggestion for any refusal; only the JSON writers do. Since habu-both-recurse-refusals-7786792b moved the fix out of the does> recurse reason into its suggestion, a --load reader of that refusal sees no fix. Found by that lane's review (claim 6). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: a prose refusal that has a suggestion prints it after its reason, or the decision that prose carries none is recorded in docs/forth.md beside the packet's fields; a case pins it. Files: src/core/render.f, test/signature-nesting.f. Verify: the does> recurse program under --load. Depends: habu-both-recurse-refusals-7786792b landed. Ownership: the checker's diagnostics. Claim: unassigned.
