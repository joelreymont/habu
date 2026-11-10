---
title: "A refusal's verdict is decided in two places"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T12:07:44.382454+02:00"
---

Problem: the load's verdict reads the UNCK flag (CHECK-VERDICT, src/core/checker.f:26197-26198) while the packet's verdict goes through REFUSAL-UNCHECKABLE? and UNCK-REASON? (src/core/render.f:1349-1350, checker.f:21203-21205), so a reason latched beside UNCK prints "rejected" in the packet and stays uncheckable in the load unless UNCK-REASON? lists it. Each new uncheckable reason must be added there, and only a packet test catches a miss. Found by the uncheckable-reason re-review. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: one word answers whether a refusal is uncheckable, and the load and the packet both ask it; a reason latched beside UNCK needs no second list. Files: src/core/checker.f, src/core/render.f. Verify: test/signature-nesting.f's two packet cases and the suite. Depends: habu-tokens-refused-as-e0ec22ab landed. Ownership: the checker's diagnostics. Claim: unassigned.

Cases 2026-11-10 (bob): CHECK-CANDIDATE! answers 1 (uncheckable) for a text whose name is undefined while its packet says rejected; CHECK-VERDICT puts UNSEEN ahead of OK, so a mismatch followed by a name a rendering statement may define answers deferred rather than rejected (first-failure worker; a reviewer is writing the case).
