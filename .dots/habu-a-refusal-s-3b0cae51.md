---
title: "A refusal's verdict is decided in two places"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T12:07:44.382454+02:00"
---

Problem: the load's verdict reads the UNCK flag (CHECK-VERDICT, src/core/checker.f:26197-26198) while the packet's verdict goes through REFUSAL-UNCHECKABLE? and UNCK-REASON? (src/core/render.f:1349-1350, checker.f:21203-21205), so a reason latched beside UNCK prints "rejected" in the packet and stays uncheckable in the load unless UNCK-REASON? lists it. Each new uncheckable reason must be added there, and only a packet test catches a miss. Found by the uncheckable-reason re-review. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: one word answers whether a refusal is uncheckable, and the load and the packet both ask it; a reason latched beside UNCK needs no second list. Files: src/core/checker.f, src/core/render.f. Verify: test/signature-nesting.f's two packet cases and the suite. Depends: habu-tokens-refused-as-e0ec22ab landed. Ownership: the checker's diagnostics. Claim: agent=bob workspace=.jj-ws.noindex/habu-a-refusal-s-3b0cae51

Cases 2026-11-10 (bob): CHECK-CANDIDATE! answers 1 (uncheckable) for a text whose name is undefined while its packet says rejected; CHECK-VERDICT puts UNSEEN ahead of OK, so a mismatch followed by a name a rendering statement may define answers deferred rather than rejected (first-failure worker; a reviewer is writing the case).

Re-review 2026-11-10 (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/unseen-rereview.md, claim 3; probes cases8/unseen-refold probe.f, probe3.f): CHECK-CANDIDATE! answers 1 (uncheckable) while the packet says rejected for any undefined word (q3 ': Q3 ( -- ) NOPE'), for p2, p7 and q4, on every engine: CHECK-VERDICT (src/core/checker.f :26277-26281) does not count RCOMPILE among the refusal flags and tests UNCK before OK. The fix ranks a recorded compile-time refusal as refused, above UNCK; then UNFIT-UNCHECKABLE's arm keeps only its EXIT. It changes the answer for every undefined word from 1 to 0: before briefing, name every reader that tells 1 from 0 (DIAGXT's renders at :26961, TOP-JUDGE :31315, CK-AOT-RETRY-DUE?) and what each does with an undefined word.

Fold 2026-11-10 (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/unseen-refold.md): UNFIT-UNCHECKABLE's arm now also clears OK, so w1's program answers 0; only a probe shows that answer (probe2.f in /Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/unseen-refold2), since UNSEEN-CASES cannot put two packages around a candidate: this dot's lane adds the test. The arm's '0 UNDEFERR !' makes CK-AOT-RETRY-DUE? (checker.f :25695) answer false for that definition, seen only on a seeded engine, which nobody ran: the reader census covers it.
