---
title: "Name the cast in a refused CAST: error"
status: active
priority: 2
issue-type: task
created-at: "2026-10-31T19:10:40.963749+02:00"
---

tim (Maki) 2026-10-31: on hb d5384ec8 a refused CAST: ends the Maki image build with only 'hb: uncaught throw code 7135' (rc 67); CAST-CERTIFY and CAST-HEAD-REFUSE (src/core/checker.f ~25014-25046) throw E-CAST-CLASS, E-CAST-LINEAR, E-CAST-OWNER, E-CAST-MINT as bare codes. A 191-cast program needs a hand search. Acceptance: the error names the cast word, its file and line, and for E-CAST-OWNER the foreign family and its owning package, like a type error's 'at X expected: ... actual: ...'; a rejected-program test asserts the text. Files: src/core/checker.f, the cast rejection tests. Verify: the cast-negative row. Depends: none. Ownership: jerry (cast-owner author). Claim: agent=jerry worker workspace=.jj-ws.noindex/jerry-casterr, brief /tmp/jerry-big/casterr-brief.md and /tmp/jerry-big/casterr2-brief.md (settled design, second worker), launched 2026-10-31.
