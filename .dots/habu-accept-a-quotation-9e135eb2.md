---
title: Accept a quotation handed to a word the check left to the run
status: active
priority: 1
issue-type: task
created-at: "2026-11-25T16:34:47.024849+01:00"
---

Problem: tools/check.f --verify-only rejects with E-LOCAL-ESCAPE a call that hands a quotation reading a local to a word whose body the check left to the run (W-CHECK-DEFERRED): the declared row of such a word counts each quotation input as kept (src/core/checker.f KEEP-QUOTS), though the load checks the body and accepts the call. Maki's maki.f ends in 18 such rejections (src/ops.f:15091, src/project-rebuild.f:412, src/project-views.f:109). Acceptance: a four-line program whose word is left to the run through a name a definer makes checks without rejection, its caller counted as left to the run, not certified; a checked body that keeps its quotation, and a declared signature with no body, still reject; check.f --verify-only maki.f reports no E-LOCAL-ESCAPE. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-ck-esc.
