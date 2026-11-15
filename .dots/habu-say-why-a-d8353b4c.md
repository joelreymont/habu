---
title: Say why a read type has no stored form
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T10:43:59.981471+01:00"
---

Problem: `s" xt" EFFECT:READ-TYPE` (src/core/effect.f:36, reached from src/lib/fsm.f:187 and any define-word program) prints `Error[E-BAD-SIGNATURE]: no stored type holds it 'xt' in the type 'xt'` with help `write the type as a signature writes it`. The reason has a stray 'it' (src/core/sumtype.f:686), names the type twice (src/core/check-hook.f TEXT-TYPE-REFUSE tests `tu xu <>`, true when the token is empty), and its help tells the agent to do what it did. The storage definers already classify the same cases (src/core/checker.f CHECKER-STORAGE-TYPE-REFUSE: STG-XT-TYPE, STG-SCHEME-TYPE, ...; src/core/render.f STGR-REASON$ and STGR-SUGGEST$). Acceptance: the rejection names the type once, says why it has no stored form in the storage definers' words (`xt in a stored type` for `xt`) and gives their fix; a test pins the `xt` case. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/carl-ttype.
