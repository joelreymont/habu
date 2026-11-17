---
title: "Place a storage type's parse rejection at its token"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-17T17:15:17.716102+01:00"
closed-at: "2026-11-17T17:18:33.861378+01:00"
---

Problem: a typed storage whose type does not parse is rejected E-BAD-STORAGE "the type of `XB` does not parse" placed on the whole type (src/core/checker.f STORAGE-TYPE-REJECT passes ta tu for STG-MALFORMED-TYPE), though the parse kept the token it stopped on (SGBAD-A, SGBAD-U) and its kind, as a signature's rejection shows them (SGBAD-TOKEN, SGBAD-LABEL$). Seen: `TYPED-VARIABLE XB [ -- renders renders ]` underlines the whole quotation type, not the second `renders`. Acceptance: such a rejection stands at the token the parse stopped on, labelled by its kind as a signature's is, the same under the load and tools/check.f; a test pins the place. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-stgpos.
