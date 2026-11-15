---
title: Agree on xt inside a stored quotation
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T12:01:08.689511+01:00"
---

Problem: `s" [ xt -- n ]" EFFECT:READ-TYPE` is rejected 'xt in a stored type' (src/core/sumtype.f TERM-SCHEMA: the schema has no node for xt), while `TYPED-VARIABLE V [ xt -- n ]` loads (src/core/checker.f XT-STORED?* does not look inside quotations). Every caller of EFFECT:READ-TYPE (src/lib/fsm.f, src/lib/ffi-abi.f, src/lib/ffi-callback.f) uses its type in a signature, and a signature takes `xt` (`: W ( [ xt -- n ] -- ) drop ;` loads). Acceptance: EFFECT:READ-TYPE reads `xt` and `[ xt -- n ]` as a signature does, and a built word over either checks; a stored top-level xt stays rejected ('xt in a stored type', docs/forth.md:1270-1271); a build-items test pins it. Depends: none. Claim: unassigned.
