---
title: Report an ambiguous MATCH family as ambiguous
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T14:04:52.043570+01:00"
closed-at: "2026-11-16T04:07:34.293649+01:00"
---

Problem: TFAM-SIG-RESOLVE (src/core/type-family.f:4036) answers an ambiguous public tail as not found and says why only through flags read after the call (TFSR-AMBIG, TFSR-HIDDEN: :4053, :4061, structure-decl.f:78, sumtype.f:535, enum-decl.f:61), so MATCH (TFAM-MATCH-FAM :4921) rejects an ambiguous family as E-MATCH-UNKNOWN-FAMILY. Acceptance: the resolve answers its outcome, the flags are gone, and a MATCH on a family two used packages make public prints the ambiguity with the packages that offer it. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
