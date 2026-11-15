---
title: Reject an ENUM parameter used with incompatible kinds at its declaration
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T03:36:26.198051+01:00"
---

Problem: since an ENUM payload is read by STRUCTURE's node parser, LETTER-TYPE records a parameter's kind (TFAM-PK!), but ENUM's close (src/core/enum-decl.f, after the variants) never checks the kinds as STRUCTURE's SD-SEAL does: /tmp/jerry-ekk2/enum-kind.f (a used as a value, then as a scope) loads with no declaration rejection and fails at the constructor, E-BAD-STORED-SIGNATURE 'the signature does not parse here', while /tmp/jerry-ekk2/struct-kind.f is rejected 'type parameter has incompatible kinds'. Acceptance: the ENUM is rejected at its declaration with the STRUCTURE's label and a help, pinned by an enum-decl-suite case. Depends: none. Claim: unassigned.
