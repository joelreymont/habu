---
title: Reset the header parameters for a wrap
status: active
priority: 2
issue-type: task
created-at: "2026-11-16T04:59:45.665748+01:00"
---

Problem: NEWTYPE and DEFTYPE (src/core/structure-decl.f WRAP-DRIVE) never reset the header parameters TYPE-DECL holds, so a wrap reads the previous declaration's letters: after 'STRUCTURE ws<a,q> FIELD x a ;STRUCTURE', 'NEWTYPE wn read-view<q,q,a>' dies 'tfam: bad param index' with status 76 and 'NEWTYPE wn a' is rejected 'type parameter has incompatible kinds' where a fresh load says 'unknown type' (/tmp/jerry-kinds/wrap1.f, wrap2.f). Acceptance: both are rejected 'unknown type' at the letter, status 70, as /tmp/jerry-kinds/wrap3.f is; a declaration-suite case pins it. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-w183.
