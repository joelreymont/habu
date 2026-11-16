---
title: Declare a payloadless variant of a region-parametric ENUM
status: active
priority: 2
issue-type: task
created-at: "2026-11-16T03:43:13.187976+01:00"
---

Problem: `ENUM ekrs<a,q> VARIANT vone FIELD x read-view<a,q,n> ;VARIANT VARIANT wtwo ;VARIANT ;ENUM` is rejected E-BAD-STORED-SIGNATURE at the constructor EKRS:WTWO, 'its outputs name a variable no input supplies' (src/core/checker.f:7233 SGBAD-UNSUPPLIED?): its output ekrs<a,q> names a scope or region parameter no input carries, while a type parameter is admitted (OPTION:NONE). The installed hb fails the same way (uncaught 7156), so C4 did not cause it. Reproducer: /tmp/jerry-m12/ekrs/e.f. An ENUM whose header has a scope or region parameter can have no payloadless variant. Acceptance: that declaration loads, and a WTWO value is accepted where its caller's ekrs<a,q> is expected. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-w178.
