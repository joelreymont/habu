---
title: Accept DERIVE init outside a package
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T12:01:08.668361+01:00"
---

Problem: top-level `PRODUCT ckplain DERIVE init FIELD x n ;PRODUCT` and `STRUCTURE cks DERIVE init FIELD x n ;STRUCTURE` are rejected 'derive init requires a canonical fixed-cell record', though the record is canonical: src/core/sumtype.f TDINIT-REQUIRE (:1936) requires a package, left from when the accessors were TRUSTED: text; they are now typed children named as DERIVE addr's are, which loads at top level. Acceptance: both load and their init accessors check; a type-decl test. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/carl-init.
