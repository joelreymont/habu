---
title: "Call DECL-EVENT from PRODUCT's record words"
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T13:14:19.900564+01:00"
---

Problem: src/core/sumtype.f:972-975 declares four hooks (TDECL-EVENT-CURRENT-XT, -DECL-XT, -ARITY-XT, -FIELD-XT) that src/core/decl-event.f:613-617 installs, because TDECL-PRODUCT-RECORD and TDECL-PRODUCT-FIELD (sumtype.f:1022-1050) load before decl-event.f; their only caller is now src/core/sumtype-front.f, which loads after it, and docs/design.md 'Machinery being replaced' turns a hook into a plain call. Acceptance: the four defers and their installation are gone; PRODUCT's record and field words call DECL-EVENT directly; hb builds from source and type-decl and structure-decl pass. Depends: none. Claim: unassigned.
