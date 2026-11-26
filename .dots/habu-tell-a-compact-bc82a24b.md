---
title: "Tell a compact enum's variants from its derive features"
status: active
priority: 2
issue-type: task
created-at: "2026-11-26T10:02:52.448241+01:00"
---

Problem: src/core/enum-decl.f ED-FIRST-VARIANT reads every token after DERIVE that DERIVE-FEATURE? accepts as a feature, so a compact ENUM whose first variant is named eq, hash, tag, name or order (`ENUM e DERIVE eq tag low ;ENUM`) silently derives it and loses the variant. Approach: each DERIVE clause names one feature in ENUM, STRUCTURE and SUMTYPE (`DERIVE eq DERIVE hash`), as POLICY names one policy; a derive feature right after a DERIVE's feature is rejected with the token's place, in every form. Acceptance: such a declaration is rejected naming the token; enum-decl, structure-decl and type-decl suites cover it; no multi-feature DERIVE is left in the tree. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-derive.
