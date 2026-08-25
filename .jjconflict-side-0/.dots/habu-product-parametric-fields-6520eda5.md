---
title: Product parametric fields need arg-aware layout
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T07:23:54.385194+02:00"
---

Follow-up from habu-universal-enum-parametric-ad011c21: parametric family applications are accepted as SUM VARIANT payload elements only; PRODUCT fields keep the scalar/arity-0 grammar because a parametric field needs arg-aware product byte layout - instantiated width/alignment for the applied family computed per argument substitution, hidden-field expansion for nested layout families, and PF record schema storage for the application (PF rows currently store scalar schema roots). Extend the PF byte-layout computation in src/core/type-family.f and the product field grammar in the front-end path (structure-decl.f once landed - do NOT extend the legacy PRODUCT parser, which is slated for retirement). Sequencing: after habu-structure-parse-typed-c5a01e1f lands; coordinate with habu-fields-attach-variant-151e2713 territory.
