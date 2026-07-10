---
title: "Checker capability: layout-kinded product/sum fields"
status: open
priority: 2
issue-type: task
created-at: "2026-07-10T03:09:22.923186+02:00"
---

Gap proven during cad-adt-swap audit (2026-07-10, probe P6): a PRODUCT/SUMTYPE field/payload typed as another layout family (e.g. FIELD d dtype where dtype is an ENUM) rejects at declaration (E-TDECL-PAYLOAD). docs/type-families.md §18 keeps v1 params cell-kinded (rejects option<result<n,n>>) and the FIELD/payload grammar rejects family applications, so the literal 'SKEY as a PRODUCT with enum fields' shape (dot habu-cad-adt-swap-7bf0bb1f priority 1) cannot be authored: a product cannot hold dtype/layout/align enum fields. Needs layout-kinded parameters/fields (§18 'Later syntax' PARAM a layout) so a layout family can be a field of a product or a payload of a sum, with nested width accounting + hidden-field expansion. Depends: TFAM 15 (PRODUCT), TFAM 16 (packed/boxed layout). Related: habu-checker-capability-typed-a480c423 (storage), habu-checker-capability-derive-23788e95 (eq).
