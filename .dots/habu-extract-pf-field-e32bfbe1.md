---
title: Extract PF field arena into src/core/type-field.f
status: open
priority: 2
issue-type: task
created-at: "2026-07-20T22:50:58.957435+02:00"
---

One-concern-per-file follow-up to habu-fields-add-shared-6b063c62 (closed as landed-in-substance): move the PF field-arena section out of src/core/type-family.f into a new src/core/type-field.f. This is a 3-way split, not a clean lift: PF depends on the TFAM/SUMV core above it (PF-OWNER-OK? uses SUMV-FAM@) and is consumed below in the same file by PRODUCT-IWIDTH/SUM-IWIDTH and TFAM-CONCRETE-LINEAR?, so load order must become type-schema.f -> TFAM/SUMV core -> type-field.f (PF) -> width/linearity -> sumtype.f, and it cannot land while another lane holds type-family.f. Seed-affecting (new src/core file + load rows in all five build manifests + FILEMAP). Sequence after habu-fields-attach-variant lands.
