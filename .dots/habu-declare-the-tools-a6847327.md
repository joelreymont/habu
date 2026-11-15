---
title: Declare tool storage needed by pointer rules
status: open
priority: 3
issue-type: task
created-at: "2026-09-18T10:24:05.100828+03:00"
---

Problem: habu-refuse-ptr-field-331a9731 designs two checker refusals that land last, after the tree is moved off the shapes they refuse: a `ptr-field` whose base's pointee is not a pointer, and a `byte-view` whose base's pointee is a pointer or an unfenced type variable. This dot is that design's tools lanes (A7 tools and tests, B2 tool records; their lane text is gone). The tools still hold storage in those shapes. `ptr-field` over a ( ptr n ) base: tools/lint/text.f:88 (LINT-SLAB:BUF-FIELD; a slab is three caller-owned cells, backing pointer, capacity and length, used by tools/lint/shadow-lint.f, tools/public-signatures-core.f and tools/error-code-lint-core.f), tools/check-core.f:149 and :1378, tools/json-only.f:17, tools/json-only-core.f:32, tools/repair-packet-core.f:34, tools/lsp-test-lib.f:1635 and :1645. `byte-view`: tools/data-table-census.f:35, tools/image-size-lib.f:43, tools/image-names.f:56 and :152, tools/sha256-file-test.f:247. Existing bounds checks do not establish the pointer-cell type these rules need, so a tool the rule refuses stops loading when the refusal lands.

Acceptance: each listed site whose base the designed rule refuses reads through a declared pointer cell (PTR-VARIABLE, TYPED-VARIABLE NAME ptr t, a declared record's generated accessor) or a private `CAST:` in its owning package, with bounds, published sizes and behaviour unchanged; a site the rule admits stays as it is, and the report says which sites were which. Not every private pointer/length pair has to become a record, and no tree-wide count of `ptr-field` is a goal.

Files: the tools files above.

Verify: the lint, check, build or JSON end-to-end tests selected from the changed paths pass; a change to shared checker code also needs the repository native suite on a matching engine; once 331a9731's refusal is on a candidate engine, the changed tools load under it.

Depends: none (331a9731's refusal comes after this). The unused CHK-PTR-U8-SLOT family (tools/check-core.f:157-164) is deleted under habu-delete-the-dead-11e9a4f3, not here; the used non-indexed pointer-slot helpers stay.

Ownership: unassigned. Claim: unassigned.
