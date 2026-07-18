---
title: Choose EXTENT nominal substrate (role/TFAM/atom)
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T17:42:45.515733+02:00"
---

Decision dot blocking habu-extent-typed-tensor-bde435dc, habu-extent-role-product-8e364885, and Foundation A1b package-scoping. Three existing substrates can carry a declarable nominal integer: (1) CT-ROLE roles (roles.f DEFTYPE - global codes, auto-converters, strict vs n; contract locked by test/type-nominal-suite.f); (2) TFAM arity-0 cell families (type-family.f - already package-scoped, T-PARAM, no converters); (3) extent-atom prefix family (checker.f:2476-2489 extent-* - already the kernel/PTX extent mechanism used in matrix<space-global,f32,extent-r,extent-c>). golden-syntax.md candidate B writes idx<#M> (idx family applied to an extent arg), which most naturally wants the extent to be an atom/con usable as a family argument. The BTC-7 product/factorization requirement (habu-extent-role-product-8e364885, docs/batch-sequence-design.md) must inform the choice. Pick the substrate, record why, re-scope A1b and the EXTENT: dot accordingly, deliver a minimal probe fixture showing the chosen substrate typing idx<#M> with two extents distinct.
