---
title: Consolidate codegen buffers and case folds
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-18T23:07:22.032604+02:00\""
---

Bloat finding from the 2026-07-18 review of merged definer work. Three private byte-append codegen buffers now exist with near-identical words: XG-* in maki/extent.f (shared by extent-tensor.f and spec.f), NG-* in lib/type/value-nominal.f (a deliberate mirror of XG), and the older DTC/CAP evaluate builders in src/core/roles.f and maki/cad.f. Three private ASCII case folds also exist: XM-LC (maki/extent.f), NM-LC (lib/type/value-nominal.f), SP-UC (maki/spec.f). Design: add one lib-level codegen-buffer module (a defining word that mints a buffer descriptor plus reset/append-byte/append-string/append-decimal/contents words with one named capacity error) and one case-fold pair in lib/string.f; port maki/extent.f, maki/spec.f, and lib/type/value-nominal.f onto them. Each package KEEPS its own audited TRUSTED evaluate wrapper - the trust boundary stays per package, only the untrusted text-building mechanics are shared. Do NOT touch roles.f/cad.f in this pass (core churn; separate decision). BLOCKED: serialize behind the claimed treg-opus lane habu-type-the-new-13b0d871, which also edits maki/extent.f.

UNBLOCKED 2026-07-18: the treg-opus lane merged as 03410e95 and its claim is released; maki/extent.f is free. Note the file set grew: the typed-registry step 1 added option/nominal plumbing around XG-* in maki/extent.f - port on top of that shape. Claim 2026-07-18: agent=cgbuf-opus workspace=.jj-ws/habu-consolidate-codegen-buffers-71a8818f
