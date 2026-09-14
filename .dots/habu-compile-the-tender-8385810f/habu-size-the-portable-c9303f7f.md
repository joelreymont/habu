---
title: Size the portable effect pool from its actual encoded bytes
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-14T03:34:37.750797+03:00\""
---

The genuine cold compiler window freezes 7,670 unique live effects into 3,635,865 bytes of names and binary effect graphs. The obsolete 262,144-byte text pool cap rejects it. Use existing DYNAMIC-BUFFER ownership for the opaque effect arena and contiguous emitted staging span, reserving from validated lengths in capture, read, owned import and merge. Preserve format, graph semantics and aggregate section budget, including negative/overflow and malformed aggregate rejection before copy. Validate complete bytes through large-pool file roundtrip, source release and owned import, merge, and writer staging; no broad cap increase.

The pool and writer staging now use existing dynamic storage. File READ, owned
IMPORT and MERGE reserve through their shared section loader after the existing
complete room/budget checks. Capture admits its known row/pool extent before
growth and retains the completed capture's body/registry budget check. Staging
admits its three-section sum before growth; empty sections do not access an
unallocated pool. The chain's poison pass reserves its existing margin.

Registered `test/aot-effect-pool.f` passes both tiers on actual M
(`/tmp/cedar-family-stage-abi/hb-integrated-M`, SHA
`9522a8e5685129b17b107bd547dc0797a1f11e0bb3f89f8282b2b8206770b58c`):
`M --load test/aot-effect-pool.f` and
`M --load /tmp/cedar-alias-tier1.f test/aot-effect-pool.f` in this workspace.
It captures 1,536 checked effects / 847,893 pool bytes; compares every byte
after file and owned source release, stages beyond the former combined static
capacity, and checks both full merged pools and every row's three rebased
offsets and unchanged flags. Native source/window provenance is asserted.
Named negative/overflow, file/owned/import/merge/staging budget refusals leave
the parent's bytes intact. The two-nonempty-registry refusal remains tested;
the successful merge carries their identical registry once, from the input.

Logs: `/tmp/cedar-M-effect-pool-{native,jit}.{out,err}`. Section-reach fixture
passes with 31 labels (`...-section.{out,err}`). A capture-only 1,024-effect
control on parent 4aab6c21 refuses rc74 at the old string-buffer limit
(`/tmp/cedar-M-effect-pool-red.{out,err}`). The real cold compiler now passes
that 3,635,865-byte copy and reaches independent closure blocker c57132da;
full chain success is not claimed. Root owns independent review and the
rebuilt integrated compiler/runtime gate; no timing acceptance is claimed.
