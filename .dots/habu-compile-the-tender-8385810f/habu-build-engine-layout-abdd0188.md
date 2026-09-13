---
title: Build the first engine with its source target layout
status: active
priority: 1
issue-type: task
created-at: "\"2026-09-13T14:51:13.073131+03:00\""
blocks:
  - habu-preserve-complete-addr-258c0288
  - habu-walk-the-dynamic-e03edf85
  - habu-track-retained-jit-1dc23a17
---

Plan: [PLAN.md](../../PLAN.md). Design reconciled 2026-09-13; replaces stale diagnosis/claim. Claim: Cedar implementation in `cedar-target-layout`.

Own native-build.f phase/output API, aot-capture.f host-to-target capture interface, native-runtime.f SEAL preparation split and habu2.f ENGINE-EMIT entry. Host readers retain host ABI. Freeze target declaration/code/signature membership, persist registry DATA before final D1 (USIGS always allots), prepare once and copy immutable owned capture. Then reopen native compiler session and compile source-bound target emitter outside capture. Pass owned capture explicitly, no ambient AOT-BUF cross-instance access/new format/disk stage. Translate supported moved fixed slots by identity; refuse incompatible host. Caller-selected private output includes temp/smoke/promotion. Test generation1 growth/shrink actual cap/heap versus constants, forced registry growth, session reuse and capture bytes unchanged by writer compilation.

Verification: focused real-load cases above; rebuild and run `bin/hb --load test/run.f` for compiler/runtime integration. Speed acceptance uses the all-AOT campaign pair; functional/count evidence can be developed in parallel.

September13 integration verification: private local-case product825c2c609a06
failed to build the combined local-case/arena source at capture with rc74,
`aot-capture: defer metadata outside DATA window`. The original integration
host28e11361 built the same source successfully, yielding0c602d1c194c.
Logs are in `.jj-ws/cedar-correctness-verify/build/`. The baseline comparison
subsequently proved this specific refusal was the local `cell`/constant `CELL`
collision in ACAP-DEFER-SITE, fixed undera16875d6. Unchangedccc0661a buildsB1/B2;
after the caller rename, host825c2c60 builds product968cabff successfully. This
does not establish the separate source-layout transition or all-AOT acceptance.

Implementation in progress: capture-only host dependencies; target writer compiled after an explicit owned copy of the existing section payload; target preparation moved outside its source load and final DATA bound taken afterwards. Caller-selected output includes temporary emission, smoke and promotion. Four large capture buffers use existing transient mappings so the host/writer pair does not exhaust the32MiB target heap. The owned-copy regression passes at both tiers, retaining both full address rows and code bytes after original buffers change. An all-tier1 build exposed raw indirect calls in CHECKER-OWNER (1dc23a17); typed dispatch repair is active. Full first-generation layout/provenance/restore acceptance remains pending.

The reviewed driver now has a shared core and two explicit entries. Production
selects tier1 before dependencies and holds executable-build scope; the old-seed
bootstrap reloads the paired compiler at tier1 and requires an explicit output.
The owned capture carries the query result for its frozen live code span; the
source writer forwards it to FORTH-ORIGIN. Both-tier core loads and owned-buffer
checks pass. Independent controls preserve native/unknown origins, refuse JIT
even during bootstrap, refuse unknown for production and check the extent before
the query. Unknown bootstrap output remains private; no complete engine or
product-hosted positive rebuild is accepted yet. Native script arguments are an
explicit core dependency after prefix rewind. See commits d6d20b8e/f91cacb4.

Private bootstrap from d7184652 now succeeds (341.180 s), yielding
`/tmp/cedar-family-stage-abi/hb-bootstrap-B1-registry`, SHA-256
`a4fc1996d67ae9a46d53d9cea58622a31291fc5d3f5cf9e1de58f2ce07a6c85a`.
The heap-start and source-identity regressions pass on that actual first product.
The tracked rebuild refuses after 0.517 s with E-NFEED-STATE (-8400), reduced to
`1 set-tier : ID ( n -- n ) 1 + ;`. Its native tape installer retained the build
address of deferred cells because DO-IS emitted ADDR-NONE. Replacing only the
installer in a test process restores compilation; fa0c0c49 owns the DATA-address
fix and fresh capture regression. Full product-hosted acceptance remains open.
