---
title: "Preserve named provider rows when publishing verified effects"
status: open
priority: 1
issue-type: task
created-at: "\"2026-09-13T11:45:43.535614+03:00\""
---

Plan: [PLAN.md](../../PLAN.md). Claim: unassigned (stale claim cleared 2026-09-16). Independent Astra source review accepted; independent native-provider-rows and ir-context real loads pass on private candidate e0f2fee7. Combined rebuild and native gate are next.

Cause: quotation execution aliases declared named rows to inference tails with fixed-window/literal kinds. CHECKER-USIG-CERT-PARSED copied those temporary kinds into the provider's reusable effect. Recording disabled reproduces it too; CALL-FREEZE and a finally-only diagnosis are disproved. p-only.f and v-quot-exec.f are the reduced real-load failures in .jj-ws/habu-keep-a-row-f2c4f3d4/build/repro.

Fix owns checker.f declaration-row capture and effect publication, excluding owner dispatch/payload sections. Save the declared named-row identities, follow their verified aliases and fixed prefixes, and restore only their terminal kinds while serializing the successful effect. Keep inferred fixed cells/types, independent anonymous callback kinds, and the live width graph. Reset the publication overlay on throw, checker retry/reset, and capture preparation.

Verification: a fresh private native build passes test/compiler/native-provider-rows.f across all four provider/caller tier pairs: direct/quotation/finally, empty/nonempty prefixes, higher-order forwarding, wrong types and borrow negatives, and anonymous callback windows. Native-provider RB1/RB3 controls preserve inferred input requirements; tier 0 still records its textual declaration. The original engine fails the new Q-PREFIX positives. Existing finally, native-generic-calls, effect-read-api, engine-candidate, underdepth, and ir-context tests pass; ir-context also passes an explicit tier-1 load through WITH-CONTEXT-BOUND. Integration still requires rebuild and `bin/hb --load test/run.f`. Speed acceptance remains the all-AOT campaign pair.
