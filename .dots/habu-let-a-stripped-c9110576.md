---
title: Let a stripped image register a lifecycle hook
status: open
priority: 2
issue-type: task
created-at: "2026-09-21T18:07:21.895718+03:00"
---

Problem: a stripped image can read IMAGE-LIFECYCLE's registry (its lock and counters are claimed fresh, 835093fd) but cannot register a hook: REGISTER/PREPARE store a quotation into a declared cell through xt!, and xt! needs the engine's address-cell table a stripped image does not carry, so the build is refused with 'aot: PC-relative target removed or outside closure site=xt!' (measured by the PZB slice 2 worker with HOOKS and PERSISTENT claimed as well). Every library that registers its cleanup on first use (UNICODE:CASEFOLD= in the probe) reaches it, so Tender's server and scraper are refused here, by name (caller=STORE+376 target=DICT+56 today). Acceptance: decide the mechanism - an xt! that a stripped image carries (an address-cell row for declared quotation cells in the closure), or hook registration that does not store an xt into a declared cell - implement it, claim HOOKS and PERSISTENT the way the counters are, and prove it with an HBT-STRIPPED-* fixture that registers a hook, runs, and prints from the hook at exit; aspen re-probes Tender's three entry points on the head. Files: src/habu/aot-closure.f, aot-owned-cells.f, aot-lib.f, lib/image-lifecycle.f, tools/hb-build-test.f, docs/native-applications.md. Verify: tools/hb-build-test.f, test/stripped-address.f, test/run.f. Depends: none. Ownership: hazel. Claim: unassigned.
