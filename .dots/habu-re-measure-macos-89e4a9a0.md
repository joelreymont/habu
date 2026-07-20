---
title: Re-measure macOS size pins after decl-event
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-21T00:27:42.441385+02:00\""
---

Master red on macOS since 8763905f: test/gate-size-attribution-test.f fails assert 5 - expected 148855, measured 165367 (+16512 bytes) - on the envleak train tree with engine c53c2766. Spark re-measured size pins at their merged fixpoint but cannot build or measure the macOS target, so the macOS-evaluated rows are stale after decl-event (+69 words to census 3626, CODELEN +44). Fix: run the size-attribution tooling on the current macOS engine, ATTRIBUTE the +16512 delta (prove the growth is decl-event and its suite/reflection surface, not an unrelated regression - if any part is unattributed, that part is its own bug to RCA first), then update the macOS pins/floors with an attribution comment per the existing per-target pin style. Do not loosen non-macOS rows. Verify test/gate-size-attribution-test.f green and rerun test/run.f size-sensitive groups.

Claim: agent=sizepins workspace=.jj-ws/habu-re-measure-macos-89e4a9a0
