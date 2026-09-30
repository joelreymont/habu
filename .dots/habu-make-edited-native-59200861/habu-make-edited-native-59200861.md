---
title: Make edited native rebuilds incremental
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-29T12:09:07.007563+02:00\""
---

Problem: tools/native-build.f recompiles unchanged checked packages and build tools after an edit. One qualified macOS ARM64 baseline on source `089a587043eee49a44e2ece46ed7f8965fdcfafd`, hosted by `product-hb` SHA-256 `43d162ad79c940d9ef13633342cced3b11658527411374e79ce9f1b43158c4eb`, took 124.39 s externally: target source load 90.386 s (checker 13.119 s, native runtime 63.045 s), writer source load 21.218 s, and actual writer emission 0.153 s. The engine and .names were byte-identical to the qualified release candidate. Raw output and method are retained at `/Users/joel/.cache/tmp/habu-prefix-probe-20260929-knAWMN/probe.receipt.md`. A late-source prefix hit alone cannot meet the handful-of-seconds whole-invocation target.

Outcome: reuse checked and compiled unchanged packages across invocations, then check and compile changed source through ordinary publication, capture, writer, signing and smoke paths. The saved-builder first slice preserves the completed builder/writer source state and produced a functional byte-identical E2E, but its roughly 64-second whole invocation is far from the target. The next slice is an explicit NBR package artifact with fresh checker-state import, relocated code and native dictionary publication. Package dependency and registry remapping must eventually allow early and mid-source edits to reuse independent packages; an exact source prefix is insufficient.

Acceptance: E2E cases precede implementation. Explicitly export NBR, edit its downstream A64PASS client, import NBR without loading its source body, and compare engine and .names byte-for-byte with a cold build of the same tree. Execute the edited client; verify fresh checker refusal and changed NBR source invalidation with no bad output promotion. Preserve source ownership, private/package resolution, native provenance and product/whitebox distinction. Measure cold, repeat and edited whole-invocation wall/user time and phase time, including startup, source load, capture, writer, sign and smoke; claim a speedup only after a real cache hit is measured. Run focused real-load E2E, full native suite and uncached multi-generation convergence for compiler/capture changes. Root owns review, integration, gates and push.

Retained work: `.jj-ws/rebuild-perf` change `uwyrrqpztlrmrwwkxszpmosuxvqsttoz` at `b5b69a22113cde660903613dc56ae5efb2838196` preserves the prepared-HIR/backend-cache experiment for later evaluation. Its Astra review found unresolved relocation and key blockers; it is not part of this checkpoint implementation or a verified speedup.
