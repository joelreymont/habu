---
title: Make edited native rebuilds incremental
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-29T12:09:07.007563+02:00\""
---

Problem: tools/native-build.f recompiles unchanged checked packages and build tools after an edit. One qualified macOS ARM64 baseline on source `089a587043eee49a44e2ece46ed7f8965fdcfafd`, hosted by `product-hb` SHA-256 `43d162ad79c940d9ef13633342cced3b11658527411374e79ce9f1b43158c4eb`, took 124.39 s externally: target source load 90.386 s (checker 13.119 s, native runtime 63.045 s), writer source load 21.218 s, and actual writer emission 0.153 s. The engine and .names were byte-identical to the qualified release candidate. Raw output and method are retained at `/Users/joel/.cache/tmp/habu-prefix-probe-20260929-knAWMN/probe.receipt.md`. A late-source prefix hit alone cannot meet the handful-of-seconds whole-invocation target.

Outcome: make packages separately reusable checked compilation units, with explicit ordered inputs, interfaces, imports, relocations and checker/registry remapping. The first measurable implementation slice is a reusable, source-authoritative builder/writer application image, followed by one real package artifact for NBR. The completed-load prefix checkpoint remains an optional late-edit accelerator, not a prerequisite for package artifacts; it cannot independently retain later packages after an early edit. A source transaction must bind discovery, keys and compilation to the same owned bytes before cache publication.

Acceptance: E2E cases precede implementation; restore reusable state in a fresh process, build a semantically edited late source, and compare engine and .names byte-for-byte with a cache-off build of the same tree. Verify prefix-input and producer invalidation, changed constants/effects, rejected source, corruption refusal, private/package resolution, literal ownership, native provenance and product/whitebox distinction. Measure cold, repeat and edited wall/user time and phase time, including startup, restore, remaining source, capture, writer, sign and smoke; claim a speedup only after a real cache hit is measured. Run focused real-load E2E, full native suite and uncached multi-generation convergence for compiler/capture changes. Root owns review, integration, gates and push.

Retained work: `.jj-ws/rebuild-perf` change `uwyrrqpztlrmrwwkxszpmosuxvqsttoz` at `b5b69a22113cde660903613dc56ae5efb2838196` preserves the prepared-HIR/backend-cache experiment for later evaluation. Its Astra review found unresolved relocation and key blockers; it is not part of this checkpoint implementation or a verified speedup.

Retained work: `.jj-ws/rebuild-prefix` change `kwqzrrvykvwwrzrmpnqvwyqnnxqlspyq` at `6cf87a47118b137605bda5179f32c655741cf9ce` preserves the exact-prefix WIP, edited-callee E2E acceptance, source cut and strict closure fix for later evaluation. The active builder/writer slice lives in `.jj-ws/rebuild-builder` from exact stable base `72299f982c258dceb2a81a2cbef5d4c02a9733db`.
