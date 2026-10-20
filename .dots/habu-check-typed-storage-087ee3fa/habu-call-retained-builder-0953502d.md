---
title: Call retained builder actions after rewind
status: closed
priority: 2
issue-type: task
created-at: "2026-10-19T13:10:39.708339+03:00"
closed-at: "2026-10-19T13:36:40.432268+03:00"
close-reason: The checked driver enters RUN before rewind and reinstalls checking before target loads. Actual Wasm build/data tests, validators and runtime cases pass; original capture roots and linker refusals remain. Independent Astra review clear.
---

After correcting numeric capture coordinates, real tools/wasm-build.f still rejects execute with70 after prefix-rewind. A minimal numeric-only retained RUN(n n--) action passes without rewind and fails after it, with no capture or pointer coordinate involved. Reproducer is retained under ~/.cache/tmp/carl-capture-coords/retained-action.f. Find whether the responsible boundary is caller protocol or typed-row preservation, fix without weakening top-level checks or restoring retired source authority, then build a real Wasm artifact.
