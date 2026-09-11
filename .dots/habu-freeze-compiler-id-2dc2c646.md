---
title: Freeze compiler ID manifest
status: open
priority: 1
issue-type: task
created-at: "2026-07-27T13:50:14.622291+02:00"
---

Claim: agent=freeze_manifest workspace=.jj-ws/habu-freeze-compiler-id-2dc2c646 (RELEASED 2026-08-21: workspace gone, no live lane - gc)

Scope: freeze the exact IR-0.1 identities only: IR-ID:ir-module-key and every IR-ID ID family. Implement one checked Habu canonical schema manifest, its digest, and one shared artifact containing valid and hostile reachable numeric key/serial/owner/local/bound vectors. The scalar rows include discriminating values above bit 32 and prove that `ir-count` and `ir-pool-offset` round-trip unchanged, never acquire a module serial, and never use owner/local projection. Keep wrong-family rejection as a separate checked static fixture, and keep require replay as a separate executable load-path fixture; neither is encoded as a runtime numeric vector. Acceptance: deterministic bytes and digest; every IR-0.1 family appears exactly once; hostile runtime vectors cover every reachable numeric guard; scalar rows distinguish raw scalar identity from packed-ID behavior; the static wrong-family fixture is named; focused checked manifest/vector test passes. Ownership: checked ID manifest, digest, shared numeric vector artifact, and the static fixture reference only. Excludes Rocq model/theorems/parity gate, allocator proof, replay implementation, shared records, tables, opcodes, general witnesses, dialects, native/GPU, and maki. Depends on habu-add-compiler-ir-21e976fc.

Checkpoint:

1. Owner: proof-support package `COMPILER-ID-PROOF`; production subject `IR-ID`.
2. Entry: `require src/compiler/ir/id.f` through the self-contained compiler-ID test entry.
3. Green: the accepted IR-0.1 focused suite on the verified prerequisite.
4. Red: the same load has no canonical manifest/digest/vector artifact, so schema spelling or scalar-vs-packed drift has no production-path parity check.
5. Interface: one ordered identity manifest, one SHA-256 digest over its canonical bytes, one ordered valid/hostile numeric vector artifact, and references to the existing wrong-family and replay fixtures; no runtime compiler API.
6. Forbidden: runtime kind tags, copied Habu/Rocq vectors, numeric wrong-family or replay rows, packing either scalar family, or added trust.
7. Focused: `bin/hb --load test/compiler/ir-id-manifest.f`.
8. Broader: compiler-ID, typed-local, package, refine, error, suite coverage, host, stale-status, dot dependency, Maki, PTX standard library, and native publication gates.

BLOCKER RESOLVED 2026-08-04 (dot-purge): `habu-add-compiler-ir-21e976fc` is closed and its edge removed here. The IR-ID module landed - `src/compiler/ir/id.f`, `test/compiler/ir-id.f` and `test/compiler/ir-id-concurrency.f` are present on the `proofs` branch and `proofs@origin` is at the same revision (1bb76eab), which is exactly the close condition that dot recorded for itself. The dependency is satisfied, not dissolved.
