---
title: Load GPT-2 atomically
status: active
priority: 1
issue-type: task
created-at: "2026-07-28T17:13:41.372373+02:00"
blocks:
  - habu-delete-model-config-1c71a13e
---

Why: GPT-2 loading is split across public prepare, check, ready, mapped, and copied states. That split admits a second configuration authority, exposes cleanup states no product caller needs, and keeps result-valued release shells after owned unmap failure became fatal. The copied loader has no product caller and is wrong for the mapped unified-memory path being built.

Dependencies: `habu-make-owned-release-79de2b5c` supplies fatal `MEM:UNMAP`; `habu-add-bounded-u32-9bd95c8c` supplies the preserving bounded weight read.

Result: package `GPT2LOAD` exposes one load operation, `LOAD-MAPPED ( SAFET:census MDLCFG:mcfg -- load-result )`. `load-result` has exactly `loaded(gpt2-model)` and `rejected(code)` arms, and both arms consume both inputs. Validate the complete model family, tensor set, shapes, types, spans, and mapped extent before moving ownership. Complete every recoverable allocation before detaching the mapping. A recoverable failure releases every provisional owner and the census, drops the non-linear configuration, and returns `rejected(code)`. Success detaches the mapping, releases the census metadata, constructs one mapped `WSTORE:store`, and returns a model containing exactly `{ WSTORE:store, MDLCFG:mcfg, model-proof }`. `MODEL-CONFIG ( gpt2-model -- gpt2-model MDLCFG:mcfg )` preserves the model; `RELEASE-MODEL ( gpt2-model -- )` consumes it.

Hard-cut the complete release caller chain in the same change: `SAFET-MAP:UNMAP ( ptr u8 CAD-NUM:byte-len -- )` calls fatal `MEM:UNMAP`; SAFET converts only the validated length stored in its owner record before crossing that package boundary. `SAFET:UNMAP-MAPPING`, `WSTORE:DISPOSE`, `WSTORE:TABLE-DISPOSE`, `WSTORE:BUILDER-DISPOSE`, and `WSTORE:BUFFER-DISPOSE` consume their owner and return nothing. Delete their catches, result unions, byte-count results, and result helpers. Do not migrate the staged GPT-2 cleanup ladder to the new signatures; delete that ladder.

Delete public `PREPARE`, `CHECK-MAPPED`, `CHECK-COPY`, `LOAD-COPIED`, every prepared or ready type/result, discard/return lifecycle, copied-buffer machinery, copied scalar configuration fields, `MODEL-LAYER-COUNT`, `MODEL-CONFIG-KEY`, `E-CONFIG-MISMATCH`, and all compatibility paths. Retain at most the private `TABLE>CELL`, `CELL>TABLE`, and `MAKE-MODEL-PROOF` boundaries; no raw census, mapping, prepared block, ready block, or copied buffer cell survives. Delete the prepare, copy, and payload suites after moving unique production validation cases into the atomic mapped suite. The atomic `load-result` itself is the real linear-payload proof, so the separate 443-line payload capability suite is duplicate test machinery.

Owner: `SAFET-MAP` and `SAFET` own fatal checkpoint release, `WSTORE` owns resultless store exits, and `GPT2LOAD` owns validation, ownership transfer, the result, and the model. No package may duplicate another package's validation or representation.

Production red: `FIXTURE-PATH SAFET:LOAD MATCHING-CONFIG GPT2LOAD:LOAD-MAPPED` fails today because `LOAD-MAPPED` requires `mapped-ready`. Acceptance: that exact real provider path returns `loaded`; the model returns the complete original configuration and bounded reads return the fixture bytes; release restores every live-owner counter. Every retained malformed checkpoint case returns its exact `rejected` code with zero leaked owners. A revoked real mapping passed through the surviving SAFET and WSTORE disposal entry points exits 71, writes exactly `memory: unmap failed` to stderr, and executes no survival marker. A raw length cannot reach `SAFET-MAP:UNMAP`. The old words, types, error, files, trust rows, and result signatures do not resolve. Focused SAFET, WSTORE, atomic GPT-2, full Maki, trust, typed-local, package, and exact-diff checks pass.

Exact write set: `maki/infer/safetensors.f`, `maki/infer/safetensors-test.f`, `maki/infer/weight-store.f`, `maki/infer/weight-store-test.f`, `maki/infer/gpt2-load.f`, `maki/infer/gpt2-checkpoint-fixture.f`, `maki/infer/gpt2-mapped-test.f`, `maki/test.f`, `maki/test-core.f`, `TRUSTED.md`, `tools/refine-lint-core.f`, and `STATUS.md`; delete `maki/infer/gpt2-prepare-test.f`, `maki/infer/gpt2-copy-test.f`, and `maki/infer/gpt2-payload-test.f`.

Forbidden: copied residency, retry state, staged public lifecycle, always-successful result wrapper, compatibility alias, version or ABI field, scalar configuration reconstruction, second configuration input, public raw field, new trust class, fallback, or unrelated serving and forward work.

Claim: agent=claude-gpt2-atomic workspace=.jj-ws/habu-carry-model-config-c9085fa1-atomic
