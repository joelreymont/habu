---
title: Migrate public recovery callers off the legacy BF pipeline
status: closed
priority: 2
issue-type: task
created-at: "\"2026-09-14T02:48:35.995132+03:00\""
closed-at: "2026-09-16T14:34:49.455199+03:00"
close-reason: "superseded by habu-campaign-c1-finish-1f129a00: Public recovery callers still run the legacy BF pipeline; keep the migration with the retirement of the old build path."
---

Current native-build/native-bootstrap use source-owned native capture and NATIVE-EMIT, with no BF certification. Public bootstrap.sh final install refresh, seed.f, ddc-verify.f tools/aot-chain-bake.f and build-fixpoint CLI still invoke BF stage/stdin/snap generations; non-REPL hb-build uses BF assembly and COMPILER-BUILD directly. Migrate supported callers and corresponding docs/acceptance before retiring BF; preserve no-binary recovery and diverse-build evidence. Separate from bounded WID/chain fixture migration, and not a prerequisite for current native emitter acceptance.
