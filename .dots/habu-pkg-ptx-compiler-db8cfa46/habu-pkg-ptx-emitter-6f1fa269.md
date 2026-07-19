---
title: Package PTX emitter core
status: open
priority: 1
issue-type: task
created-at: "2026-07-19T21:18:27.914756+02:00"
---

src/arch/ptx/emit.f:10-135, lib/ptx/header.f:5-59, lib/ptx/cg.f:26-517, lib/ptx/launch.f:5-8, and lib/ptx/cg-activation.f:16-66 expose roughly 160 EMIT-/PTX-/CG- globals, including output cursors, register allocation state, raw token render helpers, and internal opcode constants. Maki lowerers and every PTX test prelude require this active core. Wrap the architectural text emitter/header/core code generation surface in a real package such as PTX-EMIT; export only the documented begin/header/declaration/instruction/finalize and launch derivation API, keep buffers/cursors/register helpers/private op encodings private, and use short tails internally. Update all consumers in one dependency-ordered migration without compatibility aliases. Preserve exact PTX bytes, register numbering, resource declarations, launch geometry, error propagation, and zero-allocation behavior. Add negative package fixtures proving old EMIT-/CG-/header globals and qualified private helpers reject; public qualified calls must certify. Record dictionary-name bytes, loaded JIT/DATA, CODELEN, and generation throughput before/after and require no unexplained growth or regression. Verify emitter/header/launch/activation goldens, all PTX lowering suites, Maki, typed-local diff, package/host/filemap/dot lints, fixpoint, and full native gate. Parent completion census: habu-pkg-ptx-compiler-db8cfa46. Coordinate with typed PTX instruction/render work; this dot owns package boundaries and renames only.
