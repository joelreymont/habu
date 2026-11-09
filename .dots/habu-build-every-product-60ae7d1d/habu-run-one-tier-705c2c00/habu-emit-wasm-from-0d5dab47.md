---
title: Emit Wasm from T0
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T14:52:51.664135+02:00"
blocks:
  - habu-cross-compile-through-66516afb
---

Problem: Wasm links tier 1 output from src/arch/wasm/backend.f. Acceptance: src/habu/t0-wasm.f writes WENC's emission format (encode.f:48-57) so capture.f, link.f, link-wasm.f, dynamic.f, kernel*.f, leb.f, profile.f survive; structured control mapped per umbrella section 4; vregs = i64 locals, SPILL-ALL at calls; a record->arity reader for lanes; does>/create stay refused in a Wasm link; wasm-build.f requires t0-wasm.f; delete backend.f, passes.f, select.f, structure.f, wstruct.f, encode.f (helpers move). Files: src/habu/t0-wasm.f (new), src/arch/wasm/*, tools/wasm-build.f. Verify: test/wasm rows under bun, maki viewer build, size/run vs tier 1. Depends: L6. Ownership: those files. Claim: unassigned.

Acceptance cases (defects the replaced Wasm build shows on the arm64 landing tree, 2026-11-09):
- tools/wasm-build.f (Maki's viewer build, maki/viewer.f) dies at load: it includes src/habu/prefix-rewind.f, which the old builders' deletion removed. A Wasm build runs through the linker, with no rewind of the running hb.
- test/wasm/capture.f and link-capture.f die "tfam: the captured type registry does not fit the artifact's buffer": a capture copies hb's whole type registry into the fixed AOT-REG-CAP (src/habu/aot-decl.f), which the hb carrying the linker and the typed interpreter outgrew. A Wasm build does not fail by what hb itself carries. Both tests are off (`false [if]` in test/gate-stdlib-cases.f) until this move.
