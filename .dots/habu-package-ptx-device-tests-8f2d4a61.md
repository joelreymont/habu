---
title: Package co-loaded PTX device tests
status: open
priority: 1
issue-type: bug
created-at: "2026-07-21T23:00:00+02:00"
---

Problem: the required `maki/test.f` gate reaches the spawned `ptx-toolchain`
suite and exits 78 at `duplicate definition: HX`. The suite deliberately loads
`tools/ptx/softmax-gradcheck.f`, `rmsnorm-device-test.f`,
`rope-device-test.f`, and `layernorm-device-test.f` into one fresh process.
Those one-concern modules still define unowned global scratch words such as
`HX`, `HDY`, `HYP`, and `HYM`; later modules collide with the first. The exact
control-publication run recorded `RED: ptx-toolchain kind=exit code=78` with
the duplicate diagnostic, while all preceding Maki tests were green.

Acceptance: give every co-loaded PTX device-test module a real package owner and
keep its scratch storage, helpers, and runner private to that owner. Preserve
only the smallest package-qualified public entry point that an external runner
actually calls; a self-running module needs no new global API. Do not solve the
collision by renaming only `HX`, by adding arbitrary global prefixes, by
changing suite order, or by spawning each file separately. Inspect the complete
`ptx-toolchain` member list for every other unowned definition that can collide
under the same composition contract, and package each changed module at its
responsibility boundary. Keep all device goldens, finite-difference checks,
private artifact ownership, target probing, and original failure diagnostics
unchanged.

Verify: reproduce the current exit 78 on the exact spawned `ptx-toolchain`
path; prove each changed module still passes alone; prove the complete
co-loaded suite passes in one fresh process; run package, typed-local, and host
gates; then rerun the co-loaded `ptx-toolchain` suite plus the required
`maki/test.f` and PTX standard-library gates on the exact integration tree.

Dependencies: none. This is an existing master-gate blocker discovered while
publishing the minimal control wave. It does not change the enum,
generated-declaration, or Unicode implementation contracts.

Claim: unassigned.
