---
title: Let tools/check.f accept the build drivers
status: open
priority: 3
issue-type: task
created-at: "2026-10-16T08:39:00.873233+03:00"
---

Problem: CLAUDE.md says build drivers are checked Habu programs, yet tools/check.f refuses them. Measured on master a2b276e9's engine: bin/hb tools/check.f tools/wasm-build.f rc 1 'CHECKER-MUTATION tools/wasm-build.f:92:3: set-check mutates checker state in strict boundary mode'; tools/build-fixpoint-certify.f rc 1, the same at :64; tools/native-build.f rc 78 ('check.f: throw code 78'). The first two open a '0 set-check' window before 'include src/habu/prefix-rewind.f'. The strict boundary lint (tools/checked-boundary-lint-core.f:304 UB-CHECKER-MUTATION?) authorizes hook installs only through the HOOK-SITES registry or a TRUSTED: wrapper. Acceptance: census every top-level tool under tools/ through tools/check.f; for each refusal, fix the layer that is wrong (the driver's unchecked window, a registry row for it, or the lint). Every driver then passes tools/check.f, with a case that keeps it passing. Files: decided by the census. Verify: tools/check.f on each driver. Depends: carl's checker, bootstrap and build-tooling chain (held until it lands). Ownership: the drivers and the lint. Claim: unassigned (held for carl's chain).
