---
title: Let tools/check.f accept the build drivers
status: open
priority: 3
issue-type: task
blocks:
  - habu-let-the-pre-c145b1a2
created-at: "2026-10-16T08:39:00.873233+03:00"
---

Problem: CLAUDE.md says build drivers are checked Habu programs, yet tools/check.f refuses them. Measured on master a2b276e9's engine: bin/hb tools/check.f tools/wasm-build.f rc 1 'CHECKER-MUTATION tools/wasm-build.f:92:3: set-check mutates checker state in strict boundary mode'; tools/build-fixpoint-certify.f rc 1, the same at :64; tools/native-build.f rc 78 ('check.f: throw code 78'). The first two open a '0 set-check' window before 'include src/habu/prefix-rewind.f'. The strict boundary lint (tools/checked-boundary-lint-core.f:304 UB-CHECKER-MUTATION?) authorizes hook installs only through the HOOK-SITES registry or a TRUSTED: wrapper. Acceptance: census every top-level tool under tools/ through tools/check.f; for each refusal, fix the layer that is wrong (the driver's unchecked window, a registry row for it, or the lint). Every driver then passes tools/check.f, with a case that keeps it passing. Files: decided by the census. Verify: tools/check.f on each driver. Depends: carl's checker, bootstrap and build-tooling chain (held until it lands). Ownership: the drivers and the lint. Claim: unassigned (held for carl's chain).

Landed: tools/check-drivers-test.f (gate suite check-drivers) runs tools/wasm-build.f, build-fixpoint-certify.f, tier0-profile.f, build-fixpoint-main.f, ddc-drive.f, imagedisasm.f and json-only.f under tools/check.f with no arguments and asserts each reaches its own exit; test/effect-store-census-test.f does the same for tools/effect-store-census.f and effect-store-census-run.f. Both rows share tools/check-exit-test-lib.f, which also fails a case on check.f's `check.f:` prefix or usage line. The fixes: wasm-build.f and build-fixpoint-certify.f lost the `0 set-check` before `include src/habu/prefix-rewind.f`; tier0-profile.f clears the hook in one TRUSTED: word; build-fixpoint-main.f, ddc-drive.f, imagedisasm.f and json-only.f require their own libraries. Remaining: tools/app-build.f, native-file-explicit.f, native-build.f and build-profile.f, which the pre-verifier refuses (habu-let-the-pre-c145b1a2). When it lands, add them to check-drivers-test.f and close this dot.
