---
title: "Sweep TRUSTED: out of lib and tools"
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T15:49:57.936377+03:00"
---

Problem: 190 TRUSTED: definition sites remain in lib/ (112) and tools/ (78, effect-store-census.f 25). Joel (2026-09-16): TRUSTED: is to be retired entirely. The FFI consumers in lib/ (udp4, tcp4, serial, zip-ffi, f64-text, task, fs-identity, process-pty-handle) are swept by aspen through a checked FFI:CALL inside package FFI once habu-pkg-owned-prim-e08e345f lands; this dot owns every other lib/ and tools/ site. Acceptance: `rg -c "^\\s*TRUSTED:" lib tools` counts only the files aspen owns until those land, then 0; each converted body is checked, with a package-owned PRIM axiom only where a primitive is genuinely unmodelled; suites of every touched file green; full gate green. Files: lib/*.f (except the FFI consumers listed), tools/*.f. Verify: rg count; per-file suites; bin/hb --load test/run.f. Depends: none for a body that calls no trust-boundary primitive (the majority; start there). A body that calls a PE-TRUSTED-ONLY primitive (checker.f 6925-6945 code-injection prims, ffi-call-bounded) waits for the mechanism decision recorded in habu-delete-the-trusted-42b30edd: hazel recommends dropping the bit and making those primitives private to their owning packages, so package privacy, not a checker flag, bounds who may call them; the FFI consumers go through aspen FFI declarer. Ownership: lib/ tools/ minus the FFI consumers. Claim: unassigned. Parent: habu-trusted-dies-prim-4fd12d60.
