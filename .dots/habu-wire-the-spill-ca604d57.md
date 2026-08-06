---
title: Wire the spill rewrite loop into production
status: active
priority: 2
issue-type: task
created-at: "2026-08-06T14:17:18.067105+02:00"
---

PROBLEM: src/compiler/native/migrate.f:517 EMITTED runs SELECTED -> A64RA:ALLOCATE -> A64RAV:ACCEPT -> A64EMIT:EMIT and never rewrites. src/compiler/native/spill.f A64SPILL:REWRITE exists but only test fixtures call it (test/compiler/native-chain-fixture.f:180 NFIX:LOWER is the working idiom). This is PREREQUISITE 2 of the cut, habu-cut-colon-compilation-a5aa3f1f. FIX: allocate; if the plan holds spills, A64SPILL:REWRITE, re-allocate the rewritten module, re-validate through A64RAV, then emit. ACCEPTANCE: a body with planned spills compiles through NMIGRATE:DEFINE end-to-end with answers pinned; the two refused corpus rows PRESSURE-LOOP and CALL-PRESSURE stay refused with their gap lines unchanged; no compiled corpus row changes bytes (codegen-compare 0 findings). Also carries the two whole-tree probes the cut leaf asks for (trapping arithmetic under CNUM-OVERFLOW:TRAP; to/^ on typed locals), recorded verbatim into .dots/habu-cut-colon-compilation-a5aa3f1f.md as a PROBED block. Files: src/compiler/native/migrate.f, test/compiler/native-migrate.f. Verify: bin/hb --load test/run.f; maki; codegen-compare; native+regalloc+emit suites; four lints. Depends: none. Ownership: src/compiler/native/migrate.f. Claim: agent=spillwire workspace=.jj-ws/habu-wire-the-spill
