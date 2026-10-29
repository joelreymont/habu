---
title: "check.f refuses src/arch/wasm/leb.f: FIELD is a reserved definition"
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T00:34:24.419251+02:00"
---

Problem: hb --load tools/check.f -- src/arch/wasm/leb.f exits 1 with E-RESERVED-DEFINITION at 102:3 (the file defines FIELD, a reserved name), on master 18f0605d before and after the comment prune; the engine provides the file, so the suite never checks it from source. Acceptance: leb.f checks clean from source (rename or restructure the definition at the responsible layer, callers updated) and a check.f run on it is part of the lane's proof. Files: src/arch/wasm/leb.f and its callers. Verify: hb --load tools/check.f -- src/arch/wasm/leb.f rc 0; the wasm rows of the suite green. Depends: none. Ownership: src/arch/wasm/leb.f. Claim: unassigned.
