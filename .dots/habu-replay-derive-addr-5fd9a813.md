---
title: Replay DERIVE addr accessors on the source scanner
status: open
priority: 2
issue-type: task
created-at: "2026-09-18T10:24:05.093628+03:00"
---

Problem: src/habu/verify-source.f replays declarations on the source scanner (RECORD-STRUCTURE-DECL and the definer table at :811); the DERIVE addr accessors and the reserved engine-cell definer of the prefix migration have no rows there, so a file that uses its own record accessors in the same source fails the pre-scan, and a pre-checker definer needs two effect rows (src/core/cell-effects.f TRUST plus the verify-source.f definer table; LESSONS 2026-09-18) or the seal marks it DNAME-INT. Acceptance: a file that uses its own DERIVE addr accessors passes the pre-scan; the reserved-cell definer ( : RESERVED-PTR-U8-CELL ( n -- ) create , does> ( -- ptr ptr u8 ) @ data-base swap + ; in src/core/pointer-storage.f, measured to certify and answer the reserved offset) has its TRUST row in cell-effects.f, its row in verify-source.f's definer table and its case in tools/lint/def.f; a fixture shows a clone of the definer without its two rows is sealed internal and with them is not. Files: src/habu/verify-source.f, src/core/cell-effects.f, src/core/pointer-storage.f, tools/lint/def.f, test/. Verify: fixtures; three generations with cmp; tools/bootstrap.sh check; test/run.f. Depends: habu-generate-typed-field-ba63866e. Ownership: type system. Parent: habu-campaign-c2-mem-c3d7662b. Claim: unassigned.
