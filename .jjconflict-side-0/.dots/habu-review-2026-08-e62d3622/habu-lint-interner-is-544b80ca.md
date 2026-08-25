---
title: lint interner is a quadratic linear scan
status: open
priority: 2
issue-type: task
created-at: "2026-08-23T00:10:51.008321+02:00"
---

Problem: tools/lint/intern.f INTERN-FIND (:116-121) scans every interned string on each INTERN call; measured 2026-08-23 through the real INTERN entry with distinct 2-byte keys, engine boot subtracted: 1000 ids 0.07 s, 2166 ids 0.26 s, 4096 ids 0.84 s, 8192 ids 3.26 s (x2 ids -> x3.9 time). At today's 2166-leaf tracker the interner is about a third of dot-dep-lint's ~0.79 s of work and grows quadratically with the tracker; every lint that interns (dot-dep, error-code, public-signatures, repl-lint, set-test) pays it. Acceptance: a hash index (or sorted vector with binary search) behind the same INTERN/INTERN-FIND/INTERN? surface, no behaviour change (set-test green, same ids assigned in insertion order), 8192-id intern under 0.1 s measured the same way, numbers in the commit body. Files: tools/lint/intern.f, tools/lint/set-test.f. Verify: bin/hb --load tools/lint/set-test.f; the timing probe. Depends: habu-lint-intern-table-85ae462f (the file is being packaged there; build on it). Ownership: lint interning. Claim: unassigned.
