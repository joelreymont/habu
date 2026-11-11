---
title: tools/check.f refuses a structure field whose type is an arena owner
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T11:30:37.351865+02:00"
---

Problem (tim, 2026-11-11; /tmp/tim-habu-limits.md, output /tmp/tim-habu-limits/habu-test-arena.check.out): tools/check.f rejects Habu's own test/arena.f, by default and with --json-errors: E-BAD-DECLARATION 'unknown field type' at FIELD items recs (line 23, column 40), where recs is declared by ARENA. --load and --verify-only accept the same file, and the same happens on /tmp/tim-habu-limits/h1-fixed.f. Acceptance: tools/check.f accepts every file --load accepts for this form; test/arena.f checks clean; the cause is found in the layer that disagrees (check.f's declaration pass against the loader's) and fixed there. Files: tools/check-core.f and the declaration pass it runs. Verify: bin/hb --load tools/check.f -- test/arena.f rc 0. Depends: none. Ownership: unassigned. Claim: unassigned.
