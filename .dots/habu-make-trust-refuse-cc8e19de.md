---
title: Make TRUST refuse a name that resolves to nothing
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T22:06:16.598606+02:00"
---

TRUST accepts a name with no definition and silently mints a global checker symbol for it: s" NO-SUCH-WORD-XYZ" s" n -- n" TRUST exits 0 (pkgasm lane 2026-08-11 - this is what turned insn-schema.f's stale encoder rows into E-USING-SHADOW-GLOBAL two layers away instead of an error at the row). Fail closed: TRUST refuses a name the dictionary cannot resolve, by name, at the row. Acceptance: the probe exits nonzero naming the word; every existing TRUST row still resolves (the full gate is the sweep); a stale-row fixture reds at the row not downstream. Files: src/core/checker.f (TRUST's reader). Depends: none. Ordered WITH habu-turn-the-registry-4c064064 (converting rows while TRUST is fail-open hides conversion mistakes).
