---
title: Key shadow rows by shipped record
status: open
priority: 2
issue-type: task
created-at: "2026-10-01T12:04:04.882830+03:00"
---

Problem: the capture keys the shadow by window record index, but the artifact ships only the records the capture keeps. `src/habu/aot-shadow.f` `SH-REC+` and `SH-TARGET` store `idx - ACAP-W-R0`; `src/habu/aot-file.f` `?SH-RECS` bounds that index by the shipped record table. The two agree only while no window record is stripped: a stripped private word moves every later shipped record down by one, so a shadow row or site target past it names the wrong record. Found by X4b (`habu-link-records-and-647852d3`): `src/habu/link-x64.f` refuses the case only when a routine then lands on a package row or past the last shipped row (its `strip` child, rc 74); a strip in the middle of the window maps a routine onto the wrong record with no refusal.
Acceptance: shadow record rows, site targets and XT rows are keyed by shipped record row; a live routine whose window record is not shipped is either carried with a shipped owner or refused by name at capture; the reader bounds every index by the shipped table (unchanged); `test/aot-shadow-capture.f` gains a window with a stripped private word between two shadowed records, showing each routine's row names its own record, and `test/x86-64-link-records.f`'s `strip` child is updated to the new rule.
Files: `src/habu/aot-shadow.f`, `src/habu/aot-decl.f` if a table changes, `test/aot-shadow-capture.f`, `test/x86-64-link-records.f`, `docs/x86-64.md` "Dual emission".
Verify: the two suites; the `test/aot-*` rows; the ARM64 product unchanged when no shadow (rebuild byte-identical).
Ownership: krait (Intel lane).
