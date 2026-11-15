---
title: Fail a load whose text throws a computed 0
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T23:43:43.536857+01:00"
---

Problem: a computed 0 throw at a file's top level, or in text it evaluates, ends the load early and reports success: the loader's catches (src/core/include.f INTERPRET-SAVED-RANGE ~1757, LOAD-CURRENT ~1891) read the catch's 0 as a normal return and EVALERR holds 0, so the rest of the file is skipped and hb exits 0. Acceptance: such a load stops with a rejection that names the 0 throw and its place, and exits nonzero. Depends: none. Claim: unassigned.
