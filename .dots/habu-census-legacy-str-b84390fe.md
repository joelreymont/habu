---
title: Census legacy string callers
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T14:23:30.562760+02:00"
blocks:
  - habu-migrate-str-numeric-2febad4b
---

Full context: MODEL-CAD-V2-PLAN.md B5.5 deliberately leaves the global ptr-u8-n string surface outside the STR owner, but no exhaustive caller census or migration leaves exist. Perform a fixed-string census for STR-LEN, STR-OFF, STR-COUNT, FIND-SUB, INDEX-OF, SPLIT-NEXT, BUF-RESET, BUF-LEN@, BUF-APPEND and their length-bearing helpers outside lib/string.f and lib/string-test.f. Classify each call by owning file, typed target API, zero/not-found semantics, and overlap with existing numeric caller waves; add disjoint migration dots with exact files and amend habu-integrate-sealed-cad-ba510e2e to block on every leaf. Acceptance: committed B5 census, every live call owned exactly once, no source consumer edits, dot-dep/host/filemap/status green. Depends on the STR owner API dot habu-migrate-str-numeric-2febad4b.
