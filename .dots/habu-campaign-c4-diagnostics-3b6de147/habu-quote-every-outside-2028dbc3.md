---
title: Quote every outside path in a prose record on one line
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T13:36:18.771874+01:00"
---

Problem: a record quoting a path or other text from outside the source writes it raw with REJECTION:EMIT-STR, so a path holding a LF or CR breaks the prose record across lines (e.g. `hb` given a missing file named with a LF: E-MISSING-SOURCE, E-UNREADABLE-SOURCE in src/core/include.f, and tools/check-core.f's records quoting a subject or label). REJECTION:EMIT-ESC (lane W227) writes such text on one line. Acceptance: every record that quotes a path or argument uses EMIT-ESC, found by a search of REJECTION:EMIT-STR calls given non-literal text; a test with a LF in a missing file's name prints one whole record. Depends: lane W227 landed. Claim: unassigned.
