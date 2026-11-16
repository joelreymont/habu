---
title: "Report a check's first record as the load does"
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T21:49:48.939671+01:00"
---

Problem: tools/check.f on /tmp/jerry-c4/probe/ae.f (line 1 an undefined word, line 2 an unclosed string) reports line 2's E-UNTERMINATED-STRING, while hb ae.f reports line 1's E-UNDEFINED: tools/check-core.f CHK-RUN-INNER runs CHK-MATERIALIZE's discovery walk, which writes its stop's record, before the run stage. Dropping the walk's record in default mode as --all-errors does (hunt W127) leaves stops the run stage does not meet as the load does: a require past the path capacity (column 1:4, the load's 1:5006), a dynamic loader path (E-LOADER-FORM, the load's E-MISSING-SOURCE), a shadowed loader word (E-RESERVED-DEFINITION, the load's E-DUPLICATE-DEFINITION), a loader word with no path (E-LOADER-FORM, the load's E-MISSING-NAME), a retired or reopened loader word (E-UNREADABLE-SOURCE 74, the load's records at 70), and a missing or unreadable require (place 1:1, the load's 1:9); the default preverify (CHK-RUN-PREVERIFY, PREVERIFY-BYTES) writes records of its own before the run stage. `s" f" included` of a stream or a directory: the load's record has no place, check.f places it at the string (1:4); of a missing file the load places it at `included` (1:12), check.f at the string (1:4). Acceptance: check.f's first record on that file is line 1's, as the load's. Depends: habu-report-every-record-85422112. Claim: unassigned.
