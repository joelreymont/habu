---
title: "Restore a definer row's signature on a frame pop"
status: open
priority: 3
issue-type: task
created-at: "2026-10-02T18:39:44.828761+02:00"
---

Problem (lane 335 r4-inproc, reading): src/habu/verify-source.f:458 DEFINER-ADD rewrites a known definer's signature in place; if that row predates a checker rollback frame, popping the frame (VERIFY-DEFINER-N rewinds the count) does not restore the old signature. Narrowed by duplicate-definition refusals; not reproduced. Acceptance: reproduce through a rollback (a frame that redefines a definer's clause signature, then pops) or show it unreachable; if reachable, the row keeps its signature until the frame commits (append a new row instead of rewriting, as the count rewind expects). Files: src/habu/verify-source.f.

Review 356 adds a demonstrated member: a definer row survives `undefine` in one scope. src/core/checker.f CHECKER-UNDEFINE (~:12614) deletes the signature and appends a NORET retraction but keeps the symbol id, and src/habu/verify-source.f UNDEFINE-WORD (~:1140) calls only that, so the row keyed by the id stays: $HOME/.cache/tmp/kestrel-r4-rev356/undef.f (`: RV-D ( n -- ) create , does> ( -- n ) @ ;` / `undefine RV-D` / `: RV-D ( -- ) ;` / `RV-D` / `: RV-USE ( -- n ) RV-NOPE ;`) checks rc 70 at the run stage (E-UNDEFINED RV-NOPE, no "preverify failed"): the top-level RV-D is read as a definer that swallows the next `:`; same on the old engine. Acceptance now also: undefine (and any redefinition of a sym) drops or retracts its definer row, undef.f refused at the pre-pass, seen failing first.
