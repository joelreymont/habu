---
title: Keep checking after an export rejection under --all-errors
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T07:23:10.712164+01:00"
---

Problem: package QX public : OTHER ( -- n ) 2 ; ;package, then package QC private EXPORT QX:NOPE : T2 ( -- ) undefined-word ; ;package (one statement per line): --all-errors reports only E-EXPORT-UNDEFINED 7:8, since RECORD-EXPORT (src/habu/verify-source.f) rethrows every rejection but a duplicate and the run ends (the d5135 re-review; master behaves the same). Recheck once W201 (check.f's walk removed) lands. Acceptance: --all-errors also reports T2's E-UNDEFINED, as it does after a duplicate export. Depends: W201 landing. Claim: unassigned.
