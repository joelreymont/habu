---
title: Keep top-level cells live across nested evaluate and hook changes
status: active
priority: 2
issue-type: task
created-at: "2026-10-26T19:10:40.215139+02:00"
---

Rule defects on the candidate (repros /Users/joel/.cache/tmp/carl-mcatch.IpcS/probe/pr/): A) TRUSTED: W ( -- ) s" 1 drop" evaluate ; then ' W catch . refuses 'stale cell' at '.'; B) after a 0 set-check window and re-arm, later cells read stale (R-CATCH-STALE, prop, shadowed-arity :184); C) after 0 set-current, VAR @ refused 'catch left stale'. All in CHECKER-TOP-LOAD/SAVE; fed into the literal-cost design (Plan running), fix lane follows it.
