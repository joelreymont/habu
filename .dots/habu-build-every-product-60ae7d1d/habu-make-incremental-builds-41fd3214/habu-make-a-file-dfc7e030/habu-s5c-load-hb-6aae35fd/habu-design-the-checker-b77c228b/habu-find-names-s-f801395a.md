---
title: "Find names:'s record by its symbol, not a stale BIND-REC"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-07T06:39:54.620636+02:00"
closed-at: "2026-11-10T21:50:20.626250+02:00"
close-reason: "done: src/core/checker.f:29080-29083 CHECKER-NAMES-ROW takes the selected record from TOP-BINDING-REC"
---

Problem: src/core/checker.f CHECKER-NAMES-ROW (:28283 at tmmqutun) writes NORET-ADD-SYM (:28296) and the link row RECORD-CONTROL-AT! (:28297) of the record in BIND-REC @ (:28295), but its symbol comes from TOP-BINDING (:22987), whose VIS-PICK restores BIND-REC (:22945, :22969), and drec is dropped by `rot drop` (:22989). So the record written is probably stale (likely `names:` itself) and its link-row control is overwritten with the named word's. Found by the C2d amender census (Explore agent, 2026-11-07, tree ltprsmov); not yet reproduced. Acceptance: a probe that declares `names:` on a word and then checks a caller of `names:` and of the named word shows the defect or refutes it; if shown, CHECKER-NAMES-ROW writes the record TOP-BINDING found, and a rejected-program test covers it. Files: src/core/checker.f. Verify: candidate build; the probe; bin/hb --load test/run.f -- checker-soundness. Depends: none (C2d's design may absorb it). Ownership: checker.f CHECKER-NAMES-ROW. Claim: unassigned.
