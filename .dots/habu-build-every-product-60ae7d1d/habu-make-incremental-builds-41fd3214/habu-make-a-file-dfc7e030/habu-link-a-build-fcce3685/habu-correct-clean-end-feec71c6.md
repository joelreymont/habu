---
title: "Correct CLEAN-END's comment for a package opened and closed in the buffer"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-05T23:20:37.960316+02:00"
closed-at: "2026-11-06T01:07:38.391541+02:00"
close-reason: "CLEAN-END's comment and layout.f EVAL-FRAME:PKG-OUTSIDE now state the behaviour (a buffer that ran `package` leaves PKG-OUTSIDE-CELL 0, read only in a package); review REVISE fixed inline; habu2.f's twin comment is a case in habu-move-the-interpreter-d35780fa"
---

Problem: src/habu/interpret.f CLEAN-END's comment says that unless the buffer left a package open, the includer's PKG-OUTSIDE-CELL comes back. A buffer that opens and closes a package leaves the includer's cell at 0 (CLEAN-END restores only when the cell is nonzero; habu2.f's engine copy does the same). The cell is read only while a package is open and every scope restore writes it back, so no keyword sees it (S5b1b fold review, probe /home/joel/.cache/tmp/rv-fold.1yHd/q3.f stage 1). Acceptance: the comment states what CLEAN-END does; if a case can observe the zero, restore the value instead. Files: src/habu/interpret.f. Verify: bin/hb --load test/run.f -- linker. Depends: none. Ownership: jerry. Claim: unassigned.
