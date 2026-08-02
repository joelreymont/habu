---
title: Make pre-hook core throw codes nameable
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T11:41:33.920474+02:00"
---

Why: constants defined before the check hook installs are runtime-visible but checker-invisible. The live owner is src/core/util.f:77 E-PATH-RANGE, thrown by PATHZ at src/core/util.f:78-81; checked callers therefore cannot name the same code that the runtime throws. Measured red on current master: stdin `: P ( -- n ) E-PATH-RANGE ;` exits 70 undefined; stdin `: Q ( n -- n ) CORE-FOLD-C ;` exits 70 undefined/non-certified. Result: make pre-hook error-code constants nameable by checked code without certifying arbitrary pre-hook helpers. Owner: src/core/util.f:77 and the pre-hook constant-certification seam; focused test owner: test/engine-suite.f. Acceptance: a checked-candidate positive in test/engine-suite.f names E-PATH-RANGE and certifies; a hostile checked candidate naming the unmodeled pre-hook helper CORE-FOLD-C remains non-certified; the real engine suite and owning native gates pass.
