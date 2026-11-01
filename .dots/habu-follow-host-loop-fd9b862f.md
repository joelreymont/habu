---
title: Follow host loop repeats and exit in clobber lint
status: open
priority: 3
issue-type: task
created-at: "2026-11-01T09:12:30.301014+02:00"
---

Problem: after lane sealed fold k the clobber lint (tools/lint/clobber-lint-core.f FLOW-STEP) walks host control words as Forth's control-flow stack, but three shapes stay unmodeled (review /tmp/jerry-sealed-rv6-tmp). (1) A host loop body is walked once, though the emitter writes it once per turn: a call at the end of one copy that clobbers a register the next copy reads is missed. (2) A host `exit` is ignored, so the code after `cond if … exit then` also sees the arm's state; inside a quotation `exit` leaves only the quotation. (3) fx/cross.f: an `if` opened in routine A and closed after routine B's opening; pass 1 stops A at B's opening and never resumes A after the arm's closer (0, true 1). Acceptance: each shape a fixture asserted in tools/lint/clobber-lint-test.f failing before; loop repeats through the walk-until-stable loop pass 2 already runs for label back edges; the clobber-lint row clean with its routine and call counts explained. Verify: the clobber-lint row. Depends: lane sealed landed (habu-answer-tools-check-745b0ccd). Claim: unassigned.
