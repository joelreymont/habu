---
title: put the recovery check on a scheduled gate
status: open
priority: 1
issue-type: task
created-at: "2026-08-22T22:38:25.803635+02:00"
---

Problem: no scheduled gate runs HABU_BOOTSTRAP_CHECK_ONLY=1 tools/bootstrap.sh; the seam break (760e9c90, 2026-08-14) and the text-page cliff both landed unseen, and the July-21 P1 dot sat open for a month. docs/bootstrap.md:79 and README:321 advertise a recovery that did not work. Acceptance: a gate slice (test/run.f phase or a named slice in test/gate-stdlib-lib.f) runs the recovery check through the real tools/bootstrap.sh on every full gate; the slice is listed in docs/gate.md and the docs/forth.md commit gate names it for diffs touching bootstrap/, src/core, src/habu; the check fails loudly without gforth (no skip logic). Files: test/run-lib.f, test/gate-stdlib-lib.f, docs/gate.md, docs/forth.md. Verify: bin/hb --load test/run.f schedules and passes the slice; deleting one mirror publish word turns it red. Depends: 9269e3a3 and the text-page dot (the leg must be green before it can gate). Ownership: gate runner. Claim: unassigned.
