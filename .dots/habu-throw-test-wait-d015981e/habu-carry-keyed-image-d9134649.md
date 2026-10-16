---
title: Carry keyed-image child deadlines as 124
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T16:28:19.771553+03:00"
blocks:
  - habu-share-a-named-85cc93f5
---

Problem: the parent's rule (a wait whose clock ends throws E-PROC-TIMEOUT after one named stdout line; a task records it and main throws it at the join; no deadline raised) is broken in the files below. The parent holds the shared interface (`TEST-WAIT:EXPIRED`, `TEST-WAIT:UNTIL` in src/lib/test/wait.f), the rulings and the proof notation `P(name)`.

**Files and edits**

- `test/keyed-image-reap-child.f:28–30,60–77`: remove `UNRELEASED-RC`; name SAVE expiry and exit 124. Add a BUILD entry boundary mapping caught `E-PROC-TIMEOUT` to 124, preserving other errors.
- Preserve available maker diagnostics through `KEYED-IMAGE:CAPTURE-OUT$` / `CAPTURE-ERR$`; the builder deliberately retains them on timeout (`tools/keyed-image.f:124–128,165–171`).
- `test/keyed-image-reap-test.f:231–280`: keep MARKED’s cleanup-aware result, change `UNMARKED` to kill/reap then report; make AWAIT-GONE a void throwing wait; recognize 124 in `EXITED`.
- Update consumers at `329,340,347,366,370,381`.
- `NEXT-BUILD:343–359`, `LAST-BUILD:368–376`, `FINISH-LIVE:378–389`, `MAIN:403–415`: retain the age/live-directory observations at their original points, but assert them after classifying the live child’s completion. Pass the few recorded values explicitly; add no status registry.

**Comments**

Replace “never started” at `269–274` and “never released” in the child with “did not publish/release before the deadline.”

**Proof**

Run `keyed-image-reap`: `test/keyed-image-reap-test.f` (`gate-stdlib-cases.f:2631–2633`). Copies force missing mark and process-disappearance waits. A separate copy withholds only LIVE’s release and shortens only its child deadline, including expiry while the intervening builds run.

Require internal **124 → 124**, then `P(keyed-image-reap-test build wait)`, without earlier live-directory assertion failures.

**Acceptance**

Every child boundary preserves timeout. Existing killed-builder and cache-retention checks remain behavioral checks.

Files: test/keyed-image-reap-child.f, test/keyed-image-reap-test.f. Verify: the rows and forced copies under Proof. Depends: habu-share-a-named-85cc93f5. Ownership: the files above. Claim: unassigned.
