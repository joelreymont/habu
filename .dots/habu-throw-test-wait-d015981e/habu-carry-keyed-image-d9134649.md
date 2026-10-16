---
title: Carry keyed-image child deadlines as 124
status: closed
priority: 2
issue-type: task
created-at: "2026-10-16T16:28:19.771553+03:00"
closed-at: "2026-10-16T23:46:56.499588+03:00"
close-reason: "Landed: keyed-image-reap-child's SAVE writes its expiry file, then names 'keyed-image-reap-child AWAIT-RELEASE' and exits 124, and BUILD exits 124 on E-PROC-TIMEOUT with the builder's capture; keyed-image-reap-test names 'keyed-image-reap-test build wait' on a 124 (EXITED), MARKED? and AWAIT-GONE, reads a builder's expiry file (BUILDER-EXPIRED) before KILL-BUILD's and KILL-BUILDER's checks, defers the live and killed directories' checks until their build's end is classified, and names the build wait when the kill ended the killed build (137) past the span of its own capture deadline (SUITE-BUDGET:CHILD-MS). Proofs: row keyed-image-reap rc 0; forced live, marked, gone, orphan-expiry, killed-after, cleanup-window and capture-race copies rc 67 with one named line and no TFAIL; a forced builder failure past the bound rc 1 with TFAILs, unnamed; master or earlier shapes forced the same way rc 1 with TFAILs or a masked timeout. Astra review: two Mediums (killed build's 124 discarded; orphaned builder's release deadline read as TFAILs), fixed with a per-builder expiry file; follow-ups found the kill during timeout cleanup (BUILDER-EXPIRED in KILL-BUILD), the killed build's capture deadline (causal bound) and a failure masked by that bound (137 only); fourth Astra follow-up APPROVE."
---

Problem: the parent's rule (a wait whose clock ends throws E-PROC-TIMEOUT after one named stdout line; a task records it and main throws it at the join; no deadline raised) is broken in the files below. The parent holds the shared interface (`TEST-WAIT:EXPIRED`, `TEST-WAIT:POLL` in src/lib/test/wait.f), the rulings and the proof notation `P(name)`.

**Files and edits**

- `test/keyed-image-reap-child.f:28–30,60–77`: remove `UNRELEASED-RC`; name SAVE expiry and exit 124. Add a BUILD entry boundary mapping caught `E-PROC-TIMEOUT` to 124, preserving other errors.
- Preserve available maker diagnostics through `KEYED-IMAGE:CAPTURE-OUT$` / `CAPTURE-ERR$`; the builder deliberately retains them on timeout (`tools/keyed-image.f:124–128,165–171`).
- `test/keyed-image-reap-test.f:231–280`: keep MARKED’s cleanup-aware result, change `UNMARKED` to kill/reap then report; make AWAIT-GONE a void throwing wait; recognize 124 in `EXITED`.
- Update consumers at `329,340,347,366,370,381`.
- `NEXT-BUILD:343–359`, `LAST-BUILD:368–376`, `FINISH-LIVE:378–389`, `MAIN:403–415`: retain the age/live-directory observations at their original points, but assert them after classifying the live child’s completion. Pass the few recorded values explicitly; add no status registry.
- The killed build's builder outlives its parent, so its 124 reaches no one: every builder gets an expiry file argument and writes it before it names its expired release wait. `KILL-BUILD` classifies the killed build's exit before asserting the kill; `KILL-BUILDER` names a recorded expiry once the builder is gone, before asserting the kill or the killed build's directory, which `NEXT-BUILD` observes and passes on.

**Comments**

Replace “never started” at `269–274` and “never released” in the child with “did not publish/release before the deadline.”

**Proof**

Run `keyed-image-reap`: `test/keyed-image-reap-test.f` (`gate-stdlib-cases.f:2631–2633`). Copies force missing mark and process-disappearance waits. A separate copy withholds only LIVE’s release and shortens only its child deadline, including expiry while the intervening builds run.

Require internal **124 → 124**, then `P(keyed-image-reap-test build wait)`, without earlier live-directory assertion failures.

**Acceptance**

Every child boundary preserves timeout. Existing killed-builder and cache-retention checks remain behavioral checks.

Files: test/keyed-image-reap-child.f, test/keyed-image-reap-test.f. Verify: the rows and forced copies under Proof. Depends: habu-share-a-named-85cc93f5. Ownership: the files above. Claim: agent=worker workspace=.jj-ws/jerry-wait-d12.
