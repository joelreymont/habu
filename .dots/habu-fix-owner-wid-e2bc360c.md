---
title: Fix owner-wid snapshot trailer corruption
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-23T12:40:54.337084+02:00\""
---

Stop-the-line master red, found 2026-07-23: enrolled suite owner-wid-internal (test/gate-stdlib-cases.f line 604) fails deterministically on master - test/owner-wid-child.f builds hb-stdin from current source with the bundle-injected owner-wid emitter (BUILD-EXT:OWNER-WID-BUILD sets test/owner-wid-emitter.f and keeps test/owner-wid-source.f), the snap step produces hb-new and prints candidate validated, then hb-new dies at boot with hb: snapshot trailer corrupt, exit 79, and the suite reports 22 assertion failures. Reproduced identically with the stale 07-22 binary and a fresh fixpoint-refreshed binary (census 4027; in-child bundle census 4063), across three separate HB_TMP roots and two workspaces, so it is source-determined, not environmental. The PLAIN fixpoint refresh and install work (bin/hb rebuilt and boots), so the defect is specific to the injected-bundle image: writer (snap-lib.f SNAP code run by the fresh hb-stdin) and reader (habu2.f EM-SNAPSHOT-RESTORE baked into hb-new) are BOTH current source yet disagree - suspect the trailer or region-length arithmetic (snap-lib.f SNL computation, 48-byte format-versioned trailer) breaks under the bundle-grown image size or content, or a recently landed engine/checker commit changed emitted code the snapshot arithmetic depends on. The red was invisible because the gate-stdlib pool fail-fasts after any earlier red phase and the routinely-run slices did not include this pool. Investigation is debugger-evidence-first per docs/debugging.md: dump the emitted image trailer (scratch tooling and gdb acceptable as evidence; the permanent tools/ dumper is split to habu-build-snapshot-trailer-96945cb1 by orchestrator order after two peer reviews), compare writer-computed offsets against loader-expected offsets on the failing artifact, identify the violated invariant with file:line, fix at the owning source, and add a regression that boots the bundle-injected image through the real child path. Identify the first red master commit (content-keyed fixpoint bisect) only if the direct artifact dig does not pin the cause. Acceptance: test/owner-wid-internal.f standalone exit 0; the full test/gate-stdlib.f pool green twice consecutively; the plain refresh still byte-identical fixpoint; regression red on the pre-fix tree; both diff lints exit 0. Blocks every master fast-forward until closed.

INTEGRATION STATE (2026-07-23): the accepted functional payload a2c4ec40 (both RBASE-VA sites plus the name-length pin, destruction-accepted with zero corrections) is HELD; no implementation work remains on it. Integration is blocked ONLY on redesigning the smallest complete atomic package/caller closure for the restore family - no migration exception - after the migration-neutrality approach was rejected. The claim stays with this owner because the accepted payload remains in integration.

Claim: RELEASED 2026-07-29 by the stale-claim audit. Agent `owidrca` and workspace `.jj-ws/habu-fix-owner-wid-e2bc360c` are both gone: the directory does not exist and `jj workspace list` has no record of it. WARNING for the next owner: the held payload named above is gone with the workspace - `jj log -r a2c4ec40` reports "Revision `a2c4ec40` doesn't exist", so the accepted RBASE-VA fix is not recoverable from this repository and has to be redone. The only related commit still present is e3d1df57bdb1 "Publish owner-wid incident-repair claims", which is dot text and carries no repair. The dot stays active and is free to claim.

CORRECTION 2026-07-29 (orchestrator): an earlier note in this session claimed
the accepted payload a2c4ec40 was DESTROYED by a workspace sweep. That claim
was not established. What is verified: a2c4ec40 is absent from all 500
operations in this repository's operation log. That is consistent with the
lane having run on another machine (this dot's sibling claims carry
machine=spark tags) and NOT with destruction here. jj retains abandoned
commits — they stay reachable via `jj --at-op`. Before treating the payload as
gone, check the spark repository. The re-derived fix is recorded in
habu-fix-snapshot-wid-f8817137 either way, so no work is blocked on the
answer.

