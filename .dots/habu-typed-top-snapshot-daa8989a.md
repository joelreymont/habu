---
title: "typed-top: snapshot/AOT effect-row parity"
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T10:38:59.771262+02:00"
---

Sub-dot 6 of docs/typed-top-level.md sec 5 (landed 8cefda08). Files: src/habu/habu2.f + src/habu/aot-lib.f snapshot/AOT slices, test/top-row-snapshot-test.f. Acceptance: a snapshot/AOT image reproduces identical tier-1 warnings and tier-2 rejects for p1/p2/p3 as a cold source load (EFF-REC/ER.SYM persistence, now for full rows). Depends: tracker sub-dot.

Snapshot DATA rebasing prerequisite landed at db68192c.


STOP-THE-LINE RCA 2026-07-15 (tsnap lane, no commit - investigation only): the
dot's premise was UNDERSTATED. Snapshot boots did not skip the tier-1 tracker;
a captured-ARMED TOP-HOOK-CELL made EVERY snapshot boot SIGSEGV (rc 134, even
on an empty program) and test/owner-wid-child.f was red on master. Root cause
(lldb-proven, deterministic): on warm boot the tracker fires on the unguarded
`provided` re-establishment rows (PFX-PROVIDE-FILES, habu2.f:1058 - cold boots
arm AFTER them, so cold never sees them); the tier-1.5 sig-store scan
(EFFECT-QUERY -> FIND-SIG) for the sig-less token then dereferences an
UN-REBASED build-time engine-__text pointer persisted at DATA offset 0x378729
(stale base 0x102360000 vs live 0x100001000). The checker itself works on
snapshots - only the tracker's sig-less-token full-store scan reaches the
stale pointer. MITIGATION LANDED (integrator, snap-lib.f SND-ZERO-LIVE +
TOP-HOOK-CELL SND-ZERO-CELL): images capture the hook DISARMED - snapshot
boots are clean and owner-wid-child is green; the tracker is tracker-OFF on
snapshot boots (the behavior sub-dot 3 had claimed all along).
REMAINING SCOPE (why this dot stays open) - two sub-problems the original
framing missed:
(1) arm timing: re-arm at the cold-equivalent point (after `provided` rows,
before user source);
(2) a NATIVE re-arm mechanism: prefix-package words are NOT name-findable on
warm boot (TOP-ROW:TR-INSTALL and bare TR-INSTALL both E-UNDEFINED on the
snapshot even via package re-open; they resolve on cold) - needs a
`top-check-rearm` prim reading a saved TR-HOOK xt, or a computed-address
`@ set-top-check`.
After both land, test/top-row-snapshot-test.f must assert identical p1/p2/p3
warnings per the original acceptance. Investigation artifacts (image cell
dumper, text-band pointer scanner, lldb scripts) were session-temp; rebuild
from this note's pointers.

CROSS-REF 2026-07-15: the shared persisted-pointer defect (the un-rebased DATA
pointer, stale engine-text base + 0x7220) is the SAME defect sol hit independently via
`snap --force` and now owns as habu-snapshot-rebase-persisted-4bd33351 (their
lldb shows the identical +0x7220 dereference; tsnap's evidence - DATA offset
0x378729 feeding FIND-SIG's sig-less-token scan - belongs in that
investigation). The shared persisted-pointer fix landed at db68192c. This dot
retains parts (1) arm timing and (2) the native re-arm mechanism.
