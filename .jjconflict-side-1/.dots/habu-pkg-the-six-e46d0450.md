---
title: Package the six snapshot relocation engine words
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T21:34:38.649724+02:00"
---

Full context: the step-2 relocation WIP (commit e50fb3ec in .jj-ws/habu-relocate-snapshot-region-752042fe) introduces six new words in src/habu/habu2.f: LSNAPCALL, SNAPCALL-MSG-LEN, BL-OPCODE-HI, LSNAPRBC, EM-SNAPSHOT-REBASE-CALLS, EM-SNAPSHOT-REBASE-DATA-XT. The ENGINE-BODY-EDIT exemption from commit a943eb40 deliberately admits only body edits - new global definitions in habu2.f still fail the package gate (verified 2026-07-29 by probe: new global PROBE-GLOB rc=1). These six words need a real package owner (or placement in an already-packaged snapshot module) before the WIP can commit. Depends on the layout.f/snap-lib.f packaging dot only if the words move into snap-lib.f. Acceptance: package-diff-lint exit 0 on the exact step-2 diff; engine fixpoint rebuild; the 200-consecutive-clean-boot acceptance run from the parent relocation dot still applies to the campaign, not this dot.

Claim: agent=snapreloc workspace=.jj-ws/habu-relocate-snapshot-region-752042fe (RELEASED 2026-08-21: workspace gone, no live lane - gc)

MEASURED 2026-07-29 (agent=snapreloc, workspace .jj-ws/habu-relocate-snapshot-region-752042fe).

What was done. A real package, SNAP-RELOC, now owns the whole snapshot
relocation subsystem. It is opened in src/habu/layout.f for the band offsets and
exit statuses, reopened in src/habu/habu2.f for the labels, the one instruction
constant and the emitted passes, and reopened in src/habu/snap-lib.f for the
writer's half. The six words the dot names became short package-local tails:
LSNAPCALL -> SNAP-RELOC:LCALLMSG, SNAPCALL-MSG-LEN -> SNAP-RELOC:CALLMSG-LEN,
BL-OPCODE-HI -> SNAP-RELOC:BL-OP-HI, LSNAPRBC -> SNAP-RELOC:LCALLS,
EM-SNAPSHOT-REBASE-CALLS -> SNAP-RELOC:EMIT-CALLS, and
EM-SNAPSHOT-REBASE-DATA-XT was replaced by the table-driven
SNAP-RELOC:EMIT-XT plus SNAP-RELOC:EMIT-MARK and SNAP-RELOC:MARK-CELL. The new
constants the step-2 work introduced went into the same package rather than
staying global: CALLMAP-BYTES/OFF/END, CALLMAP-RC (was SNAP-CALLMAP-RC), and the
address-cell table constants XTCELL-CAP/N-CELL/ROWS-OFF/END/RC.

Two words were removed from the diff rather than packaged, by fixing the code
that had made them change at all. The step-2 WIP had deleted LMMAPCODE and its
message length and rewritten the shared declaration line that also declares
LMMAPDATA, which put a pre-existing engine word into the gate's sights and
forced a matching edit to tools/bootstrap-codegen-test.f (which would then have
pulled that whole test file into the packaging work). The deletion was wrong on
its own terms: with the region no longer pinned to an exact address, a failed
mmap was being detected only by the BL-range assertion, so a genuine
out-of-memory failure would have been reported as "code region out of BL range".
EM-MMAP-CODE-REGION now tests the syscall error the kernel actually reports --
SYS, leaves the same carry-flag convention on both targets, Darwin natively and
the Linux emitter by reconciling -errno -- and keeps the accurate exit 78
diagnostic. LMMAPCODE and MMAPCODE-MSG-LEN are alive and unchanged,
tools/bootstrap-codegen-test.f is out of the diff entirely, and the mmap failure
path is named again rather than mislabelled.

Gate output on the exact working-copy diff against parent 3baa62b0
(bin/hb --load tools/package-diff-lint.f -- <artifact>):

  E-PACKAGE-OWNERSHIP src/habu/layout.f:41:12: `SNAP-FORMAT-VERSION` ...
  E-PACKAGE-OWNERSHIP src/habu/layout.f:652:32: `DATA-START` ...
  E-PACKAGE-OWNERSHIP src/habu/snap-lib.f:259:3: `SNAP-CANON-DATA` ...
  exit 1

Nineteen findings before, three after. All three are pre-existing global words in
layout.f and snap-lib.f whose bodies this work had to change (the snapshot format
version, the DP-heap start that the new bands move, and the writer entry point
that calls the new pass). Those two files belong to the parallel packaging lane,
so this lane deliberately did not repackage them; the gate cannot reach exit 0
here until that lane lands. Every word this work introduced has an owner.

tools/typed-local-diff-lint.f on the same artifact: exit 0.
bin/hb --load tools/error-code-lint.f: 1323 files, 844 claims, 39 reservations,
0 findings, exit 0.
Engine rebuild: HB_TMP=<private> bin/hb --load tools/build-fixpoint-refresh.f
-- snap reports "self-check census (macos-arm64): 0 uncheckable, 0 rejected,
certified = 4236", "bin/hb refresh OK: compiler fixpoint", "snapshot image OK:
candidate validated".

OPEN CONFLICT with the orchestrator's later instruction. The instruction that
arrived during this work says layout.f must not receive new definitions at all,
because packaging it is blocked on stage0 `using` support (dot
habu-add-using-to-d815f0ab). The two new bands cannot live anywhere else: their
offsets are derived from USE-BAND-END, which layout.f defines, and DATA-START,
which layout.f also defines, has to be derived from the end of the new bands, so
any other file would have to sit both after and before layout.f. What is in
layout.f now is a package block containing only the new constants; none of
layout.f's existing globals were packaged, and the package block uses only
`package`/`public`/`;package`, not `using`. The package gate accepts it. If the
stage0 recovery compiler cannot open a package that early, the alternative is to
split layout.f at the band boundary, which is a separate leaf.

Is this the best long-term solution or a patch? Long-term. The subsystem now has
one owner across all three files it spans, the private helpers are private, and
the public surface is exactly the labels and passes other engine regions call.
The one judgement call worth re-deriving is the mmap change: it rests on the
syscall ABI's own error signal, which SYS, already normalises across both
targets, rather than on any property of the returned value, so it is a structural
test and not a value heuristic. The residual three gate findings are a lane
boundary, not a weakening.

UPDATE 2026-07-29 (agent=snapreloc), after rebase onto proofs 2511e0a2.
The SNAP-CANON-DATA finding is gone: on proofs that word is CANON-DATA inside
`package SNAP`, and the rebase conflict was resolved by folding the new pass call
into the packaged body. The writer's four helper words were moved out of a nested
`package SNAP-RELOC` reopen and into SNAP itself with SND-XT-* tails, because the
nested package broke the snapshot build (child exit -2802); see the report in dot
habu-relocate-persisted-defer-7aa681c4.
Two findings remain, both in layout.f and both explained in dot
habu-admit-layout-f-7e317a72: the engine-trunk admission now covers layout.f, but
its DEF-TAIL-ADDED key cannot admit a `constant` whose VALUE changed, because the
definer, the name and the value share one line. That needs the old-side name
lookup described there, which is a separate leaf.
