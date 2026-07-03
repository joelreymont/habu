---
title: Retire snapshot restore machinery
status: open
priority: 2
issue-type: task
created-at: "2026-07-02T21:50:58.476020+02:00"
---

Once the AOT-REPL binary emission lands (habu-decide-unbake-repl-735b1565: compiled REPL goes into the binary via the EM-SEED-DICT-style relative-record seed path; NO restored images ever, per user architecture decision 2026-07-03), delete the runtime restore machinery: EM-SNAPSHOT-RESTORE and the LSNAPRBD/LSNAPRBC loader relocation walks in src/habu/habu2.f, the snap-rebase primitive (habu1 emitter + checker PRIM row), the snapshot trailer format + validation, the '-- snap' command's candidate-restore validation in tools/build-fixpoint.f, src/habu/snap.f + src/habu/snap-lib.f writer (the SND-ZERO/SND-QUARANTINE canonicalization knowledge must be transcribed into the AOT emitter's persistent-vs-runtime cell classification BEFORE deletion - it is the authoritative map of which data cells are content vs live state), and the SNAP-CELL branch (cwok) in EM-STARTUP-RUNTIME-STATE if nothing else uses it. Also remove the snapshot rows from TRUSTED.md and any gate phases that exercise restore. Precondition: AOT milestone 2 green (baked source text dropped, gate green, binary size recorded). Do NOT retire before then - the snap command is currently the only harness exercising the object image writer end-to-end.

## ACCELERATED (2026-07-03, user decision): remove NOW, not after AOT
"All the warm image stuff needs to be removed." The wait-for-AOT
precondition is DROPPED. snap.f's header states its purpose: restore
regions and "boot WARM" - this whole surface is the warm-image machinery
and it goes now. If the object image writer needs an end-to-end harness
after snap is gone, give it a focused test instead.
COMPLETE REMOVAL INVENTORY (verified against the tree):
- src/habu/snap.f, src/habu/snap-lib.f (writer, SNAPGO, SND-*/SNC-*
  canonicalization, SND-QUARANTINE) - FIRST transcribe the persistent-vs-
  runtime cell classification (SND-ZERO-LIVE set + quarantine buckets +
  eval-frame/rstk windows) into the AOT dot habu-decide-unbake-repl as
  the emitter cell map; it is the one durable output.
- tools/build-fixpoint.f: BF-APPEND-SNAP-KEEP/-REPL/-MARK/-BUILD,
  BF-EMIT-SNAP-RUN-SOURCE, BF-BUILD-SNAP, the "snap" command wiring,
  BUILD-SNAP-HDR trust row (line 582 area).
- src/habu/habu2.f (BAKED): EM-SNAPSHOT-RESTORE, LSNAPRBD/LSNAPRBC
  parameterized walks + their EM-SNAPSHOT-REBASE-DICT/CALLS emitters,
  trailer detection in startup, SNAP-CELL cwok branch in
  EM-STARTUP-RUNTIME-STATE (keep the non-snap arm's stores), snapshot
  layout constants that lose all users.
- src/habu/habu1.f (BAKED): BSNAPREBASE prim emitter + its FPRIM
  registration (s" snap-rebase").
- src/core/checker.f (BAKED): snap-rebase PRIM row; CHECKER-SNAPSHOT-
  PREPARE, USIGS-SNAPSHOT-PERSIST, NORET-SNAPSHOT-PERSIST if no other
  caller remains. src/core/include.f: INCLUDE-SNAPSHOT-PREPARE ditto.
  Hooks SNAP-CHECK-HOOK/SNAP-INSTALL-HOOK + their HOOK-INSTALL entries.
- TRUSTED.md: all snap/snc/snd rows + baseline decrement (big win for
  the trusted ratchet); layout.f SNAP-CELL/STB/SDB-class constants that
  lose all users; engine-suite/gate tests exercising restore.
SEQUENCING: three running workers currently own snap-lib.f/build-
fixpoint.f (opus-tools), checker.f (opus-checker), habu2.f (opus-engine).
Execute this removal AFTER their in-flight batches merge (same day), as
ONE dedicated pass: transcribe cell map, delete, refresh (byte-for-byte
fixpoint), re-pin/remove TRUSTED rows, full gate, expect a binary-size
DROP (update test/gate-build-size.f baseline downward - shrink is a
ratchet failure by design, so the baseline edit is part of the commit).
NOTE: opus-tools dot 4 (quarantine owners) acceptance criterion (snap
byte-identity) dies with snap; keep only its owner-map findings.

## SCOPE CORRECTION (2026-07-03, user): WRITER STAYS. Only warm-image dies.
I over-scoped this dot. User clarification: delete the WARM-IMAGE concept
(restore-at-runtime as a warm boot), NOT the snapshot writer. The writer
(snap.f/snap-lib.f/BF-APPEND-SNAP-* build flow) is how habu builds
binaries carrying compiled content - it is the seed of the AOT binary
emitter (habu-decide-unbake-repl) and is KEPT and evolved. The checker/
include persist hooks (CHECKER-SNAPSHOT-PREPARE, USIGS-SNAPSHOT-PERSIST,
NORET-SNAPSHOT-PERSIST, INCLUDE-SNAPSHOT-PREPARE) are writer-side and
KEPT. REMOVAL SCOPE NARROWED TO: the warm-boot restore path as a RUNTIME
mechanism once the AOT emitter produces OS-loaded binaries (the endpoint
is segments mapped by the OS loader, not a startup restore pass) - i.e.
EM-SNAPSHOT-RESTORE + trailer detection + SNAP-CELL warm branch get
retired ONLY when AOT milestone 2 lands and the writer emits directly
loadable binaries. Until then the restore path stays (it is the writer''s
validation consumer). The earlier "remove NOW" inventory in this dot is
VOID where it names writer components; the cell-classification transfer
note stands.

## FINAL PLAN (2026-07-03, user-approved) - remove when no longer needed
Route B is authoritative: (1) extend the hb-build AOT object-writer path
with PERSISTENT DATA REGION emission (the gap named by aot-closure.f''s
own diagnostic "stripped AOT has no persistent data region"), using the
snapshot work''s cell-classification map (persistent content vs runtime
state - SND-ZERO-LIVE set, eval-frame/rstk windows, quarantine buckets,
USIGS-P/NORET-P are load-bearing) as the data-region content spec;
(2) AOT-compile the REPL/debugger into bin/hb through that hb-build
path (habu-decide-unbake-repl milestones now target this, NOT the snap
writer); (3) THEN delete ALL snap machinery in one pass - writer AND
restore: snap.f, snap-lib.f, BF-APPEND-SNAP-*/BF-BUILD-SNAP/the snap
command, EM-SNAPSHOT-RESTORE + relocation walks + trailer + SNAP-CELL
branch, snap-rebase prim (habu1 emitter + checker row), CHECKER/INCLUDE
SNAPSHOT-PREPARE + persist hooks, SNAP-CHECK/INSTALL hooks, all
TRUSTED.md snap rows (baseline shrinks), snap layout constants, snap
tests. Trigger condition: AOT milestone green (bin/hb boots with
compiled REPL via object-writer emission, baked source text dropped,
full gate green). One build lineage remains: habu builds binaries via
the object writer. Nothing restores, ever.
