---
title: Split HB build maker and cache
status: open
priority: 1
issue-type: task
created-at: "2026-07-15T23:50:57.703346+02:00"
blocks:
  - habu-split-hb-build-c3bdd7eb
---

Full context: after compose and lints extraction, move maker key/source/build lifecycle into tools/hb-build-maker.f and artifact/object key, lock, restore/store/install lifecycle into tools/hb-build-cache.f under reopened HB-BUILD. Rename private collisions RESTORE-OBJECT? and BUILD-ELAPSED; keep cache keys byte-identical and add every component to the maker closure key. No public cache internals. Acceptance: cold, maker, artifact and object hit/miss/invalidation fixtures pass; lock/error paths propagate; no legacy names resolve.

Claim: RELEASED 2026-07-20 (lane stopped with evidence, no edits; orchestrator mis-dispatched from the open list instead of `dot ready` - this dot's blocks: c3bdd7eb was already correct). Stop evidence: this is step 3 of the strict extraction chain (f854f76a state+CLI incl. the HBB- prefix strip -> c3bdd7eb compose+lints -> this -> 6e53c639 facade cut); steps 1-2 have not landed, tools/hb-build-{state,cli,compose,lints}.f do not exist, hb-build-lib.f is still the intact HBB- monolith, and the dot's two renames only exist as collisions AFTER the strip (HBB-OBJECT-HIT? lib.f:933 vs HB-BUILD:OBJECT-HIT? report.f:149; HBB-ELAPSED-NS lib.f:88 vs HB-BUILD:ELAPSED-NS report.f:165). Ready-made split plan for the re-dispatch: maker.f = MAKER-KEY!/MAKER-NAME!/MAKER-CACHE$/MAKER-SOURCE/STAGE2-SOURCE/BUILD-MAKER + key-source builders (KEY-LOAD-FILES/KEY-*-SOURCES/APPEND-DRIVER); cache.f = artifact/object key + SUFFIX!/paths, lock (TRY-ARTIFACT-LOCK?/RELEASE/INSTALL-ARTIFACT-LOCKED), restore/store/install/object lifecycle with the two renames; both reopen HB-BUILD, no public cache internals.

REOPENED 2026-08-04 (dot-purge): this dot carried `status: active` with no live owner - no `agent=`/workspace claim, or a claim explicitly released. An active dot with no owner is invisible to `dot ready` and holds its id hostage, so the status is now `open` and the dot is free to claim.
