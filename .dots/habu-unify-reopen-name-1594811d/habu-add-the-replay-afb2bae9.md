---
title: Add the replay-record and record-wid! writers
status: open
priority: 1
issue-type: task
created-at: "2026-10-01T18:16:50.965037+02:00"
blocks:
  - habu-share-reopen-name-92885254
---

Leaf 3 of habu-share-reopen-name-92885254 (replay overlay, stage 1). Two engine rows for a Habu checker overlay: `replay-record ( ptr u8 n n -- )` publishes a codeless record (name, wid; [0] a trap leaf `hb: replay record executed` exit 76; wid -1 = a namespace row with public and private wids) with alias-record's guards minus OPEN-WID; `record-wid! ( n n -- )` rewrites record[40] for an index below NDICT that is neither seeded nor a namespace row, refusing wid -1 and a live task. Both registered at wid 0 (ARM64 FPRIM, x86-64 REFUSE), checker rows trusted-only in prims.f, seed twins in bootstrap/cg/forth.fs if the recovery chain compiles their callers. No new TRUSTED: site. Acceptance: test/engine-writers.f rows for both (bindings, trap exit, refusals 83/79, checked caller refused); fixpoint census 0/0/6052; test/run.f; generations; gforth recovery check. Blocks leaf 4. Worker: worker. Brief: the Plan section below.

### Leaf 3 brief
Goal: the engine gains `replay-record` and `record-wid!`, codeless-record publication and wid rewriting, so a Habu checker overlay can place and hide dictionary records; nothing in the checker changes. Read the corrections in habu-bind-replayed-names-24dce136 (items 8-9 apply here).
- habu2.f, inside `package DEFWRITE`, modeled on ALIAS-RECORD and NAMESPACE-RECORD: `REPLAY-RECORD` for `replay-record ( ptr u8 n n -- )` name, wid. wid = DICT-WL:NAMESPACE -> namespace row: [0] fresh wid, [8] second fresh wid (WIDN-CELL $30 advanced by 2), [40] -1, refuse a colon in the name. Other wid -> guards REAL-WID, RETIRED only, NOT-PENDING, DICT-ROOM, NAME-SIZE, FRESH, B-TASK-LIVE-GUARD; no OPEN-WID (the prefix certify replays sealed packages); [0] = a new trap leaf `hb: replay record executed` exit 76 (label beside LSCOPEREC, body beside EMIT-SCOPE-REC in the section list), [8] 0, flags 0, name via NAME-BANDS,/NAME-COPY, PUBLISH,. `RECORD-WID` for `record-wid! ( n n -- )` wid, index: refuse index >= NDICT (unsigned), index < dict[LNCOUNT] (seeded rows), a namespace row ([40] = -1), wid = -1, task-live; write [40] through REC-SPAN,/PROT:LCLOSE (NAMESPACE-PRIVATE shape). Register beside `ndict!` with FPRIM (wid 0), not GLOBAL-INT-WID.
- prims.f after scope-find: `EPRIM: replay-record PE-PTR-U8 PE-IN PE-N PE-IN PE-N PE-IN EPRIM; ETRUSTED-ONLY!` and `EPRIM: record-wid! PE-N PE-IN PE-N PE-IN EPRIM; ETRUSTED-ONLY!`, comment stating both refusals exit (83/79) and the namespace mode. The owner-private EPPRIM rows belong to leaf 4.
- kernel-x64.f: `s" replay-record" REFUSE` and `s" record-wid!" REFUSE` beside `ndict!`.
- bootstrap/cg/forth.fs: twins (BREPLAYRECORD/BRECORDWID) registered with FPRIM beside scope-find, if the seed compiles their callers. Measure with the gforth recovery check first; if it passes without twins, record that fact in docs/bootstrap.md and omit them.
- test/engine-writers.f, RUNS/REFUSES shape, rows called from top-level `evaluate` text (no TRUSTED: twin): `parse-name RR get-current replay-record` then RR binds and executing it exits 76 with the trap text; namespace mode answers `package X` reopen and `X:` qualification; `record-wid!` to -2 makes RR E-UNDEFINED (70) and back restores it; refusals: empty name, live pair, NDICT at DICT-CAP, pending definition, wid -2 on replay-record, index >= NDICT / seeded index / namespace row / wid -1 on record-wid!, task-live (79); `s" EWX ( ptr u8 n n -- ) replay-record" CHECK-CANDIDATE! 0 T=` pins the checked-caller refusal outside an owner.
Red first: before the change the child's evaluate dies E-UNDEFINED (70) at replay-record.
Proof: `bin/hb --load test/engine-writers.f`; `bin/hb --load tools/build-fixpoint-refresh.f -- all --force` on a scratch HABU_FIXPOINT_ENGINE (census 0/0/6052); `bin/hb --load test/run.f`; generations; gforth recovery check.
Dependencies: stage 2 landed in bin/hb. Not B11 or the seal; the two new trusted-only rows join habu-honour-owner-private-0a19f45d's deletion list once owner-private rows are honoured.
