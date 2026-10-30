---
title: "Track reads past a record's extent"
status: active
priority: 2
issue-type: task
created-at: "2026-10-28T23:43:55.968782+02:00"
---

False reuse in the incremental build: the dependency model ends a record's DATA extent at the next record's origin and does not track reads past it. Example (L7 review): file A has `create L 8 allot variable R 5 R ! : L-DEF create L 8 + @ , ;`; file B runs `L-DEF X`. B's walk reaches only L, whose DATA row stops at R's origin, so editing `5 R !` in A leaves B reused with a stale X. Holds with R live; with R retired since L7 commit 2 too (before it, L's extent covered a retired R and the edit missed). Fix at the model: a read through an address must depend on the record that owns the address read, not the record the walk started from. Source: L7 review 2026-10-28 (carl-l7).

Claim (carl, 2026-10-30): probe on the 11B engine in .jj-ws.noindex/carl-probe confirmed it (fixture p1.f, fix site DATA-HASH dep:906). Fix: worker in .jj-ws.noindex/carl-fext on spvqktuy, brief carl-dots/.out/carl-file/brief-fix-ext.md.

Finding (lane EXT, 2026-10-30): the defect is wider than the extent. Runs made while a colon body is read (immediates, evaluated text) give the definition no rows: its dependency frame opens only at native compile (COLD-CANDIDATE dc:678-681), so ADD-ROW files the run rows in the file frame, and the warm per-definition install checks only definition rows. F and H in p1.f have no EXECUTED or DATA row at all. Evidence carl-fext/.out/probe/, handoff carl-fext/.out/l11/fext-handoff.md. Design on Fable (Plan, brief carl-dots/.out/carl-file/brief-ext-design.md); carl-fext keeps the workspace for the lane that follows.
