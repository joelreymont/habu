---
title: Keep typed storage pointer addresses across a save
status: closed
priority: 2
issue-type: task
created-at: "2026-10-31T22:20:05.977329+02:00"
closed-at: "2026-11-04T18:49:53.471962+02:00"
close-reason: "Landed 1183fb56: typed storage pointer cells keep their addresses across a save (DATA address rows). Proof on eda8315c: engine build rc 0, full suite 520 of 520, two-generation convergence gen 5 = gen 4; fold 2984eb50 reviewed LAND, stripped-cells test rc 0."
---

T1b. Typed storage (TYPED-VARIABLE, TYPED-BUFFER, LAYOUT-BUFFER, DYNAMIC-BUFFER) has no PERSISTED- form, so by Joel's ruling (2026-10-31: scratch pointer forms are written as null, only PERSISTED- forms carry an address across a save) its pointer cells are scratch rows: STORAGE-MARK-TERM (src/core/layout-buffer.f:185) marks a T-PTR term with scratch-cell-mark at allocation and at each capture walk, TFAM-STORAGE-HAS-QUOT? (src/core/type-family.f:5690) answers for any address cell, and ADDRESS-CELLS:REMOVE-XT-SPAN (src/habu/address-cells.f:250) drops scratch rows in the span as well as xt rows. The first brief made the cells persisted DATA rows; that contradicted the ruling.

Read before measuring, not yet measured: IMAGE-LIFECYCLE's PROCESS table (src/lib/image-lifecycle.f:17, `64 TYPED-BUFFER PROCESS ptr n`) holds declaration-time DATA cell addresses that a restarted image needs at its next capture, so it must be declared persisted. The seven `VEC:HEADER-CELLS TYPED-BUFFER X ptr u8` headers (src/lib/source-lex.f:43, :48 and others) would come back as empty vectors. A sum slot that is a pointer in alternative 0 and a quotation in another would be scratch-marked at allocation, and a later xt! into it would exit 99 (XTKIND-RC).

Lane T1b-M (measurement, does not land): workspace .jj-ws.noindex/carl-t1bm, brief carl-dots/.out/carl-file/brief-t1b-measure.md. It censuses the non-null typed pointer cells at the engine capture, runs the full suite on an engine built with the change against the baseline /tmp/carl-t1b0/suite.log (workspace .jj-ws.noindex/carl-t1b0), and probes the sum hazard. Then the implementation brief, with its public items (the widened TFAM-STORAGE-HAS-QUOT? and REMOVE-XT-SPAN, plus whatever the measurement shows needs a persisted form) to Joel one question each. Verify: tools/hb-build-stripped-cells-test.f with a typed pointer cell into the program's window that reads NULL-PTR in the image (the shape of HBT-STRIPPED-MAPPED-SCRATCH), test/pointer-storage-test.f, a rebuild, the full suite. Depends: T1a (landed ed087c68). Claim: agent=carl.

Census 2026-11-04: an acceptance case of its parent; fix it in the code that stays (named above), never in the machinery the parent replaces.

T1b-M result 2026-11-04 (commit dfdc57d9, change zuxvuksk, carl-t1bm): saved as null, 18 suite rows fail that pass at baseline (/tmp/carl-t1b0/suite.log, 520 of 520). 17 through PROCESS (src/lib/image-lifecycle.f:17), the only typed pointer cells non-null at the engine capture ("snap: process cell outside copied data", exit 74); test/structure-quotation-field.f SAVED-BOX (:37, a route record with text pointers and a handler) crashes after restore. No sum puts a pointer and a quotation in one slot (351 SUMTYPEs); the exit-99 hazard did not occur. Lane T1b-M2 running (same workspace, brief carl-dots/.out/carl-file/brief-t1b-measure2.md): the pointer cells as DATA rows that keep their addresses. Then the choice goes to Joel.

T1b-M2 result 2026-11-04 (commit 1902a68d, change sqnmosrz, on dfdc57d9, carl-t1bm): the pointer cells marked as DATA rows (STORAGE-MARK-TERM T-PTR arm `base ptr-cell-mark`; REMOVE-XT-LOCKED drops XT and DATA rows in the span). Engine build rc 0 (96 s), suite 520 of 520, rc 0 (839 s), rows identical to the baseline; the 18 rows above pass. No save refusal fired: no suite row saves a typed pointer cell into memory the build mapped, so that refusal needs its own test. Convergence and other machines not checked. zuxvuksk's type-family.f line (T-PTR counts as HAS-QUOT) is still under it.

Decided by Joel 2026-11-04 ("Keep the address (Recommended)"): typed storage keeps its pointers' addresses (M2's change). TFAM-STORAGE-HAS-QUOT? becomes TFAM:HAS-ADDRESS? (Joel's name); REMOVE-XT-SPAN, which now drops data-address rows too, becomes ADDRESS-CELLS:REMOVE-SPAN (lead's, by the rule that a smaller change is the lead's). The rest of TFAM's prefix is dot habu-drop-the-tfam-949c93fc.

Status 2026-11-04: implementation lane running, workspace .jj-ws.noindex/carl-t1b (on d83105cc), brief .out/carl-file/brief-t1b-impl.md, scratch /tmp/carl-t1b. carl-t1bm (measurements) retires once it lands.
