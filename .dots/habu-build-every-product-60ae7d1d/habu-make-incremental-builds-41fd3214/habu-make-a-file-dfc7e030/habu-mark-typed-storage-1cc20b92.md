---
title: Save typed storage pointer fields as null
status: active
priority: 2
issue-type: task
created-at: "2026-10-31T22:20:05.977329+02:00"
---

T1b. Typed storage (TYPED-VARIABLE, TYPED-BUFFER, LAYOUT-BUFFER, DYNAMIC-BUFFER) has no PERSISTED- form, so by DECISIONS.md "typed data" (2026-10-31: scratch pointer forms are written as null, only PERSISTED- forms carry an address across a save) its pointer cells are scratch rows: STORAGE-MARK-TERM (src/core/layout-buffer.f:185) marks a T-PTR term with scratch-cell-mark at allocation and at each capture walk, TFAM-STORAGE-HAS-QUOT? (src/core/type-family.f:5690) answers for any address cell, and ADDRESS-CELLS:REMOVE-XT-SPAN (src/habu/address-cells.f:250) drops scratch rows in the span as well as xt rows. The first brief made the cells persisted DATA rows; that contradicted the ruling.

Read before measuring, not yet measured: IMAGE-LIFECYCLE's PROCESS table (src/lib/image-lifecycle.f:17, `64 TYPED-BUFFER PROCESS ptr n`) holds declaration-time DATA cell addresses that a restarted image needs at its next capture, so it must be declared persisted. The seven `VEC:HEADER-CELLS TYPED-BUFFER X ptr u8` headers (src/lib/source-lex.f:43, :48 and others) would come back as empty vectors. A sum slot that is a pointer in alternative 0 and a quotation in another would be scratch-marked at allocation, and a later xt! into it would exit 99 (XTKIND-RC).

Lane T1b-M (measurement, does not land): workspace .jj-ws.noindex/carl-t1bm, brief carl-dots/.out/carl-file/brief-t1b-measure.md. It censuses the non-null typed pointer cells at the engine capture, runs the full suite on an engine built with the change against the baseline /tmp/carl-t1b0/suite.log (workspace .jj-ws.noindex/carl-t1b0), and probes the sum hazard. Then the implementation brief, with its public items (the widened TFAM-STORAGE-HAS-QUOT? and REMOVE-XT-SPAN, plus whatever the measurement shows needs a persisted form) to Joel one question each. Verify: tools/hb-build-stripped-cells-test.f with a typed pointer cell into the program's window that reads NULL-PTR in the image (the shape of HBT-STRIPPED-MAPPED-SCRATCH), test/pointer-storage-test.f, a rebuild, the full suite. Depends: T1a (landed ed087c68). Claim: agent=carl.

Census 2026-11-04: an acceptance case of its parent; fix it in the code that stays (named above), never in the machinery the parent replaces.
