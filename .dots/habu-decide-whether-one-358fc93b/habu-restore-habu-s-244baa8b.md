---
title: "Restore Habu's clients on master"
status: active
priority: 2
issue-type: task
created-at: "2026-10-26T20:50:25.267318+02:00"
---

Joel 2026-10-26: tender and zen are key clients; fix zzusage and Etch too. Claim: jerry. Engine: Habu master, ~/Work/habu/.jj-ws.noindex/jerry-land/bin/hb (cwd or engine tree resolves Habu libs).
- zzusage: nothing to fix. Its master already requires src/lib (52d7726f) and loads rc 0; the rc 74 came from the stale root checkout (old commit plus an uncommitted HANDOFF.md, left alone). Measurements use workspace ~/Work/zzusage/.jj-ws.noindex/jerry-zz on master.
- zen: src/document/cfb.f still uses the removed legacy VEC words (VEC-HEADER-BYTES, VEC-PUSH-AT, VEC-IDX, VEC-N@, VEC-COUNT, VEC-INIT, VEC-DISPOSE), so tender test/c2-office-scale.f exits 70. Worker lane in ~/Work/zen/.jj-ws.noindex/jerry-vec (main b59eb4d1), handoff ~/.cache/tmp/jerry-zenvec-handoff.md. Tender is verified with TENDER_KAIZEN_ROOT pointing at the lane.
- Etch: master fdbce183 tests.f dies 'uncaught throw code -2506' (E-PROC-PATH) after seven suites; an older checkout failed asserts 54 and 61. Root-cause lane in ~/Work/etch/.jj-ws.noindex/jerry-etch, handoff ~/.cache/tmp/jerry-etch-handoff.md.

zen landed: bca6a811 "Use the typed VEC API in the CFB reader" on zen main, reviewed ACCEPT; zen test/reader.f and test/normalize-ingest.f exit 0, and tender test/c2-office-scale.f exits 0 against zen's root. Duplication left for Joel: the n -> NUM:item-count / NUM:index converters (MATCH with E-VEC-CAPACITY / E-VEC-BOUNDS) are copied in Habu vector.f (private N>ITEM/N>INDEX), source-lex.f, word-docs.f, Etch kicad_read.f and zen cfb.f, because VEC exports none; making VEC's pair public is an API change and needs Joel's decision.

Etch (root-cause lane, no commits): nothing in Habu or Etch is at fault. Every Habu-only suite passes on Habu master. The failures come from the host's KiCad: this Mac has stock KiCad 10.0.6 while the suite is pinned to a Linux 10.99 build. Details: -2506 means kicad-cli is not on PATH (set KICAD_CLI=/Applications/KiCad/KiCad.app/Contents/MacOS/kicad-cli). F54/F61 (test/zone.f CORNER-POUR-PAIR): 10.0.6 reports 1 zones_intersect where the 10.99 pin expects 0 (docs/kicad.md:1238). test/library.f CONSTRAINED: 10.0.6 rejects the (constraint ...) footprint form. The kiapi suites open libnng.so.1 (Linux name; macOS has libnng.1.dylib). tools/import-probe.f:238 reads /proc/loadavg (Linux only). Running Etch's full suite on macOS needs the pinned KiCad here or a port of those three reads: Joel's call, recorded, not dotted as work.
VEC export landed (habu-export-vec-s-4422a056): Habu c4657b44 makes VEC:N>ITEM / VEC:N>INDEX public and drops the copies; Etch master 631c6c72 and zen main 151f4147 use them. Fable review REVISE (negative-input test rows, stale headings), fixed and re-reviewed ACCEPT. Proofs: Habu vector-test and lint tests rc 0; zen test/reader.f passes (2181 checks); Etch tests.f with KICAD_CLI passes the 33 suites before zone's environment-only F54/F61, kicad_read among them.
