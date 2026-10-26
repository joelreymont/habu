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
