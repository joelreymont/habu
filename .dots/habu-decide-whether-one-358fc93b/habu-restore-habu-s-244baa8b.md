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
