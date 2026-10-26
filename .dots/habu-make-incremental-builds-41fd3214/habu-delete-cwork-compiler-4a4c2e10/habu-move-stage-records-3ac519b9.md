---
title: "Move stage records onto IR-CTX:RECORD"
status: active
priority: 2
issue-type: task
created-at: "2026-10-26T15:50:55.571657+02:00"
blocks:
  - habu-take-module-records-8ff62458
---

Lanes L1-L13 of /Users/joel/.cache/tmp/carl-file/cwork-revision.md §5, parallel from R's commit, no two sharing a file: L1 build/op/fun (tolerant ABANDON, *-REAP), L2 schema+hir, L3 hir-word, L4 feed, L5 host+host-publish, L6 verify/digest/publish/select-x64/emit-x64/frozen/type, L7 elaborate, L8 select, L9 regalloc, L10 regalloc-verify, L11 spill, L12 emit, L13 loop. Each: module key + WORK accessor, CWORK words gone from the file, its focused test/compiler rows. Briefs: ~/.cache/tmp/carl-file/record-lanes.md + record-common.md. Running: L1, L7, L8, L10 in .jj-ws.noindex/carl-rec<N> on qwrtlnrl. Claim: carl workers.
