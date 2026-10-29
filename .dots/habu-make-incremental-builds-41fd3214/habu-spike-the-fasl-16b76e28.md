---
title: Spike the fasl load model on the engine build
status: active
priority: 2
issue-type: task
created-at: "2026-10-29T13:06:05.470397+02:00"
---

Problem: Joel 2026-10-29: 'run this design by hand, drive a spike and determine if it works. then implement'. Design: an object = a file's compiled definitions + its top level as the load routine (the source re-interpreted, each definition installed from the object and its body skipped while its recorded inputs hold, else compiled from there on); no snapshots, diffs, owners, judging or refusals. By-hand trace: .jj-ws.noindex/carl-dots/.out/carl-file/fasl-by-hand.md (points 1-6). Acceptance: report with numbers: warm product image byte-equal to cold (else every differing span named by owner); memory.f comment edit compiles 1 file; interface edit compiles memory.f + exactly its consumers, image equal to a fresh build; wall/CPU for cold, warm, both edits. Files: spike in carl-fasl, not landed. Verify: the spike's own comparison commands. Depends: none. Ownership: tools/native-definition-cache.f, tools/native-unit-object.f, loader hooks. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-fasl

Continuation 6 (carl-fasl, 2026-10-29): hook moved to src/habu/habu2.f C-BODY-HOOK in EM-INTERPRET-COLON, fires on tiers 0 and 1; maker selection unchanged (hb-build-lib.f HBB-SELECT-ENGINE keys the maker on the running engine). Cold build with objects: rc 0, 4265 s real / 813.74 s user against base 93.55 s user; "definitions installed 0 compiled 14180"; 125 objects; 67 rowless refusals (31 checker-mark, 36 record-kind-unknown); cold product byte-equal to base. Warm build: rc 83 after 4 s with no output, undiagnosed. Cost root cause runs in carl-fasl2 (brief .out/carl-file/brief-fasl-cost.md). Kill criterion: warm product == cold product and warm CPU well under 93.55 s user, else close the whole-file-object model with these numbers and bring the alternative.
