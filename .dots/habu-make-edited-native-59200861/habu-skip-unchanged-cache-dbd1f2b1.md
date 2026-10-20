---
title: Skip unchanged cache-row moves
status: closed
priority: 2
issue-type: task
created-at: "2026-10-20T22:44:40.082325+03:00"
closed-at: "2026-10-20T22:50:37.022098+03:00"
close-reason: "Skip COPY-ROW byte copying only when destination equals source, after existing ROW-SIZE validation. Independent Astra adversarial review clear. Existing body-edit/history, strict object, explicit reclaim and TASK state E2Es pass. Real 14,220,637-byte/165-row cache microbenchmark: retained self-copy 5.27-6.33ms to 0.017-0.034ms; overlapping compaction and serialized bytes unchanged. No whole-build timing claim."
---

A real warm Habu edit profile attributes 416 inclusive samples to NDEF-CACHE:COPY-ROW while compacting the native definition cache. FILE-COMPACT copies retained rows even when their destination equals their current offset. After the existing ROW-SIZE validation, omit only this self-copy and preserve real forward compaction, dirty tracking, row validation and serialized bytes. Use existing FILE/cache state and reclaim behavior tests rather than a copy-counter unit test. Measure the relevant actual-row copy path before and after without another cold engine build; independent review before landing.

Final engine edit matrix passes exact cached/cold executable-map equality, runtime/source-origin checks and rejected input: cold 143.971s, unchanged .759s, early 9.548s versus 101.739s cold, middle 10.050s versus 116.393s cold, late 10.354s versus 99.960s cold, rejected 7.397s. Whole matrix exit 0, 501.24 wall / 442.14 user / 11.56 system seconds. These shared-host wall observations qualify the combined build changes; they are not isolated self-copy savings.
