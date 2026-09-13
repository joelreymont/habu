---
title: "Index address cells at the owning registrar"
status: open
priority: 2
issue-type: task
created-at: "2026-09-13T11:09:21.785913+03:00"
blocks:
  - habu-size-the-snapshot-1ca5db10
---

Plan: [PLAN.md](../../PLAN.md). Design reconciled 2026-09-13; replaces stale diagnosis/claim. Claim: unassigned.

Own habu2.f EMIT-MARK lookup, layout.f derived index and native-build.f ADDR-ROWS!/KEEP-ROWS-BELOW invalidation only. Index exact DATA byte offsets into ordered rows. Same-kind duplicate no-op; conflict/capacity/range refuse before store; unaligned cells legal. Rebuild at restore/reset/compaction; persist no scratch pointer/stale ordinal. Test collisions, both kinds, unaligned/boundary, compaction/restore,40000 cells and byte identity. Count probes across sizes and all-AOT full-build pair. Quadratic scan proven; wall share unmeasured. No second relocation representation.

Verification: focused real-load cases above; rebuild and run `bin/hb --load test/run.f` for compiler/runtime integration. Speed acceptance uses the all-AOT campaign pair; functional/count evidence can be developed in parallel.

Additional source-confirmed quadratic walk: aot-capture.f ACAP-NEXT-CELL scans
every captured address row once per DATA gap. Its comment still assumes4096
rows; the current table supports32768 and the campaign requires40000. Preserve
ordered authoritative rows, but traverse the window's cell locations through a
sorted temporary view when scanning sparse DATA runs. Include this capture cost
in the cold-build measurement; the separate producer validator now uses sorted
lookup and does not fix this walk.


2026-09-14 measured registrar cost, separate from the capacity repair:
G2 SHA 36f25285 on frozen Tender 4cc58705 starts the first live-symbol marking
pass with 31,230 unique rows in a seven-symbol attribution probe. The actual
`ptr-cell-mark` loop takes these monotonic elapsed times (two rows per symbol):

| Newly marked symbols | Cumulative ms | This 4096-symbol block, ms |
| ---: | ---: | ---: |
| 4096 | 177.853 | 177.853 |
| 8192 | 397.073 | 219.221 |
| 12288 | 657.784 | 260.711 |
| 16384 | 959.889 | 302.105 |

The next block hits the old 65,536-row limit, so this is a partial marking
measurement, not a full-build speed result or a quiet timing pair. Evidence:
`/home/joel/.cache/cedar-capture-rows-u5l55np1/trace-timed.log`. Both the old symbol
store and current store remain supported views; the uninstrumented Tender
capture needs 75,900 unique rows. Capacity work preserves their append order.
A following exact-offset index should store row ordinals, reacquire the current
backing span, and invalidate on explicit compaction/restore; base growth alone
preserves ordinals. No index is included in the capacity leaf.

The same monotonic trace measures the surrounding owner preparation: lifecycle
0.031 ms, includes 0.083 ms, buffers 0.018 ms, compiler 0.023 ms, checker scratch
0.348 ms, constructor persistence/marking 0.018/0.101 ms, record persistence/
marking 0.018/0.017 ms, and the first 1,310,720-byte symbol persistence copy
1.683 ms. Each interval includes the preceding state print; these figures locate
the dominant marking work, not a quiet microbenchmark of each helper.
