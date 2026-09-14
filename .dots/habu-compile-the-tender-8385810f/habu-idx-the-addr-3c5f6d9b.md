---
title: "Index address cells at the owning registrar"
status: open
priority: 2
issue-type: task
created-at: "2026-09-13T11:09:21.785913+03:00"
blocks:
  - habu-size-the-snapshot-1ca5db10
---

Plan: [PLAN.md](../../PLAN.md). Design reconciled 2026-09-13; replaces stale diagnosis/claim. Claim: registrar_index_finish, registrar implementation and focused acceptance.

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

2026-09-14 registrar acceptance is ready for independent review. Production
`d160ab31` uses ordinal+1 hash slots and a process-owned pointer at DATA+$36C8;
the ordered row vector and snapshot schema stay authoritative. The follow-up
scopes KEEP-BELOW/PERSIST locking with `finally`: the real evaluator's caught
DP refusal previously retained the lock, and now preserves every row/header
field while releasing the lock and invalidating the index.

M-hosted rebuild after that correction completed in 135.81 s and reproduced the
earlier indexed executable byte for byte, SHA-256
`934393a3d2dcb7d2286fb56a71661df860db6af56239920decb024a6f0744436`, at
`/tmp/cedar-family-stage-abi/hb-address-index-corrected`. This candidate passed
the new 40000-row index and caught-refusal suites, 75900-row capacity suite,
real OS allocation refusal with duplicates at capacity, concurrent task stores,
poisoned incoming snapshot index/lock cells, app-image's three generations and
grown source-window rewind, program diagnostics, and snapshot declaration
tests. Clobber lint is clean at 350 routines/501 calls with the new helper
contracts; its negative fixtures catch stale x16/x8 at MARK-ROWS/INDEX-SHAPE.
Reserved-name lint passes. Logs are `/tmp/cedar-address-index-accept-*.out/.err`;
the recovery stderr is the expected caught `hb: data space out of range`.

The same unmodified native probe, SHA-256
`1433a08f87cfd115c5dd610fe3f8db3d42b4ef99088a1bb5887769350e54ae86`, measured
four successive 8192-registration blocks in the same checkout. M took
179.540/220.813/262.175/303.959 ms; indexed took
0.413/0.147/0.157/0.180 ms. Arithmetic controls were 16.2–19.5 us, exact ordered
rows passed on both, and the indexed initial prefix had ten additional rows.
Observed load was 1.51 before and 0.97 after, with no competing engine observed
before the sequential pair. Logs are `/tmp/cedar-address-index-probe-M-repeat.log`
and `/tmp/cedar-address-index-probe-index.log`. These intervals measure only
registration, excluding startup/loading/validation; they do not establish a
full Tender build improvement. Root owns the full composed native suite and
the application timing pair after integration.
