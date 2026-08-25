---
title: Structure source path lists
status: open
priority: 1
issue-type: task
blocks:
  - habu-lowering-hash-unified-586f7881
created-at: "2026-07-19T21:32:36.824946+02:00"
---

lib/source.f:112-157 exposes CONCAT-FILES and WRITE-SOURCE-LIST with two independent ptr-a columns for paths and lengths. Reversing the columns is checker-valid because both parameters have the same type; length cells then become addresses and path pointers become lengths, leading to arbitrary reads, misleading capacity errors, or crashes. The two arrays can also diverge in count/ownership. Define STRUCTURE source-path with typed bytes pointer and length fields, store one row per path in LAYOUT-BUFFER or accept a typed bounded source-path view, and make both APIs iterate that single collection. Remove SOURCE-PATH-A@/U@ and parallel-column callers; retain raw argv/source-discovery parsing only at the constructor boundary. Preserve input order, concatenated/source-list bytes, quoted-path safety, provided lines, file error propagation, zero/full counts, and capacity semantics. Add checker negatives for reversed/foreign fields and raw parallel arrays, malformed length/bounds canaries, constructor count/capacity failures, and exact multi-file/source-list byte goldens. Measure call-site stack operations, source/JIT/DATA/CODELEN, buffer bytes, and concatenation throughput before/after. Files: lib/source.f, source-test.f and all direct callers. Verify source/discovery/check/build/cache/fixpoint suites, typed-local diff, type/package/host/dot and full native gates. Coordinate stdlib package migration; ownership here is source-path representation/API.
