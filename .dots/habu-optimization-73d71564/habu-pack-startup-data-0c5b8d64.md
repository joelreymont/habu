---
title: Pack startup DATA values
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-09-28T20:49:03.306781+02:00\\\"\""
closed-at: "2026-09-29T08:57:20.205190+02:00"
close-reason: "Cancelled by user: no DATA or CODE compression without explicit approval. Not landed or installed; no savings accepted. Preserve source cbfdab2dff57dafa14ceef8a75639253d19f0d7c and existing scratch evidence."
---

Repeated-byte census proves LZ4 over exact canonical DATA values saves164228 physical bytes with the reference fast encoder; HC9 measures189388, before production code and target overhead. Implement one bounded deterministic checked-Habu hash-chain greedy encoder and native boot decoder, using at most64KiB independent blocks ending at complete cells. Price the local encoder first. Preserve every value, presence bit and relocation; WINDOW-tagged declared cells are excluded structurally while fixed rows may be present. Keep bitmap, reusable AOT13, owned merge, snapshot10 and stripped output grammar unchanged. Reserve LBMGROUPS bit32 for value wrapper; distinguish AOT-PACK VALUE state from existing DATA-site state. Exact physical accounting and decoded logical owner costs must stay separate. New path requires bounded LZ4 and canonical cell/bitmap reads, one stack-owned transient64KiB map released before relocations, warm restore and user code. No runtime codec dependency, source/code compression, pruning or error fallback. Add genuine real-load boundary/corruption E2E before production; measure local exact-stream economics, native total cost, cold/warm startup and scratch lifetime; require independent Astra review, full qualification and installation. Design ~/.cache/tmp/habu-data-codec-design-20260928-03.md with01/02; census receipts01/02. Lead owns integration and closure; Sol owns isolated implementation.
