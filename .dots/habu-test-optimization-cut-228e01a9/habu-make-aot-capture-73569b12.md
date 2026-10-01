---
title: "Make AOT capture's call scan linear"
status: open
priority: 2
issue-type: task
created-at: "2026-10-01T04:32:37.542167+02:00"
---

Problem: src/habu/aot-capture.f CAPTURE (:2526) spent 8751 ms of 8786 ms in ACAP-SCAN-CALLS (:1683) for the roughly 1100-record window of test/aot-wide-format's BIG build, about 8.6 ms per filler word, while the same words compile in 0.27 s (measured by the r4-rows-aot lane on d40cc36d with timestamps between CAPTURE's steps, load 7-13). Inferred, not proven: per-site work in ACAP-SITE-ADD (:1662) and ACAP-?SITE (:1626), with ACAP-PKG-ROW (:618) walking the whole dictionary (`ndict@ 0 ?do`) on each call, so the scan is quadratic in records. It sets aot-wide-format's row time and part of aot-chain-producer's, and every production AOT capture pays it. Acceptance: the cost is attributed by measurement to the step that grows faster than linearly; that step is made linear (or n log n) in records with identical capture output (cmp of captured images before and after for aot-wide-format BIG and the aot-chain fixtures); CAPTURE time measured at 256, 512, 1024 and 2048 fillers before and after. Files: src/habu/aot-capture.f. Verify: the aot rows (aot-wide-format, aot-chain-producer, aot-chain-capture, aot-payload-graph, aot-prefix-literal), native build convergence, two-generation build. Depends: none. Ownership: AOT capture's call scan.
