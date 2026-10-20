---
title: Reuse checker import scratch
status: active
priority: 2
issue-type: task
created-at: "2026-10-16T14:43:59.577995+03:00"
---

A private 7cc7 self-build with12971 definition hits still spends3.32 thread-CPU seconds in definition preparation. RECORD-CONTRACT maps/frees an initial64KiB graph store per query, and each prepared import maps its header, copied blob and up to7 row arrays separately. Reuse a checker-owned growable graph scratch buffer at the existing synchronous query boundary and stable private chunks for all plans in one prepare session. Preserve complete validation, failed preparation without publication, multiple live plans and install after capture; zero reused row arrays. Release retained stores at existing checker scratch teardown/capture boundaries. Existing capture E2E must first prepare nonempty plans spanning chunks and install them after capture; use codec E2E for large-to-small reuse and malformed-then-valid recovery. No new public ABI or file format. Independently review, rebuild matching engine, run focused E2Es, full native and convergence, then repeat warm self-build CPU/allocation measurement; do not infer speed from allocation counts.
