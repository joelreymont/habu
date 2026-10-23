---
title: Retire native image transient state
status: active
priority: 2
issue-type: task
created-at: "2026-10-23T12:46:32.454400+00:00"
---

Pulsar image_state in .jj-ws.noindex/pulsar-intel-image-state, bf8af33a over140a8c27. All7 auto image-byte failures measured: GTOD two syscall cells, OUTER LOOP-EVAL process frame, X64SEL D-MEET/S-ACC/EDGE-V scratch; other sections/code identical. SNAP canonicalizes only copy, OUTER owns field clearing, selector uses existing CWORK lifetime. No master tests changed. Native build passes after producer staging; unchanged auto/app-image/CWORK/repl-twin and gen5 running, independent review running. Mac shared copy/selector ARM check OPEN. Staging workspace retired immediately after seed transferred.
