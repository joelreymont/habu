---
title: "ARM-check pulsar's pstore and PTY lanes"
status: open
priority: 2
issue-type: task
created-at: "2026-10-23T18:58:19.015885+03:00"
---

When pulsar lands pstore or the PTY tier-1 expectation (habu-resolve-pty-tier-5105302d) on intel, check them on Mac ARM64 in .jj-ws.noindex/jerry-armlsp: jj new intel@origin; rebuild bin/hb (tools/native-build.f with HABU_UNDER_TEST/HABU_FIXPOINT_ENGINE/HB_TMP); two-generation build; bin/hb --load test/run.f; record results in the lane dots. Master (ARM) behaviour wins. Claim: jerry.
