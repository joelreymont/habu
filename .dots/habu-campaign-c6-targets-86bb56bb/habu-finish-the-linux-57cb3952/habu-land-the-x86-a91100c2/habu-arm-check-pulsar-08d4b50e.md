---
title: "ARM-check pulsar's pstore and PTY lanes"
status: closed
priority: 2
issue-type: task
created-at: "2026-10-23T18:58:19.015885+03:00"
closed-at: "2026-11-04T11:48:48.223584+02:00"
close-reason: "Census 2026-11-04: goes with habu-make-a-file-dfc7e030, the cross-build whose product is data (docs/design.md \"Machinery being replaced\"). ARM-checks intel-stack lanes; workspace jerry-armlsp not listed (Claim: jerry, open)."
---

When pulsar lands pstore or the PTY tier-1 expectation (habu-resolve-pty-tier-5105302d) on intel, check them on Mac ARM64 in .jj-ws.noindex/jerry-armlsp: jj new intel@origin; rebuild bin/hb (tools/native-build.f with HABU_UNDER_TEST/HABU_FIXPOINT_ENGINE/HB_TMP); two-generation build; bin/hb --load test/run.f; record results in the lane dots. Master (ARM) behaviour wins. Claim: jerry.
