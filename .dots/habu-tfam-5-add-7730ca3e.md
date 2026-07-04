---
title: "TFAM 5: add dynamic-tail-manifest.f to owned key lists"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T18:37:32.944416+02:00"
---

tools/dynamic-tail-manifest.f is now a behavior-bearing dep of the discovery producer (tools/source-discovery.f requires it; a manifest edit changes closure computation). Two cache-key lists owned by the stdin-manifest worker must add it: (1) tools/hb-build-lib.f HBB-KEY-TOOL-SOURCES (~line 452, next to tools/source-discovery.f) so hb-build keys change when the manifest changes; (2) test/run-files.f TR-GATE-COMMON-FILES must add tools/source-discovery.f AND tools/dynamic-tail-manifest.f because tools/check-core.f now requires both (check-core.f top), else gate-common result-cache keys under-list their content. Files were out of scope for the TFAM 5 producer worker (scope boundary); do the two one-line additions and rerun the owning gates.
