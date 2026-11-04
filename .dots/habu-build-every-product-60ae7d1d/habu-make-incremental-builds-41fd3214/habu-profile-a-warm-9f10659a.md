---
title: Profile a warm engine build at lane 11
status: closed
priority: 2
issue-type: task
created-at: "2026-10-31T12:49:17.789039+02:00"
closed-at: "2026-10-31T13:07:16.879057+02:00"
close-reason: "Measured at uyzslxrr: no-change warm engine build 46.0 s (cold 117 s); 31.1 s (67.5%) is checker.f + type-family.f refused every build (dot habu-reuse-checker-f-5bd58a27); hb-build startup 3.6 s, writer load 2.29 s, capture 0.80 s, write 0.42 s, handover 0.38 s. The five Stage 2 dots carry the current costs and word ranges. Report carl-dots/.out/carl-file/s2prof-report.md."
---

Problem: the five Stage 2 dots (262ba8c9, 6b58d051, 088fea49, 00d1dd6a, 0a6c8b1d) carry 2026-10-22 timings and word names from before lane 11; 262ba8c9 names CHECKER-REG:ORDER-* words that no longer exist, so none can be briefed. A no-change warm engine build at uyzslxrr takes 46 s (cold 118 s), 160 files reused, 4 compiled (the four refused every build). Acceptance: per-phase cost of a warm engine build at uyzslxrr, the top profiler rows, and per dot its current cost and responsible words and lines; each Stage 2 dot updated from the report. Files: none (measurement). Verify: carl-dots/.out/carl-file/s2prof-report.md. Depends: none. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-s2prof brief=carl-dots/.out/carl-file/brief-s2prof.md
