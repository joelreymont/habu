---
title: "The profiler's inclusive count of an outer word is too low"
status: active
priority: 3
issue-type: task
created-at: "2026-11-10T21:26:33.316478+02:00"
---

Problem (observed 2026-11-10 in the profile of a cold Wasm build of Maki's viewer, tools/build-profile.f): HB-BUILD-CORE:APP, which the whole build runs under, has 42 inclusive samples of 37,215; `execute` is the highest inclusive row at 16,336 (43.8%). The report header counts 488,068 frames, about 13 a sample. Cause, read from src/habu/prof.f C-PROF-WALK (:186-209): a sample scans at most ARN-WALK machine-stack cells above the interrupted sp (PROF-WALK-CELLS, 64, src/habu/prof-abi.f :57, :82) and never past sp's 4 KiB block (PROF-PAGE-MASK), so a word whose frame lies further out is not counted for that sample; the build's stack is deeper than that under almost every sample. The limit is deliberate (the walk measured 230 instructions a tick; one cell past the block could be unmapped). docs/debugging.md 'Sampling profiler' says the inclusive section shows a phase word and does not state the limit. Acceptance: docs/debugging.md states that an inclusive count covers only the frames within the scan and can fall far short for an outer word, with the fact that proves it (not 'a lower bound': the scan is a superset of the true chain, prof.f :156-159, so a word off the stack can gain samples). Files: docs/debugging.md. Verify: the passage read against prof.f. Depends: none. Ownership: jerry. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w4.
