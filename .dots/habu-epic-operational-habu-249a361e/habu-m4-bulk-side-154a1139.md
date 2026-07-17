---
title: "M4: bulk side-content scanner"
status: open
priority: 1
issue-type: task
created-at: "2026-07-17T13:03:36.756504+02:00"
blocks:
  - habu-m3-authenticated-modular-ed51c697
---

Start only after M3 is closed on green master. Own one checked jj external-diff scanner invocation that binds ordered metadata rows to left/right path bytes and kinds, uses the checked no-follow outcome, hashes complete content, classifies binary bytes, emits deterministic HABUSIDE framing, rejects escape/corruption/truncation, and proves constant child-process count. Existing owner: habu-tools-bulk-diff-f36d0508. Finish by building the real scanner through hb-build, registering its focused/real-jj gates, independently reviewing it, and promoting the green milestone to master.
