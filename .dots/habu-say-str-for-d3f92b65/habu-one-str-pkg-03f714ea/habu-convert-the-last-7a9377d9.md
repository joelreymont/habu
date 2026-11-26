---
title: Convert the last seven split-str files
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T14:19:46.544488+01:00"
---

Problem: src/core/sha256.f, src/core/arena-pages.f, src/habu/intern.f, src/habu/main.f, src/lib/c2-memory.f, src/lib/process-tree.f and src/lib/span.f still start with the split-str marker, so they keep ^u8 n rows Joel ruled out (2026-11-19: str or vector); the marker machinery cannot go until they convert. Acceptance: none of them carries the marker; the engine fixpoint and the suites that cover them pass on arm64 and x86-64. Depends: none. Claim: unassigned.
