---
title: Reuse bodies after same-file edits
status: closed
priority: 2
issue-type: task
created-at: "2026-10-16T15:30:45.673262+03:00"
closed-at: "2026-10-21T15:42:49.858450+03:00"
close-reason: Implemented in tools/native-definition-cache.f span admission; test/native-file-auto-e2e.f covers same-file and shifted edits, delimiter/signature/trust misses, reader binding changes, incomplete old objects and warm/cold byte equality; real engine edit matrix passes.
---

Whole-file digest and absolute offsets still invalidate every pre-reader body shortcut when one body in a source file changes. Preserve exact-file reuse, then admit unchanged ordinary checked bodies by exact name-through-terminator spans including the inspected delimiter, unchanged scope, complete raw-reader passive-token facts, and existing implementation dependencies. No replay of source actions. Validate before any state change; failed admission resumes the actual reader. Cover meaningful same-file and shifted edits, parsing-binding changes, old incomplete objects, origins, cold/warm byte equality and runnable native artifacts. Measure real edited Habu self-builds after matching-engine qualification.
