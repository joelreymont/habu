---
title: Keep reuse after an effect change in an early file
status: open
priority: 1
issue-type: task
created-at: "2026-10-22T12:17:10.179189+03:00"
---

Problem: engine build into cache C, then in src/lib/memory.f widen private RELEASE-RANGE from ( ptr u8 n -- ) to ( ptr a n -- ) and add public RELEASE-CELLS after ALLOC-CELLS (diff ~/.cache/tmp/carl-zero/memory.diff), rebuild with the same host: definition hits 0, body skips 0, early prefix hits 0, compiled 13945. A comment-only edit to the same file keeps 13288 hits. Root cause under investigation (worker lane carl-zero): some key includes a position or prefix digest that the edit shifts. Acceptance: such an edit recompiles memory.f's changed definitions and definitions that actually bind RELEASE-RANGE or the new word, nothing else; E2E proves it. Files: tools/native-unit-dependency.f, tools/native-definition-cache.f (to be confirmed by the root cause). Depends: none. Claim: unassigned.
