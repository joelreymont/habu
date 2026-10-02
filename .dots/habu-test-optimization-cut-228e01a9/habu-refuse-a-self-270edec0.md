---
title: Refuse a self-including file by name
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T22:28:53.502256+03:00"
---

Found by lane 540 (incio, dot 04c48ec0) on its g1 (macOS arm64): a source file that includes itself ends with SIGSEGV, rc 139, and no message, so a mistake in a user's file looks like an engine crash. Acceptance: through the real load path (--load and a stdin include), a file that includes itself directly, and a two-file cycle, end with one named line ending in LF that names the file and the reason (a cycle or the nesting bound, whichever the loader can know), and a refusal rc rather than a signal; seen failing first (rc 139). Decide at the loader whether the refusal is cycle detection over the open-file stack or a nesting-depth bound, and say why. Files: src/core/include.f (baked), a new e2e test registered in test/gate-stdlib-cases.f. After 04c48ec0.
