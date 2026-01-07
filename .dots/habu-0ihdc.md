---
title: Implement nreverse in habu0 for O(n) list building
status: closed
priority: 1
issue-type: feature
assignee: ""
created-at: "2025-12-10T21:29:43.943365+02:00"
closed-at: "2025-12-10T21:32:32.862968+02:00"
close-reason: ""
---

The tac-codegen function uses push+nreverse for O(n) list building in SBCL mode, but falls back to O(n²) append in habu0 mode because nreverse isn't available. Implement nreverse as a primitive so habu0 can also benefit from the 18x speedup.
