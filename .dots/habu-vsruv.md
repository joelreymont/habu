---
title: Implement oracle architecture recommendations for Stage 1
status: closed
priority: 1
issue-type: task
assignee: ""
created-at: "2025-12-10T10:47:47.181726+02:00"
closed-at: "2025-12-10T10:52:48.120341+02:00"
close-reason: ""
---

Oracle recommendations for clean Stage 1 self-hosting:

1. Keep arm64/asm.lisp separate (don't concatenate to habu0.lisp)
2. Add keyword normalization at h0-eval-builtin boundary (convert SBCL keywords to native once)
3. Unify intern tables with package tagging (single table, keywords vs symbols distinguished by tag)
4. Remove string-compare fallbacks - use pointer comparison after normalization
5. Mark boundary functions notinline to prevent stale inlined code
6. Rebuild everything natively so no SBCL keyword constants remain embedded
