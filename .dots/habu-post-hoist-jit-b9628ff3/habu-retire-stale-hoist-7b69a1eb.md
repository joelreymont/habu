---
title: Retire stale hoist workarounds
status: open
priority: 1
issue-type: task
created-at: "2026-02-22T19:46:39.095719+01:00"
---

src/jit/backend.zig: audit and remove stale workaround gates/comments tied to historical hoist phi/call issues where Habu-side fix is now possible without touching hoist. Add backend regressions for any unblocked path.
