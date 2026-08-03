---
title: Stop dot-on requoting created-at
status: open
priority: 2
issue-type: task
created-at: "2026-08-03T21:29:47.532660+02:00"
---

The external dots CLI (dots 0.6.4, /opt/homebrew/bin/dot) mangles frontmatter when 'dot on <id>' activates a dot: created-at: "2026-..." becomes created-at: "\"2026-...\"" (the already-quoted string is quoted again). Hit twice on 2026-08-03, by two independent worker agents activating freshly-added dots; one repaired it by hand, the other's had to be repaired at merge. dot-dep-lint does not catch it, so the corruption propagates silently. Work: reproduce with a scratch dot, find whether the fault is in the CLI's YAML round-trip (likely re-serializing a string that includes its own quotes), fix or report upstream, and add a check to tools/dot-dep-lint.f that refuses a doubly-quoted created-at so the corruption fails loudly until the CLI is fixed. Not habu code; the lint half is.
