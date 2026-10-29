---
title: Cut comments in src/core
status: active
priority: 2
issue-type: task
created-at: "2026-10-29T21:49:33.387204+02:00"
---

Problem: the comments under src/core restate code, narrate history or name deleted words; rule and outcomes in the parent and /tmp/jerry-big/t0-comments-brief.md. Acceptance: one comment-only commit for src/core (code lines byte-identical), every kept comment a fact the code cannot state, headers a short map, no named word missing from the tree; reviewed by a code-reviewer against the rule and landed. Files: src/core. Verify: the comment-only diff filter prints nothing; the names check finds every named word; hb --load tools/check.f on each file where the engine does not provide it. Depends: none (the worker takes the directories in the parent's order). Ownership: comments under src/core. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-t0c-core.

2026-10-30: 32 files done in lnmnqntn (jerry-t0c-core); sumtype.f, type-family.f and checker.f left; worker relaunched 00:30.
