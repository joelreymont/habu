---
title: Cut comments in src/arch
status: active
priority: 2
issue-type: task
created-at: "2026-10-29T21:49:33.428090+02:00"
---

Problem: the comments under src/arch restate code, narrate history or name deleted words; rule and outcomes in the parent and /tmp/jerry-big/t0-comments-brief.md. Acceptance: one comment-only commit for src/arch (code lines byte-identical), every kept comment a fact the code cannot state, headers a short map, no named word missing from the tree; reviewed by a code-reviewer against the rule and landed. Files: src/arch. Verify: the comment-only diff filter prints nothing; the names check finds every named word; hb --load tools/check.f on each file where the engine does not provide it. Depends: none (the worker takes the directories in the parent's order). Ownership: comments under src/arch. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-t0c-arch.

2026-10-30: lane finished (33 files, 2281 -> 1527 comment lines, strip.py code identical, check.f rc 0 on 28, four engine-provided, wasm/leb.f rc 1 before and after: see the leb.f dot); rebased onto master 18f0605d; code-reviewer running 00:33.
