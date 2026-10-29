---
title: Cut comments in tools
status: active
priority: 2
issue-type: task
created-at: "2026-10-29T21:49:33.441757+02:00"
---

Problem: the comments under tools restate code, narrate history or name deleted words; rule and outcomes in the parent and /tmp/jerry-big/t0-comments-brief.md. Acceptance: one comment-only commit for tools (code lines byte-identical), every kept comment a fact the code cannot state, headers a short map, no named word missing from the tree; reviewed by a code-reviewer against the rule and landed. Files: tools. Verify: the comment-only diff filter prints nothing; the names check finds every named word; hb --load tools/check.f on each file where the engine does not provide it. Depends: none (the worker takes the directories in the parent's order). Ownership: comments under tools. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-t0c-tools.

2026-10-30: 188 of 265 files done in vpprrtqk (jerry-t0c-tools); the remaining 77 split by comment lines into three lists: files 189-246 stay in vpprrtqk, 15 files in vlkxmqyz (jerry-t0c-tools2), the 4 largest in swkutwqn (jerry-t0c-tools3); squashed into vpprrtqk before review. Workers relaunched 00:30.
