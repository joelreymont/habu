---
title: Cut comments in src/lib
status: active
priority: 2
issue-type: task
created-at: "2026-10-29T21:49:33.407991+02:00"
---

Problem: the comments under src/lib restate code, narrate history or name deleted words; rule and outcomes in the parent and /tmp/jerry-big/t0-comments-brief.md. Acceptance: one comment-only commit for src/lib (code lines byte-identical), every kept comment a fact the code cannot state, headers a short map, no named word missing from the tree; reviewed by a code-reviewer against the rule and landed. Files: src/lib. Verify: the comment-only diff filter prints nothing; the names check finds every named word; hb --load tools/check.f on each file where the engine does not provide it. Depends: none (the worker takes the directories in the parent's order). Ownership: comments under src/lib. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-t0c-lib.

2026-10-30: two commits on master, squashed before review: kqxumopp (jerry-t0c-lib, files 1-177 of the order done, five left plus proofs) and qroqvkuk (jerry-t0c-lib2: task.f done, errors.f from line 873, ffi-abi.f and aio.f). Workers relaunched 00:30.
