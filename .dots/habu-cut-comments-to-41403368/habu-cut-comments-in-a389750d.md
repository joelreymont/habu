---
title: Cut comments in the T0 files
status: active
priority: 2
issue-type: task
created-at: "2026-10-29T21:49:33.329677+02:00"
---

Problem: the comments under src/habu/t0.f, t0-arm64.f, t0-emit.f, t0-label.f, t0-x64.f, escape.f, src/compiler/native/definition.f, publish.f restate code, narrate history or name deleted words; rule and outcomes in the parent and /tmp/jerry-big/t0-comments-brief.md. Acceptance: one comment-only commit for src/habu/t0.f, t0-arm64.f, t0-emit.f, t0-label.f, t0-x64.f, escape.f, src/compiler/native/definition.f, publish.f (code lines byte-identical), every kept comment a fact the code cannot state, headers a short map, no named word missing from the tree; reviewed by a code-reviewer against the rule and landed. Files: src/habu/t0.f, t0-arm64.f, t0-emit.f, t0-label.f, t0-x64.f, escape.f, src/compiler/native/definition.f, publish.f. Verify: the comment-only diff filter prints nothing; the names check finds every named word; hb --load tools/check.f on each file where the engine does not provide it. Depends: none (the worker takes the directories in the parent's order). Ownership: comments under src/habu/t0.f, t0-arm64.f, t0-emit.f, t0-label.f, t0-x64.f, escape.f, src/compiler/native/definition.f, publish.f. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-t0-comments (commit ypyowpqq on rqxnywxq: all but the second half of t0.f, in progress).
