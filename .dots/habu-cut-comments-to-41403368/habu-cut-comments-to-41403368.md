---
title: Cut comments to facts the code cannot state
status: active
priority: 2
issue-type: task
created-at: "2026-10-29T21:49:15.455343+02:00"
---

Problem: Joel 2026-10-29: comments that restate their code rot with it (three T0 comments named a deleted word) and cost the next agent reading time; a comment earns its place only when the next agent would otherwise spend tool calls learning what it says. Campaign over the whole tree, one child per directory, each a comment-only commit (code lines byte-identical) reviewed by a code-reviewer against the rule in /tmp/jerry-big/t0-comments-brief.md and landed with or after the T0 line. Rule: delete a comment that restates the code or narrates history; keep, cut to the fact, one that states what the code cannot (a contract between files, a refusal and its reason, a measured number, an architecture constraint, where the next thing lives); a header is a two-to-six-line map. No comment names a word that does not exist in the tree. Acceptance: every child closed. Files: src, tools, test (docs are not comments). Verify: per child, the comment-only diff filter prints nothing and the names check finds every named word. Depends: none. Ownership: the whole tree's comments, by directory. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-t0-comments.

Fan-out 2026-10-29: src/core, src/lib, src/arch, tools and test run in parallel, one worker each in .jj-ws.noindex/jerry-t0c-<name> (children claimed); src/habu continues in jerry-t0-comments (WIP tvzpwmwt, 51 files done at the last handoff); batch 2's remaining one-line restatements run in jerry-t0c-comp6 (xnkkosyp, squashed into txpzkrxl afterwards). Batch 1 (ypyowpqq, T0 files) and batch 2 (txpzkrxl, src/compiler) are reviewed with fold-ins applied. Findings parked in their own dots: the STEP 1023-byte cap (src/habu/stepper.f:90) and NFEED:OBSERVE's visibility.
