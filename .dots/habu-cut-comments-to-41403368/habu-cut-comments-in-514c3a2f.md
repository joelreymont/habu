---
title: Cut comments in src/core
status: closed
priority: 2
issue-type: task
created-at: "2026-10-29T21:49:33.387204+02:00"
closed-at: "2026-10-30T11:33:20.863428+02:00"
close-reason: "implemented, reviewed, merged: 35 src/core files comment-only (strip 35/35 identical to master), four code-reviewer reports' fold-ins applied, landed as master 3e06713f"
---

Problem: the comments under src/core restate code, narrate history or name deleted words; rule and outcomes in the parent and /tmp/jerry-big/t0-comments-brief.md. Acceptance: one comment-only commit for src/core (code lines byte-identical), every kept comment a fact the code cannot state, headers a short map, no named word missing from the tree; reviewed by a code-reviewer against the rule and landed. Files: src/core. Verify: the comment-only diff filter prints nothing; the names check finds every named word; hb --load tools/check.f on each file where the engine does not provide it. Depends: none (the worker takes the directories in the parent's order). Ownership: comments under src/core. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-t0c-core.

2026-10-30: 32 files done in lnmnqntn (jerry-t0c-core); sumtype.f, type-family.f and checker.f left; worker relaunched 00:30.

2026-10-30 01:25: main commit lnmnqntn done (34 files, 5312 -> 3264 comment lines); checker.f halves in jerry-t0c-core2 (first half, through line 10610 of 13051) and jerry-t0c-core3 (second half, through 30356 of 30602), fourth workers running. Fold-in before review: src/core/enum-decl.f:371 cites sumtype.f CHECKER-DEFENUM-BODY, which no longer exists.
