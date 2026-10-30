---
title: Cut comments in src/habu
status: closed
priority: 2
issue-type: task
created-at: "2026-10-29T21:49:33.365464+02:00"
closed-at: "2026-10-30T10:56:42.447058+02:00"
close-reason: "Landed as master 83f95498 'Cut src/habu comments to facts' (70 files) after the Opus review's ten fold-ins and a rebase over master's src/habu changes; strip.py code identical for all 70 against master, check.f per file identical to master's copies. The 58 banner lines in t0.f, t0-emit.f, t0-arm64.f, t0-x64.f, t0-label.f and created-body-abi.f's header are the banner-sweep dot."
---

Problem: the comments under src/habu (the files not in the T0 child) restate code, narrate history or name deleted words; rule and outcomes in the parent and /tmp/jerry-big/t0-comments-brief.md. Acceptance: one comment-only commit for src/habu (the files not in the T0 child) (code lines byte-identical), every kept comment a fact the code cannot state, headers a short map, no named word missing from the tree; reviewed by a code-reviewer against the rule and landed. Files: src/habu (the files not in the T0 child). Verify: the comment-only diff filter prints nothing; the names check finds every named word; hb --load tools/check.f on each file where the engine does not provide it. Depends: none (the worker takes the directories in the parent's order). Ownership: comments under src/habu (the files not in the T0 child). Claim: agent=jerry workspace=.jj-ws.noindex/jerry-t0-comments.

Batch 3 running 2026-10-29 in tvzpwmwt (on txpzkrxl); check.f proofs run with the line's g2 engine (jerry-s2b1/.out/land/g2/hb).

2026-10-30: three commits on master, squashed before review: tvzpwmwt (jerry-t0-comments, the directory except three files; kernel-x64.f and verify-source.f left), unouvzrk (jerry-t0c-habu2, habu1.f done, layout.f from line 1357), xukqpust (jerry-t0c-habu3, habu2.f full-line comments done, trailing ones left). Workers relaunched 00:30.

2026-10-30 01:25: the three src/habu commits squashed into tvzpwmwt (70 files); the EVAL-ENTER citations in interpret.f and definers.f now name EMIT-EVAL-BODY; rebased onto 5505243f as 0429bef7; Opus review running.
