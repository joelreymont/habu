---
title: "Delete XREF:HIDE-DEFS-FROM and XREF:FORGET-DEFS-FROM"
status: active
priority: 2
issue-type: task
created-at: "2026-11-18T18:23:41.421280+01:00"
---

Problem: the two public words (src/habu/xref.f:751, :875) have no caller in src, tools or the consumers (zen, tender, maki); only tests and docs/forth-card.md:600-602, docs/forth.md:906 name them. Their LIVE-TASK-GUARD (xref.f:739) is a run-time check, and test/engine-suite.f:2128-2132 asserts an index state no user sees. Acceptance: both words, the code only they reach and the tests that test only them are gone; a test that uses them to reach other behaviour reaches it another way or goes with that behaviour if it too is unreached; the card and forth.md no longer name them. Depends: none. Claim: agent=jerry workspaces=.jj-ws.noindex/jerry-x1 (src), jerry-x2 and jerry-x3 (tests).
