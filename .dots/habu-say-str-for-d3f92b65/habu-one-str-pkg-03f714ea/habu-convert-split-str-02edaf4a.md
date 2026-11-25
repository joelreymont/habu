---
title: Convert split-str engine lane e11
status: closed
priority: 2
issue-type: task
created-at: "2026-11-26T00:52:03.979974+01:00"
closed-at: "2026-11-26T01:06:36.097606+01:00"
---

Problem: the files in /tmp/jerry-p1/unmark/b-e11.txt, which hb's own build loads, still carry the split-str marker. Acceptance: none carries it; the engine rebuilds itself and its tests pass. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-unm-e11.
