---
title: Convert split-str engine lane e12
status: closed
priority: 2
issue-type: task
created-at: "2026-11-26T00:52:04.305154+01:00"
closed-at: "2026-11-26T01:01:05.886791+01:00"
---

Problem: the files in /tmp/jerry-p1/unmark/b-e12.txt, which hb's own build loads, still carry the split-str marker. Acceptance: none carries it; the engine rebuilds itself and its tests pass. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-unm-e12.
