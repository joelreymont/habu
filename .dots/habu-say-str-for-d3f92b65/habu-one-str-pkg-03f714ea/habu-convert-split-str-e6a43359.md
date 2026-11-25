---
title: Convert split-str engine lane e14
status: closed
priority: 2
issue-type: task
created-at: "2026-11-26T00:52:05.040234+01:00"
closed-at: "2026-11-26T01:09:45.189704+01:00"
---

Problem: the files in /tmp/jerry-p1/unmark/b-e14.txt, which hb's own build loads, still carry the split-str marker. Acceptance: none carries it; the engine rebuilds itself and its tests pass. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-unm-e14.
