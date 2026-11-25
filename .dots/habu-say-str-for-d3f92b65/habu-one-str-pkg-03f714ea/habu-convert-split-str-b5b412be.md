---
title: Convert split-str engine lane e3
status: closed
priority: 2
issue-type: task
created-at: "2026-11-26T00:36:50.162402+01:00"
closed-at: "2026-11-26T00:54:15.827175+01:00"
---

Problem: 19 files hb's own build loads still carry the split-str marker (/tmp/jerry-p1/unmark/b-e3.txt). Acceptance: none of them carries the marker; each passes check.f; the engine rebuilds itself and its tests pass. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-unm-e3.
