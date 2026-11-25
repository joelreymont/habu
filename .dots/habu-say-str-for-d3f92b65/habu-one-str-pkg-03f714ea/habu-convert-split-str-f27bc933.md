---
title: Convert split-str engine lane e2
status: closed
priority: 2
issue-type: task
created-at: "2026-11-26T00:36:49.763659+01:00"
closed-at: "2026-11-26T00:49:06.764252+01:00"
---

Problem: 22 files hb's own build loads still carry the split-str marker (/tmp/jerry-p1/unmark/b-e2.txt). Acceptance: none of them carries the marker; each passes check.f; the engine rebuilds itself and its tests pass. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-unm-e2.
