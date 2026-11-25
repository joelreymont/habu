---
title: Convert split-str engine lane e10
status: closed
priority: 2
issue-type: task
created-at: "2026-11-26T00:40:35.661716+01:00"
closed-at: "2026-11-26T08:45:40.716313+01:00"
---

Problem: src/core/checker.f, which hb's own build loads, still carries the split-str marker (about 640 sites). Acceptance: it carries no marker and passes check.f; the engine rebuilds itself and its checker tests pass. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-unm-e10.
