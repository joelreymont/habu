---
title: Name an unclosed locals group where it opens
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T05:55:00.944888+01:00"
closed-at: "2026-11-16T04:07:35.372609+01:00"
---

Problem: an open {: is reported as the definition left open, at its name in a run and at {: in check.f, with a wrong help. Acceptance: bin/hb FILE, check.f and --verify-only print one and the same record for it, rc 70 (74 for an unreadable file). Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
