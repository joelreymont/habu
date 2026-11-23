---
title: Say str for MAP keys
status: closed
priority: 2
issue-type: task
created-at: "2026-11-22T17:00:09.335043+01:00"
closed-at: "2026-11-23T10:11:21.238469+01:00"
---

Problem: src/lib/map-core.f spells a key ^u8 len, so callers add >LEN. Acceptance: key rows and EACH's quotation say str; callers pass str; suites pass. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-land
