---
title: "Name ?dup's refusal over a layout value"
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T17:57:13.396769+02:00"
---

Problem: src/core/checker.f QDUP-STEP sets QDUPBAD for ?dup over a layout value, and check-walk.f:868 refuses on it with no refusal name or message, so ?dup over a layout value in a body, or over a one-cell one (an enum) at top level, says only "at '?dup'". Acceptance: each is refused E-LAYOUT-VALUE with a message naming ?dup. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/habu-name-dup-s-35b88586.
