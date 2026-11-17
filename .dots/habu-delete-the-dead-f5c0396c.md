---
title: Delete the dead pending-owner checks
status: active
priority: 2
issue-type: task
created-at: "2026-11-17T11:02:55.262376+01:00"
---

Problem: src/core/checker.f PENDING-OWNER-READY (~16583) and PENDING-MARKED (~16664) are never set to anything but 0, so PENDING-OWNER-* and PENDING-DIRTY+ never do anything. Acceptance: both cells and the code they guard are gone; the checker tests pass. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-pd1.
