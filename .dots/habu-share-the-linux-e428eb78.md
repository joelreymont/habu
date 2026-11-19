---
title: "Map nothing over the x86 image's region load"
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T08:41:48.809257+01:00"
---

Problem: x86's engine boot (src/habu/boot-x64.f CODE-REGION,) maps anonymous memory fixed over the region its image's own load already reserves. Acceptance: the x86 engine boot maps nothing over its region load; the x86 image unchanged but for that. Depends: none. Claim: unassigned.
