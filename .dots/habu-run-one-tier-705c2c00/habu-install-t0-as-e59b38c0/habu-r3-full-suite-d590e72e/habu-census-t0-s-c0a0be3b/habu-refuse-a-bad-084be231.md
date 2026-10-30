---
title: Refuse a bad literal, MATCH and construct in T0
status: closed
priority: 2
issue-type: task
created-at: "2026-10-31T09:31:53.041838+02:00"
closed-at: "2026-10-31T12:11:57.954722+02:00"
close-reason: "Landed with the T0 release (ttloqwsx b17a1ee3): full suite 510/510, two generations converge, edit matrix F53/F105/F156 only, as master"
---

Workspace jerry-t0fb. Brief /tmp/jerry-big/t0fb-brief.md + t0f-common.md: out-of-range integer pushes 0; MATCH arm without OF compiles; MATCH bad tag / empty stack rc 67 -9606; construct unknown variant or family compiles nothing. T0 refuses rc 70 naming the token.
