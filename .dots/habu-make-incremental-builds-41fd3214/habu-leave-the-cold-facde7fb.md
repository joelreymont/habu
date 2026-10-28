---
title: Leave the cold image after a family file hit
status: closed
priority: 2
issue-type: task
created-at: "2026-10-25T04:38:32.738895+01:00"
closed-at: "2026-10-28T17:13:32.656439+02:00"
close-reason: landed 0ff39c03
---

Workspace carl-famw (base qtstsksr). A reused file declaring a private ENUM or STRUCTURE in a package gives a warm product differing from cold (2.44M / 360K) at the integration tip; blocks lane wl (baf25f2b): types {46 51}, auto {227}, frame {263} there. Brief ~/.cache/tmp/carl-file/brief-famw.md.
