---
title: Name the DP and heap cells by BOOT-LAYOUT
status: closed
priority: 2
issue-type: task
created-at: "2026-11-04T14:14:14.907693+02:00"
closed-at: "2026-11-04T14:30:42.639261+02:00"
close-reason: landed 7934570e
---

habu1.f EM-LAYOUT mirrored BOOT-LAYOUT's HEAP-START, DP-FLOOR and DP-CEILING cells on the claim that the engine build compiles against the host's layout names. False: the installed host has no DP-FLOOR-CELL, yet native-build.f built a tree naming BOOT-LAYOUT:DP-FLOOR-CELL in habu1.f (data-claims.f already does). EM-LAYOUT goes; habu1.f and habu2.f name BOOT-LAYOUT. Workspace .jj-ws.noindex/jerry-mirror on the dpcell commit; lead's own commit.
