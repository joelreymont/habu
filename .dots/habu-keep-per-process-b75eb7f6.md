---
title: Keep per-process family values out of a build
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T11:44:45.829691+01:00"
---

Problem: handles that live only in the process that made them are saved by a build: ZIP:archive, entry and input (src/lib/zip-types.f:5-7), WS:socket, MCP:client and held, PG:connection, FONT:font and image, FFI-CB:callback in typed storage; SQLITE:connection, FS-LIST:context, EVENT-STREAM:decoder and TASK:sem hold an address through a private cast in a cell with no address row, so E-ADDRESS never sees it. Acceptance: each such family carries the per-process mark of habu-refuse-a-build-894c93b1, and a build that keeps one is rejected naming the cell; ZIP:E-HANDLE's run-time check of a kept handle is deleted. Depends: habu-refuse-a-build-894c93b1. Claim: unassigned.
