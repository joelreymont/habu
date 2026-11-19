---
title: Five address handles state their address
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T11:44:45.829691+01:00"
---

Problem: SQLITE:connection (src/lib/sqlite.f:36), FS-LIST:context (src/lib/fs-list.f:38), EVENT-STREAM:decoder (src/lib/net/event-stream.f:24), TASK:sem (src/lib/task.f:84) and CURL:handle (src/lib/net/curl.f:54) hold an address behind a nominal cell with no address row, so a build that keeps one saves a host number and LINKER:E-ADDRESS never sees it. Design: .out/designs/design-scratch-values.md item 3: each family states its pointer base (names and widths unchanged); ZIP, FONT, PG, WS and MCP handles are identity numbers whose saved counter already rejects a kept one, and do not change. Acceptance: a built program that keeps one handle of each of the five families is rejected by E-ADDRESS naming the cell, cold and warm; the engine builds; Zen, Tender and Maki build cold and warm. Depends: none. Joel 2026-11-19 named CURL's empty handle CURL:EMPTY-HANDLE ( -- CURL:handle ); it replaces `0 CURL:>HANDLE` (Tender tenderd/request.f:122, kevin's move, told before the install), and the public CURL:>HANDLE and CURL:HANDLE>N go. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-hand, .jj-ws.noindex/jerry-curl, .jj-ws.noindex/jerry-sem.
