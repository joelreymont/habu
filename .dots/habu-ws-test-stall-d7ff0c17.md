---
title: ws-test STALL-CLOSES expires on a 16 KiB-page kernel
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T21:32:01.725159+01:00"
---

Problem: on Arch Linux ARM (Asahi, kernel 7.2.11, getconf PAGESIZE 16384, tcp_rmem min 16384), src/lib/net/ws-test.f STALL-CLOSES ends 'a deadline expired' (-2502) on master 76bbcbfe; spark (4 KiB pages, tcp_rmem min 4096) passes. PEER-RCVBUF (:68) is 4096. Acceptance: the cause is named as path:line and fixed where it lies; the websocket suite passes on 4 KiB and 16 KiB pages. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-h16ws.
