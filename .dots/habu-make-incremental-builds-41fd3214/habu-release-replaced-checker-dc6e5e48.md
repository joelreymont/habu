---
title: Release replaced checker store mappings
status: closed
priority: 2
issue-type: task
created-at: "2026-10-23T19:58:44.693153+03:00"
closed-at: "2026-10-28T17:13:32.885765+02:00"
close-reason: landed 18228369
---

Since the file seam, owner resets drop stores to boot rows at every file boundary; grow words (ARENA-BYTES-GROW callers: TDPLAN, TDPV, CTOR-PEND, DEV, protection GROW, TX-GROW) and resets (STAGE-P, DEV-A-P, DEV-TX-P) leak the replaced mapping once per file. Brief ~/.cache/tmp/carl-file/brief-grow-leak.md. Base: linear carl tip. Workspace carl-leak (worker running, on wvxplqoy).
