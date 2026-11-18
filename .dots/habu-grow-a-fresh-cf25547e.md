---
title: Grow a fresh byte buffer
status: active
priority: 2
issue-type: task
created-at: "2026-11-18T13:16:26.489396+01:00"
---

Problem: src/lib/byte-buffer.f GROW-CAP (:87-95) doubles a capacity of 0 to 0, so BUF:APPEND-SPAN on a buffer the header calls fresh (capacity 0, never INITed) loops forever. Acceptance: appending to a fresh buffer grows it; a byte-buffer test appends to a fresh buffer and fails (hangs, under its WAIT line) first. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-bufgrow.
