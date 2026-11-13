---
title: Keep signature text out of a stripped program
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T22:47:30.516459+01:00"
---

Problem: a stripped program carries the stack-effect text of words it does not contain. /tmp/jerry-child/app2/progt.f, a nine-line program, built stripped holds 136 signature strings in its data (4611 of 22656 written bytes, such as '( ptr u8 n n n -- ) primitive '); adding two CAST: words to src/lib/task.f and widening USER-ROW's effect grows that program's data by 144 bytes, all of it their signature text, with no change to its code. A stripped program has no dictionary. Acceptance: a hunt names where signature text is laid and why the stripped link keeps it, as path:line; then a stripped program's data holds no signature or other dictionary-only text, and a library word the program does not reach leaves the program's bytes unchanged. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-ref (read-only hunt).
