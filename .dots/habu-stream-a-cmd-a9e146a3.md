---
title: "Stream a command's stdout lines under an idle window"
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T19:29:11.593651+02:00"
---

Maki design r2 (Joel approved 2026-10-31; /Users/joel/Work/maki/.jj-ws.noindex/tim/.out/chat-tools/design-chatgpt-r2.md §4.5, §5 item 2, lane H3): CMD:RUN-LINES ( CMD:command ptr u8 n ms [ ptr u8 n -- ] -- CMD:command outcome ), as RUN-OUTCOME (src/lib/process-command.f:500) but ms is an idle window restarted by each stdout byte, each complete stdout line goes to the quotation as it arrives, stderr captured as today, cancel as today, timeout means the window passed in silence and the tree is killed. Caller: Maki's job runner (STALL-MS 300000, Joel's number). Files: src/lib/process.f poll loop (~641-700), process-command.f, process-command-test.f (lines 1 s apart under a 3 s window complete; silence past the window is timeout with the lines already delivered; cancel mid-run). Name to Joel through the interview before the lane. Ownership: carl. Claim: unassigned.
