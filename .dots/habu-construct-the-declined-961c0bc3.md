---
title: Construct the declined-BL-into-protected-WID case
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T11:35:40.619143+02:00"
---

Found by the prewindow landing (3443a30d), unexercised: a body the capture declines becomes a BL to a prefix word; if that word sits in a protected WID, EM-AOTWIDGATE rejects it at boot with rc 84. Zero production sites reach the decline today so nothing constructs the case. Build the fixture (a declined body whose BL target is WID-protected), assert the rc-84 rejection by message, and decide whether the decline should refuse EARLIER (at capture, naming the WID) rather than at boot. Files: test/aot-wid-suite.f or sibling, src/habu/aot-capture.f if the earlier refusal is ruled. Depends: none.
