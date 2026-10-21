---
title: Start websocket suite before the tail
status: active
priority: 2
issue-type: task
created-at: "2026-10-21T04:23:07.816168+03:00"
---

Latest integrated fullgate leaves websocket among its final asynchronous rows at39.346s; prior cold/warm gates also finish it near the tail39.610/48.340s. Registry order starts the unchanged row near the end. Move its existing SUITE block into the initial independent long-row group, preserving args/grants/budgets and all behavior. No setup/assertion changes or CPU savings claim. Verify unique identical registry block and direct row pass; independent review. Measure any actual wall-tail improvement in next required whole batch gate.
