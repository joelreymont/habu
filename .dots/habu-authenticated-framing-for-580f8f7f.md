---
title: Authenticated framing for schedule persistence
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T17:44:18.071254+02:00"
---

Review finding 5 (pin 8195257e): maki/store.f:231 treats EOF like newline so a torn "key|1" without LF replays as committed; maki/store-replay.f:85 sets ready BEFORE loading, so "good|7\nbad" threw -5095, retained good=7, marked ready, and returned success on retry - partial failed recovery published as success. Fix: authenticated record framing (length or hash-delimited, torn tail = named reject) + transactional staging with explicit cold|ready|failed(error) states; failed recovery must stay failed. SEQUENCING: land before or with habu-committed-autotune-menu-4321e05d - the committed winners table rides this store.
