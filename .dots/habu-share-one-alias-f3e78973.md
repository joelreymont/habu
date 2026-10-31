---
title: Share one alias-copy emitter in habu2.f
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T10:48:28.680228+02:00"
blocks:
  - habu-land-lane-11-5a304603
---

Lane 11's xtyzkytp 'Write the created signature through the alias' adds C-PLACE-CREATED-SIG (src/habu/habu2.f ~:3209 on the lane 11 stack), whose inline copy through the RW alias duplicates C-SDQ (~:6081) and C-CQ (~:6120). One shared alias-copy emitter serves all three; delete the two inline copies. After lane 11 lands.
