---
title: Route header stores into an open space
status: open
priority: 1
issue-type: task
created-at: "2026-11-04T21:49:02.953190+02:00"
---

Cross-build step 4 (design r2 3.3, amended by /tmp/jerry-big/step4-amend.md; brief /tmp/jerry-big/step4-brief.md; workspace .jj-ws.noindex/jerry-route when it starts). Found 2026-11-04: three product-boot pointer cells are written with plain ! after a separate ptr-cell-mark (src/core/checker.f:186-188 TARGET-CELL; src/habu/native-runtime.f:219-220,227 BIND-OWNER DECL-A/TARGET-DECL-A; src/lib/task.f:1166-1170 TASK-CHAIN-CELL), so routing the three primitives as 3.3 says cannot move them: with a space open the product's DECLARATIONS would land in the host's $360/$368 and the host's definers would call the product's checker. MARK records offsets from DATA-VA and refuses offsets above DATA-SIZE - 8, so an image cell is out of band. Waiting on Joel for two new public names: the space cell (BOOT, $6C8) and xt!'s twin for a data pointer (stores and declares, routed like xt!).
Lead picks 2026-11-04, for Joel (Phil's relay: long-term lens; public names stay his): an image cell's row goes in the image's own address-cell table at TB, as an offset from TB (the product's own offset), not in the host's table; ptr-cell-mark and scratch-cell-mark do not route, because after the pointer writer no caller marks a header cell (this narrows 3.3 from three routed primitives to two); the space cell is not copied into a task's region, so a task's header stores stay in its own region; include.f:2256's clear becomes xt! of 0, so set and clear address one cell; the !-clears in process-fork.f:111-114 (the child's own state) and host.f:298-305,:568-575 (old capture and the definition cache, deleted in step 7) stay; x86-64 gets the pointer writer's body now without routing, and its routing joins step 8 (one arch first); the routed path is first reached when step 5 opens a space.
