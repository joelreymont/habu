---
title: "The sparse window's Linux legs"
status: open
priority: 2
issue-type: task
created-at: "2026-08-18T21:47:26.043871+02:00"
---

master 669eb949 made the window's DATA sparse (v5: run table + S-WRUNS + span scalar). Two Linux measurements are owed: (1) test/aot-data-span-forge.f skips its PTY boot cases on macOS, so the boot decoder's CONTENT/TRAP coverage has only run through the capture suite's baked-program probe (M9); run the forge on Linux. (2) test/gate-size-attribution-test.f Linux rows are owed the same -5560 as macOS, split aot-seed -5692 / compile-exit +132, plus the product-side deltas. Device lane via ssh (Orin). Depends on nothing; blocks nothing.
