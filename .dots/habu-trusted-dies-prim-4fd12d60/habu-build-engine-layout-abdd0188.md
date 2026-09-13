---
title: Build engine layout and emitted code from the same target source
status: open
priority: 1
issue-type: task
created-at: "2026-09-13T14:51:13.073131+03:00"
---

Cedar review of 5226a994, 2026-09-13: tools/native-build.f loads habu2.f against the resident host layout and loads the target layout only later in LOAD-TARGET. docs/bootstrap.md explicitly records a generated engine reporting capacity 65536 while enforcing 32768. Using a second generation or an old source-reading seed hides this mismatch. Separate host-state interpretation from target-layout emission so one normal product-hosted build uses the target ABI consistently; verify a changed reserved band in the first generated engine. Required for reliable AOT selfbuild and Tender capacity, not a new seed-management framework. Unassigned.
