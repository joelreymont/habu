---
title: Drop stale process maps and genio resets at capture
status: closed
priority: 2
issue-type: task
created-at: "2026-09-30T15:12:33.680237+02:00"
closed-at: "2026-09-30T15:13:06.381194+02:00"
close-reason: Landed 3295ca0c, a2ba5cbc, 6a2f4785. Fable review accepted. T-CAPTURE-RESETS failed before the fix (expected 0 got 257); stripped-address no longer compiles the linker from source. Native gate 504/504 rc 0, 281.2 s wall, 1984 s pooled at load 78-105 (gated at d4d16008, pushed as d8867baf after rebasing over a dots-only commit).
---

Problem: a captured image kept the process map read by its builder (src/habu/proc-maps.f LOADED flag and rows in DATA), and lib/genio.f RESET-ROUTING's registered flag stayed set after PREPARE ran the one-shot hook, so a device made after the first capture never re-registered its reset. That blocked running test/stripped-address.f on the keyed linker image instead of compiling the linker from source. Acceptance: every capture drops the map (DYNAMIC-BUFFER storage, read on demand) and re-arms the genio reset; stripped-address loads the keyed linker image through LINKER-LOAD. Verify: lib/genio-test.f T-CAPTURE-RESETS fails before the fix (expected 0 got 257); stripped-address and the proc-maps readers pass standalone; full native gate.
