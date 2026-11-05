---
title: Map NORESERVE on Linux arm64
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T16:15:12.436538+02:00"
---

Problem: src/os/linux/sys.f OS-MMAP-FLAGS keeps MAP_SHARED|MAP_PRIVATE|MAP_FIXED ($13) of the macOS flags hb passes and turns macOS MAP_ANON $1000 into Linux $20; macOS MAP_NORESERVE $40 is dropped. The cross-build's open (step 5 S5, /tmp/jerry-big/design-s5.md open step 2) maps DATA-SIZE bytes private, anonymous, NORESERVE for the product's space, so on Linux that mapping, DATA-SIZE $4000000000 (256 GB, src/os/linux/layout-constants.f), would be charged in full instead of by touched page (sys.f MAP-DATA-RESERVE comment: MAP_NORESERVE is Linux's lazy anonymous memory under overcommit modes 0 and 1); mode 0 refuses a charged mapping larger than RAM plus swap. Acceptance: Linux OS-MMAP-FLAGS also turns $40 into $4000; nothing else in the flag translation changes; the sys.f header comment names the translation. Files: src/os/linux/sys.f OS-MMAP-FLAGS. Verify: on spark (Linux arm64), a Linux hb built from the tip runs S5's open/close test, and while the space is open /proc/self/smaps for the space's range shows VmFlags 'nr'. Depends: S5b for the end-to-end check (its first caller); the edit itself may land before the first Linux run. Ownership: src/os/linux/sys.f. Claim: unassigned.
