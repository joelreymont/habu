---
title: Fix nested eval exits
status: open
priority: 1
issue-type: task
created-at: "2026-02-17T22:23:16.853763+01:00"
blocks:
  - habu-rca-indirect-call-d9f594ad
---

src/interp/vm.zig non-local exits. Cause: nested eval and handler flows can lose cleanup/stack shape. Fix: consistent unwind and frame restoration rules.
