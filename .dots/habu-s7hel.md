---
title: Fix register symbol mismatch in h0-codegen for native runtime
status: closed
priority: 1
issue-type: bug
assignee: ""
created-at: "2025-12-10T05:18:54.950556+02:00"
closed-at: "2025-12-10T05:23:57.427651+02:00"
close-reason: ""
---

When h0-codegen runs at native habu0 runtime (self-compile mode), register keywords like :x0, :x1 created at SBCL compile time don't match keywords created at habu0 runtime. The reg function fails with "Unknown register: ~S". Need to convert register dispatch to use runtime keyword comparison.
