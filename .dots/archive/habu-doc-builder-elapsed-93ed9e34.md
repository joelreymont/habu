---
title: Document builder elapsed timeout
status: closed
priority: 2
issue-type: task
created-at: "2026-10-19T14:25:33.538260+03:00"
closed-at: "2026-10-19T14:28:25.202902+03:00"
close-reason: Documented supported HB_BUILD_TIMEOUT_MS default600000, positive1..2147483647ms range,2400000ms example, elapsed-time behavior and new-invocation scope. Checked exact public600 and current parsing/constants; runtime unchanged. Stuart received and accepted the supported override for his next regularCLI build only.
---

Stuart regular CLI builds hit the public600 builder default600000ms while child CPU remained active under preemption and no host suspension occurred. Installed HBB-READ-MAKER-TIMEOUT supports HB_BUILD_TIMEOUT_MS=1..2147483647, but public docs omit it. Document default, elapsed-time units, positive range, a2400000ms example and scope to new builder invocations. No runtime change or timeout inflation in tests.
