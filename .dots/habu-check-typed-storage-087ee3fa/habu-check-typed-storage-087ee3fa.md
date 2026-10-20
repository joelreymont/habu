---
title: Check typed storage stores at the top level
status: active
priority: 3
issue-type: task
created-at: "2026-09-23T11:30:21.005778+03:00"
---

Joel chose the same storage type rules at top level as inside definitions. The checker-owned interpret row refuses pointer and execution-token stores into raw or numeric cells, preserves typed pointees through calls and nested evaluation, and keeps the explicit unchecked window. Capture retains its stored-value checks because unchecked stores and builder-only pointers remain possible. Focused tests passed on the feature product; independent review, the combined native suite and convergence remain pending. Mark owns the isolated feature for Carl's combined integration.
