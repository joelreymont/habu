---
title: Track adopted portability work
status: open
priority: 2
issue-type: task
created-at: "2026-10-03T21:56:44.081016+03:00"
---

The portability design was adopted in 7b9871783499; it now lives in
docs/xcomp.md, docs/architecture.md "Builds" and the reconciled Wasm design. Its
review/adoption phase is complete; do not commission another historical review
or recreate handoff artifacts.

This group remains open for its concrete unfinished children: cross-target
created DATA, qualification of brought-up compiler hosts, and Windows native
profiles when a design and host lane exist. Windows and MCU implementation
remain parked. Completed or unnecessary protocol proposals are closed
individually. Current build-speed work stays under the incremental/shared-build
dots, with real native execution and required qualification at their boundary.
