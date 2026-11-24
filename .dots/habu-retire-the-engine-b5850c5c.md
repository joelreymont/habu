---
title: "Retire the engine's LEGACY paths and names"
status: open
priority: 2
issue-type: task
created-at: "2026-11-01T02:24:48.794165+02:00"
---

Problem: docs/design.md "Machinery being replaced": hard cutover, Habu has no legacy. Engine words still name or serve a legacy path: src/habu/verify-source.f LEGACY-TYPE-OUTCOME (:2664, the sumtype declaration's outcome at :3959). Acceptance: each path that exists only for an older format or an unfinished migration goes, and a path that stays is named for what it does now, with no LEGACY in any name; rows that pinned a removed path go with it. Files: src/habu/verify-source.f. Verify: engine build; row runtime-regression; full suite (shared checker and image paths). Depends: none. Ownership: unassigned. Claim: unassigned.
