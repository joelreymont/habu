---
title: "Retire the engine's LEGACY paths and names"
status: open
priority: 2
issue-type: task
created-at: "2026-11-01T02:24:48.794165+02:00"
---

Problem: docs/design.md "Machinery being replaced": hard cutover, Habu has no legacy. Engine words still name or serve a legacy path: src/core/type-family.f SUMV-FAMILY-LEGACY-PAYLOAD? (:1593, throws E-TFAM-PAYLOAD at :1615); src/habu/layout.f SNAP-TRL-LEGACY-BYTES (:76, a pre-version snapshot trailer the loader recognises to fail rc 80; boot-x64.f :175-185). Acceptance: each path that exists only for an older format or an unfinished migration goes, and a path that stays is named for what it does now, with no LEGACY in any name; rows that pinned a removed path go with it. Files: the two above and their callers. Verify: engine build; rows runtime-regression, check (tools/check-test-lib.f), snapshot rows; full suite (shared checker and image paths); generation chain if the snapshot loader changes. Depends: none. Ownership: unassigned. Claim: unassigned.
