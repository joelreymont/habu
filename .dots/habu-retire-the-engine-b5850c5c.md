---
title: "Retire the engine's LEGACY paths and names"
status: open
priority: 2
issue-type: task
created-at: "2026-11-01T02:24:48.794165+02:00"
---

Problem: docs/design.md "Machinery being replaced": hard cutover, Habu has no legacy. Engine words still name or serve a legacy path: src/core/generated-declaration.f REPLAY-LEGACY (public GENERATED-DECL-CTOR word; SUMTYPE and PRODUCT still generate constructors in sumtype.f outside the participant; callers verify-source.f ~2522 ~2609, check-hook.f header, test/runtime-regression-test.f ~1523-1554, tools/check-test-lib.f ~3620); src/core/type-family.f SUMV-FAMILY-LEGACY-PAYLOAD? (~1820, throws E-TFAM-PAYLOAD); src/habu/layout.f SNAP-TRL-LEGACY-BYTES (a pre-version snapshot trailer the loader recognises to fail rc 80; habu2.f ~8488, boot-x64.f ~178-188); src/habu/habu2.f COMPILE-EMIT:EM-COMPILE-LEGACY (the JIT compile loop, kept for experiments). Acceptance: each path that exists only for an older format or an unfinished migration goes (finish the migration: SUMTYPE/PRODUCT constructors through the participant), and a path that stays is named for what it does now, with no LEGACY in any name; rows that pinned a removed path go with it. Files: the four above and their callers. Verify: engine build; rows runtime-regression, check (tools/check-test-lib.f), snapshot rows; full suite (shared checker and image paths); generation chain if the snapshot loader changes. Depends: none. Ownership: unassigned. Claim: unassigned.
