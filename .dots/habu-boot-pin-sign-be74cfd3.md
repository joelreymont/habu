---
title: "Boot-pin sign-off: bump BPT-PFX-ROW# 22->26"
status: open
priority: 1
issue-type: task
created-at: "2026-07-11T19:56:47.036851+02:00"
---

FOR THE TFAM LANE - user sign-off RELAYED 2026-07-11 via the maki orchestrator. The boot-prefix growth 22->26 is APPROVED: the 4 added PFX-LOAD-ROW files were verified by direct diff of src/habu/habu2.f between the pin commit (plnynxmk) and master = src/core/type-family.f, src/core/type-schema.f, src/core/sumtype.f, src/core/type-family-sha.f - exactly the TFAM type-system core the checker needs at boot for family-typed code; no removals; each landed via reviewed gated TFAM commits (2a, 6, 8, derive). Boot-pin suite re-run on the maki side: digest determinism + drift-detection legs PASS; the ONLY red is the count assert (expected 22 got 26). Action (tfam lane): bump BPT-PFX-ROW# in test/boot-pin-test.f to 26 with a comment naming the 4 files + this sign-off, re-run the boot-pin suite + gate-stdlib path green, close this dot.
