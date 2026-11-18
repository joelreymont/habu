---
title: Delete the REQUIRE-BOOT-OPEN and REQUIRE-BOOT-FREEZE globals
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T17:22:22.783804+01:00"
---

Problem: src/core/include.f keeps two global words, REQUIRE-BOOT-OPEN and REQUIRE-BOOT-FREEZE, only because an installed hb's carried tools/hb-build-core.f END-WRITE writes them into end.f, which loads after the seal. Once an hb whose END-WRITE writes REQUIRE-REG:BOOT-OPEN and REQUIRE-REG:BOOT-FREEZE is installed, nothing writes them. Acceptance: both are deleted; ' REQUIRE-BOOT-OPEN drop is E-UNDEFINED; a cold engine build by the installed hb passes. Depends: c0423387 landed and installed. Claim: unassigned.
