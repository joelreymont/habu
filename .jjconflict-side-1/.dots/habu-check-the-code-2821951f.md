---
title: Check the code-signing format mirror
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T15:36:21.009598+02:00"
---

Found by the icode-window lane: macho.f's four code-signing format numbers ( page,  hash,  CD header,  SuperBlob-era) MIRROR sign2.f's CS-PAGE/CS-HASH/CD-HDR and nothing checks the mirror - macho.f loads first and cannot read them; a worst-case SB-SIZE fixture is macOS-only-nameable while the shared image test runs on Linux too. Failure mode today is the loud 73/75 die, never a bad image - but the mirror is a drift waiting to happen. Design the check (a load-order-safe assertion, or sign2.f validating against MACHO-SIG-MAX at its own load). Files: src/os/macos/{macho,sign2}.f. Depends: none.
