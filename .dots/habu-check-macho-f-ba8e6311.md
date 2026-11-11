---
title: "Check macho.f's four format numbers against sign2.f"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T09:12:26.542171+02:00"
---

Problem: src/os/macos/macho.f MACHO-SIG-MAX is computed from four numbers that mirror sign2.f's CS-PAGE, CS-HASH and CD-HDR and its $14 SuperBlob header (macho.f, the comment above MACHO-SIG-MAX); sign2.f loads after macho.f, so the owner's constants cannot be read there, and nothing checks the copies. If sign2.f's numbers grow, a signature past the bound throws M-BOUNDS-RC (75) from image-bytes.f M-FAIL, never a bad image. Acceptance: the numbers have one owner both files read (a small constants file loaded before both), or sign2.f refuses at load when its constants disagree with macho.f's; the comment is cut to what is then true. Files: src/os/macos/macho.f, src/os/macos/sign2.f. Verify: the engine builds twice byte-equal; an edited CS-PAGE fails the load with a named message. Consumers: none meets it. Depends: none. Ownership: carl. Claim: unassigned.
