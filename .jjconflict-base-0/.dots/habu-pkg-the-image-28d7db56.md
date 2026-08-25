---
title: Package the image writers
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T15:36:21.002485+02:00"
---

Retirement condition for the interim lint-allowlist entry landed at e625291b: tools/package-diff-lint-core.f's GLOBAL-IMPLEMENTATION? category now admits body edits to src/os/{image-bytes,elf,macho}.f (plus the pre-existing render/icode/mnem entries) because the lint rejects EVERY change to an unpackaged file - measured: a one-line constant change reported E-PACKAGE-OWNERSHIP. The real fix: open packages in the three image writers (and the three pre-existing entries if their condition matches) and migrate the bare callers - driver-io.f, snap-lib.f, habu2.f, object-image.f, image-bytes-test.f. Each admitted path leaves the allowlist as it gains its package. Files: src/os/*.f, tools/package-diff-lint-core.f. Depends: none.
