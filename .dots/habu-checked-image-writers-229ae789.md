---
title: Checked image writers
status: open
priority: 2
issue-type: task
created-at: "2026-07-01T23:07:20.896981+02:00"
---

Rewrite src/os/{linux/elf.f,linux/sign.f,macos/macho.f,macos/sign2.f} under records + ptr-arith capability so BF-APPEND-CHECK-OFF (build-fixpoint.f:536) and BF-APPEND-IMAGE-TRUSTS (:555-560, 5 generated TRUST rows) are deleted - the image writers become ordinary checked source in stage2. Also converts aot-lib.f's open 0-set-check region (~237 ln ARM64 relocation core) into named TRUSTED: words at minimum (raw region -> named+tested boundaries). Effort M (~4d). Depends: ptr-arith + dict-record capabilities.
