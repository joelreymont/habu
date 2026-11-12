---
title: Bring image-bytes.f, driver-io.f and x86 elf.f comments and requires to the card
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T09:29:12.457196+02:00"
---

Problem: src/os/linux-x86-64/elf.f keeps the size-tail words (ELF-TAIL-RC, ELF-TAIL-N, IMG-TAIL-N, IMG-TAIL-NAME, IMG-TAIL-BYTES, :330-344) whose comment cites the deleted DRV-SIZE-MARKS, and keeps SNAP-EXTRA-PTR (:290), SNAP-EXTRA-SIZE (:294) and BUILD-SNAP-HDR (:318), which only test/engine-suite.f calls. Acceptance: the x86 items are deleted or corrected in the x86 step. Consumers: none meets it. Depends: the x86 step. Claim: unassigned, owner carl.
