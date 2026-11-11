---
title: Bring image-bytes.f, driver-io.f and x86 elf.f comments and requires to the card
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T09:29:12.457196+02:00"
---

Problem: found by the review of the file writers' dead-words commit (2026-11-11), in code it did not touch: src/os/image-bytes.f names a dot id in a comment near the top ('dot habu-refuse-a-ptr-5ad2734e') and its M-FAIL comment tells history ('proven: the 104-byte MSIZE overrun exited 75 with no output'); src/habu/driver-io.f calls FDIO:WALL without requiring src/habu/fdio.f (card section 7; fdio.f's header documents the load-order reliance); src/os/linux-x86-64/elf.f keeps the size-tail words (ELF-TAIL-RC, ELF-TAIL-N, IMG-TAIL-N, IMG-TAIL-NAME, IMG-TAIL-BYTES) whose comment cites the deleted DRV-SIZE-MARKS, keeps SNAP-EXTRA-PTR, SNAP-EXTRA-SIZE and BUILD-SNAP-HDR uncalled, and its header says image-bytes.f sizes MSIZE from a bare CODE-CAP-BYTES while the writer sizes it through MSIZE!. Acceptance: the two arm64-side comments state present facts without ids, driver-io.f requires what it calls or the reason it cannot is one present-tense line, and the x86 items are deleted or corrected in the x86 step. Files: src/os/image-bytes.f, src/habu/driver-io.f, src/habu/fdio.f, src/os/linux-x86-64/elf.f. Verify: the engine built twice byte-equal; the x86 rows when that step runs. Consumers: none meets it. Depends: the x86 items wait for the x86 step. Ownership: carl. Claim: unassigned.
