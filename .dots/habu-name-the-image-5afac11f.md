---
title: Name the image text end portably for the code bound
status: open
priority: 3
issue-type: task
created-at: "2026-09-18T04:45:29.635028+03:00"
---

Problem: src/habu/code-bytes.f (habu-read-code-bytes-844e7c50, 2026-09-18) bounds the baked text band as [rbase, dbase@) because the exact text-segment end is target-specific (LINUX-TEXT-SIZE in src/os/linux/layout.f), so CODE-BYTES:AT admits an address in the process's read-write tail between the real text end and the region mapping. Acceptance: a portable IMAGE-TEXT-END ( -- n ) in the OS layout layer, every target's layout.f answering it (linux from LINUX-TEXT-SIZE, macos from its own header, linux-x86-64 from its ELF text size), CODE-BYTES:AT bounding the text band with it, and a test that an address in the RW tail is refused. Files: src/os/*/layout.f, src/habu/code-bytes.f, test/compiler/code-bytes.f, docs/porting.md. Verify: the test; fixpoint; test/run.f. Depends: habu-read-code-bytes-844e7c50 landing. Ownership: OS layout. Claim: unassigned.
