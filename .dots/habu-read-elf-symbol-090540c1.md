---
title: Read ELF symbol sizes for the clang reference column
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T10:51:30.541065+02:00"
---

tools/codegen-compare-macho.f reads per-symbol code sizes out of a Mach-O object through nm -m and size -m, so the comparison's clang reference column exists only on a Mach-O host; tools/codegen-compare-cc.f names that as the reason the column is absent elsewhere and points here. Add an ELF reader beside the Mach-O one - ELF symbol table entries DO carry st_size, so the derivation is simpler than the Mach-O one, and readelf -sW or nm -S prints it - plus the Linux build flags (-shared -fPIC, no -arch), so a run on the aarch64 Linux device gets three columns instead of two. Same seam: CODEGEN-MACHO:LOAD-FROM takes the two texts as parameters, so the ELF reader is a second parser behind one lookup surface and the scheduled test attacks it on listings built to fool it, the way the Mach-O one is attacked today. Retires the host restriction named at the head of tools/codegen-compare-cc.f.
