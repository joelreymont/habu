---
title: Factor typed byte writer layer
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-06-25T12:19:43.538458+02:00\\\"\""
closed-at: "2026-06-25T15:15:27.435058+02:00"
close-reason: "completed: factored shared target image byte layer into src/os/image-bytes.f; removed local ELF/Mach-O/signing cursor and endian writer definitions; wired tools/build-fixpoint.f, tools/srclist.f, filemap, shadow lint, and build-helper gate; validated image-bytes-test, trust-lint, stale-status-lint, filemap-lint, shadow-lint, focused build-helper bundle, full native gate, and recovery probe rc 69 with bin/hb sha fd83258137f0c679a6d738378beebe8e437a724d367fbd1a9759a6fb1a61f371 unchanged"
---

Finding F10. Evidence: docs/factorization-review.md:38; src/os/macos/macho.f:7, src/os/linux/elf.f:6, src/os/macos/sign2.f:34. Root cause: ELF, Mach-O, and signing duplicate byte cursor/store vocabulary. Fix: factor shared typed byte writer/reader words with endian stores and cursor copy/pad helpers. Why: executable writers should share one checked byte cursor contract. Validate with build-fixpoint, hb-build, image dump/compare, and full native gate on Linux and macOS.
