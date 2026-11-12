---
title: Read a Linux arm64 program in the size report
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T15:11:16.968511+02:00"
---

Problem: tools/image-size-lib.f reads only a Mach-O program; an ELF executable is refused by name (:53), so --size-report and --report-json fail on every Linux arm64 build, and test/file-objects.f runs its report cases only on macOS (HB-TARGET-MACOS? at :506, :541, :559, :609, :773, :803), leaving files_reused and files_compiled unread on Linux. Acceptance: on Linux arm64 (ssh spark) the report measures the ELF program the linker wrote and its .map with the seven numbers summing to the file size, as on macOS; the macOS-only guards in test/file-objects.f go and those cases pass on spark. Depends: none. Claim: unassigned.
