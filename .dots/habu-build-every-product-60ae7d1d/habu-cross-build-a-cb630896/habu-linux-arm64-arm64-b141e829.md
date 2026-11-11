---
title: "Linux arm64: arm64-link-run and file-objects fail on spark"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-11T08:58:57.259004+02:00"
closed-at: "2026-11-11T09:30:21.190714+02:00"
close-reason: "landed: arm64-link-run's bound is 128 KiB, rc 0 on the Mac and on spark; file-objects Q is the libzip name, dot habu-zip-asks-linux-88bcf59f"
---

Problem: on spark (Linux arm64) with the Mac-built hb of the cross-OS stack (spark:~/carl-runs/xos4/habu, 2026-11-11), bin/hb --load test/compiler/arm64-link-run.f ends rc 1 with 4 failures, the last 'each file is under 64 KiB: only the kernel code the program reaches is written' (assert 14, expected true got false), and bin/hb --load test/file-objects.f ends rc 1 at 'Q: the build exits 0' (assert 276, expected 0 got 74). Both pass on the Mac. Logs: spark:~/carl-runs/xos4/o/red/arm64-link-run.log, file-objects.log. Causes (2026-11-11): arm64-link-run bounds each stripped program at 64 KiB, and an ELF file is 65,728 bytes at the least because src/os/linux/elf.f pads the text to one PROT-PAGE-MAX page (64 KiB) and appends the 192-byte read-write tail; the test's bound, not the writer, was wrong, and the bound is now 128 KiB (the whole kernel is 207,552 bytes), rc 0 on the Mac and on spark. file-objects Q fails because the build it runs loads libzip and spark has no libzip.so.5 on the loader path (stderr: E-FFI-DLOPEN: libzip.so.5), which is dot habu-spark-lacks-the-eea347b1, not a Habu defect. Acceptance: the cause of each failure is named with path:line and fixed at its owner; both tests pass on spark and on the Mac. Files: unknown until the cause. Verify: both tests on spark. Depends: none. Ownership: carl. Claim: agent=carl workspace=.jj-ws.noindex/carl-eo (commit "Bound a stripped program's size on every target", lands with the ENGINE-ORIGIN batch).
