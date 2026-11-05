---
title: Map NORESERVE on Linux arm64
status: closed
priority: 2
issue-type: task
created-at: "2026-11-05T16:15:12.436538+02:00"
closed-at: "2026-11-05T22:37:27.084905+02:00"
close-reason: "landed as \"Map NORESERVE on Linux arm64\": on spark (overcommit 0, 121 GB) a Linux arm64 hb without it fails LINKER:WITH's open with E-MEM-MAP rc 67; with it test/linker.f passes and the open space's VMA shows VmFlags nr; reviewed"
---

Problem: src/os/linux/sys.f OS-MMAP-FLAGS keeps MAP_SHARED|MAP_PRIVATE|MAP_FIXED ($13) of the macOS flags hb passes and turns macOS MAP_ANON $1000 into Linux $20; macOS MAP_NORESERVE $40 is dropped. The cross-build's open (step 5 S5, /tmp/jerry-big/design-s5.md open step 2) maps DATA-SIZE bytes private, anonymous, NORESERVE for the product's space, so on Linux that mapping, DATA-SIZE $4000000000 (256 GB, src/os/linux/layout-constants.f), would be charged in full instead of by touched page (sys.f MAP-DATA-RESERVE comment: MAP_NORESERVE is Linux's lazy anonymous memory under overcommit modes 0 and 1); mode 0 refuses a charged mapping larger than RAM plus swap. Acceptance: Linux OS-MMAP-FLAGS also turns $40 into $4000; nothing else in the flag translation changes; the sys.f header comment names the translation. Files: src/os/linux/sys.f OS-MMAP-FLAGS. Verify: on spark (Linux arm64), a Linux hb built from the tip runs S5's open/close test, and while the space is open /proc/self/smaps for the space's range shows VmFlags 'nr'. Depends: S5b for the end-to-end check (its first caller); the edit itself may land before the first Linux run. Ownership: src/os/linux/sys.f. Claim: agent=carl workspace=.jj-ws.noindex/carl-noreserve

Evidence (spark, vm.overcommit_memory 0, 121 GB RAM; Linux arm64 hb cross-built on the Mac from master 16eb9979 with and without the edit, host the installed 6f3717e8 engine): without it, LINKER:WITH's open throws E-MEM-MAP (-3201, rc 67) in a smaps probe and in test/linker.f; with it, test/linker.f passes (test: ok, rc 0, 0.95 s) and /proc/self/smaps while the space is open shows one merged anonymous VMA 340000000-8340000000 (512 GB: hb's DATA plus the space right after it) with VmFlags rd wr mr mw me nr. The x86-64 twin is dot habu-map-noreserve-on-d42866b0.
