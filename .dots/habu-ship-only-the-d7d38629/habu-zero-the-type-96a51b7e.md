---
title: Zero the type-variable boot planes at capture
status: closed
priority: 1
issue-type: task
created-at: "2026-09-30T18:50:15.065383+02:00"
closed-at: "2026-09-30T21:10:00.000000+02:00"
close-reason: "Premise false: no such boot buffers exist on master. Dot habu-init-checker-scratch-4c2afab4 retired them; the eleven type-variable planes are one mmap arena (checker.f:216-239, ARENA-ALLOC :168) that TV-SNAP-RESET unmaps and nulls at every capture (:10826, via CHECKER-CAPTURE-PREPARE). The master census (size-census/ck-census.out:66-77) charges 0 value bytes to TV-CAP, TVT-P and the other pointer cells; the first checked definition maps the planes below data-base. The 51,840 B came from the historical owners table in docs/engine-size.md, measured before 4c2afab4. Nothing to zero."
---

Problem: TVT-BOOT, RVT-BOOT, EC-TV-BOOT and EC-RV-BOOT ship 1,280 present cells each: 12,960 B of image each, 51,840 B in all. `TV-SNAP-RESET` (`checker.f:10828`) releases the mapped arena but leaves the boot buffers' build-time content.
Acceptance:
- First show whether the four planes are scratch (re-mapped on first use through TV-READY) or carry state a checked definition after boot reads.
- If scratch: zero them at capture, as `ARENA-SNAP-BOOT` (`:10834`) does for the other scratch arenas, and add a regression in which a checked definition after boot still certifies.
- If not scratch: record why, and close this dot with that evidence.
Files: `src/core/checker.f`.
Verify: data-table census rows before and after; `test/run.f`; generations byte-identical.
Depends: none. Parallel with everything (a different region of `checker.f`).
Parent: habu-ship-only-the-d7d38629. Design: the Fable surface design of 2026-09-30 (~/.cache/tmp/heron-arm64/design-surface.md); census: ~/.cache/tmp/heron-arm64/size-census/.
