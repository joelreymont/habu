---
title: "Give the engine's raw map failures a record"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-18T07:27:44.182326+01:00"
closed-at: "2026-11-18T08:44:40.291608+01:00"
---

Problem: three engine maps still die with a bare line and no record because build hosts load their files, so REJECTION:MAP-STOP (src/core/rejection.f) must be in the installed engine first: src/habu/intern-table.f TABLE-MAP (:73, loaded before rejection.f, src/habu/boot-files.f :72-:73; rejection.f uses nothing of it, so the rows can swap), src/arch/arm64/icode.f CODE-ALLOC (:44) and ICODE-TAB-ALLOC (:52), and src/os/image-bytes.f M-ALLOC-BUF (:24), the last two loaded through src/os/macos/macho.f. src/core/arena-pages.f ARENA-ALLOC (:10) cannot call rejection.f, which grows its rooms through the arena (rejection.f ROOM-GROW :80); its first rooms are 16 KiB of image storage, so a short record can be built after an arena refusal. Acceptance: each refusal prints an E-MAP record (what was mapped, bytes, the address-space help) and exits SYSTEM. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-mapstop.
