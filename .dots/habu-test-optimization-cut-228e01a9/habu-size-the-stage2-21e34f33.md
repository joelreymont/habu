---
title: Size the stage2 reader from what it must hold
status: open
priority: 3
issue-type: task
created-at: "2026-10-02T19:08:13.934472+02:00"
---

Problem (lane 363 r4-gfstdin): src/habu/stage2.f:46 READ-SRC refuses a source of 4 MiB or more ('stage2: source exceeds buffer', SOURCE-CAP = SOURCE-ARENA-CAP), a fixed limit like the one b20c2e72 removed from the source arena; the stdin variant is 3,344,094 bytes today (about 20% headroom), so the Gforth recovery chain will fail as the compiler source grows. Acceptance: the reader sizes its buffer from the source it reads (file size, as SOURCE-ARENA-LEN does from SRCN) or from the same bound as the arena it feeds; a padded probe past 4 MiB passes recovery check-only; Gforth recovery rc 0. Files: src/habu/stage2.f, bootstrap mirror if any.
