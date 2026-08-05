---
title: Grow the republication log with the program
status: open
priority: 2
issue-type: task
created-at: "2026-08-03T00:10:52.873485+02:00"
---

src/compiler/native/publish.f LOG-MAX is a fixed table of 128 rows: the publication seam's record of every word it republished, which is the only thing that still knows what the old emitter produced for a name once the record has been rewritten. It cannot evict, so the ceiling is a refusal (E-NPUB-CAP). 32 was not enough for test/compiler/native-migrate.f once the float comparison leaf added eleven bodies; 128 is what the system migrates today. A whole-system migration - maki is hundreds of words - needs the log to grow with the program instead. The seam runs while the engine is compiling and has nowhere to allocate from, so the fix is either an arena the migration entry hands it or a log the caller owns; decide which before raising the number again.
