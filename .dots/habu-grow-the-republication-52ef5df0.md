---
title: Grow the republication log with the program
status: open
priority: 2
issue-type: task
created-at: "2026-08-03T00:10:52.873485+02:00"
---

src/compiler/native/publish.f LOG-MAX is a fixed table of 128 rows: the publication seam's record of every word it republished, which is the only thing that still knows what the old emitter produced for a name once the record has been rewritten. It cannot evict, so the ceiling is a refusal (E-NPUB-CAP). 32 was not enough for test/compiler/native-migrate.f once the float comparison leaf added eleven bodies; 128 is what the system migrates today. A whole-system migration - maki is hundreds of words - needs the log to grow with the program instead. The seam runs while the engine is compiling and has nowhere to allocate from, so the fix is either an arena the migration entry hands it or a log the caller owns; decide which before raising the number again.

EVIDENCE 2026-08-09 (from the clobber-growth lane, dot f1ada10f): raising the
clobber table alone freed nothing — the census's 1275 E-NCLOB-CAP refusals
became E-NPUB-CAP one for one, so LOG-MAX 128 is the NEXT fixed ceiling and the
cut cannot publish thousands of routines until it grows. The design question
this leaf left open is answered by the worked example now in
src/compiler/native/clobber.f: lib/vector.f columns, capacity ensured in the
validation phase (publish.f's LOG-CK is where the room must be taken so the
commit cannot throw), a structural backstop ceiling, growth/copy/truncate
regression tests through the real seam. Follow that shape.
