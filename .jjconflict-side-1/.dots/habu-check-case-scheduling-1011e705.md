---
title: Check case scheduling inside a suite file
status: open
priority: 2
issue-type: task
created-at: "2026-08-01T18:21:33.484252+02:00"
---

Proven by experiment in the multi-block test leaf: deleting a case word from a test file's RUN leaves the suite green - the case silently never runs again. Suite registration schedules FILES, not a file's own cases. Wanted: a lint that walks a test file's -CASE/-CASES definitions and refuses one that no RUN (transitively) reaches, with the same fixture-built-to-fool-it discipline the lint standard demands. Found while adding the multi-block allocator fixtures (habu-test-the-multi-139f37ac); the lesson is in LESSONS.md.
