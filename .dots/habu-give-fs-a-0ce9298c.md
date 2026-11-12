---
title: "Give FS a public limit for a file name's bytes"
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T22:27:48.324129+02:00"
---

Problem: FS has no public word for the longest file name a directory entry may hold, so a program that builds a name from parts restates the host's limit: Maki's src/build.f:448 '255 constant NAME-MAX' (reported by tim for Maki; nothing fails today). Habu's own tests restate it too (src/lib/fs-cause-test.f:22, src/lib/fs-mutate-test.f:1772). Acceptance: one public FS constant holds the limit in bytes (255 on macOS and Linux), FS's header says a longer component is refused with the code the failing call already throws, and Habu's own restatements read it. The word's name is a public-API choice to settle at dispatch. Files: src/lib/fs.f, the two tests above, docs/forth.md where FS is described. Verify: the fs tests through bin/hb --load test/run.f -- <names>. Depends: none. Ownership: unassigned. Claim: unassigned.
