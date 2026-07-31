---
title: Remove the unconsumed checking certificate
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-31T21:21:11.180399+02:00\""
---

src/compiler/native/cert.f (package NCERT) publishes a verdict-plus-two-digests result that no production stage reads: the end-to-end chain test binds source to module through A64SEL's own digest check, and the only NCERT readers are its tests. Delete the module rather than keep it warm: remove cert.f, rework NFEED:END-UNIT to answer the sealed tape view and the verdict directly, move any digest-matrix test facts that are really about NTAPE or the source registry to those suites (the two-digests insight is recorded in LESSONS.md and stays), retire the -8410..-8419 error block in lib/errors.f's map, update docs/compiler-ir-design.md 7.1, and close the now-moot seal dot habu-seal-the-checking-ea38d83b. When the migrated compiler entry (habu-migrate-the-first-fe78ec52) needs to bind verdict to tape, it does so in-process against NFEED's own state - identity implies content in one process; a certificate value gets rebuilt only if a cross-process consumer ever exists.

Claim: agent=certkill workspace=.jj-ws/habu-remove-the-unconsumed-701ce6d1
