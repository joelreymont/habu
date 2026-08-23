---
title: BF-SOURCE-BUF is one scratch shared by five call sites
status: open
priority: 3
issue-type: task
created-at: "2026-08-23T12:48:23.074972+02:00"
---

Problem: tools/build-fixpoint.f BF-SOURCE-BUF is a single growable scratch that the diag-origin capture, the user-source read, the object-text read, the engine-source reads and the stream chunk take turns with (five call sites across BUILD-FIXPOINT and HB-BUILD-CLI); after the 2026-08-23 resizing it grows on demand, so a future caller that holds a pointer across another caller's BF-SOURCE-ENSURE reads freed memory - the invariant is stated in a comment, not enforced. Also: BF-RUN-ARGV-ENV-OUTFILE (the child-stdout-to-file spawn with a deadline) lives in BUILD-FIXPOINT only because lib/process-env.f is an unpackaged legacy file whose eight RUN-ARGV-ENV-* siblings are frozen globals; its home is process-env.f once that file is sealed (habu-unpackaged-modules-frozen-780b5f31, habu-twelve-process-capture-73563893). Acceptance: one buffer per concern (or one buffer with an owner token that ENSURE refuses to grow while held), the comment-invariant replaced by a check; BF-RUN-ARGV-ENV-OUTFILE moved into the process package when it exists. Files: tools/build-fixpoint.f, tools/hb-build-lib.f, lib/process-env.f. Verify: build-fixpoint and hb-build tests. Depends: habu-hb-build-cannot-d09df17e (landing). Ownership: build tool. Claim: unassigned.
