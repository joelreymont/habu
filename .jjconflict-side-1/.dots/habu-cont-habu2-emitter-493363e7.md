---
title: Continue habu2 emitter packaging
status: open
priority: 2
issue-type: task
created-at: "2026-07-28T18:10:39.897485+02:00"
---

Full context: the first packaging pass (dot habu-pkg-seal-and-79ae5370) established seams KWDATA, LOOP-EMIT, LASTC-TRUST, DOESPATCH, INTERP-EMIT, COMPILE-EMIT, LABELS, and reopened ENGINE-BUILD in src/habu/habu2.f. Still global: EM-COMPILE-CONTROL/STRING/META-KEYWORDS, the EM-INTERPRET-COLON/FIND siblings, the J-IF/J-THEN control family, EMIT-CREATE, and the remaining dictionary-section emitters. Migrate them into the established seams (same membership-with-unchanged-text technique to terminate the changed-caller cascade). Also fix the stale comment src/habu/habu1.f:2608 that still names EMIT-LABEL-CORE (now LABELS private CORE) — allowed once the paren-lex fix is in the base. Gate: package-diff-lint 0 findings on the artifact, engine build slice green, fixpoint census 4224, image delta limited to the owner-WID renumbering class documented in habu-capture-owner-wids (or byte-identical once that dot lands).
