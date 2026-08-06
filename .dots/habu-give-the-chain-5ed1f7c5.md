---
title: Give the chain a no-emit compile mode
status: open
priority: 2
issue-type: task
created-at: "2026-08-06T16:07:09.741993+02:00"
---

THE structural blocker of the cut (thecut audit, 2026-08-06): the chain is a post-pass over the old emitter — its only input is the tape, whose sole producer is the checker's reader at every ';' AFTER the old emitter succeeds, and migrate.f reaches it via evaluate with PUBLISHED-ONE enforcing old-emitter success first. The cut needs compilation-without-publication: the checker certifies and produces the tape, the chain compiles it, and NOTHING publishes until the chain's publisher commits — the old emitter's emission becomes unnecessary rather than prerequisite. This is engine+checker surgery at the ';' seam (habu2.f EM-COMPILE / the reader), designed so a chain refusal leaves the definition uncompiled with a named reason. First consumer: the cut. Blocks habu-cut-colon-compilation-a5aa3f1f.
