---
title: Resolve pre-pass names against the subject, not the checking tool
status: open
priority: 2
issue-type: task
created-at: "2026-10-02T18:39:44.808811+02:00"
---

Problem (lane 335 r4-inproc, 804b5887): src/habu/verify-source.f:362 FIND-SYM resolves names against the checking process's own dictionary: in-process (check-test's harness) a fixture naming lib/test.f's T= passes the pre-pass and the run refuses it (E-UNDEFINED: T=); the CLI refuses it at the pre-pass; the CLI does the same with its own FILE-SIZE (a word tools/check.f loaded). rc agrees (70) but the stage and diagnostic differ, and a subject can lean on the checker tool's words in the pre-pass. Fixtures: $HOME/.cache/tmp/kestrel-r4-inproc/p/hl/. Acceptance: the pre-pass resolves a name only in the engine's baked dictionary plus what the subject itself loads (the view the run has), so a tool-loaded word is undefined to the pre-pass in both modes, with the same diagnostic and stage in-process and on the CLI; seen failing first. Files: src/habu/verify-source.f (baked), tools/check-core.f, tools/check-test-lib.f.
