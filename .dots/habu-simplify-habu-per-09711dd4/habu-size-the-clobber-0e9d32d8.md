---
title: "Give the clobber lint a row for every definer's name"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T22:41:40.788231+02:00"
closed-at: "2026-11-24T19:30:37.860585+01:00"
close-reason: "the clobber lint is deleted: nothing ran it and the suite drops lints"
---

Problem: the clobber lint (tools/lint/clobber-lint-core.f, which reports the registers each emitter routine reached by a branch-and-link call clobbers) gives a row only to colon definitions, so a name another definer makes does not hide a global host word: a package's '12 constant SCRATCH' counts 1, a false finding; no engine name has that shape today. It reads src/habu/rt.f, crash.f, habu1.f, prof.f and habu2.f (EACH-FILE); habu1.f and habu2.f are machinery being replaced (docs/design.md "Machinery being replaced"), so its subject shrinks as they go. Acceptance: every name a top-level definer makes gets a row that emits nothing, so the constant case counts 0, and a test of the lint holds that case; 'bin/hb --load tools/lint/clobber-lint.f' gives the same findings before and after on master. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-unrun.
