---
title: "Rename EFFECT's words that spell a global"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-14T21:19:15.334111+01:00"
closed-at: "2026-11-14T22:24:22.984262+01:00"
close-reason: "qtvxkvpl renames EFFECT's words that spelled a global"
---

Problem: EFFECT:VARIABLE, EFFECT:READ and EFFECT:NEW (src/core/effect.f) spell the globals variable, read and new, which docs/forth.md 'Names fold case' says a public word must not; the linker files a dependency by name (docs/architecture.md:214), so every program file spelling variable now names src/core/effect.f (test/file-objects.f STALE-CASE S2/S3 fail). Acceptance: they are EFFECT:VARIABLE-OF, EFFECT:READ-TYPE and EFFECT:EMPTY everywhere; file-objects passes. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-rename.
