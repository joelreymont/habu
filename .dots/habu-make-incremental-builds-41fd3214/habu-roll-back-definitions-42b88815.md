---
title: Roll back definitions in caught evaluates
status: closed
priority: 2
issue-type: task
created-at: "2026-10-30T18:21:13.295437+02:00"
closed-at: "2026-10-31T20:52:54.553592+02:00"
close-reason: "Landed with lane 11 (ROLL merged): spike-slices 0 hits; ROLLED case in test/native-file-auto-e2e.f."
---

A cold file build throws -8597 (E-NUNIT-PROFILE, FILE-ENCODE 'spike-slices') when a colon definition compiled inside evaluate-closed under catch is rolled back: the file's checker slices and index rows disagree (o:1190, o:3017 at spvqktuy). Reproducers: carl-probe/.out/probe/p3.f (ROLL-HIT-CASE), `s" : Q ( -- ) ; 1 throw" ['] evaluate-closed catch drop`, test/native-file-frame-e2e.f dying at incut.f. Blocks 063c95e4.

Claim (carl, 2026-10-30): worker in .jj-ws.noindex/carl-froll on spvqktuy, brief carl-dots/.out/carl-file/brief-fix-roll.md.
