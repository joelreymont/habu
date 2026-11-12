---
title: Find why a cold app build throws E-INTERN-REWIND
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T17:46:16.733766+02:00"
---

Problem: building /tmp/jerry-hunt/app-sealed.f (seals engine packages through TEST-EVAL:RC, some not yet made, then defines a word with a string literal) with tools/hb-build.f throws -8638 E-INTERN-REWIND cold, twice with fresh out roots, and the immediate rebuild over the same out root builds. A warm build must never differ from a cold one. Acceptance: the cause, as path:line with the evidence, kept apart from inference; the fix is a second brief. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/hunt-rewind.
