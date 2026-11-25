---
title: "Keep an owned alias's word time"
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T23:32:27.660644+01:00"
---

Problem: src/habu/linker.f WORD-TIME (3709) gives a word bound to a record with no time the time now, and OWNED-RECORDS (5462) lays the owned primitives' records at time 0, so every compile of a file holding an alias such as EXPORT BOOT:data-base gives the alias a new time and fails each dependent's WORDS row (3147): touching src/core/checker.f with no change recompiles 46 objects, 3.99 s against 0.49 s. Acceptance: touching checker.f recompiles checker.f's object only, and a warm build equals a cold one. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-touch.
