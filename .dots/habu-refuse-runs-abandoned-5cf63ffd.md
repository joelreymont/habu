---
title: Refuse runs abandoned under an unjudged run
status: active
priority: 2
issue-type: task
created-at: "2026-10-29T10:08:02.385094+02:00"
---

RUN-AFTER (tools/native-unit-dependency.f ~1875) returns at ticket 0 before its run-abandoned loop, so a seed run observed inside a user word's evaluate that throws to a catch in that word keeps its ticket, and the file can be reused. Every ticket opened while a file is open must be closed by its after or refused run-abandoned. Lane carl-abandon (brief .out/brief-abandon.md); found by the FFI scratch review; habu-rest-ffi-declaration-0c289325 lands after it. Claim: carl.

Claim (carl, 2026-10-30): symmrvvk rebased onto lane 11B spvqktuy in .jj-ws.noindex/carl-abandon; worker proves it on the stack (brief-runs.md part 1).
