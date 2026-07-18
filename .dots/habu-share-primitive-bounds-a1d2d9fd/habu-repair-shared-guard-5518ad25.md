---
title: Repair shared-guard regressions
status: active
priority: 2
issue-type: task
created-at: "2026-07-18T10:24:27.955825+02:00"
---

Own test/seal-absence.f and test/protection-span.f. Replace spoofable substring recognition with the existing source lexer/token APIs so only real GUARD-SPAN tokens or the exact [ also GUARD ] CALL-SPAN [ previous ] token sequence count; return typed bools and pin exactly two bodies/eight calls. Fix the atomic CAS positive fixture by wrapping executable source in a named checked definition. Acceptance: exact native test loads pass, self-tests reject guard text in strings/comments, typed-local diff lint clean.
