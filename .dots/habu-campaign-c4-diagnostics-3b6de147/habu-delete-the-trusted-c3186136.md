---
title: "Delete the TRUSTED: test of internal calls"
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T12:01:12.481072+01:00"
---

Problem: every internal (DNAME-INT) record a name lookup finds carries DNAME-OWNED (src/habu/primitive-registry.f DNAME), and engine helpers are not found by name (TRUSTED: X ( -- ) (FLUSHTO) ; is E-UNDEFINED), so src/compiler/native/dict.f INT-CALL?'s TRUSTED-CELL test and src/habu/layout.f's 'DNAME-INT without it' sentence decide no record, nor does src/habu/interpreter.f POLICY-CHECK-REC's DNAME-INT line (no admittable package holds an internal word). Acceptance: each such test in dict.f, t0.f and interpreter.f is deleted with its comment, or the record that reaches it is named; src/lib/vocabulary.f's header (~:41) drops the clause 'and the engine has not marked internal' with interpreter.f's test. Depends: none. Claim: unassigned.
