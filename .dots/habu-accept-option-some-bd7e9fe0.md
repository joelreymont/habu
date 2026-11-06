---
title: "Accept OPTION:SOME of a declared type's value"
status: active
priority: 2
issue-type: task
created-at: "2026-11-06T17:54:05.351802+02:00"
---

Problem: the checker refuses OPTION:SOME (src/lib/adt/option.f:6-9) applied to a value of a declared type (ENUM, STRUCTURE) unless the definition's declared output or the next word's input fixes the option's type: `ENUM kk chat embed ;ENUM : S ( -- ) construct kk embed OPTION:SOME drop ;` is rc 70 'at OPTION:SOME expected: a actual: kk'; storing it into a field or TYPED-VARIABLE of type option<kk> reports 'expected: kk actual: kk'; `5 OPTION:SOME drop` passes. Reported by phil (Zen: 'expected: call-kind actual: call-kind' storing an optional into a record field), 2026-11-06; engines back to 2026-11-04 refuse alike. Reproducers: .jj-ws.noindex/carl-opt/.out/repro/ (phil's r1-r10, the lead's a-i). Root cause: worker lane carl-opt (brief .out/brief-opt-rca.md); the fix is a second brief. Acceptance: every refused reproducer is accepted, the passing ones unchanged, an ill-typed store (option<kk> into an option<n> field) still refused, a rejected-program test through the real load path. Files: src/core/checker.f, src/core/type-family.f (to be named by the cause). Verify: the reproducers on a candidate; the checker suites. Depends: none. Ownership: carl. Claim: agent=carl workspace=.jj-ws.noindex/carl-opt.
