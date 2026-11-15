---
title: Pass a record to a variadic C function
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T04:52:35.298926+01:00"
---

Problem: FUNCTION: rejects a record input in a VARIADIC declaration (src/lib/ffi-abi.f STRUCT-VARIADIC), so no Habu program can call a variadic C function with a struct by value. Apple arm64 gives each variadic input 8-byte stack slots and PLACE-NEXT has no record case for that. Acceptance: a variadic call with a record input passes it by each target's rule, tested on Linux and macOS arm64. Depends: habu-pass-records-with-af1b7f72. Claim: unassigned.
