---
title: "A dropped bare OPTION:NONE is refused with no catch"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T22:33:32.367303+02:00"
---

Problem: with `require src/lib/adt/option.f`, `: Y ( -- ) OPTION:NONE drop ;` and `: Z ( -- option<n> ) OPTION:NONE dup drop ;` are refused E-MISMATCH ('expected: a actual: option<b>') with no catch in sight, against docs/forth.md:1321-1326 ('a variable copied or dropped while still polymorphic is tainted ... Non-linear polymorphic code is unaffected'). Working guess, not traced: a payloadless constructor takes its type arguments only from the definition's declared output (src/core/checker.f CONSTRUCT-DECL-TERM :5414), so a value dropped before any output binds its variable stays open; habu-a-payloadless-constructor-97de4c1d (jerry's) holds that cause for typed locals and calls. Acceptance: Y and Z load and run (rc 0), with a linear value in the same shape still refused; or the cause is reported at file:line and shown to be 97de4c1d's, and its owner takes Y and Z as cases of that dot. Depends: none. Claim: unassigned.
