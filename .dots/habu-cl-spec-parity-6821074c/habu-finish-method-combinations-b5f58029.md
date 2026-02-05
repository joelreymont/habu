---
title: Finish method combinations
status: closed
priority: 2
issue-type: task
created-at: "\"2026-02-05T12:16:28.228085+01:00\""
closed-at: "2026-02-05T22:07:03.241662+01:00"
close-reason: Added call-method, make-method, error helpers, standard symbol, and integration coverage
---

docs/cl-symbols.md:1065/1092/1095/1097/1109 mark missing call-method/make-method/invalid-method-error/method-combination-error/standard. Root cause: define-method-combination macro present but helper forms + standard combination not implemented. Fix: implement helper macros in lib/stdlib.habu + runtime support in src/runtime/primitives/clos.zig for calling effective methods; add tests in docs/clos-testing.md style exercising define-method-combination and errors.
