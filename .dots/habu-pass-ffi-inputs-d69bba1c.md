---
title: Pass FFI inputs past the eighth register on the stack
status: active
priority: 2
issue-type: task
created-at: "2026-11-14T23:36:25.850400+01:00"
---

Problem: src/lib/ffi-abi.f PLAN-SPILLS gives a non-variadic arm64 call no stack cells, so a declaration with a float input or result and more than 8 integer inputs, or more than 8 r inputs, is rejected (REG-ROOM from PLAN-CALL: INT-REGS-FULL, FLOAT-REGS-FULL), though AAPCS64 and Apple arm64 pass such inputs on the stack in argument order; an integer-only call already spills past x7 (ffi-call-n) and SysV spills go in order through X86-PLACE and STAGE-SPILL. Acceptance: on arm64 such a declaration loads and its calls return C's answer, tested against a fixture library; one placement word serves both ABIs; macOS variadic staging is unchanged. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-spill.
