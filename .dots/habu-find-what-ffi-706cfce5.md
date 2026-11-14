---
title: "Reject FUNCTION: types C does not pass, at the token"
status: active
priority: 2
issue-type: task
created-at: "2026-11-14T23:57:06.716000+01:00"
---

Problem: a FUNCTION: input typed idx, fd, len, bool, an ENUM, a sum or nullable<u8>, or an output typed idx, fd, len, bool, an ENUM or a sum, is rejected at the generated VALUE! or CALL ("habu: in f: at 'VALUE!' expected: n n actual: fd n", then "hook: non-certified definition"), not at its token: src/lib/ffi-abi.f IN-TOKEN files every type but ptr, f32 and r as K-VALUE with an n local, and OUT-TOKEN every other output as R-VALUE. A declaration states the C prototype, so these have no C meaning there; Zen, Tender and Maki declare none of them. RESULT-TAIL masks a u32 result to its width but not u8, u16 or char, whose high bits AAPCS64 and SysV leave unspecified. Acceptance: each such type is rejected at its token with an E-FFI-SYNTAX record naming the types FUNCTION: passes; a u8, u16 or char result reads only its width. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-ctypes.
