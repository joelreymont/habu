---
title: Give FFI definitions their own descriptions
status: open
priority: 2
issue-type: task
created-at: "2026-10-26T12:20:27.653394+02:00"
---

Problem: the FFI library/function tables grow inside DATA at load time (src/lib/ffi-abi.f ~325, ~654, ~963); a file whose LIBRARY line grows the table's capacity refuses ffi-grown (tools/native-unit-object.f FFI-SEG-CK ~930) and is never reused; callers bake numeric slots (ffi-abi.f ~1740-1767). Joel 2026-10-26: same principle as CWORK, own change; lands with the stack if the second engine build shows any engine file refused ffi-grown, otherwise right after. Design: LIBRARY/FUNCTION definitions own path, symbol, ABI and a reference to their library; handles and addresses resolved lazily into transient storage; delete table growth, numeric-ID replay and ffi-grown. Needs a design pass and a major review. Depends: the PENDING lane's engine measurement. Claim: unassigned.
