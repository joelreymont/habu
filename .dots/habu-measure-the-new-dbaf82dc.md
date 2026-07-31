---
title: "Measure the new chain's cost without the FFI trampoline"
status: open
priority: 2
issue-type: task
created-at: "2026-07-31T18:58:36.244850+02:00"
---

The head-to-head table in tools/codegen-compare.f now has a new-chain cost column, and it cannot yet decide the nanosecond half of the goal. Measured on a 12-core Apple Silicon host: an emitted empty routine costs 253-257 ns per call while the old emitter's empty word costs 1.8-2.0 ns. The difference is not the emitted code - it is one instruction - but the only way the harness can enter the routine: NRUN:EXEC0..EXEC3 do FFI:RESET (a loop over every argument slot) plus argument stores plus ffi-call-bounded's bounds checks. The cost column is therefore a ratio to an empty call of the same kind, which is honest but blunt: ADD3 reads 1197-1212 and SQUARE-SUM 1130-1154 against the old 1900-2061 and 3567-3761, and most of both new numbers is the trampoline and the per-argument marshalling rather than the routine. A real time comparison needs the emitted routine to be callable the way a Habu word is, which is the calling-convention binding (dot habu-bind-arm64-arg-f76afa3a). When that lands, replace the new column's call path with it, drop the caveat CODEGEN-REPORT:CAVEAT prints, and re-run. Until then the bytes and the results are the decisive columns, and they are exact.
