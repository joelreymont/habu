---
title: Load test/wasm/dynamic.f again
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T23:41:58.145970+02:00"
---

test/wasm/dynamic.f, and so test/wasm/device.f, fails with uncaught -8651 E-NELAB-QUOT on master's engine (carl-scr3/bin/hb), on the THROW0 lane engine and in carl-scr3 alike, so device.f's rows (throw, depth, ...) cannot run as committed. The THROW0 review proved device.f only through a scratch copy that requires src/arch/wasm/backend.f + WBACK:INSTALL instead. Reduce the failure, fix the responsible layer, and make device.f run in its gate row. Source: THROW0 review 2026-10-28.
