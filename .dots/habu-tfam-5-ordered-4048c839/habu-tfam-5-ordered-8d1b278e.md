---
title: "TFAM 5: ordered event log store + core loader instrumentation"
status: closed
priority: 2
issue-type: task
created-at: "\"2026-07-04T08:53:29.659947+02:00\""
closed-at: "2026-07-04T09:58:18.890944+02:00"
---

Add ordered source-composition event storage to src/core/include.f (kind include/included/require/required/provided, exact path bytes, loader-token byte span from TKA-CELL $3690/TKL-CELL $3698, package/checker state delta, multiplicity). Instrument included(:180)/required(:186)/provided(:192) to append an event. include replays every occurrence; require/provided record exact-string registry state without collapsing spellings. Gate recording behind an enable flag so normal boot/gate does not record (avoid overflow/overhead). Add interpreter/current-token span accessor (byte span reachable via TKA/TKL; file line/col NOT reachable per census sec5 -> that is a separate span-capture capability). Rebuild bin/hb, prove native self-refresh/fixpoint, keep self-check cert count green. NOT dead code only when landed WITH a consumer (see discovery/replay dots).
