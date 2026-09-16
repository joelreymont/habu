---
title: Prevent ordinary JIT loads from exhausting code span tracking
status: closed
priority: 1
issue-type: task
created-at: "\"2026-09-11T17:23:20.225919+03:00\""
closed-at: "2026-09-16T14:34:47.951000+03:00"
close-reason: "superseded by habu-campaign-c1-finish-1f129a00: The 512-row bound is now 8192, so the reported tier0 exit-96 load failure is gone; residue is that span coalescing was never diagnosed, only out-sized."
---

Owner: Rowan tier lane, rowan-jit-nest; reported on frozen f115e7885d20 in BB20260911-141821.342-rowan-93e9. Ordinary tier0 loading of src/habu/driver-io.f, aot-arm.f, aot-capture.f and src/compiler/native/string.f reaches the512-row TIER-PROV bound and exits96. This is separate from the intentional513-alternating-span capacity test. Diagnose whether pass2 rewind, inter-definition rewind or interleaved code emission prevents legitimate coalescing; preserve exact retained JIT detection, partial-overwrite suffixes and failed-emission handling. Fix bookkeeping before considering a bound change. Acceptance: this real load succeeds at tier0, span counts explained, adversarial exact-range and save-guard tests stay green.
