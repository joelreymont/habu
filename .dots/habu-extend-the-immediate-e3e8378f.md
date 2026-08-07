---
title: Extend the immediate fold to logic and compare
status: open
priority: 2
issue-type: task
created-at: "2026-08-07T13:36:13.421214+02:00"
---

Audit: ~96 gap bytes across 10 rows are constants built in registers where the instruction takes an immediate — and/orr/eor bitmask immediates (TAG: mov #7;and vs and #7), cmp/cmn immediates (SYM-FOLD-C, WS?, LADDER guards), movn for -1 (BYTE-FIND wastes 12 bytes building it with movz+3 movk), lsl for *2 (CALL-FAN). The add/sub immediate fold landed in the combine pass (2605426e) — extend it to the logical/compare families the encoders already ship (verify each ENC-* exists; bitmask-immediate encoding is its own validity check — refuse unencodable masks, never approximate). Also LERP's latent selection waste: a runtime zero-divisor check (mov #100;cbnz;brk = 12 bytes) on a CONSTANT nonzero divisor — fold the check when the divisor is a known nonzero constant. Measure-first per row, answers bit-for-bit, boundary fixtures straddle, deliberate re-pin.
