---
title: Model smulh and madd before magic division
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T17:02:22.178674+02:00"
---

Magic-multiply constant division needs smulh (and madd, which the combining dot also wants) — neither is in formal/Common/Insn.v. Per the CG-02 per-lane discipline: enc/wf/decoder/roundtrip rows, mutation-falsified, BEFORE any emitter uses the forms. Prerequisite of the magic-division half of habu-fold-constants-and-cbe4e25e (which itself waits on a division corpus row with byte headroom — LERP's gap is zero, so today the transform would trade bytes for ns only: 0.449ns available on LERP) and feeds habu-combine-instructions-the-870b23d4 (madd/msub).
