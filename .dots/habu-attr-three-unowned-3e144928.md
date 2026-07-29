---
title: Attribute three unowned gate-stdlib reds
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-29T20:36:18.188769+02:00\""
---

Full context: measured 2026-07-29 on the proofs base, before and after an unrelated engine change, so none is owned by the snapshot incident: pre-trust-defer asserts exit 73 and gets 70 (matches the Gforth-mirror replay defect described in docs/debugging.md); aot-wid-restore asserts 0 and gets 67 three times; stdlib-process-fixtures fails a boolean assertion with no exit-code evidence captured. (A fourth, engine-error-package expecting 70 getting 67, is already owned by habu-restore-fail-closed-4f1d6375.) These were hiding behind the snapshot reds in the pool ordering. Root-cause and attribute each separately to an owner or a new dot; do not batch-fix.

Claim: agent=attrthree workspace=.jj-ws/habu-attr-three-unowned-3e144928
