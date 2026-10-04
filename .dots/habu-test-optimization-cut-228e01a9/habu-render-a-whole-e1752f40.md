---
title: Render a whole effect row in diagnostic records
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T21:58:27.891741+03:00"
---

Review 538 of batch 4d (finding 3): src/core/render.f RBUF-CAP 64 (:360; RBUF+ :480-485, REND-SIG :519-528, DROW :537 on 568f2b04) cuts each rendered effect row to its top 64 values with no marker, while the checker admits up to 255 inputs (EFFECT-MIN-IN-MAX, 4d copy 2). Measured on the 4d g1 with `: W70 ( n x70 -- ) drop ;`: the E-MISMATCH record's declared_effect holds 64 of 70 `n`, inferred_effect 128 (64 in, 64 out); declared_effect_source is whole. Text only: CHECKER-ASIG-CAPTURE (checker.f:8960) drops the rendered text, so no stored or captured signature is narrowed, and an undeclared 70-input definition certifies. Acceptance: every admitted row renders whole in text and JSON records, with no silent cut. A gate row with the W70 fixture and one at the 255 bound asserts the full counts, seen failing first. A record past the 16 KB buffer follows c702474a. After 4d and c702474a. Files: src/core/render.f (baked), the diagnostics gate rows. Evidence: ~/.cache/tmp/kestrel-r4-rev538/fx/w70.f.
