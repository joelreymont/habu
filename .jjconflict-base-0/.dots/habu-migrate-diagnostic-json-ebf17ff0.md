---
title: Cut diagnostics to explicit JSON
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T16:07:53.567835+02:00"
blocks:
  - habu-add-explicit-json-5d7ee868
---

Why: DIAG:RENDER-JSON uses the singleton and returns a span valid only until the next render. Result: RENDER-JSON consumes and returns a caller-supplied JSON-WRITE:writer through the final explicit emitters; every private JSON helper threads that writer while the diagnostic remains immutable; the caller uses JSON-WRITE:COPY and closes once. Human RENDER is unchanged. Delete every DIAG-owned JSON buffer, raw JSON return span, and singleton call. Owner and touch points: maki/db/diagnostic-render.f and maki/db/diagnostic-test.f only. Production red: two diagnostic renders alias the same ambient output. Acceptance: canonical JSON stays byte-identical; two diagnostics render interleaved; required copy preserves writer and destination; the diagnostic-render owning suite passes while unrelated singleton consumers remain unchanged on the feature branch. Forbidden: human renderer change, second serializer, global buffer, adapter, compatibility, version, metric, or lint. Smallest owning check: bin/hb --load maki/db/diagnostic-test.f. Claim: unassigned.
