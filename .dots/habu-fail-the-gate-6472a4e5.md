---
title: Fail the gate when docs/engine-size.md drifts from the engine
status: active
priority: 3
issue-type: task
created-at: "2026-09-17T17:20:32.571466+03:00"
---

Problem: docs/engine-size.md hard-codes four tables (budget, DATA owners, shipped dictionary, engine-entry packages) copied from tools/engine-size.f output, and nothing fails when a landing that touches capture changes the engine; the document was wrong by 1.4 MB (5,374,144 vs 3,932,352 bytes) and 23% of its figures for four landings before c1483491 re-measured it by hand. Acceptance: a checked Habu test (test/ or tools/) runs tools/engine-size.f on the tree's bin/hb and diffs the tool's tables against the tables in docs/engine-size.md, failing by name on the first differing row (class, owner, dictionary class or package) with both values; the prose stays a human's job. It runs in the full gate (test/run.f) and is skipped by name when bin/hb is not a baked engine image (the tool already refuses snapshots and stripped applications). Files: tools/engine-size.f (a table-emitting entry if the text layout is not parseable as is), test/ (new suite), test/run.f, docs/engine-size.md (only if a table needs a stable marker). Verify: the suite green on d5e871c0; edit one figure in the document and see the suite name the row. Depends: none. Ownership: tools and docs. Claim: unassigned.
