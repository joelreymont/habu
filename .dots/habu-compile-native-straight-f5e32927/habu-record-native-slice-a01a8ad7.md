---
title: Record native slice metrics
status: closed
priority: 1
issue-type: task
created-at: "\"2026-07-26T22:57:52.876000+02:00\""
closed-at: "2026-08-14T11:51:17.271555+02:00"
close-reason: "Closed SATISFIED (Wave-3 audit 2026-08-14): source maps span-validated, pass dumps shipped as tools, coverage measured stronger than asked (the judge board re-measures its own refusal column every run - 46 rows, 0 refused, 0 disagreeing), metrics pinned in committed baselines with byte-for-byte check, time deliberately out of gates (the leaf's own metrics-never-replace-correctness applied), stage digests canonical and domain-separated, regressions attributed (the loop pass exists because attribution named its class). The old-emitter baseline column dies at the cut by design (63b152cd); the chain self-baseline + clang column are the standing regime."
---

Full context: Wave 2 exits require source maps, pass dumps, coverage, and per-pass/per-artifact metrics against the pinned native baseline. Record time, size, stack traffic, spills, calls, and runtime for the exact straight-line corpus with stage digests. Acceptance: results are reproducible and attributable; regressions name the responsible pass; metrics never replace correctness. Dependency: native shadow comparison.
