---
title: "audit: gaps pass over the 2026-07-12/13 campaign (plan-vs-reality)"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-13T09:22:21.176397+02:00\""
---

User-requested gaps audit (per ~/.claude/skills/gaps): 4 parallel read-only auditors in ONE exhaustive round over the unified tree at master 3329ca69+: (1) plan-vs-code - every acceptance criterion in MODEL-CAD-V2-PLAN R1-R3 sections marked landed, the merge-policy dot, and all 10 closed campaign dots (in-body, diag-renderer, tk-cell, typed-storage, foo2, region x2 v2-r3, raw-audit, typed-launch, concat) traced to live code; (2) flow-tracer - one end-to-end MODEL: capture -> plan -> sched-key -> durable store flow and one eval transcript replay, verifying claimed enforcement fires on the real path (not just suites); (3) dead-code auditor - every word the campaign added, zero-caller check (LBUF converters, FP-REGION-ID consumers, MV-PACK-* helpers, min-in plumbing, DIM>ROWS etc.); (4) acceptance-live - re-run each closed dot's acceptance command live on the shipped engine (bare U-TYPE, FOO2, label collision, ST2 launders, P1/P2, opener rejects, TDLR pins). Output: per-gap Gap/EVIDENCE/FIX lines -> new dots for real gaps; then close this dot with the audit record. Read-only pass; fixes go through normal dot->worker->review->window flow.
