---
title: Land lane 11 as one commit
status: active
priority: 2
issue-type: task
created-at: "2026-10-31T00:39:57.601702+02:00"
---

Stack every accepted fix lane on zxpyvzqn (rebased onto master 1187c3c1) in carl-l11r (brief-l11-stack.md: GROW, MAP, R1, R4, SCRATCH, EXT, eval-head test, OWN, S, U1, CEN, auto and frame tests, merge-time edits), add FR1 (carl-tfr1) when accepted, then P0, the five native-file e2e tests, the full suite, the lead's docs/forth.md passage, squash into zxpyvzqn and push. Closes dot habu-reuse-a-file-706bf8ec with the cut ruling. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-l11r.

Ruling 2026-10-31 (OWN 3-5 against R4): R4 supersedes OWN 3-5's ORIGIN-SINK? checks and load-word move. Each harvest segment carries its sink and SPIKE-SEEK misses a row whose segment recorded another kind than the live one, so every install imports under the sink live at that point; a warm-kept object a fresh cold would refuse only for a sink switch inside a segment with rows is sound row by row. Invariant: warm product == fresh-cache product; cache contents may differ. OWN's three HISTORY-CASE scenarios stay, asserting what R4 does. Stack worker 3 continues from .out/l11/stack-handoff.md.
