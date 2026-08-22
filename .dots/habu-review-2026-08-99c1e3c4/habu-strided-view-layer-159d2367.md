---
title: strided-view layer is test-only and unchecked at its seam
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:38:25.998647+02:00"
---

Problem: maki/tensor-value.f:469-479 TV-AT@/TV-AT! do no i/j bounds check (the :209-210 comment covers constructed views, not caller indices); non-test consumers: TV-AT@ 0, TV-AT! 0, TV-WINDOW 0, TV-HEAD-SPLIT 0, TV-VIEW 0, TV-TRANSPOSE-VIEW 0, TV-MATERIALIZE 0, TV-VIEW-ADJOINT+ 0, TV-STRIDED? 0, PLINEAR 0, PGELU 0, TV-LINEAR 0, TV-NEW 0 (live: TV-DESC 12, PLAN-OP-BEGIN 17, TV-NEW-HOST 1); ~250 lines (164-219, 413-543, 684-718) plus docs/strided-views.md and two tests serve nothing. Six tensor representations coexist (raw ptr arrays, the TENSOR handle, evaluated EXTENT/ITENSOR/EXTPROD accessors, model-IR nodes, GPT-2 byte spans, PTX phantom matrix types) with no converter but TV-LINEAR (0 consumers). Acceptance: a ruling which pair survives (raw+model-IR are live), the view half deleted with its tests and doc, or a bounds check + negative test if kept. Files: maki/tensor-value.f, maki/view-test.f, docs/strided-views.md. Verify: maki/test.f. Depends: none. Ownership: maki tensors. Claim: unassigned.
