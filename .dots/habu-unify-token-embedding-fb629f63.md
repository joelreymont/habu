---
title: Unify token embedding on bounded path
status: open
priority: 1
issue-type: task
created-at: "2026-07-28T19:56:11.804764+02:00"
---

Why: the integer-id EMBED-ROWS gather validates every token id before any read, but the float-id path it was meant to converge with is still unbounded and is the one production calls: MAKI:EMB-GATHER accepts id 99 against a 3-row table and reads 99 rows past the buffer (proven by probe), and TOKPOS-EMBED - the training/forward composition - routes through it. The EMBED-ROWS contract called this recorded debt but no dot existed; this is that dot. Exact result: every embedding lookup path validates ids against the vocabulary row count before any read with the existing named code MAKI:E-TOKEN-RANGE, by routing EMB-GATHER's id resolution through the same bounded pre-pass EMBED-ROWS uses (all-or-nothing: a rejected batch writes nothing); TOKPOS-EMBED behavior for valid ids is byte-identical; the float-id surface either converges on the integer path or carries the same bound - no second validation formula. Owner: package MAKI in maki/embedding.f. Dependencies: the EMBED-ROWS commit must land first. Acceptance: the id-99-against-3-rows probe rejects on every public lookup path with E-TOKEN-RANGE and untouched destination; embedding, autograd, pos-embed, gradcheck suites green; mutation runs prove per-path kills; both diff lints. Forbidden: clamping, dual validation formulas, behavior change for valid ids, loader types.
