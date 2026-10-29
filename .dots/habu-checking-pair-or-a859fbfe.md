---
title: Checking pair or sum locals beside a quotation crashes the engine
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T23:31:36.682655+02:00"
---

Problem: checking a definition that declares pair or sum locals (pair<a,b>, opt<a>) beside a quotation makes the engine itself exit 76 with 'lowering certificate replay drift', on the body lane's parent engine and its current engine alike, so it predates every open lane. Reproducers: /Users/joel/Work/habu/.jj-ws.noindex/tim-body/.out/rereview/a3.f and b_OPTDUP.f (run3.sh <stem> there runs one on all three engines); found by the body re-review while testing those locals for linear copies, which blocks that coverage. Acceptance: both reproducers check and run on a rebuilt engine, or the construct is refused by name with the rule in docs/forth.md 'Rules learned by refusal' and the card; a regression row in the compiler suite with pair and sum locals beside a quotation; the cause named at its layer (declaration, checker, lowering certificate or replay). Files: src/compiler (lowering certificate and replay), src/core/checker.f locals, test/compiler. Verify: the two reproducers; bin/hb --load test/run.f (shared compiler behaviour); two-generation convergence if codegen changes. Depends: none. Ownership: compiler. Claim: unassigned.
