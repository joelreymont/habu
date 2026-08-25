---
title: CHECK! records go stale across dictionary rollback
status: open
priority: 2
issue-type: task
created-at: "2026-07-07T15:37:35.061030+02:00"
---

Engine/checker-registry bug found while building tools/codegen-role.f. Reproducer (bin/hb --load, prop-test CHK-MARK rollback pattern): region 1: MARK (cp@/ndict@/UEND save), evaluate 'variable HXX', CHECK! 'GE1 ( -- ) 0 HXX !' -> -1, 0-set-check evaluate ': GE1 ( -- ) 0 HXX ! ;', evaluate 'GE1', FORGET (ndict!/cp!/UEND restore + UTERM!). Region 2: MARK, evaluate 'variable HXX' again, CHECK! 'GE2 ( -- ) 1 HXX !' -> 1 (rejected; expected -1). A CHECK!-certified definition that references a region variable leaves a registry record that survives ndict rollback; after the same variable name is re-defined, later CHECK! calls resolve the stale record and reject well-typed bodies. Likely the insert-once hash-index/xref registry vs ndict rollback (TFAM registry-rollback territory; see 'TFAM 3: reentrant registry rollback frames'). Regression: the two-region fixture asserting -1 in region 2. tools/codegen-role.f avoids rollback entirely via generation-suffixed names.

OWNERSHIP 2026-07-07 (fable orchestrator): feasibility-gated and LEFT FOR THE
TFAM CAMPAIGN - the dot self-identifies as registry-rollback territory (the
insert-once hash-index/xref registry vs ndict rollback), which is exactly what
TFAM 3 (reentrant registry rollback frames) owns; a fable-side fix would
collide with that campaign's in-flight registry work. The two-region fixture
above is the ready-made regression when they pick it up.
