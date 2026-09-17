---
title: Migrate tree to the converted type surface
status: closed
priority: 1
issue-type: task
created-at: "2026-07-30T16:16:01.775217+02:00"
closed-at: "2026-09-16T14:34:48.694832+03:00"
close-reason: "superseded by habu-campaign-c3-the-a2477c89: The tree-wide migration sweep is unstarted: arity NEWTYPE and legacy declarations are still everywhere"
---

Claude's half. After the engine half lands and one fixpoint refresh: migrate every SUMTYPE/PRODUCT declaration to STRUCTURE/ENUM; every DEFTYPE and arity-NEWTYPE to carrier form or CONSTRUCT owner structures (census 2026-07-30: 118 arity-0 NEWTYPEs, 44 parameterized, all DEFTYPEs); evaporate proof tokens (cfg-proof, layer-proof, mints, TRUSTED rows); respell all generated-namespace references to the nested form; migrate tests and docs. No gates during the sweep; the campaign's single gate battery at the end.
