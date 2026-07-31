---
title: Migrate tree to the converted type surface
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T16:16:01.775217+02:00"
---

Claude's migration half. After the source-only engine leaves land, migrate every SUMTYPE/PRODUCT declaration to STRUCTURE/ENUM; every DEFTYPE and arity-NEWTYPE to carrier form or CONSTRUCT owner structures (census 2026-07-30: 118 arity-0 NEWTYPEs, 44 parameterized, all DEFTYPEs); evaporate proof tokens (cfg-proof, layer-proof, mints, TRUSTED rows); respell every generated-namespace caller to its exact nested path; and migrate tests and docs. Each existing M1-M16 leaf owns one declaration plus its complete caller closure. No engine refresh or runtime gate occurs during the sweep. M17 performs the only refresh, the whole-tree retired-spelling census, and the single terminal gate battery.
