---
title: Declaration-time arity gate for variant payload letters
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-21T11:28:43.868275+02:00\""
---

Pre-existing gap re-confirmed by the SC-QUOT full-rows lane (2026-07-21): a bare parameter letter inside a SUM VARIANT payload (including quotation sides, e.g. the r in [ ... | r -- r ]) is arity-checked only at construct time via TFC-SCH-TERM, because sum-variant payload schemas are not validated through PF-NODE-KIND? at declaration - only product fields are. A declaration whose payload references an out-of-range parameter letter therefore parses and publishes, and the reject surfaces later at the first construct/MATCH site, far from the declaring line. Add the declaration-time validation pass for sum-variant payload schema roots (walk each variant's payload range with the same node-kind/arity discipline PF-NODE-KIND? applies to product fields, resolving letters against the declaring family's arity) so the reject lands at the declaration with the offending token. Red-first fixture: ENUM/SUMTYPE declaring option<z> payload or [ z -- z ] quotation side where z exceeds the family arity must reject at declaration; existing valid fixtures stay green. Files: src/core/sumtype.f declaration path + src/core/type-family.f validators + type-decl-suite.

Claim: agent=varitygate workspace=.jj-ws/habu-declaration-time-arity-4c70e37c (Mac; owns the sum-variant payload declaration-time validation pass: src/core/sumtype.f declaration path + type-family.f validators + type-decl-suite. structparse lane is consumer-only of these files.)
