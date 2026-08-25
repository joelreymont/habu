---
title: Model arity-n type families in Rocq
status: open
priority: 1
issue-type: task
created-at: "2026-07-28T13:04:18.518091+02:00"
---

Full context: formal/Common/Effects.v models arity-0 type families only. That single limitation blocks three separate pieces of checker behaviour from being modelled, all of which are currently abstracted rather than proved. (a) The MATCH scrutinee: the checker walks a width-expanded layout bundle cell by cell (MATCH-SCRUT? at src/core/checker.f:8236) while the model requires one TFam cell. Everything above that pop - payload refinement, the arm join, MD-JOIN, exhaustiveness - is already exact in formal/Common/Control.v; only the scrutinee pop is abstracted. (b) uniform<bool> branches and the block-collective barrier rule (COND-UNIFORM? at checker.f:7778, ALL-CF-UNIFORM? at :7681) need arity-1 families. (c) construct (CONM), field projection and the transport operations are row surgery over the same missing layout machinery. Required result: Effects.v models type families of arity greater than zero, including layout width expansion and hidden fields, so a bundle-valued term can be represented. Acceptance: Rocq 9.2 builds via formal/Makefile; definitional examples cover a multi-cell layout value, its width expansion, and a hidden field; every existing example in Effects.v and Control.v still holds; no theorem beyond definitional examples and no Admitted. Then the three abstracted areas above become their own leaves. Build from src/core/checker.f and measure every rule against bin/hb, following the method used for Effects.v and Control.v; documents are known unreliable here.
