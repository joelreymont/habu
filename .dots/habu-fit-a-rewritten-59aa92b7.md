---
title: "Fit a rewritten module's arenas in scratch"
status: open
priority: 2
issue-type: task
created-at: "2026-08-06T14:34:37.008525+02:00"
---

Wider spilling bodies (12 and 20 live values) hit E-IR-CTX-SCRATCH -6644 after A64SPILL:REWRITE — the migration context's mapping does not fit a rewritten module's arenas. Blocks relying on spills at scale; the cut's stdlib bodies may exceed it. Measure the scratch demand of a rewritten module vs the context's budget, then either size the budget from the module or make the refusal name the shortfall. Found by the spillwire lane 2026-08-06.
