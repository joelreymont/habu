---
title: Measure inline cost at the call site
status: open
priority: 2
issue-type: task
created-at: "2026-08-03T20:53:12.410827+02:00"
---

Destruction review of NINL, medium. SMALL? (inline.f:238-240) asks A64EMIT:INSNS — the callee's emission under the callee's own register pressure. The splice replays those tokens into a caller holding its own live values, where spill.f may charge spills the measured emission never paid. The rule proves 'this routine's body is short', not 'this body costs no more at this site than the call did'. Decide the honest bound (e.g. re-measure at the site, or bound tokens+max-pressure) and make the derivation in the file header true.
