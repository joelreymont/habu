---
title: Isolate the outermost check-run replay scope
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T10:32:14.774884+02:00"
---

Full context: CHK-RUN-SCOPED in tools/check-core.f uses CHECKER-SCOPE-START-NEUTRAL, but no black-box regression can distinguish it from its inner scopes today — the neutral declaration propagates into nested CHECKER-SCOPE-START frames, so CHK-RUN-NOMINAL-LINTS and CHK-RUN-PREVERIFY mask it. Measured: with only CHK-RUN-SCOPED inheriting, both existing regressions stay green. Either find a replay path inside CHK-RUN-CURRENT that lies outside both inner scopes and give it an owned regression, or decide the outer scope is the sole authority and demote the inner two back to CHECKER-SCOPE-START with a comment explaining why. Do not leave the ambiguity: today a future edit could remove any one of the three and no test would notice.
