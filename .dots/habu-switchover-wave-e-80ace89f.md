---
title: "Switchover wave E: self-hosting resolvers + trust discharges"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T22:18:57.015932+02:00"
---

docs/census-switchover.md sections 1a/1c/4+5 wave E, LAST and bootstrap-sensitive. (1) type-family.f find/resolve family (TFAM-FIND-IN :248, TFAM-RESOLVE :280, SUMV-FIND :370, PF-FIND :432, LAY-FIND :489, TFAM-QUAL/SIG-RESOLVE :696/:704) from '-- id true | false' to option<id>/result<id,ambig>; TFAM-SIG-RESOLVE catch-on-E-TFAM-AMBIG throw-as-signal removed. Fixpoint-sensitive: the registry migrating onto itself — prove each step through the full fixpoint + bootstrap check. (2) Persisted checker tags (T-*/VR-*/SC-*/TK-*/TL-*) migrate ONLY if AOT-image encoding change proven fixpoint-safe; else record as explicit self-hosting boundary with rationale. (3) Trust-row discharges, one dot each when reached: BP-NULL, TASK-NULL, c-defer-find-unset/c-defer-cell, NULL$/ENV-FALSE (census section 4 list) — the campaign itself adds zero trust rows. DEPENDS: waves A-D landed and stable.
