---
title: Fix cad-test child-replay timeout flake
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T08:21:55+02:00"
---

maki/cad-test.f's replay case spawns a child engine (RPL-CHILD-TILE$ at maki/cad-test.f:84) with a fixed 30-second timeout (30000 >MS at :88, RUN-ARGV-CAPTURE). Under machine saturation - e.g. while a fixpoint rebuild or several gate batteries run concurrently - the child occasionally exceeds the budget and the suite fails with a timeout even though the replay output is correct when run alone. Observed during the 2026-07-19 merge trains. Root-cause properly per the RCA rule (do NOT just raise the number): measure what the child actually spends its time on under load (load phase vs replay phase), decide whether the child can reuse prepared state instead of reloading the full stack, and pick a budget derived from a measured cold run with a stated multiplier, as the ptx toolchain tests do for their probes. Also make the failure diagnostic name the phase that timed out so a future flake is attributable from the log alone. Files: maki/cad-test.f RPL-CHILD-TILE$ and its capture words. Verify: maki/test.f green under a deliberately loaded machine (run alongside a fixpoint build).
