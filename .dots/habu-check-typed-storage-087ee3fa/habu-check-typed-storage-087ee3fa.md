---
title: Check typed storage stores at the top level
status: active
priority: 3
issue-type: task
created-at: "2026-09-23T11:30:21.005778+03:00"
---

Joel chose the same storage type rules at top level as inside definitions. The checker-owned interpret row refuses pointer and execution-token stores into raw or numeric cells, preserves typed pointees through calls and nested evaluation, and keeps the explicit unchecked window. Capture retains its stored-value checks because unchecked stores and builder-only pointers remain possible. Focused tests passed on the feature product; independent review, the combined native suite and convergence remain pending. State 2026-10-21: the feature is not on master (feature commit e70ec527 "Check top-level storage types" is not a master ancestor; src/core/top-row.f is unchanged and checker.f has none of TOP-ROW-SELECT, TOP-FRAME-SAVE, E-WENSURE). Its latest form is on bookmarks typed-top/jerry-toprow, typed-top/jerry-catch and typed-top/jerry-mig (WIP) with head aa435b27 "Restore public top-level checking" and integration commits ba983394/4a533229. Carl's combined integration no longer exists; the five closed children's adaptations are on master. State 2026-10-26: jerry judged the typed-top bookmarks dead (5+ days stale, not on master); the feature is re-applied in a fresh lane from master, not rebased: workspace carl-ttop, plan by Codex Astra (Fable at its limit) in ~/.cache/tmp/carl-orttop.Ts1E/out.txt, then a rule lane, a suite census lane and migration lanes. Joel (2026-10-26) ruled that the incremental-build stack waits for this and habu-scan-only-unchecked-91788cad, so ffs.f stays red until both land. Claim: carl.
