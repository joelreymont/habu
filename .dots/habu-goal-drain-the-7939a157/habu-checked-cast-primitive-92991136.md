---
title: Checked cast primitive to end per-declaration trust
status: open
priority: 1
issue-type: task
created-at: "2026-07-18T23:21:11.265434+02:00"
---

Root-cause fix for TRUSTED growing linearly with declarations: every EXTENT: mints a TRUSTED injector (>#M), every NOMINAL: mints two trusted identity casts, because the checker cannot express a retype into a nominal family. Design target: one checker-known cast form so declaration-generated converters are CHECKED, not trusted. Sketch: a core declarer CAST: <name> ( n -- <family-tail> ) [guard-body] ; where the checker itself certifies the body under the rule 'input cell flows to output unchanged; the only allowed effect is a guard that throws' - i.e. the checker types the identity data-flow and the guard, and the retype is sanctioned by the declaration form rather than by trust. Extent injectors then declare their range guard in the open (n 0 < over #M >= or if E-EXT-RANGE throw then) and stop being trust rows; NOMINAL: converters become guardless checked casts; the roles.f built-in pairs (>IDX/IDX>N etc.) migrate too. This retires the largest class of TRUSTED rows and stops the per-declaration growth. Phase-0 priority: design the checker rule first (what exactly the certifier must prove: single-cell identity flow, no other writes, guard throws only), write the negative suite (a cast that mutates, drops, or swaps must be rejected), then implement. Depends on nothing in flight; touches checker.f, so serialize with any other checker.f lane.
