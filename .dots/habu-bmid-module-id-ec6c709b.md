---
title: BMID module-id table aliased in the merged image
status: open
priority: 2
issue-type: task
created-at: "2026-08-16T07:38:53.013243+02:00"
---

The milestone's next layer, localized by bake-chain-13 (2026-08-16) with instrumented checks, control vs seeded: same code path, same mint sequence, same slot indices, but IR-BUILD:MODULE@ (build.f:770, LIVE-SLOT BMID @) answers 5 where control answers 1, and BMID slot 0 changes to 10 when the module at slot 1 is created - another table's writes land on BMID's cells (SLOT-MAX TYPED-BUFFER BMID, build.f:208). Milestone dies rc 67, uncaught -8361 E-A64SEL-SOURCE (was -8021 E-IR-OP-OWNER before a semantically-neutral SERIAL-CK rewrite moved it - LAYOUT-SENSITIVE, which is the diagnosis: a displaced base, same family as the does-branch and window-alignment defects, NOT a load-time effect - no installer writes module ids, TRAPPED-BELOW measures 6=6). Bake is reproducible: identical artifact+paths give byte-identical engines (a6710fc8 x2); different HB_TMP paths shift DP (the bake splices the artifact path into its generated driver) and move the symptom. Repro is a 15s capture+bake loop; instrumentation originals in the scratchpad (op.f.orig, select.f.orig, build.f.orig, id.f.orig). Suspect surface: a TYPED-BUFFER base cell among the captured/rebased DATA - find which table's writes land on BMID by watching the aliased cell with the residue/owner tools, then trace the writer's base derivation. Blocks the milestone and e98b03d4 items (3)-(6). The milestone suite case is two words, ready to land with this fix (PROBE-ARTIFACT leaves ART$; RUN-CASE on aot-chain-bake + stdin-capture on hb-chain).
