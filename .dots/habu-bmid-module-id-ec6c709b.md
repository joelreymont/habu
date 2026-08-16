---
title: BMID module-id table aliased in the merged image
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-16T07:38:53.013243+02:00\""
---

The milestone's next layer, localized by bake-chain-13 (2026-08-16) with instrumented checks, control vs seeded: same code path, same mint sequence, same slot indices, but IR-BUILD:MODULE@ (build.f:770, LIVE-SLOT BMID @) answers 5 where control answers 1, and BMID slot 0 changes to 10 when the module at slot 1 is created - another table's writes land on BMID's cells (SLOT-MAX TYPED-BUFFER BMID, build.f:208). Milestone dies rc 67, uncaught -8361 E-A64SEL-SOURCE (was -8021 E-IR-OP-OWNER before a semantically-neutral SERIAL-CK rewrite moved it - LAYOUT-SENSITIVE, which is the diagnosis: a displaced base, same family as the does-branch and window-alignment defects, NOT a load-time effect - no installer writes module ids, TRAPPED-BELOW measures 6=6). Bake is reproducible: identical artifact+paths give byte-identical engines (a6710fc8 x2); different HB_TMP paths shift DP (the bake splices the artifact path into its generated driver) and move the symptom. Repro is a 15s capture+bake loop; instrumentation originals in the scratchpad (op.f.orig, select.f.orig, build.f.orig, id.f.orig). Suspect surface: a TYPED-BUFFER base cell among the captured/rebased DATA - find which table's writes land on BMID by watching the aliased cell with the residue/owner tools, then trace the writer's base derivation. Blocks the milestone and e98b03d4 items (3)-(6). The milestone suite case is two words, ready to land with this fix (PROBE-ARTIFACT leaves ART$; RUN-CASE on aot-chain-bake + stdin-capture on hb-chain).

Claim: agent=bake-chain-14 workspace=.jj-ws/habu-bake-chain

FOUND AND FIXED 2026-08-16 (bake-chain-14). ROOT CAUSE: a fourth
relocation class, and the format needs no new one. Every generated
storage accessor reached its storage as `data-base <baked offset> +`
(src/core/layout-buffer.f LBUF-SOURCE / TYPED-VAR-SOURCE /
LDEFER-SOURCE). That offset is a SCALAR - habu2.f emits scalars through
a different path from addresses ON PURPOSE, so no relocation pass can
see it - and it is DP-derived, so it only means anything while the
window's DATA sits at the offset from `data-base` it was compiled
against. A snapshot moves the whole region and keeps that true; the AOT
seed copies the window to the booting engine's DP and the merge places
it after the host's window, and both break it. MEASURED in the merged
engine with two words compiled into the window beside BMID
(`here data-base - constant ZOFF0` and `create ZMARK`): ZMARK's
relocated address is 152 bytes BELOW what the baked offset says
(source-loaded control: 0), and `0 BMID` computes ZMARK+152. BKEY and
BMID are 16 cells each, so BKEY slot i writes BMID slot i+3 - the
reported aliasing, arithmetic included. FIX: the accessors address
their storage through a `create`d word (`<NAME>#base`), the one
relocatable DATA-address form, so they ride the DSITE class that
already exists; the deferred column's control cells travel the same way
and its stored region offset is now measured from those cells instead
of from `data-base`. One checker clause went with it: RAW-BLOCK? now
exempts the armed LAYOUT-INTRO window exactly as its sibling
NOMPTR-BLOCK? already did - without that, a `create`d (RAW) pointer
cannot enter a nominal family and the definer had no shape left but the
broken one. MILESTONE: capture + bake + `echo 's" : FOO ( n -- n ) 1 +
;" 1 1 8 NMIGRATE:DEFINE 7 FOO .' | hb-chain` prints 8, exit 0. Landed
as PROBE-BAKED in test/aot-chain-capture-suite.f (bake with the
production tool, run the two-word program, exact stdout); reverting
LBUF-SOURCE to the baked offset reds that case by name (rc 67).
