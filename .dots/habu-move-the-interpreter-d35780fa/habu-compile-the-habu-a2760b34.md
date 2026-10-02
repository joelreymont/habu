---
title: Compile the Habu loop at tier 1
status: open
priority: 2
issue-type: task
created-at: "2026-10-02T16:41:18.856233+03:00"
---

Problem: the Habu loop loads only at tier 0. `1 set-tier require src/habu/interpret.f` dies `ncomp: cannot compile HOOK`; NCOMP refuses 11 words, the same list on intel 7554c01df with engine a95f: src/habu/outer.f HOOK, PUSH-STR, PUSH-CSTR, PUSH-ESC-STR, PUSH-ESC-CSTR, PUSH-CHAR, PUSH-XT; src/habu/definers.f DEF-PREFLIGHT and DEF-COMPILE (both at execute), DEF-TAKE; src/habu/interpret.f DISPATCH. Snapshots and executable builds refuse tier-0 code by design (APP-IMAGE:SAVE rc 100 'snap: retained code lacks native provenance'; hb-build rc 70 'hb: executable build requires native tier 1'), so no image holds the Habu loop, and habu-boot-the-arm64-d0d4421a needs it native. Acceptance: the Habu loop loads at tier 1; for each refused word the reduction names the layer that is wrong (NCOMP, the word's declared effect, or the language rule) and the fix lands there; outer-interpret agrees with the loop loaded at tier 1; and the item moved from I7c (habu-compile-defer-through-d02393a1): a snapshot (test/native-defer-image.f) and a stripped capture of a defer the Habu loop compiled relocate its cell and name its record as the cell's owner. Files: the layers the reductions name, src/habu/{outer,definers,interpret}.f, test/outer-interpret.f, test/native-defer-image.f, test/stripped-address-cases.f. Verify: spark rebuild gen2 == gen3, outer-interpret, native-defer-image, the stripped family. Ownership: krait (Intel lane).
