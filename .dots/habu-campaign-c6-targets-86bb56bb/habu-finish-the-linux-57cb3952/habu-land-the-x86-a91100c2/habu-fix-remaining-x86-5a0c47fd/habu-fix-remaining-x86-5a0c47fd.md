---
title: Fix remaining x86 rows outside the open lanes
status: open
priority: 2
issue-type: task
created-at: "2026-10-22T13:12:42.407093+03:00"
---

Problem: x86 reds not owned by another child: test/outer-interpret.f stops at row 'numbers' (ncomp: cannot compile OI-UF, -8304); runtime-regression GE-DOES-WIDE (does>-split wide facts rc 0, expected 75); hb-build-stripped asserts 28-30 (HBT-STRIPPED-PTR-MARK 'ptrmark: stack') and ffi-callback-file-cache; stripped-image 393216 cap; queue rows after capture lands: minin (sumtype-unchecked 9,10, checker.f MULTI-ERR MIN-IN byte), ownerxt (structure-opaque-e2e 3,5-7 OWNER-XT), checker-surface 7, internal-word-gate 56,530,581,660-668,709,717,783,845, snapshot-writer, hb-build-repl-twin, hb-build-fixtures 108, aot-wid-refuse-address-span, aot-wid-capture-refusal, check-verify F930/F2575-2578, native-create-does F5/F6, build-fixpoint-source, dictionary-capacity (x86 app image 64295 < 65536 premise), gate-pool F40. Split into one lane per root cause after the next full pulsar probe. Depends: habu-review-and-land-2437d9b0. Ownership: per lane. Recipes: ~/.cache/tmp/jerry-c/lane-recipe.md (Mac/pulsar build+run), ~/.cache/tmp/jerry-c/lane-queue.md. Rule: master (ARM) behaviour and tests win; fix x86, never rewrite a master test. Claim: unassigned.

Found under C4a on pulsar, the same on master's x86 engine (/tmp/jerry-x86base/bin/hb): compiler-native-create-does stops with E-UNDEFINED 'UNPATCHED' at line 72; one definition `create 99 , if exit then does>` reproduces it at tier 0 and tier 1 (x86 drops the created word when `exit` precedes `does>`; master's engine segfaults on the whole row, rc 139). outer-interpret's oi-package-part case exits 83 with no output, through both the engine loop and the Habu loop: opening any part does it, `1 . package OI-A private 2 . package OI-B 3 .` prints `1 2` and exits 83 (master's x86 engine and the C4 stack alike; /tmp/jerry-c4rv-tmp/jerry-pp6.f on pulsar). Master's outer-interpret stops earlier, at `numbers`, so only the C4 stack reaches this case.

hb-build-aot on x86 (master efef8267's x86 engine and the t0x86 stack alike): the linker image build fails, `E-UNDEFINED habu: in map-rows: undefined word 'CODE-END'` at tools/aot-code-map.f:186 (`end CODE-END @ end - MAP-RUNTIME`): the x86 engine does not define CODE-END. Log: /tmp/jerry-big/x86base-suite.log ~42.
