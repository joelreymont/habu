---
title: Review and land x86 capture through one entry
status: closed
priority: 2
issue-type: task
created-at: "2026-10-22T13:12:42.309141+03:00"
closed-at: "2026-10-23T11:57:30.150623+03:00"
close-reason: "landed on intel (e2ba74dc 6dba1633 d8dca0f8), reviewed; ARM rows rc 0 at the merged tip; intel b63e453c; Mac ARM: two-gen ok gen 5 = gen 4, run.f 501/501 with one red (hookless-reject, master 569dc3bf not on intel)"
---

Problem: workspace .jj-ws.noindex/jerry-x86-capreach holds zlzpkqkt 'Share the x86 call-site codec' -> qrurzuns 'Reach x86 captures through recorded sites' -> zynrxtuy 973e2403 'Capture x86 shadows through one entry' (one TARGET-CAPTURE entry; test/aot-native-does.f now asserts private MAKER ships under its private WID per master SH-KEEP-ONE — test change to report to Joel). Done: pulsar two-gen rc 0 (author); pulsar staged build rc 0 — the seed needs an engine built from zlzpkqkt's tree first (ADDRESS-CARRIER:REL32-BYTES); native-prefix-declarations, trust-row-test, name-length-test rc 0. Open: aot-named-cells-suite, aot-native-merge/does/export-fixed, compiler-images on the staged engine; major Fable review (brief: one entry, recorded-site reach, shared codec, SH-KEEP-ONE incl. does> companion, ARM output unchanged, test-change check); handoff notes ~/.cache/tmp/jerry-c/capreach/. Acceptance: review ACCEPT, rows rc 0 on pulsar, land all three with jj rebase -s zlzpkqkt. Files: src/habu/aot-capture.f aot-shadow.f, tools/native-build-core.f, test/aot-*. Verify: the rows above on pulsar. Depends: none. Ownership: capture path. Recipes: ~/.cache/tmp/jerry-c/lane-recipe.md (Mac/pulsar build+run), ~/.cache/tmp/jerry-c/lane-queue.md. Rule: master (ARM) behaviour and tests win; fix x86, never rewrite a master test. Claim: unassigned.
