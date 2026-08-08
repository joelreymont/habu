---
title: Teach the combine inventory the D-file pairs
status: open
priority: 2
issue-type: task
created-at: "2026-08-08T23:28:38.301816+02:00"
---

tools/codegen-combine-inventory.f's LDR?/STR? classifiers know only the general-file encodings, so its LDP/STP pairing counts understate float rows now that the chain emits ldr/str/ldur/stur of D registers (a64.fldr family, landed with habu-keep-floats-in-9f0fe969): ldp d0,d1,[x] is a real pairing opportunity the inventory cannot see. Nothing pins those numbers so no gate is red, but planning off the inventory undercounts float rows. Proper fix needs the pair rule extended, not just the classifiers: LDP-PAIR? must refuse MIXED-FILE pairs (an X access beside a D access at adjacent offsets is not pairable), with fixtures for the refusal and for a real D-file pair. Files: tools/codegen-combine-inventory.f, test/compiler/codegen-combine-inventory.f. Verify: bin/hb --load test/compiler/codegen-combine-inventory.f. Depends: none.
