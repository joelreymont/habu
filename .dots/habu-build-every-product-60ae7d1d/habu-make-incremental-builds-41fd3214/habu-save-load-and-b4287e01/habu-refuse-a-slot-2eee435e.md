---
title: "Refuse a slot or call row on another row's site"
status: active
priority: 2
issue-type: task
created-at: "2026-11-07T05:54:23.155824+02:00"
---

Problem (review of 99e0bfd7, by reading, not reproduced): src/habu/linker.f EMISSION-CODE+ zeroes both a call row's site and the does> slot's site (:607-608), so SLOT-CK's zero test and CALL-CK's cannot tell them apart. An object whose slot P equals a call row's site (tag 5), or with two call rows on one site, passes FRESH?; NEMIT-FILL places the BL there (:1795) and then writes RET over it (:1818), and NEMIT does not check overlapping rows (src/compiler/native/emission.f:167-185), so the file links wrong with no error. Only a faulty object writer makes such an object: the object's own SHA-256 is checked first. Acceptance: EMISSION-CK refuses a slot P equal to any call row's site of the same emission and two call rows on one site; two crafts in test/fixtures/file-objects/build.f CRAFTS (each compiles, 1, on the crafted line). Files: src/habu/linker.f, test/fixtures/file-objects/build.f, test/file-objects.f. Verify: bin/hb --load test/run.f -- file-objects. Depends: none. Ownership: linker.f validation. Brief: .jj-ws.noindex/jerry-wasm/.out/briefs/link-crafts.md. Claim: agent=jerry workspace=.jj-ws.noindex/link-crafts.
