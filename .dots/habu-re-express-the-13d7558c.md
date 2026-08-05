---
title: Re-express the deleted memory-ordering pins
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T12:28:12.451258+02:00"
---

The scaffold-del lane deleted two text pins whose invariants now have NO check: MMAP-DIAG (fail-closed mmap-failure diagnostics — an unforceable failure path) and PROT-PUBLISH (LDAR/STLR acquire/release ordering on publish). Both are real invariants of the emitted engine; text pinning was the wrong check, absence of any check is worse. Re-express structurally: a LINT-LEX token-kind scan over the emitted source (the shape the lane used for the kept arena-parity check), or an executed probe where reachable. The bulk-window publisher dot habu-publish-native-code-886e3ef9 owns the publish ordering going forward — coordinate so the ordering invariant lands once, in the publisher's contract.

PUBLISH ORDERING: DONE, and this dot no longer owns it. The bulk-window publisher landed the acquire/release discipline as a STRUCTURE rather than a text pin, which is what the re-expression was for. src/habu/habu1.f BXREFRETARGET is now the only writer of a record's start/len pair: it takes both cells in one protection window, stores the LENGTH first with a plain store and publishes the START last with STLR, so the release is the primitive's own instruction and cannot be edited out of the emitted engine without changing that primitive. src/compiler/native/publish.f states the ordering in its contract prose ("AND THE RECORD IS COMMITTED IN ONE ORDER, WITH RELEASE") and its COMMIT word makes the retarget the last write of the publication, after the bytes, the cache flush and the relocation bits. test/compiler/native-publish.f pins the observable half — the published bytes, the code pointer and the call map are all correct at the moment the record points at them. What is LEFT for this dot is only MMAP-DIAG, the fail-closed mmap-failure diagnostic; the LDAR/STLR half is closed here.
