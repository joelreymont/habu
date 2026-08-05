---
title: Re-express the deleted memory-ordering pins
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T12:28:12.451258+02:00"
---

The scaffold-del lane deleted two text pins whose invariants now have NO check: MMAP-DIAG (fail-closed mmap-failure diagnostics — an unforceable failure path) and PROT-PUBLISH (LDAR/STLR acquire/release ordering on publish). Both are real invariants of the emitted engine; text pinning was the wrong check, absence of any check is worse. Re-express structurally: a LINT-LEX token-kind scan over the emitted source (the shape the lane used for the kept arena-parity check), or an executed probe where reachable. The bulk-window publisher dot habu-publish-native-code-886e3ef9 owns the publish ordering going forward — coordinate so the ordering invariant lands once, in the publisher's contract.
