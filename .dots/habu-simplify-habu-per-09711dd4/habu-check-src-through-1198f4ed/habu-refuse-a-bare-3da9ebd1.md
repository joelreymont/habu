---
title: "Refuse a bare use of a disabled global parses: row"
status: open
priority: 2
issue-type: task
created-at: "2026-11-01T16:16:14.330579+02:00"
---

Problem: in tools/source-discovery.f a harvest pass reads a bare use ahead of its row as source, so a definer there records a second declaration of the name and disables the row; a disabled global row then lets the bare use through without the E-DISC-SHADOW refusal, and the replay reads the operand as ordinary source. Fixture globoff, entry.f: 'package P : W ( -- ) ; ;package' / ': W ( -- ) parse-name 2drop parse-name 2drop parse-name 2drop ;' / 'W create W \ require dep.f' / 'parses: W 3': the engine prints LOADED, discovery on master and lane disc finds entry.f only. Lane disc's fold-in refuses this shape for package rows (ECT-TEST-BARE-DISABLED); global rows keep master's behaviour. Acceptance: globoff finds dep.f or refuses E-DISC-SHADOW; real-all.f closures unchanged; a tools/source-discovery-test.f case (suite row source-discovery) failing before. Files: tools/source-discovery.f (SD-R-USE-OK?), tools/source-discovery-test.f. Verify: tools/source-discovery-test.f, the fixture. Depends: lane disc landed (habu-key-discovery-records-b367e77e). Ownership: tools/source-discovery.f. Claim: unassigned.

Census 2026-11-04: a disagreement between two implementations of one thing; it is closed by its parent removing the second implementation, not by patching that copy.
