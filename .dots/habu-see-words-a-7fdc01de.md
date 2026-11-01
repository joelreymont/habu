---
title: See words a user-defined definer makes in discovery
status: open
priority: 3
issue-type: task
created-at: "2026-11-01T15:41:47.206827+02:00"
---

Problem: tools/source-discovery.f records declarations only from the definers it knows, so a word made by a user-defined definer (`: MKW ( -- ) create ;` then `MKW W`) is invisible to the walk. A bare use of W after it still takes an earlier parses: row for W in a scope the use reaches, reads the next token as W's argument, and can miss a load the engine makes. Fixture /tmp/jerry-disc-rv4-tmp/fx/userdef (A's W parses 1; in part A:B, `MKW W 0 drop` then `W require dep.f drop`): the engine prints LOADED; lane disc fold 5 misses dep.f by design (the row precedes the use in the same file), and master's tools miss the global-row analogue. Acceptance: a use that may bind to a word a definer outside the known set made either applies the right row or refuses E-DISC-SHADOW; userdef and its global-row twin find dep.f or refuse; a tools/event-closure-test.f case failing before. Files: tools/source-discovery.f, tools/event-closure-test.f. Verify: tools/event-closure-test.f, tools/source-discovery-test.f, real-all closures unchanged. Depends: lane disc landed (habu-key-discovery-records-b367e77e). Related: habu-resolve-a-bare-242051fb. Claim: unassigned.

Words made or retired inside `evaluate` are invisible the same way (fold 5 review, /tmp/jerry-disc-rv5-tmp/fx): evalredef (P's W parses 1, then `s" undefine W : W ( -- ) ;" evaluate W require dep.f`) and evalclose2 (`s" ;package" evaluate` between P's cr row and `cr require dep.f`) are LOADED by the engine and missed by master and lane disc; evalredefg, the global-row twin, misses on master.
