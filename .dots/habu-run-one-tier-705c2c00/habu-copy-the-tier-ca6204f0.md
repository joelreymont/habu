---
title: Copy the tier-1 back end T0 uses into COMP
status: active
priority: 2
issue-type: task
created-at: "2026-11-03T12:29:33.248064+02:00"
---

Joel 2026-11-03 (LESSONS "editing tier 1"): T0 shares only front-end code with tier 1; what T0 needs from tier 1's back end is copied into COMP and owned there; tier 1 keeps its copy unedited. Design /tmp/jerry-big/t0copy-design.md (rev 4; rev 3 kept as t0copy-design-r3.md). Lanes: 1 back-end copy (t0.f private PUBLISH-IT, HOST-ARITIES, HOST-ROOT-CONTRACT, HOST-DECL?, BODY-KIND, FIXED-VAL; no public items), workspace .jj-ws.noindex/jerry-t0pub, change tnsprlqx; 2 the callback request's typed state in definition.f, T0 reading it directly, FFI-CB EMIT ending the request on a throw (fixes habu-disarm-a-cb-7870abbf, supersedes habu-give-the-cb-e6876f60's design); 3 T0's fixed-shadow cell, picked by the tier at the builder sites (brief /tmp/jerry-big/t0c3-brief.md, to revise). Lanes 2 and 3 add public items (NDEFINITION:STUB-REQUEST, STUB-PLACED!, CALLBACK-STUB@'s contract, one cell): critic, then Joel. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-t0pub.

Lane 1 landed 10abdcfe "Give T0 its own publish step" (src/habu/t0.f, +73 -15). Proofs on the candidate .jj-ws.noindex/jerry-t0pub/.out/cand/hb (built from 5c9ea988 plus the change; master since differs only in dots, LESSONS, a comment in tools/native-file-inputs.f and x86-only kernel-x64.f): candidate build rc 0; 10 arm64 rows and native-file-state rc 0; full suite 518 of 518 PASS (/tmp/jerry-big/t0pub-suite.log); two-generation chain gen 5 matches gen 4 (t0pub-chain.log); x86-64 cross-build rc 0; on pulsar tier, does-clause-record and compiler-native-create-does give the same results before and after (tier's 6 TFAILs 39-45 are the replay gap). Review LAND, no fold-ins. Coverage gap the review named: compiler-native-export-fixed runs at `1 set-tier`, so T0's defer, value and address publication is exercised only by the suite's default-tier compiles. Install: candidate sent to carl; jerry-t0pub is kept until carl confirms.
