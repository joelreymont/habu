---
title: Copy the tier-1 back end T0 uses into COMP
status: closed
priority: 2
issue-type: task
created-at: "2026-11-03T12:29:33.248064+02:00"
closed-at: "2026-11-10T21:56:45.441200+02:00"
close-reason: merged into habu-copy-every-tier-86155a25, which carries its whole text (dots audit 2026-11-10)
---

Joel 2026-11-03: T0 shares only front-end code with tier 1; what T0 needs from tier 1's back end is copied into COMP and owned there; tier 1 keeps its copy unedited. Design /tmp/jerry-big/t0copy-design.md (rev 4; rev 3 kept as t0copy-design-r3.md). Lanes: 1 back-end copy (t0.f private PUBLISH-IT, HOST-ARITIES, HOST-ROOT-CONTRACT, HOST-DECL?, BODY-KIND, FIXED-VAL; no public items), workspace .jj-ws.noindex/jerry-t0pub, change tnsprlqx; 2 the callback request's typed state in definition.f, T0 reading it directly, FFI-CB EMIT ending the request on a throw (fixes habu-disarm-a-cb-7870abbf, supersedes habu-give-the-cb-e6876f60's design); 3 T0's fixed-shadow cell, picked by the tier at the builder sites (brief /tmp/jerry-big/t0c3-brief.md, to revise). Lanes 2 and 3 add public items (NDEFINITION:STUB-REQUEST, STUB-PLACED!, CALLBACK-STUB@'s contract, one cell): critic, then Joel. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-t0pub.

Lane 1 landed 10abdcfe "Give T0 its own publish step" (src/habu/t0.f, +73 -15). Proofs on the candidate .jj-ws.noindex/jerry-t0pub/.out/cand/hb (built from 5c9ea988 plus the change; master since differs only in dots, DECISIONS, a comment in tools/native-file-inputs.f and x86-only kernel-x64.f): candidate build rc 0; 10 arm64 rows and native-file-state rc 0; full suite 518 of 518 PASS (/tmp/jerry-big/t0pub-suite.log); two-generation chain gen 5 matches gen 4 (t0pub-chain.log); x86-64 cross-build rc 0; on pulsar tier, does-clause-record and compiler-native-create-does give the same results before and after (tier's 6 TFAILs 39-45 are the replay gap). Review LAND, no fold-ins. Coverage gap the review named: compiler-native-export-fixed runs at `1 set-tier`, so T0's defer, value and address publication is exercised only by the suite's default-tier compiles. Install: candidate sent to carl; jerry-t0pub is kept until carl confirms.

Public items approved by Joel 2026-11-03: NDEFINITION:CALLBACK-REQUEST@ ( -- n n ) (the design's STUB-REQUEST) and NDEFINITION:CALLBACK-ENTRY! ( n -- ) (the design's STUB-PLACED!) for lane 2; CALLBACK-STUB@'s new contract, with FFI-CB EMIT refusing 0 with E-COMP-STATE; NCOMP-DISPATCH:COMP-SHADOW-CELL for lane 3. New lane before lane 2: the E-NCOMP- codes become E-COMP- (dot habu-rename-the-e-e405461b).

Lane 3 is held and lands with "Emit Wasm from T0" (habu-emit-wasm-from-0d5dab47), lead's decision 2026-11-03. Critic round 1 on t0c3-brief.md (/tmp/jerry-big/t0c3-critic-r1.md): picking the fixed-body recorder by tier sends a tier-0 create, does> child or defer under an open Wasm shadow to T0's SHADOW-FIXED, whose SHADOWED exits with no Wasm emitter (t0.f :1864-1865); test/wasm/backend.f :120-144 and test/wasm/capture.f :42, :64, :66, :280-298 define such words and would lose their routines, and a does> child would throw E-NSHADOW-ROW (shadow.f :414-424). Keeping tier 1's recorder for machines T0 cannot emit is the tie Joel's 2026-11-03 ruling refuses; moving those test words under `1 set-tier` changes a tested contract to fit. The Wasm lane gives T0 a Wasm emitter, after which the tier pick holds on every machine with the tests unchanged. The other round-1 findings (edits 2-6, proofs) are applied when t0c3-brief.md is revised for that lane.

Lane 2 (brief /tmp/jerry-big/cbreq-brief.md): critic round 2 REJECTed on wording (t0.f header, callback-stub.f :3, CALLBACK-REQUEST@'s refusal), a missing pulsar row (ffi-callback) and CALLBACK-STUB@'s name: it now ends the request, a fetch name on a take (card section 1); notes /tmp/jerry-big/cbreq-critic-r2.md. The brief carries the fixes; the name goes to Joel.

Lane 2 held, no workspace: asked to rename CALLBACK-STUB@, Joel objected to any use of NDEFINITION and the N* packages by T0 (2026-11-03). T0 today calls 16 of tier 1's N* packages from src/compiler/native/ (NEMIT 31 sites, NDICT 19, NHOST 11, NDEFINITION 10, NSTR 8, NPUB 8, NCBSTUB 8, NSHADOW 7, NFETCH 5, NFETCH-CHECK 4, NCOMP-DISPATCH 4, NTRAP 2, NFAM 2, NELAB 2, NCOMP 1, NABI 1; rg over src/habu/t0*.f at 2cdee53f).

Census 2026-11-04: the claim named a workspace or agent whose work is gone or landed; reset to open, Claim: unassigned.
