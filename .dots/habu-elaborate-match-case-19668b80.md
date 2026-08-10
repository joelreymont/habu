---
title: Elaborate MATCH, case and construct over existing IR
status: active
priority: 2
issue-type: task
created-at: "2026-08-10T00:23:52.581327+02:00"
---

f0cfa96a's aggregate-kinds premise is FALSE (measured 2026-08-10): an ADT value is W flat cells with the tag on top (observed via generated constructors: SOME 42 = [42][1] top-first), the chain already compiles every dispatch shape (probes: 4/4 hand-elaborated match equivalents and 2/2 constructs compiled), and the registry publishes everything needed (TFL-MATCH-FAM?, TFL-CVAR?, TFAM-INST-WIDTH@ - the same words the engine's emitter bridges to at habu2.f:7090/7109/7135). DESIGN (ruled): a MATCH-SCAN pre-pass sibling of RESOLVE-SCAN resolving family/variant tokens BY POSITION via the checker's registry; four HIR:ctrl codes (open-match/match-arm/close-arm/close-match); arms lower to hir.const+hir.eq+hir.brz, joins via existing block args, vector reshaped per arm at compile time (tag+pads dropped, payload kept); mismatch edge gets hir.trap. case = same machinery, tag from a popped value; construct = pre-scan + constant pushes. NO new IR type kinds. 78 MATCH + 7 case + 3 construct definitions; 53% of 303 sites are two-arm; measured engine cost to beat: 128B two-arm, 184B four-arm. Acceptance: the eight criteria in the match-design report (option UNWRAP through the census entry executing both variants; payload-free/payload/7-arm/44-arm agree with engine value-for-value; adversarial refusals carry the CHECKER's reason - non-exhaustive, duplicate variant, wrong family, family name in a comment/string, missing of, stray ;match; dead arm compiles; trap fires once-emitted; codegen-compare rows beat 128/184; census MATCH spelling gone; maki+ptx green). Files: src/compiler/native/{elaborate,hir,hir-word}.f. Depends: habu-model-dead-paths-725fbaa0 (the throw-arm 19%), habu-give-hir-a-ba02f451. Closes the premise of habu-give-the-ir-f0cfa96a.

Claim: agent=matchb workspace=.jj-ws/habu-elaborate-match
