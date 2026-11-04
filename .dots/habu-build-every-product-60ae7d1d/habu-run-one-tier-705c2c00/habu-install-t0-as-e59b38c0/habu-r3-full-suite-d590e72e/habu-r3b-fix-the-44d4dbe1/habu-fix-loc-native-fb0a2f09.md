---
title: "fix loc: native refusals name the token's file and line"
status: closed
priority: 2
issue-type: task
created-at: "2026-10-30T22:41:49.777141+02:00"
closed-at: "2026-10-31T12:11:58.156642+02:00"
close-reason: "Landed with the T0 release (ttloqwsx b17a1ee3): full suite 510/510, two generations converge, edit matrix F53/F105/F156 only, as master"
---

Problem: master's JIT refusals carry ' at <path>:<line>' (E-UNDEFINED: CKT-MISSING at …/fwd.f:1; hb: ; with a quotation open: RXSC at …:1); the native tiers print none (ncomp: cannot compile RXSC at ;). Tests that ratified the location-less text: tools/check-test-lib.f:1345, :1786, :7197, :3233; test/c2-memory-refusals.f:40; runtime-regression RXSC/RXSF legs. Acceptance: every native refusal names its token's path and line with the JIT's own location code; those tests assert the location again. Files: src/compiler/native/definition.f REPORT-FAILURE/REPORT-UNDEFINED, src/habu/outer.f AT-SOURCE/LINE, the listed tests. Verify: rows named in /tmp/jerry-big/fix-loc-brief.md. Depends: none. Ownership: those files. Workspace: .jj-ws.noindex/jerry-loc. Claim: jerry.

The arena red (test/arena.f OWNER-CHILD expects master's `E-UNDEFINED: FOOS:HEAD at ` twice from its child) has this cause: the arena row is a proof row of this lane, unedited. Brief /tmp/jerry-big/fix-loc-brief.md, worker launched 2026-10-30 23:00.

loc-a first worker handed off unbuilt (/tmp/jerry-big/loc-handoff.md): rows band BODY-SOURCE, EMIT-BCAP rows, NDEFINITION:BODY-LINE; SOURCE-TEXT:LINE in util.f is not visible to definition.f. Fresh worker continues in jerry-loc and picks a home before outer.f, check-hook.f and definition.f. Finding for after loc-a: checker.f DIAG-POS is a third newline count (line, column, byte); checker.f loads before check-hook.f, so sharing it depends on where loc-a puts the word. Brief for loc-b drafted at /tmp/jerry-big/fix-locb-brief.md (LOCA-* placeholders).

loc-a second worker stopped (/tmp/jerry-big/loca-handoff2.md): SOURCE-TEXT:LINE needs a PPRIM: row (cold-naming rule), stays in util.f; snap-lib heap writes from the running floor. Blocker: a band below DATA-START moves DATA-START; the build from the public hb refuses (native-build-core.f RESET-ADDRESS-ROWS); bootstrap.md names a cold source writer not found in tools/. Design on Fable (Plan, brief /tmp/jerry-big/loc-design-brief.md).

Design (Fable Plan) returned; lead cut: no new body-source.f (reader stays private in definition.f, its only caller); tier 1 gets its refused token's address (elaborate.f span start), per the earlier ruling. Root causes of the build failures are build code reading the tree's DATA-START for the host's floor: snap-lib.f heap writer (HEAP-START) and native-build-core.f RESET-ADDRESS-ROWS (equality dropped; CHECK-HOST-LAYOUT checks the host rows). loc-a2 worker in jerry-loc, brief /tmp/jerry-big/fix-loca2-brief.md; the reader hunk moved out to /tmp/jerry-big/loc-reader.diff for loc-b. x86-64: snap-x64.f and snap-decode-x64.f read DATA-START the same way; they need the host floor when x86-64 is next built (habu-port-the-ffi-676f745d).

loc-a2 vsqlmulv and loc-b1 mypxtnpx done (stack: on t1q ryonyrtn). loc-b2 worker in jerry-loc @ sluqzqln, brief /tmp/jerry-big/fix-locb2-brief.md: the checker-refused body names its token and line through the owner record, test texts back to master's located lines, tier 1's open-construct refusal names `;` through the shared last-read-token reader (t1q legs assert T0's text).

loc-b2 first worker handed off before building (/tmp/jerry-big/locb2-handoff.md): owner field PIN-SPAN at $5D0 (record $5D8), CLOSE$ moving to definition.f. Continuation worker launched on /tmp/jerry-big/fix-locb2-cont-brief.md (RENDER-DOES-BAD names `+`, the checker's stop token; LOCAL-QUOTATION located `at char`).
