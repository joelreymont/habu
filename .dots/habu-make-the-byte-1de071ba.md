---
title: Make the byte comparison count what runs
status: active
priority: 2
issue-type: task
created-at: "2026-08-07T13:36:13.413538+02:00"
---

Claim: agent=benchfix workspace=.jj-ws/habu-make-the-byte-1de071ba

Audit findings (2026-08-07, full report in the audit transcript; evidence re-derivable from /private/tmp/ccref): (1) every chain row's byte count omits its trailing ret while clang's includes it — XREF-LEN records the body only; the compare must add the return's 4 bytes (or count clang's without) so the columns measure the same thing; NOOP-N reporting 0 bytes for a ret-only word is the tell. (2) The corpus-5 tail rows print the wrong sign: honestly counted, TAIL-BIG is chain 4 + its 52-byte callee = 56 vs clang's inlined 40 — the table must count the callee a tail branch reaches when the reference inlined it, or annotate the row as convention-different. (3) Twin fidelity: STORE-LOAD's twin does one load and one store regardless of len (the loop-carried dependence the row is NAMED for is gone), CELL-BUMP's twin dead-stores the first write — both need volatile in tools/clang/twins.c so clang keeps the memory behavior the rows measure; PRESSURE-LOOP's twin hoists the whole body (the 14-live-value pressure never occurs in the reference — annotate the row). (4) tools/codegen-compare-macho.f's exactness rests on 'no non-external code in __text', unasserted — LOAD-FROM must refuse a non-external text symbol the way it refuses a non-tiling one. Re-pin all tables deliberately after; every delta must be the accounting fix, no row's real code moves.
