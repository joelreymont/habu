---
title: Split call-crossed values around the loop
status: active
priority: 2
issue-type: task
created-at: "2026-08-05T17:03:15.149470+02:00"
---

CALL-PRESSURE (corpus 4) refuses E-A64RA-SPILL because elaborate.f CROSS-STEP threads a call-surviving local through every loop block (LOCAL-ARGS+) and the call's operand list (CALL-OPERANDS+); the exclusions from MB-SPILLABLE? originate there, upstream of the allocator (five allocator-side mutations measured, none moves the refusal; tools/codegen-spill-probe.f is the merged regression instrument pinning the facts).

REFUTED 2026-08-12, and this replaces the "PROVEN FIX, ONE LINE" that stood here. Suppressing the threading really does close CALL-PRESSURE — measured on today's tree: the row compiles at 148 bytes, the judge's answer columns agree, and every other judge row is byte-identical. The half of the old claim that does NOT hold is "no mark, no mandatory spill is needed". The threading is not only a guard against the operand-list hazard; it is the only HOME a surviving local has when the callee publishes no record of what it destroys. Such a callee is taken to destroy the whole pool, no register survives the branch, and the data-stack slot the call's operand list buys is the only place left.

WHAT BREAKS, MEASURED THROUGH THE PRODUCTION ENTRY. With CROSS-L answering nought, two programs the tree compiles today refuse with E-A64RA-POOL (-8446): the stdlib's own multishot site ARRAY:A-MAPI!, migrated at file level in test/compiler/native-exec.f, and LGP-CALL in test/compiler/native-migrate.f. Both call a routine with no clobber record — `execute` in one, an engine-compiled word in the other. A full test/run.f under the suppression is red in exactly those two places plus the three artifacts the change legitimately moves (codegen-spill-probe, codegen-compare-test, judge-test); the same tree without it is green.

THE WALL TABLE, all through NMIGRATE at eighteen registers, locals read after a loop that calls:

  callee                                threading ON        threading OFF
  chain-published (has a record)        7 cross, 8 refused  8 cross (CALL-PRESSURE green)
  engine-compiled (no record)           6 cross, 7 refused  1 refused, E-A64RA-POOL

The two 7/8 and 6/7 pairs are now cases in tools/codegen-spill-probe.f (CROSSING-CASES and RECORD-CASES), so the record's worth — exactly one crossing value — is a suite member and not a note. The threading-OFF column is a mutation result recorded in that file's prose.

THE TWO HALVES THE OLD CLAIM RAN TOGETHER. The hazard IS double-enforced downstream, and that half was verified on today's tree: regalloc.f MB-CROSSES?/MB-FORBID bars every register a crossed call destroys, over the dataflow liveness MB-LIVENESS/MB-EXTEND1 computes rather than over a textual reading, and regalloc-verify.f CLOB-AT re-derives the same thing from its own tables and throws E-A64RAV-CLOBBER. So removing the threading cannot make a wrong program; it makes an uncompilable one. Safety and capability are different questions and the leaf answered only the first.

ROADS THAT REMAIN, none of them started.
  (a) Make the threading conditional on the callee leaving nothing alone. NCLOB:KNOWN?/GPR-CLOB/FPR-CLOB already answer it and clobber.f has no cycle with elaborate.f, so the elaborator could ask at the call row and thread only when no register can survive. It is a structural capability probe, not a value heuristic. Cost: a new dependency from the HIR builder onto a machine-level record, and a re-measurement of the recorded-callee wall, which the table above says rises from seven to eight.
  (b) Teach the allocator to put a class defined or read in a middle block into a frame slot — habu-spill-from-a-4145325c. Then neither shape needs threading and BOTH corpus rows close on one capability, which is what tools/codegen-compare-new4.f has said all along: the two refusals are one refusal reached two ways. The 2026-08-05 reading that split them is what this refutation withdraws.
  (c) The retry the old text proposed (attempt without threading; on refusal re-run with it). NMIGRATE:MEASURE-HELD now separates compile-attempt from publish, so the bridge objection may have expired — but it is two compilations per definition and it hides the wall instead of moving it.

Recommendation: (b). It is the only one of the three that closes PRESSURE-LOOP as well, and (a) buys one corpus row at the price of a layering the tree has so far kept.

Claim: agent=spillclose workspace=.jj-ws/habu-spill-close
