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

ROAD (a) IS BUILT, 2026-08-12. The threading is conditional on the callee's record: src/compiler/native/elaborate.f CALL-KEEPS? asks NCLOB:KNOWN? at each call row of CROSS-SCAN, CALL-BARE records that one of the body's calls keeps nothing, and LCROSS? answers true only when the mark AND that flag agree. A call the file cannot name — `execute`, `is`, RECURSE, every CTRL-CALL? form — answers "keeps nothing", which is the fail-closed direction. That is the elaborator's first read of a machine-level record, and NCLOB:KNOWN?'s first production caller; both costs were accepted knowingly when the road was chosen.

WHAT IT BOUGHT. CALL-PRESSURE compiles at 148 bytes and is a measured row in both harnesses: the judge's refused count is 1 (PRESSURE-LOOP alone), and the fourth corpus declares one gap where it declared two. The engine-callee wall is untouched at six crossing values, so ARRAY:A-MAPI! and LGP-CALL still compile. Both walls are cases in tools/codegen-spill-probe.f and each forced constant reds one of them: CALL-KEEPS? forced true takes the engine-callee wall to nothing (-8446, and both of those programs with it), forced false puts CALL-PRESSURE back to -8508.

IT IS TRANSITIONAL BY CONSTRUCTION. Post-cut every callee is chain-published and carries a record, so CALL-BARE is never set, the threading never fires, and CALL-KEEPS? with the whole LCROSS/LS-PEND scan behind it retires with the cut's cleanup. Nothing else has to happen for that; the code path simply stops being reached.

THE ROADS NOT TAKEN, kept because the reasoning is still the tree's.
  (b) Teach the allocator to put a class defined or read in a middle block into a frame slot — habu-spill-from-a-4145325c. It would close PRESSURE-LOOP too, and it stays gated behind its own leaf's rule: a real program shape has to demand it. Neither corpus row provides one now that (a) and the remat pair are the road for PRESSURE-LOOP.
  (c) The retry (attempt without threading; on refusal re-run with it). NMIGRATE:MEASURE-HELD separates compile-attempt from publish so the bridge objection has expired, but it is two compilations per definition and it hides the wall instead of moving it.

Claim: agent=spillclose workspace=.jj-ws/habu-spill-close
