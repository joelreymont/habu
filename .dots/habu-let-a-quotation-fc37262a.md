---
title: Let a quotation body hold a control structure
status: open
priority: 2
issue-type: task
created-at: "2026-08-13T19:06:58.686093+02:00"
---

Found pre-existing by the exceptions lane (6ceb7667, reproduced on the PARENT binary through the pre-catch route - a body handed to a callee declaring a quotation argument): a quotation body holding ANY control structure refuses E-IR-VERIFY-SUCCARG. Bounds the catch production shape alongside the scope ceiling. Files: src/compiler/native (quotation build path - find the owner; the verifier is naming a successor-argument invariant the QBUILD walk does not maintain for branching bodies). Depends: none technical; sibling of habu-compile-a-quotation-7efa798e.

DIAGNOSED 2026-08-13 (quot-scope lane, landed 354b8937 fixed the
elaborator half). Root: a block id is a MODULE-wide ordinal (ir/fun.f
END-BLOCK; select.f SUCC-IDX subtracts R-BASE) but several machine
passes read a successor's module ordinal as a FUNCTION-local index.
The elaborator now raises its walk ordinal over BBASE and holds the
base against END-BLOCK at every close, so a body's branch names its
own blocks - the refusal is now uniformly E-A64RA-SHAPE (-8323)
whatever encloses the body (it was -8088 or -8091 depending on the
ENCLOSING function's block count, the signature of the missing
per-function fact). REMAINING WORK, the machine-side mirror: 
regalloc.f:1064 SUCC-ORD, regalloc-verify.f:534 SUCC-ORD, :1112
VSUCC-ORD, :1889 VDEDGE?, emit.f:1155 SUCC-BLOCK all need the
R-BASE subtraction select.f already does; spill.f COPY-SUCCS is
already correct. A 4-file cascade - own lane. Pinned live:
native-catch.f BODY-CONTROL-CASE holds the moved code.
