---
title: "Checker: cp.async pipeline typestate capability"
status: open
priority: 2
issue-type: task
created-at: "\"2026-07-16T19:10:50.469826+02:00\""
---

The long-term closer for the pipelined-GEMM typed vocabulary (habu-typed-pipelined-register-4d20acb5): make the checker EXPRESS the cp.async pipeline discipline so the staging bodies stop being trusted. Today the tilepipe vocabulary encodes what the type system can already state fail-closed (nominal families: buffer-parity types, alignment obligations, layout-parameterized tiles, pipelined-tile distinct from scalar tile) but the DYNAMIC pipeline protocol - cp.async.cg.shared.global issue -> commit_group -> wait_group N -> bar.sync -> read staged data, with double-buffer parity alternation and no read-before-wait - is enforced only by the proven emitter bodies (named TRUSTED boundaries owned by THIS dot). Work: a checker typestate/linear capability modeling the pipeline: staged-buffer tokens minted by the async-copy word in state pending<parity>, consumed exactly-once through commit/wait into state ready<parity>, reads require ready of the matching parity, bar.sync transitions per the M5 barrier model (habu-ptx-m5-mask-eb0716f1 - block-uniform reachability; compose, do not duplicate), loop-carried parity alternation provable across TILE-LOOP iterations (relates to the linear-once capability habu-linear-once-resource-4c58a7a1 and the loop-carried row machinery). Negatives: read-before-wait rejects; parity mismatch rejects; missing commit rejects; double-wait rejects. Acceptance: the tilepipe staging words' bodies re-expressed as CHECKED code certifying under the new model, byte-identical PTX preserved (capture+cmp), the TRUSTED rows this dot owns REMOVED from TRUSTED.md, trusted-inventory ratchet down, all ptx suites + device goldens (on zed return) green, negative fixtures for each reject. Files: src/core/checker.f + type-family machinery (COORDINATE: tfam sealed-packages lane), lib/ptx staging words, tests. Ownership: checker capability - the stored-xt/trust-retirement program's kernel-side sibling. Blocks nothing today (tilepipe lands with named boundaries); this dot's landing is what deletes them.

FOLDED IN 2026-07-16 (GEMM stage-3 landing): remove the now-unreferenced
mmacc TFAM row from src/core/type-family.f when this dot's capability work
re-touches the registry (core edit + fixpoint rebake; deliberately excluded
from the lib-only stage-3 commit). mmctx remains referenced.

Claim: agent=cpasync workspace=.jj-ws/fable-cpasync (host lane - checker typestate; NO zed access, byte-identical PTX is the device-equivalence proof)

BLOCKED ANALYSIS 2026-07-17 (cpasync lane, LESSONS finding landed as commit
26ae31a5-rebased; claim RELEASED, no untrust, no engine edit, checker never
weakened). The capability CANNOT land until prerequisites resolve - four
proven blockers, full detail in LESSONS.md:
(1) DOMINANT: all 9 tilepipe bodies mint phantom kernel tokens from bare
register literals - untrusting them needs the checked-mint capability owned
by habu-ptx-phantom-preserving-3df9db92 (open, unstarted); a cp.async
typestate alone discharges ZERO rows (same precedent as
habu-linear-once-resource-4c58a7a1's note).
(2) the dynamic protocol (issue->commit->wait->bar.sync->read + parity flip)
is fused inside the shared byte-sensitive emitter MM-PIPE-KLOOP-WITH
(lib/ptx/cg-matmul-emit.f), consumed verbatim by cg-matmul.f/cg-mma.f/
maki/lower-mm.f - needs the staging-emitter decomposition dot
habu-decompose-pipelined-staging-49c97cba (minted 2026-07-17) with device re-goldens.
(3) the bar.sync transition must compose with habu-ptx-m5-mask-eb0716f1
(open, unstarted) - not duplicate it.
(4) RUNTIME loop-carried parity alternation ($KLOOP xor-flipped %r15) is a
property of the emitted PTX's runtime dataflow, outside an emit-time
stack-effect checker - only same-body parity consistency is ever
emit-time-checkable; the alternation slice stays a named boundary
permanently unless a runtime-dataflow capability exists.
ORDERING: phantom-preserving-3df9db92 AND m5-mask-eb0716f1 AND the
decomposition dot land first; THEN this dot adds the linear pipeline-slot
kind, re-expresses tilepipe bodies, lands the 4 dynamic negatives
(read-before-wait, parity mismatch, missing commit, double-wait). The 7
structural negatives already reject today (tile-pipe-neg-test.f green).
mmacc TFAM row: verified unreferenced, deliberately left to ride the next
registry edit (avoids a standalone fixpoint rebake).

BLOCKER 2 DISCHARGED 2026-07-17 (habu-decompose-pipelined-staging-49c97cba
closed, commit 9e642e70): the protocol steps now exist as named checked
CPP-* words in cg-matmul-emit.f with documented per-step obligations
(issue opens pending<parity>; CPP-COMMIT closes the group; CPP-WAIT n
retires; CPP-SYNC separates wait/read and read/reuse; CPP-CUR-WINDOW
reads ready<parity>; CPP-NEXT-WINDOW writes pending<!parity>; CPP-FLIP =
same-body parity consistency). Remaining blockers: (1) phantom mint
capability leg 2b (habu-ptx-phantom-preserving-3df9db92 - the checked-mint
half; legs 1/2a landed the preserving+sink half), (3) the M5 barrier model
(habu-ptx-m5-mask-eb0716f1), (4) runtime loop-carried parity stays a named
boundary permanently (out of checker scope).

BLOCKER 3 DISCHARGED 2026-07-17 (habu-ptx-m5-mask-eb0716f1 closed, commit
e87cb494): the block-uniform barrier model exists (CTL-BARRIER +
E-DIVERGENT-BARRIER, #CFC>0 rejection). The cp.async typestate's bar.sync
transition composes with CTL-BARRIER at the CPP-SYNC seam. Remaining
blockers: phantom mint capability leg 2b (dominant), and the permanent
runtime-parity named boundary.

BLOCKER 1 SHAPE RESOLVED 2026-07-17 (phantom leg 2b, commit a2a8386e):
the NP-MINT-CHECK seal proves genuine fresh mints CANNOT become checked
callers (a checked word may not introduce an unbound phantom output var -
that is the soundness line itself). So blocker 1's resolution for tilepipe
is NOT "zero trust": the PIPE-* bodies keep a SMALL audited mint core
(the mmctx/mmstage/... literal mints) while the PROTOCOL becomes checked
via the typestate capability over the CPP-* seams. When this dot lands,
its acceptance should read "TRUSTED surface shrunk to the audited mint
core + checked protocol", not "rows deleted to zero".
