---
title: PTX phantom-preserving effects
status: active
priority: 2
issue-type: task
created-at: "\"\\\"2026-07-01T23:07:20.878978+02:00\\\"\""
---

lib/ptx TRUSTED: base is 75 sites but only 17 are genuine register mints; the other ~66 exist because kernel newtype wrappers cannot carry the phantom through checked EMIT-* words (PTX string emitters underneath are already checked). Capability: phantom-preserving effects - kernel-typed values carry their n register representation through checked emitters. Retires 66 sites to a 17-cast mint core. Effort L (~1wk). Directly serves maki: every new kernel op stops minting trusted wrappers (feeds habu-checker-capability-typed-e0c76a02 adjacent work but is orthogonal to loops/smem).

## Audit refresh (2026-07-06, head 1eb3b5d3)

Count drift: the lib/ptx TRUSTED base has GROWN to 87 inventory rows
(tools/trusted-inventory.f; raw `TRUSTED:` tokens 95) since the 75 counted at
mint — new kernel work keeps minting trusted wrappers exactly as this dot
predicts. The capability itself is unstarted; the 17-mint-core target stands,
the retire count is now ~70.

Claim: agent=phantom workspace=.jj-ws/fable-phantom (host lane, LEG 1: capability + pilot; lib/ptx/cg-mma.f + cg-matmul.f are FENCED to the wave3 lane - broad sweep is leg 2)

LEG 1 LANDED 2026-07-17 (phantom lane, commit 0360af4a; leg-1 claim
RELEASED). HONEST FINDING: the dot's premise was half-wrong - the checker's
existing row-polymorphic higher-order effects ALREADY express
type-PRESERVING phantom flow; no checker.f/type-family.f change was needed.
Capability = three forge-proof combinators in lib/ptx/rep.f (package
PTXREP): REP1 ( a [ n -- n ] -- a ), REP2 ( a a [ n n -- n ] -- a ),
REPMIX2 ( a b [ n n -- n ] -- a ) - bodies are bare execute; the UNIFIER
enforces forge/kind/arity rejection (negative regressions in
rep-neg-test.f, positives in rep-test.f, wired into gate slices).
23 type-preserving wrappers converted to CHECKED callers (tile.f,
tile-v4.f, tile-v4a.f, collective.f, ad-saved.f); +3 combinator rows;
NET -20 trust sites, ratchet ok. Byte-identity proven (golden capture
pre/post/rebased). Full run.f perf-verdict pass attempts=1 on the exact
merge tree; no engine prefix touched.
LEG 2 REMAINDER (by category):
(2a, cheap, unfenced, NO new capability): ~13 SINK wrappers via a sink
combinator ( .. [ n .. -- ] -- ): STORE STORE-ONCE SCATTER-ADD
FANIN-SCATTER-ADD INDEX-DENSE-STORE INDEX-SCATTER-ADD INDEX-STORE
(tile.f), ROW-STORE ROW-STORE-ONCE ROW-SCATTER-ADD (collective.f),
STORE-V4 (tile-v4.f:21), STORE.V4 (tile-v4a.f:58), SSTORE
(tile-smem.f:31); plus 3 ternary via REPMIX3: FMA. (tile.f:104),
BLOCK-MAX-SELECT (collective.f:98), ACC-FMA (tile-acc.f:30).
(2b, needs the DEEPER rep-provenance/typed-emitter capability - mint a
NEW phantom from an emitter output): ctx/load/reduce/stage families in
tile.f, tile-v4.f, tile-v4a.f, tile-smem.f, collective.f, tile-acc.f,
cg-attention.f (full list in the leg-1 report).
(2c, cp.async-blocked): the 9 tile-pipe.f PIPE-* rows (see
habu-checker-cp-async-6ba788a5 ordering).
(2d, intended PERMANENT trusted mint core ~17: MK-SPAN*, MK-MATRIX*,
V4-ALIGN, cg.f *-REG R>BITS BITS>R, cg-attention Q/K/V/O-REG).
(2e, fence-blocked: cg-matmul.f:1 only - the staging-decomposition lane
barely intersects this program.)

LEG 2A LANDED 2026-07-17 (commit c2f1c298, habu-phantom-leg-2a-cf31f9f0
closed): SINK3/SINK4/REPMIX3/REPMIX3B added, 16 more wrappers checked,
net -12 (TRUST now 314, trust-lint 664 sites). Remaining on this dot:
leg 2b - the DEEPER capability (mint a NEW phantom from an emitter
output: ctx/load/reduce/stage families), category 2c rides the cp.async
program, 2d (~17 mints) is the intended permanent core, 2e is
cg-matmul.f:1 (unfenced once the decomp lane retired - now available).

Claim: agent=leg2b workspace=.jj-ws/fable-leg2b (LEG 2B: the checked-mint / rep-provenance capability - owns src/core/checker.f + type-family.f this session)
