# Habu maintainer handoff

Cedar resumed integration ownership on 2026-09-13. The current architecture,
completion criteria and dispatch order are in [PLAN.md](../PLAN.md), with tasks
under `habu-compile-the-tender-8385810f`. The integration workspace remains
`.jj-ws/rowan-root`; the default checkout contains unrelated historical work.
Hazel handed back source `5226a994` and engine `28e11361`; pending tier, combine,
capacity and stage stacks are identified in the plan. No replacement compiler
or improved all-AOT timing is claimed by this documentation update.

The notes below preserve the 2026-09-12 handoff and subsequent investigation.
Their former ownership, checkpoints and ordering are historical. In particular,
refreshing the ancient stdin seed is not a prerequisite to using an existing
native optimizer for the current selfbuild. Use the current plan and dot graph
for work assignments and dependencies.

## Identities and channels

Agents talk through Herdr session messaging; the `.blackboard` board and the
`bb` tool were retired on 2026-09-18. Names in use and never to be reused:
cedar, rowan, tender, tender-build, kestrel, maki, astra (cedar's reviewers).
Pick a fresh name (see CLAUDE.md, Session start in Herdr).

## Roots and bookmarks

- Integration root: the tip of workspace `.jj-ws/rowan-root`
  (`jj -R .jj-ws/rowan-root log -r @-`); the workspace keeps its name. On
  2026-09-12 morning it is 04701ef9, lineage a04dd347 → bb7bb027 → 04701ef9.
  Cedar's last independently reviewed root is 5568d9c0.
- The root advances linearly. Whoever lands on it tells the owners who wait on
  it. No force, no rewriting of landed commits, no `master` move and
  no `jj git push` except by hazel at a coherent point (master 2057bcbd and
  origin are far behind).
- The tracker lives in `.dots/` on the root; `habu-trusted-dies-prim-4fd12d60`
  is the compiler epic, `habu-compile-the-tender-8385810f` the 1.7 s campaign.

## Ownership boundary

Hazel owns all of Habu, including the campaign
`habu-compile-the-tender-8385810f` and every leaf under it, the compiler
epic `habu-trusted-dies-prim-4fd12d60`, the engine, core, compiler, library,
tools, tests and documents. Rowan owns Maki and odin and files Habu asks on
`#general`; tender-b3 asks for scoring slots there and posts start and end.

The campaign order from Joel (2026-09-12): the suite runner
(habu-run-every-registered-56d4962d), the strict parametric suites
(habu-integrate-strict-parametric-c39bbc70), image byte reproducibility
(habu-make-the-engine-9db99082), then the tier stack landing
(habu-land-the-tier-5ad0a198), the floor lanes (habu-reuse-one-compile-5ff96de6,
habu-make-register-allocation-d7ba9e30, habu-make-spill-rewrite-ca192310,
habu-bring-the-trivial-*), the Tender attribution round
(habu-attribute-the-tender-*), and the local-binding case rule
(habu-bind-locals-by-a16875d6) after the tier stack. Not in scope: PTY
follow-ups, profiler tooling asks, docs-only items unless a core item needs a
line. PTX and model CAD are not Habu's:
on 2026-09-11 Joel had lib/ptx, src/arch/ptx, the PTX lint, the PTX and
model-CAD documents and the Loom-only tracker entries moved into ../loom unchanged;
fixing any PTX or model-CAD code in Habu is prohibited, and Loom's agent wires
the moved code into Loom. Every landing on the root follows an independent
review and is announced with the tip; an engine change also rebuilds the root's
`bin/hb` cold and runs `tools/two-generation-build.f` when it moves persisted
DATA.

Independent review is mandatory before any landing: a read-only reviewer
agent given the specification and the diff, never the author's narrative.

## Lanes in flight, 2026-09-12 afternoon (state on disk)

Landed today after the runner: create-alignment (04701ef9), path capacity
(f5885b3b), the strict parametric migration (62f5dbb3), two build-fixture
fixes (9d7bb928), the compiled-tick internal-word gate and nine record/effect
suites (f3e7f800, d88e3efd), diagnose-hb retired (7aebfda6), certify learns
DYNAMIC-BUFFER (4145a44f, 15d51e38), byte-identical builds (a611d84d), the
register-allocation lane (7f49cf17), seven remaining suites (41d1d6c7). The
recorded red set is LESSONS.md's last entry; re-measure on a quiet box after
each engine rebuild.

Opus workers implement in isolated lanes; hazel reads every diff before
landing. Lanes cut by the API session limit on 2026-09-12 13:30 UTC keep their
on-disk state and are relaunched from it:

- `.jj-ws/rowan-tier` (tier stack, 23 commits rebased onto f0a26624): landing
  blocker found by its worker: with the stack's tier-1 selection in
  tools/native-build.f a product engine cannot rebuild the tree (tier 1 cannot
  resolve the package-private CHECK-RC in src/core/check-hook.f's PREFLIGHT),
  and two-generation fails at generation 1. The fix goes in the compiler's
  private-name resolution, never in the selection.
- `.jj-ws/rowan-reuse` (session-compile stack): the review found a blocker
  (SESSION-OPEN under an open context retires the wrong slot: use-after-free
  and a wedged process) and a non-self-green commit; a worker was folding the
  fixes (finding list on the board, 2026-09-12 09:2x UTC) when cut.
- `.jj-ws/rowan-combine` (spill/combine, seven commits on f0a26624): the
  NPROF accumulator must move out of DATA cells (byte identity); a worker was
  doing that and adding the E-A64COMB-PLAN negative when cut. F-NEED-FILL is
  O(blocks^2) (spill.f:947), a follow-up.
- `.jj-ws/habu-green-the-aot-d30c5a39` (AOT gate family): worker cut while
  duplicating the certify fix that 15d51e38 landed; rebase and resume.
- `.jj-ws/habu-rebuild-the-node-73ff3eff` (intern index + verify-window seal):
  worker cut mid-evidence.
- `.jj-ws/habu-recover-stage0` (recovery bootstrap): four commits ready (seven
  stage0 primitives, the seed's prefix mirror, a fail-open crash made a
  refusal, nf.fs paths); the chain now stops at hb-stage rc 82
  (habu-let-a-stage2-6744d545).
- `.jj-ws/habu-size-the-snapshot-1ca5db10` (Tender's address table): built,
  cannot be captured by any host until habu-classify-captured-addr-68fcc1df.
- `.jj-ws/rowan-arena`, `rowan-pflayout`, `rowan-selfbuild`, `rowan-verify`,
  `rowan-checker`, `rowan-floor`, `rowan-jit-nest`: earlier lanes; retire once
  their work is on the root.

## Rules that cost us a day when broken

- Never poll with `pgrep -f <literal>` from an agent shell: the shell's own
  command line contains the literal. Use the background job's completion
  notification, its PID, or an anchored pattern (`pgrep -x hb`).
- Timing numbers only when the 1-minute load average is under 4 and no other
  `hb` is above 50% CPU; prefix every timing line with the load. The box runs
  a dozen lanes.
- Measure Tender only from a frozen copy (`.jj-ws/rowan-hash/tmp/tender-snap`
  is a pinned export); `~/Work/Tender` is edited live and is 175 GB.
- The cold engine build: `HB_TMP=$PWD/build/tmp
  /tmp/cedar-crossing-realpath/hb-stdin --load tools/native-build.f </dev/null`
  from the workspace, about 20 s. Source replay of compiler.f is a no-op on
  a baked engine; every compiler change needs this rebuild.
- Every executable is built by the optimizer; the JIT is for `--load` and the
  REPL only. The shipped compiler is still JIT code until the selfbuild
  lands; every optimizer timing is a JIT-compiled optimizer measuring itself.

## Known pre-existing failures on the root

- `test/run.f` on the root before the runner fix ran 151 of 307 registered
  suites and showed ten reds: check-cli-boundary, compiler-ir-id,
  compiler-ir-id-manifest, compiler-ir-id-proof,
  compiler-ir-structure-manifest, compiler-ir-structure-proof,
  compiler-asm-package, compiler-native-elaborate,
  compiler-codegen-tail-probe, app-image; tool-boundary-aot-call is flaky
  under load. The complete red set is recorded in LESSONS.md once the runner
  runs every suite (habu-run-every-registered-56d4962d). engine-suite has two
  pre-existing reds F308/F309 (habu-fix-the-two-284ac502).
- Self-refresh (`bin/hb` rebuilding its own tree) fails at capture with
  `aot-capture: call target has no dictionary record`; 0c3099ea is the fix
  candidate.
- `tools/xref-test.f:33` and `test/drec-shape-test.f:18-28` still declare
  `ptr a` after the hide.f migration; `test/gate-dictionary.f` dies
  `E-TRUST-UNRESOLVED` on `USIGS-RESTORE-END`.
- A script reading an argument it was not given segfaults; dotted. The
  over-wide local name defect (unbounded copy into LOC-REC) is fixed on the
  root by e259542a (hazel); engine and checker now refuse it by name.
