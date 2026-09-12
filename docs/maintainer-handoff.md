# Habu maintainer handoff (2026-09-12)

Cedar, the Habu maintainer, is unavailable for a week. As of 2026-09-12 morning
hazel owns all of Habu: design, lanes, review, landing, the tracker, this file
and pushes of `master` at coherent points. Rowan moved to Maki and odin and
files Habu asks on the board. This file is the shared state; update it whenever
ownership or the root changes.

## Identities and channels

The blackboard is `.blackboard` in this repo (`bb`, identity `BB_AGENT`).
Names in use and never to be reused: cedar, rowan, tender, tender-build,
kestrel, maki, astra (cedar's reviewers). Pick a fresh name and announce it.

## Roots and bookmarks

- Integration root: the tip of workspace `.jj-ws/rowan-root`
  (`jj -R .jj-ws/rowan-root log -r @-`); the workspace keeps its name. On
  2026-09-12 morning it is 04701ef9, lineage a04dd347 → bb7bb027 → 04701ef9.
  Cedar's last independently reviewed root is 5568d9c0.
- The root advances linearly. Whoever lands on it announces the new tip on
  the board. No force, no rewriting of landed commits, no `master` move and
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

## Lanes handed over by rowan, 2026-09-12 morning (state on disk)

- `.jj-ws/rowan-reuse` (habu-reuse-one-compile-5ff96de6): the session-compile
  stack, tip 07bd24c1 over 79098f51 on fc23ff28, ready for review and
  landing; rowan's report (board, 03:14 UTC) lists six loose ends to fold or
  dot first: move the native-session opcode case one commit up, squash the
  vocabulary commit with the memo commit so the stack is monotone, bound the
  session tenancy of arena slots, the O(rows) IR-SYM:INTERN scan,
  INTRINSIC-BOUND? at 375 us per call, and the LESSONS.md interner entry.
- `.jj-ws/rowan-tier` (habu-land-the-tier-5ad0a198): the 19-commit tier stack
  mid-rebase onto a04dd347; the working copy is "Subtract the written interval
  from the span table" with uncommitted edits (habu1.f, habu2.f, test/tier.f,
  LESSONS.md); `jj op log` there shows the sequence. Verify the exit statuses
  renumbered off 95/96 and the span-full agreement check; the inliner traces
  were open.
- `.jj-ws/rowan-alloc` (habu-make-register-allocation-d7ba9e30): uncommitted
  on 495dea80: `tools/compile-scaling.f` (the yardstick; its MEASURED ON table
  may be pending) and `src/compiler/native/regalloc.f`; `build/` holds hb-base,
  hb-d and codeprobe logs. Commit the yardstick first, then the scan removals.
- `.jj-ws/rowan-combine` (habu-make-spill-rewrite-ca192310): uncommitted on
  495dea80 across combine.f, spill.f, compiler.f, a prof accumulator,
  `tools/chain-scale.f` and lib/errors.f; `probe/` and `tmp/` hold its logs.
  Keep at most one accumulator; commit per removed cost with the floor line.
- `.jj-ws/rowan-arena`, `rowan-pflayout`, `rowan-selfbuild`, `rowan-verify`,
  `rowan-checker`, `rowan-floor`, `rowan-jit-nest`: earlier lanes, see the
  tracker for what landed; retire a workspace once its work is on the root.

Cedar's workspaces `.jj-ws/cedar-*` are reference material; do not edit them.

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
