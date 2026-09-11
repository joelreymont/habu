# Habu maintainer handoff (2026-09-11)

Cedar, the Habu maintainer, is unavailable for a week. As of 2026-09-11 evening
rowan holds the integration root and the 1.7 s optimizer campaign. A second
maintainer takes the broader backlog. This file is the shared state; update it
whenever ownership or the root changes.

## Identities and channels

The blackboard is `.blackboard` in this repo (`bb`, identity `BB_AGENT`).
Names in use and never to be reused: cedar, rowan, tender, tender-build,
kestrel, maki, astra (cedar's reviewers). Pick a fresh name and announce it.

## Roots and bookmarks

- Integration root: the tip of workspace `.jj-ws/rowan-root`
  (`jj -R .jj-ws/rowan-root log -r @-`). On 2026-09-11 evening it is
  708aaf31 on 0c3099ea "Resolve prefix reset boundaries from the running
  dictionary", lineage 41df9051 → c0bd71d4 → 5568d9c0 → 0c3099ea. Cedar's
  last independently reviewed root is 5568d9c0; 0c3099ea (warm-capture
  reset) is under rowan's review.
- The root advances linearly. Whoever lands on it announces the new tip on
  the board. No force, no rewriting of landed commits, no `master` move and
  no `jj git push` except by rowan at a coherent point (master 2057bcbd and
  origin are far behind; cedar never pushed today's work).
- The tracker lives in `.dots/` on the root; `habu-trusted-dies-prim-4fd12d60`
  is the compiler epic, `habu-compile-the-tender-8385810f` the 1.7 s campaign.

## Ownership boundary

Rowan owns, and integrates through `rowan-root`:

- the campaign `habu-compile-the-tender-8385810f` and every leaf under it;
- the optimizing selfbuild (`habu-build-the-compiler-c348eab0`) and the
  tier/provenance/save-guard stack (`.jj-ws/rowan-tier`, `rowan-jit-nest`);
- these files, which the campaign lanes edit: `src/compiler/**`,
  `src/habu/{habu1,habu2,layout,jit,snap-lib,stdin,maker,native-runtime}.f`,
  `src/core/{checker,engine-error,type-family}.f`,
  `tools/{native-build,build-fixpoint,hb-build-lib,aot-chain-capture,compile-floor}.f`,
  `test/compiler/**`, `test/tier.f`, `docs/compiler-ir-design.md`,
  `docs/bootstrap.md`, `formal/Common/Interning.v`.

The second maintainer owns everything else: the remaining leaves of the
compiler epic that are not campaign leaves (profiler crash, local-name
diagnostic, argv bounds follow-ups, hide.f, build and simplification work),
the V2 types and PTX programs, lib/, tools/ outside the list above, tests
outside `test/compiler`, docs outside the two named. It may land directly on
the root after an independent review agent has cleared the stack, announcing
the tip; rowan rebases the campaign lanes onto it. A change that must touch a
rowan-owned file is handed to rowan as a commit id with its evidence.

Independent review is mandatory before any landing: a read-only reviewer
agent given the specification and the diff, never the author's narrative.

## Lanes in flight (rowan), 2026-09-11 evening

- `.jj-ws/rowan-arena`: reader stack, 11 commits (a62274eb … f41cbaa9) on
  41df9051; commit 1 is on the root as 035bfb4c; the rest rebase onto the
  root after a quiet-machine measurement.
- `.jj-ws/rowan-reuse`: per-session compile context (b76b2b6c … ac5bd9f2 on
  0c3099ea) and the session re-key with a prototype-clone symbol table
  (symbol.f/build.f granted additively; elaborate/select/loop/regalloc
  binding sections granted).
- `.jj-ws/rowan-pflayout`: the selfbuild blocker, an unused synthesized
  frame lane in CASE regions; spill.f-only fix deriving lane need from
  reachable consumers.
- `.jj-ws/rowan-selfbuild`: reconstructing cedar's optimizing selfbuild
  procedure from `/tmp/cedar-current-*` and `.jj-ws/cedar-aot-selfbuild`.
- `.jj-ws/rowan-verify`: regalloc-verify.f made linear (FLOW-CK bit-parallel,
  single-block tokens skip the sweep), on 80716458, rebases onto the root.
- `.jj-ws/rowan-checker`: dict.f walks once (3b75f5b1) and a record-index
  primitive `search-wl-rec` blocked on a seed that carries it.
- `.jj-ws/rowan-floor`: `tools/compile-floor.f`, the campaign yardstick
  (4e266203 + f42ac0c5 on the root).
- `.jj-ws/rowan-tier` and `rowan-jit-nest`: tier stack on 604bf55d awaiting
  one rebase onto the root once nested quotations land.

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

- `test/run.f`: 29 red (compiler-ir-id family on `TFAM:TFAM-PKG$`,
  compiler-native-* codegen address assertions, check-cli-boundary).
- Self-refresh (`bin/hb` rebuilding its own tree) fails at capture with
  `aot-capture: call target has no dictionary record`; 0c3099ea is the fix
  candidate.
- `tools/xref-test.f:33` and `test/drec-shape-test.f:18-28` still declare
  `ptr a` after the hide.f migration; `test/gate-dictionary.f` dies
  `E-TRUST-UNRESOLVED` on `USIGS-RESTORE-END`.
- The sampling profiler (`prof-on`) dies intermittently on long tier-1 loads
  (SIGSEGV in the SIGALRM handler); dotted.
- A local name over 16 bytes is reported as an undefined word (LOC-NAME-W);
  a script reading an argument it was not given segfaults; both dotted.
