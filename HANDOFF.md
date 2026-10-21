# Handoff: bootstrap the Intel port on ARM64 macOS

State: 2026-10-21. Herdr agent name: none (outside Herdr).

## Task

The user requested: “build and merge into master periodically, after rebasing
on top of master”. Work stopped before any build when the ARM64 donor
requirement became clear. The user will switch to their Mac and requested
this handoff committed and pushed to `intel`.

Continue on Apple Silicon: rebase onto current master, build and qualify a
macOS ARM64 product, then cross-build Linux x86-64. Native x86-64 execution,
self-hosting and the full suite must subsequently run on Linux x86-64.
Merge verified, independently reviewed checkpoints into master.

## State

- Origin: https://github.com/joelreymont/habu.git.
- Previous cwd: `/home/joel/Work/habu`, Ubuntu 26.04 x86-64.
- Published intel is the clean original port merge plus this replacement
  handoff. It has **not been rebased onto latest master, built or run**.
- Original merge: `40f36d55`, parents `5999ba5f` (master) and `4db1d989`
  (Intel checkpoint). Original handoff: `46c33a36`.
- Latest fetched master: `2d7b92c9`, “Rename source policy to vocabulary”.
  Refresh again on the Mac.
- The interrupted **local-only** rebase is preserved in the Linux repository
  under `intel-rebase-local` at `df6513d8` (change `sqsqruws`). Its merge is
  `9101c76d` (change `xwyrlqpu`), with four unresolved conflicts. The empty
  resolution child `d9232c35` (change `romlzqxr`) has no edits. These were
  not pushed; reproduce the rebase from the clean remote line on the Mac.
- The original handoff was consumed/deleted locally; this replacement is
  committed on clean intel for machine transfer, in change `nxxxkmrm`.
- Master was neither changed nor pushed. No implementation fixes were made.
- Linux `bin/hb` is an ignored Oct 7 x86-64 checkpoint with `hb.names`.
  It cannot host the current tree: old baked `lib/` paths conflict with
  `src/lib/` (earlier handoff reports duplicate E-A-FIRST, exit 78).
  No new engine, build log or gate result exists.

## Decisions

- The Gforth recovery seed emits ARM64 only. Bootstrap natively on the Mac,
  then cross-build the first Intel engine; Intel should subsequently self-host.
  Use a native arm64 shell, not Rosetta.
- A donor must match the host OS and architecture. An ARM64 Linux executable
  is not a macOS donor. Prefer a qualified installed macOS ARM64 hb, otherwise
  recover with Gforth 0.7.9 or newer.
- Preserve master's design and public API, re-expressing Intel capability on
  it. Libraries live under `src/lib/`.
- Current user instructions put private workspaces under `.jj-ws.noindex/`.
  Candidates stay private, outside `~/.local/lib/habu/`, until qualified.
- Build work remained stopped during handoff preparation. The user explicitly
  requested this file committed and pushed to intel.

## Findings

The attempted rebase command was:

```sh
jj rebase -s 40f36d55 -o master -o 4db1d989
```

This replaces the merge's master parent and retains its Intel checkpoint
parent. Do not replace both parents with master and assume the port is kept:
the merge's resolution diff is relative to its merged parent trees.

On master `2d7b92c9`, the four conflicts were:

- `docs/policy.md`: master renamed it to `docs/vocabulary.md`. Carry the
  Intel captured-interpreter/kernel enforcement documentation into the new
  file, keeping master's public vocabulary names.
- `src/lib/policy.f`: carry the correction that both kernels enforce admission
  into `src/lib/vocabulary.f`, keeping master's `VOCABULARY` API.
- `src/lib/policy-test.f`: preserve the Intel additions in
  `src/lib/vocabulary-test.f`: locals-many, design/ordinary-keyword package
  cases and captured-loop seam coverage. Inspect port-only harnesses under
  `test/policy/` too: their requires/calls may still name the old API.
- `src/habu/layout.f`: retain Intel's NCOMP-DISPATCH cells at $2D90, $2D98,
  $2DA0 and TASK-CHAIN-CELL at $2DA8, with master's renamed documentation
  references. The observed conflict surrounds the inserted block/comment.

Useful port comparison:

```sh
jj diff --from 5999ba5f --to 40f36d55 -- src/lib/policy.f src/lib/policy-test.f docs/policy.md
```

Prior handoff's known gaps, **not reverified in this session**:

1. x86 checker replay writers and source-exec observer still refuse.
2. `does-patch` does not refresh occurrence numbers for patched records/aliases;
   `test/does-empty-clause.f` expects old handles to refuse E-STALE.
3. Throws inside Habu `evaluate` do not retract master's eval-frame rows.
4. TARGET-CAPTURE does not copy ACAP-WHITEBOX? into ACAP-ALL-NAMES, so a
   whitebox cross-build may lose names.
5. x86 FFI float32 stubs are unexecuted. Bundle/incremental/cached-object paths
   have gaps, and some old Intel test coverage was lost during reconciliation.
6. `test/task-prepare-failure.f`'s page budget predates removal of the task
   altstack. Check its real refusal; do not raise budgets to hide failures.
7. Earlier suite on frozen `1f98c905`: 614 suites, 68 red; checkpoint
   xt-effect-test F34/F36 also red. Neither result qualifies the merged tree.
8. `intel-aot-wip@origin` (`97b4ddab`) retains unmerged AOT work with requested
   review corrections. Do not merge blindly.

The merged checker owner is $550 bytes / 170 cells; the task TCB is master's
$170 bytes. Checker/compiler loads, retained-host layout, fixed/defer paths,
boot/unit occurrence numbering, snapshots, pinned spills, multi-error recovery
and sealed-source routing all need execution. The full original merge inventory
and retained AOT context remain available:

```sh
jj file show -r 46c33a36 HANDOFF.md
jj file show -r 4db1d989 handoff.md
```

## Workers

Previous parent session ID: unknown. No active worker or task-owned build
process was observed when writing this file. The read-only `/root/arm_donor`
inventory agent was interrupted by a server restart and supplied no result.
No editing worker/workspace was created; do not restart the old workspace pool.

## Next

### 1. Fetch and rebase on the Mac

From the Mac's repository root, preserve existing work and choose a fresh
workspace name if the example already exists:

```sh
uname -s
uname -m
jj status
jj git fetch
jj bookmark list intel master --all-remotes
jj workspace list
jj workspace add .jj-ws.noindex/mac-intel-bootstrap --name mac-intel-bootstrap -r intel@origin
cd .jj-ws.noindex/mac-intel-bootstrap
jj rebase -s 40f36d55 -o master@origin -o 4db1d989
jj status
```

Expect Darwin and arm64. Resolve the known conflicts in a child of change
`xwyrlqpu`, snapshot with `jj describe`, and squash the resolution into that
merge; then build from the rebased Intel tip. Keep its descendants accounted
for. No conflicted tree is a build candidate.

### 2. Native ARM64 donor or Gforth recovery

Read [docs/bootstrap.md](docs/bootstrap.md) and
[docs/forth-card.md](docs/forth-card.md). Allocate external, disk-backed scratch:

```sh
mkdir -p "$HOME/.cache/tmp"
OUT=$(mktemp -d "$HOME/.cache/tmp/habu-intel-arm.XXXXXX")
mkdir -p "$OUT/tmp" "$OUT/arm" "$OUT/x64"
export HB_TMP="$OUT/tmp"
```

Inspect `~/.local/lib/habu/current/bin/hb` with `file` if present; use it as
HOST only if it is a working macOS ARM64 donor compatible with this tree.
Otherwise follow Gforth installation guidance in docs/bootstrap.md and probe
the selected interpreter (0.7.3 is too old):

```sh
GFORTH=$(command -v gforth)
printf ': x {: a :} a . ; 7 x bye\n' | "$GFORTH"
```

It must print 7 and exit zero. Recover privately, keeping all recovery gates:

```sh
HABU_ALLOW_BOOTSTRAP=1 HABU_TARGET=macos-aarch64 \
  HABU_BOOTSTRAP_CHECK_ONLY=1 GFORTH="$GFORTH" \
  HB_TMP="$OUT/recovery" tools/bootstrap.sh
```

Success ends `bootstrap check OK: .../hb-stdin`. Build the native product from
that source-only recovery host with the documented compiler preload:

```sh
HOST="$OUT/recovery/hb-stdin"
cat > "$OUT/native-preload.f" <<'EOF'
require src/compiler/native/compiler.f
NCOMP:INSTALL
EOF
HABU_UNDER_TEST="$HOST" HABU_FIXPOINT_ENGINE="$HOST" HB_TMP="$OUT/tmp" \
  "$HOST" --load "$OUT/native-preload.f" tools/native-build.f -- "$OUT/arm/hb"
```

If using an already qualified native product donor instead, set HOST to that
binary and use the ordinary command:

```sh
HABU_UNDER_TEST="$HOST" HABU_FIXPOINT_ENGINE="$HOST" HB_TMP="$OUT/tmp" \
  "$HOST" --load tools/native-build.f -- "$OUT/arm/hb"
```

Recovery is not product qualification. Run the documented generation chain:

```sh
HB_TMP="$OUT/tmp" "$OUT/arm/hb" --load tools/two-generation-build.f -- "$OUT/arm/hb"
```

It prints its private directory and leaves bin/hb alone. Copy the converged
product and its **matching .map** into this private tree as regular bin/hb and
bin/hb.map files, checking for symlinks first. Read docs/gate.md for the cold ARM
prefix check and suite requirements, then run its macOS awake wrapper:

```sh
sh -c 'caffeinate -is -w $$ & bin/hb --load test/run.f </dev/null'
```

Documented Mac suite dependencies include Homebrew libzip, libunistring,
openssl@3, libpq, socat, librsvg, qpdf, Poppler and PostgreSQL tools on PATH.
Inspect installed dependencies first. Retain genuine failures and logs; do
not weaken the gate. Rule out host sleep before diagnosing timeouts.

### 3. Cross-build Linux x86-64, then qualify it on Linux

Using the qualified private ARM product from the same source:

```sh
HB_TMP="$OUT/tmp" bin/hb --load src/arch/x86-64/passes.f tools/native-build.f \
  -- "$OUT/x64/hb" --target linux-x86-64
file "$OUT/x64/hb"
shasum -a 256 "$OUT/x64/hb" "$OUT/x64/hb.map"
jj log -r @ --no-graph
```

Record the exact source commit, hashes and build/gate logs. The driver has a
held x86 writer and skips launch-smoke for a foreign architecture
(FILE-PREPARE and IMAGE-INSTALL, tools/native-build-core.f). A macOS-to-Linux
x86-64 cross-build was **not tested here**; diagnose host/target ABI or image
format refusals at the responsible boundary.

Transfer the binary, its matching .map and exact source revision to Linux
x86-64. In a private matching tree, copy the pair as regular bin/hb and
bin/hb.map; run tools/two-generation-build.f using the Intel candidate, then
the full `bin/hb --load test/run.f </dev/null` on the converged Intel product.
Do not substitute the old checkpoint .names sidecar for the new .map.

Fix confirmed failures at the responsible layer. Obtain the independent
adversarial reviews required by current AGENTS.md before landing significant
changes. Refresh/rebase onto master again if it advances, merge verified
checkpoints periodically, and read each gate's exit status before a separate
push command.

## Pointers

- [docs/bootstrap.md](docs/bootstrap.md): recovery, preload, older-host staging,
  generation chain and reserved layout transitions.
- [docs/gate.md](docs/gate.md): product qualification, macOS dependencies.
- [docs/x86-64.md](docs/x86-64.md): target design and build commands.
- [INTEL.md](INTEL.md): lane overview; parts predate the merge.
- [docs/debugging.md](docs/debugging.md): native diagnosis.
- Full previous handoff: `jj file show -r 46c33a36 HANDOFF.md`.
- Linux scratch `/home/joel/.cache/tmp/habu-intel-build` was empty. There are no
  new artifacts to transfer. Old .jj-ws/ workspaces are unrelated history.
- Codex transcript path: unknown.

Start a new Codex session on the Mac and use
`$pickup /absolute/mac/path/to/habu/HANDOFF.md`.
