# Intel checkpoint and Windows pickup

Checkpoint: 2026-10-07. This is a development checkpoint; the full native gate
is still red and the Intel port is unfinished.

## Task

The user's latest request was: "finish work to the next checkpoint, then
commit and push everything to the intel branch. include a handoff.md to
describe what's next". Earlier requests authorized reviewing the repository,
branches and jj workspaces, finishing the Intel port, and refreshing Dotfiles
rules and skills from GitHub.

The destination is native Windows Habu for SolidWorks automation. The user
explicitly accepted WSL initially and wants to continue on their Windows
machine rather than carry this Linux machine. WSL is the next development
host; native Windows and COM remain subsequent work. Do not start ARM builds
while Intel's required qualification remains unfinished.

Completed: native Intel self-build and three-generation byte convergence;
reviewed source diagnostics and multi-error recovery integration; seeded FIELD
reader repair; five native pre-trust fixture entries and their registration;
remote preservation of the unfinished upstream merge and portable AOT work.
Finish Intel qualification before claiming the port or a release complete.

## State

Previous session cwd: `/home/joel/Work/habu` on x86-64 Arch Linux.
Previous parent session ID: `01a10744-2625-7471-b7b6-d49ff84cf229`.

Herdr agent name: none. Identity could not be admitted: running
`python3 ~/.codex/codex-herdr-identity.py identity` exits 1; the transcript
has `source: vscode`, and the inherited pane lacks `agent_session` metadata.
No pane was renamed or controlled. A new session must verify its own identity;
do not restore a guessed name.

| Ref | Revision | Meaning |
| --- | --- | --- |
| `intel` | Commit containing this handoff, following `b16066cd56f2` | Development checkpoint and committed handoff |
| `intel-checkpoint-2026-10-07` tag | `b16066cd56f233a6ea3fceec47d8e2709f9fa38e` | Frozen source for the draft bootstrap assets |
| `intel-upstream-wip` | `17a63cf9acfaf1dc5b908ceb124d3990d808e461` | Unqualified merge of `1f98c905` and `50d9f706` |
| `intel-aot-wip` | `97b4ddabde18` | Unqualified portable graph/storage restoration |
| `archive/intel-20261007` | `3e502d32b932` | Previous Intel tip, preserved before the sideways bookmark move |
| `master` | `50d9f706` at checkpoint | Left unchanged by this work |

The runtime/compiler source at `b16066cd` is identical to reviewed integration
`079e48b65534`; its additional changes are pre-trust tests and registration.
The handoff commit changes only this file and `docs/branch-work.md`.
Use `jj`, not Git commands, and push further work to `intel` unless the user
changes the target. Preserve unrelated bookmarks/workspaces.

Dotfiles was refreshed from GitHub to `50a03458` ("Forbid shortcuts, hacks and
workarounds outright"). Local Dotfiles change `c504a6bc` preserves the user's
Codex and Git configuration. Read current rules on the destination machine;
apply platform rules only to the actual runtime.

The default checkout's ignored `bin/hb` was not promoted. The tested candidate
is installed only in `.jj-ws/root-intel-checkpoint-oct7/bin/hb`. Its sources
are frozen at the checkpoint. The prior baseline remains in
`.jj-ws/root-intel-next-oct7`. Neither ignored binary travels with a clone;
use the draft assets below. No builds or test jobs remain running.

## Decisions

- The user chose a checkpoint and handoff, rather than waiting for the entire
  Intel port or the upstream merge. Commit `handoff.md` as explicitly requested,
  overriding the handoff skill's usual uncommitted-file convention.
- Keep the tested checkpoint on `intel`. Both unfinished lines are pushed as
  the named Intel WIP refs above so their work is available on another machine;
  they are not integrated or approved for landing.
- Keep `master` unchanged. The old Intel tip was not an ancestor of the new
  checkpoint or current master, so retain its archive rather than lose it.
- No ARM, Spark SSH, macOS, WSL, or native Windows execution was performed for
  this checkpoint. Spark is available later; avoid repeating authentication
  attempts before an actual ARM check is needed.
- Windows needs its own ABI/runtime/image work. Keep SolidWorks-specific
  application policy outside the Habu language core. Existing portability
  requirements live in `docs/portability.md`; Windows/COM design [WN] is absent
  and the Windows lane remains required but unqualified.

## Findings and checks

The fresh combined engine built three times through `tools/native-build.f`.
All builds exited 0. Actual `cmp` comparisons show all three executables and
all three names sidecars are byte-identical. The final artifacts are:

```text
hb SHA-256:    4ee71d2a196148fd6cc1719cd881d007d5bd284b91e28947831d25f3b5cba39e
names SHA-256: d46cf302c76831e4c1bb922459f6c35d913ef543d304f657a071dd5af12aaf82
```

On the combined candidate, these real load paths exited 0:
`test/native-multi-error-recovery.f`, `test/using-test.f`,
`test/program-diagnostics-test.f`, `test/address-cell-tasks.f`, and
`test/pre-trust-defer-table.f`. The diagnostic suite built a matching whitebox
engine and exercised replacement-checker source. Stdin `41 1+ . cr` printed
`42` on generation 3. Independent Astra follow-up approved the owner correction:
119 payload cells / `$3B8`, refusal-token `$3A8`, recovery-used `$3B0`.

All five pre-trust entry paths passed in the fixture worker; source review
approved `66d1c34b`, and all five are now registered as product `SUITE` rows.
The seal/backstop runs measured 220.952/212.948 CPU seconds under concurrent
builds. They are below the existing 360-second hard budget but above the
180-second target; quiet timing remains unqualified. Do not raise budgets to
hide this. The table path additionally passed on the final combined source.

The completed full native suite was on the prior frozen `1f98c905` baseline,
not the final integration: **614/614 suites, 68 red, exit 1**. Breakdown:
55 exit failures, 7 CPU-budget failures, 6 timeouts under load. The log is a
draft asset, `full-native-baseline.log`. It includes every final `RED:` row and
failure output. The full suite has not been rerun on this final checkpoint.
Some baseline failures exercise ARM-specific premises; classification and
portable coverage repairs remain on `intel-aot-wip`. Other failures require
real investigation. Direct `test/xt-effect-test.f` also remains red on the
combined candidate: F34 expects 70 but receives 102, and F36 expects false but
receives true. Do not describe all reds as fixture or timing issues.

### Retained AOT work

`intel-aot-wip` includes graph change `efc726736c49` and storage change
`97b4ddabde18`, atop `2286ec35`. It extracts shared SIGSTR staging and restores
portable graph/storage cases. Intel valid metadata import passed with 42 rows;
the registered scalar group passed, as did direct and registered storage,
including 1,536 checker-produced rows and 847,893 pool bytes.

Independent Astra review requested two corrections before integration:

1. `test/aot-graph-width-child.f:178`, `GW-RETIRE-SOURCE`, must also erase the
   rolled-back type-registry records with `TFAM:ERASE-REGISTRY-DELTA`. Clearing
   USIGS alone leaves stale source records that can rescue a broken importer.
2. `test/aot-graph-width-child.f:219`, `GW-ACCEPT`, must retain the portable
   native compile/execute control after source destruction/import:
   `GRAPH-NATIVE-USE` applies `[: 1+ ;] PAYLOAD-QUANT`, mapping 17 to 18.
   Checker verdicts alone do not cover native consumption.

Six groups are unrun: `composite`, `structure-a`, `structure-b`, `structure-c`,
`exception`, `source`. Run their existing entry groups after correcting the
findings, then request focused independent review. The full reviewed range is
`051e94eb..97b4ddab` (11 paths), not only the last worker commit. ARM execution
is deferred.

### Retained upstream merge

`intel-upstream-wip` has no conflicts but no working merged engine. Existing
Intel hosts lack `DNAME-OWNED` and `NCOMP_DISPATCH_DECL_CALL_BINDING`. A mixed
source stage failed at the checker/runtime boundary. Old owner offset `$328`
means `EFFECT-RIN-N`; the merge reuses it for `CALL-BINDING`. Declaration-only
compatibility aliases are unsafe. A real staged runtime/checker bridge is
required. This merge predates the final refusal-token/recovery integration;
reconcile those owner fields as part of future integration.

Independent Astra source review found five production defects:

1. `src/habu/boot-x64.f:252`: `OCC-INIT,` allocates zeroed occurrence storage but
   does not initialize IDs for existing linked/snapshot dictionary records.
   Fresh baked-word selection therefore refuses `E-SELECT`.
2. `src/habu/kernel-x64.f:5167`: all six replay writers use refusal stubs.
   Implement actual replay semantics, namespace high-water tracking and writer
   overlay guards. Verify `engine-writers.f`, `replay-binding.f`, checker scopes.
3. `src/habu/kernel-x64.f:5071`: `does-patch` does not replace occurrence IDs
   for patched records and affected aliases. Existing `does-empty-clause.f:183`
   requires pre-patch handles to refuse `E-STALE`.
4. `src/habu/kernel-x64.f:2924` and `src/habu/habu2.f:5084`: native-unit and
   ARM checked-cast publication bypass occurrence-aware append. Newly published
   records therefore lack selectable IDs. Correct both paths; defer ARM runs.
5. `src/core/checker.f:13153`: `EFFECT-QUOT-CALLABLE?` incorrectly aliases the
   stricter return-neutral predicate. Restore Intel's matching-return-tail
   admission; existing `native-elaborate.f:2538` and `native-return-abi.f` cover it.

These findings belong to the retained merge, not the tested checkpoint.
After correction and a genuine bootstrap, require focused behavior checks,
independent follow-up review, the full native suite and byte convergence.

## Workers

All native in-process workers are completed; no continuation channel or receipt
is needed. New root-scoped agent tools cannot be assumed to address them.
Do not restart the old agent pool. Relevant completed assignments were:

| Worker | Result |
| --- | --- |
| `/root/recovery_fix_oct7` | `ebf78334`, integrated; private fallback during recovery and after expiry pass |
| `/root/recovery_review_oct7` | Approved final `079e48b6` initializer correction |
| `/root/pretrust_pickup_oct7` | `66d1c34b`, integrated and registered; five paths pass, timing limit above |
| `/root/native_error_review_oct7` | Approved diagnostic/pre-trust changes; requested the two AOT corrections above |
| `/root/aot_graph_fix_oct7` and its `/sigstr_storage_oct7` child | Frozen `97b4ddab`, retained; six graph groups unrun |
| `/root/upstream_pickup_oct7` | Frozen `17a63cf9`, retained; bootstrap blocked at ABI/runtime boundary |
| `/root/upstream_review_oct7` | Finished source review; five upstream defects above |

Task workspaces for the two unintegrated lines are retained with their refs.
The previous broader workspace census is historical evidence, not a license
to delete unrelated or unaccounted work. Cleanup follows `AGENTS.md`; compare
content/ancestry rather than subjects. Existing unrelated archives remain.

## Next

Start a genuinely new session with `$pickup` and the absolute path to this file.
On the current host that command is:

```text
$pickup /home/joel/Work/habu/handoff.md
```

For a fresh **x86-64 WSL2** checkout, run the following inside WSL. These need
`jj` and an authenticated `gh` for the repository owner; draft assets require
authentication. Keep the checkout on the WSL Linux filesystem. These steps
install an unqualified development candidate in an isolated workspace, not the
main checkout's product slot:

```sh
mkdir -p "$HOME/Work"
cd "$HOME/Work"
jj git clone --branch intel --branch intel-aot-wip --branch intel-upstream-wip \
  https://github.com/joelreymont/habu.git habu
cd habu
jj workspace add --name intel-pickup -r intel@origin .jj-ws/intel-pickup
cd .jj-ws/intel-pickup
mkdir -p bin build/tmp
gh release download intel-checkpoint-2026-10-07 --repo joelreymont/habu \
  --pattern 'hb-linux-x86_64*' --dir bin
mv bin/hb-linux-x86_64 bin/hb
mv bin/hb-linux-x86_64.names bin/hb.names
chmod +x bin/hb
sha256sum bin/hb bin/hb.names
printf '41 1+ . cr\n' | bin/hb
HB_TMP="$PWD/build/tmp" bin/hb --load tools/native-build.f -- build/hb-native
cmp bin/hb build/hb-native
cmp bin/hb.names build/hb-native.names
realpath ../../handoff.md
```

Send `$pickup` followed by the absolute path printed by the last command.
WSL execution is not yet verified. Record the actual host/build result before
extending claims. If resuming on Linux instead, use the existing frozen
candidate workspace and cache products; do not repeat already passed checks
without a source/host change or unresolved concern.

The first source task is correcting the two AOT coverage findings and running
the six remaining groups on Intel, then independently reviewing the corrections
and integrating their qualified result. Continue triaging the actual full-gate
reds through real load paths. Keep the large upstream merge separate until its
five defects and bootstrap boundary are fixed. Full-suite success and converged
engine/names are still required before calling Intel complete or promoting the
product. Later native Windows work needs PE/COFF, Windows host services and
x64 foreign calls/callbacks; generic COM support enables a separate SolidWorks
client. No Windows design or implementation was started here.

## Pointers

- [Draft bootstrap assets](https://github.com/joelreymont/habu/releases/tag/untagged-a09988f29e6f62ade906),
  tag `intel-checkpoint-2026-10-07`: executable, matching names,
  `checkpoint-checks.txt`, and `full-native-baseline.log`. Asset upload digests
  match the hashes above; draft download by tag was exercised successfully.
- Read `AGENTS.md`, `docs/forth-card.md`, `docs/bootstrap.md`, `docs/gate.md`,
  `docs/debugging.md`, `INTEL.md`, `docs/branch-work.md`, `docs/portability.md`.
- Local products/logs: `/home/joel/.cache/habu/intel-combined-oct7/`; prior
  baseline `/home/joel/.cache/habu/intel-oct7/`; uploaded copies
  `/home/joel/.cache/habu/intel-checkpoint-2026-10-07/`.
- Pre-trust evidence: `/home/joel/.cache/tmp/habu-pretrust-*-oct7.{log,time}`.
  Previous workspace census:
  `/home/joel/.cache/tmp/habu-cleanup-census-3acb4566.json` and
  `/home/joel/.cache/tmp/habu-cleanup-hunks-3acb4566.json`. Preserve referenced
  scratch files; they are not present in a fresh clone.
- Exact previous transcript:
  `/home/joel/.codex/sessions/2026/10/04/rollout-2026-10-04T17-14-29-01a10744-2625-7471-b7b6-d49ff84cf229.jsonl`.
  Search only for a needed fact; do not export or read it wholesale.
- Durable Windows references:
  [WSL2](https://learn.microsoft.com/en-us/windows/wsl/about),
  [Microsoft x64 ABI](https://learn.microsoft.com/en-us/cpp/build/x64-calling-convention?view=msvc-170),
  [SolidWorks COM interfaces](https://help.solidworks.com/2026/english/api/sldworksapiprogguide/Overview/COM_vs_Dispatch.htm).
