# Cedar restart — 2026-09-14

Latest user instruction: correctness and missing/wrong compiler behavior first;
performance later. User then requested review of `/tmp/habu-audit-2026-09-14.md`
and preparation for a Herdr restart. Do not call the compiler finished.

## Resume here

- Workspace: `/home/joel/Work/habu/.jj-ws/cedar-closure-identity`.
- Bookmark: `cedar/compiler-integration`, GitHub `joelreymont/habu`.
- Last code commit before this checkpoint: `2195659e` (native snapshot route).
  Prior: `9932a2de` (seed bounds/prefix fixture), `1dfebae6` (pure infinite loops),
  `e0fad783` (captured compiler provide rows), `9d0a44a3` (dispatch restoration).
- `bin/hb` now points to a durable local copy:
  `bin/checkpoints/hb-correctness-46921f52`, SHA-256
  `46921f5299b27048cb4f736539168c170a00d90d498f3a715cbd7d048495e15b`.
  It includes terminal-order and negative seed-index repairs, but predates later
  source edits. Rebuild after the next compiler changes; do not mistake it for
  a binary of the bookmark's exact final source.
- Frozen app acceptance pair: `.jj-ws/cedar-compiler-acceptance`, source
  `9d0a44a3`, binary SHA
  `24d8075c4ac640df811daf736cec2ffaba7176471753d9a5d27ff031f31148c9`.
  Do not edit that workspace or change application pins without acceptance.
- All three spawned agents stopped cleanly. No build or BB watcher remains live.
  Read BB as Cedar, then start one managed `bb watch -t 0` session:
  `BB_ROOT=/home/joel/Work/habu BB_AGENT=cedar bb unread --ack`.
  Skill: `/home/joel/.codex/skills/bb/SKILL.md`.

## Audit findings — priority

Audit pin is old `c3e1b024` with a September10 engine. Its totals and dispatch/gate
claims are not the integration baseline. Original probes/reports are under:
`/tmp/claude-1001/-home-joel-Work-habu/3954d386-699e-4c6e-b454-0300ac86489f/scratchpad/`.
Independent Astra checked the following on the current SHA46921f52 binary:

- **C2 remains: quotation input covariance.**
  `checker-audit/pv-quot-contra.f` and `pv4-quot-param-pass.f` exit0, but direct
  `pv2-direct-narrow.f` rejects expectedu8/actuali64. Dot
  `habu-respect-contravariance-in-a5d74c27`. U-TYPE at checker.f2089 pairs quotation
  din/rin with outer polarity; inspect contravariant inputs and covariant outputs.
- **C4 remains: implicit return-row reads.**
  `checker-probe/a38r-rfetch-run.f` reads0; `a42-sr-2rfetch.f` leaks return storage;
  `a38s-rfetch-ptr.f` crashes rc134/SIGSEGV. Dot
  `habu-seal-implicit-return-8e7e4152`. Inspect XG-READ-VAR, XPORT-RS-APPLY and
  implicit SGRBASE setup. Repro: `: W ( -- ptr n ) r@ ; : V ( -- n ) W @ ; V`.
- **M8 remains: signature tail ignored.**
  `checker-audit/pg-sig-trailing.f` certifies `( n -- n -- n n )` and runs.
  Dot `habu-reject-trailing-tokens-e0160f72`; PSIG around checker.f3794.
- **C1 tested cases fixed:** p19-repeat-exit, p20-repeat-throw,
  p19c-repeat-leave-loop reject before execution. Validp5 compiles/runs5 but its
  legacy `bye` is undefined; validp20b compiles and deliberately throws5.
- **C3 listed exploits now reject callers:** h01-rowvar-alias-caller and
  m19-rowvar-caller-depth-ptr. Broader row cases untested; NP-COLLECT still excludes
  S-ROW, so do not close the general claim on these two controls.
- **C6/M10 superseded:** dispatch installed; complete gate ran every suite.
- **Minor m8:** CHECK-DOES! still lacks some CHECK setup/publication by source;
  no runtime exploit established.
- Other audit correctness claims remain unvalidated. Local source still suggests
  data/return-stack capacity gaps, public ndict! bounds, integer overflow latch,
  stripped quotation address relocation, foreign IR successors, environment cap
  and zero-length UNMAP. The audit revalidation dot tracks this unfinished review.
  Do not label all47 findings current or fixed. Performance findings are deferred.

Quotation-variance minimal source:
```forth
TRUSTED: TAKE-U8 ( u8 -- ) drop ;
TRUSTED: MK-I64 ( -- i64 ) 300 ;
: CALLQ ( [ i64 -- ] -- ) MK-I64 swap execute ;
: PV ( -- ) [: TAKE-U8 ;] CALLQ ;
PV
```

## Other compiler blockers and agents

**Maki finally result grouping**, dot `habu-preserve-grouped-result-b343e371`.
Rowan confirms full Maki suite50/50, plain image and errors.f image build, isolated
REPL, warm recapture and native_image.py pass on the frozen pair. The former
address-table-full reproduction is repaired. Adding only `src/kiapi/items.f` after
maki.f fails compiling `FOOTPRINT-POSE`, E-NELAB-JOIN -8503. Its body ticks
`POSE-BODY ( -- point angle side )` and `RELEASE ( -- )`, then finally. Local source
inspection finds `NELAB:DO-FINALLY` (~3120) hardcodes NDICT:GLUE-NONE for returned
cells; ordinary DO-EXEC reads NDICT:CALL-GLUE. Reduce grouped outputs and preserve
checker-owned glue. No code patch yet. Tell Rowan exact candidate after fix.

**Warmed source-order verification**, existing dot0c9fe3d7.
Agent workspace `.jj-ws/cedar-source-order`, empty4547de6b at1dfebae6; no edits.
Verified source: global SORDER-VALUE `( n -- n ) 1 +`; package SORDER-TARGET defines
EARLY `( -- n ) 41 SORDER-VALUE` before a local SORDER-VALUE `( n n -- n ) +`.
VERIFY cold passes, runtimeEARLY returns42, VERIFY warm rejects. Changing EARLY's
effect to `( n -- n )` cold-rejects but warm-certifies. Defect is ambient source
visibility, not a filename exemption. Selected design: use VERIFY's existing
scanner and real declaration events to discover source-owned package/visibility/
tail identities (including generated words/EXPORT), rollback discovery, then
verify with a scoped overlay hiding ambient owned definitions until republished.
External dependencies remain visible. Apply to CHECKER-FIND-ACTIVE-SYM, global
claims, using lookup and CK-AOT-SERVE; release overlay on success/throw. No colon-
only blacklist. Test valid/invalid cold/warm source and existing dependencies.

**Decimal scaling**, existing dotee486b1c.
Agent workspace `.jj-ws/cedar-decimal-correctness`, WIP commit79f6766e,
bookmark `cedar/decimal-correctness-wip`; clean childae97f248. Only lib/float.f and
lib/float-test.f. Keeps18 significant decimal digits, accounts for omitted/fraction
places with saturating exponent addition, scales directly with exact powers<=10^22
and division, preserves signedzero. 89 float assertions pass at both tiers, fmt
and JSON reader focused tests pass. Independent Astra review still required;
check approximation contract/extreme rounding boundaries and unusedFL-IX cleanup.
Do not integrate WIP without review. No remaining agent process.

## Recovery and test status

`2195659e` deletes the BF giant snapshot assembly and missing-hook fixture path.
BF snap now builds a complete native runtime with tools/native-build.f and saves
through APP-IMAGE:SAVE. Version/trailer corruption and warm immediate checks remain.
Bootstrap fixture canonical source paths, sabotage payload and source-host marker
controls were repaired. `src/habu/snap-lib.f` now declares its FS/CODESIGN deps.
`bin/hb --load tools/build-fixpoint-test.f` **passes**, log
`/tmp/cedar-fixpoint-current-writer.log`; it built the full native runtime and
snapshot, exercised the trailer refusals, and cleaned its temp root.

Full no-binary/public recovery is still open (dota2551190). Partial recovery binary
`/tmp/cedar-compiler-fixpoint/tmp/hb-stdin` now advertises compiler paths and installs
dispatch, but `--load tools/native-build.f -- /tmp/cedar-recovery-native` fails
REG-INCOMING? / TFAM:REG-AOT-MERGE-INCOMING? with E-HIR-UNMODELED -8286.
Log `/tmp/cedar-recovery-native.log`. It is not an application runtime. Do not
revive ENGINE-SNAP-XT callbacks or weaken provenance to make it an image.
bootstrap.sh's finalrefresh/seed/ddc/chainbake/nonREPL hb-build still need migration
assessment. `src/habu/snap.f` remains unused old source after BF snapshot migration.

Full native gate evidence: `/tmp/cedar-compiler-gate.log`, all353 ran,351 passed.
Old failures: build-fixpoint (now focusedgreen), WID restore timeout360s.
Standalone WID recheck also timed out -2502 at a nested aggregate child after
many builds: `/tmp/cedar-compiler-wid-recheck.log`. aot-data-span-forge builds six
variants under its own per-build240s deadline but outer WID gives the entire child
240s; gate gives all WID360s. Resolve fixture deadline scope without dropping cases
or pretending timeout is correctness acceptance. No performance comparison now.

Other focused successes on SHA46921f52: native-order-exit + native-regalloc,
build-rewind bounds, engine-suite, native-prefix-declarations, current AOT
native-checker-prefix ownerhandoff. Independent Astra found no blocking defect in
seed bounds/prefix scan or terminal-order logic; possible loaded-host flake:
new prefix fixture180s vs existing owner fixture600s. Full native rebuild/gate,
product-hosted rebuild, and downstream acceptance remain required after fixes.
