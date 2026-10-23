# Handoff: finish the Intel port

State: 2026-10-23. Current lead: Codex on pulsar-ua-106 (Linux x86-64,
22 cores), following Claude (jerry)'s Mac handoff. Joel's Mac (ARM64) runs the
ARM gate; pulsar never accesses the Mac (Joel). Pulsar pushes `intel` and
lane branches only; `master` moves only after the Mac's ARM gate. This file
is tracked on `intel`: read it at session start instead of `$pickup` (which
would trash it), and keep it current as work lands.

Pulsar has `bun` and `wasm-tools` (the versions the Mac uses) and `uv` from
`~/.config/mise/conf.d/host.toml`, on PATH through mise shims in `~/.profile`
and `~/.bashrc`, so Wasm rows that fail there are x86 reds. `trash` is
trash-cli; `agent-clean` comes from Dotfiles. Pushes use gh's HTTPS token.

## Goal (Joel)

Done only when command output shows all of:

- (A) macOS ARM64, at the final tip (this stack plus a merge of current
  master): `tools/two-generation-build.f` prints "ok gen 5 matches gen 4 byte
  for byte" and `bin/hb --load test/run.f` exits 0 with no "red tests".
- (B) `jj git push` moves `master@origin` to a commit containing that merge,
  and `intel@origin` to the same tip.
- (C) on pulsar the cross-built `hb` runs; a native build plus
  `tools/two-generation-build.f` prints "ok gen 5 matches gen 4"; and
  `bin/hb --load test/run.f` exits 0. Fix every red there at its source,
  re-gate on ARM, land on master, and rerun (C) on the final commit.

## Where the work is

- This branch (`intel`) is the integration stack: the Intel port plus the
  landed x86 lanes, based on an older master. Its root commit for rebasing is
  `vznuxmnw` "Define CODE-END for the x86-64 link"
  (`jj rebase -s vznuxmnw -d master@origin`; never `-b`).
- The task list is the dot tree `habu-land-the-x86-a91100c2` ("Land the
  x86-64 integration stack on master") under `habu-finish-the-linux-57cb3952`
  on master (see Next, step 1). Each child names its lane
  workspace, change ids, what is proven, what is open and its acceptance.
- The remaining original lane is pushed as `intel-pstore`. Its tip is the
  lane's last commit. The original lane bases are
  older points of this stack: land one with
  `jj rebase -s <first lane commit> -d <stack tip>`. The WIP lanes carry
  their `lane-handoff*.md` files; drop those files from the commit that
  lands.
- Lane recipe, designs, review/plan briefs and probe scripts are in
  `~/.cache/tmp/jerry-c/` on pulsar (`lane-recipe.md` first). The recipe was
  written for a Mac lead driving pulsar over ssh: on pulsar, edit and test in
  `.jj-ws.noindex/<lead>-<lane>` workspaces with the recipe's build and run
  commands, and skip its Mac and rsync steps.
- Pulsar's `~/Work/habu/bin/hb` (+ map) is self-built on pulsar from c848c7d7
  by an engine cross-built on the Mac from the same commit; two-gen there
  prints "ok gen 5 matches gen 4". No Intel bootstrap exists, so never lose
  the x86 engines: the older seed (built from 20b75344) is kept at
  `bin/hb.20b75344` (+ map). Neither engine builds a tree containing
  `zlzpkqkt` (E-UNDEFINED `ADDRESS-CARRIER:REL32-BYTES` in ACAP-X64-SITE):
  build an engine from `zlzpkqkt`'s own tree first, then build with it.

## Next on pulsar

Active lanes use `.jj-ws.noindex/pulsar-intel-{images,pstore,sessions,image-state,app-extent}`;
the dots workspace is `pulsar-intel-dots` on master. The unchanged image
producer now exits 0, producing all three executable/map pairs and the merged
seed image. Its fix includes x86 placement/writer ordering, publication across
compiler replacement, boot contract seeding/restoration, native window capture
and the structural shadow-row bound. Final delta review and convergence remain
open before integration. Shared changes retain an open Mac ARM check.

Landed on intel: `ruypmwwq` / `e65e0176`, "Retire x86 profiler spans on trim
and reclaim". Independent adversarial review found no defects; native build,
`test/prof-rewind.f` and `test/prof-index.f` exit 0; native two-generation
check exits 0 with "ok gen 5 matches gen 4 byte for byte". Only x86 source
files changed. The full native census still needs the image-build fix.

Landed on intel: `pwsnnpqn` / `4407e672`, "Resume x86 native breakpoints
with INT3". Independent adversarial review found no defects; native build,
full `test/gate-debug.f` and generation 5/4 byte convergence exit 0. The
combined profiler/breakpoint engine also passes `prof-rewind`, `prof-index`
and `gate-debug` in one process. No master tests changed. Shared DEBUG's
Mac ARM check remains OPEN in `habu-implement-x86-native-261ee8bb`; its
pulsar workspace is retired.

Landed on intel: `kwttvzym` / `d0024147`, "Restore evaluate cursors with
live tasks". Independent adversarial review found no defects; native build,
generation 5/4 byte identity and all 220 `outer-interpret` cases pass. The
combined profiler/debug/recovery engine passes all four focused files in one
process. Public mutator guards remain intact; no tests changed. Its five
shared source files still need the Mac ARM check in
`habu-recover-x86-throws-b66ec686`.

Landed on intel: `rvmtmsrk` / `140a8c27`, "Enable x86 definition cache body
reuse". Independent adversarial review found no defects. The lane proves
cold 0 hits/0 skips/7 compiled, warm 7 hits/7 skips/0 compiled, native build
and generation 5/4 byte identity. The combined engine passes forms-zero,
state, admission, AOT cache, provider refusal, DATA claims and x86 seam in
one process. Mechanical IR/harness fixture updates and x86 unit-format cases
are listed in `habu-finish-the-x86-5a565370`; its Mac ARM check remains OPEN.
Remaining auto byte differences belong to image-state, session close to
sessions, and missing seeded contracts to the image boot fix.

Joel decisions are pending for whitebox classification of FMT/byte-buffer
tests and a tier-1 PTY diagnostic expectation. No master assertions have been
changed for these requests. The image-state lane removes measured transient
DATA differences from seven native-file-auto image comparisons; it preserves
all original comparisons.

1. The dots live on master, not on this branch. From the repo root:
   `jj workspace add .jj-ws.noindex/<name>-dots -r master@origin`, then
   `dot tree habu-land-the-x86-a91100c2` and `dot show <id>` there; dot
   updates are committed from that workspace to master. The dots name the
   Mac lane workspaces `.jj-ws.noindex/jerry-x86-<lane>`; on pulsar start a
   lane from its branch `intel-<lane>@origin` in a new workspace.
2. Give each red of the x86 census below ("x86 state of this tip") to a
   child dot or to `habu-fix-remaining-x86-5a0c47fd`. Rerun the census with
   `bin/hb --load test/run.f` (PATH with `/usr/lib/postgresql/18/bin`,
   `nohup` into a log).
3. The Mac's ARM lanes are all on `intel`: capreach
   (`habu-review-and-land-2437d9b0`), gep64 (`habu-land-the-64-1111310e`),
   tier0 (`habu-review-and-land-d6a57b7c`), the four tier-1 branch commits
   of runtime-regression-test.f (`habu-give-runtime-regression-cf20fae7`)
   and pstore's shared allocator fix ("Restrict fixed-register read
   exemptions", `habu-unblock-x86-inline-a7682014`). Two lanes move to
   pulsar, because what is left in them is x86 work that only pulsar can
   prove:
   - bodyabi (`habu-finish-the-x86-5a565370`, branch `intel-bodyabi`). Its
     ARM side is done: ARM build, two-gen and its 9 rows pass. Its
     requirement is positive x86 definition-cache hits, and x86 cannot
     cache a definition yet: `tools/native-unit-capture.f` (about line 91)
     requires an AArch64 host and target, `tools/native-unit-object.f`
     rejects a non-`AARCH64-PROFILE?` profile, writes and reads
     `ARCH-AARCH64` at its pack/read words, and relocates calls by decoding
     ARM `BL`/`B`. Port that capture/import path to x86 (architecture tag,
     instruction lengths, call and address relocations) first, then land
     the body-reuse ABI with it: without the port the ABI has no x86
     caller. Review: `~/.cache/tmp/jerry-c/oracle/bodyabi.txt`. The Mac
     re-checks ARM byte identity afterwards (step 5).
   - pstore (`habu-unblock-x86-inline-a7682014`, branch `intel-pstore`).
     Blocker 1 (the allocator exemption) is fixed on `intel`. Blocker 2 is
     the cross-capture guard resolution: the `aot-shadow.f` `SH-TARGET`
     fallback for `OWNER-API-PRI-WID` helper rows, and `link-x64.f`
     `KERNEL-BODY` admitting registered helper rows. Design:
     `~/.cache/tmp/jerry-c/oracle/pstore.txt`; cut it to the smallest
     mechanism before a worker gets it.
4. Pulsar owns the x86-only lanes: throws
   (`habu-recover-x86-throws-b66ec686`) and the remaining rows
   (`habu-fix-remaining-x86-5a0c47fd`, after capreach lands). After capreach
   lands, self-build `bin/hb` again (native build plus two-gen) so later
   trees build in one stage. "Fable" or "Opus" review or Plan in a dot means
   the Codex reviewer or the lead's design.
5. A pulsar change to shared (non-x86) code needs an ARM check that only the
   Mac can run: record it in the lane's dot as open for the Mac session and
   never mark it done from pulsar. Goal (A) in
   `habu-rebase-gate-and-62c0a60f` runs on the Mac at the final tip.

## ARM state of this tip

On the Mac, the engine built from "Restrict fixed-register read
exemptions" (the later commits change tests only): two-gen prints "ok gen
5 matches gen 4 byte for byte"; `bin/hb --load test/run.f` ran 501 of 501
with one red, compiler-native-hookless-reject (see Rebasing onto master).
On pulsar, an x86 engine built from this tip (the seed built capreach's
first commit, that engine built the tip) runs runtime-regression-test.f
to its first FAIL, process/pty; with GE-FAIL made non-throwing in a
scratch copy it reports exactly two FAILs, process/pty and process/pty
output (213 before the tier-1 commits). The engine is kept at
`~/.cache/tmp/jerry-c/x86-engine-b63e453c/` (+ map): it builds trees
containing capreach in one stage. The census log is
`~/.cache/tmp/jerry-c/tier1/rr-census-b63e453c.log`.

## x86 state of this tip

On pulsar, at the tree of "Record the x86 runtime-regression census": the
x86 engine self-built from that tree (rc 0); two-gen prints "ok gen 5
matches gen 4 byte for byte"; `bin/hb --load test/run.f` ran 485 of 485
with 97 red. Logs and the red list (`reds.txt`: name, kind, code) are in
`~/.cache/tmp/jerry-c/x86-49af42cc/`.

The same suite on an x86 engine self-built at `4124c31f` (this branch
before any Mac lane; `base-4124c31f-suite.log` and `-reds.txt` there) ran
485 of 485 with 110 red. No row is red here and green there. 96 of the 97
reds fail there with the same kind and exit code; rows were matched by
kind and code, not by their inner assertions. The 13 rows red only there
are fixed by the Mac lanes: aot-named-cells, aot-named-cells-native,
bare-copy-lint, build-fixpoint-source, compiler-native-eval,
compiler-native-prefix-declarations, dictionary-capacity,
error-code-lint-fixtures, gate-pool, name-length, trust-row-refusal,
wasm-capture and wasm-link-capture. Details:

- compiler-images-build is red before the Mac lanes, with a different
  code: at `4124c31f` it exits 74 in the suite and, run alone, exits
  83 (`ENGINE-ERROR:SEAL-VIOLATION`, a kernel-x64 seal trap); at this tip it
  throws 7179 (`E-LIVE`, src/habu/xref.f, code reclaim refuses a floor with
  a live routine above it). Run alone, both fail at the same point, after
  the build's "compiled" line and before any image is written, and no Mac
  commit touches code reclaim or the seal. The 45 rows that exit 75
  depend on its images.
- check-verify is red before the Mac lanes. Run alone
  (`bin/hb --load tools/check-verify-test.f`), base and tip fail the same
  four asserts: uses-visit counts 11 calls, not 10 (F930), and
  trusted-tick-order reports `E-UNDEFINED` on `CVT-NOT-DEFINED` at column
  41 instead of `E-CAP-TRUSTED` on `patch32` at column 28 (F2575-F2578).
  It spends 1202 s user at base and 1082 s at the tip; the suite stops it
  at its 360 s CPU budget (cli-usage alone takes about 120 s).
- capreach's x86 rows pass: aot-native-merge, trust-row-refusal,
  name-length, and its two artifact probes run by hand
  (`bin/hb --load test/aot-native-does.f -- <dir>/x.aot`, the same for
  `aot-native-export-fixed.f`; no suite row runs them) print "captured".
  aot-wid-refuse-address-span, aot-wid-rebase, aot-wid-capture-refusal,
  aot-data-window, aot-payload-graph, aot-payload-unsupported and
  aot-named-cells-image (CPU budget) fail with the same kind and code at
  `4124c31f`.
- engine-runtime-regressions is red only for process/pty (see below).

## x86 reds the Mac lanes found

Each is x86 work for pulsar; a fix that touches shared code needs the Mac's
ARM check (step 5).

- `test/compiler/native-identity-spill.f` F6/F8 decode ARM64 encodings:
  move the file to the ARM-only cases or give it an x86 path.
- `test/outer-interpret.f`: `oi-live-too-long.f` exits 79, not 76 (about
  line 1171).
- runtime-regression-test.f `GE-PROCESS-PTY`: x86 crashes (SIGSEGV) at
  case 54, `' SQB BP+` then `7 SQB .`; later cases see an empty buffer.
  ARM with the REPL at tier 1 passes 1-64 and then times out at case 65:
  `BP*` does not fire at tier 1 (`6 PB .` prints 12 with no `habu-bp:`
  hit). Both belong to the breakpoints lane.
- The ARM regalloc validator (A64RAV) accepts a value in a register that
  its own op overwrites before reading it. ARM allocation never produces
  that today; the validator gap stays open.

The tier-1 branches assert ARM's `1 set-tier` output. x86's `evaluate` is
the Habu loop (`src/habu/definers.f`), so where ARM's engine loop and the
Habu loop differ at tier 1, ARM tier 1 through the Habu loop
(`test/outer-loop-on.f` loaded before `1 set-tier`) is the oracle. The
65-locals case (RXTML) is one: both print master's tier-0 text, so it
stays unbranched.

## Rebasing onto master

- `test/compiler/native-hookless-reject.f` is red on ARM at this tip: its
  tier-1 AMBIGUOUS sessions print no E-USING-AMBIGUOUS. Master's "Admit a
  package definition a using shadows" (`569dc3bf`) changed `KEEP-PRIOR`
  (src/compiler/native/compiler.f), added `NDICT:USING-REFUSED?` (dict.f)
  and rewrote the tier-1 SHADOWED/AMBIGUOUS expectations; this branch has
  the older test.
  Resolve the conflict to master's test and code, then keep this branch's
  x86 `NO-TIER-ZERO` session and `HB-TARGET-LINUX-X86-64?` selection.
- Master, as of `e1220a95`, has folded `docs/x86-64.md` and
  `docs/porting.md` into `docs/xcomp.md` (its "x86-64" and "Porting"
  sections) and dissolved `docs/vocabulary.md` and `docs/policy.md` into
  code headers and `docs/design.md`. This branch still edits
  `docs/x86-64.md`: carry each edit into `docs/xcomp.md`, or into the header of the code it describes
  (a doc passage that restates code moves to that file's header or is
  dropped), delete the old files, and repoint the links in `INTEL.md` and
  this file.

## Rules

- Master wins: fix x86 code to fit master; never rewrite a master test to fit
  x86. Every test change gets reported to Joel (list in the dots and commit
  messages).
- x86-64 runs tier 1 only. Tier-0-dependent tests select by `tier@`, keep the
  ARM tier-0 assertion byte-identical, and assert ARM's `1 set-tier` output
  on the tier-1 branch.
- Each lane gets an independent read-only review before landing, given the
  requirements and the code, not the implementation discussion.
- On pulsar, kill only PIDs your own commands started; never `pkill -f`.
- A gate and its push never share one command chain.
- Beware of overengineering with Astra (Joel): each design is the smallest
  mechanism that closes a concrete failure. Every new word, cell or row kind
  needs a failure it fixes; run `$complexity` on a design before a worker
  gets it, and do not let Astra's own critique be the only check.

Port design background: `INTEL.md`, `docs/x86-64.md`, `docs/gate.md`.
