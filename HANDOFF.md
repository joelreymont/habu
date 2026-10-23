# Handoff: finish the Intel port

State: 2026-10-23. Last lead: Claude (jerry) on the Mac; the next lead is
Codex on pulsar-ua-106 (Linux x86-64, 22 cores). Joel's Mac (ARM64) runs the
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
- Unlanded lanes are pushed as branches `intel-capreach`, `intel-proflate`,
  `intel-gep64`, `intel-tier0`, `intel-bodyabi` and `intel-pstore`. Each tip
  is the lane's last commit; the dots name the earlier commits in a chain
  (capreach is three commits, starting at `zlzpkqkt`). Their bases are older
  points of this stack: land one with
  `jj rebase -s <first lane commit> -d <stack tip>`. The WIP lanes carry
  their `lane-handoff*.md` files.
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

1. The dots live on master, not on this branch. From the repo root:
   `jj workspace add .jj-ws.noindex/<name>-dots -r master@origin`, then
   `dot tree habu-land-the-x86-a91100c2` and `dot show <id>` there; dot
   updates are committed from that workspace to master. The dots name the
   Mac lane workspaces `.jj-ws.noindex/jerry-x86-<lane>`; on pulsar start a
   lane from its branch `intel-<lane>@origin` in a new workspace.
2. Census this tip: `bin/hb --load test/run.f` (PATH with
   `/usr/lib/postgresql/18/bin`, `nohup` into a log). The last census (159
   red of 491) ran on an older stack; give each red to a child dot or to
   `habu-fix-remaining-x86-5a0c47fd`.
3. Lanes are split by who can prove them. The Mac (jerry) owns every lane
   with ARM work and lands it on `intel`: capreach
   (`habu-review-and-land-2437d9b0`), gep64 (`habu-land-the-64-1111310e`),
   tier0 (`habu-review-and-land-d6a57b7c`), the tier-1 branches
   (`habu-give-runtime-regression-cf20fae7`), bodyabi
   (`habu-finish-the-x86-5a565370`) and pstore's shared allocator fix
   (`habu-unblock-x86-inline-a7682014`). Do not edit their files; take
   their commits by rebasing onto `intel@origin` as they land.
4. Pulsar owns the x86-only lanes: proflate
   (`habu-review-and-land-5ce7ebe9`), breakpoints
   (`habu-implement-x86-native-261ee8bb`), throws
   (`habu-recover-x86-throws-b66ec686`) and the remaining rows
   (`habu-fix-remaining-x86-5a0c47fd`, after capreach lands). After capreach
   lands, self-build `bin/hb` again (native build plus two-gen) so later
   trees build in one stage. "Fable" or "Opus" review or Plan in a dot means
   the Codex reviewer or the lead's design.
5. A pulsar change to shared (non-x86) code needs an ARM check that only the
   Mac can run: record it in the lane's dot as open for the Mac session and
   never mark it done from pulsar. Goal (A) in
   `habu-rebase-gate-and-62c0a60f` runs on the Mac at the final tip.

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
