# Handoff: finish the Intel port

State: 2026-10-23. Last lead: Claude, Herdr name jerry. Hosts: pulsar-ua-106
(Linux x86-64, 22 cores, where work continues) and Joel's Mac (ARM64, the
ARM gate).

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
  on master: `dot show <id>` for each child. Each child names its lane
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
  written for a Mac lead driving pulsar over ssh; on pulsar run its pulsar
  steps locally.
- Pulsar's `~/Work/habu/bin/hb` (+ map) is self-built on pulsar from c848c7d7
  by an engine cross-built on the Mac from the same commit; two-gen there
  prints "ok gen 5 matches gen 4". No Intel bootstrap exists, so never lose
  the x86 engines: the older seed (built from 20b75344, which cannot build
  trees containing `zlzpkqkt`) is kept at `bin/hb.20b75344` (+ map).

## Rules

- Master wins: fix x86 code to fit master; never rewrite a master test to fit
  x86. Every test change gets reported to Joel (list in the dots and commit
  messages).
- x86-64 runs tier 1 only. Tier-0-dependent tests select by `tier@`, keep the
  ARM tier-0 assertion byte-identical, and assert ARM's `1 set-tier` output
  on the tier-1 branch.
- Each lane gets an independent review before landing (Fable for capture,
  kernel, ABI and codegen; Opus for focused fixes and tests).
- On pulsar, kill only PIDs your own commands started; never `pkill -f`.
- A gate and its push never share one command chain.

Port design background: `INTEL.md`, `docs/x86-64.md`, `docs/gate.md`.
