---
title: Typed defining words + provenance mints
status: open
priority: 2
issue-type: task
created-at: "2026-07-01T23:07:20.866786+02:00"
---

Two S/M capabilities retiring ~48 TRUSTED: sites: (1) role-typed variable/constant/create family generalizing PTR-VARIABLE (~24 sites incl. treeshake x17, MLEN, STB-CELL@); (2) named checked provenance mints - MMAP>PTR, VA>PTR, N>CODE-PTR, null-ptr role (~24 sites: DATA-VA x2, LINUX-VA>PTR, INCLUDE-MMAP-PTR, MBUF/CODE/ICODE-TABS, BP-NULL/NULL$, imgdump/imagedisasm, FFI pointer-to-cell, task x3). Each mint is one audited word with a test; consumers become fully checked. Effort M (~3d). checker.f + roles.f + docs.

## Live capability boundaries

- `src/core/roles.f:DTC-EVAL`: DTC-EVAL evaluates the auto-derived
  deftype converter pair built as TRUSTED: source text (roles.f:32-41). A typed
  defining-word capability mints the converters directly and retires the
  evaluate boundary.
- `lib/ffi-abi.f:PTR>CELL stdlib-boundary`: the sealed FFI package privately
  reinterprets foreign pointers as ABI cells. A checked provenance mint retires
  that boundary without republishing a global converter.

BATCH 1 PROVEN, LANDING HELD FOR A PERF WINDOW (2026-07-17, typedefs lane;
claim retained, workspace .jj-ws/fable-typedefs holds commit 804961ca
'treeshake: retire redundant raw-cell trust rows'): 17 treeshake rows deleted
as REDUNDANT - the reframing finding is that Capability A largely already
exists (raw variable/create/constant publish TVK-RAW cells since the
SIG-RAW-MODE! auto-effect of 2026-07-15; CHECKER-STORAGE-INFO deliberately
rejects the raw partition; typed-storage R4 pins it), so the per-cell
override rows dated 06-26/30 predate the capability and protect nothing.
TRUST 363->346. Fixpoint x2 155d3f27; old-binary boot ok; all pins + lints +
maki green twice; PEINV ratchet ok (no PRIM movement). Perf verdicts
unresolvable at integration time: the tree hard-failed by 0.04 percent and
the MASTER BASELINE itself then ran inadmissible/high on the same box (user's
morning workload, load avg 8.6) - land when a quiet window gives an
admissible bracket (re-run full run.f + baseline comparison).

RESIDUALS (enumerated by the lane, each with why-not-now):
(1) STB-CELL@ snap-lib row - same redundant class but rides the
snapshot-builder tail, prove through the snapshot gate;
(2) MMAP>PTR mint placement decision - image-bytes.f bakes at pos 7 BEFORE
roles.f pos 12, so the mint cannot live in roles.f for its earliest baked
consumer;
(3) VA>PTR/DATA-VA rows are platform-split - os/linux is not checked by a
macOS fixpoint; prove through the Linux gate (zed - device-window class);
(4) N>CODE-PTR / null-ptr / private FFI PTR>CELL / task x3 / imgdump code-ptr - the
mint batch proper, after (2) and (3) settle.
DTC-EVAL and the private FFI pointer boundary remain untouched.

BATCH 1 LANDED 2026-07-17 (typedefs lane, commit 07f493c1; claim RELEASED,
workspace retired): the 17 redundant treeshake raw-cell rows are gone,
TRUST 363->346. Rebased onto master (one LESSONS.md conflict, resolved as
pure-addition superset; no treeshake row was touched by master; master's
new CPR-EVAL/CPR-HOOK rows are checker-hook boundaries, NOT the redundant
raw-cell class - no scope change). Re-proven on the rebased tree: fixpoint
x2 byte-identical b09ed0ea (and re-verified identical on the final
integration tree - the proven->final delta was doc/ptx-tool only, engine
prefix clean); old-binary boot ok both directions; seal-absence, boot-pin,
maki, gate-stdlib all green. PERF BRACKET: tree pass admissible (e=54706
b=76300) AND master baseline pass admissible (e=55245 b=77000) in the same
quiet window - the batch is perf-neutral and the prior held-state
hard-fail (0.04% over at load 8.6) is retroactively confirmed as box
noise. REMAINING on this dot (~31 sites, unclaimed): DTC-EVAL via a typed
defining-word capability, and the private FFI pointer boundary plus the
named checked provenance mints (MMAP>PTR, VA>PTR, N>CODE-PTR, null-ptr
role et al.).
