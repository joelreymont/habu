---
title: "TFAM 2b-iii: residual dictionary-truncation gaps after the seal"
status: open
priority: 2
issue-type: task
created-at: "2026-07-05T09:58:59.936201+02:00"
---

The truncation seal (commit 08dbce8a) closes FORGET-DEFS-FROM / HIDE-DEFS-FROM (ndict watermark SEAL-NDICT-CELL, guard SEAL-DICT-GUARD in src/habu/xref.f) and direct CHECKER-USIGS-TRUNCATE-FROM (reject-when-sealed wrapper in src/core/checker.f). Two low-severity residuals remain, both proven: (1) SCRIPT-ARGV TAIL GAP: SEAL-CAPTURE (src/habu/habu1.f BSEALCAP, called at end of src/habu/xref.f) freezes the watermark at ndict-after-xref (~2746), but src/os/script-argv.f + src/habu/driver-io.f load AFTER xref.f (indices > watermark), so FORGET/HIDE of those ~7 leaf argv/driver helpers is still allowed (FORGET-DEFS-FROM of SCRIPT-ARGV tail -> rc 0). Fix: capture the watermark at the true engine-source end (last baked file) instead of end-of-xref, e.g. an explicit SEAL-CAPTURE token appended by the source-emit stage after all engine files, or move the call to the last file. (2) RAW ndict!/cp! : BNDSET/BCPSET are deliberately left open post-seal by the 2b-i design (test/seal.f SLV-FORGET-FORGE is a positive that raw ndict!/cp! FORGET round-trips still work for user marks). A raw 100 ndict! post-seal truncates the dict (rc 0), a self-DoS, but CANNOT complete a package spoof now that CHECKER-USIGS-TRUNCATE-FROM is sealed: the checker USIG registry survives a raw ndict!, so a subsequent redefine of the forgotten engine word is blocked (2740 ndict! then redefining an engine word -> rc 78). A full BNDSET/BCPSET watermark guard is entangled with the engine refresh (src/habu/hide.f BFR-NDICT! and bootstrap.sh BOOT-HIDE-DICT-FROM legitimately truncate below the watermark post-seal during self-rebuild), so it needs a friend/refresh exemption before it can be added. KEY ARCHITECTURE FINDING for the seal-hardening lane: the friend latch is set EARLY (at ndict~148, before the engine's own checker/xref/stdlib source is evaluated); that source loads POST-latch via guard-bypassing DATA stores and grows ndict to ~2750, THEN user source runs. So 'latch sealed' != 'engine fully loaded', which is why the watermark must be captured by a Forth-reachable bypass at engine-source end, not natively at EMIT-SEAL-FRIEND.

## Item 1 FIXED, item 2 assessed documented-open (2026-07-08, engine lane)

ITEM 1 (script-argv tail gap) - LANDED. The cold-prefix assembler now appends
`SEAL-CAPTURE` as the LAST engine-prefix source token, after script-argv.f and
the provide rows (habu2.f EMIT-SEAL-CAPTURE-TOKEN, called in LCOLDPFX between
PFX-PROVIDE-FILES and EMIT-SEAL-FRIEND; all four native entry paths -
pipe/file x2/repl - BL LCOLDPFX, so one site covers them). Stage0 mirrors the
word and calls it at the tail of its PFX-PROVIDE-FILES (its four inline
cold-prefix sites all go through it). Key mechanics honored per the
ARCHITECTURE FINDING: the capture stays a SOURCE token (BSEALCAP is the
Forth-reachable post-latch bypass) because at cold-prefix assembly time
nothing has evaluated and ndict is still the native-prim boundary; snap-gated
like PFX-LOAD-SCRIPT-ARGV-COLD so a snapshot keeps its bake-time watermark.
xref.f's in-file SEAL-CAPTURE stays as the BASELINE capture (monotonic:
re-running only raises the watermark) for contexts that load base files
without the cold-prefix assembler (C-SOURCE-BAKED stage engines - which take
no post-source input, so no gap there). Proof: `s" SCRIPT-ARGV$"
FORGET-DEFS-FROM` now rc 83 + `seal: cannot FORGET/HIDE sealed engine
definitions` on BOTH --load and stdin paths (was rc 0); engine-word forge
still 83; user-mark FORGET/HIDE round-trips still rc 0; SCRIPT-ARGC still
works. Regression: test/seal.f SLV-FORGET-TAIL/SLV-HIDE-TAIL forges, four legs
in SLV-NEGATIVES-TRUNCATE (committed RED first: expected 83 got 0).

ITEM 2 (raw ndict!/cp! watermark guard) - REMAINS OPEN, blocker restated after
re-assessment against the current friend machinery:
- The spoof is already closed WITHOUT the guard: post-seal `2600 ndict!` then
  redefining an engine word still exits 78 (the sealed checker USIG registry
  survives raw truncation). Residual harm is self-DoS only, and that now
  fails LOUDLY: a raw `100 ndict!` truncates the engine's own publish-time
  helper lookups, so the next definition dies 70 (`wf-wide?` C-FIND-GLOBAL
  miss) instead of silently continuing.
- A prim-level guard on BNDSET/BCPSET still cannot distinguish principals:
  the engine refresh runs as ordinary post-seal `--load` source and
  LEGITIMATELY truncates below the watermark (src/habu/hide.f BFR-NDICT! ->
  raw ndict!, appended into the refresh source by tools/build-fixpoint.f;
  bootstrap.sh BOOT-HIDE-DICT-FROM ditto). Any exemption channel reachable by
  that user-level source (flag word, magic value, special prim) is reachable
  by an attacker's user-level source in the same engine - a guard with such
  an exemption is theater, and the one-way friend latch cannot be selectively
  reopened by design.
- The clean path is to ELIMINATE legitimate below-watermark truncation
  instead of exempting it: the staged-fixpoint source-checking redesign
  (habu-staged-fixpoint-src-0b5fc6e6, which already plans to dissolve
  hide.f's BFR-CHECK-OFF) would replace the in-process truncate-and-reload
  refresh with fresh-process builds; once no legitimate caller truncates
  below the watermark, the prim guard becomes a trivial follow-up. Until
  then a forced guard would break every self-rebuild.
