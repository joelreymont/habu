---
title: "EPIC: type system, habu switchover, dot burndown"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T00:50:28.082247+02:00"
---

The campaign goal in three ordered phases. PHASE 1 - implement the type-family/ADT system: every PLAN.md item 1-16 (TFAM dots) lands green through its per-item 17a-p gate - registered parametric families, sums/enums/products, generated constructors without trust, checker-owned exhaustive MATCH, runtime tag death, linear layouts, layout-aware stack ops with width-aware native+Gforth lowering, sealed registries with boot-latch friend capability, ADT diagnostics/repair packets; no new TRUST/TRUSTED:/set-check/TRUSTED.md rows; master moves only by verified-green fast-forward. PHASE 2 - switch habu itself over: once MATCH+constructors execute (TFAM 9/10), migrate habu sources to the new types wherever the checker can express them - option/result returns replace sentinel/flag conventions in lib/ and tools/ public APIs, block ENUM families retire ENUM+ call sites, PRODUCT unifies VALUE-RECORD (TFAM 15) and PTX IR, typed ADT protocols replace raw-cell conventions so TRUSTED boundaries shrink (feeds [[habu-epic-type-habu-a34713f0]] retire-TRUSTED); every migration checked, gated, size-ratcheted. PHASE 3 - tackle remaining dots on the typed foundation: maki adoption epic (habu-epic-adopt-adts-64833911), layout-polymorphic params (habu-checker-capability-layout-9b8540bd), deriving (habu-checker-capability-derive-23788e95), then the open PTX/AD/maki backlog (fusion, attention, ONNX, training loop) written in typed ADT style. DONE WHEN: all TFAM dots closed; no sentinel-encoded results remain in checked public APIs; maki suite green on ADT-typed APIs; dot list empty or every survivor explicitly deferred with rationale.

## HANDOFF 2026-07-10 (orchestrator -> next type-system agent)

The Model CAD orchestrator drove the TFAM gate to completion and returns to
maki work. This section is the complete transfer contract; read it with
`.dots/habu-epic-adopt-adts-64833911.md` before starting anything.

STATE (all merged into `fable`, pushed; fable is the working branch now —
`maki-type-families` is historical, fully contained in merge b0556262):
- TFAM 9/10/12/14/15 CLOSED with full ledgers (see each dot's archive in the
  jj history of maki-type-families; closes at 1091b31f, 96052b3b, 28a96a09,
  and the 15-close). TFAM 1-8 landed earlier. EXPORT capability landed+closed
  (e79695c7): package re-export alias, seal/ctor/dup/prim rejects, AOT
  one-body proof. Capability-campaign DESIGN landed (1669e81a): three
  implementation-ready designs + ordered 5-step DAG in dots
  habu-checker-capability-typed-a480c423 / -layout-4e7f1f03 /
  -derive-23788e95. Step 1 (enum-tier typed store/fetch, checker-only) is IN
  FLIGHT in workspace .jj-ws/fable-cap as of this writing; the orchestrator
  merges it when green, then this epic owns steps 2-5.

YOUR QUEUE, in recommended order:
1. Capability campaign steps 2-5 per the DAG in dot a480c423 (enum-in-product
   fields; derived eq/hash; wide-bundle store + LAYOUT-BUFFER; later slices).
2. TFAM 11 tail (dot habu-tfam-11-linear-99fa9990): whole-bundle accounting
   landed; remaining = parametric (arity>0) constructor publication (item-8
   blocker), open-arg parametric MATCH scrutinee, TLP unpacker retirement
   (habu-retire-tlp-mk2-ac7760d2).
3. TFAM 13 (habu-tfaam-13-adt-5d3288f0): repair-packet + public-signature
   halves (MATCH §24 diagnostics already landed in 9 slice 4). Related eval
   packet gaps: habu-repair-packet-machine-879ad716, -typed-62bc5df2.
4. TFAM 16 layout policies (habu-tfam-16-layout-a764d28c) — note the derived-
   eq design gates DERIVE to stack-cell-tag; 16's policy work must keep that
   sound or extend the eq generator.
5. TFAM 8 residual: declaration-time collision vs pre-existing engine
   wordlists/packages. 2b residuals: habu-tfam-2b-v-9cbd0019 (+ 2b-iii dots
   on the old master-side tree). NOTE: the previous type-system agent's
   UNCOMMITTED work sits untouched in workspaces .jj-ws/tfam-current (render.f
   edit + TFAM-8-rc sub-dot), .jj-ws/engine (2b seal RCA), .jj-ws/lane-2b,
   .jj-ws/zed-ptx (vjp dot) — adopt or salvage before writing over that
   territory.
6. Infra debts: habu-bootstrap-mirror-pass-f1714953 (TRIPWIRE: pass-2 +
   rec-wide-publish Gforth mirror must land before the first production wide
   family use enters the recovery compile path), habu-tfam-12-item-346f03c2
   (snap certify regression + labeled 79/80 loader exits),
   habu-interpret-wide-gate-1d70acf7, habu-logical-shape-depth-9686f5c1,
   habu-export-alias-diagnostics-5fd8dcde.
7. Adoption: switchover waves A-E (dots habu-switchover-wave-*) under
   habu-epic-adopt-adts; wave D (ptxir->PRODUCT) has the R8 recipe in its
   dot. The CAD ADT swap itself (habu-cad-adt-swap-7bf0bb1f) stays with the
   maki orchestrator — you unblock it by landing capability steps 1-4.

DISCIPLINE (non-negotiable, proven over ~20 merged lanes):
- Workers NEVER move bookmarks or push; commits stack on `fable`; the
  reviewer runs the gates on the exact tree and fast-forwards.
- Engine/checker commits: byte-fixpoint x2 (use the FULL documented prelude:
  bin/hb --load lib/errors.f ... tools/build-fixpoint.f
  tools/build-fixpoint-main.f -- install --force; NEVER bare --load
  tools/build-fixpoint.f), full gate test/run.f (lib/process-test flakes —
  retry once), maki/test.f, seven type suites, error-code/namespace/host/
  filemap/dot-dep lints, trusted-inventory strict, typed-local-diff-lint,
  TRUSTED.md pins, prop census for new prims.
- Two-commit staging for engine + running-engine-tool changes (each commit
  one-refresh buildable from its parent's binary). checker.f is a boot-time
  PREFIX (follows the tree, not the binary).
- Engine facts: RX window (LPROT 5/3) around mid-compile bridge calls
  (pc==x11 SIGBUS is that crash); per-leg LBCAP (central capture is
  downstream of the ADT dispatch); x11 stays clear around C-CALL-X11-SAVED;
  bare local labels use ADR/CBZ (LABEL@ only for label variables).
- macOS: rm before cp over bin/hb (SIGKILL on overwritten inode). gforth
  bootstrap needs ~/.local/bin/gforth. Audit-first commits; design forks get
  options + recommendation in the dot; capacity exits must attribute
  themselves; no silent caps.
