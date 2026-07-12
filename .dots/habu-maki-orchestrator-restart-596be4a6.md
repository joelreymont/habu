---
title: "Maki orchestrator restart: queue + protocol"
status: open
priority: 2
issue-type: task
created-at: "2026-07-10T15:49:29.124317+02:00"
---

RESTART CONTEXT for the maki/Model-CAD orchestrator. Start every session in ~/Work/habu-fable (a jj workspace of ~/Work/habu; the branch is the fable bookmark; bin/hb here must match the tree — if stale, rm bin/hb then cp a fresh fixpoint or rebuild via the docs/bootstrap.md FULL prelude invocation, never bare --load tools/build-fixpoint.f). TWO-AGENT SPLIT: a separate type-system agent owns src/core, src/habu, test/type-*, bootstrap/ per the handoff contract in .dots/habu-epic-type-system-b88c9ecc/ (TFAM tail 11/13/16, capability campaign derive-eq + wide-store + LAYOUT-BUFFER, switchover waves); it advances fable directly. I own maki/* + docs/paper + tools lints. MERGE PROTOCOL: workers implement in .jj-ws/fable-<name> workspaces off fable and NEVER move bookmarks or push; I review hunk-by-hunk, rebase the stack onto the live fable tip, run the owning gates on the exact tree (maki/test.f 77 suites + focused suites + namespace/error-code/host/filemap/dot-dep lints + typed-local-diff-lint; full test/run.f for anything wider; lib/process-test flakes - retry once), freshness-check the bookmark, fast-forward, push, then forget+rm the workspace in the same merge window. QUEUE (updated 2026-07-11, evening): ACTIVE MISSION = .dots/habu-goal-maki-host-b69ed256.md "GOAL: maki host-complete (no Orin)" - five ordered waves over the existing dots (Wave 1 ad-validate-multi keystone; Wave 2 autograd stack orchestration->tensor-batched-host->transformer-block->higher-order->end-to-end-CPU-parity; Wave 3 training loop; Wave 4 tensor/array host layer; Wave 5 onnx-import-audit + eval-matrix + llm-target harnesses). Every device-verify leg goes to the pending-zed queue, never silently dropped. Execute one wave item per worker cycle via the PROVEN WORKER PROTOCOL: dispatch fresh-context Opus workers into .jj-ws/fable-<name> (seed bin/hb by cp - fresh file only, never over an existing one), brief with the authoritative dot + precedent commits + proven authoring patterns (family-typed surface: dtype/layout/align/opkind/dimclass ENUMs w/ DERIVE eq, skey product, W=1 typed locals NOW WORK), verify gates independently, ALWAYS destruction-review against the authoritative spec (caught 1 critical + invalidated 1 design this campaign), fix-round via SendMessage resume, then merge-window: fetch, merge master into fable (append-only), rebase worker commit, REBUILD bin/hb (master's checker moves daily), full gates on the exact tree, ff fable, push, forget+rm workspace IN THE SAME WINDOW (workspace op-state races caused divergent commits twice - forget before rm, snapshot workspace edits with a jj command run INSIDE the workspace before any main-workspace jj call). COMPLETED 2026-07-10/11 (records in .dots/habu-cad-adt-swap-7bf0bb1f.md + habu-maki-onnx-graph-51adfd39.md): family-typed Model IR (descriptor trio 6c58e1d6 + opkind 1e75815e + OPK=->derived EQ 64e96678), ONNX residuals (8a0a9fe0), SKEY typed key (0a7a7d2a; replay table stays STR=-keyed on the PROVEN-injective render - durable load path has text only; typed-column upgrade gated on a480c423 S2). STILL GATED: evidence rows (LAYOUT-BUFFER S3); recursive IR (TFAM 16); Zed device backlog when the Orin returns ~2026-07-14 (.dots/habu-infra-zed-unreachable-c3d8c991.md + pending-zed queue); paper device rows + TFAM ablation after that; USER-GATED: onnxruntime real-model golden + PyTorch parity cross-check (both need host runs from the user). macOS gotcha: cp over an existing bin/hb SIGKILLs at exec - always rm first.

## STATUS 2026-07-12 (post LAYOUT-BUFFER reconciliation)

- fable@origin = 64cdb81c: reconciled onto master's wide-ADT stack (LAYOUT-BUFFER
  only-introduction-form; boot prefix 31, pin approved 26->31 by orchestrator, dot
  habu-boot-pin-26-ca4bffb4), plus eval v1.1 `tokens` directive + tok-src column +
  public-signatures numeric-name fix (destruction-reviewed clean; dot
  habu-public-signatures-skips CLOSED).
- S2 wide-store lane RETIRED UNSALVAGED: master landed the same slice (same dot
  a480c423) in its wide-ADT stack. Lesson recorded: probe the other lane's
  in-flight claims (master commit subjects + dot blocker graph) before
  dispatching against a shared dot.
- IN FLIGHT: crash-rca lane (.jj-ws/fable-crash, dot habu-hb-crash-bare-c5be6634)
  rebasing its internal-word gate (DNAME-INT flag + seal-time marking pass +
  tick gating; boot prefix grows to 32 with src/core/internal-mark.f) onto the
  reconciled tree; needs destruction review + merge window. Residue dot filed:
  habu-habu-certified-words-84e84eaf (FOO2-class below-base reads).
- NEXT BIG JOB: master 441b834e landed Model-CAD V2 (MODEL-CAD-V2-PLAN.md, epic
  habu-epic-model-cad-70b629a9) R3 nominal CAD-KIND identity kinds across ~50
  maki files that fable also changed - MERGE POLICY in dot
  habu-merge-policy-master-961bb2b7 (adopt master's identity/index kinds; keep
  fable's strictly-stronger dtype/layout ENUMs; re-apply master's R3 as a
  semantic patch over fable content). Dispatch a dedicated xhigh recon worker
  AFTER the crash lane merges; destruction-review; then tfam's fable-merge
  becomes conflict-free adoption.
- Known repo debris (tfam-lane, untouched): conflicted bookmark maki-layout-valid;
  divergent changes xvznslzy/xxxqnrku/klwoxorp/snwrzmwt/wmromqny/zkmpnxlk/rxxvroyr.

## PROTOCOL AMENDMENT 2026-07-12: stage-then-fan-out (user-directed)

Multi-file missions are NOT one long worker. Decompose every dispatch:
1. CORE stage (serial, one worker): resolve the semantic core / policy
   decisions / target API on the minimal file set; publish the contract in
   the report.
2. FAN-OUT stage (parallel workers): dependent file clusters under DISJOINT
   file ownership - one workspace is fine when ownership is disjoint; brief
   each worker with the stage-1 contract.
3. INTEGRATE stage (serial): one agent runs the exact-tree gate ladder.
Reviews run as a PANEL of disjoint lenses (loss-hunt / acceptance probes /
semantic attack / generalist), not one generalist. Fix rounds fan out one
worker per disjoint finding cluster. Only the core contract, the merge commit
itself, and the bookmark/push window are inherently serial.

## STATUS 2026-07-12 evening (R3 merge landed)

- fable@origin = 1842573c "Merge master R3 nominal kinds; keep enum dtype/layout":
  master's Model-CAD V2 R3 campaign reconciled per the (closed) merge-policy dot -
  CAD-KIND identity/index kinds adopted, fable's dtype/layout/opkind/align ENUMs
  kept (strictly stronger; master's refine/raw converters dropped), 4-reviewer
  destruction panel clean (0 crit/high/med), fix round added executor stale-id
  negatives + MP-SLOT reset. Gates on the exact tree: maki 89 PASS, test/run.f
  PASS, six lints + trust-lint 564/651/0 + inventory 969/0 + boot-pin 32 + stale +
  typed-local all green. Crash lane also landed (internal-word gate, DNAME-INT;
  bare U-TYPE now rc 70 diagnostic).
- tfam ADOPTION: fable now CONTAINS master 441b834e entirely - their fable merge
  is conflict-free adoption. Dots habu-for-tfam-boot-1a6fba72 (their boot-pin
  tool is latently red until they merge) still open for them.
- OPEN maki-lane queue: habu-maki-skey-typed-0cc6f543 (SKEY typed columns +
  evidence rows; V2-supersession probe FIRST), habu-maki-audit-raw-25d3bf5e
  (*-RAW boundary audit), habu-maki-apply-cad-27b7a7d7 (region kinds for fusion
  path). Type-system-lane dots filed: habu-habu-certified-words-84e84eaf (FOO2),
  habu-checker-in-body-af7cf855 (DSL openers), friend-latch note in
  habu-seal-set-check-b3676b33.
- PROTOCOL now in force: stage-then-fan-out (see amendment above) + dot claims
  (dot on + push at dispatch; dots skill ~/.claude/skills/dots).
