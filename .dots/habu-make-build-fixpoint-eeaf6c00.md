---
title: Make build-fixpoint install produce a usable engine
status: active
priority: 2
issue-type: task
created-at: "2026-09-18T13:38:13.305569+03:00"
---

Problem: three lanes on 2026-09-18 rebuilt with 'tools/build-fixpoint.f -- install --force' (private host, HABU_FIXPOINT_ENGINE private) and got a ~5.4 MB engine that fails where the tools/native-build.f product image (3,997,888 bytes) passes: test/gate-stdlib.f dies at load with 'duplicate definition: STORE' rc 78 because the engine does not provide src/core/quotation-storage.f the way the release engine does (span lane handoff), tier-1 compilation dies 'bad arch tag' even from pristine sources (div-trap lane), and ncomp reports unresolvable CC-N (field-accessors lane). docs/bootstrap.md 'What the engine carries' still calls the install route the reference native chain and the fresh-workspace recipe. Acceptance: measure the two routes from one pristine tree and one host (release engine): sizes, provided-file lists (src/habu/native-runtime.f manifest vs what install bakes), the STORE duplicate, a tier-1 ': INC 1 + ;' run; fix the responsible verb in tools/build-fixpoint.f / build-fixpoint-refresh.f so an installed engine provides the same closure as the product image, or, if the install route is meant only for the warm dev snapshot, make it refuse to install over a product engine and rewrite docs/bootstrap.md so native-build.f is the documented refresh; a regression in tools/build-fixpoint-test.f that loads test/gate-stdlib.f on the installed engine. Files: tools/build-fixpoint.f, tools/build-fixpoint-refresh.f, tools/build-fixpoint-test.f, docs/bootstrap.md. Verify: the fixtures; three generations with cmp; test/run.f. Depends: none. Ownership: build tooling. Claim: unassigned.

Claim: alder, .jj-ws/alder-quotation-declarations (base b4efad25). The original
route comparison remains in .jj-ws/alder-fixpoint-install (base 39d9a399).

Measured on pristine 39d9a399, with separate private copies of release engine
2bb46473 and scratch HOME/HB_TMP under /tmp/alder-fixpoint-install:

- `install --force` reports success and produces 5,505,216 bytes; the ordinary
  `tools/native-build.f` route produces 4,391,104 bytes. The installed engine
  is private at `/tmp/alder-fixpoint-install/install-engine`, never promoted.
- `1 set-tier : INC ( n -- n ) 1 + ;` crashes the installed engine (rc134,
  its handler reports SIGSEGV); the native product runs `41 INC` and prints 42.
- Gdb stops at PC 0x410a5c (`ldrb x9,[x9]`, x9=0x40). The return address
  resolves through the live dictionary to CHECKER-COLON-SCAN+108. Captured
  return sites include CHECKER-QUALIFIED?, CHECKER-FIND-ACTIVE-SYM,
  CTL-FLAGS, CTL-DEAD?, CHECKER-OWNER:DEAD-TOKEN?, NDICT:SPELL-DEAD? and
  NBACK:PROTOTYPE. The HIR:NAME address on the stack is its inline string,
  not a return address. Direct DEAD-TOKEN? on `s" +"` succeeds; direct
  HIR:NAME prints `hir`. This is narrower than a generally broken checker.
- Breaking before NBACK:PROTOTYPE's execute (0x1828bd8) shows the restored
  P-PROTOTYPE callback is 0x1881a74. That address starts inside a body,
  without the quotation's frame prologue, and reaches NDICT:SPELL-DEAD?
  with the compiler context (0x40) in place of a string pointer. Re-running
  A64PASS:INSTALL avoids this SIGSEGV but then exits 85 (`hb: bad arch tag`);
  this is diagnostic evidence, not a repair. Compare captured and restored
  callback addresses to isolate the faulty capture/relocation boundary.
- The same image's A64PASS:INSTALL embeds the correct quotation address
  0x1881318; the restored P-PROTOTYPE cell instead contains 0x1881a74. A reduced
  TYPED-VARIABLE callback store adds zero declared address rows on the private
  install capture host (tier 0), but one row on the native product (tier 1).
  Both call the stored quotation successfully before capture. Native lowering
  alone routes quotation stores through QUOTATION-STORAGE:STORE; the typed
  storage definer zeroes its allocation without declaring the quotation cells.
  The observed address is consistent with an unrelocated raw capture address.
  The responsibility is the typed storage declaration, as previously described
  by e92b0571 (absorbed into C1); coordinate that core seam with Hazel before
  changing it. Probes: store-mark.f and gdb-install.log in the private root.
- ENGINE-PROVIDES lists differ only in src/habu/repl.f and
  src/os/linux/repl-term.f (present in the native product). The earlier
  missing quotation-storage/STORE symptom does not reproduce on this head.
- Loading test/gate-stdlib.f under the installed engine crashes its native
  fixture writer. This entry includes a registry-ending RUN: it is a full
  gate, not a definitions-only load. The product comparison unintentionally
  entered the full gate and was stopped; no full-gate result is claimed.
  Future regression must request the serial gate slot or explicitly load
  only the required dependencies. Scratch XDG_CACHE_HOME must also be set.

Artifacts: install.log, native.log, gdb-tier1.log, install-tier1.log,
install-provided.log, native-provided.log, owners.log in the private root.
No systemd coredump was present; gdb read the stopped process directly.
No resource-exhaustion evidence (65GiB available; no OOM kernel entries).
Hazel's rule remains: no further install --force runs, and no writes to any
shared engine. Continue via private candidate generation and focused probes;
full gate belongs to Hazel's serial integration chain.

Repair: TYPED-VARIABLE and TYPED-BUFFER declare every directly typed quotation
cell with the existing xt! declaration when allocating it, including null cells.
This is the same primitive QUOTATION-STORAGE:STORE uses for image DATA, available
before that later compiler module loads. No tier-0 store hook or new primitive.

Private proof at /tmp/alder-quotation-declarations, all source from b4efad25 plus
this repair, with HOME/HB_TMP/XDG paths isolated:

- Three native product generations are identical, each 4,391,104 bytes;
  SHA256 e96e1d4b29e000650ece910df35e925241878aff1e9531a39af51aea31c354de.
- `tools/build-fixpoint-refresh.f -- stdin` builds the install route's private
  candidate without installation. Compiler fixpoint and two-process capture
  identity pass. Its hb-stdin is 5,505,216 bytes.
- row-probe.f prints declaration deltas 1/3/0 (quotation variable, three-cell
  quotation buffer, numeric variable), store delta 0, then 42 under the capture
  host at tier 0, the native product at tier 1, and the seeded stdin candidate
  at tier 1. The formerly failing callback dispatch now compiles and executes.
- capture-probe.f stores a quotation with ordinary tier-0 ! into declared
  storage in hb-host, captures it, and restores it through the production
  source writer. The fresh callback-image prints 42.
- typed-storage passes both tiers; typed-storage-structural, xt-cell,
  layout-buffer, native-stored-quot (tier 1), certify-dynamic-buffer and
  program-diagnostics pass. A fresh private whitebox also passes
  snapshot-xt-cell-decl and the whole native-window-owner row. The seeded
  stdin candidate passes typed-storage at tier 1. No full gate run here.

Astra review found an evaluator rollback prerequisite: catching an enclosing
evaluation that successfully declares storage and then throws restores DP but
leaves the address row. Reusing that address with the opposite kind exits 99.
This already affects defer and PERSISTED-PTR-VARIABLE; declaration-time marking
extends it to typed quotation cells. Reproducers are
/tmp/alder-quotation-review/{typed,defer,persisted}.f. Coordinate the shared
rollback fix 620cbf86 with Hazel before landing; do not hide it in a storage
wrapper.

After the declaration and rollback repairs integrated at d3c99b9b, the fresh
private candidate in /tmp/alder-install-final/tmp/hb-stdin passes the original
tier-1 INC probe (42), declaration counts 1/3/0, store delta 0 and callback 42.
It is 5,570,752 bytes, SHA256
39dba34d4211cdefb98882a5d7c2dd685fba78f9b47a8c4c70b84c5da667a5ea.
The stdin refresh reports compiler fixpoint and two-process capture identity
green. Its provided-file set differs from the product only by repl.f and
linux/repl-term.f. A fresh tier-0 hb-host capture of the callback, baked through
tools/aot-chain-bake.f, restores and prints 42 in a new process. The artifact
is callback.aot and the resulting executable is callback-tmp/hb-chain.
No installation, shared engine write or full gate was performed; Hazel's
serial slot was requested for the candidate's remaining full-gate proof.

Hazel's private d3c99b9b gate of that candidate fails at load, before any
suite: `duplicate definition: STORE at src/core/quotation-storage.f:14`,
exit 78. The stdin product carries the quotation-storage definitions but its
require behavior permits the tree to redeclare them. This is the same failure
class as d00185b1. The raw REQUIRE-BOOT-N listing above is not proof that the
candidate provides the same modules to the tree's real load path. eeaf6c00
remains open; fix this module-provision failure and repeat the candidate gate.

Reduced without running a gate: the same candidate successfully runs a file
containing `require src/core/quotation-storage.f` from its original build tree,
but the identical file fails with duplicate STORE (78) from an exact private
copy at /tmp/alder-install-final/other-tree. SOURCE-ROOT:CWD$ correctly names
the copied tree there. This is a relocation/provision failure, not an absent
row in the raw registry listing. Scratch probes: require-quote.f, root-probe.f.

Portable-capture repair: alder, .jj-ws/alder-install-portable, base de37c3e6.
LATCH-CLOSURE was copying absolute application require rows into the artifact's
provided-file list. Boot registration shortened them only when its CWD matched
the capture tree, concealing the problem in the original listing. Normalize
with SOURCE-ROOT:CWD$ RELATIVE while capturing, matching the loader's existing
boot-row rule and retaining canonical paths outside the tree.

The production-capture fixture rejects the original rows with
`chain-closure: captured a build-tree absolute path` (75). The full
aot-chain-capture registry row passes on the repaired de37c3e6 tree. Independent
Astra review is clear. The fresh private stdin candidate recognizes the copied
tree's quotation-storage.f through ENGINE-PROVIDES? and requires it successfully
(42), where the prior candidate exits 78. No install or local full gate was run.

Three private stdin-route generations (tools/build-fixpoint-refresh.f -- stdin)
are byte-identical, SHA256
5c30c4a0fc46a18ce53de57d44edb202a869dd0c7022f1bdfecf14c8bb1c5a10.
Generations 2 and 3 build from the copied root using the preceding candidate,
also copied to that private tree's bin/hb because 5f3468a9 is still open.
Artifacts and logs: /tmp/alder-install-portable-final/{gen1,gen2,gen3},
aot-chain-capture.log and relocated.log. The final candidate still needs
Hazel's serial full gate; eeaf6c00 remains open until that proof passes.

Relocated-tree gate of /tmp/alder-install-portable-final/gen3 over a copy of
its matching tree: Hazel reports 426 PASS and 51 FAIL lines. Read-only source:
~/.cache/hazel/logs/gate-IN.log; gate exit 1 after 1240 s. These include nested
fixture FAIL labels, not 51 independent root causes. The earlier load-time
STORE duplicate (78) is gone; the product still is not gate-equivalent to the
native route. The native route remains the release route, and eeaf6c00 stays
open. No install or new local full gate was run for this evidence update.

Representative failures: `ncomp: cannot compile TO-RECOVERY? at
CHECKER-EFFECT-AUTHORITY:RECOVERY-USED?` followed by -8286;
`install-repl-read at 'is': non-certified definition`. The final pool report
classifies build-fixpoint-fixtures as TIMEOUT-UNDER-LOAD (360177 ms); do not
attribute that row to a semantic defect without a focused reproduction.

Complete FAIL-label list from gate-IN.log, in log order:

```text
compiler-native-tape-owner
compiler-native-string
print-min-int
stripped-address
' word , cell: refusal reason
aot-data-cell-refusals
stripped-quotation
stripped retained literal build
stripped-literal
aot-xt-cells
stripped default global MAIN build
stripped-entry
native-resource-image
native-defer-image
process-image
stripped-sparse-data
compiler-native-code-span
pointer-storage
effect-read-api
checker-scan-index
effect-intern
snapshot-writer
stdlib-standalone-load
aot-wid-restore
address-cell-tasks
repl-address-cell-rollback
engine-stack-debugger
aot-wide-format
genio
hb-build-fixtures
hb-open-failure
source-root
engine
breakpoint resumes across protection pages
debugger-resume
prop-test
native-gate-debug
test/engine-suite.f
program-diagnostics
aot-named-cells-image
boot-row
process/pty
engine-runtime-regressions
build-fixpoint-fixtures
maker self-test build rc
fork hb-build AOT bundle/data
fork hb-build AOT preseed
native-gate-aot-positive
field-proj
prop
structure-decl
```
