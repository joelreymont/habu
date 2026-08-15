---
title: Seed the chain behind one prefix require
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-10T18:56:46.248678+02:00\""
---

ORIGINAL DESIGN (kept for the record, now refuted below): append 'require src/compiler/native/migrate.f' into the prefix buffer via the existing C-SOURCE-APPEND-X4-TO shape (PFX-APPEND-ENGINE-SNAP-HOOK precedent) - ONE row, ~40 bytes of IBUFSZ; the closure loads through include.f's own buffers (53 flat rows = 87.5% of IBUFSZ, forcing 4->8MB). Cost: +1.24s per cold bin/hb, +7464 dict records, +1.2MB code, +1.5MB DATA. docs/bootstrap.md gains: gforth stays a pre-chain recovery host and stays correct only while the engine can compile without the chain. Original acceptance: boot ndict 13,2xx; NMIGRATE:DEFINE works at first user token; full gate green; byte fixpoint. Files: src/habu/habu2.f. Depends: Stage A (landed), Stage C (landed).

PRE-MEASURE (2026-08-11, kqueue process-tree watcher cross-checked against lib/process.f PROCESS-TRACE): one full battery = 652 completed bin/hb boots (install 2, test/run.f 603, maki 47). At +1.24s per boot: +808s CPU, predicted battery wall 226s -> ~420-450s (+86% to +99%). Resident pool phases fork and pay nothing; the boots are dominated by members spawning fresh engines.

REFUTED END TO END (stageb-design lane 2026-08-11, full prototype): the one-require-row design was BUILT (chain seeded in the cold prefix, engine rebuilt, byte-identical x2, NMIGRATE:DEFINE at first user token - the original acceptance passes) and the full gate run on it: 503s wall vs 194s baseline (+159%), 8 RED phases on wall-clock ratchets. Worse than the census estimate, and the estimate missed that ratchets BLOW, not just slow. Do not land this design.

WHERE THE 1.27s GOES (sampled at 1kHz + independently corroborated): checking 81% (2001-definition probe: 25us/def unchecked vs 135us/def checked; the 'prefix is unchecked' window covers only the first ~15 checker-prefix files - src/core/check-hook.f:138 installs the cert hook and EVERYTHING after loads checked, including any chain row; of the checked cost ~90% is checker.f itself), mprotect 12% (whole-8MB region toggles at 8.6us/call, two per definition, 119ms measured; narrowing to pages ~150ms of the 1270ms - LPROTREC narrow-flip precedent habu2.f:2179), file I/O 0.08%. Ceiling of pure load-cost cutting: 5.3x, not 10x.

STRUCTURAL FACTS any successor design must respect:
(1) Stage B never seeds the METABUILD host - src/habu/build.f:62 routes stage/maker engines to C-SOURCE-BAKED = base prefix only (no stdlib, no shared row); so the pre-window class does not bite at Stage B, AND the cut still needs a host-side seeding nobody has scoped - harder, because the base prefix lacks the stdlib the chain requires.
(2) The AOT seed is TTY-ARMED (habu2.f:7305, armed only at SRC-REPL habu2.f:1433; proved by experiment: baked-only words are E-UNDEFINED under --load and piped stdin, work on a pty). BAKED code serves 0 of the gate's 323 engine boots until the arming contract changes - a PRODUCT decision (it changes what names exist in every batch program's dictionary); escalated to the user.
(3) The gate's 991 forks already collapse what can collapse (505 execs, 323 engine binaries; the insn gate's 167 are FORKS inheriting a booted image, not boots). Keeping +1.27s/boot under +10% of a 194s gate needs <=15 boots - unreachable by spawn reduction.
(4) Lazy-load post-cut triggers on every colon definition (win ~0: migrate.f:987 DEFINE-HELD "IS THE ENTRY THE CUT NEEDS"); a load inside an open compile re-enters the checker against live CTOR-PEND/tape/package state - the only atomic trigger is a quiescent one, which is Stage B again. One unprobed rescue: does `require` inside an open `:` work? Cheap probe; could revive lazy for the pre-cut window only.
(5) 0 set-check around the chain dies 3/3 at checker.f:10938 CTOR-PEND-REQUIRE-DONE (69 declaration lines across 20 files) - a certified-source load mode must stand the declaration machinery down too, and BF-CERTIFY today covers stage2/stdin/snap sources, not the disk prefix or the chain closure.

THE REFRAME (ruled 2026-08-11): the deliverable is not "seed the chain" - it is AN ENGINE BOOT MUST NOT RECOMPILE THE COMPILER FROM SOURCE. First consumer: the chain (+1.27s). Second: the prefix itself - a 15.8MB snapshot restore boots in 0.015s where the source-prefix boot takes 0.40s, so the gate's 323 boots spend ~124s recompiling the prefix before any test runs; warm images are retired by user decision (3098fa63) and the tree's chosen replacement IS the AOT-seed path. SEQUENCE: (a) rule habu-aot-has-no-0b01043c (gates every baked route); (b) AOT format widening, own dot (u16 site/name offsets -> u32 across call-site/DATA-site/CODE-site/XT tables + boot-side walkers, AOT-BLOB-CAP 64KB -> MBs vs the chain's 1.15MB, AOT-REC-MAX 256 -> 7000+, AOT-NAMES-CAP 16KB -> ~51KB, EXT-name records which capture refuses today at aot-capture.f:172; acceptance: capture and boot a blob > 64KB); (c) the arming-contract decision (USER); (d) bake the chain; (e) bake the prefix. LAND NOW independent of all of it: LPROT code-region narrowing + (PROT-SPAN) guard cost - ~20% of every boot today, pays on the 323 boots the gate already runs.

Probes, logs, gate telemetry: session scratchpad probe/ and stageb-lane/ (stageb-design lane report has the full method per number).

RIDER (prewindow landing 3443a30d): the CHAIN's own inliner
(src/compiler/native/inline.f) has no equivalent below-window
decline - not a live gap today (the chain never compiles window
code), but this leaf must re-derive it before the chain enters the
capture window. The engine-side model is C-CALL-SCAN-SAFE +
AOT-WINDOW:EMIT-OUTSIDE (arithmetically-accepting unarmed state).

Claim: agent=bake-chain-4 workspace=.jj-ws/habu-bake-chain

RULED 2026-08-14 (bake-chain checkpoint, probes /tmp/bc-probe/):
the metabuild-host capture entry is REFUTED BY EXPERIMENT - the
host dictionary is not the target's (three ordered deaths:
ARM64-W32 dup, ENGINE-ERROR dup, then regalloc.f's BMAX binding
to the ENGINE'S max EMITTER - icode: label redefined; a colliding
constant would have baked wrong data silently; and host-captured
ASM: names do not exist in bin/hb so the seed's LFIND rejects at
boot). RULING: the TWO-PROCESS design - capture in bin/hb itself
(window opens at the first user token, ndict 6993/WIDN 276 = the
seed point; names resolve in the booting engine BY CONSTRUCTION;
137 chain WIDs all land above the target's WIDN - measured), the
metabuild host reads the serialized artifact and bakes it
unchanged. This lane's implementation scope: the artifact format
+ writer + reader + the BYTE-FIXPOINT DETERMINISM PROOF for a
two-process capture; DATA-CAP derived-lifted (window DATA
measured 1.53MB vs 1MB cap); provided rows for the 44 in-window
files; boot-run reach to A64RAV:DKEEP-HOOK (the one declared
cell); prove the armed decline empties the 26 out-of-window
chains at capture. PRECONDITION dotted separately: ADR reach -
the REAL blob ceiling (the +-1MB ADR field binds before the 2MB
blob cap; section reorder cannot alone cover 2.7MB of payloads;
a far-address form is needed, with Rocq rows per CG-02 if a new
instruction form enters the emitter). Capture-side O(sites x
dict) in ACAP-TGT>REC dotted (the boot gate's disease, same
cure). Inliner rider VERDICT: not this lane - inline.f splices
tokens not addresses; the real hazard is the chain publication
seam baking pre-window literals, which cannot arise until the
cut and lands with it (recorded).

HANDOFF 2026-08-14 (bake-chain lane, deep-session stop at the
foundation blocker; the C commit 754b81e0 sits STACKED in
.jj-ws/habu-bake-chain on master 9eb6437a - the fresh lane
inherits workspace and commit). FOUNDATION BLOCKER, scoped
exactly: aot-capture.f cannot load in plain bin/hb
(E-UNDEFINED: AOT-CODE-B0) - 17 of its 73 referenced symbols
are declared in habu2.f (the AOT-*-BUF/-CAP/-MAX set,
AOT-CODE-B0, AOT-DATA-D0, AOT-REC, the *-LEN/*-N siblings, the
AOT-WINDOW:/AOT-XTSITE: packages); the PROT-/SNAP-RELOC:
layout constants already load (verified). IMPLEMENTATION ORDER
for the fresh lane: (1) EXTRACT the 17 declarations + the
AOT-SECTION:BYTES+AGREE budget into one shared file loaded by
both the engine builder and the capture tool - MECHANICAL
(declarations not logic), LINT-GATED (stdin-closure-lint
rejects an unwired host file), EMIT-AOT-SEED stays put so the
section-reach lint's label set is unaffected; RISK: the caps
AGREE sums must TRAVEL TOGETHER with the budget - split files
are the drift AGREE exists to stop. (2) The capture tool in
bin/hb (window at first user token). (3) Writer/reader with
the DATA-CAP lift riding the writer commit ($100000->$200000
obligates AOT-SECTION-CAP $400000->$500000 and CODE-CAP-BYTES
$900000->$A00000 - AGREE proves the chain or dies named).
(4) The PROMOTED acceptance: cross-process double-capture
sha256 comparison IN the build, fail-closed - NO band gate, NO
band diagnostic (ruled; the block A/B localizes by hand).
(5) Fixpoint loop (build->capture with new engine->rebuild->
converge, referencing 05728727's partial coverage). (6)
Provided rows generated from the artifact's own closure list.
(7) Battery. Boot-side DKEEP-HOOK re-install rides the bake
commit. All prior rulings on this leaf stand.

ITEM (1) COMPLETE 2026-08-15 (bake-chain-2, stack in the
workspace on d50dfa80-era base): aot-decl.f landed as ruled
(package AOT-BUF + AOT-WINDOW/AOT-XTSITE + the AGREE budget +
SNAP-RELOC's five SHAPE constants - the arbiter proved they are
format not emitter; the emitters correctly stay host-only);
habu2.f -227 lines with top-level using; all four lists + the
manifest wired (three tool files gained packages - ownership
debt paid, zero cascade); reloc pins repointed (own commit).
Fixpoint d1585042 x4 byte-identical; full battery green ON THE
STACK. CRITICAL FOR THE CONTINUATION: the aot-wid-build.f
fixture repair (DRV-AOT-CAPTURE emitting opener+import at all 8
sites + DRV-IMPORT-CHECK counting structure over the assembled
buffer, mutation-proved rc 74) was LOST to the pool incident -
master now carries the REVERT (df09c744), so the repair MUST be
re-applied INSIDE the extraction commit at rebase time or the
battery reds on aot-wide-format. Item (2) handoff: the capture
tool is a NEW tool sharing the format (aot-capture.f loads in
bin/hb now but calls host-only CARRY-SITE/EMIT-OUTSIDE);
prelude asm+icode+layout+aot-decl (+38ms). Items (2)-(7)
otherwise per the earlier HANDOFF section; all rulings stand.
POOL RULES (from the incident): a worker's jj new/squash is
unsafe while another lane holds an open working copy; after any
rebase, diff the stack for foreign paths.

PREMISE CORRECTION 2026-08-15 (bake-chain-3 checkpoint): the "26
out-of-window chains" are NOT chains - they are stale recorded-
address bits from pass-2 recompile (dot: clear-the-address-map
P1, blocks this leaf); the armed-decline acceptance clause is
REPLACED by that dot's stale-bit regression. Measured for item
(3): chain window DATA span 1,531,272B (the $200000 lift is
real); code 1,194,680B and 6586 records fit with room. Item (2)
prelude re-measured in-boot: ~100ms, 475 records, aot-capture.f
loads and self-tests IN bin/hb - the two-process premise holds.
OPEN RULING for the tool (item 2): the window starts at the
first user token and therefore CONTAINS the prelude's words
(asm/icode/aot-capture) which do not exist in the target - any
window-to-prelude call would fail LFIND at boot; did not bite in
the probe but is unproven. The tool must carry an explicit
fail-closed assertion (design it at the tool commit: every BL
target inside the window resolves to a window record or a
TARGET-prefix name, refused by name otherwise).

FOUNDATION MERGED 2026-08-15 (master 5b155731, four commits: C
storage-on-demand, aot-decl extraction + fixture repair, reloc
pins, the four-term cap lift with AGREE and MACHO-MSIZE gates
mutation-proven). The capture RUNS end-to-end over the chain
window in bin/hb: recs 6586, sites 18602, blob 1194680, dsites
3536, csites 16, xtsites 0, xtoff 1 (the predicted DKEEP-HOOK
cell), datasz 1531275 - every buffer has headroom. ACCEPTANCE
CLAUSE RETIRED: the armed-decline-empties-the-26 clause is
doubly refuted (decline never involved; cure was the map-rewind
fix) - test/p2-map-rewind.f is its replacement. PRELUDE-HAZARD
RULING for the tool commit (items 2+): the assertion is
TWO-SIDED and structural - the tool records four marks (ndict
and DP at prelude-start and at window-open) and refuses BY NAME
any site landing in the prelude band on either axis: (a) every
BL target resolves to a record BELOW prelude-start or INSIDE the
window - a target in [prelude-start, window-open) names a word
absent from the booting engine; (b) every recorded DATA address
lies below prelude-d0 or inside [d0,d1) - a window word holding
a prelude buffer's address would bake a dangling pointer that
ACAP-UNCLASSIFIED cannot see. Both audits run over the FULL site
populations (18602 + 3536), fail-closed, at capture. Items
(2)-(7) otherwise per the standing handoffs; the fourth-coupled-
term lesson (CODE-CAP feeds MPAGE feeds signature feeds MSIZE)
recorded.

BAND LANDED + THREE RULINGS 2026-08-15 (bake-chain-4 reviewed
hunk-by-hunk, gated, merged). The two-sided audit is in
src/habu/aot-capture.f: PRELUDE-MARK is MANDATORY (unmarked
capture refuses), the call audit refuses by name over all sites,
and the DATA half is a CLASSIFICATION on the existing
ACAP-UNCLASSIFIED refusal - a second refusal was tried and
refuted (the D0-SKEW forge made it steal the first refusal's
only producer; one die line keeps both tested). Suite
aot-prelude-band (7 child cases incl. the NAMER string-not-call
fixture). AOT-REND renamed AOT-RLEN ([8] is code LENGTH -
verified against EM-AOT-REGISTER-RECS, habu2.f:4185).

RULING (tool ordering): prelude-first is REFUTED BY MEASUREMENT
- 98 of 18602 call sites resolve into the prelude band (first:
chain word MASK calls A64ASM's LIMM?; asm.f is in the chain's
own closure, so prelude and chain share it and every such name
would fail LFIND at a shipped boot). The FOUNDATION-MERGED
"end-to-end" capture was producing an unbootable seed. ORDER IS
CHAIN-FIRST: window opens at the first user token, migrate.f
(with asm.f) inside it, icode/aot-decl/aot-capture load after it
closes - orchestrator-reproduced: 0 violations, recs=6764
sites=18939 blob=1215872 dsites=3717. Arming ownership: mint
src/habu/aot-arm.f - ONE word owning the AOT-WINDOW:D0-CELL/
B0-CELL write, required by aot-capture.f (WINDOW-OPEN delegates
to it) and loaded by the tool BEFORE the window opens (it needs
only layout.f, proven checked). Manifest slot in
tools/stdin-closure-lib.f + both stdin builders - the aot-decl
shape, already paid once. Two writers of those cells is drift;
refused.

RULING (one blob, two windows): the artifact READER MERGES.
The reader appends the artifact's blob/records/sites into the
host's live capture buffers with the three offset shifts, and
rebases the artifact's DATA and CODE literals into the host's
coordinates so EMIT-AOT-SEED still bakes ONE blob, ONE name
pool, ONE (D0,size) pair. The seed frame does NOT grow a second
span - the boot-side machinery stays untouched; merge complexity
lives in the reader, which is new and fail-closed. This is what
puts DKEEP-HOOK on the boot-run list (item 5). The
below-prelude-d0 address class: today's audit refuses it and the
measured population is empty; if the merge design ever needs
that class, the argument is made HERE explicitly, not inherited.

RULING (fixpoint host): once the chain is baked, require
migrate.f is a no-op in the product and include dies on
duplicates - the product engine can never capture its own chain.
THE METABUILD EMITS TWO ENGINES from the same captured prefix in
one run: the CAPTURE HOST (no artifact baked - today's bin/hb
shape, a build artifact, never shipped) and the PRODUCT
(artifact merged and baked). The emit differs only in the
artifact parameter - no mode flag, no second prefix. Fixpoint:
gen N emits the capture host, capture runs in it, artifact A_N,
product = prefix + A_N; CONVERGED when sha256(A_N) =
sha256(A_N-1) - which is also the promoted cross-process
double-capture acceptance, run IN the build, fail-closed.
