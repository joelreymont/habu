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

Claim: agent=bake-chain-8 workspace=.jj-ws/habu-bake-chain

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

ITEMS (a)+(b) LANDED 2026-08-15 (bake-chain-5, reviewed
hunk-by-hunk, gated, merged): src/habu/aot-arm.f - AOT-ARM:OPEN
is the ONLY writer AND the only name for the window-cell write
(the WINDOW-OPEN forwarder was minted, then deleted by ruling:
a second name for a one-writer operation is the one thing that
can grow a second body); manifest slot SDC-ARM$ in both stdin
builders. tools/aot-chain-capture.f: chain-first, marks first,
and a STRUCTURAL first-load check - REQUIRE-N minus
REQUIRE-BOOT-N must be exactly 1, asked of the engine's own
registry (run behind any other file, the marks would swallow
that file's records and the capture would exit 0 with an
unbootable seed - measured before the check existed). Suite
aot-chain-capture: order case is a mutant DERIVED from the real
tool file (fail-closed on its anchor), decoy proves the detector
reads exit codes not text. Census: recs=6764 sites=18939
blob=1215872 dsites=3717 datasz~1531450, capture 1.31s. Engine
+4B (CODELEN 129528), attributed by control build + producer
census instrumentation + foreign-change falsification. Payoff
re-measured: 1023ms/boot is what the bake removes (270 bare vs
1293 chain-compiling, 5 runs each). NOTED: an UNARMED chain
window captures near-identically today - the arm is a backstop
for the chain, not its mechanism. Items (c)-(g) remain: the
artifact writer/fail-closed reader (reader MERGES per ruling),
the two-engine emit, the sha-convergence fixpoint, provided
rows, battery + boot delta.

ITEMS (a)+(b) COMPLETE 2026-08-15 (bake-chain-5, two commits
stacked in .jj-ws/habu-bake-chain on master 3a74ac2b: 7701f63b
and c59e7879; full battery green ON THE STACK, fixpoint 24bce3e6
x2 byte-identical, 165367 bytes).
(a) src/habu/aot-arm.f minted: package AOT-ARM, one public word
OPEN ( b0 d0 -- ), the ONLY writer of AOT-WINDOW:D0-CELL/B0-CELL.
RULED at review and done: AOT-CAPTURE:WINDOW-OPEN is DELETED, not
delegating - a second name for a one-writer operation is the one
thing that can grow a second body - and its three callers
(stdin.f CAPTURE-REPL, aot-band-lib.f OPEN and OPEN-UNARMED) call
AOT-ARM:OPEN directly. aot-capture.f's now-unused AOT-CELL! TRUST
row is deleted with it; the engine bytes are unchanged by the
deletion (host-side only, proven by rebuilding). Manifest slot
SDC-I-ARM/SDC-ARM$ plus both stdin builders and the closure
lint's two consumer checks. Size ratchet paid IN the commit with a measured
attribution (control build of master in a scratch export
reproduced the pre-change engine byte for byte; only aot-seed
+4/text-pad -4 move; the metabuild capture census is identical in
eight of nine numbers and the ninth is the window's DATA span
5726 -> 5730, falsified against an unrelated pre-window edit).
(b) tools/aot-chain-capture.f: chain-first, line order IS the
contract, census on stdout (recs=6764 sites=18939 blob=1215872
names=49014 dsites=3717 csites=16 xtsites=0 xtoff=1
datasz=1531450, blob==codespan, datasz==dataspan, band 16 recs /
95 bytes). Suite aot-chain-capture registered + slice-selected
(schedule lint falsified by deleting the predicate); its order
case is a MUTANT DERIVED FROM THE REAL FILE (splice
`require src/arch/arm64/asm.f` before the window-open anchor,
fail-closed if the anchor is not unique), plus a not-first case
and a decoy that proves the suite reads exit codes, not text.
Four mutations red it (injection neutered 3, REFUSED ignoring rc
1, tool census inconsistent 1, first-load check tautology 3).

NEW STRUCTURAL FINDING, fixed in (b): the band audit trusts every
record BELOW the mark to be one the target has, which is true of
the ENGINE's surface and of nothing else. `bin/hb --load X.f
tools/aot-chain-capture.f` marks X's records as the engine's and
captures a SMALLER window that calls names no target defines -
measured: asm.f first gives code span 1194680 instead of 1215872,
exit 0, unbootable seed. The tool now asks the engine's own
registry instead of trusting a convention: src/core/include.f
freezes REQUIRE-BOOT-N at the end of the boot prefix, so
REQUIRE-N - REQUIRE-BOOT-N is how many files THIS process loaded
and the only acceptable answer is one. MEASURED SIDE FACTS the
successor needs: layout.f is already registered in a booted
engine (require = 0 records) which is why aot-arm.f may depend on
it; asm.f is NOT (178 records), which is why the chain must bring
it in; an UNARMED chain window captures identically today (the
arm is a backstop for the chain, not its mechanism); the chain
compile costs 1023 ms per boot on this host (270 -> 1293 ms).

REMAINING, items (c)-(g), unstarted. Authoritative section list
for the artifact is habu2.f EMIT-AOT-SEED (13 sections + the
scalars: blob/len, compact recs/n, sites/n, names/len, datasize,
data-d0, dsites/n, xtoffs/n, window DATA, code-b0, csites/n,
xtsite rows/n, boot-run, prot tag + bitmap). Suggested split for
the next lane: (c1) format + writer + fail-closed parse-and-
verify reader + round-trip suite with truncation / version /
digest / section-order fixtures - this alone unlocks the
fixpoint's sha comparison; (c2) the MERGE reader per the ruling;
(d) two-engine emit; (e) fixpoint loop; (f) provided rows (the
mechanism already exists: include.f REQUIRE-BOOT-FREEZE +
ENGINE-PROVIDES?); (g) battery + the 1023 ms delta. All rulings
on this leaf stand.

(c) PREMISE PROBES DONE 2026-08-15 (bake-chain-5, on merged
master 4a2fea12; no edits, nothing to commit - this entry IS the
result). Re-baselined census after the staging-road deletion:
recs=6744 sites=18893 blob=1212352 names=48767 dsites=3686
csites=16 xtsites=0 xtoff=1 datasz=1531042, blob==codespan,
datasz==dataspan, band 16 recs / 95 bytes, exit 0 - inside the
suite's floors, no test change needed. bin/hb 24bce3e6 unchanged
(the chain is not in the engine).
PRIMITIVES BOTH PROCESSES HAVE, so one shared format file can
serve writer and reader the way aot-decl.f serves two fillers:
SHA256 ( ptr u8 n ptr u8 -- ), SHA256>HEX, SHA256-RESET/-UPDATE/
-FINAL (src/core/sha256.f, in BF-APPEND-COMMON and in the baked
prefix - verified live in bin/hb against the "abc" vector
ba7816bf...), PATH0 (a checker PRIM, so it is everywhere), and
raw open/read/write/close (verified live: wrote 32, read back
32). NOT available in a booted bin/hb: driver-io.f's DRV-WALL
(E-UNDEFINED) - driver-io.f is a prefix source but its words are
not baked into the dictionary, unlike layout.f's, so a writer
that wants short-write safety either requires driver-io.f after
the window closes or carries its own loop.
STRUCTURAL FINDING - (f) IS A PREREQUISITE OF (c), NOT THE LAST
ITEM. The chain closure needs no new mechanism and no hand list:
the engine's own require registry already IS it. REQUIRE-N read
at window-open and at window-close brackets exactly the files the
chain loaded, and REQUIRE-SLOT/REQUIRE-LEN@ enumerate them in
load order - measured in the capture tool's own shape: pre=1,
window=43, starting src/compiler/native/migrate.f, abi.f,
target.f, digest.f ... ending publish.f, branch.f. That list is
(f)'s provided rows AND the only honest input to (c)'s "chain
digest", so the closure list should be built FIRST and the format
should carry it.
TWO QUESTIONS THE FORMAT CANNOT BE WRITTEN WITHOUT (the format is
baked into the engine, so it migrates once - guessing these is
how a weak identity check becomes permanent):
  1. PRODUCER SHA - over what? Proposal: sha256 of the capture
     HOST BINARY, which the two-engine metabuild (item d) has the
     path to because it just emitted it, and which the capture
     tool can be handed as an argument. The reader then refuses an
     artifact produced by any engine other than the one this build
     made.
  2. CHAIN DIGEST - over what? Proposal: sha256 over the ordered
     concatenation of the 43-file closure above, with the file
     LIST carried in the artifact so the reader can re-derive the
     digest rather than trust it, and so item (f) reads its
     provided rows out of the artifact instead of a second list.
  Also proposed, and NOT in the ruling, so it needs a yes: a
  third digest over the payload region. The ruled two identify the
  producer and the input; neither catches a partially written or
  corrupted file, and the header's size arithmetic only catches
  truncation. One SHA256 pass, 32 bytes.
Suggested build order once ruled: closure list + digests -> format
+ writer + verifying reader (round-trip acceptance: capture,
write A, clear the buffers, read A back, write B, sha256(A) =
sha256(B) - the same comparison item (e) promotes) -> (c2) merge
-> (d) -> (e) -> (f) reading the artifact's own list -> (g).

CLOSURE + DIGESTS LANDED 2026-08-15 (bake-chain-5, commit
6dcb71c4 in .jj-ws/habu-bake-chain on master 4a2fea12; full
battery green, fixpoint 24bce3e6 x2, 165367 bytes - the engine is
untouched, both new paths are tool-side). src/habu/aot-ident.f:
package AOT-IDENT, engine primitives only (SHA256*, PATH0,
open/read/close, the include registry) so the metabuild host can
load it unchanged when the reader needs it. CLOSURE! ( r0 r1 -- )
latches the require-registry span into a path table (refusing an
empty span and any path over the cap), COUNT/PATH$ read it, and
CHAIN-DIGEST ( ptr u8 -- ) streams the files' bytes through
SHA256 in load order. The capture tool latches REQUIRE-N at
window open and close and now reports closure=43,
first=src/compiler/native/migrate.f,
last=src/compiler/native/branch.f, chaindigest=3a3f8d0f... and
producer=24bce3e6..., the last being EXACTLY `shasum -a 256
bin/hb` - so the two independent readings ruling #1 asks for are
proven equal end to end, on a real run, not by construction.
Digest evidence: identical across two runs; changes when one byte
is appended to the FIRST closure file (27d3eafc...) and when one
byte is appended to the LAST (eafe16b5...); returns to
3a3f8d0f... when both are restored. Suite gained PROBE-IDENT
(closure floor, chain-root assertion, 64-wide digest, and the
producer double-reading computed in-suite with SHA256-FILE);
falsified by two code mutations - reporting the chain digest as
the producer key reds it, and latching the closure over an empty
span reds it.
NEXT: the format file. Header layout is unblocked now that all
three digests are ruled and the closure is derivable; the
artifact carries the file LIST from AOT-IDENT:PATH$ so the reader
re-derives the chain digest from disk, and (f) reads its provided
rows out of that same list. aot-ident.f needs a manifest slot in
tools/stdin-closure-lib.f plus both stdin builders at the moment
the READER lands in the metabuild host - it has none yet, because
nothing host-side uses it yet and an unwired host file is what
the closure lint exists to reject.

FORMAT RULINGS 2026-08-15 (the three questions, answered; the
format bakes once, so these are final):
(1) PRODUCER SHA = sha256 of the CAPTURE-HOST ENGINE BINARY,
computed twice as two independent readings of one fact: the tool
hashes its own running binary (the lib/engine-id.f path - the
same identity the cad-store key already uses) and writes it; the
metabuild reader recomputes sha256 of the capture host it just
emitted and hard-equality-refuses a mismatch. No path strings, no
timestamps - the bytes of the engine that produced the capture.
(2) CHAIN DIGEST = sha256 over the ordered concatenation of the
closure files' BYTES, in load order, with the FILE LIST carried
in the artifact - and the reader RE-DERIVES the digest from disk
rather than trusting the stored one. A mismatch means the chain
sources moved since capture: refuse, and the fixpoint loop's
recapture is the cure. RATIFIED with it: the closure list comes
from the engine's own require registry (REQUIRE-N bracketed at
window open/close, REQUIRE-SLOT enumeration - measured pre=1,
window=43) - never a hand-maintained list; and item (f)'s
provided rows READ THE ARTIFACT'S OWN LIST, making (f) a
prerequisite of (c) exactly as the lane found.
(3) PAYLOAD DIGEST: YES - sha256 over everything after the
header, verified by the reader before any section is parsed. 32
bytes and one pass buys a precise refusal where size arithmetic
only catches truncation; a fail-closed reader that can say
"corrupt" instead of "the sizes add up" is the ruled shape, not
new ceremony.
Also ratified: the round-trip acceptance (capture, write A, clear
the live buffers, read A back into them, write B, assert
sha256(A)=sha256(B)) - the same comparison item (e) promotes to
cross-process; and the DRV-WALL finding - the writer requires
src/habu/driver-io.f AFTER the window closes (it is prelude, above
the band) rather than hand-rolling a second short-write loop.
Build order as proposed: closure+digests -> format+writer+reader
-> merge -> two-engine emit -> fixpoint -> provided-rows-from-
artifact -> battery.

CLOSURE+DIGESTS LANDED 2026-08-15 (bake-chain-5b, merged
6dcb71c4): src/habu/aot-ident.f (package AOT-IDENT, engine
primitives only so the metabuild host loads it unchanged) -
CLOSURE! latches the require-registry span (43 files,
migrate.f..branch.f), CHAIN-DIGEST streams their bytes through
SHA-256 in load order, refuse-never-skip. The tool reports
closure/first/last/chaindigest/producer; producer proven equal to
an outside shasum of bin/hb (the two-independent-readings
mechanism), chain digest falsified four ways (byte appended to
first file, to last, both restored). PROBE-IDENT in the suite.
NOT yet wired into tools/stdin-closure-lib.f BY DESIGN - the
manifest slot lands with the reader, else the closure lint
rightly rejects an unwired host file. Next: the format file +
writer + verifying reader (round-trip sha acceptance), then
merge, two-engine emit, fixpoint, provided rows from the
artifact's list, battery.

FORMAT RULINGS AMENDED 2026-08-15 (bake-chain-6 checkpoint, two
premises refuted by measurement, both fixes ruled):
(1) driver-io.f is NOT requirable in a booted engine
(E-UNDEFINED: MBUF - it drags the image-writer closure incl.
macho/sign2). RULED: split the fd-write concern into a new
common prefix source, package FDIO, FDIO:WALL ( n ptr u8 n -- );
the 8 call sites (driver-io.f, aot-lib.f, snap-lib.f x6) move to
the packaged name; all five builder lists gain the file. The
flat-name shortcut (unpackaged DRV-WALL in a new file) is
refused - the Packages rule is blocking.
(2) aot-ident.f must load in the metabuild host but CLOSURE!
reads the require registry, and bootstrap.sh's stdin branch does
not append include.f. RULED: move the registry-bracketing loop
into tools/aot-chain-capture.f (its only caller, beside its
sibling refusals); aot-ident.f keeps one concern via
AOT-IDENT:RESET ( -- ) and AOT-IDENT:PATH+ ( ptr u8 n -- ) plus
COUNT/PATH$/CHAIN-DIGEST - ONE digest implementation both
processes call. Widening the recovery host to carry include.f is
refused: never widen a host to satisfy a dependency that can be
not created.
(3) FORMAT RATIFIED as tabled: 136B header (magic $00544F4155424148,
version hard-eq, target, section-count 13, payload-len, three
32B shas), payload = fixed-order 13-entry section table + bytes,
u64 LE, no padding, counts DERIVED from section lengths (one
authority per number; non-multiple lengths are named refusals),
scalars only AOT-DATA-D0 + canonical-zero AOT-CODE-B0 (per-
section sha census proved every payload byte identical across
three ASLR-shifted processes - the fixpoint has a measured
basis). Identity stays outside the format file: AOT-FILE:WRITE/
READ take (producer-key, path); the tool passes ENGINE-ID:KEY,
the metabuild the sha of the capture host it emitted.
(4) READER AMENDMENT (one addition to the two-pass shape): pass
2 re-accumulates the payload SHA-256 while filling destination
buffers and asserts it equals the header's payload sha at the
end - "the file did not change between passes" becomes a check,
like the header byte-identity assert, instead of an assumption.
(5) The poisoned-buffer round trip is APPROVED (stronger than
the ruled clear). The item-(2) flag is recorded: the merge runs
strictly AFTER the host's own capture; the artifact carries
compact rows only.

WID-SPACE HAZARD 2026-08-15 (bake-chain-6, P1 dot
habu-rebase-captured-wids-54dec421): captured records carry
capture-process wids; the seed registers them against the
TARGET's wid space. The "137 chain WIDs above the target's WIDN"
clause was measured, not enforced - host-only closure packages
shift host wids relative to the target (isolated: two dummy
packages moved a fixture wid 205->209 into the target's sealed
set, boot gate exit 84; an unsealed alias would misregister
SILENTLY). Items (d)-(g) MUST NOT ship on the accidental
alignment: they depend on 54dec421 (rebase at seed) or a
structural proof of the invariant. Interim ruled boundary: the
wid fixture refuses by name when its wid is protected in the
target.

FORMAT+WRITER+READER LANDED 2026-08-15 (bake-chain-6, merged
7a72b56f with fc1ae6f1 FDIO split): src/habu/aot-file.f owns the
ruled 136B-header/13-section format; artifact 3,095,842B, two
processes byte-identical, writer digest = outside shasum. 13
named refusals each mutation-proven incl. the forged-header
family; poisoned-buffer round trip on every write. FDIO:WALL is
the tree's one descriptor write; driver-io.f in package-lint
OLD-GLOBAL (self-retiring via acbd02b7). Wid boundary in
test/aot-wid-build.f per 54dec421. Host facts recorded in code:
hide.f retires true/false in the stdin host; driver-io.f
unreachable in a booted engine. REMAINING: (2) reader MERGE into
host buffers (artifact carries compact rows only - merge runs
strictly AFTER host capture) -> (3) two-engine emit -> (4)
sha-convergence fixpoint -> (5) provided rows -> (6) battery +
boot delta. Reader two-pass vs in-memory: revisit AT the merge
step with a measurement.

MERGE RULINGS 2026-08-15 (bake-chain-6 handoff, four decisions):
(1) ACAP-CHAINV/ACAP-SET-CHAIN move beside SNAP-RELOC's shape
constants in aot-decl.f - ONE chain reader/writer, three
consumers (relocation pass, capture scan, merge). A private copy
is the drift the existing comment forbids.
(2) READ and MERGE are separate entry points over one
section-loading machinery parameterized by per-section base
offset. READ replaces (adopts artifact D0/B0; the round trip
stays here); MERGE keeps the host's bases and rebases values.
Zeroed-buffer loading is NOT degenerate merging - hostD0=0 would
bake D0=0.
(3) Name pool: APPEND, no dedup (49,720 of 131,072 - the remap
pass buys <1KB). AOT-NAMES-LEN for a merged seed means the
concatenated pools; recorded here so item (6)'s size numbers
read right.
(4) The host's protected-WID bitmap is untouched by the merge;
the artifact's band is NOT merged.
ORDER AMENDED (the lane's own finding, adopted): the WID REBASE
(54dec421) lands FIRST, before the merge - the merge writes wids
into baked records, and rebasing first makes the record handling
simple instead of revisited. New order: wid rebase -> merge ->
two-engine emit -> fixpoint -> provided rows -> battery.
Five shift classes recorded (blob/name/DATA-offset axes + DATA
and CODE value rebases); call site STDIN-DRIVER:RUN between
CAPTURE-REPL and ENGINE-EMIT:FORTH; headroom proven (sums:
blob 1.23/2MiB, DATA 1.537/2MiB, recs 6859/16384, sites
19110/32768, names 49720/131072). When the sweep campaign trims
aot-file.f's header, its MEASURED facts (hide.f boolean
retirement, the MBUF route, forged-table reasoning) land HERE,
not deleted.

WID REBASE RULINGS 2026-08-15 (bake-chain-7 checkpoint; option B
of 54dec421 REFUTED by measurement - WIDN governs future
allocation, a captured wid 209 still registers into the existing
target wordlist at 209; both baselines reproduce on the real
build path; the unsealed alias puts AWBGATE's word into
LOWER-CERT's public wordlist, silently):
(1) THE FIX IS THE REBASE, wid as a window-relative coordinate,
exactly as proposed: capture latches [W0,W1); seed reads T0=WIDN
once, maps w=0 to 0, in-window w to T0+(w-W0), refuses anything
else BY NAME; WIDN=T0+span once (deleting the per-record
max/store). Two new baked scalars (W0, span) in EMIT-AOT-SEED,
LAOTDATAD0 shape; section-reach labels + AGREE budget updated.
The ARTIFACT carries the span: VERSION bumps to 2 - hard
equality is what the version field is FOR; a soundness fix is
the legitimate consumer.
(2) AOT-CAPTURE:WID-SPAN ( w0 w1 -- ) as a separate MANDATORY
latch mirroring PRELUDE-MARK - unmarked capture refuses. Four
call sites move.
(3) MERGE RULING (4) AMENDED: the artifact DOES carry its
in-window protected wids, as a window-relative u32 table the
seed applies AFTER rebasing (the chain's 51 constructor packages
must not ship unsealed - the seal threat model is checked habu,
and an unsealed ctor package is a real hole). The host's
PRE-window band stays untouched by the merge. EMIT-AOT-PROT-
RESTORE cannot do this (runs before T0 exists) - the new table
is the honest form. The wid fixture's gate mode 1 adapts to
protect through the new table so EM-AOTWIDGATE keeps a live
test; it must not go quiet.
NOTED: the chain clears the target's WIDN by exactly 4 wids,
owned by the capture tool's own packages - the accidental margin
the rebase retires. The PRE-window bitmap's host-numbering
soundness (correct only by shared load-order prefix) is
ef47ad69's ground; not this lane's scope.

WID REBASE LANDED 2026-08-15 (bake-chain-7, merged 3270f147, dot
54dec421 CLOSED): wids are window coordinates end to end -
mandatory WID-SPAN latch, capture-side audit by name, seed-side
rebase + single WIDN-by-span advance, window seals as a relative
table applied post-rebase, artifact VERSION 2. Six mutations red
named cases; one check deleted as redundant (unsigned span test
covers both sides). The four-wid accidental margin is retired.
Host prelude-band falsity dotted 5a992a38 (exit-81 class).
REMAINING: (2) the merge per MERGE RULINGS incl. amended (4)
(the PWIN table and wid span rebase through the merge onto the
host's window continuation) -> (3) two-engine emit -> (4)
sha-convergence fixpoint -> (5) provided rows -> (6) battery +
boot delta.

MERGE CHANNEL RULINGS 2026-08-15 (bake-chain-8 checkpoint):
(1) The artifact reaches the metabuild as a DRIVER PARAMETER
spliced into the generated source by the build driver (option 3):
STDIN-DRIVER gains one public setter (artifact path + producer-
key path, empty by default); RUN merges when declared, refuses
by name when a declared artifact is unreadable/unhashable/
wrong-keyed. No env channel (forth.md: the driver owns path
construction; no stale envp), no magic HB_TMP name. Item (3)
sets the same parameter from the capture host it emits.
(2) DKEEP-HOOK's boot-run entry is the CAPTURE TOOL's to declare
(chain names belong to the tool, as CAPTURE-REPL declares its
installers); A64RAV:DKEEP-HOOK-DEFAULT moves to public - no
wrapper (a second name for one operation is the deleted-
forwarder lesson).
(3) Acceptance is a new registered suite through the real
BF-EMIT-STDIN-RUN-SOURCE + build-fixpoint path, child-process
NMIGRATE:DEFINE probe, six shift-class mutations; the production
install lane proves the milestone again at item (3).
RATIFIED from the same checkpoint: two-pass READ stays (measured:
the second file pass costs 0.2ms, the second SHA 147ms, and the
second SHA IS the changed-between-passes check; 358ms once per
metabuild vs a 4MiB buffer is not a trade); the DSITE/CSITE
shared-buffer relocation order (host CODE rows move up BEFORE
loading; one section's base depends on another's length -
recorded so ruling 2's "per-section base offset" reads right);
package records' [0]/[8] take the WID rebase, never the blob
shift.
MERGE LANDED, MILESTONE BLOCKED BY A PRE-EXISTING GAP
2026-08-15 (bake-chain-8, three commits stacked in
.jj-ws/habu-bake-chain on master 9c23a67d: b5d4c151 claim,
1291bd60 the merge, 5a7d8191 the driver parameter).
(1) src/habu/aot-decl.f gains SNAP-RELOC:CHAINV/SET-CHAIN beside
the shape constants (ruling 1), expressed in ADDR-OPC-MASK/
ADDR-IMM-MASK/ADDR-RD-BITS instead of respelling them; the
private copies in aot-capture.f are deleted. The "relocation
pass" consumer is EMITTED MACHINE CODE (habu2.f:4951-4980) and
consumes the CONSTANTS, so the word-level reader/writer has two
consumers, not three. Proof the move is behaviour-preserving: the
artifact sha is unchanged, 70393ec8..., byte for byte.
(2) AOT-FILE:MERGE per MERGE RULINGS. READ and MERGE share one
LOAD-PASS and one section loader; the ONLY parameterization is a
BASE table (SEC-PTR now answers a BUFFER, never a position). The
five shifts plus the wid/PWIN continuation are all in, the host's
CODE rows are relocated before loading, package records' [0]/[4]
take the wid rebase, and the artifact's PRE-window bitmap is read
PAST (SKIP-SECTION) so the payload digest still covers it.
(3) src/habu/stdin.f: STDIN-DRIVER:ARTIFACT! (artifact path +
producer-engine path, empty = the capture host), MERGE-ARTIFACT
between CAPTURE-REPL and the emit, fail-closed once declared.
A64RAV:DKEEP-HOOK-DEFAULT is public and tools/aot-chain-capture.f
puts it on the boot-run list, refusing if the window's declared
address cells are ever not exactly the one it installs.
TWO-PASS READ MEASURED (the leaf's open question, now closed with
numbers): READ = 358 ms on the real 3.1 MB artifact = payload SHA
pass 1 147 ms + pass 2 147 ms + chain digest 62 ms + 2 ms of
everything else. A whole-file slurp is 0.2 ms and SHA-256 runs at
21 MB/s here. The second FILE pass costs 0.2 ms; the second SHA
costs 147 ms and IS the changed-between-passes check. Two-pass
stays; a staged read would buy 147 ms once per build for a 4 MiB
buffer.
ENGINE BYTES ATTRIBUTED: 165367 both sides, 341 bytes differ
against a control build of clean master (161e8f2b, which
reproduces bin/hb exactly). Every difference is one constant:
stdin.f allots ~800 more bytes of DP before the window opens, so
the REPL window's DATA base and every literal into it move by
800, and LAOTDATAD0 moves by the same 800 - the seed's rebase
subtracts it again. imgdump: identical dicts.

BLOCKER, AND IT IS NOT THE MERGE: a captured call site to a
PACKAGED word cannot be resolved by the seed. Measured on the
merged engine, which builds and then exits 81 with no diagnostic
at the FIRST user token: habu2.f:4401 EM-AOT-PATCH-SITES `pnf`
(exit $51, silent). Caught in lldb at 0x10000c588 - x13=0 (LFIND
missed), x22=219 (site index), x10=5, x9 -> the pool entry
`SLOT@`. That is CDIGEST:SLOT@, `public` in package CDIGEST
(src/compiler/digest.f:86), and ACAP-ADD-SITE stores the record's
BARE name, which the seed's LFIND looks up in the global
wordlist. Population, measured over the merged buffers: 18896
sites = 15421 whose callee record is PACKAGED + 3475 with no
merged record (prefix words, which resolve). So 82% of the
chain's call sites cannot be relocated today.
WHY IT NEVER FIRED BEFORE: the only window ever captured is the
REPL (repl/debug-watch/stepper/debug), which defines no package,
so every callee was global. The chain is packages nearly all the
way down. The capture-side band audit does not catch it either -
it proves the callee HAS a record, not that the name the seed
will use finds it.
COROBORATION IN THE TREE: test/aot-wid-build.f's gate modes put
the QUALIFIED name of a packaged word on the boot-run list, which
is the same wall met from the other side.
FIX DIRECTION, unruled and item-sized on its own: the site row
carries the callee's WID next to its name (8B row -> 12B, or a
parallel u32 table), window-relative, rebased by exactly the
machinery the records already use, and the seed resolves with a
wordlist-aware find. Artifact VERSION 3. Qualifying the name
instead is cheaper but only reaches PUBLIC package words; private
callees, which the chain has in quantity, still miss. The
capture-side audit that should have caught this at capture time -
"every site's name resolves the way the seed will ask" - belongs
with the fix.
DEBUGGER NOTE for whoever takes it: lldb breakpoints set BEFORE
`run` silently never fire on these images; `process launch
--stop-at-entry` first, then `br set -a`, then `continue` works.
The exit-81 class is ambiguous - layout.f BL-RANGE-RC is also 81,
and it writes a message where this one writes nothing.
UNEXERCISED BY EITHER SIDE: named code sites (xtsites) are 0 in
the chain artifact and 0 in the merge probe's window, so the
merge's shift of those two fields is carried by code and by no
measurement.

MERGE LANDED 2026-08-15 (bake-chain-8, merged 127cdbb6): READ and
MERGE share one LOAD-PASS + one section loader, differing in one
BASE table; nine shift-class mutations red named cases (sum-per-
row-family checks, not range checks); chain reader/writer unified
under SNAP-RELOC (two consumers - the emitted relocation pass
consumes the constants); driver parameter ARTIFACT! per the
channel ruling; DKEEP-HOOK on the boot-run list via the tool.
Two-pass READ ratified by measurement (second file pass 0.2ms,
second SHA is the between-passes check). Engine delta 341B
attributed to stdin.f's pre-window DP shift, dicts identical.
BOOT MILESTONE BLOCKED by P1 habu-seed-call-site-9d7d8e72: the
seed's LFIND is wordlist-blind - 15421 of 18896 chain sites have
packaged callees and exit a SILENT 81 ($51). Fix ruled on that
dot (site rows carry the callee wid as a window coordinate, 12B
rows, VERSION 3, wordlist-aware resolve, capture-side
resolves-as-the-seed-asks audit, named diagnostic for $51).
Items (3)-(6) blocked behind it. Loose ends recorded: xtsites=0
on both sides (shift carried by code, no measurement - fixture
owed when one exists); three W32 spellings in aot-capture.f
(dedup is a caller cascade, not authorized); the metabuild
acceptance fixture awaits the blocker (vehicle ready).

