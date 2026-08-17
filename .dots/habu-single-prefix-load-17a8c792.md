---
title: "Single prefix load: the four-leg implementation"
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-17T19:57:31.271581+02:00\""
---

The 87a370ae probe answered YES with a working experiment (patch banked in the session scratchpad; byte-identical fixpoint c2910ab7 x2, maki green, install 17.1->14.1s, 9 prefix loads not 15, emitted source 2.18MB->800KB, FULL host/target prefix isomorphism - same order same addresses, which retires 5a992a38's and ef47ad69's ground). Four legs, RULED: (1) CERTIFY-ONLY BOOT-PREFIX ASSEMBLY is REQUIRED - the experiment drops certify census 4580->1493 and those 3087 definitions' ONLY gate was the build pre-pass (the 0b5fc6e6 invariant; hide.f's own comment cites it): emit the boot-prefix bytes into a certify-only buffer through VERIFY:SOURCE-BUF, never fed to the compile - a new build phase, justified by preserving a named invariant while deleting 1.38MB of double-compile. (2) The watermark lives in a DEDICATED prefix source file (one concern), wired by the aot-decl/aot-arm manifest precedent - READ the veto record at generated-declaration.f:395-402 and its dot first; if that veto binds THIS edit shape, checkpoint. The rewind is TWO-PART by measurement: ndict! AND USIGS-RESTORE-END (the store's real seam - raw UEND! leaves broken index heads; the silent-78 class). (3) snap: BF-APPEND-SNAP-KEEP re-loads the prefix onto sealed one-shot defers - same file-list surgery; bonus, erases the 4.48MB-orphan-in-every-snapshot class. (4) hide.f keeps BOTH rewind forms - the Gforth recovery host has no watermark (bootstrap-mirror-lint + bootstrap-codegen-test are the tripwires). build-fixpoint-test's emitted-source substring pins rewrite to the new contract. Full seed-affecting gates; the probe's three-way proof inverted is the regression (probe prints ONCE, base word resolves, window reaches the FIRST copy). Relationship: sibling+amplifier of 8010f67c, composes, neither subsumes.

WATERMARK PLACEMENT RULING 2026-08-17 (the lane's checkpoint):
OPTION (B) - the dedicated file src/core/prefix-mark.f, ruling
as written, and THE FLEET RESEED IS ACCEPTABLE NOW: zero worker
lanes are live (all workspaces retired this cycle; only
merge-gate and the lane itself hold engines) - this is the
cheapest moment the tree will ever have for a cold-prefix file
addition. The seven-place cascade is tripwired (boot-pin-test
reds until bumped - bump it WITH attribution), and mixing the
watermark into lower-cert-seal.f's erase-authority concern is
the exact misleading-reader debt this campaign paid down
elsewhere. ALSO RULED IN: (i) the lane's capability-probe rewind
form - the emitted line resolves the watermark at runtime and
falls back to BFR-MARKER-INDEX when absent, an existence check,
both paths failing loud; BFR-USIG-CUT/-EARLIEST die as dead
code; (ii) the STALE VETO record at generated-declaration.f:
395-402 and its dot get updated with the lane's measurement
(build-fixpoint.f is packaged now; the lint is live - the
break-and-watch probe proved it); (iii) leg 4's true size
acknowledged - each replacement pin mutation-backed, none a
spelling of the new implementation; (iv) the watermark point
stays AT THE PROBE'S POINT (the only proven one; SEAL-NDICT-CELL
is past the chain rows and would collide - the lane's
duplicate-78 measurement is the argument).

PLACEMENT RULING REVERSED 2026-08-17 (the lane's leg-2 evidence;
my (B) ruling rested on its own one-file-early veto measurement -
the veto MOVED, it was not stale, and the lint refuses (B) in
three legacy files): OPTION (C) - the mark records at the END OF
lower-cert-seal.f, the file that IS the core prefix's last file.
The re-derivation: the structural invariant is POSITION (the mark
must be taken at the boundary), not file identity - the boundary
is the end of that file, so recording it there is placement AT
the fact, and a dedicated file was the expensive answer: a lint
migration (packaging three files other dots own), a proven
two-stage landing (no existing engine bakes the new row - the
build refuses until an engine rebuilds without the mark), a
fleet reseed, and nine hand lists, against four lines at the
boundary. THE GOOD PARTS OF LEG 2 ALL SURVIVE THE MOVE: the
words stay in their own package block (package PREFIX-MARK
inside lower-cert-seal.f - the lint accepts packaged words in
any file; the NDICT@-shadow lesson and its DICT/USIG naming
stand), the capability probe stays IN THE BUILD
(BF-REQUIRE-WATERMARK refusing before a byte is emitted, proven
firing), the regression test stands, and a 3-line comment at
the mark STATES the derivation and prices the rejected
alternative so the concern-mixing is a documented decision, not
drift. The stale-veto correction on generated-declaration.f
gets its own honest wording: the veto moved (build-fixpoint.f
packaged; habu2.f/boot-pin still unpackaged - their packaging
belongs to their own dots).

SEAL RULING 2026-08-17 (the leg-2/3 blocker: SEAL-DICT-GUARD
refuses the snap retire below the boot watermark - correctly,
by its own light): THE SNAP RETIRE RUNS UNDER THE BUILD-MODE
LATCH, exactly the discipline the metabuild's own rewind
already uses (it stores ndict! under MODE-BUILD's open latch
and never trips the guard). One rewind discipline, no second
floor, no watermark re-capture. The guard's invariant is
untouched: sealed definitions stay unforgettable BY CHECKED
CODE - the latch is build machinery. PRECONDITION: verify the
latch is NOT reachable from checked habu (the seal threat model
is checked habu); if it is, checkpoint - that would be a seal
hole independent of this work. The lane's option (iii) - snap
keeping the old rewind - is REFUSED in the lane's own words:
two rewind shapes in production with the 4.48MB orphan
preserved for exactly the artifact that persists it.
LANE HYGIENE RULING: rebuild legs 2-4 from the clean 65fede79
plus the banked scratchpad files (spx/lcs-C.f, bf-C.f, bft-C.f,
hide-2b.f, prefix-mark-test.f, leaf-resolved.md) - do NOT fight
the divergent conflicted twin; abandon it. Leg 4's pins get
corrected against the REAL emitted source, mutation-backed.

Claim: agent=single-prefix-4 workspace=.jj-ws/habu-single-prefix2

CHECKPOINT RULINGS 2026-08-17 (the root class named: the rewind
must restore EVERY cursor it invalidates - it restored two of
three):
(1) THE THIRD MARK: yes - capture REQUIRE-N at the boundary and
rewind the include registry through its own seam, the exact
mirror of the USIGS lesson (a raw store is not a truncation; the
registry's seam repairs what a bare REQUIRE-N ! would not). The
seam is a new public word where the registry lives; the hazard
judgment (include/included already bypass the registry) is
adopted; checkpoint if the ownership gate refuses the shape.
An image must never claim to provide what the rewind removed.
(2) verify-prim-test: EXPORT BF-APPEND-CHECKER-BOOT for the
harness (a named consumer building a legitimately unmarked
cold-load), AND the narrowing is RECORDED AS INTENDED for
production payloads - a payload executing internal-marked
definers was an escape, not a feature.
(3) aot-wid control ids: derive, never pin - approved.
(4) THE SEAL DEVIATION IS APPROVED as the ruling's own logic at
the breaking site: the rewind restores the floor it moved, same
cell, under the build latch, measured STRICTER than master
(306->239 forgettable window). Two residuals dotted separately.
(5) prefix-rewind.f approved - hide.f is deliberately unpackaged
for the recovery mirror's design (measured refusal), and the
split sharpens it: hide.f IS the recovery host's surface.
(6) The arena cap HALVING is approved with its caveat recorded -
this is 8010f67c's dividend arriving early (the dedup the
composite policy predicted); the unproven recovery path is
53355d74's standing ground, stated not claimed.

FOURTH-CURSOR RULINGS 2026-08-17 (the invariant completing
itself - fixing cursor 3 moved the failure to cursor 4, and the
deletion alternative was refuted by measurement):
(1) YES - the mark becomes the checker's own COMBINED FRAME:
EXT-SAVE at the boundary, EXT-RESTORE in the rewind (the
type-family registry's own seam, per its comment naming PF-*/
SCH-* as participants with EXT-SAVE/RESTORE as the pair). This
is the FOURTH application of the one invariant - every cursor
restored by its owner's truncation word - not a new idea. The
mark's comment states it: THE CORE-PREFIX BOUNDARY IS A CHECKER
TRANSACTION BOUNDARY. Checkpoint if the AOT capture window's
bracketing of the same counters surfaces anything unruled.
(2) verify-prim-test uses the EARLIEST-MARKER rewind hide.f
already keeps for the recovery mirror - its subject IS a
deliberately cold, unmarked checker, i.e. the recovery-host
shape, and the header says so. No second PRODUCTION shape is
minted; the seal ruling's refusal covered production, not a
harness whose subject is the cold form itself.
(3) aot-wid: derive the control id from the shipped engine's
live band in the suite and THREAD it into the spawned build
through the fixture's existing knob mechanism.
(4) The REQUIRE-REG package-block self-resolution is RATIFIED -
it is the PREFIX-MARK precedent applied, and the gate is green.

RULING 1 REVISED 2026-08-17 (refuted on the bench: an unpaired
EXT-SAVE leaves TFAM/SCHEMA one frame deeper than the core for
the whole boot and the first post-mark declaration dies 7113 -
the depth-lockstep assert doing its job): OPTION (B) - the
DEPTH-NEUTRAL COUNTER PAIR. The boundary is a checker COUNTER
boundary, not a transaction boundary - my phrasing was the wrong
abstraction and the bench said so. The deciding argument is
uniformity: the three landed cursors are each a RECORDED NUMBER
restored through its owner's truncation seam; option (a) would
make the fourth a live stack frame spanning the entire boot and
payload, coupled to the rollback stack's lifetime and privates -
structurally different from its siblings and fragile in exactly
the way the lane flagged. (B) is the fourth application of the
SAME shape: capture the five TFAM counters + the PF and SCH
counts in the mark's record; rewind through TFAM-REWIND,
PF-SCRUB + its counts, and the schema equivalent - the same
bodies TF-RESTORE-TOP uses, read from the mark's own record, no
depth change, no stack interaction. The mark's comment drops the
transaction phrasing and states the counter-boundary fact.

FIFTH-APPLICATION RULING 2026-08-17 (the warm-image declaration
crash: TO-CORE restores four cursor families; RBF-PUSH at
checker.f:12618 is the AUTHORITATIVE enumeration of ~18 a
checker scope invalidates; a declaration reads exactly the
unrestored ones and stores through a stale pointer into the
baked read-only band - the root class a fifth time, at the
checker's own core record):
(1) THE CHECKER OWNS THE LIST, SO THE CHECKER OWNS THE SEAM.
Do NOT enumerate eighteen numbers in prefix-rewind: mint ONE
checker-owned depth-neutral pair (CHECKER-BOUND:MARK/REWIND or
kin) DEFINED BESIDE RBF-PUSH/RESTORE-TOP whose record and
restore bodies are derived from THE SAME field list - lockstep
by construction, so an RBF field added later cannot silently
miss the boundary. The index-retire seams (HIDX-SYMS-RETIRE,
ASIG-SYMS-RETIRE) run BEFORE their counts move - the recorded
lesson, now load-bearing twice.
(2) PREFIX-BOUND and SCHEMA-REG COLLAPSE INTO IT - the four
type-registry cursors are members of the full set; two
overlapping seams is two authorities. The fourth cursor's
machinery either becomes the general pair or defers to it;
keep the TRUSTED-withholding form and its priced argument.
(3) The mark's comment: the cursor COUNT comes off the seam,
never a hand-written number - "four cursors" was honest for a
day; the sentence that replaces it should not need replacing.
(4) Residual 2 (the watermark probe blind to the boundary):
the structural form is DATA not name - the probe reads a value
in the mark's record that only the new seam writes; no lucky
same-commit coupling; checkpoint if that needs anything
unruled.
(5) This is NOT a separate dot: it is the completing
application of this dot's own invariant, in scope. The crash
exists only in the lane's unmerged state - master never had
TO-CORE - so nothing shipped is affected.

PROBE MECHANISM RULING 2026-08-18 (residual 4 - a build tool
must read a number that may not exist on an old host): BOTH
LAYERS, each doing its own job - CHECKER-RESOLVES? on
PREFIX-MARK:CURSORS guards the COMPILE hazard (an old host
cannot even compile the call - the resolvability check is the
existence guard, which is what it is FOR), then evaluate the
call and require the VALUE nonzero and equal to the seam's own
field count (every build host has taken the mark - it is
top-level in lower-cert-seal - so zero means the boundary set
never landed, not never-ran). Name-shaped guard for the
name-shaped hazard, data for the fact; neither alone.

RESIDUE FIX RULING 2026-08-18 (the DEV-A-P persisted malloc
pointer, root-caused to the byte - 0x102fbc000 + 29x40 = the
faulting address; master green BY ACCIDENT, the double load
re-seeding a module whose own header promised a snapshot re-arm
nothing implemented): THE FIX IS APPROVED AS PROPOSED - both
ruled shapes were correctly refuted (not scope-recorded state;
beside-TO-CORE rests on the lucky zero-events value), and the
violated invariant is the SNAPSHOT's (a persisted image carries
no host-process pointer), whose enforcement point and mechanism
the tree already owns with three precedents (SEEN-, TDECL-
SCRATCH-, and the protection chain's SNAPSHOT-RESET reaching
participants by qualified call). The new public SNAPSHOT-RESET
in decl-event.f + the one chain line in descending participant
order (900->850->800) is the precedent applied, not new
machinery; the event-log drop REPRODUCES master's observable
contract (DEV-PUB-N=0 measured on shipped images). The
ASLR-intersect discriminator (two runs of one image, persisted
pointers identical, live ones vary) is adopted as the class's
detection method - record it in docs/debugging.md with the
landing.

EQUALITY CLAUSE CONFIRMED-AS-REFUTED 2026-08-18: the seam sits
below the lower-cert seal, so no consumer outside the prefix can
hold both numbers - the equality clause has no home except the
assignment, where it is a tautology. The lane's form preserves
the ruling's INTENT in the reachable shape: the nonzero clause
forged and mutation-backed; the off-the-seam property held BY
CONSTRUCTION (one assignment, in the mark's owner, from the
seam); and the unreachability itself ASSERTED in the test so the
day the wall moves, the test says so. Ruled: this is the final
form. COMMIT SPLIT KEPT as the truth-recording shape - the
parent is green on every gate that exists at it, and the child
repairs the latent defect the parent unmasks; recorded here so
a future bisector reads this sentence.

