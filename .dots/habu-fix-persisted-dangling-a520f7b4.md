---
title: Fix persisted dangling-pointer owners
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-02T19:14:25.425920+02:00\""
---

20 data cells (quarantine table in src/habu/snap-lib.f SND-QUARANTINE, offsets tree-dependent) hold dead ASLR mmap pointers cached by their owning structures at snap time: buckets at drel 0x17CFxx/0x51DBxx (two instances of the same pre-/post-checker structure class caching source-text pointers - identify the lib), 0x31D1xx/0x32ECxx, 0x6CF8A8, 0x743ED0, 0x7571xx (FAM/USIGS-variable area), 0x75ABxx (USIGS-SNAP-P snapshot-copy bookkeeping cells). RCA proof (arena worker, 2026-07-02): none is read after restore; persist words store offsets, so these are per-owner CACHED pointers. Proper fix: identify each owning variable/field (map data offsets to owners via a diagnostic that walks known structure tables or prints allot positions at build), zero/reset at the owner (their SNAPSHOT-PREPARE or equivalent), then shrink SND-QUARANTINE to empty. The two-build byte-compare in the snap flow enforces exactness meanwhile: any layout shift makes the compare fail loudly with this dot as the pointer.

## Investigation (2026-07-02, opus-tools) — evidence, no owner fix applied

Did NOT modify baked source: every owner guess was either ineffective or
crashed the restored engine (see disproven list). Per the dot's own rule
("do NOT guess"), recording evidence instead of a speculative fix.

BUILD METHOD (important — the parent's `-- snap` / refresh command form fails
here): multi-file `bin/hb --load <11 files> -- snap` reliably exits rc 74 in
~0.00s with no output and no artifacts on this workspace (repro: any `--load`
with ~>=5-11 files intermittently/reliably fails at startup; also blocks the
`-- install --force` refresh form). WORKAROUND that works reliably: a single
`bin/hb entry.f` where entry.f `require`s the libs + tools/build-fixpoint.f and
calls `BF-BUILD-SNAP-FRESH` (or BF-BUILD-ALL + step words). This is a real
tooling bug (multi-file --load startup failure) worth its own dot.

ACCEPTANCE CAVEAT: hb-new (post-codesign) can NEVER be byte-identical on macOS —
the ad-hoc codesign signature is ~154 nondeterministic bytes at the file tail.
The meaningful compare is the PRE-codesign snapshot hb-snap0. Harness: build
hb-stdin once (BF-BUILD-ALL; keep hb-stdin + hb-snap-src), then
`hb-stdin < hb-snap-src` twice -> two hb-snap0 -> cmp. Baseline drift on this
tree = 12 pointer cells (27-30 bytes), NOT byte-identical.

STALE-QUARANTINE PROOF (the compare already fails on the current tree; my
commits touched no baked src). Trailer of hb-snap0: magic OK, SCL=1168012,
SDL=8763288 -> DATA_start = filesize - extra - 40 - SDL. The 12 pre-codesign
drift cells map to drel: 0x51DA08 0x51DA20 0x51DA28 0x51DB48 0x6CF750 0x743C20
0x756E98 0x756EA0 0x756EB0 0x75A8D0 0x75A940 0x75A948. These sit in the SAME
buckets as SND-QUARANTINE but shifted DOWN by -0x158..-0x2b0 (e.g. quar 0x51DB60
vs live 0x51DA08; quar 0x6CF8A8 vs live 0x6CF750; quar 0x757148 vs live 0x756E98;
quar 0x75AB80 vs live 0x75A8D0). CONCLUSION: SND-QUARANTINE is STALE for the
current tree — it zeroes the wrong cells and misses the real dead ones. The
offset table is inherently fragile to any layout shift; the owner-side reset is
the only durable fix.

OWNER FILE + HOOK: the reset must live in src/core/checker.f
`CHECKER-SNAPSHOT-PREPARE` (~line 3111; called by src/habu/snap.f before the
retire/SNAPGO, while names are still live). It already resets TOKBUF, HIDX,
USIGS store (USIGS-SNAPSHOT-PERSIST), NORET. snap-lib.f canNOT do this: at
snap-write time the builder tail is retired and owner names like USIGS-P are
UNDEFINED there (proven: probe in snap-lib.f threw `undefined word USIGS-P`).
That is exactly why the original used an offset table. snap-lib.f is NOT baked
into bin/hb (retired before the image), so it can be iterated cheaply; but
checker.f IS baked, so the real fix needs the byte-exact fixpoint refresh + full
gate + SND-QUARANTINE shrink.

DISPROVEN owner guesses (cheap loop: edit checker.f -> regen hb-snap-src via
BF-SNAP-SOURCE -> hb-stdin<hb-snap-src twice -> cmp; verify hb-new boots+checks):
- Zero FAM-A/FAM-U/FAM-N array + USIGS-SNAP-P in CHECKER-SNAPSHOT-PREPARE:
  drift UNCHANGED at 12 cells -> these are NOT the drifting owners (the dot's
  "FAM/USIGS-variable area" / "USIGS-SNAP-P bookkeeping" bucket names describe the
  REGION, not these exact vars).
- Zero USIGS-P + NORET-P: restored hb-snap0 (codesigned to hb-test) CRASHES on
  boot (rc 134, habu-crash regs) -> USIGS-P/NORET-P are READ/relocated after
  restore, i.e. NEEDED, NOT dead. DO NOT zero the store base pointers.

REMAINING WORK for the next session (owner ID is unfinished; the true owners are
cells in these buckets I did not map): the blocker is that every checker.f edit
shifts the layout, and there is a small constant frame offset between the probe
frame (`VAR data-base -`) and the trailer-derived DATA_start frame, so
offset<->name correlation is fiddly. Recommended: ONE probe run in a FIXED layout
that (a) prints every checker variable's `VAR data-base -`, and (b) dumps the
8-byte VALUE at each of the 12 drift drels, then correlate by value+offset in that
single layout to name each owner; confirm each is dead by zeroing it and checking
BOTH drift-drops AND hb-new still boots+checks a def. Only then move the reset
into CHECKER-SNAPSHOT-PREPARE, refresh the fixpoint byte-exact, and shrink
SND-QUARANTINE. Sandbox artifacts kept under /tmp/dot4 (hb-stdin, hb-snap-src,
entry files) for a fast restart.

Claim: agent=dangling workspace=.jj-ws/habu-fix-persisted-dangling-a520f7b4

## MEASURED 2026-07-30 (agent=dangling, workspace .jj-ws/habu-fix-persisted-dangling-a520f7b4)

Owner identification is done, with a tool rather than a guess. One owner class is
fixed and proven. Two things this dot was believed to own turn out not to be its
class, and both are now separate dots with the evidence attached.

### Method

Everything below was measured on an engine and images rebuilt from this exact
tree (`bin/hb --load tools/build-fixpoint-refresh.f -- install --force`, then
`-- snap`). Two images built from one `hb-stdin` and one `hb-snap-src` give the
exact set of persisted cells that hold a live process address, because only such
a cell can differ between two builds of the same source. `cmp -l` on the two
images, split by the trailer's own section lengths, gave 113 differing DATA cells
and about 3100 differing REGION bytes.

Turning a byte offset into an owner is the new tool, `tools/snap-heap-owner.f`
(documented in `docs/debugging.md`). It walks the live dictionary and prints
`<heap offset> <name>` for every word that owns a piece of the DP heap, and
`<region offset> <length> <name>` for every word that has code. A heap owner is
recognised by the single fixed shape `create` and `variable` compile - the
four-instruction MOVZ/MOVK x9 address chain from `habu2.f` `C-ADDR-RAW`, the push
stencil, a return, recorded code length 24 - and the address it owns is read out
of the chain's immediates. Nothing anywhere in this looks at what a cell
contains. On this tree the map has 1793 owners, and every named cell below lands
strictly inside its owner's own allotment, not merely after it.

### The crash that reds owner-wid-internal is not a DATA cell

The dispatch brief said one more undeclared DP-heap cell at DATA offset 0x81d948
held a writer-run address executed after restore. That is not what is there now.
A restored image boots and prints for `1 . cr`, then dies the instant it compiles
a definition: signal 11, program counter 0x10627b8b4, live region base 0x1053e0000,
0x69b8b4 past the end of an 8 MiB region. A search of the whole DATA region for
that value under lldb finds nothing. The value comes out of REGION CODE: at region
offset 0x2eb8cc sits the four-instruction MOVZ/MOVK x9 chain that pushes the entry
address of the quotation eight instructions earlier at 0x2eb8b4, followed by a call
to `catch` - an ordinary `[: ... ;] catch`. The correct value for that run is
0x1012fb8b4; the image carries the writing run's 0x10627b8b4.

Nothing relocates address literals compiled into region code. `LSNAPRBD`
(`habu2.f` `EM-SNAPSHOT-REBASE-DICT`) walks dictionary RECORDS only, fields [0]
and [24]. The `layout.f` comment about region-internal pointers canonicalising to
the RBASE-VA sentinel is true of record [0] and of nothing else, so every
quotation entry address, `[']` and postpone target compiled by `C-CODE-ADDR` is
persisted as the writing run's absolute address. That is the whole of the
remaining owner-wid-internal failure, and it is a different subsystem from this
dot. It is now dot `habu-relocate-persisted-region-47de06b9`, priority 1, with the
disassembly, the emit chokepoints, and the fix shape (declare the site at
`C-CODE-ADDR`, relocate it exactly like `SNAP-RELOC:EMIT-CALLS` does for calls).
The `test/owner-wid-child.f` failures are the same class: its image dies at signal
4 on 0x1017423f0, which sits between this run's text base 0x100db0000 and its
region base 0x101db0000 - again the writing run's region, not this one's.

### SND-QUARANTINE was stale, and worse than stale

The twenty offsets were checked against the measured drift set. Not one of them
names a cell that actually differs between two builds. Eight of them
(DATA-START +0x1732B0 through +0x324FF8) point inside the heap the refresh prelude
abandons, which is now handled as a span. The other twelve
(+0x513EC0 through +0x750F58) point into live checker and symbol buffers of the
current generation and zero cells that do not differ between builds at all - so
they were never treating anything, and were quietly clearing live-looking data on
every image. The table is removed, not shrunk: keeping a wrong address because it
is written down is not a treatment.

### Owner found and fixed: the heap the refresh prelude abandons

54 of the 113 cells - and 4.48 MB of image - are not owned by anything at all.
The native refresh truncates the dictionary back to the primitive boundary
(`src/habu/hide.f` `BFR-HIDE-DICT-FROM-EARLIEST`, driven by
`tools/build-fixpoint.f` `BF-STAGE2-HIDE-DEFS`) and reloads the whole prefix from
source. Truncating the dictionary does not move DP, so everything the previous
generation allotted stays in the heap with no owner and no reader, still holding
that generation's own mmap addresses and region pointers, and the writer copies
all of it into the image. Measured: the live generation's first allotment is at
heap offset 4818843, boot DP is 4818726, and DATA-START is 335016.

`src/habu/snap-lib.f` now zeros `[DATA-START, live-heap-start)` in the scratch
copy, beside the eval-frame and return-stack spans it already clears for the same
reason. The live-heap floor is `IMK-NDICT0`, the first variable of the first
prefix source file - `src/core/util.f` records the primitive record watermark
there precisely because it is first, so the anchor and the property are the same
fact. A build with no truncation ahead of it puts `IMK-NDICT0` at DATA-START and
the span is empty. It is reached through a named `TRUSTED:` boundary
(`SND-DEAD-HEAP-END`, row added to TRUSTED.md) because the whole prefix loads
inside the prelude's check-off window and carries no charted effects - the same
reason `src/habu/snap.f` reaches `CHECKER-SNAPSHOT-PREPARE` through one. It fails
closed if the floor ever comes out below DATA-START.

Proof: two images rebuilt from the changed tree differ in 65 DATA cells instead of
113. All 50 heap cells in the abandoned span are gone (the 4 remaining cells in
that address range are below DATA-START, in the engine-reserved band, and are a
separate class). `test/snapshot-xt-cell-decl.f` stays green, including its
negative regression. A restored image boots clean in 50 of 50 consecutive bare
runs.

### Owner table, remaining 65 cells

Below DATA-START, engine-reserved band, 4 cells:

| offset | owner | treatment | status |
| --- | --- | --- | --- |
| 0x40 | DEF-WL-CELL (layout.f:183) | zero in SND-ZERO-LIVE | identified, NOT applied - untestable until a restored image can compile a definition |
| 0x360 | unnamed band inside LVF-OFF..LASTC-CELL | reset at band owner | owner not named |
| 0x3AA0 | unnamed band between SNAP-CELL and TASK-TCB-CELL | reset at band owner | owner not named |
| 0x3EC0 | inside PROT-WID-OFF..PROT-WID-END | reset at band owner | owner not named |

Those four are dot `habu-name-three-drifting-84dd52a9`, including why DEF-WL-CELL
was deliberately not zeroed on this tree.

In the DP heap, 61 cells across 14 named owners, every one a scratch or cache
buffer holding source-text mmap pointers, page bases or region addresses:
SPAN-BUF 25, REACHBUF 6, SHA-IO 5, PES 5, FROZEN 5, FAM-U 3, EI-AK 2,
REQUIRE-LENS 2, FIELD-I 2, the type-family row buffer near TF-RBF-DEPTH 2,
CK-USE-NAMES 1, NDH 1, NORET-BOOT 1, BUF-BOOT 1. Every one lands inside its
owner's own allotment. The treatment for all of them is an owner-side reset in a
snapshot-prepare, which is dot `habu-reset-checker-and-92aefb23` with the full
table and the reproduction recipe. They are left alone here rather than zeroed
from the writer, because zeroing a buffer from the outside is exactly the pattern
that produced the stale table this change removed.

### Acceptance, honestly

Not reached, and not reachable in this lane. A snapshot image boots and runs, 50
of 50, but it still cannot compile a definition, so it cannot execute a deferred
word either, and `test/owner-wid-internal.f` is still red - for
`habu-relocate-persisted-region-47de06b9`, not for anything in this dot's class.
The 200-run campaign target is not attempted for the same reason. What is closed
here is owner identification (the tool exists and works), the largest owner class
(4.48 MB of abandoned heap, 50 cells), and the stale offset table.
`tools/build-fixpoint-test.f` fails at assert 152 with rc 134 both before and
after this change - measured on the unmodified tree first - so that failure is
pre-existing and is the same region-literal crash.

### Gates run

Engine fixpoint rebuild green (`-- install --force`, then `-- snap`, both clean).
`test/snapshot-xt-cell-decl.f` green. `tools/trust-lint.f`: 939 trust sites, 972
manifest rows, 0 findings, exit 0. `tools/error-code-lint.f`: 1345 files, 902
claims, 39 reservations, 0 findings, exit 0. `tools/typed-local-diff-lint.f` and
`tools/package-diff-lint.f` on the exact `jj diff --git` artifact: exit 0, no
findings (both verified to fail loudly on a missing input, so the silence is a
pass and not a no-op). Full gate-stdlib deliberately not run.

### Is this the best long-term solution, or a patch?

Long-term, and the re-derivation does not rest on the label. The invariant behind
the change is that a snapshot may only persist heap that a live word owns. The
refresh prelude breaks that invariant deliberately - it drops the owners and
keeps the heap - so the repair belongs at that seam, and the writer clears exactly
the span the seam abandoned. It is a span with a structural endpoint, not an
address list: no offset can go stale, because the endpoint is recomputed from the
dictionary on every build, and it cannot over-reach, because the endpoint is the
first thing the live generation allotted. The alternative that was rejected is the
one already in the tree: a hand-maintained offset table, which had drifted so far
that twelve of its twenty rows were clearing live buffers. The one soft spot a
reviewer should check independently is the anchor: it is the first variable of the
first prefix source file rather than a value the truncation itself records. A cell
written by `hide.f` at the moment it truncates would be the more direct statement,
and it was not taken because `hide.f` executes in the previous generation's engine
and can only name constants that engine already bakes - a new layout constant
there needs a two-generation rollout. If that rollout is done later, this anchor
should move to it; the span and the writer pass do not change.
