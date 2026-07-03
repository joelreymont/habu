---
title: Fix persisted dangling-pointer owners
status: open
priority: 2
issue-type: task
created-at: "2026-07-02T19:14:25.425920+02:00"
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
