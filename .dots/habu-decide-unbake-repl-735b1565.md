---
title: "Decide: unbake REPL source from binary"
status: open
priority: 2
issue-type: task
created-at: "2026-07-02T16:51:25.423903+02:00"
---

The binary carries ~19 KB of plain REPL/debugger/stepper source text (repl.f, debug-watch.f, stepper, breakpoints), compiled at tty startup (stdin.f LSRC model). Removing it and loading that bundle from the checkout at startup (exactly how the checker prefix already loads) drops the code section below a 16 KB page boundary: file goes ~132 KB -> ~116 KB. Trade-off: a bare bin/hb without a checkout would have no REPL/debugger - USER DECISION needed. If approved: move the repl bundle from the baked LSRC into the checkout-load prefix list in src/habu/stdin.f, keep a clear diagnostic when the checkout is missing at tty start, lower the size ratchet baseline in the same commit, prove with the gate + a tty smoke test. Measurements in habu-bisect-engine-growth findings.

## Options (2026-07-02, after composition audit)
1. Keep baked source (today): 132 KB file; REPL compiles UNCHECKED at every
   tty start (0 set-check in the baked prefix - a tracked trust boundary).
2. Load the REPL bundle from the checkout at startup (like the checker
   prefix): ~116 KB file, bundle becomes checkable at load; bare binary
   without a checkout loses the REPL/debugger.
3. AOT-compile the bundle at build time via the snapshot image writer and
   persist the compiled dictionary into the binary: ~same file size as
   today, faster tty start, the bundle is checker-certified ONCE at build
   (discharges the startup 0 set-check), REPL still works without a
   checkout. Recommended once staged source certification
   (habu-bf-certify-stage) lands; option 2 if the ~100 KB size target
   dominates.

## Implementation plan (2026-07-02, user direction: AOT)
1. Build step (tools/build-fixpoint.f stdin stage): after the engine
   self-build, compile the REPL/debugger/stepper bundle with the real
   compiler THROUGH THE CHECKER (discharges the startup 0 set-check
   boundary in the baked prefix; repl sources already carry TRUSTED.md
   rows for their genuine boundaries).
2. Persist the compiled dictionary + code region into the binary via the
   snapshot writer segment machinery (BUILD-SNAP-HDR / the SNAPGO path at
   small scale), replacing the ~19 KB baked source text (LSRC in
   src/habu/stdin.f).
3. Startup: register the persisted words instead of evaluating source;
   tty start skips the compile; bare-binary REPL still works.
4. Fixpoint: compiled output must be byte-deterministic for a given
   engine+source - the content-addressed warm cache already relies on
   this; add the byte-for-byte check to the fixpoint proof for the new
   segment.
5. Size ratchet: expect roughly no file-size change (compiled code ~=
   source text); update the baseline honestly either way. If the ~100 KB
   target later dominates, the alternative is checkout-loading (drops to
   ~116 KB) - record whichever is chosen.
Risks: snapshot-segment determinism across generations; dictionary
relocation on ASLR/base differences (snapshot images already solve this -
reuse, do not reinvent).

## Build-path anchors (2026-07-02 scoping)
- Snapshot emit source: tools/build-fixpoint.f:736 BF-EMIT-SNAP-RUN-SOURCE
  (dev-surface-first source ordering + SNAP-TAIL-MARK retirement, comment
  at :675); runtime side src/habu/snap.f (SNAPGO at :30, retirement at :14).
- Image segment writer: BUILD-SNAP-HDR at src/os/macos/macho.f:173 and
  src/os/linux/elf.f:214 (data-segment persistence both targets).
- Generated trust rows for the writer: tools/build-fixpoint.f:582
  (BF-APPEND-TRUST "BUILD-SNAP-HDR") - the AOT step must reuse this exact
  machinery, not duplicate it.
- The baked source to replace: src/habu/stdin.f READ-REPL (LSRC assembly,
  ~19KB: repl-term/repl/debug-watch/stepper/debug + the 0 set-check line).
Approach: a new stdin-stage step compiles the repl bundle checked, then
persists dictionary+code via the BUILD-SNAP-HDR path into the stdin image;
startup registers instead of evaluating. First milestone: prove a minimal
one-word bundle round-trips (compile at build, call at startup) before
moving the full REPL.

## Key discovery (2026-07-02, second scoping pass)
The warm snapshot ALREADY AOT-compiles the full REPL bundle:
BF-APPEND-SNAP-REPL (tools/build-fixpoint.f:703-710) compiles
repl-term/repl/debug-watch/stepper/debug into the snapshot image, and
SNAP-TAIL-MARK + retirement keep only the dev surface. So the mechanics
exist end to end; the bin/hb delta is:
1. Give the stdin image the snapshot shape at small scale: engine keep
   surface + compiled REPL bundle, build tail retired, persisted via
   BUILD-SNAP-HDR (both targets).
2. Prove byte-determinism of the persisted dictionary across fixpoint
   generations (the whole fixpoint claim depends on it; the warm image is
   content-cached but never byte-compared generation-to-generation - add
   that comparison first as its own small test).
3. Lift the bundle from unchecked (BF-APPEND-CHECK-OFF wrapper, used even
   in the snapshot) to checked compile - requires the repl sources to
   certify; run the checker over them first to get the real miss list.
Order: determinism test -> repl certification miss list -> stdin image
reshape. Each is its own commit.

## Determinism experiment (2026-07-02, MEASURED)
Two back-to-back `-- snap` builds differ in 3,618 bytes, first at byte
111,906 - immediately after the baked source text, at 48-byte strides =
the dictionary records' address fields (DREC stride), holding ABSOLUTE
pointers that vary with the process load base (ASLR). Everything else is
byte-identical. Conclusion: persisted dictionaries are deterministic
modulo base; the byte-for-byte fixpoint requirement needs base-relative
persistence: rebase address cells (xt/code fields, and any other absolute
cells - 3,618 bytes / 8 = ~452 cells) to image-relative on persist and
back on restore, or make the image writer subtract the recorded base.
This is the ONLY blocker class for AOT determinism - the rest of the
image already reproduces exactly. Next step: find where snap.f/image
writer persists the dict records and add the rebase pass + a committed
two-build byte-compare test.

## Rebase design (2026-07-02)
src/habu/snap-lib.f:50-61 records the OLD text base (STB) in the trailer
and copies the dict/code/data regions RAW; the engine startup loader
already relocates using (new base - old base). Cleanest determinism fix:
CANONICAL-BASE PERSISTENCE - at write time, run the same relocation the
loader applies but targeting base 0 (or a fixed canonical base), and
record that base in the trailer; the loader needs no change (its delta
math is base-agnostic), and images become byte-identical under ASLR.
Constraint: the loader's relocation pass is emitted engine code
(habu2.f startup), so either (a) mirror the exact field walk in the
snap-lib writer (Forth-side, must enumerate the same cells: dict record
fields [0]/[8]/[24]-when-EXT + whatever data cells the loader touches -
derive the list FROM the loader emitter, cite both sites in the commit),
or (b) factor the loader's field walk into a BL routine the writer can
call pre-write. (b) is smaller and single-sourced - prefer it.
Then: committed regression = build snapshot twice in one gate fixture,
byte-compare, fail on any diff.

## Relocation surface (2026-07-02, complete)
Loader walks (src/habu/habu2.f): EM-SNAPSHOT-REBASE-DICT (:2334) - dict
fields [0] xt and [24] when DNAME-EXT, rebased when inside
[x21 old-base, x21+x22) by x25 = new-old; EM-SNAPSHOT-REBASE-CALLS
(:2351) - scans compiled code for MOVZ/MOVK/MOVK/BLR-x16 absolute-call
chains and rewrites the 48-bit target the same way. Both are already
base/delta-parameterized via registers.
Writer plan: factor both walks behind BL-callable labels
(LSNAPRBD/LSNAPRBC, register contract x9 walk cursor, x21 base, x22 len,
x25 delta - same LABEL@ LBL, allocation in EMIT-LABEL-CORE; definition
sites MUST use LABEL@ LBL, not @ LBL,). snap-lib SNAP-WRITE then:
copy the region to scratch is NOT enough (walks assume live addressing) -
instead canonicalize in place with delta = canonical - current, write,
then restore with the inverse delta so the running process continues.
Record canonical base in the trailer (loader unchanged). Regression:
gate fixture builds the snapshot twice, byte-compares, fails on any diff.

## DECISION (2026-07-02, user): REPL + debugger stay IN the binary,
## AOT-COMPILED. Baking source text to compile at every startup is the
## thing to remove; checkout-loading is rejected (bare binary must have
## the REPL/debugger). So the target is: compile the bundle at build
## time, persist the compiled dictionary+code into bin/hb, register at
## startup instead of evaluating source. Size is expected ~neutral; the
## wins are (a) no unchecked-at-startup compile, (b) faster tty start,
## (c) the bundle is checker-certified once at build.

## Draft attempt + bug found (2026-07-02, reverted)
Wrote a Forth canonical-base pass in snap-lib.f (SNAP-CANONICALIZE around
SNAPGO). Reverted: two defects to fix next pass.
1. Undefined accessors: BFR-N>REC / BFR-N>U8 live in hide.f, which is NOT
   in the snapshot build tail (BF-APPEND-SNAP-BUILD, build-fixpoint.f:715
   loads treeshake/rt/crash/image/habu1/prof/regalloc/jit/habu2/driver,
   no hide). Use pointer-native cell access instead: dbase@/cp@ give
   `ptr a`; `@`/`!` for [0]/[24] fields; for the 4-byte code words either
   add a small TRUSTED ptr->u8 view + c@/c! word set in snap-lib, or
   factor the loader walk (option b below).
2. ASYMMETRIC PREDICATE BUG: canonicalize sets field -= base so in-text
   fields become [0,len); the reverse pass then cannot re-detect them with
   [base,base+len). Correct design: canonicalize detects with
   [base, base+len) and subtracts base -> [0,len); DEcanonicalize detects
   with [0, len) and ADDS base. Parameterize SNC-APPLY with
   (detect-lo, detect-len, delta). Non-text fields are huge pointers, so
   [0,len) never false-matches them.
Trailer: record CANONICAL base 0 (not real STB) so the loader's
delta = newbase - 0 and its [0,len) membership both work unchanged - VERIFY
the loader (EM-SNAPSHOT-REBASE-DICT/CALLS habu2.f) uses the trailer base
for BOTH delta and membership before changing what STB writes.
STRONGLY PREFERRED (option b): factor the loader's exact field walk
(EM-SNAPSHOT-REBASE-DICT :2334 + EM-SNAPSHOT-REBASE-CALLS :2351) into two
BL-callable engine routines; call them from BOTH the loader AND the writer
so there is ONE relocation implementation, not two that can drift. A
byte-compare test only catches writer-vs-writer drift, NOT writer-vs-loader
mismatch, so single-sourcing is the real safety, not the test.
Only after determinism holds: lift the bundle to CHECKED compile (drop the
BF-APPEND-CHECK-OFF wrapper at build-fixpoint.f:704) - run the checker over
repl.f/debug-watch.f/stepper.f/debug.f first for the miss list.

## Milestone landed (2026-07-02, commit "Canonical-base snapshot persistence")
- Relocation walks single-sourced: EM-SNAPSHOT-REBASE-DICT/CALLS are now
  BL-callable routines (LSNAPRBD/LSNAPRBC) parameterized by x8 region base,
  x15 record count, x16 code end, x21 detect base, x22 detect len,
  x25 TARGET BASE (value - x21 + x25; NOT a delta - passing 0-base double-
  subtracts, cost one debug round). Loader BLs them; new `snap-rebase`
  primitive ( base end count dbase dlen newbase -- ) exposes them to Forth.
- Writer (snap-lib.f): region copied to scratch mmap, canonicalized to
  base 0, streamed; live region untouched (in-place rewrite would break the
  writer's own call chains). Data region copied to scratch with every
  loader/startup-overwritten live cell zeroed (named layout cells + the
  whole EVAL-FRAME window). Trailer records canonical base 0; loader
  unchanged and validated (snapshot restores and passes the gate).
- Determinism progress, measured two-build byte-compare:
  3,636 diffs -> 353. Dict+code region now BYTE-IDENTICAL.
## Remaining for full byte-identity (sub-task, precisely scoped)
~34 cells deep in the persisted DATA (first at data+0x17cf50 area in one
run) hold per-run mmap ADDRESSES inside checker-persisted arenas (USIGS/
NORET copies written by USIGS-SNAPSHOT-PERSIST/NORET-SNAPSHOT-PERSIST,
src/core/checker.f:3111 CHECKER-SNAPSHOT-PREPARE). Fix belongs in the
persist words: normalize (relativize or zero) pointer-bearing record
fields at persist; they are dangling post-restore anyway (TOKBUF-RESET
runs first), so this is also latent-bug hardening. After that the Mach-O
signature (last ~319 diff bytes) becomes identical automatically since it
hashes content. Acceptance: two `-- snap` builds byte-identical; then add
the committed compare to the snap command.

## DETERMINISM ACHIEVED (2026-07-02, commit "Snapshot images byte-identical across builds")
Two independent `-- snap` builds now produce BIT-FOR-BIT identical images
(cmp clean, signature included). Final pieces: return-stack window zeroed
in the data scratch (two stale old-USIGS pointers proven there by the
arena worker's RCA), and a documented 20-cell quarantine table
(SND-QUARANTINE) for dangling per-owner mmap-pointer caches - proper
owner fixes tracked by habu-fix-persisted-dangling-a520f7b4; the table
entries are enforced by the two-build compare. NOTE: an earlier spec
claiming the diffs lived in USIGS/NORET persist was FALSIFIED by the
worker's RCA (persist is offset-based); the quarantine classification is
the evidence-based replacement.
The AOT-REPL prerequisite is DONE: the snapshot infrastructure is fully
deterministic, so a compiled dictionary segment can be added to bin/hb
under the byte-for-byte fixpoint. Next milestone: minimal one-word
AOT bundle round-trip in the stdin image (see Implementation plan).

## ARCHITECTURE CORRECTION (2026-07-03, user decision - OVERRIDES prior plan)
There are NO restored images, now or in the future. habu builds BINARIES;
binaries do not get restored. The snapshot-write + restore-at-boot plan
above is dead. The compiled REPL/debugger goes INTO the binary as
ordinary emitted content, exactly like the engine itself:
- PRECEDENT ALREADY IN THE ENGINE: EM-SEED-DICT stores baked dict records
  TEXT-RELATIVE in the binary and adds the runtime rbase at boot (the
  `7 XREG-RBASE 5 ADD` in the seed copy loop). The AOT dictionary rides
  the SAME mechanism: metabuild compiles the REPL/debugger words, emits
  their dict records into LDICT/LNCOUNT (relative xts) and their compiled
  code into an emitted code blob that boot copies to the fixed region VA
  (0x300000000) alongside the existing seed pass. No loader, no trailer,
  no relocation walk, no validation pass - boot seeds, then runs.
- VERIFIED FACT: bin/hb is built PIE (otool: MH_PIE) - engine-text
  pointers are NOT stable across loads; the relative-encoding + rbase-add
  seed pattern is therefore mandatory for text-pointing fields (or the
  object writer drops PIE - decide during implementation; relative
  encoding is the safer default since it already works today).
- HOW TO GET THE COMPILED WORDS AT BUILD TIME: the stage build already
  boots a candidate that parses the baked REPL source and JIT-compiles it
  into the live region; the build extracts those records + code (the
  canonicalization work tells us exactly which fields are text-pointing
  vs region-pointing vs runtime state) and emits them via the object
  image writer (tools obj-writer work: commits "Add object image writer",
  "Consume cached objects in hb-build").
- The byte-identical determinism work TRANSFERS: it identified every
  live/dangling cell class - that knowledge defines what may be emitted
  (persistent content) vs what must be runtime-initialized. The
  snapshot-specific machinery does NOT transfer and gets retired (see
  dot habu-retire-snapshot-restore).
Milestones: (1) one-word experiment - emit a single AOT-compiled word
(dict record + code blob) via the seed path, prove it callable at boot
with baked-source parse still on; (2) full REPL/debugger set, drop the
baked source text, measure binary size (~19KB source replaced by ~?KB
code) and startup time; (3) retire snap/restore.

## MILESTONE 1 LANDED (2026-07-03) - one-word AOT seed, end to end
Mechanism proven: AOT-PROBE ( -- n ) 12345 is compiled by the metabuild
engine, its region code + dict record baked into bin/hb, and it is
callable at boot with NO source parse. `AOT-PROBE .` -> 12345 on the
installed bin/hb. Baked REPL source parse stays fully on.
- Size before/after: 148855 -> 148855 (NO change; M1 keeps the baked REPL
  source and the AOT section is ~88 bytes = code(32)+len(8)+record(48),
  absorbed by Mach-O page alignment). Startup 0.05s -> 0.05s (unchanged).
- Fixpoint: install --force = "compiler fixpoint"; an AOT-seeded bin/hb
  rebuilds itself BYTE-IDENTICAL (verified cmp). Deterministic across 3
  clean builds. Full gate PASS, typed-local-diff-lint 0, trust-lint 0,
  trusted-inventory strict clean, `-- snap` green.
Design as built (matches the ARCHITECTURE CORRECTION seed path):
- Boot: EM-SEED-AOT (src/habu/habu2.f) runs in EM-STARTUP right after
  EM-SEED-DICT (region mapped; DBASE=x26, NDICT=x27, CP=x28 live; DATA not
  yet mapped). It copies the baked LAOTCODE blob to CP, appends one dict
  record at DBASE+NDICT*DREC with xt = CP + blob-offset (region-relative,
  NOT text-relative like primitives), bumps NDICT and CP. LAOTCODELEN=0
  (stage2/maker/snap builds) skips the whole pass via CBZ.
- Build: EMIT-AOT-SEED (last section in EMIT-CODE-SECTIONS) bakes
  LAOTCODELEN + LAOTCODE (code bytes) + LAOTDICT (one 48-byte record,
  xt/end as blob offsets, inline name). New labels LAOTCODE/LAOTDICT/
  LAOTCODELEN in EMIT-LABEL-CORE (declared in habu1.f).
- Capture: the sample word is defined in the STDIN DRIVER (stdin.f),
  measured at load time (' AOT-PROBE AOT-XT ! ; cp@ AOT-XT @ - LEN),
  copied into scratch by AOT-CAPTURE (habu2.f). Buffers/accessors live in
  habu2.f (AOT-XT@/AOT-CODE-BUF@/AOT-NAME-BUF@, TRUST ptr-u8 views, the
  SRCA@ pattern; 3 new TRUSTED.md rows + TRUST baseline 344->347).
Gotchas that cost debug rounds (record for M2):
1. REGISTER TRANSPARENCY: x13/x14/x15 hold argc/argv/envp from EM-ENTRY-ARGS
   until EM-DATA-INIT stores them. EM-SEED-AOT runs BEFORE that, so it must
   NOT touch x13/14/15 (EM-SEED-DICT avoids them for the same reason). A
   copy-loop counter in x14 clobbered argv -> SIGSEGV in argv processing at
   boot. Use x12 (like EM-SEED-DICT). x20 is XREG-RBASE (text base) before
   EM-DATA-INIT and DATA base after - same register, dual role.
2. COLLISION / FIXPOINT: the sample word must be defined in a driver only
   loaded for the stdin build (stdin.f), NOT in habu2.f. habu2.f is loaded
   in EVERY build (incl. the stage2 build that an AOT-seeded bin/hb runs to
   rebuild itself); an AOT-seeded engine reloading `: AOT-PROBE` from
   habu2.f source hit "duplicate definition" and aborted the install. The
   stdin metabuild host is hb-stage (built from stage2.f, empty AOT, no
   seed) so its `: AOT-PROBE` is always fresh. For M2 the REPL words are
   likewise compiled in hb-stage (no REPL seed) - safe by the same logic.
3. CHECKER STATE: `evaluate`/`set-check` inside the metabuild's GO throw
   (fragile interpreter/checker-hook state); define + measure through the
   normal top-level source path instead. habu1/jit/habu2 load CHECKED
   (' HOOK set-check re-enables after BF-APPEND-CHECK-OFF), so raw region
   byte reads need SRCA@-style TRUST ptr-u8 accessors.
M2 NOTE (call relocation): AOT-PROBE has NO external calls (inline literal),
so it is fully position-independent and needs no relocation. REPL words DO
call __text primitives via absolute movz/movk/movk/blr-x16 chains
(ASLR-dependent) and region words via fixed-VA chains (stable). M2 must
place the REPL blob at the region offset it was compiled at (right after
the cold prefix, natural order) and rebase the primitive-call chains from
builder text base to boot text base (the EM-SNAPSHOT-REBASE-CALLS / LSNAPRBC
walk does exactly this). Region->region and region->cold-prefix calls stay
valid iff the cold-prefix compile is byte-identical build vs boot (it is,
deterministic). Seed point moves from EM-SEED-DICT-time to post-cold-prefix
(where the REPL source is compiled today).

## M2 BLOCKED (2026-07-03) — the M2 NOTE above is architecturally WRONG; evidence below
The M2 NOTE assumes the metabuild host that captures the REPL and the final
`bin/hb` share `__text` primitive offsets (so LSNAPRBC's single base-delta
rebase suffices) and share the region cold-prefix layout (so region->region /
region->cold-prefix calls stay valid). BOTH assumptions are FALSE. Proven with
measurements against a clean `-- install --force` fixpoint build (byte-for-byte
green) whose intermediates were left in HB_TMP.

FACTS (all evidence-backed):
1. Where the REPL is captured: stdin.f `GO` runs in **hb-stdin-mk**, the
   metabuild host = a BIG engine (585 KB `__text`), built by hb-stage.
   (build-fixpoint.f BF-BUILD-STDIN-FROM-STAGE: hb-stage emits hb-stdin-mk from
   the stdin bundle; hb-stdin-mk then runs stdin.f GO -> emits bin/hb.)
   hb-stage and hb-stdin-mk are ~100% identical (both BIG, C-SOURCE-BAKED).
2. bin/hb is a SMALL engine (113 KB `__text`). Its engine code is only **6.7%
   byte-identical** to hb-stdin-mk over the first 0x16000; block match 1/368;
   longest common run from 0x1000 is 129 bytes.
3. Chunk-shift study (bin/hb code chunks located in hb-stdin-mk): 53/92 found,
   39 NOT found; of the found, 40 share a uniform shift of -0x9960 (~39 KB) and
   the rest are scattered. So it is NOT a clean base relationship — a single
   base delta (all LSNAPRBC can do) cannot map host offsets to bin/hb offsets.
4. ROOT CAUSE: `EMIT-SOURCE` (habu2.f:781) = `STDIN? @ IF C-SOURCE-STDIN ELSE
   C-SOURCE-BAKED THEN`. The BIG builders (hb-stage/hb-stdin-mk) are emitted
   with STDIN?=false -> C-SOURCE-BAKED (compile own baked LSRC). bin/hb is
   emitted with STDIN?=true (stdin.f:86 `0 0= STDIN? !`) -> C-SOURCE-STDIN,
   which emits the ~39 KB PFX-LOAD-from-checkout + stdin/tty-REPL machinery the
   BIG engines never emit. So host and target have STRUCTURALLY DIFFERENT
   `__text`; primitive bodies sit at unrelated offsets.
5. Consequence for calls: every colon-body word call is a `movz/movk/movk x16;
   blr x16` ABSOLUTE chain (habu2.f:100 C-CALL-EMIT-ABSOLUTE; opcodes
   D2800010/F2A00010/F2C00010/D63F0200 == LSNAPRBC's detection at habu2.f:2481).
   A REPL captured in hb-stdin-mk encodes hb-stdin-mk's __text primitive
   addresses AND hb-stdin-mk's region-word addresses. LSNAPRBC only rebases a
   detect-range by ONE base delta (value - x21 + x25) and skips region VAs; it
   CANNOT remap the non-uniform host->target primitive offsets, and it CANNOT
   remap region-word offsets (bin/hb's region = [cold-prefix][REPL] vs
   hb-stdin-mk's region = [full toolchain]; different layouts). Confirmed by the
   parallel architecture investigation (subagent): "LSNAPRBC is sufficient ONLY
   for a snapshot of the SAME engine ... NOT sufficient for baking a word
   compiled in hb-stdin-mk's region into bin/hb."
6. bin/hb is NOT tree-shaken (treeshake.f:53 SHAKE?=0 unless build.f/hb-build
   sets it), and the checker is NOT baked (bin/hb dies `cannot open
   src/core/util.f` off-checkout): the cold prefix loads from checkout at boot
   via PFX-LOAD-BASE-FILES (habu2.f:449-469), then the baked REPL LSRC compiles
   on top. So the "cold prefix" IS deterministic and DOES exist in bin/hb's
   region — good — but ONLY bin/hb (STDIN? path) has it; the BIG builders do not.
7. Verified LSRC-content stability (needed by the correct fix below): two SMALL
   engines differing only in REPL LSRC content (added a comment) are 100%
   identical in engine code (40616-byte common run). So SMALL(STDIN?)-engine
   primitive offsets are stable across LSRC edits; capture and re-bake between
   two SMALL engines preserves offsets.
Why M1 still worked: `: AOT-PROBE 12345 ;` is a leaf literal with ZERO calls,
so no offset/relocation ever mattered. Any REPL word (which calls primitives,
cold-prefix words like NULL$, and sibling REPL words) breaks under the M2 NOTE.

## CORRECT APPROACH for M2 (supersedes the M2 NOTE)
The capture MUST happen in an engine byte-identical (modulo ASLR) to the final
bin/hb, i.e. a SMALL STDIN?=true engine whose region is [cold-prefix][REPL] at
the SAME offsets bin/hb will use. Preferred:
- APPROACH 2 (offset-preserving, minimal size overhead — matches the size goal):
  1. Move the seed point post-cold-prefix and drop the M1 offset-0 seed so the
     cold prefix starts at region offset 0 in BOTH the capture engine and the
     final bin/hb (identical region layout; NULL$ and internal REPL calls then
     resolve unchanged).
  2. Build a source-REPL bin/hb (SMALL, as today). Run it with a new "dump-repl"
     mode: after cold-prefix + REPL compile, canonicalize ONLY the __text
     movz/movk/movk chains (subtract this engine's rbase -> pure text offsets;
     region-VA chains 0x3.. are left as-is and already match the final layout),
     then write the REPL region blob [repl-start, cp) + N dict records (name,
     xt-offset, end-offset) to a file. The dump is deterministic (region VAs
     fixed; __text canonicalized).
  3. Rebuild bin/hb: hb-stdin-mk reads the dump, bakes blob + N records via a
     generalized EMIT-AOT-SEED, sets LSRC to empty/install-tail. At boot,
     EM-SEED-AOT copies the blob to CP after cold-prefix (same VA as capture),
     registers the N records, and runs ONE LSNAPRBC pass over the blob with
     detect=[0,text-len], newbase=boot-rbase to re-anchor the __text calls. No
     region rebase needed (layout identical). Then run the REPL top-level
     installs (INSTALL/BPW-INSTALL/S-INSTALL) via the normal boot path (kept as
     a tiny source tail) so per-boot tty/hook state is set.
  4. Fixpoint: the dump is deterministic, so the seeded bin/hb reproduces. The
     build loop (BF-BUILD-STDIN-FROM-STAGE) must gain the extra
     dump-then-rebake pass and prove two-build byte-identity + self-rebuild.
- APPROACH 3 (layout-independent, but adds a reloc table that eats the size win):
  capture in the host, bake a relocation table (call-site-offset -> callee NAME
  via reverse dict lookup), and at seed resolve each name with FIND in bin/hb's
  dict (primitives + cold-prefix + already-registered REPL words) and patch the
  movz/movk chain. Use only if Approach 2's build-pass proves intractable.
Either way this is multi-commit build-infrastructure work, not "reuse LSNAPRBC".
The M1 machinery (single-word, offset-0 seed, no relocation) is the wrong shape
and must be generalized (N records) AND re-homed (post-cold-prefix, capture in a
SMALL engine). Nothing here is a checker miss; it is a codegen/build-layout
constraint.

## M2 IMPLEMENTATION DESIGN (2026-07-03) — two NEW sub-walls found while building
Detailed design pass on Approach 2 surfaced two concrete sub-walls the plan
above glossed, plus a materially simpler path (Approach 3). Recording before any
large surgery so the path is chosen first.

SUB-WALL A — the seed MUST be post-cold-prefix, and a source-token hook there is
unsafe:
- The REPL cannot be seeded before the cold prefix: repl.f/debug*.f use
  `TRUSTED:` and `NULL$` (env-base.f), both COLD-PREFIX words, so the REPL only
  compiles AFTER the cold prefix (verified: `TRUSTED:` is checker-owned, `NULL$`
  is env-base). So the seed point is post-cold-prefix (as the M2 NOTE said) —
  but the M1 seed runs pre-everything (EM-SEED-DICT time), the wrong point.
- The boot compiles the cold prefix via the INTERPRETER over the assembled
  source buffer (PFX-LOAD-* append file paths; C-SOURCE-* at habu2.f:646-777;
  the REPL LSRC is appended only on the tty path, C-SOURCE-APPEND-LSRC:715).
  A seed invoked as a source token (` aot-seed `) runs in INTERPRET mode, whose
  top-level line compiles to a TRANSIENT buffer at `cp@` and executes there
  (LESSONS "cp@ is only stable inside a compiled word"). A seed that copies the
  blob to `cp@` clobbers its own executing line buffer -> crash. Confirmed by the
  emit flow.
- CLEAN HOOK: boot-asm at the source-exhausted transition EM-COMPILE-EXIT / LEXIT
  (habu2.f:3190). At the FIRST top-level exhaustion (guard: LAOTCODELEN!=0 &&
  EVALD-CELL==0 && seed-done==0) `cp@` is CLEAN (= post-cold-prefix region VA,
  deterministic), so seed there: copy blob to cp@, register N dict records
  (write [xt=cp+off][end][namelen|flags][name] at DBASE+NDICT*DREC, then
  `NDICT +1 ; LHIDXADD` PER record — the SAME incremental path runtime `:` uses,
  habu2.f:1541 — so no full LHIDXBUILD needed), advance CP, set seed-done, then
  point INP/INE at the tiny checked install-tail source (` BPW-INSTALL INSTALL
  S-INSTALL `) and fall back into LMAIN so the top-level REPL installs run and
  find the seeded words. Do not wrap this tail in `0 set-check` or add a hook
  manifest row: `repl.f` now uses a typed `defer REPL-READ ( -- ptr u8 n )`,
  installs the fixed `REPLH-CELL` bridge through `[: REPL-READ ;] REPLH!`, and
  `INSTALL`/`S-INSTALL` retarget that vector with checked `is` assignments.
  `BPW-INSTALL` is checked fixed-cell setup. Runs AFTER EM-DATA-INIT, so
  x13/x14/x15 (argc/argv/envp) are already stored -> the M1 register ban is
  relaxed here (still avoid x18/x19/x20 DATA base). One-time flag = a DATA cell.

SUB-WALL B — capture host shape:
- Approach 2 requires capture in a SMALL (STDIN?=true) engine whose region is
  [cold-prefix][REPL] at bin/hb's offsets. The metabuild host (hb-stdin-mk) is
  BIG (STDIN?=false); it does NOT compile the REPL and has the wrong layout. So
  Approach 2 needs a NEW build pass: build a source-REPL SMALL engine, run it in
  a new off-tty `--dump-repl PATH` boot mode (force the SRC-REPL compile path +
  write the dump) to emit a canonicalized dump (blob + N records; __text call
  chains canonicalized to text-offsets via one LSNAPRBC pass with newbase=0;
  region-VA chains left as-is since the seeded layout is identical), then rebake
  the seeded engine reading the dump. Plus dump determinism + fixpoint-loop
  restructuring. This is the build-orchestration wall.

APPROACH 3 (RECOMMENDED — sidesteps SUB-WALL B, no build pass):
- Capture in the HOST (like M1) by adding the REPL files to the stdin bundle so
  the host compiles them at top level (host has checker/env-base, so TRUSTED:/
  NULL$ resolve). Capture is LAYOUT-INDEPENDENT: for each `movz/movk/movk x16;
  blr x16` call site in the REPL blob, REVERSE-LOOKUP the callee xt -> its dict
  NAME (host dict; aot-closure.f already reads records via `dbase@ k 48 * +`),
  and record (site-offset, callee-name) in a de-duplicated reloc table
  (site:u32, name-index:u16 + a small name pool). Bake blob + N records + reloc
  table + install-tail.
- At boot (LEXIT hook from SUB-WALL A): copy blob to cp@, register N records +
  LHIDXADD each, advance CP; then a Forth install-tail word `AOT-PATCH` loops the
  reloc table: FIND(name) (via the existing tick/LFIND path) -> xt, `patch32` the
  3 movk immediates at blob-base+site. Patching the already-placed blob is safe
  (not cp@). Then BPW-INSTALL/INSTALL/S-INSTALL.
- Trade-off: reloc table (~3-4 KB with index encoding: ~400 sites x 6B + ~1KB
  name pool) vs Approach 2's ~0 reloc overhead. Net vs 19KB source is still a
  small WIN, and NEITHER approach reaches the 90-100k target alone (148855 is
  mostly __text engine + the C-SOURCE-STDIN cold-prefix-loader, not the 19KB
  REPL). The size goal needs separate work (e.g. shrinking the ~39KB PFX/stdin
  code block or checkout-side prefix). Approach 3 removes the build-orchestration
  risk and is layout-robust; RECOMMEND it unless the coordinator wants the
  build-pass for the marginally smaller image.

STAGING (Approach 3): (1) LEXIT seed hook + LHIDXADD, proven with the existing
single call-free AOT-PROBE re-homed post-cold-prefix (fixpoint green) — NOTE this
changes when AOT-PROBE is available (post-cold-prefix, so batch programs
referencing it before it exists no longer see it; test via the tty REPL path,
not a pipe). (2) N-record capture + reverse-name reloc + AOT-PATCH, proven with a
2-3 word fixture that calls a primitive AND a sibling AND a cold-prefix word.
(3) REPL bundle: move REPL files into the host bundle for capture, seed at boot,
drop the REPL LSRC to the install-tail, lower gate-build-size baseline in the
SAME commit. Keep the AOT-seeded self-rebuild byte-cmp regression at each stage.
