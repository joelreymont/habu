# Bootstrap

## Current recovery status (2026-09-17)

Run it with

```
HABU_ALLOW_BOOTSTRAP=1 HABU_BOOTSTRAP_CHECK_ONLY=1 HABU_TARGET=linux-aarch64 \
  HB_TMP=/tmp/hz-chain tools/bootstrap.sh
```

Measured on linux-aarch64. The chain builds four engines under `HB_TMP` and
they are the artifacts to look at after a run:

| artifact | built by | what it is |
| --- | --- | --- |
| `hb-stage0` | Gforth, through `bootstrap/cg/forth.fs` | the seed engine |
| `hb-stage` | `hb-stage0` from `stage2-src`, then itself to a fixpoint | the stage engine |
| `hb-stdin-mk` | `hb-stage` from `stage2-src` with the stdin driver | the maker for `hb-stdin` |
| `hb-stdin` | `hb-stdin-mk` | the recovery engine the check suites run on |

All four boot their prefix from source (see the cold-runtime rule below) and
the chain completes: the check-only run ends `bootstrap check OK: <HB_TMP>/hb-stdin`
after the five check suites pass on `hb-stdin`, and the full run (no
`HABU_BOOTSTRAP_CHECK_ONLY`) goes on through the native self-refresh to
`bootstrap OK: bin/hb`, installing an engine byte-identical to the one
`tools/build-fixpoint.f` refreshes. Three defects stood between the chain and
that state on 2026-09-16 and each is fixed at its layer: the cold prefix plus
`hb-stdin-mk`'s baked payload had crossed `IBUFSZ` (the prefix rows now drop
their comment and blank lines as they are read, `src/habu/habu2.f`
`EMIT-SOURCE-READ-PREFIX` and its mirror in `bootstrap/cg/forth.fs`, which took
the linux-aarch64 cold prefix from 1,635,735 to 963,642 bytes); the argv scan
read its index across the cold-prefix call (`C-SOURCE-FIND-SEP` now sets its
own); and the check suites named checker internals a from-source prefix
publishes no effect for (the checker publishes `CHECK-QUIET-CANDIDATE!` and
`CHECKER-VIS-PUBLIC` with axioms instead).

What is still narrower on the recovery lineage than on a `tools/native-build.f`
engine is recorded in the tracker: the AOT capture carries the captured REPL's
signatures but not its defer rows, so `require src/habu/debug.f` refuses on a
product engine (`habu-carry-the-captured-bf931ced`), and the optimizing tier
refuses `TFAM:REG-AOT-MERGE-INCOMING?` there (`habu-let-the-product-2f39e054`),
which keeps the cold-host fixtures off that lineage.

The pinned engine at `bin/hb` is the integrated product engine, not a recovery
seed; the release copy for other agents is `/tmp/hazel-release/hb`.

## Requirements

- macOS ARM64 or Linux AArch64.
- Linux gates require a working devpts setup: `/dev/ptmx`, `/dev/pts`, and PTY
  ioctls must be available to the user running the gate.
- Gforth with `{:` locals support. Homebrew `gforth` 0.7.3 is too old.
  A current Gforth snapshot such as `0.7.9_20260610` works.
- The Rocq proof assistant on `PATH`, version 9.2 or newer (`rocq --version`).
  The seven parity gates under `test/compiler/*-proof.f` compile `formal/` through
  `/usr/bin/env rocq` and assert what the run printed, so a host without it fails
  the gate outright. That is deliberate: the proofs are the only thing standing
  between the generated obligations and "nobody checked", and a gate that skipped
  itself on a host missing the toolchain would report green for a machine that
  proved nothing. Install it the way the host installs toolchains (`brew install
  rocq` on macOS) and re-run; do not add skip logic. Verify with
  `bin/hb --load test/compiler/ir-id-proof.f`, which must print `test: ok`.
- GB10 device gates (sm_121a) **require** the pinned 13.3 `ptxas` in
  `~/.habu/toolchain/ptxas-13.3.33`: since `habu-enforce-pinned-ptxas-4598a743`,
  an sm_121 assemble fails closed (`E-PTXTC-STALE`/`E-PTXTC-DIGEST`) unless the
  resolved assembler hashes to the pinned SHA-256 and reports version ≥ 13.3 — the
  older system CUDA 13.0 assembler (which costs ~27% GEMM throughput) is refused,
  not merely warned. Provisioning recipe (archive, sha256, install):
  `../loom/docs/ptx.md` "Pinned ptxas toolchain" (the PTX backend and its recipe live in Loom).

Verify the Gforth requirement:

```sh
tmp=$(mktemp)
printf ': f {: a :} a . cr ; 1 f bye\n' > "$tmp"
gforth "$tmp"
rm -f "$tmp"
```

That command must print `1` and exit zero. If the usable Gforth is not first on
`PATH`, set `GFORTH=/path/to/gforth`.

The preferred persistent local install path is `~/.local/bin/gforth`; verify it
with:

```sh
~/.local/bin/gforth --version
```

The known-good recovery version is `gforth 0.7.9_20260610`.

On macOS, a local snapshot build can be used as the recovery host without
installing it:

```sh
curl -L https://github.com/forthy42/gforth/archive/refs/tags/0.7.9_20260610.tar.gz -o gforth-0.7.9_20260610.tar.gz
tar -xzf gforth-0.7.9_20260610.tar.gz
cd gforth-0.7.9_20260610
./autogen.sh
UNSUITABLE_CC=none ./configure --prefix="$HOME/.local/gforth"
make -j"$(sysctl -n hw.ncpu)" gforth-itc gforth-light.fi
```

If `gforth-fast` is not installed, point `GFORTH` at a tiny wrapper around the
snapshot interpreter and image:

```sh
#!/bin/sh
exec /path/to/gforth-0.7.9_20260610/gforth-itc \
  -i /path/to/gforth-0.7.9_20260610/gforth-light.fi "$@"
```

`tools/bootstrap.sh` only requires the `GFORTH` command to pass the locals probe.
It does not require that Gforth was installed globally.

## No-Binary Recovery

```sh
HABU_ALLOW_BOOTSTRAP=1 GFORTH=/path/to/gforth-or-wrapper tools/bootstrap.sh
```

The script defaults `HABU_TARGET` from the host (`macos-aarch64` or
`linux-aarch64`). Set `HABU_TARGET` explicitly only when the host cannot be
detected.

`linux-x86-64` is a recognised target name here and not a recovery route:
everything under `bootstrap/cg/` emits ARM64 instructions, so both the script
and `bootstrap/cg/sys.fs` refuse that target by name rather than build an
aarch64 engine under an x86_64 label. Recovery for `linux-x86-64` is a
CROSS-BUILD from a working arm64 engine - the OS seam and image writer are
ordinary Habu, so an arm64 engine can write the x86_64 image - and a native
x86_64 recovery chain is later work (docs/x86-64.md).

`tools/bootstrap.sh` does the whole recovery and installs exactly one file:
`bin/hb`.

1. validates that Gforth supports `{:` locals;
2. uses `test/nf.fs` and `bootstrap/` to create private bootstrap executables in
   `HB_TMP` from the same native source layers used by `tools/build-fixpoint.f`;
3. uses those private executables to produce the small stdin engine as `bin/hb`;
4. runs the normal `bin/hb` self-refresh so the installed binary is rebuilt from
   current source and reaches the byte-for-byte fixpoint.

The native refresh certifies each generated compiler payload, then loads it
through the build-only `--build` source mode. That mode does not apply the
ordinary pre-source friend seal; the generated payload owns a mandatory
`SEAL-FRIEND` boundary after the compiler prefix and before its driver. Normal
`--load`, stdin, baked-program, and REPL paths remain sealed before their first
user token.

The ndict seal floor is the other half of that boundary, and `--build` does
**not** open it. The floor - the watermark under which the records are the
engine's own - is armed for every entry before any source runs, because
`--build` is a flag any caller of a shipped engine can pass and the pre-pass
that makes a payload trustworthy is the tool's, not the engine's. A payload's
dictionary rewind therefore goes through `seed-ndict!`, the engine's one
authorized lowering (it refuses a raise, guards the record span it redirects
the next write to, rebuilds the name index, and clears the floor as one
operation, reachable only inside a `TRUSTED:` boundary): `src/habu/hide.f`
`BFR-NDICT!` and `src/habu/prefix-rewind.f` `DICT!` are the two rows that drive
it, both in payload-only files that no shipped engine carries, and
`tools/bootstrap.sh`'s boot-hide prologue (`BOOT-NDICT!`) is the third: the
launcher feeds one prologue to the gforth stage0 and to every sealed native
stage after it, so the seed's prim table answers `seed-ndict!` with its
unsealed lowering and the native stages take the authorized one.
`tools/native-build.f` `LOGICAL-RESET` drives the same seam for the in-process
window build. `CORE-PREFIX:FIRST-RECORD` selects the earliest global `IMK-NDICT0`
dictionary row, matching recovery; the marker cell is not a saved record index.
`seed-ndict!` requires `0 <= n < ndict@` before deriving its target address.
Public `ndict!` still refuses a count below the floor, from a
payload as from any other program (`test/build-rewind-test.f` pins both halves;
`test/seal.f` pins the public refusal).

The temporary files are not build products. The final installed `bin/hb` is the
native checked stdin/TTY engine rebuilt from current source.

An engine primitive that the boot prefix uses must also be registered in the
stage0 generator `bootstrap/cg/forth.fs`, with the semantics that is honest for a
transient seed rather than the native body — a seed that never snapshots
registers nothing for `ptr-cell-mark`, and a static seed image with no loader
slot cannot reach libc for `realpath`. Nothing in the native gate notices the
omission, because the native gate never builds a stage0; the periodic check
below is what catches it, as the stage0 build dying on the bare token name.

## Periodic No-Binary Check

The normal native gate uses an existing `bin/hb`; it does not prove the
from-scratch Gforth recovery path. Run this periodic check after engine/compiler
changes:

```sh
tmp=$(mktemp -d "${TMPDIR:-/tmp}/habu-bootstrap-check.XXXXXX")
HABU_ALLOW_BOOTSTRAP=1 \
HABU_BOOTSTRAP_CHECK_ONLY=1 \
GFORTH="${GFORTH:-$HOME/.local/bin/gforth}" \
HB_TMP="$tmp" \
tools/bootstrap.sh
```

`HABU_BOOTSTRAP_CHECK_ONLY=1` builds the private Gforth/native bootstrap chain
through `hb-stdin` and exits before replacing `bin/hb`. Before returning, the
recovery-built engine runs the top-row hook subprocess regression against
itself; the missing compile-preflight path must exit 70 with empty stdout and
exactly `hb: compile preflight hook missing` plus one LF on stderr.

## Generation Chain Check

`tools/two-generation-build.f` builds five engine generations from one host —
the host builds B1, B1 builds B2, and so on to B5 — each through the production
entry point `tools/native-build.f`, and prints one line per generation. It is
the check that the chain converges instead of accreting image DATA: an engine
that carries more baked DATA than its host boots that DATA into its DP heap and
then persists a copy of it, so the cost lands twice and a later generation dies
in `LOAD-TARGET` with `hb: data space out of range`, rc 76, at `DP-CHECK`
(`src/habu/habu1.f`). It is also the check that the chain reaches its byte
fixpoint: the last pair is compared byte for byte.

```sh
HB_TMP=$PWD/build/tmp bin/hb --load tools/two-generation-build.f -- <seed-engine>
```

With no argument the checkout's own `bin/hb` supplies generation 0. The tool
copies that seed into a fresh private directory under the configured temporary
root (`HB_TMP`) and prints the directory path. Every generation stays there for
inspection; the installed engine is never moved or replaced. It exits nonzero,
naming the generation, when a
generation does not build, when generation 3's image size and shape differ from
generation 2's, or when generation 5 is not byte-identical to generation 4.
`tools/two-generation-probe.f` is the child fixture that reads one engine's
shape.

`tools/native-build.f` and the private `tools/native-bootstrap.f` require one
explicit output path after `--`; there is no implicit `bin/hb` replacement.
Their output passes a basic startup smoke check, which does not establish full
compiler or application acceptance. Keep candidates at private paths until the
required verification passes and select any installed replacement explicitly.

Measured 2026-09-12 on linux-aarch64 from a seed `hb-stdin`, 173 s wall:

```
two-gen: gen 1 built img 5439680 sym-n 12534 usigs 2295704 cap 2359296 norets 135608 cap 196608 rows 25295 heap 10293799 dp-cap 33030080
two-gen: gen 2 built img 5439680 sym-n 12534 usigs 2295704 cap 2359296 norets 135608 cap 196608 rows 25295 heap 10293799 dp-cap 33030080
two-gen: gen 3 built img 5439680 sym-n 12534 usigs 2295704 cap 2359296 norets 135608 cap 196608 rows 25295 heap 10293799 dp-cap 33030080
two-gen: ok gen 3 matches gen 2
two-gen: gen 4 built img 5439680 sym-n 12534 usigs 2295704 cap 2359296 norets 135608 cap 196608 rows 25295 heap 10293799 dp-cap 33030080
two-gen: gen 5 built img 5439680 sym-n 12534 usigs 2295704 cap 2359296 norets 135608 cap 196608 rows 25295 heap 10293799 dp-cap 33030080
two-gen: bytes gen 2 vs 3 1032266
two-gen: bytes gen 3 vs 4 0
two-gen: bytes gen 4 vs 5 0
two-gen: ok gen 5 matches gen 4 byte for byte
```

Generation 1 is the deficient seed-lineage engine — the seed's checker records
nothing across the build window, so B1 bakes a smaller registry — and
generations 2 and 3 agree field for field, which is what "the chain has stopped
growing" means. The whole B1-to-B2 boot-heap step, 262,144 bytes, is the two
persisted checker caps growing (`+196,608` signatures, `+65,536` no-returns);
nothing else moves, and at generation 3 the caps do not move either. The
B2-hosted build peaks at `here - data-base` 30,675,584 against the DP cap the
lines above report (`DATA-SIZE - PROF-CNT-BYTES`), a margin over 2.3 MB.

Before the checker stores were baked at the grain (`USIGS-ROUND-CAP`), those
caps were rounded to a power of two: generation 2 booted at 12,187,580 instead
of 10,221,500, the same build needed about 34.6 MB, and generation 3 stopped
with `two-gen: gen 3 stopped rc 76 hb: data space out of range`. Generation 1
cannot show that difference — its two pool contents round to the same cap under
either policy — so the check has to reach generation 2 to mean anything.

**Two builds by the same host are byte-identical.** They were not until dot
habu-make-the-engine-9db99082: one DATA cell held a process-local address.
`REG-PERSIST-DELTA` (`src/core/checker.f`) kept the distance between a
registry's freshly allotted DATA copy and the grown store's mmap address after
the checker's snapshot persist, and the AOT capture baked the cell, so two
builds from one host differed in the 3-4 bytes of that one cell — 4 bytes at
DATA offset 5354888 in the pair measured. The delta is a transient of that pass
with no reader in a restored image, so it now travels on the stack
(`REG-PERSIST-MOVE`) and occupies no cell. Two seed-hosted builds then agreed
in all 5,701,824 bytes, and `two-gen: bytes gen 4 vs 5` went from 4 to 0.

**Transient storage is released before DATA is copied.** A `DYNAMIC-BUFFER`
control record holds its mapping pointer, byte capacity and private registry
handle. `RESERVE` registers its first live allocation; `RELEASE` removes that
membership and zeroes all three cells. Reserving through an existing compiled
word after capture or restore registers it again. Registry capacity is reserved
before publishing an allocation, and removal updates the moved member's handle.
A small registry lock protects membership changes by independent native tasks;
buffer allocation and copying remain outside it. Capture requires stopped tasks.

`AOT-CAPTURE:CAPTURE` releases the replacement runtime's registry immediately
before `ACAP-BAKE-DATA`. With a retained runtime, it releases only control records
inside the captured DATA span; host and writer buffers outside that value stay
live. A range cutting through a live control record refuses before cleanup.
The snapshot path also walks the registry, immediately before `SND-COPY` in
`SNAP:PERSIST`, after lifecycle callbacks have released dependent resources.
Compiler preparation still resets pass counters and logical state; individual
buffer release lists are no longer needed.

`test/dynamic-buffer-registry.f` covers allocation failure, membership updates,
range boundaries and repeated cleanup. `test/dynamic-buffer-capture.f` captures
one window twice while keeping outside writer buffers live. `test/app-image.f`
dirties a subject-owned buffer, saves and restores twice, reserves through its
original compiled words, and checks that the restored compiler remains usable.
`test/dynamic-buffer-tasks.f` exercises concurrent independent buffer allocation,
growth and release through the shared registry.

**The chain reaches its fixpoint at generation 3; the (4,5) pair stays the
asserted one as margin.** The product is a function of its host as well as of
the source: the capture bakes the window's DATA as its non-zero extents
(offset/length rows plus the bytes, `AOT-WINDOW:EMIT-RUNS` / `EMIT-RBYTES`), so
build-time residue in that DATA changes the run partitioning and displaces
every later section of the image. Until 2026-09-12 that residue made the chain
four generations long: measured between B3 and B4, 21 residue cells, one
non-zero byte each, in the unused tail of a baked boot buffer (`SYM-STR-BOOT`
+983,624, with `SYM-STR-U` at 182,754), worth 21 run rows and 21 run bytes and
the whole of the 1,018,047 differing bytes, while `tools/imgdump.f` reported
identical dicts for B2 and B3. Re-measured 2026-09-12 on engine c684ef54 (tip
a6417a47): `bytes gen 2 vs 3 1032266`, `bytes gen 3 vs 4 0`, `bytes gen 4 vs 5
0`, so B3 already equals B4 in all 5,439,680 bytes and the residue is gone. The
(4,5) assertion therefore holds with one generation of margin; if it ever fails
while (3,4) still passes, a new build-time residue has appeared and the
`cmp -l` offsets name the cell.

Re-measured 2026-09-13 on a seed-hosted chain after `LOC-HW-P` stopped baking
the build window's own DATA address (`src/core/checker.f`): all five generations
report `usigs 2295800 cap 2359296 heap 10293807`, and `bytes gen 2 vs 3` is now
**0** where it was 1,032,266. The remaining 1 MB of displacement was that
residue: the stale base sat above the restored heap top, so `LOC-ADD` wrote bind
widths into DATA that `allot` later handed out, and the capture baked those cells
as extra non-zero runs. The chain now reaches its byte fixpoint at generation 2,
so the (4,5) assertion carries two generations of margin. A boot-time signature
pool grow does not disturb it: two builds hosted by one restored engine forced to
double its pool at boot are byte-identical.

The check is deliberately **not** registered in `test/gate-stdlib-cases.f`: the
five cold builds cost 3-4 minutes, and for the whole of that time the tool owns
the `bin/hb` slot, while `test/run.f` spawns `./bin/hb` children by relative
path out of a bounded process pool. Run it by hand after any change to the
checker's persisted stores, the snapshot writer, or the image layout.

## Landing a Reserved-Layout Change

A **reserved-layout change** is anything that moves a band below `DATA-START`:
raising `SNAP-RELOC:XTCELL-CAP`, appending a band, widening the protected-WID
bitmap. Every one of them moves `DATA-START` itself, and `DATA-START` is the
boundary `tools/native-build.f` classifies the host's declared address rows by --
the cells below the heap are the engine's own declarations and are kept, the rows
above it belong to the retired heap and are discarded.

**The boundary is the host's, and the host says so itself.** `EM-DATA-INIT`
(`src/habu/habu2.f`) publishes the running engine's heap floor at boot in
`BOOT-LAYOUT:HEAP-START-CELL` (`src/habu/layout.f`), out of the same register it
gives DP, so the two can never disagree. `RESET-ADDRESS-ROWS` reads that cell.
`test/heap-start-cell.f` is the registered check that an engine publishes a floor
and that it is the floor its own code uses.

**So: build a reserved-layout change from a post-cell host.** A host that predates
the cell reads zero there, and the build falls back to the source constant, which
is right only when the host's layout equals the tree's. The fallback is bounded
rather than trusted: every row it keeps must lie at or below `$7FF8`, the ceiling
a `DATA <off> LDR` can address and therefore the ceiling of every cell the engine
itself declares, and a kept row above it ends the build by name instead of baking a
retired host address into the image. That catches a host whose bands are **smaller**
than the tree's, which is the direction a growth produces. A host whose bands are
**larger** than the tree's -- a downgrade build -- is the one direction the fallback
cannot see: it drops engine rows early and says nothing. Do not downgrade a layout
from a pre-cell host.

**Verify at generation 2, not generation 1.** A build's host-side layout constants
are the *host's*: `tools/native-build.f` never loads the tree's `src/habu/layout.f`
on the host side, and a booted engine's `require src/habu/layout.f` is a no-op
because its own prefix already registered that path. Only `LOAD-TARGET` reads the
tree's copy, into the image's dictionary. So generation 1 of a moved layout is an
engine whose baked dictionary advertises the new bands while its compiled code
still uses its host's -- measured 2026-09-12: a grown `XTCELL-CAP` built from a
pre-grown host reported `cap 65536` and still refused the 32769th row, and its
published floor was its host's. The two agree from generation 2 on, which is where
`test/heap-start-cell.f` and the row cap mean what they say.

The fallback arm in `RESET-ADDRESS-ROWS`, and `EM-LAYOUT:HEAP-START-OFF`, the
host-side mirror of the cell offset that exists only because a pre-cell host cannot
name `BOOT-LAYOUT`, both go when every host in use has the cell -- the same seed
refresh that retires the by-name arm of dot `habu-retire-the-pre-a37792de`.

## DDC Audit (Diverse Double-Compiling)

`tools/ddc-verify.f` is the explicit (never per-commit) trust audit: it builds
`bin/hb` two independent ways and requires byte-identical output. A seed backdoor
would have to be mirrored in both the Gforth host and the native seed to survive
the sha256 compare, reducing seed trust to "no coordinated cross-host backdoor".
It is gated on `HABU_ALLOW_BOOTSTRAP=1`, like the launcher it drives.

## What the engine carries

`src/habu/native-runtime.f` is the manifest: the ordered list of files a built
engine bakes. It carries the closure of the compiler, the JIT and the REPL and
nothing else, so no package and no type signature reaches the image unless one
of those three requires it. `tools/manifest-lint.f` enforces that — it reads
the manifest's rows, walks the require graph out of the entry points it
declares (the files the engine loads for their own sake, each with its reason
on the row), and refuses a row that nothing in the closure requires. Run it as
`bin/hb --load tools/manifest-lint.f`; `tools/manifest-lint-test.f` drives its
line reader against the near misses and then checks the live tree.

A library the engine does not bake is still one `require` away, and that is the
intended cost: `require lib/vector.f` is about 19 ms at tier 0, and the
debugger — `require src/habu/debug.f`, which pulls the stepper and the shared
watch cells with it — about 14 ms. `bootstrap/cg/forth.fs` mirrors the
manifest's prefix rows, so a row that moves here moves there too.

The two chains and their comparison point:

- **Native chain** — the current `bin/hb`, which reproduces itself at the native
  fixpoint (`install --force` is byte-identical). This is the reference.
- **Gforth chain** — `tools/bootstrap.sh HABU_BOOTSTRAP_CHECK_ONLY=1` emits a raw
  seed engine `hb-stdin` via Gforth; the audit then runs the native fixpoint
  refresh on that seed (the exact `install --force` step the full recovery runs
  after `mv hb-stdin bin/hb`), re-targeted to a scratch engine path via
  `HABU_FIXPOINT_ENGINE` so the checkout's `bin/hb` is never replaced.

DDC compares **at the fixpoint**: the Gforth chain's refreshed engine must be
byte-identical to the native `bin/hb`. It does **not** diff the raw `hb-stdin`
seed directly. The raw seed is captured by a Gforth-lineage stage whose live REPL
sits at different absolute addresses than the native host, so its baked AOT-REPL
blob carries that host's `movz/movk` address immediates (currently ~542 `__text`
bytes plus the downstream code signature). `EM-SEED-AOT` re-relocates those bytes
at boot — they are dead yet host-dependent, so a raw seed-vs-fixpoint diff
diverges by design. The native fixpoint refresh re-captures the AOT blob from the
canonical small engine (identical layout regardless of Gforth-vs-native lineage),
which erases the dead host addresses; the two chains then converge byte-for-byte.

Ensure `bin/hb` is a fresh native fixpoint before the audit, then run it (the
Gforth chain needs `gforth` on `PATH` or `GFORTH` set, per Requirements above):

```sh
bin/hb --load lib/errors.f lib/string.f lib/memory.f lib/fs.f lib/fs-mutate.f \
  lib/process.f lib/process-argv.f lib/process-env.f lib/codesign.f \
  tools/build-fixpoint.f tools/build-fixpoint-main.f -- install --force

HABU_ALLOW_BOOTSTRAP=1 GFORTH="${GFORTH:-$HOME/.local/bin/gforth}" bin/hb --load \
  lib/errors.f lib/string.f lib/memory.f lib/fs.f lib/fs-mutate.f \
  lib/process.f lib/process-argv.f lib/process-env.f \
  tools/ddc-verify.f tools/ddc-drive.f
```

where `tools/ddc-drive.f` is a one-line `DDC-MAIN`. The tool prints
`ddc: byte-identical <sha>` and exits 0 on match, or `ddc: DIVERGENT` with both
digests, both lengths, and the first differing byte offset, and exits 1. It runs
for a few minutes (the Gforth chain dominates).

VERIFIED (2026-07-16): the two chains are byte-identical at the fixpoint. A
`DIVERGENT` verdict is a real finding — a genuine cross-host toolchain divergence
(a candidate coordinated seed backdoor, a non-deterministic emit, or a stale
`bin/hb` that was not refreshed to the fixpoint first). Investigate the first
differing offset before trusting either binary; do not paper over it in the tool.

## Refresh `bin/hb`

After `bin/hb` exists, do not use Gforth for normal work:

```sh
bin/hb --load tools/build-fixpoint-refresh.f -- install
```

`bin/hb --load` selects the host core/checker/env source prefix from the
running binary. Callers load only the libraries and tool source they need.

The `all`/`install` refresh is content-keyed. After a successful install the
tool writes a stamp — SHA-256 over the digests of `bin/hb`, the exact emitted
fixpoint and stdin stage sources captured at the moment the build consumed
them, and the whole ordered `require`/`include` closure of the native compiler
chain (`src/compiler/native/compiler.f`) — to `$HABU_FIXPOINT_STAMP` if set, else
`$XDG_CACHE_HOME/habu-fixpoint/stamp`, else `~/.cache/habu-fixpoint/stamp`.
A repeated refresh with an unchanged engine, unchanged stage sources and an
unchanged chain prints `fixpoint: cached <key-prefix>` and exits 0 without
rebuilding (~1s instead of ~7s). Because the stamp key includes the hash of the
current `bin/hb`, a replaced or stale engine can never false-skip: any byte
change to `bin/hb` or to a compiled stage source changes the key and forces the
full refresh. The chain is keyed separately because it reaches the stage engine
as prefix source read straight from the checkout rather than as emitted stage
bytes, so without that fold a chain edit would change no other stamp input; the
closure is walked once per refresh, before the build, and costs about 0.16s.
An edit to a file the chain does not load leaves the key alone.
Append `--force` to bypass the stamp and rebuild unconditionally; proof flows
(`tools/seed.f`, `tools/bootstrap.sh`) always pass `--force`. `-- all` only
writes the stamp when its product is byte-identical to `bin/hb`.

## Warm Dev Snapshot

For a hot edit/check loop, build a warm snapshot engine with the same command
but `-- snap`: it writes `$HB_TMP/hb-new`, a snapshot image that boots warm
(~0.02s vs ~0.07s) and checks user source at least as fast as `bin/hb`. The
snapshot build retires the image-writer/compiler tail before writing, so the
image carries only the dev surface (checker, stdlib prefix, REPL);
the checker stays fail-closed (a bad definition exits 70). `hb-new` is a
local dev artifact: it is never installed as `bin/hb`, never used as a gate
or candidate launcher, and must be rebuilt after source changes.

If a device tool (`maki/eval/device.f`, `maki/gpu.f`, `tools/ptx/*`) errors with a
cryptic missing-primitive name such as `ffi-call-abi`, the running `bin/hb` predates a
native FFI primitive — **refresh it with the command above.**

Run the gate after bootstrap or refresh:

```sh
bin/hb --load test/run.f
```

This is the native port gate. It runs every entry in the one native registry
through the installed `bin/hb`, using a bounded process pool and one private
capture root.
It intentionally does not run LLM benchmark fixtures or require JavaScript,
Python, Rust, TypeScript, or model runtimes.

The test suite runs directly in the small `bin/hb` engine; it does not bake a
top-level test-suite snapshot and it does not use checker/tool snapshot images
as launchers. Build and install the exact tree first with the refresh command
above. Generated snapshot images are local artifacts and must not be committed.

## Future Port Checklist

1. Add one target source seam under `src/os/<target>/` for syscalls, executable
   layout, signing policy, terminal constants, and target metadata. Startup
   argv/envp access stays shared in `src/os/env-base.f`; do not add target
   `env.f` fallbacks.
2. Wire the target into `tools/bootstrap.sh`, `tools/build-fixpoint.f`, and the
   native source-list builders (`src/habu/habu2.f`, `bootstrap/cg/forth.fs`,
   `src/habu/stdin.f`, and `tools/hb-build-lib.f`) so bootstrap, refresh,
   `--load`, baked REPL, and AOT all select the same target prefix automatically
   from the running `bin/hb`.
3. Recover `bin/hb` only if needed with `HABU_ALLOW_BOOTSTRAP=1
   tools/bootstrap.sh`; after that, use only native `bin/hb`.
4. Run the refresh command above and require a byte-for-byte fixpoint.
5. Run the native port gate above on the target machine.
