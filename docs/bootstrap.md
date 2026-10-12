# Bootstrap

How the first `hb` is made from nothing, and how that path is checked.

## The bootstrap process

**The seed builds the first `hb`.** `bootstrap/cg/*.fs` runs on Gforth and
writes `hb-stage0`, an ARM64 engine with a machine-code interpreter and its own
copy of the ARM64 primitive bodies. That engine loads the Habu sources
(`bootstrap/cg/forth.fs` `EMIT-HOST-LOAD-PREFIX`), and the chain below builds
`hb` from them. A host the seed does not target gets its first `hb` by
cross-build from any `hb` ([cross-compilation](architecture.md#cross-compilation)).

**The seed mirrors the engine.** It copies the engine facts it needs from the
Habu sources, and `bootstrap/cg/forth.fs` names each mirror where it stands. A
change to the seed or to anything it mirrors runs the
[no-binary recovery check](#periodic-no-binary-check).

## Recovery routes

Use a trusted native hb from the same OS and architecture when one is
available; [seed.md](seed.md) gives that route. On ARM64 macOS or Linux with
no native binary, tools/bootstrap.sh uses the Gforth seed in bootstrap/cg
to build a small recovery engine, then refreshes it from current source.
The recovery engine is a host for a native product build. Qualification
of that product is in [gate.md](gate.md).

The installed release, a copied seed and a newly recovered source-only
engine are different artifacts. Build and test a native product from the
exact source tree before using it as a release candidate.

## Requirements

The Gforth seed emits ARM64 code and supports macOS ARM64 and Linux AArch64.
It requires Gforth 0.7.9 or newer with locals support. Check the selected
interpreter before recovery:

    printf ': x {: a :} a . ; 7 x bye\n' | /path/to/gforth

The command must print 7 and exit zero. Gforth 0.7.3 lacks the required
locals. A known-good release is 0.7.9_20260610. If no Gforth is installed,
use its complang dist tarball, which includes the prebuilt bootstrap images:
https://www.complang.tuwien.ac.at/forth/gforth/Snapshots/0.7.9_20260610/gforth-0.7.9_20260610.tar.xz
The GitHub tag archive requires an existing Gforth to build.

The full native suite has additional host requirements. On Linux its PTY
tests need working /dev/ptmx and /dev/pts. The database row needs initdb
and postgres on PATH, run as an ordinary user because initdb refuses root.
On macOS the library rows use Homebrew libzip, libunistring, openssl@3,
libpq and socat; keg-only postgresql@18 needs its bin directory on PATH.
See [gate.md](gate.md) for the suite and its boundaries.

## No-Binary Recovery

From the repository root on a supported ARM64 host:

    HABU_ALLOW_BOOTSTRAP=1 GFORTH=/path/to/gforth tools/bootstrap.sh

The launcher selects macos-aarch64 or linux-aarch64 from the host unless
HABU_TARGET is set. It validates Gforth, builds private stages under HB_TMP
(or a new temporary directory), runs recovery checks and installs bin/hb.
The successful full run ends with “bootstrap OK: bin/hb”. The installed
source-only engine can build the native product; it is not itself the
product qualified by the native suite.

The Gforth seed cannot emit an x86-64 engine. For Linux x86-64, use a
working ARM64 compiler with the x86-64 backend loaded to cross-build the
native engine, then execute and qualify it on an x86-64 host. See
[x86-64.md](x86-64.md) for the target-specific build and execution path.

A seed primitive used by the cold prefix must also exist in
bootstrap/cg/forth.fs. The normal native gate does not build that seed,
which is why the separate no-binary check below exists.

## Periodic No-Binary Check

The normal native gate uses an existing `bin/hb`; it does not prove the
from-scratch Gforth recovery path. Run this separate check after changes to
the recovery seed, mirror, launcher or dependencies, or for an explicit release
recovery audit:

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

tools/two-generation-build.f builds five generations privately through
tools/native-build.f and checks image shape and the final byte fixpoint.
Run it for changes to code generation, self-hosting or capture that can
affect successive output:

    HB_TMP=$PWD/build/tmp bin/hb --load tools/two-generation-build.f -- <seed-engine>

With no argument the checkout's bin/hb supplies generation zero. The tool
prints its private output directory and leaves the installed engine alone.
Its same-host mode builds twice with one host to check repeatability:

    HB_TMP=$PWD/build/tmp bin/hb --load tools/two-generation-build.f -- --same-host <seed-engine>

A codegen change can first appear in generation two because generation one
was compiled by the old host. Other source changes usually reach generation
one. Compare successive products, not an arbitrary seed and product. A
pre-window build file resolves names against its host, so a newly called
engine primitive or fixed layout declaration may require a staged host.
The window's captured records and cells must not retain the host's record
numbers or process addresses; test/host-checker-row-e2e.f checks a changed
host primitive table. For a reserved-layout transition, use the cold source
writer described below.

## Landing a Reserved-Layout Change

A cold source build binds the writer to the fresh target layout before it
captures pre-trust declarations. A 64-entry donor can therefore build a target
with 128 pending slots and pass `test/pre-trust-defer.f`. A saved builder or
direct donor writer retains the donor's layout and cannot stage that transition;
use the cold source writer for a reserved-layout change.

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

**Use a donor with a matching published layout.** `RESET-ADDRESS-ROWS` refuses a
zero heap floor or a floor that disagrees with the donor's `DATA-START`; it has no
pre-cell fallback. `CHECK-HOST-LAYOUT` also validates the donor's declared fixed
slots before the target window opens. These checks describe the running donor,
whose layout can differ from the target tree's.

**The cold source writer uses the target layout in generation 1.** `LOAD-TARGET`
reads the tree's `src/habu/layout.f` into the fresh target dictionary.
`SOURCE-WRITER` then resolves the source emitter against that dictionary, so its
compiled layout and the image's advertised bands agree in the first product.
The live donor's heap floor remains the boundary used by `RESET-ADDRESS-ROWS`.
The PD64 transition passed the first-product pre-trust, using and protection
checks; its five-generation chain converged byte for byte.

**A direct saved writer retains its builder's layout.** Its emitter was compiled
when the builder image was saved. Rebuild that builder from matching source for
a reserved-layout transition; do not use a saved writer from the previous layout
to construct the new one. Continue to check multi-generation convergence when
codegen, self-hosting or capture changes require it.

## DDC Audit (Diverse Double-Compiling)

tools/ddc-verify.f is an explicit trust audit. It builds through the
Gforth recovery route and the native route, then compares their refreshed
products byte for byte. It is separate from the ordinary gate and requires
HABU_ALLOW_BOOTSTRAP=1 and a working Gforth. Run the driver from the
repository root only when auditing recovery or a changed seed:

    HABU_ALLOW_BOOTSTRAP=1 GFORTH=/path/to/gforth bin/hb --load \
      lib/errors.f lib/string.f lib/memory.f lib/fs.f lib/fs-mutate.f \
      lib/process.f lib/process-argv.f lib/process-env.f \
      tools/ddc-verify.f tools/ddc-drive.f

A past match does not establish that the current source or engine matches.
Inspect a DIVERGENT result before claiming the routes converge.

## What the engine carries

src/habu/native-runtime.f is the ordered manifest of source baked into a
native product. Loading a baked file by name does not reread edits from disk;
a native rebuild is needed to test those changes. Files outside that closure
load normally from disk. Run bin/hb --load tools/manifest-lint.f when changing
the manifest, and use the generation check for changes baked into the engine.

## Refresh `bin/hb`

After `bin/hb` exists, use it as the host for normal native builds. Build a
private product image with `tools/native-build.f -- <out>` and qualify that
image against its matching source tree as described in [gate.md](gate.md).
Promote the verified product to `bin/hb` only after the gate passes.

If the checkout's `bin/hb` is missing or too old to load current source, use a
trusted installed native engine as `HOST` and build from the checkout root:

```sh
HABU_UNDER_TEST="$HOST" HABU_FIXPOINT_ENGINE="$HOST" HB_TMP="$TMP" \
  "$HOST" --load tools/native-build.f -- "$OUT"
```

Build a second product with `$OUT` as `HOST`; if those bytes differ, build
another generation until successive products match. Keep each output private,
with its paired `.names` file, and run the [native gate](gate.md) on the
converged product before promotion. A stale host can fail to compile an
application against newer source while still compiling the current engine.

Build from the tree. A build's boot files are recorded and opened under the
working directory; the target's prefix sources may be read from the host
executable's tree. A build from a directory without the tree dies at the
first boot file it opens (`include: cannot open <cwd>/src/core/…`), exit 74,
and writes nothing (`test/source-root-exe-test.f`). A boot file outside the
root is refused by name, exit 74:
`source root: a boot file is outside the engine root: <path>`.

The separate stage-2 recovery/development refresh is:

```sh
bin/hb --load tools/build-fixpoint-refresh.f -- install
```

A retained AOT host resolves named calls against its cold prefix before reading
tool input. Removing a prefix provider can make that host exit 82 with
`hb: AOT call site unresolved`. Use the [check-only recovery chain](#periodic-no-binary-check)
to regenerate a private seed from current source before refreshing it.

`install` promotes `hb-stdin`, the source-only recovery engine. It does not
qualify that engine as the native product. In particular, its checked access
to internal compiler signatures differs from the captured runtime. Use
`HABU_FIXPOINT_ENGINE` to keep this route isolated from a verified product.

`bin/hb --load` selects the host core/checker/env source prefix from the
running binary. Callers load only the libraries and tool source they need.

The first stage-2 build runs on `HABU_FIXPOINT_ENGINE` when set (otherwise
`bin/hb`); subsequent stage-2 generations run the resulting private `hb-stage`.
The selected engine must already exist and is also the install destination.
Setting it therefore isolates both the build host and installation from the
checkout's engine.

Before installation replaces the engine, the staged candidate boots from a fresh
copy of `src/` and `lib/`, requires its compiler and compiles and executes a
tier-1 probe. A timeout, failed process or wrong answer refuses promotion and
removes the temporary copy; the existing engine and stamp remain intact. Running
from the copied tree also checks that provided modules survive relocation.

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

## Stable local command

Keep the checkout's `bin/hb` for development. Install a verified product and
its matching sources under `~/.local/lib/habu/<revision>/`, with `current`
selecting that version and `~/.local/bin/hb` pointing to `current/bin/hb`.
Changing the checkout then leaves dependent projects on the verified version.

Libraries loaded from source must match the installed engine. Habu resolves
application paths from the application's root, then the working directory,
then the engine's source root: the tree two levels above the executable,
found through the `~/.local/bin/hb` and `current` symlinks. So an installed
engine runs source-based tools from any directory:

```sh
~/.local/bin/hb --load /absolute/path/to/etch/etch.f
```

A working directory that is itself a Habu tree (it holds `src/core/util.f`) is
the root instead, so inside a checkout the checkout's sources load
(`test/source-root-exe-test.f`). The working directory is searched before the
root, so while the root is the executable's tree, one that is not a tree must
not hold copies of engine files: its `lib/errors.f` loads as an application
file beside the engine's, a duplicate definition, exit 78. With no tree at
either place the working directory is the root, and its copy is the engine's
row, never loaded.

Retain the previous version when promoting a new verified release so switching
`current` back restores both the binary and its sources.

## Warm Dev Snapshot

For a hot edit/check loop, build a warm snapshot engine with the same command
but `-- snap`: it writes `$HB_TMP/hb-new`, a snapshot image that boots warm
(~0.02s vs ~0.07s) and checks user source at least as fast as `bin/hb`. The
snapshot build retires the image-writer/compiler tail before writing, so the
image carries only the dev surface (checker, stdlib prefix, REPL);
the checker stays fail-closed (a bad definition exits 70). `hb-new` is a
local dev artifact: it is never installed as `bin/hb`, never used as a gate
or candidate launcher, and must be rebuilt after source changes.

If a tool fails on a missing native primitive such as `ffi-call-abi`, rebuild
and qualify the native product against its matching sources.

Run the gate after bootstrap or refresh:

```sh
bin/hb --load test/run.f
```

This is the native port gate. It runs every entry in the one native registry
through the installed `bin/hb`, using a bounded process pool and one private
capture root.
It intentionally does not run LLM benchmark fixtures or require JavaScript,
Python, Rust, TypeScript, or model runtimes.

The test suite runs directly in the product `bin/hb` engine; it does not bake a
top-level test-suite snapshot and it does not use checker/tool snapshot images
as launchers. Build the exact tree first with `tools/native-build.f` and follow
[gate.md](gate.md). Generated snapshot images are local artifacts and must not
be committed.

## Future ports

The Gforth seed currently emits ARM64 only. Bring up another host by
cross-building with an existing Habu compiler and executing the product
on the new host. Keep target-specific requirements and qualification in
[portability.md](portability.md) and the target's own document. Extend
the no-binary seed only when native recovery on that host is needed.
