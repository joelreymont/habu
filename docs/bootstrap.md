# Bootstrap

How the first `hb` is made from nothing, and how that path is checked.

## The bootstrap process

**The seed builds the first `hb`.** `bootstrap/cg/*.fs` runs on Gforth and
writes `hb-stage0`, an ARM64 engine with a machine-code interpreter and its own
copy of the ARM64 primitive bodies. That engine loads the Habu sources, and the
chain below builds `hb` from them. A host the seed does not target gets its first `hb` by
cross-build from any `hb` ([cross-compilation](architecture.md#cross-compilation)).

**The seed mirrors the engine.** It copies the engine facts it needs from the
Habu sources, and `bootstrap/cg/forth.fs` names each mirror where it stands. A
change to the seed or to anything it mirrors runs the
[no-binary recovery check](#periodic-no-binary-check).

## Recovery routes

Use a trusted native hb from the same OS and architecture when one is
available ([below](#recovery-from-a-trusted-native-seed)). On ARM64 macOS or
Linux with no native binary, tools/bootstrap.sh uses the Gforth seed in
bootstrap/cg to build a small recovery engine, then refreshes it from current
source. The recovery engine is a host for a native product build.
Qualification of that product is in [gate.md](gate.md).

The installed release, a copied seed and a newly recovered source-only
engine are different artifacts. Build and test a native product from the
exact source tree before using it as a release candidate.

## Recovery from a trusted native seed

A trusted native hb (a previous release asset, or a seed copied from a trusted
checkout) is a recovery seed, not a qualified product. It runs the checked
recovery command:

```sh
HABU_SEED_SHA256=<hex> /path/to/hb-seed --load tools/seed-main.f \
  -- /path/to/hb-seed
```

`HABU_SEED_SHA256`, when set, pins the seed's digest. `tools/seed.f` installs
the seed as `bin/hb` and refreshes it from current source through the
self-rebuild fixpoint. After that, refresh `bin/hb` as
[Refresh `bin/hb`](#refresh-binhb) describes; the seed is not needed again.

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
and postgres on PATH and libpq 17 or newer for native query cancellation,
run as an ordinary user because initdb refuses root.
On macOS the library rows use Homebrew libzip, libunistring, openssl@3,
libpq and socat; SVG/PDF checks also need librsvg, qpdf and Poppler.
The FFI row compiles its struct-by-value fixture library with /usr/bin/cc.
Keg-only postgresql@18 needs its bin directory on PATH.
See [gate.md](gate.md) for the suite and its boundaries.

## No-Binary Recovery

From the repository root on a supported ARM64 host:

    HABU_ALLOW_BOOTSTRAP=1 GFORTH=/path/to/gforth tools/bootstrap.sh

The launcher targets the host's OS unless HABU_TARGET is set, builds private
stages under HB_TMP (or a new temporary directory), runs recovery checks and
installs bin/hb. That source-only engine can build the native product; it is
not itself the product the native suite qualifies.

For a source-only recovery host, load and install the source compiler before
the native driver's first target scope selects tier 1. With `HOST` naming
that host and `TMP` outside the checkout, build from the target tree:

```sh
cat > "$TMP/native-preload.f" <<'EOF'
require src/compiler/native/compiler.f
NCOMP:INSTALL
EOF
HABU_UNDER_TEST="$HOST" HABU_FIXPOINT_ENGINE="$HOST" HB_TMP="$TMP" \
  "$HOST" --load "$TMP/native-preload.f" tools/native-build.f -- "$OUT"
```

This route emits the ordinary native product and its `.map`; qualify that
pair against the matching sources before promotion.

When a source update adds a baked checker API that snapshot support uses, an
older native host cannot save the new builder support: its ordinary build
refuses with `E-UNDEFINED`. Stage a fresh checker-bearing whitebox host once
through the same compiler preload, then build the product on that host:

```sh
HABU_WHITEBOX_IMAGE=1 HB_TMP="$TMP" \
  HABU_UNDER_TEST="$HOST" HABU_FIXPOINT_ENGINE="$HOST" \
  "$HOST" --load "$TMP/native-preload.f" tools/native-build.f \
  -- "$TMP/hb-whitebox" whitebox
HB_TMP="$TMP" \
  "$TMP/hb-whitebox" --load tools/hb-build.f -- --engine -o "$OUT"
```

Keep `HABU_WHITEBOX_IMAGE` unset for the second command, which builds the
ordinary sealed product and its matching `.map`.

These builds, like every build and gate run from the checkout, write their
objects under its out root ([gate.md](gate.md#scratch-and-cache)).

The Gforth seed cannot emit an x86-64 engine. For Linux x86-64, use a
working ARM64 compiler with the x86-64 backend loaded to cross-build the
native engine, then execute and qualify it on an x86-64 host;
`docs/xcomp.md` "Build and bootstrap" gives the target-specific build and
execution path.

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

`HABU_BOOTSTRAP_CHECK_ONLY=1` builds and checks the private bootstrap chain and
exits before replacing `bin/hb`.

## Generation Chain Check

Run tools/two-generation-build.f for changes to code generation, self-hosting
or capture that can affect successive output. It builds the generations
privately and leaves the installed engine alone; with no argument the
checkout's bin/hb is generation zero, and `--same-host` builds twice with one
host to check repeatability:

    HB_TMP=$TMP bin/hb --load tools/two-generation-build.f -- <seed-engine>
    HB_TMP=$TMP bin/hb --load tools/two-generation-build.f -- --same-host <seed-engine>

Use an external scratch directory for `$TMP`, as described in
[gate.md](gate.md).

A codegen change can first appear in generation two because generation one
was compiled by the old host. Other source changes usually reach generation
one. Compare successive products, not an arbitrary seed and product. A
pre-window build file resolves names against its host, so a newly called
engine primitive or fixed layout declaration may require a staged host.
The window's captured records and cells must not retain the host's process
addresses or record numbers: a host whose checker differs from the tree's
numbers its records and symbols differently, so a record number left in DATA
names a host record, not the product's. For a reserved-layout transition, use
the cold source writer described below.

## Landing a Reserved-Layout Change

A **reserved-layout change** moves a band below `DATA-START`: raising
`SNAP-RELOC:XTCELL-CAP`, appending a band, widening the protected-WID bitmap.
Each one moves `DATA-START` itself, and the host that builds the tree does not
move with it.

Build it with the cold source writer. That writer loads the target tree's
layout before it captures pre-trust declarations, so its compiled layout and
the image's bands agree in the first product, while the donor's own published
heap floor still classifies the donor's address rows
(`tools/native-build-core.f`). The donor must publish a heap floor that matches
its own layout. The build also runs the tree's source in the donor, so an
engine cell that source reads by DATA offset sits at a fixed offset no band
change moves (`src/habu/layout.f` REPLAY-SCOPE, UNIT-COMPILE-CELL,
BOOT-LAYOUT:HEAP-START-CELL); the moving address-cell header is read at the
donor offset the build passes (ADDRESS-CELLS:WITH-HOST-HEADER). A direct saved writer keeps the layout of
the builder it was saved from: rebuild that builder from matching source rather
than use a saved writer from the previous layout. Run `test/native-layout-transition.f` with a
saved same-machine donor whose reserved layout differs to check the first
product, and check multi-generation convergence when codegen, self-hosting or
capture changes require it.

## DDC Audit (Diverse Double-Compiling)

tools/ddc-verify.f is an explicit trust audit. It builds through the
Gforth recovery route and the native route, then compares their refreshed
products byte for byte. It is separate from the ordinary gate and requires
HABU_ALLOW_BOOTSTRAP=1 and a working Gforth. Run the driver from the
repository root only when auditing recovery or a changed seed:

    HABU_ALLOW_BOOTSTRAP=1 GFORTH=/path/to/gforth bin/hb --load tools/ddc-drive.f

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
trusted installed native engine as `HOST` and build from the checkout root
with the candidate command in [gate.md](gate.md#qualifying-a-candidate).
Build a second product with `$OUT` as `HOST`; if those bytes differ, build
another generation until successive products match. Keep each output private,
with its paired `.map` file, and run the [native gate](gate.md) on the
converged product before promotion. A stale host can fail to compile an
application against newer source while still compiling the current engine.

Build from the tree's root: a build opens its boot files under the working
directory and refuses one outside the engine root.

The separate ARM stage-2 recovery/development refresh, the positive stage-2
fixpoint check, is:

```sh
bin/hb --load tools/build-fixpoint-refresh.f -- install --force
```

A retained AOT host resolves named calls against its cold prefix before reading
tool input, so removing a prefix provider can make that host fail with
`hb: AOT call site unresolved`. Use the
[check-only recovery chain](#periodic-no-binary-check) to regenerate a private
seed from current source before refreshing it.

`install` promotes `hb-stdin`, the source-only recovery engine. It does not
qualify that engine as the native product: its checked access to internal
compiler signatures differs from the captured runtime. Set
`HABU_FIXPOINT_ENGINE` to an existing engine to make it both the first build
host and the install destination, isolating this route from a verified
product. On Intel, build the product with `tools/native-build.f` and check
convergence with `tools/two-generation-build.f`; the recovery refresh has no
Intel route. The `snap` verb remains available on both targets: it builds a
native product before saving its image.

The `all`/`install` refresh skips an unchanged rebuild by a content-keyed
stamp at `<out root>/fixpoint/stamp`, where the out root is `$HABU_OUT` (a
relative value against the working directory), else `.out` in the working
directory. Append `--force` to rebuild unconditionally; proof flows always pass
it.

## Stable local command

Keep the checkout's `bin/hb` for development and build candidates privately.
Use one installed tree at a stable path for the executable, map and matching
library sources, with `~/.local/bin/hb` pointing to its `bin/hb`. Update that
installation when needed. Do not create per-revision release directories;
ordinary development does not require installation or publication.

Libraries loaded from source must match the installed engine. Relative paths
resolve by the source-root rule in [forth.md](forth.md#packages), so an
installed engine runs source-based tools from any directory:

```sh
~/.local/bin/hb --load /absolute/path/to/etch/etch.f
```

Keep the installed executable, map and sources together when updating the
installation. Running applications retain their loaded executable; restart
long-running source-loading tools against the updated installation.

On macOS, sign the installed executable with a team-issued identity before it
replaces the old one: `codesign -f -s <identity> <copy>`, then rename the copy
into place. The login keychain partitions an item by its filer's signature; an
ad-hoc signature changes with every build, so a rebuilt `hb` is refused the
items its predecessor filed, while every build signed under one team ID keeps
them. Renaming leaves running engines on their old file.

## Warm Dev Snapshot

For a hot edit/check loop, build a warm snapshot engine with
`bin/hb --load tools/build-fixpoint-refresh.f -- snap`: it writes
`$HB_TMP/hb-new`, a snapshot image that boots warm with the checker, stdlib
prefix and REPL. `hb-new` is a local dev artifact: it is never installed as
`bin/hb`, never used as a gate or candidate launcher, and must be rebuilt after
source changes.

If a tool fails on a missing native primitive, rebuild and qualify the native
product against its matching sources.

Run the gate after bootstrap or refresh (`bin/hb --load test/run.f`,
[gate.md](gate.md)). It runs in the product `bin/hb` engine, never through a
snapshot image; generated snapshot images are local artifacts and are never
committed.
