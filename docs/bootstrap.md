# Bootstrap

How `hb` builds `hb`, and how a missing or broken `bin/hb` is recovered.

## How hb builds hb

`hb --load tools/hb-build.f -- --engine -o <out>` builds this tree's hb inside
the running hb and writes it, with its `.map`, to `<out>`
(tools/hb-build-core.f). The build is a cross-build for the running
machine: the target is data the build writes, never a slice of the
running hb's memory. The text of its start-up files, its kernel and its layout
come from the tree, but which files it loads, and in what order, are the
running hb's, which carries the build command. A kernel build first compiles
the tree's src/arch/arm64/kernel-object.f and writes a kernel object under the
out root, which the build then links. An engine build takes at most 5 minutes; one
that runs past it has a defect to root-cause, never a longer limit.

Build from the tree's root: a build opens its boot files under the working
directory and refuses one outside the engine root. Builds write their objects
under the out root (src/lib/build-out.f), in a directory
named by the building hb's change time and size, so two hb never share an
object.

No route builds hb without an hb. x86-64 builds are off until x86-64 moves onto
the linker.

## Recovery

A trusted native hb of the same OS and machine (a previous release asset, or
a copy from a trusted checkout) rebuilds `bin/hb` from the checkout:

```sh
HABU_SEED_SHA256=<hex> /path/to/trusted-hb --load tools/seed-main.f \
  -- /path/to/trusted-hb
```

`HABU_SEED_SHA256`, when set, pins the trusted hb's digest. `tools/seed.f`
installs it as `bin/hb`, checks that it runs, and has it build `bin/hb` from
current source with `tools/hb-build.f -- --engine -o bin/hb`. That result is a
candidate, not a qualified target: run the
[convergence check](#convergence-check) from it, then
[qualify](#refresh-binhb) the converged hb before installing it.

A trusted hb too old to build the tree needs a [staged host](#staged-hosts).

## Requirements

The full native suite has additional host requirements. On Linux its PTY
tests need working /dev/ptmx and /dev/pts. The database test needs initdb
and postgres on PATH and libpq 17 or newer for native query cancellation,
run as an ordinary user because initdb refuses root. With libpq 16 the test
dies `E-FFI-DLSYM: PQcancelCreate in libpq.so.5`. Debian and Ubuntu keep
initdb and postgres in `/usr/lib/postgresql/<version>/bin`, which must be put
on PATH. The FONT test needs HarfBuzz 14.2 or newer, libharfbuzz and
libharfbuzz-vector, on the loader path (Ubuntu 24.04 ships 8.3), and on
Linux the Ubuntu font packages test/font-native.f's header names.
The Wasm device suite needs wasm-tools and bun on PATH.
On macOS the library tests use Homebrew libzip, libunistring, openssl@3,
libpq and socat; SVG/PDF checks also need librsvg, qpdf and Poppler.
The FFI test compiles its struct-by-value fixture library with /usr/bin/cc.
Keg-only postgresql@18 needs its bin directory on PATH.

## Convergence check

`tools/chain-run-build.f` builds generation 1 with a host, generation 2 with
generation 1, and generation 3 with generation 2 when the first two differ,
each by `tools/hb-build.f -- --engine`, and passes when two successive
generations are the same image (CODESIGN:SAME-IMAGE?):

    TMPDIR=$TMP bin/hb --load tools/chain-run-build.f -- bin/hb $TMP/g1 $TMP/g2 $TMP/g3 $TMP

Run it for changes to code generation, self-hosting or capture that can affect
successive output. Use a scratch directory outside the checkout for `$TMP`.
Every generation compiles the checkout's source, so
leave the tree unchanged until the tool exits.

A codegen change can first appear in generation two because generation one
was compiled by the old host. Compare successive targets, not an arbitrary
seed and target.

## Staged hosts

A change to the kernel, the start-up or the layout lands in one build unless it
raises where DATA starts. One to what the running hb's compiler does with the
tree's code (code layout, records, wordlist numbering, the address list), a
body that calls a primitive its own change adds, a new primitive-spec atom code
(the running checker replays the tree's rows and dies `checker: unknown
primitive-spec atom`), an existing atom code given a new meaning (the running
checker replays the rows with the old meaning and says nothing), or a tool that
calls a word hb carries (the linker) by a name or effect its own change gives
it, or a change to which files a build
loads or their order, needs a stage: the tool itself runs the words hb carries
(outside a build, `require` of a file hb carries loads nothing), though inside
the build each `require` loads the tree's file (src/habu/linker.f
REQUIRE-FRESH). Build the stage first, from a revision that adds the change but
does not yet need it, with the running hb; then build the tree with that stage:

    jj workspace add --name stage -r <revision> "$TMP/stage"
    (cd "$TMP/stage" && TMPDIR="$TMP" hb --load tools/hb-build.f -- --engine -o "$TMP/stage-hb")
    TMPDIR="$TMP" "$TMP/stage-hb" --load tools/hb-build.f -- --engine -o "$OUT"

A tree word that calls hb's word through a `PROVIDED-XT` cell calls the running
hb's copy in a build, so a change to that word's stack effect is two commits.
The first stores the new form in a new cell and keeps the old cell, holding an
adapter, for the callers; the second moves the callers and drops the old cell.
Each builds with the engine of the commit before it.

The link refuses a kernel object the running hb cannot read
(src/arch/arm64/link-kernel.f READ). A change to the object's format needs a
stage that changes the reader before the tree changes the writer. READ also
refuses an object whose DATA-START is above hb's: the hb building
it sends a build's xt! and ptr-cell! stores into its header to the image only
below its own DATA start (src/habu/habu2.f ROUTE-HEADER).

A build types the primitives by the tree's kernel object, not by the running
hb's rows (src/arch/arm64/link-kernel.f STATE-PRIMS), so an engine carries the
facts of a primitive its tree adds after one build. A primitive with a package
row is the exception: the link takes its record from the running hb's kernel
(src/habu/linker.f OWNED+), so hb must hold the primitive before the tree gives
it a package row, or the build stops with `linker: hb has no primitive <name>,
which an owned row of <pkg> names`. The running hb's no-return
axioms each name a primitive (src/core/checker.f NORET-AXIOMS): a tree that
drops one needs a stage that drops the axiom first, or the build stops with
`checker: the kernel object has no row for <name>, which a no-return axiom
names`.

A change in one commit that no revision can stage leaves a host whose hb
predates it unable to build the tree: a package renamed together with the owner
rows that name it makes an hb built before the rename refuse
src/habu/interpreter.f with `E-CAP-TRUSTED`. Seed that host with an engine a
current host cross-builds for it, such as `hb --load tools/hb-build.f --
--engine --target linux-aarch64 -o "$OUT"` for Linux arm64, copied there with
its `.map`; it then builds the tree as any trusted hb does.

## What the engine carries

tools/hb-build-core.f lists the source baked into hb. Loading a baked file by name does not reread edits from disk;
a native rebuild is needed to test those changes. Files outside that closure
load normally from disk. Use the convergence check for changes baked into the
engine.

The engine knows the files it carries by path only (`src/core/include.f`
ENGINE-PROVIDES?). Once the tree renames one, a tool the previous engine runs
that requires the new path loads the file a second time and stops at its first
definition (`duplicate definition`, rc 78), and an engine built from the tree
stops at a `require` of the old path. While the renamed file's bytes equal the
old file's, build the first engine in three steps; edit the file in a later
change:
1. Run the previous engine with a one-line file, `s" <new path>" provided`,
   loaded before `tools/hb-build.f -- --engine -o $OUT`. Never install this
   engine: the next one differs from it.
2. On that engine, build again with `tools/hb-build.f -- --engine -o $OUT`.
3. On the step-2 engine, build once more; qualify and install it once
   CODESIGN:SAME-IMAGE? holds between it and the step-2 engine.

## Refresh `bin/hb`

After `bin/hb` exists, use it as the host for normal native builds. Build a
private hb with `TMPDIR=$TMP $HOST --load tools/hb-build.f -- --engine -o
$OUT` and qualify it: copy it and its `.map` as regular files, not symlinks,
into `bin/` of a private copy of its matching tree and run the test suite
there (test/AGENTS.md). The size report reads the `.map` beside the program
(tools/image-names.f), so copy the two together. Promote
the qualified hb to `bin/hb` only after the suite passes, by copying it beside
`bin/hb` and renaming it into place: a copy over `bin/hb` keeps the inode whose
code signature the kernel cached, and a later launch can die by SIGKILL, rc 137
(the kernel logs `load code signature error 2 for file "hb"`).

If the checkout's `bin/hb` is missing or too old to load current source, use a
trusted installed native engine as `HOST` and build from the checkout root
with the command above. Build a second target with `$OUT` as `HOST`; if those
bytes differ, build another generation until successive targets match. Keep
each output private, with its paired `.map` file, and qualify the converged
target before promotion. A stale host can fail to compile an
application against newer source while still compiling the current engine.

## Stable local command

Keep the checkout's `bin/hb` for development and build candidates privately.
Use one installed tree at a stable path for the executable, map and matching
library sources, `~/.local/lib/habu`, with `~/.local/bin/hb` pointing to its
`bin/hb`. Update it by running `bin/hb --load tools/install.f` from the root of
the tree that passed its checks; the tool's header says what it writes. Do not
create per-revision release directories; ordinary development does not require
installation or publication.

Build and test candidates in a workspace or a temporary directory. An install
never waits for a client: a client that needs an older Habu keeps its own copy.

Libraries loaded from source must match the installed engine. Relative paths
resolve by the source-root rule in [forth.md](forth.md#packages), so an
installed engine runs source-based tools from any directory:

```sh
~/.local/bin/hb --load /absolute/path/to/etch/etch.f
```

Restart long-running source-loading tools against the updated installation.

On macOS every build signs its target with the team identity
(src/lib/codesign.f), and an install keeps that signature; a host without the
identity fails the build with `CODESIGN:E-IDENTITY`. The login keychain
partitions an item by its filer's team, so each build reads the items any other
build filed with at most one prompt per identifier.
