# Native applications with a REPL

Build a standalone application from the Habu checkout:

```sh
bin/hb --load tools/hb-build.f -- --repl app.f -o app
```

The executable contains the application's compiled definitions and state, the
native compiler, the checker, and the Habu REPL. It runs from another working
directory without the checkout or application source files.

Define a checked global `MAIN ( -- )` as the entry point. Habu calls it once in
each new process, before reading stdin. The build rejects any other entry
effect. Uncaught startup throws terminate the process. Returning from `MAIN`
enters the interpreter: terminal input opens the interactive REPL, piped input
executes as checked Habu, and EOF exits. A command that should finish immediately
can use `die` with its message and exit status.

```forth
package APP
public

: GREET ( -- ) s" hello" type cr ;

;package

: MAIN ( -- )
   SCRIPT-ARGC 0= if APP:GREET exit then
   0 SCRIPT-ARGV$ type cr ;
```

`SCRIPT-ARGC` and `SCRIPT-ARGV$` read the new process's application arguments.
An optional leading `--` is removed. Thus `./app hello` and `./app -- hello`
both provide one argument, `hello`. These application arguments are not treated
as Habu source filenames. A program can choose a command such as `repl` and
return from `MAIN` when it sees it.

An application image's arguments start at `argv[1]`, and that holds for a
stripped image as well as a `--repl` one: `./app one two` gives `SCRIPT-ARGC` 2,
argument 0 `one` and argument 1 `two`. `SCRIPT-ARG-START` (`src/os/script-argv.f`)
decides between that convention and the engine's own — where `argv[1]` names a
source file and the program's arguments begin after a `--` — by reading
`APP-ENTRY:XT-CELL`, the cell that says *this process is an application*. A
`--repl` image's entry record writes it with `APP-IMAGE:START!`; a stripped image
has no record to write it, so its entry publishes the claim itself
(`src/habu/aot-owned-cells.f`). Left unpublished the cell reads zero, the image
answers as the engine would and its FIRST argument disappears.

A separate vector runs program code on the way *out*. The engine calls the xt in
`EXIT-HOOK-CELL` (`src/habu/layout.f`) once, with the cell cleared before the
call, immediately before the `exit_group` of a deliberate exit: the normal
top-level exit, `die`, and an uncaught top-level throw. A stripped image has no
engine label to call, so its entry emits the same sequence inline and runs it
before its own `exit(0)` — returning from `MAIN` fires the hook. The engine's
fail-closed exits (`ENGINE-ERROR:*`, rc 70/74/75/76) do not call it; a broken
engine runs no program code. The exit code is preserved across the call, and a
hook that dies or throws cannot recurse, because the cell is already empty when
it runs. `lib/fs-mutate.f` is the one library that arms it: registering a
cleanup path installs `CLEANUP-AT-EXIT`, and a vector another component holds is
chained rather than replaced.

Application source is loaded once during the build. Top-level initialization
runs then; it is not replayed at startup. Put work that needs the new process,
such as opening a window or socket, in `MAIN` or a word it calls. If build-time
initialization acquires a process resource, register its cleanup with
`IMAGE-LIFECYCLE:REGISTER ( [ -- ] -- )`. Capture runs those callbacks before
saving state; a failed callback prevents the image from being written.

The native REPL build compiles the current source into a fresh running image.
It does not use the AOT maker or artifact caches. The existing build report
therefore records no cache source and no cache hits for `--repl`.

### Process page size

`OS-MEMORY:PAGE-SIZE` reports the current process's host page size through the
checked `getpagesize` binding. It is a runtime fact for mapping descriptions;
it does not replace Habu's fixed `STACK-ABI:PAGE-BYTES` guard-window contract or
change allocation growth. The query resolves again after image preparation, so
an image never carries a process-owned function address. The Linux AArch64 path
is covered by `lib/os-memory-test.f`; macOS and x86-64 are supported by the
same process-symbol contract but are not exercised on this host.

Every build prints one line saying where the bytes of the image it just wrote
went, `--size-report` prints the whole table and `--report-json` carries the
same numbers in the report object's `size` field. A `--repl` image is mostly
zero bytes — it copies the DATA window verbatim and the dictionary slot array
whole — which is a property of the format and not of your program; [Where an
application image's bytes go](engine-size.md#where-an-application-images-bytes-go)
measures both classes and says what moves the number. It attributes the code
band by package and the DATA window by owner, and the answer for a `--repl`
image is usually the build's own compiler rather than the application: the maker
child loads the tier-1 optimizer from source before your program, and the
snapshot keeps it.

## Stripped images and persistent execution tokens

Without `--repl` the same driver builds a *stripped* image: only the closure of
`MAIN` travels, with no compiler, no dictionary and no REPL, which is what lets
a unit run it under `MemoryDenyWriteExecute`.

```sh
bin/hb --load tools/hb-build.f -- app.f -o app
```

**The startup addresses its four far labels through the code base.** The fixed
entry `src/habu/aot-lib.f` emits sits at text offset zero and has to name four
things that bind past the copied code: the entry word, the crash handler, the
signal stub and the sparse data blob. `adr` reaches ±1 MiB, and a program whose
closure is larger than that puts all four out of reach — Tender's standalone
build died `icode: adr out of reach site=844 target=1363312`, 844 bytes into the
startup for a label 1.36 MB later. Each of the four now goes through `TEXT-ADR,`:
the label's byte offset from the code base in a movz/movk pair, `adr` to `LTEXT`
at text offset zero, and an add — sixteen bytes a site, forty-eight more per
image, and no bound on how far the label may be. The base is `LTEXT` and not the
`TEXT-BASE` claim's cell, because the startup publishes that cell itself and the
first of these four sites runs before the store. Nothing else in an emitted image
crosses the window: a copied body's own instructions are re-encoded by the
linker's relocation with its own refusal, and data and string references travel
as movz/movk chains. `tools/aot-startup-reach-lint.f` keeps the rule — an
emitter may only `ADR,` a label its own definition binds.

### What the window contains

The maker child opens the capture window **before anything the application can
`require` is loaded**, so the application's own require closure is the only
library content inside the restored span. The maker's two halves enforce that:
`tools/aot-build-open.f` loads a lib-free prefix (`lib/executable-build.f`,
`src/os/script-argv.f`, `src/habu/aot-window-latch.f`), opens the window, loads
the application and latches the span; `tools/aot-build-core.f` then brings in the
linker, `src/habu/app-image.f` and the eight `lib` modules it pulls in, which all
land above the span. A module the application has already loaded is shared in the
harmless direction: the linker uses the application's copy, at build time only.

The application therefore sees **only what it requires itself**. Nothing is
preloaded on its behalf any more, so a program that used a `lib` word without
requiring its module — which the maker's own eight modules used to supply — now
fails to compile, naming the undefined word.

The invariant matters because a library loaded *before* the window is the copy
the application's own `require` resolves to. Its persistent cells then sit below
the span, and the linker refuses the image with *address refers to data outside
the restored span*. That refusal is correct — such a cell would read as zero in
the image — so the fix is to load nothing early, not to relax the check.

Cells the **engine itself** bakes are below the window whatever the maker does.
Some of them are not persisted data at all: they are runtime inputs, the way
argv is, or state that starts empty in every runtime instance. The stripped
entry owns those exactly as it already owns `x20`, `S0-CELL` and `DP-CELL`, and
the closure walker admits them.

**The list is `src/habu/aot-owned-cells.f`**, and a cell is on it because it is
*named* there — never because of its value, its address, or the file it lives
in. Each entry declares how the entry initialises it:

| cell | declared in | claim | who initialises it |
| --- | --- | --- | --- |
| `ENV-DATA-PTR` | `src/os/env-base.f` | image base | the entry stores this image's own DATA base (`x20`), the value the file writes when the engine loads |
| `ARGC-CELL`, `ARGV-CELL`, `ENVP-CELL` | `src/habu/layout.f` | fixed startup cells | the entry stores the kernel's `argc`/`argv`/`envp`, read off the untouched entry frame before anything else runs |
| `APP-ENTRY:XT-CELL` | `src/habu/layout.f` | entry xt | the entry stores the address of the word this image starts, the token an entry record would have written with `APP-IMAGE:START!` |
| `ENV-Z`, `ENV-A`, `ENV-U`, `ENV-QA`, `ENV-QU` | `src/os/env-base.f` | fresh | nothing: they are `GETENV`/`ENV=?` cursors, written from the caller's arguments before they are read, and the fresh mapping's zero is their correct start |
| the registry head, count and lock | `src/core/dynamic-storage.f` | fresh | nothing: a runtime instance with no mapping, no members and an open lock is correct, and that is the zero a fresh anonymous mapping holds |
| the `WITH-BYTES` scope stack | `lib/memory.f` | fresh | nothing: depth zero with no cached mapping is the same fresh state |
| `RBASE-CELL` | `src/habu/layout.f` | text base | the entry stores this image's own text content base — the address of its first instruction — which `rbase` answers and the engine's own entry writes at boot |
| `PZB` | `src/core/util.f` | fresh bytes, `PATH-CAP` + 1 | nothing: it is the one path scratch, and `PATHZ` writes the caller's path and its NUL into it before `PATH0` hands it to `open` inside that one call |

A **fresh** claim emits no instruction at all — `EMIT-DATA-REGION-MAP` has just
mapped DATA anonymously, so the cell already holds the zero the claim declares
correct. `FRESH-BYTES` is the same claim over a **buffer**, named with its byte
length, for scratch that is written before it is read within one call. An
**image-base** claim gets one store of `x20`, an **entry-xt** claim one store of
the entry root's own address, and a **text-base** claim one store of the image's
own code base. Both readers work from
that one table: `src/habu/aot-lib.f EMIT-OWNED-CELLS` emits what each claim
declares, and `src/habu/aot-closure.f CLAIMED-CELL?` admits exactly the same
addresses, so the entry and the walker cannot disagree about a cell.

**A claim's length is its extent, whatever its kind.** `CLAIMED-AT?` admits
`[cell, cell + length)` — one cell for the kinds that name a single cell, the
declared span for a carried table or a fresh buffer — because a buffer is
reached at an interior offset as often as at its head: `PATHZ` writes its NUL at
`PZB + u`. The bytes *next* to a claim are refused as loudly as ever; the range
rule admits what a claim declares and never its neighbours.

The list lives with the linker and not beside the cells it names, because a
claim is a DATA offset and **an offset computed while the engine's own prefix
loads does not survive the capture that makes an engine**: the prefix compiles
into the building host's DATA, millions of bytes above that host's base, and the
image keeps only the captured span. Measured on a generation built with the
claims in `src/os/env-base.f`, `ENV-QU` was claimed at offset 16893592 in an
engine whose `ENV-QU` lives at 5466416 — a code-spelled address is relocated by
the capture, a number in a raw cell is not. The offsets therefore have to be
taken from the live cells in the process that links, which is the maker. Where
the declaring package's cells are private — `dynamic-storage.f`, `memory.f` —
that package exports one claim applier (`OWNED-CELLS ( [ ptr u8 -- ] -- )`) that
hands the list its own cells and nothing else, because a private word of a baked
package is unreachable from outside it by any spelling.

So a stripped image reads its environment: `: MAIN ( -- ) s" HOME" GETENV type
cr ;` with no `require` at all builds, runs and prints the variable, explicitly
set or inherited through `PROC-ENV-INHERIT-MISSING`, and `MEM:WITH-BYTES`
allocates in the same image
(`tools/hb-build-stripped-test.f HBT-STRIPPED-ENGINE-CELLS`).
Every other engine cell is refused as loudly as before — `env-base.f`'s own
`TMP-PATH` cursors are the nearest miss: same file, same transient character, no
claim, so a stripped program calling `TMP-PATH` still gets *outside the restored
span*, naming `TPU` (`HBT-STRIPPED-UNOWNED-CELL`). Adding a cell to the list is
a deliberate act that has to state, cell by cell, why the entry may own it.

**A baked constant an application reads is carried by name.** Printing an
integer, parsing one or hashing reaches a table the engine baked below every
window — `lib/string.f`'s `STR-MAX-I64$` and `STR-MIN-I64$`, SHA-256's `KK` and
`HH0` — whose bytes no entry store can recompute and no fresh mapping can
supply. Such a cell is claimed **carried**, with its byte length: at link
`src/habu/aot-lib.f CARRY-CELLS` copies `[cell, cell+length)` into the
cell-aligned run `src/habu/aot-window-latch.f CARRY-RESERVE` reserves inside the
span, so the bytes travel in the image's own data blob, and `aot-closure.f
CARRIED-TARGET` rewrites every spelled address in that range to the copy at the
same interior offset — the map a re-interned literal already goes through, so
`KK i cells +` lands where `KK` does. No entry code runs for a carried cell and
nothing below the window is written.

**The carried run travels in every stripped image.** The copies are made for
every link, whether the program reaches them or not, and the blob ships every
non-zero byte of the window: a hello-world image carries 473 written data bytes
and no zero ones (measured on this engine, from the build's own report — `data
473 written + 0 zero` — and `HBT-SIZE-AOT` pins the same shape). The
run itself is `CARRY-BYTES` = 1024 bytes and the four claims use 624 of it
(`STR-MAX-I64$` and `STR-MIN-I64$` at `STR-I64-DIGITS` = 19 bytes each, rounded
up to a cell, `KK` at 64 cells and `HH0` at 8 cells); what no claim uses stays
zero and never travels. A fifth claim larger than the remaining 400 bytes fails
the build with *aot: carried engine cells exceed the window's carried run*, and
a claim reaching into the capture window fails with *aot: a carried claim
reaches into the capture window* — the window is restored on its own and a copy
of part of it would never be read. The scratch cells and buffers of
`src/core/sha256.f` are claimed **fresh** by the same list, each after reading
the word that writes it before it reads it, and so is `src/core/util.f`'s `PZB`
— with `FRESH-BYTES` and its `PATH-CAP` + 1 bytes, because `PATH0` hands `open`
the buffer itself. Until it was claimed, *every* path a stripped image opened
was refused at `caller=PATH0 target=PZB`, which is where Tender's three entry
points stopped; `HBT-STRIPPED-OPEN-PATH` is that reproducer, and it now opens
`/dev/null` and prints. The site every entry point stopped at next is
`lib/image-lifecycle.f`'s hook registry, reached by any library that registers
its cleanup on first use: its lock and its two hook counters are claimed
**fresh**, because a new process has registered nothing, and
`HBT-STRIPPED-LIFECYCLE-REGISTRY` reads the count out of a stripped image, and
that file's `HOOKS` buffer head and `PERSISTENT` table base are claimed **fresh**
beside them — the bases only, because both accessors compute their slot by
arithmetic and no code spells a cell behind either — so a stripped image
*registers* a hook as well as counting one. Before those two claims the store
was refused at the `HOOKS` control head (`caller=STORE+424 target=DICT+56`), and
with them it moved exactly one step: `APPEND` stores a quotation into a declared
cell, which lowers through `xt!`, and `xt!` stores the token and then calls the
engine's address-cell registrar to declare the cell, which left the image refused
with *aot: PC-relative target removed or outside closure site=xt!* (both
measured). **A stripped image drops that declaration.** Every reader of the
address-cell table — the snapshot writer, the loader's relocation pass and the
AOT capture — is machinery a stripped image does not carry; its only restore pass
is the linker-computed `EMIT-XT-CELLS` above, and the registrar would refuse the
zero table header of its fresh `DATA` in any case. So the registrar is a sealed
engine helper record, `(MARK)`; the closure walk does not follow a branch to it
(`aot-closure.f AOT-DECLARATION?`) and the relocation pass writes a `NOP` over
the call (`aot-lib.f RELOC-W32`), leaving the store — `BSTORE`'s store,
protection guard and all — untouched. The rule is keyed on the *target* being
the registrar, never on the member that calls it, so every other unmapped branch
still dies by name. `HBT-STRIPPED-LIFECYCLE-HOOK` is the reproducer: it registers
through both entries, calls a library that registers its cleanup on first use,
and prints from each hook at exit. **`(NUM)` is the opposite case**: the
engine's number reader, which `num-parse` reaches by the same kind of direct
branch, is a sealed helper record that the closure **carries**, because the
reader does the work the caller needs at run time and its body reaches nothing
outside itself — every branch targets a label inside it, the radix is an
immediate, the float finish is inline, and the only memory it touches is the
caller's bytes. A stripped image that parses a number therefore grows by the
reader's 480 bytes and answers the parsed value; without the record it was
refused at `site=num-parse target-word=<unknown>`, which is where Tender's
stripped server stopped, and `HBT-STRIPPED-NUM-PARSE` is that reproducer.
A baked table on no list still refuses:
`src/os/env-base.f`'s `TPB`, the `TMP-PATH` buffer, reached through
`TMP-PATH-COPY-SRC`, is the nearest miss (`HBT-STRIPPED-UNCARRIED-TABLE`) — a
table travels because the list names it, never because it is a table and never
because the buffer in the file beside it is claimed. One program that prints,
parses and hashes, with its stdout pinned to `42`, `123` and the FIPS-180 digest
of `abc`, is `HBT-STRIPPED-PRINT-PARSE-HASH`.

**A persistent cell holding a pointer into memory the build mapped is refused
by name.** The walker read a cell's value three ways — a live dictionary record,
a code address by live extent, a `DATA` address — so a pointer into a mapping
the build itself made was none of them and passed as an integer: a program that
took a buffer at load time (`MEM-ALLOC-64K drop BUF !` at the top level) shipped
an address only the linking process ever held, the same in every run of one
image, a different one in every build under ASLR, and mapped by nobody once the
image runs. Tender's stripped server faulted at one of eight such cells before
any clone, and three builds of one tree differed in exactly those eight values.
`src/habu/proc-maps.f` reads `/proc/self/maps`, the kernel's own list of the
process's areas — `mmap` is reached from `lib/memory.f`, `lib/vector.f`,
`lib/aio.f` and more, and a foreign allocator calls the primitive without
passing any of them — and `aot-closure.f CELL-MAPPED?` asks it, less the four
areas a process already holds before its program runs. Three are where the image
holds them too: the `MAP_FIXED` `DATA` mapping, the dictionary/code region
whole, and the executable's own segments, because `bin/hb` and the image it
links are both `EXEC`-type ELFs at the same fixed base and a cell holding a
string's last three bytes lands in that band whenever the third byte is a letter
(four cells of `HBT-STRIPPED-LIFECYCLE-HOOK`'s window do). The fourth is the brk
area the kernel names `[heap]`, excluded because nothing can point into it: no
Habu word allocates from the break, every allocation `lib/memory.f` makes being
an `mmap`. It is also where ordinary data lands — arm64 randomizes the break
over a gigabyte above the executable's end, so the band sits somewhere in
`[0x7a4000,0x407a4000)`, moves with every build, and swallows 32-bit-shaped
values: one build of the hb-build fixture (`tools/hb-build-test.f` and the two
stripped rows beside it) was refused at an undeclared cell holding
`0x34B12C35` and the next build of the same tree linked it. Two sites
ask: `aot-lib.f AOT-DATA-TEXTPTR-CHECK` for an undeclared window
cell, beside the undeclared code-pointer refusal, and `aot-closure.f XTD-ROW`
for a declared `DATA` cell — a plain `variable` or `PTR-VARIABLE` meets the
first, `PERSISTED-PTR-VARIABLE` the second (measured by disabling the span scan:
the persisted program is still refused and the `variable` one links). The
refusal reads *stripped AOT persistent data holds a pointer into memory the
build mapped word=BUF data-off=… value=…*, and its suggestion *the image
restores no mapping the build made; allocate at run time (in MAIN or an
IMAGE-LIFECYCLE hook) and store the pointer then, or use --repl*;
`HBT-STRIPPED-MAPPED-CELL` and `HBT-STRIPPED-MAPPED-DECLARED` are the two
reproducers. The same allocation inside `MAIN` links, runs and prints a byte
out of its run-time buffer (`HBT-STRIPPED-MAPPED-LATE`), and one source linked
twice, each build with a cache root of its own, is the same bytes
(`HBT-STRIPPED-SAME-TWICE`). The reader is Linux-only: a host without
`/proc/self/maps` dies by name at the first question instead of answering. A
mapping freed before the link is no longer listed, so a pointer into it is not
refused. The residual runs the other way as well: a cell whose integer value
spells an address inside any mapped area above those four bands — an `mmap`, the
stack, the vDSO — is refused as a pointer, because a reader of values cannot
tell a pointer-shaped integer from a pointer. `test/gate-aot-negative.f`'s
mapped-band fixture pins both answers on the live process: its `[heap]` start is
mapped and not refused, and a fresh `MEM-ALLOC-64K` address is refused. The map
is a snapshot taken at the first question, so an area mapped after it is
invisible; the application takes its buffers as it loads and the walk asks
afterwards.

**The span refusal names a site no record names.** Both `caller=` and `target=`
are dictionary records, and neither is always there: a member whose name the
engine build stripped arrives with `XREF-NULL` for its record (`aot-closure.f
ADD-SPAN-CLO`), and an address interior to a buffer is spelled by no record's
code at all. The refusal then names the nearest record *below* — the site as
`NAME+off` from the record whose code starts closest below it, the target as
`NAME+off` from the nearest data any record spells — and the `+off` is what says
this is a neighbour and not the owner: `caller=STORE+748 target=COUNT+8` is the
image-lifecycle lock as it read before the claim. Ownership keeps its exact rule
(`ADDRESS-OWNER`, `DATA-CELL-OWNER`, and so the cell refusals, which name the
word whose data the cell *is*): a walk that guessed would carry another word's
bytes. A value no record spells at all stays `<unknown>`. The two code
refusals — `aot: PC-relative target removed or outside closure` and `aot: ADR
target outside its member` — name their `target-word=` by that same rule:
`ADDRESS-OWNER`'s record when it owns the target, else the record below it as
`NAME+off`, else `<unknown>`; an ADR is the writer that meets an unaligned
target, and `ADDRESS-OWNER` answers no owner for one. A code address is only
named from below when recorded code also starts *above* it, because that is
what puts it in the gap a stripped word's code occupies: a target above every
record is data, and `NAME+13950258700` says less than `<unknown>`.

**A stripped image can call a foreign function.** A `FUNCTION:` declaration
resolves its symbol at the *first call*, so no address the builder resolved ever
travels; what the image has to carry is the declaration's data and the loader's
own entry points — the FFI table, which is in the program's DATA window because
`lib/ffi-abi.f` is a library the program requires; the two GOT slots
`src/os/linux/elf.f` relocates (`dlopen`, `dlsym`) in every image it writes; and
the text-base cell those slots are located from, since `src/os/linux/layout.f
DLSYM-SLOT` reads `rbase - CODE-OFF + the image's text size + $B8`. With that
cell left at the mapping's zero the slot address came out as -$FA0 and the image
took SIGSEGV where it should have called; the claim above is what carries it.
`tools/hb-build-test.f BUILD-AOT-FFI` builds a stripped image whose `MAIN` calls
`getpid` through the declarer and holds it to its output.

A stripped image restores the program's own DATA window byte for byte, so a
persistent cell arrives holding whatever the BUILD process put there. For a cell
that holds an execution token those bytes are the builder's own code address,
and the image has to replace them with its own. It does that for every cell that
was **declared** to hold a token:

- `defer NAME` declares its dispatch cell, and `is` declares the cell it stores
  into. A binding made at load time therefore survives the strip.
- `xt!` declares a cell a checked word computed at run time. The optimizing tier
  selects it for a proven quotation store, so an ordinary `!` of a quotation into
  a persistent cell declares that cell too (see the tier note below). In a
  stripped image only the store half runs: the declaration call is dropped at
  link, because such an image has no address-cell table to declare into.

The declaration is the only authority. Nothing reads a cell as a code address
because its value happens to land in a code range: an ordinary integer can hold
any value at all, and the same rule governs snapshots and the AOT capture
(`src/habu/layout.f`, package `SNAP-RELOC`). So the linker takes the declared
address cells that fall inside the program's DATA window, records one 8-byte row
per cell — the cell's DATA offset and its target's offset in this image's code —
after the image's data blob, and the startup applies them right after restoring
the window. The word a row names joins the image's closure, and an anonymous
quotation body is carried on its own, without the initializer that bound it.
A quotation's address is an `ADR` to a later function of the same emission, and
a member always holds both ends of it — a record covers the whole emission, and
an anonymous body runs to its record's end — so the link relocates that `ADR` by
copying it unchanged. An `ADR` whose target lies in another member is refused by
name, `aot: ADR target outside its member site=…`, because no emitter makes one.

A cell **declared to hold an address** — `PERSISTED-PTR-VARIABLE` and the other
`ptr-cell-mark` definers of `src/core/pointer-storage.f` — is mapped the same
way its value would be if the code spelled it out. `src/habu/aot-closure.f
XTD-ROW` reads every DATA-kind row inside the window and puts its value through
the one map (`MAPPED-DATA`): an address in the window answers itself, an address
inside a carried claim answers with the copy at the same interior offset, and an
engine address no claim names is refused. The mapped value is stored into the
cell before the window is read out, so the image ships it in the cell's own
captured bytes and no startup pass patches it. Without that map a cell holding
`STR-MAX-I64$` at build time shipped the engine's address and the image printed
nineteen NUL bytes out of its zero-filled mapping
(`tools/hb-build-stripped-cells-test.f HBT-STRIPPED-CACHED-CARRIED`). A bare
`PTR-VARIABLE` is scratch by declaration — it joins no relocation table — so a
build-time address left in one is not mapped and not refused; use the
persisted definer for a
pointer that must survive the strip.

Five things are still refused by name, each naming the word, the cell's DATA
offset and the value:

- an **undeclared** code or dictionary pointer in persistent data. `create T
  ' W ,` fills an untyped cell, so nothing declares it; bind the token with a
  `defer` or `xt!`, or build with `--repl`.
- a **dictionary-record** pointer: a stripped image carries no records.
- a cell **below the capture window** that no claim names — a preloaded
  module's data, which the image does not restore at all. The engine runtime
  cells `src/habu/aot-owned-cells.f` names are the only exception, and they are
  admitted by that declaration, not by their value.
- a declared cell whose value is not the code of any word the image can carry.
- a **declared DATA cell** holding an engine address no claim names — the map
  above has nothing to rewrite it to (`HBT-STRIPPED-CACHED-UNOWNED` pins it on a
  cell holding `TPB`).

## Capturing an existing dictionary

`src/habu/app-image.f` provides two checked operations:

- `APP-IMAGE:START! ( [ -- ] -- )` selects an optional startup action.
- `APP-IMAGE:SAVE ( ptr u8 n -- )` saves to the supplied path and exits the
  writing process.

Invoke `SAVE` from the outer stdin stream, after required files have returned.
For example, save this as `capture.f` and run `bin/hb < capture.f`:

```forth
require src/habu/app-image.f
require app.f
' MAIN APP-IMAGE:START!
s" app" APP-IMAGE:SAVE
```

Without `START!`, the saved dictionary retains ordinary Habu input and source
argument handling. A saved image can itself compile more checked definitions
and be captured again. Typed quotation stores into persistent DATA cells use
the shared relocation table automatically; application code uses ordinary `!`.
What makes that automatic is the tier: `src/habu/app-image.f` selects the
optimizing tier as its last act, so every definition the application makes is
lowered by the compiler that knows a store holds a quotation and declares the
cell. Do not select tier 0 after requiring it - the image would then keep the
builder's own code addresses in those cells and die when it ran them.
