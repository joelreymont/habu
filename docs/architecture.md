# Architecture

What Habu is made of, from the bottom up: what each part owns and the rules it
follows. [compilation.md](compilation.md) follows a definition from source to
native code and an image; [bootstrap.md](bootstrap.md) makes the first `hb`;
[xcomp.md](xcomp.md) holds the detail for platforms and
cross-compilation. Each rule is present tense; where the code does not follow
it yet, its **Now** line says what differs.

## Kernel

**The kernel is the words compiled before the type checker runs.** It is
untyped and implicitly trusted, and integration tests cover it; there is no
census and no certification. The checker is part of the kernel, and the kernel
ends where the checker is switched on. A kernel word that checked code calls
carries a trusted type declared at its definition. Every other kernel word has
no type, is private to the system package and is stripped from the delivered
`hb`.

**One primitive table.** Each primitive is one row in `src/habu/prims.f` that
names it and states its effect. Each platform supplies one body per row, native
code per instruction set, and the build refuses a
body without a row or a row without a body. Where a primitive can be written
without itself, a reference implementation in checked Habu runs beside every
body in tests (`test/prim-parity.f`).
**Now:** 233 primitives in 285 rows, 47 of them per-owner rows that
[privacy](#packages-and-privacy) removes. The seed in `bootstrap/cg/forth.fs`
carries a second copy of the ARM64 bodies (`FPRIM`) ([bootstrap.md](bootstrap.md)).

## Dictionary

**One dictionary.** The engine's dictionary record is the only table of words.
A word's type hangs off its record: the checker's per-word data is reached from
the record, laid down with it, discarded with it and linked into the image with
it. A word's package and visibility come from its record's wordlist.
**Now:** the checker keeps a second table keyed by package, visibility and name
(`src/core/checker.f` `SYM-FIND`, `HIDX`), with its own rollback and its own
copy of the used package names. The record's flags cell restates two type
facts, `DNAME-MIN-IN` and `DNAME-WIDE` (`src/habu/layout.f`).

**A word's type comes from its definition.** The effect declared where a word
is defined is its type; nothing restates it elsewhere.
**Now:** `src/core/checker.f` restates the types of words compiled before the
checker in `PRIM:`/`PPRIM:` rows.

**A header is five cells, then its name, then its body.** The definer lays the
word's type at `HERE`, then its header, so one rewind discards both:

| Cell | Holds |
|---|---|
| link | the next header in the same wordlist chain, newest first |
| wordlist | the word's wordlist, which gives its package and visibility |
| code | the word's code entry; null for a package |
| type | the word's type; null for an untyped kernel word, which checked code cannot call |
| flags | the name's length (one byte), immediate, and the kind |

The name follows as written, padded to a cell, and the body starts at the next
cell. The first four cells are reference cells. The kind is the definer's fact
about a mention: compile a call, a constant's value read from its body, a
created word's body address, or nothing for a cast. An emission's row names a
record and, for a quotation or a `does>` clause, which function of that
record's emission it means. A package's body holds its public and private
wordlists and whether it is sealed.
**Now:** records are 48-byte slots in the code region
(`src/habu/layout.f` `DREC`, `DNAME-INL`), with a 16-byte inline name or a pointer to a
longer one (1,811 of 20,626 names are longer), a numeric wordlist id and a code
length. Their flags restate type facts and carry marks (`DNAME-MIN-IN`,
`DNAME-WIDE`, `DNAME-INT`, `DNAME-OWNED`). A `does>` clause gets a companion
record of its own (`src/habu/xref.f` `XREF-DOES-COMPANION?`).

**Each wordlist hashes its names into 64 chains.** Lookup hashes the folded
token once and walks the matching chain of each wordlist in the search order.
Plain chains would cost seconds per engine load: a load reads 237,186 tokens,
11,264 of them numbers that miss every wordlist, the global wordlist holds 6,245
words while loading, and a chain step costs about 10 ns in checked Habu. With 64
chains the global one stays under 100 headers.
**Now:** one global table of 262,144 slots is keyed by name hash and wordlist
id (`src/habu/layout.f`, `HIDX-SLOTS`).

## Memory

**Two spaces, each reserved once.** The dictionary and the code are two spaces.
Each is one reservation from the address space, far larger than any program,
and its pages fill as its pointer advances. Nothing is sized from a measured
program.

### Dictionary space

A word's header and its data are laid down at `HERE` as it is defined, and
`ALLOT` grows the space. Headers and data interleave, as in Forth.
**Now:** records are a fixed array of 131,072 slots inside the 35 MiB code
region. The record band and region grow together to preserve 30,404,608 bytes
for code (`src/habu/layout.f`, `DICT-CAP` and `CODE-BAND:BYTES`). Data is a separate space, and the engine's own
state sits at 315 hand-assigned offsets in a header before it
(`src/habu/layout.f` `DATA-START`).

### Code space

Code has its own space and pointer because a page is writable or executable,
never both ([xcomp.md](xcomp.md#linking-and-publication)).
**Now:** code shares one 35 MiB region with the records, and the engine flips
the region between writable and executable around compilation
(`src/habu/habu1.f` `PROT-EMIT`). Records in that region are write-protected
and changed through `patch32` (`src/habu/xref.f`) and page flips
(`src/habu/habu1.f` `LPROTREC`).

### Stacks

Each stack is its own mapping with an inaccessible page below and above it, so
the MMU enforces its bounds and compiled code carries no bounds check
(`src/habu/stack-abi.f` header).

### Mapped buffers

`src/lib/memory.f` hands out OS-backed mappings that the caller owns and releases.
The library keeps nothing about them except the process-wide scope stack of
`WITH-BYTES`.

## Packages and privacy

**Privacy is the only restriction.** A word not everyone may call is private to
its package. System words are private words of the system package, and the
build strips private words from the delivered `hb`. There are no owner rows,
caller checks or marks on records.
**Now:** engine-internal primitives sit in the global dictionary and are
refused by marks (`DNAME-INT`, the trusted-only flag, `DNAME-OWNED`);
`PPRIM:`/`EPPRIM:` owner rows type words per package.

**A sealed package cannot be reopened.** `package NAME` on a sealed package is
refused.
**Now:** the refusal is written twice because `package` is: in the engine's
machine code (`src/habu/habu2.f` `C-PACKAGE`, `C-PACKAGE-SEAL-GUARD`) and in a
second interpret loop written in Habu (`src/habu/interpret.f`
`OUTER:INTERPRET`, with `src/habu/packages.f` `PKG-PACKAGE`,
`PKG-SEAL-GUARD`), which only tests load.

**Stripping drops headers; reachability decides code.** The linker writes a
sealed package's private wordlist empty, so no later source can name its
words. A word's code and body stay in the image only while a kept word reaches
them through a row or a reference cell. An application image keeps no header
at all.
**Now:** the native builders emit every specified primitive. Image capture
then follows references from the program's roots to retain reachable code.

## Interpreter

**One interpreter.** The interpret loop, definers, packages and source loading
are written once in typed Habu and run on every platform.
**Now:** ARM64 runs a machine-code interpreter (`src/habu/habu2.f`). The Habu
loop (`src/habu/interpret.f`) becomes a seeded engine's `evaluate` through
`OUTER:INSTALL-EVALUATE`, which is how x86-64, whose kernel carries no
interpreter, evaluates source; it reads a definition only at tier 1
(`src/habu/definers.f`). Its `create` and `variable` end through the
`def-create` writer row, which ends the record as the engine's own `create`
does (`EMIT-CREATE`); x86-64 refuses that row and compiles the created word's
body through the native compiler instead (`DEF-ADDR-TAIL`). Its `cast:` ends
through `def-cast` as the engine's `cast:` does (`C-IDENTITY`), and its
`immediate` is the `imm-mark` row, which sets the flag as the engine's keyword
does (`C-IMMEDIATE`).

**The loop is Forth's.** `INTERPRET` reads a token and looks it up. Outside a
definition the word runs; inside one, an immediate word runs and any other word
goes to the checker as a call. A token that names no word is a number, which
is pushed or goes to the checker as a literal, or else an error. The state is
the pending definition, none while interpreting. Every construct (`if`, `{:`,
`s"`, `;` and the rest) is an immediate word that calls the checker's step for
it ([one reader](compilation.md#defining-a-word)). `evaluate`,
`evaluate-closed`, `include` and `require` are `INTERPRET` over another source.
**Now:** the machine-code reader dispatches through keyword tables, 25 rows
for interpreting and about 70 for compiling (`src/habu/habu2.f`
`EM-INTERPRET-*-KEYWORDS`, `EM-COMPILE-*-KEYWORDS`), and
`src/core/include.f` reaches the loop through its private `INCLUDE-INTERPRET`
defer. `SOURCE-ROOT:USE-INTERPRETER` installs a renderer for loader fragments;
the loader closes the stack around the whole source unit. Public range
evaluators close their own program stack and reject invalid byte bounds.

**The kernel holds what the typed interpreter needs to load.** That is:
- lookup over the hashed chains;
- the header writer and the publisher;
- `parse-name`, `parse` and `refill` over the current source;
- `execute`, `catch` and `throw`;
- `here`, `allot` and `,`;
- file reading;
- the checker and the codegen's hook cell.

When `hb` reads the kernel, its definitions go through the same definer and
codegen with checking off: the checker records each construct with every value
one cell wide, and the definer writes a null type.

**A failed definition leaves the dictionary as it was.** A throw before `;` or
a refusal at `;` rewinds `HERE` to the pending definition's type, drops its
events and clears the pending state. Its emission was never placed, so code
space is untouched. `INTERPRET` restores the source, package scope and used
packages it saved on entry, then rethrows.

**At top level, a word's type guards the stack.** The interpreter supplies the
exact selected dictionary record to the checker-owned typed row. The checker
applies that word's input and output effects and refuses a mismatch before the
call, including a pointer or execution token stored in a numeric cell. Native
`DNAME-MIN-IN`, `DNAME-WIDE`, `DNAME-INT` and policy checks remain in force.
`src/core/top-row.f` connects the token hook to this row; nested evaluators own
separate row state. See [design.md](design.md#typed-top-level).

**The REPL is `INTERPRET` per line.** When standard input is a terminal, it
reads a line, interprets it under `catch` and answers `ok` or the error. A
definition still open at the end of a line continues on the next.
**Now:** the REPL loop is machine code (`src/habu/habu2.f` `LREAD`, `LRREC`),
and its line editor (`src/habu/repl.f`) is untyped and installs itself through
an engine hook cell.

## Platforms and code generation

**One codegen per platform.** Each definition compiles through the current
platform's codegen. Hand-written code per platform is only the kernel's
primitives.
**Now:** ARM64 has a second codegen beside its platform codegen, the tier-0 JIT
(`src/habu/jit.f`). The platform codegen compiles a trivial definition far
slower than the JIT (`tools/compile-floor.f`, its `trivial-t1` and `trivial-t0`
fields), so `--load` and the REPL run the JIT; an executable never contains its
code. The process that captures an image opens
`EXECUTABLE-BUILD:WITH` (`src/lib/executable-build.f`) before it loads the code
it captures (`tools/aot-builder.f`, `tools/native-build.f`); the scope selects
tier 1 through every include, `evaluate` and immediate word, and a tier-0
compile or `0 set-tier` inside that scope refuses with `hb: executable build
requires native tier 1` (`src/habu/habu1.f` `EXECUTABLE-JIT-GUARD`).

**The codegen is one pass over the checked events.** It does four things:
- it keeps a definition's stack values in registers, and leaves its constants
  unemitted until a label or a call needs them in place;
- it folds a comparison into the branch that reads it;
- it inlines each primitive from that primitive's body in the primitive table,
  and the primitive's callable form wraps the same body;
- it maps Wasm's structured control straight from the nested constructs.

Locals live in the return-stack frame, and `>r` moves a value to the return
stack. There is no IR and no optimizing pass. A pass is added only when a
benchmark shows that the code it produces pays for it.
**Now:** tier 1 is built on the IR in `src/compiler/ir/`: the checker fills a
token tape as it certifies the definition (`NTAPE`), elaboration builds an HIR
module from the tape (`NELAB`, `HIR`), the module is frozen, verified
(`IR-VERIFY`) and its loops folded (`NBACK:FREEZE`), and the target's pass row
selects, allocates registers, verifies the allocation, spills and emits
(`src/arch/arm64/passes.f`: `A64SEL`, `A64RA`, `A64RAV`, `A64SPILL`,
`A64EMIT`). Selection and emission proper are a small share of its compile
time (`A64SEL:SELECT` and `A64EMIT:EMIT` in the exclusive samples of the
tier-1 profile below). Its code runs at least as fast as tier 0's (the median
ratio per benchmark of `tools/tier-bench.f` at tier 0 and tier 1), through
what this pass keeps: inlined primitives, stack values in registers, constants as immediates and
comparisons folded into branches. Over the corpus below tier 1 emits fewer
`bl`, fewer stack-pointer loads and stores and fewer bytes
(`tools/tier-census-join.f`). Checking is most of a tier-0 compile
(`tools/tier0-profile.f`, the `ns-per-def` of its `trivial` mode against its
`trivial-raw` twin).

These runs measure each claim now, in POSIX sh (zsh needs `${=C}`). Timings
are wall clock, so pin each run to one performance core where the host allows
(`taskset -c 8` on Linux):

```sh
C="src/lib/byte-edit.f src/lib/array.f src/lib/fmt.f src/lib/float.f \
src/lib/json-read.f src/lib/json-write.f src/lib/unicode.f src/lib/argv.f \
src/lib/fs.f src/lib/task.f src/lib/build.f tools/lint/text.f \
tools/public-signatures-core.f"
bin/hb --load tools/compile-floor.f
bin/hb --load tools/tier-bench.f -- 0 src/core/checker.f
bin/hb --load tools/tier-bench.f -- 1 src/core/checker.f
bin/hb --load tools/tier-census.f -- 0 t0.txt $C
bin/hb --load tools/tier-census.f -- 1 t1.txt $C
bin/hb --load tools/tier-census-join.f -- t0.txt t1.txt
bin/hb --load tools/tier0-profile.f -- trivial 50 4000
bin/hb --load tools/tier0-profile.f -- trivial-raw 50 4000
```

The tier-1 profile loads 4,000 definitions `: PFTn ( n -- n ) 1 + ;` from a
generated file under the sampling profiler
([debugging.md](debugging.md#sampling-profiler)):

```forth
require src/lib/tier.f
1 TIER:SELECT  250 prof-rate  0 prof-on
s" trivial.f" included
prof-off prof-report
```

### Cross-compilation

**A build names the platform it runs on and the platform it produces.** Any
`hb` builds `hb` for every required host
([xcomp.md](xcomp.md#build-host-product-and-target)). The build runs a word's
host body when it needs the word's value and compiles a target body for the
product ([xcomp.md](xcomp.md#compile-time-execution-and-target-data)).
Building for a platform never runs on it: `hb` on a macOS ARM64 machine
cross-compiles the x86-64 `hb`, and only running the product needs a machine of
its kind ([xcomp.md](xcomp.md#build-host-product-and-target)).
**Now:** the x86-64 native cross-build and the Wasm module build compile
definitions for a second target through `src/compiler/native/shadow.f` and
their target linkers. The ordinary ARM64 product build captures the running
engine.

**Every codegen emits unplaced code.** A compiled definition is its bytes plus
a row for each call, outgoing branch, code address and data address in them,
and each row names a record ([header](#dictionary)), not an address. The publisher places an emission
in the running engine by resolving its rows against that engine; the linker
places it in an image by resolving them against the image's layout. A build
compiles a definition once when the product runs on the host's platform, and
twice otherwise: once to run on the host and once for the product.
**Now:** the ARM64 codegen emits only placed code
(`src/arch/arm64/passes.f` `UNPLACED-UNSUPPORTED`), and the ARM64 capture
finds call sites again by decoding branches and address chains in the placed
bytes (`src/habu/aot-capture.f` `ACAP-TGT`, `ACAP-CHAIN@`). x86-64 and Wasm
emit unplaced code only in a second compile beside the placed one, while a
shadow is open (`src/compiler/native/compiler.f` header,
`src/compiler/native/shadow.f`).

**Build-time data is product data.** While a build loads the product's
sources, the dictionary space is the product's: a reservation the build owns,
where headers and data lie as they will in the image. `here`, `allot`, `,`,
`create` and the definers act on it; the build program's own state stays in
the build's dictionary space. The platform codegen compiles a created word like
any definition: its code pushes its data address and, under `does>`, calls the
clause. `defer` is a created cell that holds a function. Engine state the
product boots from, such as its checker hook, is product data that its sources
declare and store into; what build-time code does to the build's own state
never reaches the image.
**Now:** the build runs definers on the running engine's own data and then
copies that span (`src/habu/aot-capture.f` `ACAP-BAKE-DATA`), and carries a
fixed list of engine cells into the image (`tools/native-build-core.f`
`CHECK-FIXED-ROWS`) that `tools/native-layout.f` translates per host. Created
words get hand-written routines (`does-patch` in `src/habu/prims.f`;
`LDOESPATCH` in `src/habu/habu1.f`).

**A cell's declared type says whether it holds a reference.** Raw storage
holds no address ([forth.md](forth.md), "Raw storage never holds an address"),
so product data that holds a pointer or a quotation is laid by a declared form,
and that form's record says which of its cells hold references. The linker
writes each such cell as a reference to the object or function its value names,
and copies every other cell as bytes. Nothing scans integers for pointers, and
no registry lists pointer cells. A reference cell whose value is neither null
nor inside the product's spaces is refused, naming the cell.
**Now:** the build keeps a registry of pointer cells, filled by `defer`, `is`,
`xt!` and `ptr-cell-mark` (`src/habu/address-cells.f`). `xt!` admits an
execution token into a raw cell (its row in `src/habu/prims.f`). A record that
mixes pointer and scalar fields has no declared form, so one of its halves goes
through a cast that the linker cannot see (the open repros `V8` and `V9` in
`test/typed-storage-structural-test.f`).

## Builds

**A file is the unit of compilation.** Packages stay namespaces that any file
can open or reopen. The loader reads source from disk in its normal order, and
a file is compiled again when its inputs, the facts it consumed, the compiler
or the target change.

**A file's object is the result of compiling it.** It holds the file's
definitions, code, initialized data, literals, its persistent changes to
earlier data, and the checker and loader state that later source and the final
binary need; compiler scratch is not part of it. Addresses are relocatable
references, resolved by declaration and offset at import and never keyed, so
an edit that changes an earlier file's size leaves later objects valid.
Symbols resolve by package and name, not by the numbers the compiler assigned
them. State a subsystem owns (compiler identities, checker pools, the literal
pool, task slots, the require registry, the search order) is exported and
restored by that owner; an object never copies a span of another owner's data.

**Importing an object installs its result and runs nothing.** Compile-time code
runs when the file is compiled; an import repeats neither that code nor its
external effects. A value captured from the environment stays part of the
result until a tracked input changes, so a program that needs a fresh value on
every run reads it at run time. Imported at its original load position, an
object makes its definitions and data available to later source as compiling
it would: package visibility and `require`/`include` semantics hold, and
reopenings are not grouped ahead of intervening source. The proof is a fresh
process that imports objects, compiles downstream source and behaves as a clean
build does.

**A file's key names the file, not what loaded before it.** The key is the
engine, the target profile, the build mode, the canonical path and root, the
load kind and the occurrence: how many earlier loads of the same path, root and
kind the build made. Only `include` repeats a load, and the count is build-wide
because the parent is not in the key, so two parents' first includes of one
file stay distinct. Folding preceding files into the key would make an edit to
the root file recompile every library it loads.

**An entry is valid while what it consumed still holds.** In order: the file's
size and modification time match, or else its bytes hash the same; the
environment and path-resolution inputs it read answer the same; and every fact
it consumed from earlier files still holds. A consumed fact is a record of an
earlier file that the compilation looked up (its contract, immediacy, fixed
value or generated family, the resolution of a spelling in the search order)
or the search order at entry, recorded once per file. A comment or body-only
edit leaves later files valid; renaming a word or changing a contract, a
constant or `immediate` invalidates exactly the files that consumed it.

**A file that reads earlier data around its owners stores no object.**
Compile-time code reads earlier files' state through dictionary records and
owner-exported state. A file whose compile-time code reads earlier data through
a raw address, a trusted body outside an owner interface, or an untracked
foreign or system call is compiled on every build, and the build names the
word.

**An edit during a build is read in due course.** A file already compiled
keeps the version it read for that build and is rebuilt on the next; a file not
yet compiled is read when the loader reaches it. A build neither snapshots the
source tree nor stops because an editor changed a file.
**Now:** the loader reads and runs every file on every build. Reuse is per
definition: after a definition's source actions run, the pending native
dispatch takes its cached native unit object (`tools/native-definition-cache.f`,
`tools/native-unit-object.f`, format in `tools/native-unit-file.f`), and an
object hit in `tools/hb-build.f` links the cached maker output.

## Images

**A stripped image carries only its entry's closure.** Without `--repl`,
`bin/hb --load tools/hb-build.f -- app.f -o app` writes an image with no
compiler, dictionary or REPL: the code `MAIN` reaches and the data window the
application loaded into. It compiles nothing at run time, so it runs under
`MemoryDenyWriteExecute`.

**The window holds only what the application requires.** The builder opens the
capture window before the application loads, with only the application's own
library copies visible to `require`, so the application's require closure lies
inside the restored span. A library loaded before the window would be the copy
the application's `require` resolves to; its cells would lie below the span and
read as zero in the image, so the linker refuses the image with *address refers
to data outside the restored span*. A program that uses a library word without
requiring its module fails to compile, naming the word.
**Now:** `tools/aot-builder.f` captures the builder's own support (the window
latch, the file producer and image saving) in a keyed builder image before the
window opens. The linker (`tools/aot-build-core.f`, `src/habu/app-image.f` and
the library modules it requires) loads above the span and uses the
application's copy of a module both load.

**The literal pool is the application's while it loads.** The window's first
byte is the body arena of the pool's first segment, whose owner and row tables
precede the span; a persisted pointer to those tables is refused at link,
naming the cell. The pool grows by segments of 512 KB and 8,192 bodies
(`src/compiler/native/string.f`), never frees one, and shares equal bodies
within a segment; a body larger than a segment is refused, `E-NSTR-CAP`.
Latching the span closes the pool (`NSTR:WINDOW-CLOSE`) with room for every
literal the link copies in from another pool, and a link that needs more is
refused, `E-NSTR-CAP`. Latching also hands the builder back its own pool, so
nothing the builder compiles afterwards reaches the image.

**An engine cell below the window travels only because a list names it.**
`src/habu/aot-owned-cells.f` names each engine cell a stripped image may reach
and declares how the image gets it. The entry stores the image's DATA base, the
startup's `argc`, `argv` and `envp`, the entry token and the text base. A
*fresh* cell or buffer (`FRESH-BYTES`) is correct as the zero of the image's
new DATA mapping. A *carried* table, which no entry store can recompute (the
integer-printing strings, the SHA-256 constants, the baked FFI and TASK
tables), is copied into a run reserved inside the span (`CARRY-RESERVE`), and
every address spelled into it is rewritten to the copy at the same offset
(`CARRIED-TARGET`). A claim covers its declared extent and never its
neighbours (`CLAIMED-AT?`). The entry (`EMIT-OWNED-CELLS`) and the closure
walker (`CLAIMED-CELL?`) read the one list, so they cannot disagree. The list
holds names, not offsets, because an offset computed while the engine's prefix
loads does not survive the capture that makes the engine; the builder takes
offsets from the live cells, and a package whose cells are private exports one
claim applier, `OWNED-CELLS`. Every other engine cell is refused:
`ENV-DATA-PTR` in `src/os/env-base.f` is claimed, and that file's `TMP-PATH`
cursors still refuse (`tools/hb-build-stripped-test.f`
`HBT-STRIPPED-UNOWNED-CELL`).

**The startup reaches its far labels through the code base.** The fixed entry
at text offset zero names the entry word, the crash handler, the signal stub
and the data blob, all past the copied code and out of `adr`'s ±1 MiB, so each
is a movz/movk offset added to `LTEXT` (`TEXT-ADR,`).
`tools/aot-startup-reach-lint.f` refuses an emitter that `ADR,`s a label its
own definition does not bind.

**A token cell is relocated because it was declared.** The image restores its
DATA window byte for byte, so a cell holding an execution token arrives with
the builder's address. `defer`, `is` and `xt!` declare such cells; the linker
writes one 8-byte row per declared cell in the window (its DATA offset and its
target's code offset) after the data blob, and the startup applies them after
restoring the window (`EMIT-XT-CELLS`). The word a row names joins the closure.
Nothing reads a cell as a code address because of its value, and snapshots
follow the same rule (`src/habu/layout.f` `SNAP-RELOC`). A stripped image has
no table to declare into, so the closure walk does not follow a branch to the
registrar `(MARK)` (`AOT-DECLARATION?`) and the link writes a `NOP` over the
call (`RELOC-W32`). A quotation's `ADR` stays inside its member and is copied
unchanged; an `ADR` into another member is refused.
**Now:** the declarations are a registry of cells
(`src/habu/address-cells.f`), not a declared form (see "A cell's declared type
says whether it holds a reference" above).

**An address cell goes through the one map.** A cell declared to hold an
address (`PERSISTED-PTR-VARIABLE` and the other `ptr-cell-mark` definers of
`src/core/pointer-storage.f`) has its value mapped before the window is read
out (`XTD-ROW`, `MAPPED-DATA`): a window address answers itself, an address in
a carried claim answers the copy, and an engine address no claim names is
refused. A `PTR-VARIABLE` is scratch, neither mapped nor refused.

**A pointer into memory the build mapped is refused by name.** The image
restores no mapping the build made, so a persistent cell whose value lies in an
area the process mapped (`/proc/self/maps` on Linux, `mach_vm_region_recurse`
on macOS, `src/habu/proc-maps.f`) is refused with the advice to allocate at run
time or build with `--repl`. The areas a process holds before its program runs
are excluded: DATA, the dictionary and code region and, on Linux, the
executable's segments and the `[heap]` break, which no Habu allocation uses
(`src/habu/aot-data-cell.f` `MAPPED?`). The check reads values, so a
pointer-shaped integer in a mapped area is refused, and a pointer into an area
freed before the link or mapped after the first question is not.

**A refusal names the word, the cell's DATA offset and the value.** Refused: an
undeclared code or dictionary pointer in persistent data (bind it with `defer`
or `xt!`, or build with `--repl`); a dictionary-record pointer; a cell below the
window that no claim names; a declared cell whose value is not the code of a
word the image can carry; and a declared DATA cell holding an engine address no
claim names. A site no record names, such as a stripped member or an interior
address, is named `NAME+off` from the nearest record below it; ownership itself
never guesses (`ADDRESS-OWNER`, `DATA-CELL-OWNER`), and a value no record
spells is `<unknown>`.

**A stripped image calls foreign functions and runs tasks.** A `FUNCTION:`
resolves its symbol at its first call, so no address the builder resolved
travels. The image carries the FFI declaration table, the `dlopen` and `dlsym`
slots `src/os/linux/elf.f` relocates, and the text-base cell those slots are
found from (`tools/hb-build-aot-test.f` `BUILD-AOT-SPAN`). A baked TASK carries
its symbol names, its user-slot cursor as latched when the window closes, and
its chain of declared TCBs; its foreign address cache, park and semaphore
storage start fresh, and a pooled semaphore still held after capture cleanup is
refused (`test/stripped-image.f`).

## Tools

**Tools read source through the interpreter.** A tool that reports on source
runs a child `hb` that loads the subject under hooks and writes one JSON line
per fact. A diagnostic carries the source position the interpreter is at when
the checker records the event. There are five hooks:
- the publisher reports each definition with its file and span;
- the checker reports each use of a word;
- the loader reports each file it reads or finds already loaded;
- the loader takes an editor's open text for a file in place of the disk;
- the checker lists the words visible at a cursor.

A definition's location is the tool's fact: the tool keeps it, and the record
does not.
**Now:** tools read source with readers of their own.
`src/habu/verify-source.f` (3,905 lines) rebuilds each definition and checks it
before it is compiled. `src/lib/source-lex.f` (838 lines) tokenizes for the check
tool and nine lints. `tools/check-core.f` marks origins into a copy of the file
so that the engine's diagnostics get positions.

**Loading checks and runs; a check runs nothing that acts.** Loading a file is
the checker's regular use: the checker checks each definition at `;`, and the
interpreter runs everything else, effects included. A check, the editor's or
the check tool's, is that same load in a child `hb` with one setting: a word
that acts outside the process does not run. Everything else runs, because
running words decides which names exist: `ENUM` and the sum-type definers
generate definitions by `evaluate` (`src/core/sumtype.f`), and `STRUCTURE`
reads ahead (`src/core/structure-decl.f` `CLAUSE`). The child exits, so what ran
leaves nothing behind, and the check's verdict is the load's.
- each primitive row says whether it acts outside the process: writing or
  changing files, processes, signals, the network, terminal input or a foreign
  call;
- a word's type says whether it acts: the checker sets that at `;` when the
  definition calls a word that acts;
- in check mode the interpreter keeps the top-level stack's types; a top-level
  word runs only when it does not act and every input it takes is known, and
  otherwise the checker applies its type and its outputs are unknown;
- in check mode an acting primitive refuses, which catches an action reached
  through `execute` or an immediate word, and the interpreter treats the word
  as not run.

No definer acts: `require`, `create`, `STRUCTURE`, `SUMTYPE`, `ENUM`,
`FUNCTION:` and the buffer and variable definers reach no file, process,
signal, terminal or foreign-call primitive, only the stderr report and `die`
of a refusal. The words that act are entry words such as `RUN` and `MAIN`.

**The editor checks against a loaded closure.** Most of a load is the
file's `require` closure, which does not change while the file is edited. The
editor keeps a child per load context with the closure loaded. Each check
forks that child, loads only the edited text, reports and exits, so nothing
the check ran survives it. A new edit ends a check still running. A changed
`require` line or a changed required file rebuilds the child. Running the
definers is cheap: a cold load of `src/lib/process-command.f` with its closure takes
0.10 s, and of `tools/lsp-core.f` 0.66 s.
**Now:** the editor and the check tool's pre-pass read a file without running
it. `src/habu/verify-source.f` models each definer by name (`ENUM-END?`,
`STRUCTURE-END?` and about 45 more), and a definer that writes its word as
text needs a `generates:` row, which is a claim the pre-pass cannot prove. The
two readers disagree: after `CMD:COMMAND C`, `: U ( -- n ) C#VEC ;` checks 0
under `--verify-only` and 70 when the file loads ([forth.md](forth.md), the
`generates:` rule). The pre-pass is also slower than the load it avoids:
`--verify-only` takes 1.0 s on `src/lib/process-command.f` and 1.71 s on
`tools/lsp-core.f`, about 0.6 s of it loading the verifier.

**A check reports every independent error.** When the check sets the
multi-error flag, a refused definition is rewound as always and the
interpreter goes on at the next token. A mention of a refused word is
undefined. The tool counts a definition whose first defect is such a mention
as refused, without reporting it. An error outside a definition ends the file.
**Now:** in multi-error mode the hook publishes a refused word under its
declared signature so that later definitions resolve it
(`src/core/check-hook.f`), which leaves a word behind.

**A lint that states a language rule is a refusal.** The definer refuses a
reserved name, a broad `TRUSTED:` and a global that shadows a primitive. Facts
about a whole program, such as the error codes it uses or its public
signatures, come from walking the dictionary after the program loads.
**Now:** these are separate tools over the token stream
(`tools/reserved-name-lint.f`, `tools/checked-boundary-lint.f`,
`tools/lint/shadow-lint.f`, `tools/error-code-lint.f`,
`tools/public-signatures.f`).
