# Compilation

How Habu turns source into native code and an image.
[architecture.md](architecture.md) describes the parts this process runs on;
[bootstrap.md](bootstrap.md) makes the first `hb`. Each rule is present tense;
where the code does not follow it yet, its **Now** line says what differs.

## Reading source

The [interpreter](architecture.md#interpreter) reads source one token at a
time and either runs the word a token names or compiles it into the current
definition. The [kernel](architecture.md#kernel) loads first, untyped; the
checker is part of it, and from the end of the kernel every definition is
checked. A build checks the engine's prefix (`src/habu/boot-files.f`) from
its first file: the builder's own checker checks every row, so a definition
with a wrong effect in a prefix file is refused (`src/core/check-hook.f`
INSTALL). Only the cold prefix of an engine that was not seeded
(`src/habu/habu2.f` EMIT-HOST-LOAD-PREFIX) loads the files before
`src/core/check-hook.f` unchecked, until that file switches the checker on.

## Defining a word

When a definition starts, the definer lays the word's type and header down at
`HERE` in the [dictionary space](architecture.md#dictionary-space). The word's
type is the effect declared at its definition, reached from the header. The
header joins its wordlist's chain only at `;`.

**One reader.** The interpreter reads each token of a definition once. Every
construct (`if`, `then`, `begin`, `{:`, `[:`, `MATCH`, `;` and the rest) is one
immediate word in the dictionary. As it runs, it tells the checker what it is,
so the checker checks the definition as it is read and records it as checked
events. At `;` the current platform's codegen compiles the definition from
those events into an emission for the [code space](architecture.md#code-space).
Nothing else knows what a keyword means: there are no keyword lists, no second
reading and no pass that rebuilds the definition's structure.
**Now:** each definition is read three times. The engine's reader compiles it
through the tier-0 JIT as it reads. At `;` a hook runs the checker over the
source text again (`src/core/check-hook.f:109`), filling a token tape
(`src/compiler/native/tape.f`). Tier 1 rebuilds the structure from that tape in
an elaborator of its own (`src/compiler/native/elaborate.f`). What the control
words mean is written down about eight times, among them
`src/compiler/native/hir-word.f:1418-1643` and `src/core/type-family.f:2598`.

**The checker records what the codegen reads, and nothing more.** Each
construct is one event:
- a mention of a word, with its record and the cell widths of what it takes
  and leaves;
- a literal: an integer, a float's bits, or the address and length of bytes in
  the definition's body;
- a control marker;
- a local's declaration or use;
- the start or end of a quotation or a `does>` clause, each a function of its
  own in the emission;
- a `MATCH`, one of its arms, or a sum-type construction, with its tags and
  payload widths.

The events go in a buffer the checker reserves once and reuses for each
definition. Bytes the definition lays down while it is read, such as its
string literals, go at `HERE`, into its body.
**Now:** the tape keeps the token stream with names, modes, spans and digests
(`src/compiler/native/tape.f:24-33`), and tier 1 recovers the rest by reading
the definition again. Beside the tape, the checker files each token's event, a
kind and two arguments, under the token's ordinal in a table every pass of the
scan refills (`src/core/checker.f` `EV-COMMIT`). An observer reads it with
`CHECKER-TAPE:EVENT` while it is told the verdict, so what it reads is the scan
that publishes.

A definition becomes visible only after its code is in place. The publisher
places the emission in the code space, resolving each of its rows against the
running engine ([unplaced code](architecture.md#cross-compilation)), and only
then publishes the header (`src/compiler/native/publish.f` `PUBLISH-PENDING`).
A refused definition leaves no word behind
(`src/compiler/native/compiler.f:15-16`).

## Building an image

**One build program.** Every engine is built by one program written in Habu.
It loads the sources, runs a word's host body when the build needs its value,
compiles each definition for the product's platform through that platform's
codegen, keeps build-time data as product data, and links the image from
them ([Build host, product and target](xcomp.md#build-host-product-and-target)).
[Cross-compilation](architecture.md#cross-compilation) says which platforms a
build names. A stripped application image carries its code and data with no
compiler, dictionary or REPL ([Images](architecture.md#images)). The test
suite builds `hb` once and runs every test against it. The convergence check
([bootstrap.md](bootstrap.md#convergence-check)) is an occasional integration
test.
**Now:** `tools/hb-build.f` links this tree's hb in its own process
(`tools/hb-build-core.f` `ENGINE`) and writes it as data:

```sh
bin/hb --load tools/hb-build.f -- --engine -o bin/hb-next
```

The same command builds hb with a program's files, its `MAIN` run before the
REPL, with `--repl source.f -o out`, or a stripped program with `source.f -o
out`. Every build writes the image and its `.map` sidecar.

**Incremental-build implementation guidance.** Read files from disk and use
files as compilation units, following the C compiler / make model. Packages
remain namespaces. Finish
the current build if a file changes during it: a file already compiled is
picked up on the next build, and a file not yet compiled is read in due course.
Do not require an immutable source snapshot or reject the running build solely
because an editor changed a file.

**The linker places DATA at its fixed address and code where the OS loads
it.** It takes the product's dictionary space with its reference cells, one
unplaced emission per definition and the product platform's primitive bodies.
It keeps what the roots reach (the boot entry and the public words) and writes
the image with DATA at DATA-VA and the code at a base the OS may move. Calls
inside the code are relative, so they hold at any base. The linker lists every
stored address that points into the image, and the boot adds the load offset
to each, as dyld and ld.so do for a position-independent C program. The boot
maps DATA at its address and refuses to run when it cannot get it.
**Now:** every ARM64 image hb-build makes follows this rule: it builds hb or
the program in its own process (`tools/hb-build-core.f`) and `LINKER:WRITE`
writes it (`src/habu/link-data.f` `LINK-ENGINE`, `LINK-STRIPPED`). The
x86-64 linker places code at a fixed address (`src/habu/link-x64.f` `LAYOUT`),
and nothing in the product build calls it.
