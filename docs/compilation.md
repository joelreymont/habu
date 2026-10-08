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
checked. The engine's prefix does this today: of its 68 files
(`src/habu/habu2.f:1100-1210`), the first 19 load unchecked, the 19th,
`src/core/check-hook.f`, switches the checker on (:142), and every definition
after it is checked.

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
words mean is written down about nine times, among them
`src/compiler/native/hir-word.f:1418-1643`, `src/core/type-family.f:2598` and
`bootstrap/cg/forth.fs:4110`.

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
them ([portability.md](portability.md) §7, §13).
[Cross-compilation](architecture.md#cross-compilation) says which platforms a
build names. A stripped application image carries its code and data with no
compiler, dictionary or REPL (`docs/native-applications.md:122`). The test
suite builds `hb` once and runs every test against it. The two-generation build
and the build from zero ([bootstrap.md](bootstrap.md)) are occasional
integration tests.
**Now:** the snapshot build (`tools/native-build.f`) loads the sources into the
running engine and captures its memory. `tools/build-fixpoint.f` also compiles
glued source text to a byte fixpoint, and test files build engines of their
own.

**The linker places everything at fixed addresses.** It takes the product's
dictionary space with its reference cells, one unplaced emission per
definition and the product platform's primitive bodies. It keeps what the
roots reach (the boot entry and the public words) and writes the image with
both spaces at the addresses it chose. The boot maps each space at its address
and refuses to run when it cannot get it; nothing is relocated at boot.
**Now:** only the x86-64 linker places at fixed addresses
(`src/habu/link-x64.f` `LAYOUT`), and nothing in the product build calls it.
The ARM64 boot maps the code region wherever the kernel gives it near a hint
(`docs/x86-64.md:1030-1031`), then relocates data and patches call sites by
name (`src/habu/habu2.f` `EM-AOT-RELOC-DATA`, `EM-AOT-PATCH-NAMED-SITES`).
