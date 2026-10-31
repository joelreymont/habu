# Architecture

The structure Habu is built to, from the bottom up: what each part owns and the
rules that span files. [compilation.md](compilation.md) follows a definition
from source to native code and an image; [bootstrap.md](bootstrap.md) makes the
first `hb`; [xcomp.md](xcomp.md) holds platforms and cross-compilation. Each
rule states the target. Where the code does not follow one yet, the code is the
current state and its file headers describe it.

## Kernel

**The kernel is the words compiled before the type checker runs.** It is
untyped and implicitly trusted, and integration tests cover it; there is no
census and no certification. The checker is part of the kernel, and the kernel
ends where the checker is switched on. A kernel word that checked code calls
carries a trusted type declared at its definition. Every other kernel word has
no type, is private to the system package and is stripped from the delivered
`hb`.

**One primitive table.** Each primitive is one row that names it and states its
effect. Each platform supplies one body per row, native code per instruction
set, and the build refuses a body without a row or a row without a body. Where
a primitive can be written without itself, a reference implementation in
checked Habu runs beside every body in tests.

## Dictionary

**One dictionary.** The engine's dictionary record is the only table of words.
A word's type hangs off its record: the checker's per-word data is reached from
the record, laid down with it, discarded with it and linked into the image with
it. A word's package and visibility come from its record's wordlist.

**A word's type comes from its definition.** The effect declared where a word
is defined is its type; nothing restates it elsewhere.

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

**Each wordlist hashes its names into chains.** Lookup hashes the folded token
once and walks the matching chain of each wordlist in the search order. An
engine load reads hundreds of thousands of tokens, many of them numbers that
miss every wordlist, so a single chain per wordlist would cost seconds per
load.

## Memory

**Two spaces, each reserved once.** The dictionary and the code are two spaces.
Each is one reservation from the address space, far larger than any program,
and its pages fill as its pointer advances. Nothing is sized from a measured
program.

### Dictionary space

A word's header and its data are laid down at `HERE` as it is defined, and
`ALLOT` grows the space. Headers and data interleave, as in Forth.

### Code space

Code has its own space and pointer because a page is writable or executable,
never both ([xcomp.md](xcomp.md#linking-and-publication)). On arm64 the engine
maps the code region twice: code runs from a read-execute view, and every store
into the region goes to a read-write alias of the same memory. Neither view
changes protection after boot: compiling a word never calls mprotect.

### Stacks

Each stack is its own mapping with an inaccessible page below and above it, so
the MMU enforces its bounds and compiled code carries no bounds check.

## Packages and privacy

**Privacy is the only restriction.** A word not everyone may call is private to
its package. System words are private words of the system package, and the
build strips private words from the delivered `hb`. There are no owner rows,
caller checks or marks on records.

**A sealed package cannot be reopened.** `package NAME` on a sealed package is
refused.

**Stripping drops headers; reachability decides code.** The linker writes a
sealed package's private wordlist empty, so no later source can name its
words. A word's code and body stay in the image only while a kept word reaches
them through a row or a reference cell. An application image keeps no header
at all.

## Interpreter

**One interpreter.** The interpret loop, definers, packages and source loading
are written once in typed Habu. A target that does not run them yet keeps the
kernel's own bodies for them (`src/habu/native-runtime.f`).

**The loop is Forth's.** `INTERPRET` reads a token and looks it up. Outside a
definition the word runs; inside one, an immediate word runs and any other word
goes to the checker as a call. A token that names no word is a number, which
is pushed or goes to the checker as a literal, or else an error. The state is
the pending definition, none while interpreting. Every construct (`if`, `{:`,
`s"`, `;` and the rest) is an immediate word that calls the checker's step for
it ([one reader](compilation.md#defining-a-word)). `evaluate`,
`evaluate-closed`, `include` and `require` are `INTERPRET` over another source.

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
exact selected dictionary record to the checker, which applies that word's
input and output effects and refuses a mismatch before the call, including a
pointer or execution token stored in a numeric cell. See
[design.md](design.md#typed-top-level).

**The REPL is `INTERPRET` per line.** When standard input is a terminal, it
reads a line, interprets it under `catch` and answers `ok` or the error. A
definition still open at the end of a line continues on the next.

## Platforms and code generation

**Two tiers on ARM64.** A definition compiles through the current platform's
codegen. ARM64 has two: tier 0 compiles fast, tier 1 emits faster code, and
`set-tier` chooses. x86-64 and Wasm have tier 1 only. Hand-written code per
platform is only the kernel's primitives.

### Cross-compilation

**A build names the platform it runs on and the platform it produces.** Any
`hb` builds `hb` for every required host
([xcomp.md](xcomp.md#build-host-product-and-target)). The build runs a word's
host body when it needs the word's value and compiles a target body for the
product ([xcomp.md](xcomp.md#compile-time-execution-and-target-data)).
Building for a platform never runs on it: `hb` on a macOS ARM64 machine
cross-compiles the x86-64 `hb`, and only running the product needs a machine of
its kind.

**Every codegen emits unplaced code.** A compiled definition is its bytes plus
a row for each call, outgoing branch, code address and data address in them,
and each row names a record ([header](#dictionary)), not an address. The
publisher places an emission in the running engine by resolving its rows
against that engine; the linker places it in an image by resolving them against
the image's layout. A build compiles a definition once when the product runs on
the host's platform, and twice otherwise: once to run on the host and once for
the product.

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

**A cell's declared type says whether it holds a reference.** Raw storage
holds no address ([forth.md](forth.md), "Raw storage never holds an address"),
so product data that holds a pointer or a quotation is laid by a declared form,
and that form's record says which of its cells hold references. The linker
writes each such cell as a reference to the object or function its value names,
and copies every other cell as bytes. Nothing scans integers for pointers, and
no registry lists pointer cells. A reference cell whose value is neither null
nor inside the product's spaces is refused, naming the cell.

## Builds

**A file is the unit of compilation.** Packages stay namespaces that any file
can open or reopen. The loader reads source from disk in its normal order, and
a file is compiled again when its inputs, the facts it consumed, the compiler
or the target change. Objects and images live in the tree's own out root
(tools/build-out.f); no build cache is shared between trees.

**A file's object holds its compiled definitions.** For each `:` and
`TRUSTED:` definition it holds the code, literals and checker state the
compile produced and the facts that compile consumed; compiler scratch is not
part of it. Addresses are relocatable references, resolved by declaration and
offset at install and never keyed, so an edit that changes an earlier file's
size leaves later objects valid. Symbols resolve by package and name, not by
the numbers the compiler assigned them. State a subsystem owns (compiler
identities, checker pools, the literal pool) is exported and restored by that
owner; an object never copies a span of another owner's data.

**A reused file runs its top level again.** Its compile-time code, defining
words included, runs as it did when the file was compiled, remaking the
file's records, wordlists, DATA and constants and repeating what that code
writes to the output device, the file system or the environment. At each
definition head whose inputs still hold (below), the definition installs from
the object in place of compiling its body; any other definition compiles. A
value a body captured at compile time stays part of the object until a tracked
input changes, so a program that needs a fresh value on every run reads it at
run time. A reused file loads where its source loads it, so package
visibility, `require`/`include` semantics and the order of reopenings hold as
in a clean build. The proof is a warm build whose product equals the product
of a build from an empty cache.

**A file's key names the file, not what loaded before it.** The key is the
engine, the target profile, the build mode, the key of the file that loaded it,
the root's role (the engine root or another), the canonical path relative to
that root, the load kind and the occurrence: how many earlier loads of the same
file, with the same kind and under the same parent, the build made, which only
`include` repeats. The file's parts are names, never content: folding the bytes
of preceding files into the key would make an edit to the root file recompile
every library it loads. The engine part is a hash of the executable that
compiles, and the compiler part digests every file loaded before the compiler,
so an edit to a file in either changes every file's key. A file under its root
keys the same in a copy of the
tree or in the tree reached at another path, so those builds share entries. A
copy whose bytes differ is an edit: the file compiles whole, as an edit in
place does.

**An entry is valid while what it consumed still holds.** In order: the file's
size and change time match, or else its bytes hash the same; the
environment and path-resolution inputs it read answer the same; and every fact
it consumed from earlier files still holds. A consumed fact is a record of an
earlier file that the compilation looked up (its contract, immediacy, fixed
value or generated family, the resolution of a spelling in the search order)
or the search order at entry, recorded once per file, and the records a
compile-time run reached, an immediate word's run while a body is read
included: each word through its code and each data record through its
extent, following the addresses its pointer cells hold. A read is tracked as
a read of the record whose address it derives from, within that record's
extent; a read past the extent into the next record is outside what the
build tracks. A comment or body-only edit leaves later files valid; renaming
a word or changing a contract, a constant or `immediate` invalidates exactly
the files that consumed it. In a reused file, each definition installs from
the entry only while the file's own records,
wordlists and DATA before its head, and the loads among them, match the ones
the entry recorded; otherwise that definition compiles live, as when an
earlier file's word now makes another record, lays out other data or loads a
file at another point at this file's top level.

**A file that reads earlier data around its owners stores no object.**
Compile-time code reads earlier files' state through dictionary records and
owner-exported state. A file whose compile-time code reads earlier data through
a trusted body outside an owner interface, or an untracked foreign or system
call is compiled on every build, and the build names the word. A mapping a
record of the file holds is not earlier data: the top level that reads it runs
again on every load. Reads made inside the kernel are the kernel's and do not
make a file ordered.

**An edit during a build is read in due course.** A file already compiled
keeps the version it read for that build and is rebuilt on the next; a file not
yet compiled is read when the loader reaches it. A build neither snapshots the
source tree nor stops because an editor changed a file.

## Images

**A stripped image carries only its entry's closure.** Without `--repl`, an
image has no compiler, dictionary or REPL: the code `MAIN` reaches and the data
window the application loaded into. It compiles nothing at run time, so it runs
under `MemoryDenyWriteExecute`.

**The window holds only what the application requires.** The builder opens the
capture window before the application loads, with only the application's own
library copies visible to `require`, so the application's require closure lies
inside the restored span. A library loaded before the window would be the copy
the application's `require` resolves to; its cells would lie below the span and
read as zero in the image, so the linker refuses such an image. A program that
uses a library word without requiring its module fails to compile, naming the
word.

**Nothing is relocated because of its value.** A cell reaches the image as an
address only because a declaration, a claim or a code row names it; the linker
never reads an integer as a pointer. A value it cannot account for is refused,
naming the word, the cell and the value.

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

**Loading checks and runs; a check runs nothing that acts.** Loading a file is
the checker's regular use: the checker checks each definition at `;`, and the
interpreter runs everything else, effects included. A check, the editor's or
the check tool's, is that same load in a child `hb` with one setting: a word
that acts outside the process does not run. Everything else runs, because
running words decides which names exist: `ENUM` and the sum-type definers
generate definitions by `evaluate`, and `STRUCTURE` reads ahead. The child
exits, so what ran leaves nothing behind, and the check's verdict is the load's.
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
`require` line or a changed required file rebuilds the child.

**A check reports every independent error.** When the check sets the
multi-error flag, a refused definition is rewound as always and the
interpreter goes on at the next token. A mention of a refused word is
undefined. The tool counts a definition whose first defect is such a mention
as refused, without reporting it. An error outside a definition ends the file.

**A lint that states a language rule is a refusal.** The definer refuses a
reserved name, a broad `TRUSTED:` and a global that shadows a primitive. Facts
about a whole program, such as the error codes it uses or its public
signatures, come from walking the dictionary after the program loads.
