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
no type and is private to the system package.

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
its package. System words are private words of the system package. There are
no owner rows, caller checks or marks on records.

**A sealed package cannot be reopened.** `package NAME` on a sealed package is
refused. A build seals every package its engine files made before it reads
program source.

**Stripping drops headers; reachability decides code.** `hb` keeps every
package's private headers, which its own builds read; an application image
keeps no header at all. A word's code and body stay in the image only while a
kept word reaches them through a row or a reference cell.

## Interpreter

**One interpreter.** The interpret loop, definers, packages and source loading
are written once in typed Habu.

**The loop is Forth's.** The loop reads a token and looks it up. Outside a
definition the word runs; inside one, an immediate word runs and any other word
goes to the checker as a call. A token that names no word is a number, which
is pushed or goes to the checker as a literal, or else an error. The state is
the pending definition, none while interpreting. Every construct (`if`, `{:`,
`s"`, `;` and the rest) is an immediate word that calls the checker's step for
it ([one reader](compilation.md#defining-a-word)). `evaluate`,
`evaluate-closed`, `include` and `require` run the loop over another source.

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
space is untouched. The loop restores the source, package scope and used
packages it saved on entry, then rethrows.

**At top level, a word's type guards the stack.** The interpreter supplies the
exact selected dictionary record to the checker, which applies that word's
input and output effects and refuses a mismatch before the call, including a
pointer or execution token stored in a numeric cell. See
[design.md](design.md#typed-top-level).

**The REPL runs the loop per line.** When standard input is a terminal, it
reads a line, interprets it under `catch` and answers `ok` or the error. A
definition still open at the end of a line continues on the next.

## Platforms and code generation

**T0 compiles on ARM64 and x86-64.** A definition compiles through the current
platform's tier 0 (T0) codegen. Hand-written code per platform is only the
kernel's primitives.

### Cross-compilation

**A build names the platform it runs on and the platform it produces.** Any
`hb` builds `hb` for every required host
([xcomp.md](xcomp.md#build-host-product-and-target)). The build runs any
word but one its source defines in a target section, and compiles every word
for the target ([xcomp.md](xcomp.md#host-and-target-words)).
Building for a platform never runs on it: `hb` on a macOS ARM64 machine
cross-compiles the x86-64 `hb`, and only running the product needs a machine of
its kind.

**Every codegen emits unplaced code.** A compiled definition is its bytes plus
a row for each call, outgoing branch, code address and data address in them;
an object names what each row reaches by its record ([header](#dictionary)),
not by an address. The
publisher places an emission in the running engine by resolving its rows
against that engine; the linker places it in an image by resolving them against
the image's layout. A build compiles a definition once when the product runs on
the host's platform or the definition is in a target section, and twice
otherwise: once to run on the host and once for the product.

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

**A part of a file is the unit of compilation.** Packages stay namespaces that
any file can open or reopen. A file's source is cut into parts at each file it
requests, and each part has its own object. The loader reads source from disk
in its normal order, and each part decides at its own turn: it links its saved
object while the object still holds, and compiles otherwise, to its request or
the file's end, after linked parts too. Either way the file it requests loads
before the next part. Objects live in the tree's own out root
(src/lib/build-out.f); no build cache is shared between trees. The proof is a
warm build whose program equals the program of a build into an empty out root.

**A build decides from stamps and times and hashes nothing.** A file's stamp
is its change time and size, read before the file is read. Its parts' objects
link only while the stamp is equal, so a file put back with an older time
compiles. Each word and each stretch of data carries the clock time its result
last changed; a word that compiles to the bytes its part's old object held
keeps its time, so the parts that depend on it link. A comment edit compiles
one file.

**A part depends on the earlier words and data it uses**, and links only while
each is in the build with the time the part's object recorded:

- a word or a stretch of data its compiled code refers to;
- a public word its source, or a text its compile evaluates, spells outside
  comment lines, and for a part that reopens a package, or evaluates a text
  that does, that package's private words too;
- a part that changes a record an earlier part made, which counts as defining
  that record's name;
- when it ran an earlier word while it compiled, every word that word's code
  reaches, every stretch of data it reaches and what every part stored into
  that data, which carries its own time, those parts loading in the order they
  did then;
- the source root it starts in, each root its compile looked up and each
  environment variable it read, which must answer as they did.

**hb's identity is its own stamp.** Objects are saved at `<out>/obj/<hb's
stamp>/<target>/<mode>/<source path>.hfo`, so a new hb compiles every file
once. hb-build replaces its output only when the image differs apart from its
signature, and writes a file it generates only when the text differs.

**A part whose load a link could not repeat never links** and compiles on
every build: one that starts or ends inside an open package or `using`, one
whose request is not a plain one at its top level (made inside a definition,
with cells on the stack or from evaluated text), the part after such a
request, one that catches a load it asks for before the load runs, every part
of a file with no stamp (src/habu/linker.f UNLINKABLE), and one whose compile
read what it could not record: on a task's or a callback's thread, through
src/lib/fs-anchored.f or in a process it started. Its object only
keeps its words' times, so the parts that depend on it link while those words
compile to the same bytes.

**An edit during a build is read in due course.** A file already compiled
keeps the version it read for that build and is rebuilt on the next; a file not
yet compiled is read when the loader reaches it. A build neither snapshots the
source tree nor stops because an editor changed a file.

## Images

**A stripped image carries only its entry's closure.** Without `--repl`, an
image has no compiler, dictionary or REPL: the code `MAIN` reaches and the data
that code reaches. It compiles nothing at run time, so it runs under
`MemoryDenyWriteExecute`.

**The program is the build's product, not the builder's memory.** hb-build loads
hb's startup files and then the application into a build space of their own and
links the program from it (`tools/hb-build-core.f`), so nothing the builder
loaded for itself reaches the image. A program that uses a library word without
requiring its module fails to compile, naming the word.

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
