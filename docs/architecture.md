# Architecture

What Habu is made of, from the bottom up: what each part owns and the rules it
follows. [compilation.md](compilation.md) follows a definition from source to
native code and an image; [bootstrap.md](bootstrap.md) makes the first `hb`;
[portability.md](portability.md) holds the detail for platforms and
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
code per instruction set and a host body for Gforth, and the build refuses a
body without a row or a row without a body. Where a primitive can be written
without itself, a reference implementation in checked Habu runs beside every
body in tests (`test/prim-parity.f`).
**Now:** 233 primitives in 285 rows, 47 of them per-owner rows that
[privacy](#packages-and-privacy) removes. Gforth has no host bodies;
`bootstrap/cg/forth.fs` carries a second copy of the ARM64 bodies (`FPRIM`).

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
(`src/habu/layout.f:167-200`), with a 16-byte inline name or a pointer to a
longer one (1,811 of 20,626 names are longer), a numeric wordlist id and a code
length. Their flags restate type facts and carry marks (`DNAME-MIN-IN`,
`DNAME-WIDE`, `DNAME-INT`, `DNAME-OWNED`). A `does>` clause gets a companion
record of its own (`src/compiler/native/publish.f:236`).

**Each wordlist hashes its names into 64 chains.** Lookup hashes the folded
token once and walks the matching chain of each wordlist in the search order.
Plain chains would cost seconds per engine load: a load reads 237,186 tokens,
11,264 of them numbers that miss every wordlist, the global wordlist holds 6,245
words while loading, and a chain step costs about 10 ns in checked Habu. With 64
chains the global one stays under 100 headers.
**Now:** one global table of 131,072 slots is keyed by name hash and wordlist
id, with a linear fallback for retired records (`src/habu/layout.f:205-229`).

## Memory

**Two spaces, each reserved once.** The dictionary and the code are two spaces.
Each is one reservation from the address space, far larger than any program,
and its pages fill as its pointer advances. Nothing is sized from a measured
program.

### Dictionary space

A word's header and its data are laid down at `HERE` as it is defined, and
`ALLOT` grows the space. Headers and data interleave, as in Forth.
**Now:** records are a fixed array of 65,536 slots inside the code region; that
count and the region's 32 MiB were raised three times after overflows
(`src/habu/layout.f:167-198`). Data is a separate space, and the engine's own
state sits at 315 hand-assigned offsets in a header before it
(`src/habu/layout.f`, `DATA-START` at :2273).

### Code space

Code has its own space and pointer because a page is writable or executable,
never both ([portability.md](portability.md) §14).
**Now:** code shares one 32 MiB region with the records, and the engine flips
the region between writable and executable around compilation
(`src/habu/habu2.f:12466-12476`). Records in that region are write-protected
and changed through `patch32` and page flips (`src/habu/xref.f:476`,
`LPROTREC`).

### Stacks

Each stack is its own mapping with an inaccessible page below and above it, so
the MMU enforces its bounds and compiled code carries no bounds check
(`src/habu/stack-abi.f:1-13`).

### Mapped buffers

`lib/memory.f` hands out OS-backed mappings that the caller owns and releases.
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
**Now:** a primitive is kept when its name appears as a whitespace-separated
token in the program's text (`src/habu/treeshake.f:1-7`).

## Interpreter

**One interpreter.** The interpret loop, definers, packages and source loading
are written once in typed Habu and run on every platform.
**Now:** ARM64 runs a machine-code interpreter (`src/habu/habu2.f`); the Habu
loop (`src/habu/interpret.f`) runs only under tests and does not yet read
`constant`, `defer`, `cast:`, `linear:` or `immediate`. Its `create` and
`variable` publish the word through the `def-create` writer row, whose ARM64
body is the tail of the engine's own `create` (`DEFWRITE:ADDR-TAIL,`); x86-64
has no interpreter (`src/habu/kernel-x64.f:1752`) and refuses the row.

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
`EM-INTERPRET-*-KEYWORDS` at :10083, `EM-COMPILE-*-KEYWORDS` at :11288), and
`src/core/include.f` reaches the loop through a defer (`INCLUDE-INTERPRET`,
:1518).

**The kernel holds what the typed interpreter needs to load.** That is:
- lookup over the hashed chains;
- the header writer and the publisher;
- `parse-name`, `parse` and `refill` over the current source;
- `execute`, `catch` and `throw`;
- `here`, `allot` and `,`;
- file reading;
- the checker and the codegen's hook cell.

Gforth's reader and the interpreter share that lookup, so both resolve names
only against Habu wordlists ([bootstrap.md](bootstrap.md#the-bootstrap-process)).
When `hb` reads the kernel, its definitions go through the same definer and
codegen with checking off: the checker records each construct with every value
one cell wide, and the definer writes a null type.

**A failed definition leaves the dictionary as it was.** A throw before `;` or
a refusal at `;` rewinds `HERE` to the pending definition's type, drops its
events and clears the pending state. Its emission was never placed, so code
space is untouched. `INTERPRET` restores the source, package scope and used
packages it saved on entry, then rethrows.

**At top level, a word's type guards the stack.** Before running a typed word
outside a definition, the interpreter compares the stack depth with the word's
input count and refuses an underflow. It has no other guard.
**Now:** the guard reads `DNAME-MIN-IN` from the record's flags, and the
prompt also checks `DNAME-WIDE`, `DNAME-INT` and a policy seal
(`src/habu/habu2.f:10120-10166`). A top-level row tracker
(`src/core/top-row.f`) reports warnings only.

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
(`src/habu/jit.f`). The platform codegen compiles 2.51 ms per word against the
JIT's 0.105 ms (`docs/compiler-measurements.md:361`).

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
**Now:** tier 1 is 59,710 lines. It reads the definition again from a token
tape, builds an IR module, runs passes and two verifiers, allocates registers
and emits. It costs 524 µs for a trivial word
(`docs/compiler-measurements.md:1361`), and its selection and emission proper
are about a tenth of that (:607-613). Its code runs 1.0 to 9.1 times as fast
as tier 0's (:157-165) through what this pass keeps: inlined primitives, stack
values in registers, constants as immediates and comparisons folded into
branches (:11-133). Tier 0 compiles a trivial word in 9.0 µs with checking off
(:1081).

### Cross-compilation

**A build names the platform it runs on and the platform it produces.** Any
`hb` builds `hb` for every required host ([portability.md](portability.md)
§1.1). The build runs a word's host body when it needs the word's value and
compiles a target body for the product ([portability.md](portability.md) §7).
Building for a platform never runs on it: `hb` on a macOS ARM64 machine
cross-compiles the x86-64 `hb`, and only running the product needs a machine of
its kind ([portability.md](portability.md) §2.3).
**Now:** only the x86-64 cross-build compiles definitions a second time for the
target and links them (`src/compiler/native/shadow.f`, `src/habu/link-x64.f`);
every other build captures the running engine.

**Every codegen emits unplaced code.** A compiled definition is its bytes plus
a row for each call, outgoing branch, code address and data address in them,
and each row names a record ([header](#dictionary)), not an address. The publisher places an emission
in the running engine by resolving its rows against that engine; the linker
places it in an image by resolving them against the image's layout. A build
compiles a definition once when the product runs on the host's platform, and
twice otherwise: once to run on the host and once for the product.
**Now:** the ARM64 codegen emits only placed code
(`src/arch/arm64/passes.f:315`, `UNPLACED-UNSUPPORTED`), and the ARM64 capture
finds call sites again by decoding branches and address chains in the placed
bytes (`src/habu/aot-capture.f` `ACAP-TGT`, `ACAP-CHAIN@`). x86-64 and Wasm
emit unplaced code only in a second compile beside the placed one
(`src/compiler/native/compiler.f:537-580`).

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
words get hand-written routines (`does-patch`, `src/habu/prims.f:636`;
`LDOESPATCH` in `src/habu/habu2.f`), and the x86-64 kernel refuses `create`
and `does-patch` (`src/habu/kernel-x64.f:1777`, `:2379`).

**A cell's declared type says whether it holds a reference.** Raw storage
holds no address ([forth.md](forth.md), "Raw storage never holds an address"),
so product data that holds a pointer or a quotation is laid by a declared form,
and that form's record says which of its cells hold references. The linker
writes each such cell as a reference to the object or function its value names,
and copies every other cell as bytes. Nothing scans integers for pointers, and
no registry lists pointer cells. A reference cell whose value is neither null
nor inside the product's spaces is refused, naming the cell.
**Now:** the build keeps a registry of pointer cells, filled by `defer`, `is`,
`xt!` and `ptr-cell-mark` (`src/habu/address-cells.f`,
`src/habu/habu2.f:7592-7602`). `xt!` admits an execution token into a raw cell
(`src/habu/prims.f:452-466`). A record that mixes pointer and scalar fields has
no declared form, so one of its halves goes through a cast that the linker
cannot see ([effects.md](effects.md), the two open launders;
[type-system.md](type-system.md) §10).

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
before it is compiled. `lib/source-lex.f` (838 lines) tokenizes for the check
tool and nine lints. `tools/check-core.f` marks origins into a copy of the file
so that the engine's diagnostics get positions.

**Loading checks and runs; a check runs nothing that acts.** Loading a file is
the checker's regular use: the checker checks each definition at `;`, and the
interpreter runs everything else, effects included. A check, the editor's or
the check tool's, is that same load in a child `hb` with one setting: a word
that acts outside the process does not run. Everything else runs, because
running words decides which names exist: `ENUM` and the sum-type definers
generate definitions by `evaluate` (`src/core/sumtype.f`), and `STRUCTURE`
reads ahead (`src/core/structure-decl.f:714`). The child exits, so what ran
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
definers is cheap: a cold load of `lib/process-command.f` with its closure takes
0.10 s, and of `tools/lsp-core.f` 0.66 s.
**Now:** the editor and the check tool's pre-pass read a file without running
it. `src/habu/verify-source.f` models each definer by name (`ENUM-END?`,
`STRUCTURE-END?` and about 45 more), and a definer that writes its word as
text needs a `generates:` row, which is a claim the pre-pass cannot prove. The
two readers disagree: after `CMD:COMMAND C`, `: U ( -- n ) C#VEC ;` checks 0
under `--verify-only` and 70 when the file loads ([forth.md](forth.md), the
`generates:` rule). The pre-pass is also slower than the load it avoids:
`--verify-only` takes 1.0 s on `lib/process-command.f` and 1.71 s on
`tools/lsp-core.f`, about 0.6 s of it loading the verifier.

**A check reports every independent error.** When the check sets the
multi-error flag, a refused definition is rewound as always and the
interpreter goes on at the next token. A mention of a refused word is
undefined. The tool counts a definition whose first defect is such a mention
as refused, without reporting it. An error outside a definition ends the file.
**Now:** in multi-error mode the hook publishes a refused word under its
declared signature so that later definitions resolve it
(`src/core/check-hook.f:100-104`), which leaves a word behind.

**A lint that states a language rule is a refusal.** The definer refuses a
reserved name, a broad `TRUSTED:` and a global that shadows a primitive. Facts
about a whole program, such as the error codes it uses or its public
signatures, come from walking the dictionary after the program loads.
**Now:** these are separate tools over the token stream
(`tools/reserved-name-lint.f`, `tools/checked-boundary-lint.f`,
`tools/lint/shadow-lint.f`, `tools/error-code-lint.f`,
`tools/public-signatures.f`).
