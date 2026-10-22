# File-based incremental builds

This is implementation guidance, not a claim that the file cache is complete.
Today's native product build loads source into the running engine and captures
an image; see [compilation.md](compilation.md).

Files are compilation units. Packages remain namespaces and can be opened or
reopened across files. Read source from disk in the loader's normal order and
recompile a file when its declared inputs, dependencies, compiler or target
change.

Fix the object format before adding automatic reuse. A file's object must
preserve the result of compiling that file: definitions, code, initialized
data, literals, persistent changes to earlier data, and the checker and loader
state needed by subsequent source and the final binary. Store addresses as
relocatable references. Temporary compiler scratch is not part of the object.

Compile-time code runs when the file is compiled. Importing its object installs
the compiled result; it does not execute that code again or repeat external
side effects. Values captured from the environment remain part of that result
until an input that the build tracks changes. A program that needs a fresh
environment value on every invocation must read it at runtime.

Importing an object at its original load position must make the stored
definitions and data available to subsequent compilation. With the same tracked
inputs, prove this in a fresh process that imports objects, compiles downstream
source and produces the same observable binary behavior as a clean build.
Preserve package visibility and require/include semantics; do not group
reopenings ahead of intervening source. Resolve symbols by their package and
name, without assuming the compiler assigned them the same internal numbers.

A running build finishes after an edit. A file already compiled keeps the
version it read for that run and is rebuilt on the next build. A file not yet
compiled is read in due course. Do not require an immutable source-tree snapshot
or abort solely because an editor changed a file. This is the compiler/make
model with ordinary editable source files.

Cross-compilation selects the output target explicitly. The compiler runs on
the build host; executing the produced binary on its target establishes runtime
behavior. [portability.md](portability.md) describes target and linker roles.

## Keys and validity

A file's cache entry is named by what identifies the file, not by what was
loaded before it: the engine, the target profile, the build mode, the canonical
path and root, the load kind and the occurrence among the parent's loads.
Folding preceding files into the key makes an edit to the root file recompile
every library it loads; that was the defect of the first whole-file cache.

An entry is valid when, in order: the file's size and modification time match,
or else its source bytes hash the same; the tracked environment and
path-resolution inputs it read still answer the same; and every fact it
consumed from earlier files still holds. A consumed fact is a dictionary record
of an earlier file the file's compilation looked up: its contract, immediacy,
fixed value, generated family, the resolution of a spelling in the search
order, and the search order at entry. Each consumed fact is recorded once per
file. A comment or body-only edit leaves an earlier file's declarations and
contracts unchanged, so later files stay valid; renaming a word, changing a
contract or a constant, or adding `immediate` invalidates exactly the files
that consumed it.

Addresses are relocated, never keyed. When an edit changes an earlier file's
size, later objects resolve their code and data references by declaration and
offset at import.

State owned by a subsystem is exported and restored by that owner: compiler
identities, checker pools, the literal pool, task slots, the require registry
and the search order. An object never copies a span of another owner's data.
Compiler scratch is not part of the object.

Compile-time code reads earlier files' state through dictionary records and
owner-exported state. A file whose compile-time code reads earlier data
through a raw address, a trusted body outside an owner interface, or an
untracked foreign or system call stores no object: the build names the word
and compiles that file every time.
