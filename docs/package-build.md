# File-based incremental builds

This is implementation guidance, not a claim that the file cache is complete.
Today's native product build loads source into the running engine and captures
an image; see [compilation.md](compilation.md).

Files are compilation units. Packages remain namespaces and can be opened or
reopened across files. Read source from disk in the loader's normal order and
recompile a file when its inputs, dependencies, compiler or target change.

Fix the object format before adding automatic reuse. A file's object must
preserve the result of compiling that file: definitions, code, initialized
data, literals, persistent changes to earlier data, and the checker and loader
state needed by subsequent source and the final binary. Store addresses as
relocatable references. Temporary compiler scratch is not part of the object.

Importing an object at its original load position must have the same effect on
subsequent compilation as compiling its source there. Prove this with a fresh
process that imports objects, compiles downstream source and produces the same
observable binary behavior as a clean build. Preserve package visibility and
require/include semantics; do not group reopenings ahead of intervening source.

A running build finishes after an edit. A file already compiled keeps the
version it read for that run and is rebuilt on the next build. A file not yet
compiled is read in due course. Do not require an immutable source-tree snapshot
or abort solely because an editor changed a file. This is the compiler/make
model with ordinary editable source files.

Cross-compilation selects the output target explicitly. The compiler runs on
the build host; executing the produced binary on its target establishes runtime
behavior. [portability.md](portability.md) describes target and linker roles.
