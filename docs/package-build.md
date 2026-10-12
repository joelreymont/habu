# Package-based incremental builds

This is implementation guidance for future incremental builds, not a description
of a working package cache. Today's native product build still loads source into
the running engine and captures an image; see [compilation.md](compilation.md).

## Build model

PACKAGES are compilation units. The build reads source files from disk in their
normal package load order, compiles a package when its inputs have changed, and
reuses an unchanged package's compiled result when that result is compatible
with the current compiler and target. This is the C compiler and make model:
source files remain ordinary editable files.

A build in progress finishes when a file changes. A package already compiled
during that build keeps the version it read and is rebuilt on the next build.
A package not yet compiled reads the file when its turn comes. The build does
not require a frozen source-tree snapshot or abort solely because an editor
changed a file.

Package visibility and source order retain their language meaning. A cache
cannot expose a declaration before the loader would, skip required top-level
work, or treat a different compiler, ABI, layout or target as interchangeable.
A package result may be stored as a validated object-like artifact with code,
data and the references needed for linking. Its storage format and reuse checks
are implementation choices to verify against the real build path, not
requirements for immutable source files.

Cross-compilation selects the output target explicitly. The compiler runs on
the build host; a produced binary only proves that it runs after execution on
the matching target. [portability.md](portability.md) describes the broader
target and linker architecture.
