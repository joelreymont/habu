# Native file objects

`hb-build` uses source files as compilation units. Packages remain namespaces.
The ordinary loader determines file order, and each file owns its object in
`.out/hb-file-<key>.hfo`, or the directory selected by `HABU_BUILD_CACHE`.
Unchanged files import their compiled state; changed files compile normally.
The build then links the application or saves its REPL image.

Before writing a native range, the loader checks new DATA and changed earlier
DATA for live code, dictionary and mapped-process addresses. It skips declared
cells only when their FILE reference was encoded, and treats compiler string
literal bodies as text. A byte window cannot be a separate cell when it
straddles the recorded start of a created word's DATA allocation; windows
inside its raw body remain bytewise checked. If a range holds an address the
object cannot restore,
the loader continues compiling the source but leaves out that file's cache
artifact. Image lifecycle cleanup and the final AOT link still run at their
normal later boundary. Explicit `--export-file` refuses the same unsafe cell
and names its owner, offset and value in the diagnostic.

A parent object records its own compiled ranges and the child loads between
them. The complete object dependency tree is selected before any parent range
is imported. If a required object is missing, stale or incompatible, the parent
compiles normally from its beginning. The key includes the running engine,
build mode, canonical paths and the source prefix actually read by the compiler.
File freshness uses size and modification time. Recorded path-resolution
queries also invalidate reuse when a new nearer file changes an include's
meaning. Imported ranges restore the package and search scope needed by later
compilation.

Files are read when the loader reaches them. Edits during a build are left for
the next ordinary read or the next build; they do not require source snapshots
or a restart. Once an object and its dependencies have been selected, the build
uses those objects; later edits are picked up by the next build.

The outer file object begins with the eight-byte magic `HBNFILE9` and format
version 12. Each range records its entry and exit compiler session serials,
the opening lease, epoch, module and builder serials for an exit session,
the module, context, epoch, lease and builder serials at range entry, and their
high-water marks at exit. Selection rejects a
tree if existing state prevents exact reconstruction. A file that leaves a
session open outside the native compiler does not write a reusable object.
Replay closes or opens the compiler session at the saved boundary and advances
the identity histories before importing the native unit, so handles retired by
the skipped source cannot become valid again. Native ranges
begin with `HBNUNIT2` and use the existing native relocation machinery. They
use inner format version 6; the automatic cache key uses
`native-file-ranges-v15`. The outer `HBNFILE9` layout remains version 12. They
carry dictionary records, code relocations, wordlists, retired prefix
dictionary rows, initialized DATA,
writes to earlier DATA, typed pointer and execution-token cells, literals,
require/provided paths and checker effects. Objects restore compilation state;
they do not repeat external I/O from skipped source or recreate operating-system
resources. The current native codec stores AArch64 code.

## Explicit export and import

`tools/native-unit-build.f` also accepts `--export-file` and `--import-file`
with `prefix-file selected-file downstream-file object-path output-path
[whitebox]`. Export compiles the selected file through the ordinary loader and
writes its native object. Import restores that object at the loader
continuation, then compiles the downstream file. Application paths may be
outside the engine source tree.

This explicit path keys the engine, target profile and features, canonical
source paths and consumed bytes. Downstream files can change independently.
Nested source digests and path-resolution queries are checked before reuse.
The named object is one `HBNFILE9` file object with a single `HBNUNIT2` native
range. It uses the ordinary FILE range replay for compiler identity history,
checker state, package scope and source inputs. A range whose state cannot be
replayed does not publish an artifact.

The explicit command runs on the already-built compiler used to launch it. It
loads the prefix and selected file in that compiler, imports or exports the
selected object, compiles the downstream file, and saves the resulting REPL.
It does not rebuild the compiler source tree. The key includes that compiled
engine's identity and the loaded explicit-file driver source, as well as the
source files read through the loader. An edit to compiler source that this
command does not load takes effect when a new engine is built; its new engine
identity then changes the key. The optional `whitebox` argument requires a
whitebox input engine; omitting it requires a sealed input engine, so the
output keeps the input engine's image class.

`test/native-file-state-e2e.f` and `test/native-file-types-e2e.f` build small
applications twice through the ordinary FILE loader, then compile a new
downstream client against the imported object. They cover DATA and earlier
writes, created words, reopened packages, typed cells, literals, nominal and
linear types, deferred words, and rejected typed clients. The forms rows cover
definition-free, namespace-only, global-word and repeated-include outcomes;
they inspect the cached object's inner `HBNUNIT2` sections to distinguish
zero CODE and records from positive CODE, records, updates and requires.
`test/native-file-unit-e2e.f` covers explicit code parity, the imported
dictionary and checker, callable occurrences, source-root and image-class
refusals, dependency invalidation, stale lease and builder handles, and open
package scope.
`test/native-file-ffi-e2e.f` verifies
that an exported FFI declaration is callable after import.
The other automatic-loader tests cover aliases, stack values across includes
and stripped images.
