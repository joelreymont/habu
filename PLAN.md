# Plan: 100% ANSI Common Lisp Parity

## Scope
- **Target**: All 978 symbols in COMMON-LISP package
- **Semantics**: CL-compatible superset (Habu extensions permitted)
- **Impl-dependent**: Own choices (not copying SBCL)
- **Testing**: ansi-test (Paul Dietz) + per-feature tests
- **MOP**: Full AMOP-compatible MetaObject Protocol
- **Tracking**: Create dots for each phase/feature
- **Special vars**: CL-standard (closures capture lexical; specials dynamic at call time)
- **Pretty printer**: Full XP (Waters' algorithm, logical blocks, etc.)
- **Format**: Full CLHS compliance including ~< ~> edge cases
- **Threading**: Thread-safe core (prepare for bordeaux-threads)
- **Strings**: UTF-32/UCS-4 internally (like SBCL, O(1) char access, future-proof)
- **Debugger**: REPL-based interactive debugger for `break`

## Current State
- **Symbol coverage**: `978/978` symbols implemented (`docs/cl-symbols.md`)
- **Functional parity**: not yet proven against `ansi-test`
- **stdlib.habu**: 358 definitions, 2813 lines
- **Conformance gap**: no committed ansi-test baseline + no CI conformance gate

## Active Plan: Functional Parity Closure

### Exit Criteria
- `ansi-test` corpus pinned and reproducible.
- Habu baseline report committed and machine-readable.
- Zero failing ANSI test IDs in baseline delta.
- PR smoke gate + nightly full conformance gate green.
- Signoff report published with artifact links.

### Required Steps (with dots)

1. **Harness + Baseline**
   - `habu-pin-ansi-test-8ec33815` Pin ANSI test corpus.
   - `habu-add-ansi-test-05377306` Add deterministic test runner (`sbcl|habu`).
   - `habu-normalize-ansi-output-9aa78296` Normalize raw logs to JSON.
   - `habu-record-functional-parity-34a77dda` Commit baseline artifacts.
   - `habu-map-failures-to-e9ce25c5` Map every failing ID to a subsystem bucket.

2. **Reader/Printer closure (batch 1: <=5 failing IDs)**
   - `habu-add-reader-printer-9a30a9ff` Add reduced repro tests.
   - `habu-fix-reader-printer-d2f6a737` Implement fixes and close mapped IDs.

3. **Condition/Restart closure (batch 1: <=5 failing IDs)**
   - `habu-add-cond-restart-0e4fd31b` Add reduced repro tests.
   - `habu-fix-cond-restart-59500793` Implement fixes and close mapped IDs.

4. **CLOS closure (batch 1: <=5 failing IDs)**
   - `habu-add-clos-repro-f4f345fc` Add reduced repro tests.
   - `habu-fix-clos-failures-fb42028e` Implement fixes and close mapped IDs.

5. **Pathname/Stream closure (batch 1: <=5 failing IDs)**
   - `habu-add-pathname-stream-2228273a` Add reduced repro tests.
   - `habu-fix-pathname-stream-48fc38c3` Implement fixes and close mapped IDs.

6. **Compiler/Evaluator closure (batch 1: <=5 failing IDs)**
   - `habu-add-compiler-eval-74528b18` Add reduced repro tests.
   - `habu-fix-compiler-eval-b66ae46d` Implement fixes and close mapped IDs.

7. **Package/Readtable closure (batch 1: <=5 failing IDs)**
   - `habu-add-pkg-readtable-a3987459` Add reduced repro tests.
   - `habu-fix-pkg-readtable-b7469182` Implement fixes and close mapped IDs.

8. **Gate + Signoff**
   - `habu-add-conformance-ci-d7fca6ff` Add smoke + nightly CI gates.
   - `habu-publish-functional-parity-8447f41b` Publish final signoff report.
   - Required report artifact: `docs/ansi-parity-signoff.md`
   - Required inputs: `docs/ansi-parity-baseline.json`, `docs/ansi/results/sbcl-latest.json`, `docs/ansi/results/habu-latest.json`
   - Required decision fields: corpus revision, fail counts, regression check status, CI smoke/nightly status, final go/no-go.

### Dependency Order
- Execute strictly in numeric order.
- Each `fix-*` dot depends on its paired `add-*-repro` dot.
- Do not close `habu-publish-functional-parity-8447f41b` before all `fix-*` dots and CI gate dot are closed.

## Gap Analysis

### Phase 1: High Priority (User-selected)

#### 1.1 Full LOOP Macro ✅ COMPLETE
**All features implemented** (stdlib.habu:3274-3751):
- `for var from/to/by/downfrom/downto/upto/below/above` - arithmetic iteration ✅
- `for var in list` - list iteration ✅
- `for var on list [by step-fn]` - cons iteration ✅
- `for var = expr [then step]` - general iteration ✅
- `for var across vector` - vector iteration ✅
- `for var being hash-keys/hash-values of hash [using ...]` - hash iteration ✅
- `for var being symbols/external-symbols/present-symbols of pkg` - package iteration ✅
- `with var = expr` - auxiliary variables ✅
- `repeat n` - counted iteration ✅
- `while`/`until` - conditional termination ✅
- `do`/`doing` - side effects ✅
- `if`/`when`/`unless ... do/collect/... [else ...] [end]` - conditionals ✅
- `collect`/`collecting [into var]` - list accumulation ✅
- `append`/`appending [into var]` - list concatenation ✅
- `nconc`/`nconcing [into var]` - destructive concatenation ✅
- `sum`/`summing [into var]` - numeric accumulation ✅
- `count`/`counting [into var]` - counting ✅
- `minimize`/`maximizing [into var]` - extrema ✅
- `maximize`/`maximizing [into var]` - extrema ✅
- `always test` - universal quantifier (early exit on nil) ✅
- `never test` - negated universal (early exit on non-nil) ✅
- `thereis test` - existential quantifier (return value on non-nil) ✅
- `finally body...` - post-loop code ✅
- `initially body...` - pre-loop code ✅
- `return expr` - early exit with value ✅
- `named name` - named block for return-from ✅
- Destructuring: `for (a b) in list-of-pairs` ✅

**Files**: `stdlib.habu:3274-3751`

#### 1.2 CLOS Method Combinations (8 dots exist)
**Already implemented** (compile.zig:5841):
- `defgeneric` - generic function definition
- `defmethod` - basic method with specializers
- Multi-method dispatch (type-based)
- `make-instance` with keyword args
- `slot-value` access

**Still missing**:
- `:before`/`:after`/`:around` method qualifiers parsing
- `call-next-method` within qualified methods
- `define-method-combination` macro
- Method qualifier storage in GenericFunction
- Standard method combination dispatch ordering
- `no-applicable-method`, `no-next-method`

**Files**: `src/compiler/compile.zig:5841-5940`

#### 1.3 Condition System (8 dots exist)
**Already implemented** (compile.zig:3867-4100):
- `signal` - signal condition
- `handler-case` - catch conditions by type
- `handler-bind` - dynamic handler binding
- `restart-case` - establish restarts
- `invoke-restart` - invoke by name
- `find-restart` - lookup restart

**Still missing**:
- `define-condition` macro (expands to defclass with condition-class metaclass)
- `make-condition` primitive
- Condition hierarchy (conditions inherit from CLOS, like SBCL/CCL)
- Base condition class with slots: format-control, format-arguments
- `warn` with *error-output*
- `cerror` with continue restart
- `break` with interactive debugger hook
- `invoke-restart-interactively`
- Standard restart functions: `abort`, `continue`, `muffle-warning`, `store-value`, `use-value`

**Files**: `src/compiler/compile.zig:3867-4100`, `stdlib.habu`

#### 1.4 Pathname System (7 dots exist)
**Already implemented** (objects.zig:451):
- `Pathname` object type with all CL fields (host, device, directory, name, type, version)
- Value tagging for pathname type

**Still missing**:
- `make-pathname` - constructor with keyword args
- `pathname` - coerce to pathname
- Accessors: `pathname-host`, `pathname-device`, `pathname-directory`, `pathname-name`, `pathname-type`, `pathname-version`
- `merge-pathnames` - merge with defaults
- `parse-namestring`, `namestring` - string conversion
- `truename`, `probe-file` - filesystem queries
- `file-author`, `file-write-date` - file metadata
- `#P"..."` reader macro
- Logical pathnames (low priority)

**Files**: `src/runtime/objects.zig:451`, `src/runtime/primitives/io.zig`

### Phase 2: Type System (10 dots exist)
**Already implemented** (compile.zig:6992):
- `typep` - basic type membership (primitives only)
- `type-of` - return type symbol
- `deftype` - ADT-style type definitions (Habu extension)

**Still missing for CL compliance**:
- `subtypep` - subtype relationship
- Compound type specifiers: `(and t1 t2)`, `(or t1 t2)`, `(not t)`
- `satisfies` type specifier: `(satisfies predicate)`
- `member`/`eql` type specifiers: `(member a b c)`, `(eql x)`
- Array type specifiers: `(array element-type dimensions)`
- Numeric range types: `(integer 0 10)`, `(float 0.0 1.0)`
- Function type specifiers: `(function (arg-types) return-type)`
- `coerce` to arbitrary types

**Files**: `src/types/`, `src/compiler/compile.zig:6992`

### Phase 3: Declarations (7 dots exist)

- `declare` special form (currently no-op)
- `declaim`/`proclaim` for global declarations
- `type` declaration with runtime checking
- `ignore`/`ignorable` - suppress warnings
- `special` - dynamic binding
- `inline`/`notinline` - inlining hints
- `dynamic-extent` - stack allocation hints
- `optimize` with `speed`, `safety`, `debug`, `space`, `compilation-speed`

**Files**: `src/compiler/compile.zig`

### Phase 4: Print Control (8 dots exist)

Variables to implement:
- `*print-case*` - :upcase/:downcase/:capitalize
- `*print-circle*` - circular structure detection
- `*print-escape*` - readably vs aesthetically
- `*print-readably*` - strict readable output
- `*print-length*`/`*print-level*` - truncation
- `*print-base*`/`*print-radix*` - number base
- `*print-gensym*` - #: prefix for uninterned
- `*print-array*` - array printing style

Functions:
- `write` with keyword args
- `prin1`/`princ`/`print`
- `pprint` - Full XP pretty printer (Waters' algorithm)
  - `pprint-logical-block`, `pprint-newline`, `pprint-indent`, `pprint-tab`
  - `pprint-fill`, `pprint-linear`, `pprint-tabular`
  - `*print-pprint-dispatch*`, `set-pprint-dispatch`

**Files**: `src/interp/vm.zig`, `stdlib.habu`

### Phase 5: Reader Macros (5 dots exist)

- `#S(struct-name ...)` - structure literals
- `#A(...)` - array literals
- `#P"..."` - pathname literals
- `#C(real imag)` - complex literals (enhance)
- `#*101` - bit-vector literals
- **Runtime readtable**: `set-macro-character`/`get-macro-character` work at runtime
- `*readtable*` special variable
- `copy-readtable`, `readtable-case`

**Files**: `src/reader/parser.zig`, `src/reader/lexer.zig`, `src/interp/vm.zig`

### Phase 6: Defmacro Destructuring (4 dots exist)

- Nested parameter destructuring: `(defmacro foo ((a b) c) ...)`
- `&whole` parameter
- `&environment` parameter
- Recursive pattern matching in lambda lists

**Files**: `src/compiler/compile.zig`

### Phase 7: Compiler/Evaluator (6 dots exist)

- `compile` - compile function to native code
- `compile-file` - compile source file
- `load` with `:external-format`
- FASL format: Habu-specific binary (bytecode + constants serialized)
- `disassemble` - show compiled code (Habu-specific output format)
- `macroexpand-1` vs `macroexpand`

**Files**: `src/compiler/`, `src/bytecode/`

### Phase 8: Multiple Values (4 dots exist)

- `values-list` - convert list to multiple values
- `multiple-value-call` - call with unpacked values
- `multiple-value-prog1` - return first form's values

**Files**: `src/compiler/compile.zig`, `src/interp/vm.zig`

### Phase 9: Package System (8 dots exist)

- `do-symbols`/`do-external-symbols`/`do-all-symbols`
- `find-symbol` - lookup with status
- `unintern` - remove symbol
- `import`/`shadowing-import`
- `shadow` - create shadowing symbol
- `apropos`/`apropos-list`
- `describe` - object description

**Files**: `src/runtime/`, `src/compiler/compile.zig`

### Phase 10: Stream Enhancements (6 dots exist)

- `clear-input`/`clear-output` - flush buffers
- `stream-element-type`
- `interactive-stream-p`
- `open-stream-p`
- Broadcast/concatenated/echo/synonym/two-way streams

**Files**: `src/runtime/primitives/io.zig`

### Phase 10.5: Full MetaObject Protocol (NEW - Critical)
Required for full CL compliance per user requirement.

**Architecture** (following SBCL approach):
- Class metaobjects are heap-allocated CLOS instances (metaclass: `standard-class`)
- Global `*class-table*` hash table maps names → class objects
- This allows classes to be garbage collected if unreferenced
- Best for long-term performance: single indirection, GC-friendly

**Class Introspection**:
- `class-of` - return class metaobject
- `class-name`, `class-direct-superclasses`, `class-precedence-list`
- `class-direct-slots`, `class-slots`
- `class-direct-subclasses`
- `class-direct-methods`

**Slot Introspection**:
- `slot-definition-name`, `slot-definition-initform`
- `slot-definition-initargs`, `slot-definition-readers`, `slot-definition-writers`
- `slot-definition-allocation`, `slot-definition-type`
- `slot-boundp`, `slot-makunbound`

**Generic Function Introspection**:
- `generic-function-name`, `generic-function-methods`
- `generic-function-lambda-list`
- `generic-function-method-class`
- `method-qualifiers`, `method-specializers`
- `method-function`, `method-generic-function`

**Metaobject Protocol**:
- `allocate-instance`, `initialize-instance`, `reinitialize-instance`
- `shared-initialize`
- `make-instances-obsolete`
- `update-instance-for-redefined-class`
- `update-instance-for-different-class`
- `change-class`

**Metaclasses**:
- `standard-class`, `funcallable-standard-class`
- `built-in-class`, `structure-class`
- `forward-referenced-class`
- `validate-superclass`

**Files**: `src/compiler/compile.zig`, `src/runtime/objects.zig` (add Class metaobject)

### Phase 10.6: String Migration to UTF-32 (NEW)
Current strings are byte-based (`[]u8`). For CL compliance with O(1) `char` access:

- Add `Character` type as 32-bit Unicode codepoint
- Migrate `String` from `[*]u8` to `[*]u32`
- Update all string primitives (string-length returns characters, not bytes)
- Add `base-string` type for ASCII-only strings (optimization)
- Update reader to handle Unicode properly
- Ensure `char`, `schar` are O(1)

**Files**: `src/runtime/objects.zig:106`, `src/runtime/primitives/str.zig`

### Phase 10.7: Thread Safety (NEW)
Add basic thread safety for future bordeaux-threads:

- Mutex protection for global symbol table
- Atomic operations for reference counting (if added)
- Thread-local dynamic bindings stack
- GC safe points

**Files**: `src/runtime/heap.zig`, `src/runtime/gc.zig`, `src/interp/vm.zig`

### Phase 10.8: GC Extensions (NEW)
Advanced GC features for CL compliance:

**Finalizers**:
- `make-finalizer`, `cancel-finalization`
- Run cleanup code when object collected
- Weak finalization queue

**Weak References**:
- `make-weak-pointer`, `weak-pointer-value`
- References that don't prevent collection

**Ephemerons**:
- Key-value pairs where value kept alive only if key is
- Useful for weak hash tables

**Weak Hash Tables**:
- `:weakness :key`, `:weakness :value`, `:weakness :key-and-value`, `:weakness :key-or-value`

**Files**: `src/runtime/gc.zig`, `src/runtime/objects.zig`

### Phase 10.9: Full CFFI (NEW)
Foreign Function Interface for C interop:

**Type System**:
- `:int`, `:long`, `:float`, `:double`, `:pointer`, `:void`
- `:struct`, `:union`, arrays
- `defctype`, `defcstruct`, `defcunion`

**Function Calls**:
- `foreign-funcall`, `defcfun`
- Calling convention support

**Memory**:
- `foreign-alloc`, `foreign-free`
- `mem-ref`, `mem-aref`
- `with-foreign-pointer`

**Callbacks**:
- `defcallback`, `callback`
- Lisp functions callable from C

**Library Loading**:
- `load-foreign-library`, `define-foreign-library`
- Platform-specific library paths

**Files**: NEW `src/ffi/` directory

### Phase 10.10: Environment & Compiler Introspection (NEW)

**Environment Objects**:
- Full lexical environment access
- `macroexpand-1` with env parameter
- `variable-information`, `function-information`, `declaration-information`
- `augment-environment`

**Compiler Features**:
- `compiler-let` - compile-time bindings
- `symbol-macrolet` - symbol macros
- `define-compiler-macro` - compiler optimization hints
- `compiler-macroexpand`
- `*compile-file-truename*`, `*compile-file-pathname*`

**Files**: `src/compiler/compile.zig`, `src/compiler/env.zig`

### Phase 10.11: Gray Streams (NEW)
Extensible stream protocol (de-facto standard):

- `fundamental-stream`, `fundamental-input-stream`, `fundamental-output-stream`
- `fundamental-character-stream`, `fundamental-binary-stream`
- Generic functions: `stream-read-char`, `stream-write-char`, `stream-read-byte`, etc.
- `stream-line-column`, `stream-start-line-p`
- `stream-fresh-line`, `stream-finish-output`, `stream-force-output`
- `stream-clear-input`, `stream-clear-output`

**Files**: `src/runtime/primitives/io.zig`, `stdlib.habu`

### Phase 10.12: Series Library (NEW)
Waters' series (lazy functional sequences):

- `series`, `scan`, `collect`
- `map-fn`, `iterate`, `previous`
- `until`, `until-if`, `positions`
- `choose`, `choose-if`, `expand`
- `catenate`, `subseries`
- Fusion optimization

**Files**: `stdlib.habu` (can be mostly pure Lisp)

### Phase 10.13: ASDF/Quicklisp Compatibility (NEW)
Build system and package management:

**ASDF**:
- `defsystem` - system definition
- Component types: `:file`, `:module`, `:system`
- Dependency tracking, load order
- `asdf:load-system`, `asdf:compile-system`

**Quicklisp**:
- `ql:quickload` - download and load libraries
- Local project support
- Quicklisp dist infrastructure (or alternative)

**Files**: `lib/asdf.habu`, `lib/quicklisp/`

### Phase 10.14: Debugging & Profiling (NEW)

**Tracing**:
- `trace`, `untrace` - function call tracing
- Show arguments and return values
- Nested trace indentation

**Stepping**:
- `step` - single-step execution
- Breakpoints at source locations
- Inspector integration

**Time & Profiling**:
- `time` macro - execution timing
- `room` - memory usage report
- Profiler hooks for sampling/instrumentation
- Call count profiling

**Inspect**:
- `inspect` - interactive object inspection
- Navigate object structure
- Modify slots interactively

**Files**: `src/interp/vm.zig`, `src/interp/debug.zig` (NEW)

### Phase 10.15: Documentation Strings (NEW)

**Storage**:
- `documentation` accessor for functions, variables, types, methods
- `(setf documentation)` to set docstrings
- Store in symbol plist or separate doc table

**Retrieval**:
- `describe` - full object description with docs
- `apropos` - search by name with docs
- SLIME/SLY integration for doc lookup

**Files**: `src/runtime/primitives/doc.zig` (NEW), `stdlib.habu`

### Phase 10.16: Enhanced REPL (NEW)

**History**:
- Readline-style history
- `*`, `**`, `***` - previous results
- `+`, `++`, `+++` - previous inputs
- `/`, `//`, `///` - previous forms
- `-` - current form being evaluated
- Persistent history file

**Completion**:
- Symbol completion (tab)
- Package-qualified completion
- Keyword argument completion

**Multi-line Input**:
- Detect incomplete expressions
- Continuation prompts
- Expression-aware editing

**Debugger Integration**:
- REPL-based debugger for `break`
- Restart selection
- Frame inspection
- Stepping in debug REPL

**Files**: `src/interp/repl.zig`

### Phase 10.17: SLIME/SLY Protocol (NEW)
Editor integration via Swank server:

**Core Protocol**:
- Swank RPC over socket
- `swank:connection-info`
- `swank:create-repl`
- `swank:listener-eval`

**Completion**:
- `swank:completions`, `swank:fuzzy-completions`
- `swank:operator-arglist`

**Documentation**:
- `swank:documentation-symbol`
- `swank:describe-symbol`
- `swank:apropos-list`

**Navigation**:
- `swank:find-definitions-for-emacs`
- `swank:xref` - cross-references

**Debugging**:
- `swank:backtrace`, `swank:frame-locals`
- `swank:invoke-nth-restart`
- `swank:eval-in-frame`

**Files**: NEW `src/swank/` directory

### Phase 10.18: Networking/Sockets (NEW)
usocket-style network I/O:

**TCP**:
- `socket-connect`, `socket-listen`, `socket-accept`
- `socket-close`, `socket-shutdown`
- Socket streams for read/write

**UDP**:
- `socket-send`, `socket-receive`

**DNS**:
- `get-host-by-name`, `get-host-by-address`

**Platform**:
- POSIX sockets on Unix
- Winsock on Windows

**Files**: NEW `src/net/` directory

### Phase 10.19: Image Save/Restore (NEW)
Save and restore Lisp image:

**Save**:
- `save-lisp-and-die` / `save-image`
- Serialize heap, symbols, packages, globals
- Optionally include executable (create standalone binary)

**Restore**:
- Load image on startup
- Initialize system after restore
- Handle toplevel function

**Hooks**:
- `*save-hooks*` - run before save
- `*init-hooks*` - run after restore

**Files**: `src/runtime/image.zig` (NEW)

### Phase 10.20: Additional CL Features (NEW)

**Multiple Float Types**:
- `short-float`, `single-float`, `double-float`, `long-float`
- Type-specific constants: `most-positive-single-float`, etc.
- `float` coercion with type argument
- `coerce` to specific float types

**Circular Structures**:
- `#=` and `##` reader macros for shared structure
- `*print-circle*` for circular printing
- Cycle detection in printer

**Displaced Arrays**:
- `:displaced-to` and `:displaced-index-offset` in `make-array`
- Arrays sharing storage
- `array-displacement` accessor

**Eval-when Contexts**:
- Full `:compile-toplevel`, `:load-toplevel`, `:execute` support
- Interaction with `compile-file`
- Macro expansion at compile time

**Fill Pointers**:
- `:fill-pointer` in `make-array`
- `fill-pointer`, `(setf fill-pointer)`
- `vector-push`, `vector-push-extend`

**Adjustable Arrays**:
- `:adjustable` in `make-array`
- `adjust-array` with all options
- `adjustable-array-p`

**Files**: `src/runtime/objects.zig`, `src/reader/parser.zig`, `src/compiler/compile.zig`

### Phase 10.21: Setf Expansions (NEW)

**Core**:
- `define-setf-expander` - define complex setf expansions
- `get-setf-expansion` - retrieve setf expansion
- `defsetf` - simple setf definitions

**Standard Places**:
- All accessor functions should have setf expansions
- `car`, `cdr`, `nth`, `aref`, `gethash`, `slot-value`, etc.

**Files**: `stdlib.habu`, `src/compiler/compile.zig`

### Phase 10.22: Lambda List Keywords (NEW)

**Full Support**:
- `&optional` with default and supplied-p
- `&rest` / `&body`
- `&key` with default and supplied-p
- `&allow-other-keys`
- `&aux` for auxiliary bindings
- `&whole` in macro lambda lists
- `&environment` in macro lambda lists

**Destructuring**:
- Nested destructuring in all lambda lists
- Pattern matching in function parameters

**Files**: `src/compiler/compile.zig`

### Phase 10.23: Sequence Keywords (NEW)

All sequence functions must support:
- `:test` - comparison function (default `eql`)
- `:test-not` - negated test (deprecated but required)
- `:key` - key extraction function
- `:start`, `:end` - subsequence bounds
- `:start1`, `:end1`, `:start2`, `:end2` - for two-sequence ops
- `:from-end` - process from end
- `:count` - limit number of operations

**Functions requiring these**:
- `find`, `position`, `count`, `remove`, `substitute`
- `search`, `mismatch`
- `reduce` (`:initial-value`, `:from-end`)
- `sort`, `stable-sort` (`:key`)

**Files**: `stdlib.habu`, `src/runtime/primitives/seq.zig`

### Phase 10.24: Character Names (NEW)

**Standard Names**:
- `#\Newline`, `#\Space`, `#\Tab`, `#\Return`, `#\Linefeed`
- `#\Page`, `#\Backspace`, `#\Rubout`

**Functions**:
- `char-name` - return name of character
- `name-char` - return character for name

**Semi-standard**:
- `#\Null`, `#\Escape`, `#\Delete`

**Files**: `src/reader/lexer.zig`, `src/runtime/primitives/char.zig`

### Phase 10.25: Additional Reader Syntax (NEW)

**Already implemented**: `#x`, `#b`, `#o` (hex, binary, octal)

**Missing**:
- `#nR` - arbitrary radix (e.g., `#3r12` = 5 in base 3)
- `#:symbol` - uninterned symbol
- `#n=` / `#n#` - circular structure (covered in 10.20)

**Files**: `src/reader/lexer.zig`, `src/reader/parser.zig`

### Phase 10.26: EQL Specializers (NEW)

CLOS support for:
- `(defmethod foo ((x (eql :keyword))) ...)` - dispatch on specific value
- EQL specializer parsing in defmethod
- EQL specializer in method dispatch

**Files**: `src/compiler/compile.zig` (defmethod)

### Phase 10.27: With-* Macros (NEW)

**Standard macros**:
- `with-standard-io-syntax` - bind I/O vars to standard values
- `with-compilation-unit` - group compilation units
- `with-package-iterator` - iterate over package symbols
- `with-accessors` - slot accessor bindings
- `with-slots` - slot value bindings

**Files**: `stdlib.habu`

### Phase 10.28: Full Defstruct (NEW)

Current defstruct is basic. Missing CL options:
- `:include` - inherit from another structure
- `:type` - list or vector representation (default is vector)
- `:named` - include type tag for typed structures
- `:print-function` / `:print-object` - custom printing
- `:copier` - generate copy function (default copy-NAME)
- `:conc-name` - prefix for accessors
- `:predicate` - custom predicate name or nil
- `:constructor` - custom constructor or BOA constructors

**Files**: `stdlib.habu:2739`

### Phase 11: Remaining ~300 Symbols

Categories to audit and implement:

#### Numbers (~50 symbols)
- `boole`, `boole-*` constants
- `byte`, `byte-size`, `byte-position`, `ldb`, `dpb`, `ldb-test`, `mask-field`, `deposit-field`
- `decode-float`, `scale-float`, `float-radix`, `float-sign`, `float-digits`, `float-precision`
- `integer-decode-float`
- `random`, `make-random-state`, `random-state-p`, `*random-state*`
- Constants: `most-positive-fixnum`, `most-negative-fixnum`, etc.

#### Characters (~25 symbols)
- `standard-char-p`, `graphic-char-p`
- `char-int`, `int-char` (deprecated but in spec)
- `char-name`, `name-char`
- Character constants

#### Sequences (~30 symbols)
- `map-into`, `reduce` enhancements
- `delete`/`delete-if`/`delete-if-not`/`delete-duplicates`
- `nsubstitute`/`nsubstitute-if`/`nsubstitute-if-not`
- `stable-sort`, `merge`

#### Arrays (~20 symbols)
- `adjust-array`, `adjustable-array-p`
- `bit` operations: `bit-and`, `bit-ior`, `bit-xor`, `bit-not`, `bit-eqv`, `bit-nand`, `bit-nor`, `bit-andc1`, `bit-andc2`, `bit-orc1`, `bit-orc2`
- `sbit`, `upgraded-array-element-type`

#### Structures (~10 symbols)
- `copy-structure`
- Structure :print-function/:print-object
- :include inheritance
- :type (list/vector representations)

#### Symbols (~15 symbols)
- `copy-symbol`
- `make-symbol`
- `symbol-package`
- `symbol-plist`
- `makunbound`, `fmakunbound`

#### Evaluation (~15 symbols)
- `constantp`
- `special-operator-p`
- `macro-function`, `compiler-macro-function`
- `*macroexpand-hook*`
- Environment objects

#### Misc (~20 symbols)
- `documentation`, `(setf documentation)`
- `inspect`
- `room`
- `time` macro
- `trace`/`untrace`
- `step`
- `dribble`
- `ed`
- `lisp-implementation-type`/`-version`
- `machine-instance`/`-type`/`-version`
- `software-type`/`-version`

## Implementation Order (36 phases)

1. **Phase 1** (High Priority) - LOOP, CLOS combinations, Conditions, Pathnames
2. **Phase 2** (Type System) - typep, subtypep, compound types
3. **Phase 3** (Declarations) - declare, declaim
4. **Phase 4** (Print Control) - *print-* variables, pprint/XP
5. **Phase 5** (Reader) - #S, #A, #P, #*, runtime readtable
6. **Phase 6** (Defmacro) - destructuring
7. **Phase 7** (Compiler) - compile, compile-file, FASL
8. **Phase 8** (Multiple Values) - values-list, mv-call
9. **Phase 9** (Packages) - do-symbols, apropos
10. **Phase 10** (Streams) - clear-*, stream types
11. **Phase 10.5** (MOP) - Full AMOP-compatible MetaObject Protocol
12. **Phase 10.6** (Strings) - UTF-32 migration, base-string
13. **Phase 10.7** (Threading) - Thread-safe core
14. **Phase 10.8** (GC) - Finalizers, weak refs, ephemerons
15. **Phase 10.9** (FFI) - Full CFFI
16. **Phase 10.10** (Introspection) - Environment objects, compiler macros
17. **Phase 10.11** (Gray Streams) - Extensible stream protocol
18. **Phase 10.12** (Series) - Lazy functional sequences
19. **Phase 10.13** (ASDF/Quicklisp) - Build system, package management
20. **Phase 10.14** (Debug/Profile) - trace, step, time, inspect
21. **Phase 10.15** (Docstrings) - documentation, describe
22. **Phase 10.16** (REPL) - History, completion, multi-line
23. **Phase 10.17** (SLIME/SLY) - Swank server for editors
24. **Phase 10.18** (Networking) - Sockets, usocket compatibility
25. **Phase 10.19** (Images) - save-image, standalone executables
26. **Phase 10.20** (Additional CL) - Floats, circular, displaced arrays, eval-when
27. **Phase 10.21** (Setf) - define-setf-expander, get-setf-expansion
28. **Phase 10.22** (Lambda Lists) - &aux, &allow-other-keys, full destructuring
29. **Phase 10.23** (Sequence Keywords) - :test :key :start :end :from-end everywhere
30. **Phase 10.24** (Char Names) - #\Newline, #\Space, char-name, name-char
31. **Phase 10.25** (Reader) - #nR radix, #:uninterned
32. **Phase 10.26** (EQL Specializers) - (eql value) in defmethod
33. **Phase 10.27** (With-* Macros) - with-standard-io-syntax, etc.
34. **Phase 10.28** (Defstruct) - :include, :type, :print-function, BOA
35. **Phase 11** (Remaining) - numbers, chars, sequences, arrays, etc.
36. **Phase 12** (Polish) - Run full ansi-test, fix failures

## Verification

1. **ANSI Test Suite**: Port ansi-test (Paul Dietz) - ~23k tests
   - Source: https://gitlab.common-lisp.net/ansi-test/ansi-test
   - Run: `./zig-out/bin/habu test/ansi/*.habu`
2. **Per-feature tests**: Each feature gets ohsnap/zcheck tests
   - Run: `zig build test`
3. **Integration tests**: Real CL programs (PAIP examples, etc.)
4. **Symbol audit**: Script to verify all 978 symbols exported
   ```bash
   # Verify symbol count
   ./zig-out/bin/habu -e "(length (package-symbols :cl))"
   # => 978
   ```

## Critical Files

- `src/compiler/compile.zig` - special forms, macros (~7500 lines)
- `src/interp/vm.zig` - primitives, printer (~3000 lines)
- `src/runtime/objects.zig` - object types (pathname, condition)
- `src/runtime/primitives/*.zig` - primitive implementations
- `src/reader/parser.zig` - reader macros (#S, #A, #P, #*)
- `stdlib.habu` - library functions/macros (2813 lines)
- `docs/cl-symbols.md` - NEW: complete symbol tracking

## Concrete Next Steps

### Step 1: Create Symbol Audit (Day 1)
```bash
# Create docs/cl-symbols.md with all 978 symbols
# Mark each as ✓/⚠/✗ with location
```

### Step 2: LOOP Enhancements (Days 2-4)
1. Add `append`/`nconc`/`minimize`/`maximize` handling to case in `stdlib.habu:2554`
2. Add `named` clause support
3. Add `always`/`never`/`thereis` termination
4. Add `being hash-keys/values` iteration
5. Add loop conditional (`if`/`when`/`unless`)
6. Test each clause independently

### Step 3: Method Combinations (Days 5-8)
1. Modify `GenericFunction` struct in compile.zig to store qualifier
2. Parse `:before`/`:after`/`:around` in defmethod
3. Implement standard method combination dispatch
4. Add `call-next-method` special form

### Step 4: Condition Enhancement (Days 9-12)
1. Add `define-condition` macro to stdlib.habu
2. Implement condition object type (use defclass internally)
3. Add `warn`, `cerror`, `break`
4. Add standard restarts

### Step 5: Pathname Primitives (Days 13-16)
1. Add `make-pathname` primitive in io.zig
2. Add accessor primitives (pathname-host, etc.)
3. Add `merge-pathnames`, `parse-namestring`
4. Add `#P` reader macro

### Ongoing: Update cl-symbols.md after each feature

## Dot Decomposition

After plan approval, decompose each phase into small dots (<30 min each) using `/small-dots`.

Example decomposition for Phase 1.1 (LOOP):
```
habu-loop-append-nconc: Add append/nconc accumulation to loop
habu-loop-minimize-maximize: Add minimize/maximize accumulation
habu-loop-named: Add named clause for block naming
habu-loop-always-never: Add always/never/thereis termination
habu-loop-being-hash: Add for/being hash-keys/values iteration
habu-loop-being-symbols: Add for/being symbols iteration
habu-loop-conditional: Add if/when/unless in loop body
habu-loop-destructuring: Add iteration destructuring
```

Run `/small-dots` for each phase to create the full dot tree.

### Step 0: Link Plan to AGENTS.md
Add to AGENTS.md under "Next Steps":
```markdown
**Phase 6: ANSI CL Spec Parity**
- Plan: `~/.claude/plans/steady-jingling-seal.md`
- Target: 978 CL symbols + full MOP
- Tracking: `docs/cl-symbols.md`
```

## Remaining parity gaps (11 ✗ + 10 ⚠)

### Active objective
Close the remaining missing/partial symbols tracked in `docs/cl-symbols.md` by implementing:
- setf expander API (`get-setf-expansion`, `define-setf-expander`)
- macro lambda keywords (`&whole`, `&environment`, `&allow-other-keys`)
- method-combination helpers (`call-method`, `make-method`, `invalid-method-error`, `method-combination-error`)
- logical pathname translation loading (`load-logical-pathname-translations`)
- actionable `OPTIMIZE` behavior (safety=0 suppresses assertions)

### Dot tree (execution plan)
- `habu-cl-spec-parity-6821074c`
  - `habu-add-get-setf-f354d451`
    - `habu-add-setf-tests-c67c0876`
  - `habu-finish-method-combinations-b5f58029`
    - `habu-add-method-helpers-562d82b3`
    - `habu-add-method-tests-8946ed7a`
  - `habu-add-logical-pathname-52bc8dee`
    - `habu-add-logical-path-f29f4d16`
    - `habu-load-logical-path-afb281aa`
    - `habu-add-logical-path-cff91dd2`
  - `habu-honor-declarations-optimize-d6626aae`
    - `habu-parse-optimize-85c7bb89`
    - `habu-honor-safety-3f155793`
    - `habu-add-optimize-docs-cd7fd297`
  - `habu-reconcile-tracking-873402a0`

### Concrete implementation checkpoints
1. Complete setf expander snapshots and custom setf integration tests.
2. Implement missing method-combination helper symbols in `lib/stdlib.habu`.
3. Implement logical pathname translation store + loader + tests.
4. Parse `optimize` declarations into compiler state and honor safety level for assertion emission.
5. Reconcile `docs/cl-symbols.md`, `docs/PROGRESS.md`, and `docs/cl-spec-status.md` counts/status.

### Verification gates
- Run `zig build test` before finishing each dot that changes tracked files.
- Use `tools/dot-finish <dot-id> -m "..."` to test, commit, push, close dot, and open next change.
