# Habu ARM64 Codegen and JIT Integration

This document describes how the **native Habu (stage2) compiler** currently generates ARM64 code, and how to layer a multi-version JIT on top of the existing architecture *without* fighting it.

It is based on the actual sources in this repository, in particular:

- `arm64/asm.lisp` – standalone ARM64 assembler / encoder
- `arm64/codegen.lisp` – pure-Habu ARM64 code generator
- `native-compiler-main.lisp` / `compiler-driver.lisp` – compiler entry points
- `runtime/runtime.c`, `runtime/habu.h` – runtime ABI that codegen targets
- `docs/codegen/ARM64_COMPILER_README.md`, `docs/codegen/ARM64_COMPILER_COMPLETE.md` – status docs

No speculative backends (LLVM/Cranelift) are used here; everything is expressed in terms of **Habu’s own ARM64+GC runtime.**

---

## 1. What the stage2 ARM64 codegen actually does

### 1.1 `arm64/asm.lisp`: standalone assembler

`arm64/asm.lisp` defines the `:arm64` package and a small API:

- `encode` – core instruction encoder
- `reg` / `num-to-reg` – keyword/number → register mapping
- A large set of helpers for individual instructions, e.g.
  - `movz`, `movk`, `mov-reg`
  - `ldr-offset`, `str-offset`
  - `blr`, `br`
  - conditional branches, arithmetic/logical ops, etc.

Key traits:

- It is completely **self-contained** and works both in SBCL and native Habu.
- Instructions are represented as **lists of 32-bit words** (or bytes), and helpers like `count-instrs` are used heavily in `codegen.lisp` to compute PC-relative offsets.
- There is no IR-level type information here; it is a pure encoding layer.

### 1.2 `arm64/codegen.lisp`: Habu IR → ARM64

`arm64/codegen.lisp` is the heart of the stage2 backend. It:

- Defines stack frame layout constants:

  ```lisp
  (setq *stack-frame-size* #xFF0)
  (setq *env-base-offset*  #x180)
  (setq *temp-slot-base*   #x40)
  (setq *temp-slot-guard*  #x180)
  (setq *arg-spill-base*   #x200)
  (setq *arg-spill-stride* #x8)
  ```

- Maintains global state:

  ```lisp
  (setq *runtime-addrs* nil)
  (setq *collected-lambdas* nil)
  ```

- Exposes a main entry point (by convention) that walks a simple tagged IR and calls `codegen-expr` to produce lists of ARM64 instructions.

The **IR is a tagged S-expression**, not a class hierarchy. `codegen-expr` pattern matches on tags such as:

- Literals / variables:

  - `(lit <int>)` – tagged fixnum literal  
  - `(var <slot>)` – load from stack slot  

- Structured literals:

  - `(string-lit chars...)`
  - `(symbol-lit name)`
  - `(vector-lit ...)`

- Calls:

  - `(call-closure fn-ir arg-irs...)` – closes over environment, then calls via closure layout
  - `(call-fn name arg-irs...)` – direct call to known function

- Control / others:

  - Branch-like forms (`if`, loops) are lowered using `labels` in `codegen-expr` to assemble blocks.

The generated sequence is then fed through:

- A **frame builder** / prologue-epilogue generator
- The ARM64 assembler (`arm64/asm.lisp`) to serialize instructions to bytes
- The runtime’s JIT code loader (CFI / Mach-O and macOS JIT entitlements for code pages on Apple, see `docs/runtime/MACOS_JIT_FIX.md`)

### 1.3 Calling convention and runtime ABI

From `arm64/codegen.lisp` and the runtime headers:

- Habu uses a straightforward ARM64 C-like calling convention:
  - Arguments are passed in `x0`–`x7` (with overflow in a spill area at `*arg-spill-base*`).
  - The closure environment pointer is stored in `x24` (see `(mov-reg 24 10)` in the `call-closure` case).
  - The code pointer for a closure is loaded from offset `8` in the closure object, then called via `blr x11`.
- The code generator relies on `*runtime-addrs*` to know where runtime entry points live, e.g.:
  - Allocation functions for cons cells, vectors, strings, etc.
  - Primitives such as `habu_car`, `habu_cdr`, `habu_setcar`, etc.
- The **stack frame layout** is fixed-size for now (`*stack-frame-size*` is a constant), with:
  - Temp slots at `*temp-slot-base*`
  - Environment cells at `*env-base-offset*`
  - Argument spill area at `*arg-spill-base*`

This is a classic **baseline native compiler**: one IR, one codegen pass, no dynamic specialization yet.

---

## 2. How to layer a JIT on top of this design

The good news: the current stage2 compiler already behaves like a JIT-able backend — it takes a simple IR and produces ARM64 lists that are then turned into executable pages. We can **reuse almost all of this** and just add *runtime multi-versioning* and *IR hints*.

### 2.1 Keep the existing IR, add “rep” hints

Rather than invent a new IR, we can extend the existing tagged IR with optional representation hints. For example:

- Current form:

  ```lisp
  (call-fn 'add (list arg1-ir arg2-ir))
  ```

- With rep hints:

  ```lisp
  (call-fn :rep '(:fixnum :fixnum)
           'add
           (list arg1-ir arg2-ir))
  ```

Or equivalently, add an annotation pass that records, outside the IR, a mapping:

```lisp
;; (fn-name . ((arg-index . rep) ...))
'((add . ((0 . :fixnum) (1 . :fixnum)))
  (mul . ((0 . :fixnum) (1 . :fixnum))))
```

Recommendations:

- **Don’t** change `codegen-expr`’s core pattern matching yet.
- Instead, add a small helper `arg-rep-hint` that, given `(call-fn ...)` and an index, returns a rep if known (`:fixnum`, `:double`, `:tagged`, etc.).
- In `codegen-expr`’s `call-fn` and `call-closure` cases, use those hints to choose better code paths (e.g., assume fixnums and skip tag checks where safe).

This keeps the IR stable while allowing a specializing compile path to feed extra information to codegen.

### 2.2 Introduce `function-object` + `function-version` structs

At the runtime level (in Lisp), we can introduce lightweight JIT metadata without disturbing existing closure layout:

```lisp
(defstruct function-version
  (rep-key  nil)   ; e.g. '(:fixnum :fixnum)
  (code     nil)   ; code pointer / FFI object
  (hotness  0))    ; how often this version has been called

(defstruct function-object
  name
  env                   ; closure env pointer or Habu object
  generic-code          ; baseline code pointer
  (versions '()))       ; list of FUNCTION-VERSION
```

Integration points:

- Wherever the native compiler currently produces a “function” (e.g. `native-compiler-main.lisp` or `compiler-driver.lisp`), instead of returning just a raw code pointer, wrap it in a `function-object`.
- Keep the **closure representation on the C side unchanged**; this struct lives in Habu’s Lisp heap (or CL heap during bootstrap) and is only used by the JIT dispatcher.

### 2.3 Dispatcher: connect calls to the right version

Calls currently lower to `(call-closure ...)` and `(call-fn ...)` patterns inside `codegen-expr`. There are two strategies:

1. **Lisp-level dispatcher first, ARM64 stub later**

   - Implement a Lisp function `call-function` that:
     1. Computes a `rep-key` from argument objects (e.g. `(:fixnum :fixnum)`).
     2. Looks up a matching `function-version` in a given `function-object`.
     3. If found, calls its `code`.
     4. If not, calls `generic-code` and maybe triggers JIT specialization for that `rep-key`.

   - In the interim, `codegen.lisp` can be left alone: the native code just calls into `call-function` (via FFI) with a pointer to the `function-object` and arguments.

2. **Eventually: ARM64 entry stub**

   - For hot functions, generate a small **entry stub** in ARM64 that:
     - Loads type tags for arguments (e.g. from object headers or low bits).
     - Compares against the most common `rep-key` pattern(s).
     - Jumps directly to the specialized code if match; otherwise jumps to generic.
   - This stub can be emitted by a new backend function, e.g. `(emit-arm64-dispatch-stub function-object ...)`, and inserted before or around the existing prologue.

Initial recommendation: **start with the Lisp-level dispatcher**, because:

- It requires **no changes** to `arm64/asm.lisp` or low-level ARM64 code.
- It keeps all JIT policy logic in Lisp, which is easier to evolve.
- Once proven, critical paths can be ported into a proper ARM64 stub.

### 2.4 Specializing recompile path

To compile a specialized version, Habu needs to be able to:

1. Retrieve the “original function IR” for a given function name or closure.
2. Annotate it with **argument representation assumptions** based on the `rep-key`.
3. Run the existing native compiler with those hints.
4. Install the resulting code in a new `function-version` entry.

In practice this means:

- Ensure the native compiler keeps around an IR or AST for each top-level function (e.g. in a global table mapping function names to parsed forms or lowered IR).
- Add a simple API like:

  ```lisp
  (defun compile-specialized (fn-name rep-key)
    ;; 1. look up IR or source for fn-name
    ;; 2. attach type/rep hints for arguments from rep-key
    ;; 3. call through the normal arm64 pipeline to produce code
    ;; 4. wrap result in a FUNCTION-VERSION and return it
    )
  ```

- In the dispatcher, when a `rep-key` becomes hot for a given function, call `compile-specialized` and push the resulting version into the function object.

This uses the **same backend you already trust** (`arm64/codegen.lisp` + assembler), just with more information on the IR side.

---

## 3. Changes from earlier generic JIT suggestions

Given the actual code:

- We **do not** need or want a separate “generic backend” or LLVM/Cranelift. The ARM64 backend is already solid and used everywhere.
- We should **not** talk about x86-64 in the stage2 compiler context; the stage2 branch is clearly ARM64-centric (`arm64/asm.lisp`, `arm64/codegen.lisp`, `native/arm64-asm.lisp`, and `docs/codegen/ARM64_*`).
- All JIT work should **reuse**:
  - the existing IR tagging scheme (e.g. `'lit`, `'var`, `'call-closure`, `'call-fn`, etc.)
  - the existing stack frame layout
  - the existing runtime ABI in `runtime/runtime.c` and headers.

The main updates relative to previous, more speculative design:

1. **No new IR is required.** Just add metadata and/or attributes to the existing tagged IR forms.
2. **No generic multi-backend abstraction is necessary yet.** Focus on ARM64; if x86-64 or other targets reappear later, the JIT design can be re-evaluated.
3. **Function versioning and dispatch can live entirely in Lisp at first**, reflecting the “self-hosting” goal: Habu specializes itself using its own compiler.
4. The “JIT” in this phase is less about “compile during execution” (you already effectively do that) and more about “compile more than one version of a function and choose intelligently at runtime.”

---

## 4. Concrete patch sketch (at a high level)

The accompanying patch file proposes **only documentation additions**, to avoid touching core compiler files prematurely:

- `docs/codegen/HABU_ARM64_JIT_ARCH.md` – contains a repo-local version of this document, under the `docs/codegen/` tree that already exists.
- Stubs for JIT-related structs and dispatcher functions can be added later under `native/` or a new `jit/` directory once you decide where you want JIT metadata to live (runtime vs. compiler side).

Once you are happy with the design, the next patch iteration can:

- Introduce `function-object` and `function-version` structs in a concrete Habu module.
- Patch `native-compiler-main.lisp` or `compiler-driver.lisp` to construct these objects instead of returning raw code pointers.
- Add a minimal Lisp-level dispatcher that calls into the existing ARM64 code.

This keeps the first change-set **low risk** and focused on clarifying the architecture in the repo itself.
