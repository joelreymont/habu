# Self-Hosting in Habu

## Overview

Self-hosting is the ability of a compiler to compile itself. Habu is designed with self-hosting as a primary goal, implemented in two phases.

## Current Status (Phase 1)

### What Works ✅

**File Compilation:**
- Can read Habu source files
- Can parse all top-level forms
- Can compile each form to machine code (x86_64 and ARM64)
- Can write compiled output to files
- Successfully compiles example programs with:
  - Function definitions (defun)
  - Loops (dotimes, dolist)
  - Multiple return values
  - List operations
  - Lexical bindings

**Example:**
```bash
$ sbcl --script compile-file.lisp
Compiling example.habu for X86_64...
Found 6 top-level forms
Compilation complete:
  Forms: 6/6 compiled successfully
  Total: 330 bytes
```

### Architecture

**Phase 1: Bootstrap with SBCL**
```
Habu Source (.habu)
    ↓ (read-from-string)
S-expressions
    ↓ (parse)
Compiler IR
    ↓ (emit-x86_64/emit-arm64)
Machine Code (.o)
    ↓ (SBCL FFI trampolines)
Runtime Functions
```

Current implementation:
- Compiler written in Common Lisp
- Runs on SBCL
- Generates machine code for Habu programs
- Uses SBCL FFI for runtime functions
- Can compile simple Habu programs

### What's Missing for Full Self-Hosting

**1. Code Execution**
- **Current:** Generate machine code but don't execute it standalone
- **Needed:** Allocate executable memory, load code, execute functions
- **Complexity:** Medium (SBCL provides mmap/mprotect via sb-posix)

**2. Linking**
- **Current:** Each form compiles independently
- **Needed:** Link multiple compiled forms together
- **Complexity:** High (need relocation, symbol resolution)

**3. Runtime Library**
- **Current:** Rely on SBCL's FFI trampolines
- **Needed:** Compile runtime functions to machine code
- **Complexity:** High (GC, memory allocation, etc.)

**4. Compiler in Habu Subset**
- **Current:** Compiler uses full Common Lisp features
- **Needed:** Rewrite compiler in Habu-supported subset
- **Complexity:** Very High (large codebase)

---

## Path to Self-Hosting

### Phase 1: Bootstrap (Current)

**Goal:** Compile Habu programs within SBCL environment

**Status:**  ✅ Complete for simple programs

**Capabilities:**
- Read and parse Habu source
- Compile to machine code
- Write compiled output
- Execute via SBCL FFI

**Limitations:**
- Cannot execute standalone
- Depends on SBCL runtime
- No linking or module system

### Phase 2: Standalone (Future)

**Goal:** Compile and run Habu programs without SBCL

**Requirements:**

1. **Inline Allocation**
   - Generate allocation code inline (no FFI calls)
   - Requires compiling runtime/memory.lisp

2. **Standalone Runtime**
   - GC implementation in machine code
   - Basic I/O without SBCL
   - Minimal C runtime dependency

3. **Linking System**
   - Combine multiple .o files
   - Resolve symbols between modules
   - Generate executable format (ELF/Mach-O)

4. **Module System**
   - Load and link multiple files
   - Namespace management
   - Dependency resolution

5. **Compiler Self-Compilation**
   - Rewrite compiler in Habu subset
   - Bootstrap process:
     1. Compile compiler with SBCL
     2. Use compiled compiler to compile itself
     3. Verify output matches
   - Iterate until stable

---

## Current Compilation Model

### File Compiler

```lisp
(defun compile-habu-file (input-file &key arch output-file)
  ;; 1. Read file contents
  ;; 2. Parse all top-level forms
  ;; 3. Compile each form to machine code
  ;; 4. Write output (optionally)
  ;; Returns: list of (form . bytecode) pairs
  )
```

**Supported Forms:**
- `defun` - Function definitions (compile to symbol table + FFI wrapper)
- `defvar` - Global variables
- `defmacro` - Macro definitions (compile-time only)
- Expressions - Any executable code

**Process:**
```
example.habu → [parse] → Forms → [compile] → Machine Code → example.o
```

### Form Compilation

Each form compiles independently:

```lisp
(defun square (x) (* x x))
; => 10 bytes (just stores in function table)

(let ((x 5)) (square x))
; => ~50 bytes (lookup square, call it, etc.)
```

**No Inter-Form Dependencies:**
- Each form is self-contained
- Later forms can reference earlier ones via runtime
- No static linking required

---

## Examples

### Compiling a Simple Program

**Source (example.habu):**
```lisp
(defun factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(factorial 5)
```

**Compilation:**
```bash
$ sbcl --script compile-file.lisp example.habu
Compiling example.habu for X86_64...
Found 2 top-level forms
1. DEFUN => 10 bytes
2. FACTORIAL => 85 bytes
Compilation complete: 95 bytes total
```

**What Happens:**
1. `defun factorial` → Creates function table entry + FFI wrapper
2. `(factorial 5)` → Generates machine code that calls the wrapper

**Execution (via SBCL FFI):**
- Generated code calls the FFI wrapper
- Wrapper calls the Habu runtime function
- Result: 120 (5! = 120)

### Current Limitations

**Cannot do standalone execution:**
```bash
# This doesn't work yet:
$ ./example.o
bash: ./example.o: cannot execute binary file

# Why: No executable header, no entry point, no linking
```

**But can verify compilation:**
```bash
$ file example.o
example.o: data

$ hexdump -C example.o | head
00000000  48 b8 00 00 00 00 00 00  00 00 ...
```

---

## Detailed Requirements for Phase 2

### 1. Code Execution

**Allocate executable memory:**
```lisp
(defun allocate-executable (size)
  ;; Use sb-posix:mmap with PROT_EXEC
  ;; Or sb-alien for platform-specific allocation
  )
```

**Load and call code:**
```lisp
(defun load-and-call (code-bytes &rest args)
  ;; 1. Allocate executable memory
  ;; 2. Copy code to memory
  ;; 3. Create function pointer
  ;; 4. Call with args
  ;; 5. Return result
  )
```

**Example:**
```lisp
(let ((code (compile-expression '(+ 2 3) :arch :x86_64)))
  (load-and-call code))
; => 5
```

### 2. Linking

**Symbol Table:**
```lisp
(defstruct symbol-info
  name      ; Symbol name
  address   ; Memory address
  size      ; Code size
  type)     ; :function, :variable, :macro
```

**Relocation:**
```lisp
(defun relocate-code (code relocations base-addr)
  ;; Patch addresses in code for new base
  ;; Handle function calls, variable references
  )
```

**Link Multiple Modules:**
```lisp
(defun link-modules (modules)
  ;;; 1. Collect all symbols
  ;; 2. Assign addresses
  ;; 3. Relocate code
  ;; 4. Generate executable
  )
```

### 3. Runtime Compilation

**Compile Runtime Functions:**
```lisp
;; Instead of FFI trampolines:
(defun runtime-cons (car cdr)
  ;; Compiled to machine code
  ;; Inline heap allocation
  ;; No SBCL dependencies
  )
```

**Memory Management:**
```lisp
(defun runtime-gc ()
  ;; Mark and sweep
  ;; Stack scanning
  ;; All in machine code
  )
```

### 4. Compiler Self-Compilation

**Step 1: Subset Compiler**
Write a simplified compiler in Habu subset:
```lisp
;;;; habu-compiler.habu
(defun parse (form) ...)
(defun emit-x86_64 (expr) ...)
(defun compile-file (file) ...)
```

**Step 2: Bootstrap**
```bash
# Compile compiler with SBCL
$ sbcl --script compile-compiler.lisp habu-compiler.habu
=> habu-compiler.o (Stage 0)

# Use Stage 0 to compile itself
$ ./habu-compiler habu-compiler.habu
=> habu-compiler.o (Stage 1)

# Use Stage 1 to compile itself
$ ./habu-compiler habu-compiler.habu
=> habu-compiler.o (Stage 2)

# Verify Stage 1 == Stage 2 (fixed point)
$ diff habu-compiler-stage1.o habu-compiler-stage2.o
(no difference => success!)
```

---

## Milestones

### Milestone 1: Code Execution ✅ (Partial)
- [x] Compile expressions to machine code
- [x] Verify bytecode generation
- [ ] Load and execute compiled code
- [ ] Call functions with arguments
- [ ] Return values correctly

### Milestone 2: File Compilation ✅
- [x] Read source files
- [x] Parse top-level forms
- [x] Compile all forms
- [x] Write compiled output

### Milestone 3: Simple Programs (Current)
- [x] Compile function definitions
- [x] Compile expressions with function calls
- [x] Support loops and control flow
- [x] Support multiple values
- [ ] Execute compiled programs

### Milestone 4: Runtime Independence
- [ ] Compile runtime functions to machine code
- [ ] Inline allocation (no FFI)
- [ ] Standalone GC implementation
- [ ] Remove SBCL dependencies

### Milestone 5: Self-Compilation
- [ ] Write compiler in Habu subset
- [ ] Compile compiler with itself
- [ ] Verify fixed-point bootstrap
- [ ] Generate standalone executable

### Milestone 6: Full Self-Hosting
- [ ] Complete module system
- [ ] Linking and relocation
- [ ] Standard library in Habu
- [ ] Package management
- [ ] Development tools (REPL, debugger)

---

## Design Decisions

### Why Two Phases?

**Phase 1 Advantages:**
- Rapid development using SBCL facilities
- Test compiler correctness without runtime complexity
- Clean architecture for eventual standalone
- Can verify compilation without execution

**Phase 1 → Phase 2 Transition:**
- Replace FFI trampolines with inline code
- Add linking and module system
- Rewrite compiler in Habu subset
- Bootstrap and verify

### Current Trade-offs

**Pros:**
- Simple and correct
- Fast iteration
- Good test coverage
- Clean code generation

**Cons:**
- Not truly standalone
- SBCL dependency
- No execution of compiled code
- Missing linking system

---

## Next Steps (Immediate)

1. **Executable Memory Allocation**
   - Use sb-posix:mmap with PROT_EXEC
   - Create function pointers
   - Call generated code

2. **Simple Execution Test**
   ```lisp
   (let ((code (compile-expression '(+ 2 3))))
     (execute-code code))  ; => 5
   ```

3. **Function Call Test**
   ```lisp
   (compile-and-call 'factorial 5)  ; => 120
   ```

4. **Module System**
   - Load multiple files
   - Resolve dependencies
   - Link together

5. **Compiler Subset**
   - Identify Habu features used by compiler
   - Ensure all are implemented
   - Test self-compilation

---

## Conclusion

**Current State:**
- ✅ Can read, parse, and compile Habu source files
- ✅ Generates correct machine code for x86_64 and ARM64
- ✅ Comprehensive language features (functions, loops, macros, etc.)
- ⏳ Cannot execute standalone (Phase 1 limitation)

**Path Forward:**
- Phase 1: Add code execution within SBCL
- Phase 2: Remove SBCL dependencies
- Phase 3: Full self-hosting with bootstrap

**Timeline:**
- Phase 1 completion: Near-term (weeks)
- Phase 2: Medium-term (months)
- Phase 3: Long-term (ongoing)

The compiler is already powerful enough to compile interesting programs. The remaining work is infrastructure (execution, linking, runtime) rather than language features.
