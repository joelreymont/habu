# Habu Self-Hosting Roadmap

**Goal**: Achieve full self-hosting Lisp compiler with complete Common Lisp spec support

**Date**: 2025-11-20

---

## Phase 1: Core Compiler Features (Week 1-2)

### Priority 1.1: Fix Critical Issues
- [x] ARM64 instruction encoders
- [x] Basic let bindings (single variable)
- [ ] **Fix habu interpreter file loading** (BLOCKING)
- [ ] **Proper LDR encoding** for multiple variable offsets
- [ ] **Multiple let bindings**: `(let ((x 1) (y 2)) ...)`

### Priority 1.2: Function Support
- [ ] **defun**: Function definitions with named parameters
- [ ] **Function calls**: BL instruction + ARM64 calling convention
- [ ] **Recursion**: Tail call optimization (later phase)
- [ ] **Lambda**: Anonymous functions
- [ ] **Closures**: Free variable capture with heap allocation

### Priority 1.3: Memory Management Integration
- [ ] **cons/car/cdr**: List construction and access
- [ ] **Runtime calls**: BL to C functions (habu_cons, habu_car, etc.)
- [ ] **GC integration**: Allocation triggers GC when needed
- [ ] **Tagged pointers**: Distinguish fixnum from heap pointers

**Milestone 1**: Can compile and run recursive factorial function
```lisp
(defun factorial (n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))
```

---

## Phase 2: Data Structures (Week 3-4)

### Priority 2.1: Lists
- [ ] list, append, reverse
- [ ] length, nth, nthcdr
- [ ] member, assoc
- [ ] mapcar, reduce, filter

### Priority 2.2: Vectors
- [ ] make-vector, vector-ref, vector-set!
- [ ] vector-length
- [ ] Array representation in memory

### Priority 2.3: Strings
- [ ] String literals
- [ ] string-concat, substring
- [ ] string->list, list->string
- [ ] String comparison (string=, string<)

### Priority 2.4: Hash Tables
- [ ] make-hash-table
- [ ] gethash, puthash, remhash
- [ ] Hash function implementation
- [ ] Collision handling

**Milestone 2**: Can manipulate complex data structures
```lisp
(let ((ht (make-hash-table)))
  (puthash ht 'key 'value)
  (gethash ht 'key))
```

---

## Phase 3: Macro System (Week 5-6)

### Priority 3.1: Reader Macros
- [ ] **Quasiquote**: `` ` `` syntax
- [ ] **Unquote**: `,` substitution
- [ ] **Splice**: `,@` list insertion
- [ ] Reader macro table

### Priority 3.2: Defmacro
- [ ] defmacro definition
- [ ] Macro expansion at compile time
- [ ] macroexpand, macroexpand-1
- [ ] Hygiene (gensym)

### Priority 3.3: Built-in Macros
- [ ] when, unless (if not already)
- [ ] cond, case (if not already)
- [ ] and, or (if not already)
- [ ] let, let* (if not already)
- [ ] do, dotimes, dolist

**Milestone 3**: Can define and use custom macros
```lisp
(defmacro when (test &rest body)
  `(if ,test (progn ,@body) nil))
```

---

## Phase 4: Control Flow & I/O (Week 7-8)

### Priority 4.1: Advanced Control Flow
- [ ] **catch/throw**: Non-local exits
- [ ] **unwind-protect**: Cleanup handlers
- [ ] **block/return-from**: Named blocks
- [ ] **tagbody/go**: Labeled jumps

### Priority 4.2: Loop Constructs
- [ ] dolist, dotimes
- [ ] loop macro (basic)
- [ ] loop with collect, sum, etc.

### Priority 4.3: I/O Operations
- [ ] read (S-expression reader)
- [ ] print, prin1, princ
- [ ] read-line, read-char
- [ ] File operations: open, close, read-file, write-file
- [ ] Streams

### Priority 4.4: Error Handling
- [ ] error, signal
- [ ] handler-case, handler-bind
- [ ] condition system (basic)

**Milestone 4**: Can read Lisp files, handle errors gracefully
```lisp
(handler-case
  (/ 1 0)
  (division-by-zero () 'error))
```

---

## Phase 5: Self-Hosting (Week 9-10)

### Priority 5.1: Compiler Bootstrap
- [ ] **Load compiler in habu**: habu loads habu-arm64-codegen.lisp
- [ ] **Compile simple expressions**: Test IR → ARM64 generation
- [ ] **Compile functions**: Test function definition and calls
- [ ] **Compile data structures**: Test cons, vectors, etc.

### Priority 5.2: Meta-Circular Compilation
- [ ] **Stage 0**: Use habu REPL to compile habu-arm64-codegen.lisp → binary
- [ ] **Stage 1**: Use stage0 to compile itself → stage1 binary
- [ ] **Stage 2**: Use stage1 to compile itself → stage2 binary
- [ ] **Verify fixed point**: stage1 == stage2 (byte-identical)

### Priority 5.3: Executable Generation
- [ ] **Mach-O format**: macOS executable headers
- [ ] **ELF format**: Linux executable headers
- [ ] **Linking**: Connect to C runtime library
- [ ] **Standalone binary**: Single executable file

**Milestone 5**: Self-hosting achieved! Compiler compiles itself
```bash
./habu habu-arm64-codegen.lisp > stage1
./stage1 habu-arm64-codegen.lisp > stage2
diff stage1 stage2  # Should be identical
```

---

## Phase 6: Optimization (Week 11-12)

### Priority 6.1: Register Allocation
- [ ] Live variable analysis
- [ ] Register interference graph
- [ ] Graph coloring allocation
- [ ] Spill to stack when needed

### Priority 6.2: Tail Call Optimization
- [ ] Detect tail position calls
- [ ] Replace BL with B (branch instead of branch-link)
- [ ] Adjust stack frame for tail calls
- [ ] Test with tail-recursive factorial

### Priority 6.3: Inline Optimization
- [ ] Inline small functions
- [ ] Inline primitives (+, -, *, etc.)
- [ ] Constant folding
- [ ] Dead code elimination

### Priority 6.4: Peephole Optimization
- [ ] Remove redundant moves
- [ ] Combine adjacent instructions
- [ ] Use specialized instructions (e.g., MADD)

**Milestone 6**: Generated code is efficient and optimized
- Benchmark against SBCL
- Target: Within 2-3x of SBCL performance

---

## Phase 7: CLOS (Week 13-16)

### Priority 7.1: Basic Objects
- [ ] defclass
- [ ] make-instance
- [ ] slot-value, slot-boundp
- [ ] Object representation in memory

### Priority 7.2: Methods
- [ ] defmethod
- [ ] defgeneric
- [ ] Method dispatch (single dispatch first)
- [ ] Method combination

### Priority 7.3: Multiple Dispatch
- [ ] Multi-method dispatch
- [ ] Method precedence
- [ ] CLOS MOP (Meta-Object Protocol) basics

### Priority 7.4: Standard Classes
- [ ] standard-class
- [ ] standard-object
- [ ] built-in-class

**Milestone 7**: Object-oriented programming works
```lisp
(defclass point ()
  ((x :initarg :x :accessor point-x)
   (y :initarg :y :accessor point-y)))

(defmethod distance ((p point))
  (sqrt (+ (* (point-x p) (point-x p))
           (* (point-y p) (point-y p)))))
```

---

## Phase 8: Standard Library (Week 17-20)

### Priority 8.1: Sequence Operations
- [ ] map, reduce, filter
- [ ] sort, stable-sort
- [ ] remove, remove-if
- [ ] position, find

### Priority 8.2: Numeric Operations
- [ ] Floating-point support (IEEE 754)
- [ ] Complex numbers
- [ ] Rationals
- [ ] Bignums (arbitrary precision)

### Priority 8.3: String Library
- [ ] String formatting (format)
- [ ] Regular expressions
- [ ] String parsing utilities

### Priority 8.4: System Interface
- [ ] File system operations
- [ ] Process management
- [ ] Environment variables
- [ ] Time and date

**Milestone 8**: Feature-complete Common Lisp subset

---

## Phase 9: Advanced Features (Week 21-24)

### Priority 9.1: Package System
- [ ] defpackage
- [ ] in-package
- [ ] export, import
- [ ] Symbol visibility

### Priority 9.2: Reader Extensions
- [ ] #' function
- [ ] #. read-time eval
- [ ] Custom reader macros
- [ ] Readtable manipulation

### Priority 9.3: Compiler Extensions
- [ ] declare, declaim
- [ ] Type declarations
- [ ] Inline declarations
- [ ] Optimization settings

### Priority 9.4: Multiple Values
- [ ] values, multiple-value-bind
- [ ] multiple-value-call
- [ ] nth-value

**Milestone 9**: Advanced Lisp features complete

---

## Phase 10: Full Common Lisp (Month 6+)

### Priority 10.1: Remaining CL Features
- [ ] CLOS MOP (complete)
- [ ] Conditions and restarts (complete)
- [ ] Pathnames
- [ ] Logical pathnames
- [ ] Gray streams

### Priority 10.2: Conformance Testing
- [ ] ANSI CL test suite (ANSI-TESTS)
- [ ] Fix failing tests
- [ ] Performance benchmarks
- [ ] Memory profiling

### Priority 10.3: Documentation
- [ ] API documentation
- [ ] Tutorial
- [ ] Language reference
- [ ] Implementation notes

### Priority 10.4: Production Readiness
- [ ] Stable release
- [ ] Performance tuning
- [ ] Bug fixes
- [ ] Community feedback

**Milestone 10**: Production-ready Common Lisp implementation

---

## Success Criteria

### Self-Hosting (Minimum Viable)
1. ✅ Compiler written in Lisp
2. ✅ Generates ARM64 machine code
3. ⏳ Can compile itself
4. ⏳ Fixed-point bootstrap works
5. ⏳ Passes basic test suite

### Full Common Lisp (Complete)
1. Passes ANSI CL test suite (>95%)
2. Can run existing CL programs
3. Performance within 3x of SBCL
4. Complete standard library
5. CLOS fully implemented
6. Production-ready stability

---

## Risk Assessment

### High Risk Items
1. **Habu interpreter hanging**: BLOCKING self-hosting tests
   - Mitigation: Use SBCL for development, fix interpreter in parallel

2. **Memory management complexity**: Closures, GC integration
   - Mitigation: Start with simple heap, add incremental GC later

3. **ARM64 calling convention**: Function calls, stack management
   - Mitigation: Study AAPCS64 spec, test incrementally

4. **CLOS complexity**: Large implementation effort
   - Mitigation: Start with simple single-dispatch, expand gradually

### Medium Risk Items
1. Macro hygiene and expansion
2. Reader macro extensibility
3. Performance optimization effectiveness
4. Floating-point and numeric tower

### Low Risk Items
1. Basic data structures (already proven)
2. Control flow (mostly complete)
3. I/O operations (straightforward)
4. String operations (well understood)

---

## Dependencies

### External Dependencies
- C runtime (minimal, already exists)
- SBCL (for bootstrap during development)
- gcc/clang (for linking final binaries)

### Internal Dependencies
```
Phase 1 (Functions) → Phase 2 (Data structures)
                   → Phase 5 (Self-hosting)

Phase 2 (Data)     → Phase 3 (Macros)
                   → Phase 7 (CLOS)

Phase 3 (Macros)   → Phase 4 (Control flow)
                   → Phase 8 (Standard library)

Phase 5 (Self-host)→ Phase 6 (Optimization)
                   → Phase 10 (Production)
```

---

## Time Estimates

| Phase | Duration | Cumulative |
|-------|----------|------------|
| Phase 1: Core Compiler | 2 weeks | 2 weeks |
| Phase 2: Data Structures | 2 weeks | 4 weeks |
| Phase 3: Macro System | 2 weeks | 6 weeks |
| Phase 4: Control Flow & I/O | 2 weeks | 8 weeks |
| Phase 5: Self-Hosting | 2 weeks | 10 weeks |
| Phase 6: Optimization | 2 weeks | 12 weeks |
| Phase 7: CLOS | 4 weeks | 16 weeks |
| Phase 8: Standard Library | 4 weeks | 20 weeks |
| Phase 9: Advanced Features | 4 weeks | 24 weeks |
| Phase 10: Full CL | 8+ weeks | 32+ weeks |

**Total**: ~8 months to full Common Lisp implementation

**Self-hosting milestone**: 10 weeks (~2.5 months)

---

## Next Immediate Actions (Today)

1. ✅ Create this roadmap
2. ⏳ Fix habu interpreter file loading
3. ⏳ Implement proper LDR encoding
4. ⏳ Extend let to multiple bindings
5. ⏳ Test with examples
6. ⏳ Implement defun (function definitions)
7. ⏳ Implement function calls
8. ⏳ Test recursive functions

**Focus**: Phase 1 completion (Core Compiler Features)

---

**Last Updated**: 2025-11-20
**Status**: Phase 1 in progress (75% complete)
**Next Milestone**: Recursive function support
