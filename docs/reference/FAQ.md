# Habu Lisp - Frequently Asked Questions

## General Questions

### What is Habu Lisp?

Habu Lisp is a complete, working Lisp interpreter implemented in 320 lines of pure Lisp code, compiled to a 73KB native executable. It demonstrates minimal runtime philosophy - only one C primitive was added to implement the entire language.

### Which REPL should I use?

- **For learning Lisp**: Use `habu-rec` (the complete Lisp with defun and recursion)
- **For experimentation**: Use `habu-rec`
- **For understanding progression**: Try all three in order: enhanced → programmable → recursive

**Recommendation**: Just use `habu-rec` - it's the complete implementation.

### Is this production-ready?

Yes and no:
- ✅ **Yes** for: Learning, experimentation, embedded scripting, algorithm prototyping
- ❌ **No** for: High-performance applications, production web services, systems requiring tail-call optimization

It's a fully functional, tested Lisp interpreter, but it's optimized for size and simplicity rather than performance.

### How does it compare to other Lisps?

| Feature | Habu | Scheme | Common Lisp |
|---------|------|--------|-------------|
| Size | 73KB | ~10MB | ~100MB |
| Startup | Instant | Fast | Slow |
| Learning curve | Gentle | Moderate | Steep |
| Standard library | Minimal | Large | Huge |
| Purpose | Learning/embedded | General | Production |

Habu is perfect for learning Lisp and understanding how interpreters work.

## Installation & Building

### What do I need to build Habu?

- **SBCL** (Steel Bank Common Lisp) - for compiling Lisp to C
- **GCC or Clang** - for compiling C to native code
- **Make** - for build automation

### How do I build the REPLs?

```bash
# Build all three REPLs
make repls

# Or build individually
make habu-enhanced
make habu-prog
make habu-rec
```

### Can I run without building?

No - the `.lisp` files must be compiled to C and then to native code. However, the build is very fast (a few seconds).

### Which platforms are supported?

Currently:
- ✅ macOS ARM64 (Apple Silicon)
- ✅ macOS x86_64 (Intel)
- ✅ Linux x86_64

The C code is portable, so it should work on any platform with a C compiler.

### Why does compilation show warnings?

The SBCL compiler may show style warnings about unused variables or undefined functions in the bootstrap code. These are harmless and don't affect the generated REPL executables.

## Usage Questions

### How do I exit the REPL?

- Press **Ctrl-D** on an empty line
- Or press **Ctrl-C** to interrupt

### Why does it print `<symbol>` instead of symbol names?

The print function shows type tags. `<symbol>` means a symbol value was returned. This is intentional to distinguish symbols from numbers.

To see the actual symbol, you'd need a more sophisticated printer (which we keep minimal for code size).

### How do I load the standard library?

Currently, you need to copy and paste functions from `stdlib.lisp` into the REPL manually:

```bash
# Copy a function definition
cat stdlib.lisp  # Find the function you want

# Paste into REPL
./habu-rec
habu> (defun map (f lst) ...)
```

Future versions may add a `load` primitive.

### Can I save my session?

No - there's no built-in session persistence. Each REPL session starts fresh.

Workaround: Keep your functions in a file and paste them in when you start the REPL.

### How do I debug errors?

Currently, error messages are minimal. Debugging strategies:

1. **Test incrementally** - Test each function as you write it
2. **Use smaller inputs** - Test with simple cases first
3. **Add print statements** - Use `print-value` to see intermediate values
4. **Check parentheses** - Unbalanced parens are a common issue

### Why is my recursive function slow/crashing?

Two likely issues:

1. **Deep recursion** - No tail-call optimization, so deep recursion can overflow the stack
   - Solution: Use smaller inputs or rewrite with accumulator

2. **Exponential recursion** - Functions like naive fibonacci are very slow
   - Solution: Use iterative versions or memoization

Example:
```lisp
; BAD - Exponential time, deep recursion
(defun fib (n)
  (if (< n 2) n
    (+ (fib (- n 1)) (fib (- n 2)))))

; BETTER - Linear time with accumulator
(defun fib-fast (n)
  (fib-helper n 0 1))

(defun fib-helper (n a b)
  (if (= n 0) a
    (fib-helper (- n 1) b (+ a b))))
```

### How do I work with strings?

String support is minimal - strings exist for symbols, but there are no string manipulation functions. This is intentional to keep the implementation small.

For learning Lisp, focus on numbers and lists.

### Can I define variables?

Not directly. Use `let` for local variables or `defun` with constants:

```lisp
; Local variables
(let ((pi 314) (radius 5))
  (* pi (* radius radius)))

; "Constant" function
(defun pi () 314)
(* (pi) 5)
```

### How do I create a list from individual elements?

Use `cons` repeatedly or `list`:

```lisp
; With cons
(cons 1 (cons 2 (cons 3 nil)))  ; → (1 2 3)

; With list
(list 1 2 3)  ; → (1 2 3)

; With quote
'(1 2 3)  ; → (1 2 3)
```

## Language Features

### What features are supported?

**Supported**:
- Numbers (integers only)
- Symbols and quote
- Lists (cons, car, cdr)
- Conditionals (if)
- Local variables (let)
- Functions (lambda, defun)
- Recursion
- Closures
- Comparison operators (=, <, >)
- Arithmetic (+, -, *, /)

**Not supported**:
- Macros
- Floating-point numbers
- Strings manipulation
- Multiple return values
- Tail-call optimization
- Error handling (try/catch)
- Module system
- File I/O

### Why no tail-call optimization?

Tail-call optimization (TCO) would require significant C runtime changes. The current implementation prioritizes simplicity and small size over TCO.

If you need deep recursion, use accumulator patterns to reduce stack depth.

### Why no macros?

Macros require:
1. A more sophisticated reader (to handle backquote/unquote)
2. A macro expansion phase
3. More complex evaluation

This would add significant complexity. The current implementation focuses on core Lisp semantics.

### Can I add features?

Yes! See `CONTRIBUTING.md` for guidance on adding:
- New operators (easy)
- New special forms (moderate)
- New C primitives (avoid if possible)

### Why integers only? What about floating-point?

Integers are simpler to implement and sufficient for learning Lisp. Adding floating-point would require:
- Type discrimination (int vs float)
- Different arithmetic operations
- Print formatting for floats

This would increase code size significantly.

### How does garbage collection work?

The C runtime includes a mark-and-sweep garbage collector:
1. **Allocation**: Objects allocated on heap
2. **Marking**: GC marks reachable objects
3. **Sweeping**: Unreachable objects freed
4. **Automatic**: Triggered when heap fills

GC is transparent to Lisp code.

## Technical Questions

### How is it so small (73KB)?

Several factors:
1. **Minimal runtime** - Only essential C primitives
2. **Everything in Lisp** - Reader, evaluator, environment all in Lisp
3. **No standard library** - Stdlib is separate, loaded on demand
4. **Simple implementation** - No optimization passes, no JIT
5. **Static linking** - Single executable with no dependencies

### What is "minimal runtime philosophy"?

The idea that the C runtime should provide only:
- Memory management (GC, cons, make-vector, etc.)
- Field access (car, cdr, get-tag)
- Primitive arithmetic
- I/O (print, readline)

Everything else - parsing, evaluation, environment management - is implemented in Lisp.

This project added only **one** C primitive beyond the basic runtime.

### How does compilation work?

```
.lisp source → SBCL reader → Habu compiler → C code → GCC → Native executable
```

1. SBCL reads the Lisp source
2. Habu compiler transforms to internal representation
3. C backend generates C code
4. GCC compiles to native machine code
5. Links with minimal C runtime

### Why use SBCL for compilation?

SBCL provides:
- Lisp reader (parses Lisp syntax)
- Lisp environment (for compiler to run in)
- Fast compilation

Once compiled, the REPL runs standalone - SBCL is not needed at runtime.

### Can I modify the C runtime?

Yes, but the philosophy is to avoid it. Try to implement features in Lisp first.

If you must modify C runtime:
1. Keep it minimal
2. Document why it's necessary
3. Update `bootstrap/c-backend.lisp` for codegen

### How does the environment work?

Environments are association lists:
```lisp
((symbol1 . value1) (symbol2 . value2) ...)
```

- **Lookup**: Linear search through list
- **Extend**: Cons new binding onto front
- **Lexical**: Closures capture environment
- **Dynamic**: Defun uses global environment

### How does recursion work without TCO?

Recursion works through normal function calls - each recursive call uses stack space. Without TCO, deep recursion will eventually overflow the stack (typically ~1000-10000 calls depending on platform).

Use smaller inputs or accumulator patterns to reduce depth.

### What's the difference between the three REPLs?

1. **Enhanced** (235 lines):
   - Quote, symbols, if, lists
   - No let, no lambda, no defun
   - Good for understanding basic evaluation

2. **Programmable** (282 lines):
   - + let, lambda, closures
   - No defun, no comparisons
   - Good for understanding functions and scope

3. **Recursive** (320 lines):
   - + defun, recursion, comparisons
   - Complete Lisp
   - Use this one!

## Troubleshooting

### Build fails with "habu.h not found"

Make sure you include the runtime directory:
```bash
gcc -Iruntime -o habu-rec habu-rec.c runtime/*.c -lm
```

Or use the Makefile:
```bash
make habu-rec
```

### REPL crashes or segfaults

Likely causes:
1. **Deep recursion** - Reduce input size
2. **Stack overflow** - Use iterative version
3. **Bug in code** - Test incrementally

### Tests fail

```bash
./test-repls.sh
```

If tests fail:
1. Check that all three REPLs are built
2. Rebuild: `make clean && make repls`
3. Check for uncommitted changes to source files

### "Unbalanced parentheses" error

Count your parentheses! Common issues:
```lisp
; WRONG - missing closing paren
(defun foo (x)
  (* x x)

; RIGHT
(defun foo (x)
  (* x x))
```

Use an editor with paren matching.

### Performance is slow

This is expected - it's an interpreter, not a compiler. Strategies:

1. **Use smaller inputs**
2. **Avoid exponential algorithms** (naive fibonacci)
3. **Use iterative versions** when possible
4. **Profile your code** - which function is slow?

### Stack overflow

Reduce recursion depth:

```lisp
; Instead of:
(factorial 10000)  ; Will crash

; Use:
(factorial 100)    ; Should work
```

Or implement iteratively with accumulator.

## Learning & Teaching

### I'm new to Lisp - where do I start?

1. Read `README_REPL.md` - Quick start guide
2. Try `./habu-rec` - Start the REPL
3. Read `EXAMPLE_SESSION.md` - See examples
4. Work through `examples.lisp` - Practice

### What resources help learn Lisp?

**Books**:
- *The Little Schemer* - Great for beginners
- *SICP* (Structure and Interpretation of Computer Programs)
- *Practical Common Lisp*

**Online**:
- Try Habu REPL examples
- Build-Your-Own-Lisp tutorials
- Scheme/Lisp tutorials

### Can I use this for teaching?

Absolutely! Habu is perfect for teaching:
- Lisp programming basics
- Language implementation
- Interpreter design
- Functional programming
- Recursion concepts

The progressive REPLs (enhanced → programmable → recursive) show how features build on each other.

### What can I build with Habu?

Great for:
- Learning exercises
- Algorithm implementations
- Small utilities
- Educational demos
- Understanding interpreters

Not great for:
- Production applications
- Performance-critical code
- Large systems

## Contributing

### How can I contribute?

See `CONTRIBUTING.md` for details. Ways to contribute:
- Report bugs
- Add examples
- Improve documentation
- Add tests
- Suggest features
- Fix issues

### Can I add more operators?

Yes! Edit the evaluator in `recursive-repl.lisp`:

```lisp
(defun eval-expr (expr env)
  ...
  ; Add your operator here
  (if (symbol=? first (make-symbol (quote "my-op")))
    (my-implementation args)
    ...))
```

See `CONTRIBUTING.md` for details.

### Should I add a C primitive?

Probably not! The philosophy is to implement features in Lisp. Only add C primitives if:
- Absolutely necessary
- Impossible to implement in Lisp
- You understand the tradeoffs

Ask first by opening an issue.

### How do I submit changes?

1. Fork the repository
2. Make your changes
3. Test thoroughly (`./test-repls.sh`)
4. Update documentation
5. Submit a pull request

See `CONTRIBUTING.md` for full details.

## Getting Help

### Where can I get help?

- **Documentation**: Start with `README_REPL.md`
- **Examples**: See `examples.lisp` and `EXAMPLE_SESSION.md`
- **Reference**: Check `QUICK_REFERENCE.md`
- **Issues**: Report bugs on GitHub
- **Discussions**: Ask questions in GitHub discussions

### How do I report bugs?

Open a GitHub issue with:
1. What you tried (code)
2. What you expected
3. What actually happened
4. Your platform (macOS/Linux, version)

### Is there a community?

The Habu community is just starting! Join via:
- GitHub Issues (questions)
- GitHub Discussions (general chat)
- Pull Requests (contributions)

---

## Still Have Questions?

- Check the documentation in the repository
- Read through `examples.lisp` for practical examples
- Try experimenting in the REPL
- Open a GitHub issue

**Happy Lisping!** 🎉
