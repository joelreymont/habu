# Pure Habu Compiler Modules

Modular self-hosting compiler with NO SBCL dependencies.

## Architecture

```
pure/
  utils.lisp     - List utilities (append, reverse, mapcar, apply)
  ir.lisp        - IR generation (expression → IR)
  codegen.lisp   - ARM64 code generation (IR → bytes) [reuses existing]
  link.lisp      - Mach-O linking [reuses existing]
  main.lisp      - Entry point (reads, compiles, links)
```

## Compilation Strategy

Each module can be compiled separately to FASL:

```bash
# Compile each module
compile-file pure/utils.lisp   → utils.fasl
compile-file pure/ir.lisp      → ir.fasl  
compile-file pure/codegen.lisp → codegen.fasl
compile-file pure/main.lisp    → main.fasl

# Link FASLs → native compiler
link-fasls utils.fasl ir.fasl codegen.fasl main.fasl → habu-compiler
```

## Benefits

- Modular development (change one module, recompile only that)
- Independent testing of each module
- Uses FASL v2 infrastructure
- No bundling - true separate compilation
