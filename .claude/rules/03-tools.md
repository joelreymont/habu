# Standalone Tools

**NO MCP. All tools are in `tools/` directory. NEVER look for or try to call MCP tools.**

**WRITE TOOLS IN LISP, NOT BASH.** Shell wrappers are thin exec lines only. All logic in .lisp files.

**BUILD NEW TOOLS AS NEEDED.** When debugging or investigating, create reusable tools in tools/ instead of one-off bash commands. Future sessions will thank you.

## Build habu0
```bash
tools/build [output-name] [heap-size]
# Default: tools/build habu0 67108864
```

## Run binary
```bash
tools/run <binary> [args...]
# Captures output, exit code, crash info
```

## Check paren balance
```bash
tools/paren-check <file.lisp>
# Reports unclosed parens with context
```

## Debug binary under lldb
```bash
tools/debug <binary> [stdin-input]
# Runs under lldb, captures crash info, backtrace, registers
```

## Find function at address
```bash
tools/map-lookup <mapfile> <address>
# Finds function containing hex address in .map file
```

## Build process
1. Loads ASDF :habu system
2. Reads source files in order: shared/macros.lisp, arm64/asm.lisp, bootstrap/reader.lisp, habu0.lisp, bootstrap/reg-alloc.lisp, bootstrap/codegen.lisp
3. Expands habu macros (while, sym-case, sym-eq, define-op-defvars, etc.)
4. Filters eval-when (SBCL-only)
5. Calls deliver-forms to generate Mach-O binary

## After editing bootstrap/*.lisp or habu0.lisp
Just run `tools/build` - no restart needed.
