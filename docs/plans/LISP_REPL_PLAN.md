# Plan: Full-Featured REPL Written in Habu Lisp

## Goal
Implement a complete REPL where:
- Reader (parse S-expressions) is written in Habu Lisp
- Evaluator (eval) is written in Habu Lisp
- REPL loop is written in Habu Lisp
- C runtime provides only minimal primitives (no REPL-specific code)

## Current State Analysis

### What We Have in C Runtime:
✅ Garbage collection
✅ Basic types: fixnums, cons cells, strings, vectors
✅ List operations: cons, car, cdr
✅ Arithmetic: +, -, *, /
✅ Comparisons: =, <, >, <=, >=
✅ Vector operations
✅ File I/O
✅ Print functions
✅ String creation: habu_make_string
✅ String access: habu_string_ref (just added)
✅ Line input: habu_fgets_line (just added)

### What's Missing for Lisp Reader:

**Critical:**
1. **Type predicates** - Need to check types in Lisp
   - `fixnum?` (is-number?)
   - `cons?` (is-pair?)
   - `string?`
   - `nil?`

2. **String operations:**
   - `string=?` - compare strings for equality
   - `substring` - extract part of string
   - `string->list` - convert string to list of chars (or string-ref is enough)
   - `make-string` - build strings from characters

3. **Character/Integer operations:**
   - Characters are represented as fixnums
   - Need char predicates: is-digit?, is-alpha?, is-whitespace?
   - These can be implemented in Lisp using character codes

4. **Symbol support:**
   - Symbols as first-class values
   - `make-symbol` or `intern` - create/lookup symbols
   - `symbol?` - check if value is symbol
   - `symbol=?` - compare symbols
   - Symbol table/interning

### What's Missing for Lisp Evaluator:

**Critical:**
1. **Environments** - Variable bindings
   - Association lists (can implement with cons)
   - `assoc` - lookup in alist
   - Or use vectors as environments

2. **Symbol handling:**
   - Same as reader needs
   - Must be able to check if expression is a symbol
   - Must be able to compare symbols

3. **Function values:**
   - Closures (already have)
   - Way to check if value is a function

4. **Special form detection:**
   - Need to identify special forms: if, quote, defun, let, etc.
   - Requires symbol comparison

## Implementation Plan

### Phase 1: Add Missing Primitives (C Runtime)

**Priority 1 - Type Predicates:**
```c
habu_value_t habu_fixnum_p(habu_value_t val);   // returns 1 or nil
habu_value_t habu_cons_p(habu_value_t val);     // returns 1 or nil
habu_value_t habu_string_p(habu_value_t val);   // returns 1 or nil
habu_value_t habu_nil_p(habu_value_t val);      // returns 1 or nil
habu_value_t habu_symbol_p(habu_value_t val);   // returns 1 or nil
```

**Priority 2 - String Operations:**
```c
habu_value_t habu_string_eq(habu_value_t s1, habu_value_t s2);  // string equality
habu_value_t habu_substring(habu_value_t str, habu_value_t start, habu_value_t end);
habu_value_t habu_string_append(habu_value_t s1, habu_value_t s2);
```

**Priority 3 - Symbol Support:**
```c
habu_value_t habu_make_symbol(const char *name);  // intern symbol
habu_value_t habu_symbol_name(habu_value_t sym);  // get symbol name as string
```

**Nice to Have:**
```c
habu_value_t habu_assoc(habu_value_t key, habu_value_t alist);  // lookup in alist
```

### Phase 2: Implement Reader in Habu Lisp

**File: reader.lisp**

Components:
1. **Character utilities** (using fixnum char codes)
   - `is-whitespace?` - check if char is space/tab/newline
   - `is-digit?` - check if char is 0-9
   - `is-alpha?` - check if char is a-z/A-Z
   - `digit->int` - convert char to integer

2. **Parser state**
   - Track string and current position
   - Peek current character
   - Advance position

3. **Token parsers**
   - `parse-number` - read integer
   - `parse-symbol` - read symbol name
   - `parse-string` - read string literal
   - `parse-list` - read (...)
   - `parse-quote` - handle 'expr

4. **Main reader**
   - `read-from-string` - parse one S-expression

### Phase 3: Implement Evaluator in Habu Lisp

**File: eval.lisp**

Components:
1. **Environment operations**
   - Use association lists: ((var1 . val1) (var2 . val2) ...)
   - `env-lookup` - find variable value
   - `env-extend` - add new bindings
   - Global environment with built-in functions

2. **Evaluator**
   - Self-evaluating: numbers, strings
   - Variable lookup
   - Special forms:
     - `quote` - return unevaluated
     - `if` - conditional
     - `lambda` - create closure
     - `let` - local bindings
     - `defun` - define function
     - `setq` - set variable
     - `progn` - sequence
   - Function application
     - Eval function and arguments
     - Apply function to arguments

3. **Built-in functions**
   - Map symbols to native functions
   - Arithmetic: +, -, *, /
   - List: cons, car, cdr, list
   - Predicates: =, <, >, nil?, etc.

### Phase 4: Implement REPL in Habu Lisp

**File: repl.lisp** (update existing)

Components:
1. **Input handling**
   - Print prompt
   - Read line with `fgets-line`
   - Convert to Habu string
   - Parse with `read-from-string`

2. **Evaluation**
   - Call `eval` with global environment
   - Handle errors (if error handling exists)

3. **Output**
   - Print result with `print-value`
   - Print newline

4. **Loop**
   - Repeat until EOF (Ctrl-D)

### Phase 5: Update C Backend

Add code generation for new primitives:
- Type predicates: fixnum?, cons?, string?, nil?, symbol?
- String operations: string=?, substring
- Symbol operations: make-symbol, symbol-name

### Phase 6: Remove REPL-Specific C Code

Delete:
- `runtime/lineedit.c` - fancy line editing (REPL-specific)
- `runtime/reader.c` - reader now in Lisp
- `runtime/eval.c` - evaluator now in Lisp

Keep (general-purpose primitives):
- `runtime/gc.c` - memory management
- `runtime/runtime.c` - core operations
- `runtime/io.c` - I/O primitives
- All object operations

## Success Criteria

✅ Reader written entirely in Habu Lisp
✅ Evaluator written entirely in Habu Lisp
✅ REPL loop written entirely in Habu Lisp
✅ C runtime has no REPL-specific code
✅ C runtime provides only general-purpose primitives
✅ REPL can read, eval, and print expressions
✅ Self-hosting: REPL compiles to standalone binary

## Timeline Estimate

- Phase 1 (Primitives): 2-3 hours
- Phase 2 (Reader): 3-4 hours
- Phase 3 (Evaluator): 4-5 hours
- Phase 4 (REPL): 1 hour
- Phase 5 (C Backend): 1-2 hours
- Phase 6 (Cleanup): 1 hour
- **Total: 12-16 hours**

## Notes

- Start simple: support only numbers and lists initially
- Add features incrementally: symbols, strings, special forms
- Test each component independently before integrating
- Keep reader and eval separate for clarity
- Consider implementing a simple printer in Lisp too
