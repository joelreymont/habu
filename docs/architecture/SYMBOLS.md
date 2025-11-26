# Symbol System

## Overview

Habu's symbol system provides interning, value/function bindings, and property lists. In bootstrap mode, symbols are managed by the runtime with full GC support.

## Symbol Structure

Each symbol occupies 48 bytes total:
- **Header** (8 bytes): Size, tag (0x2), mark bit
- **Name pointer** (8 bytes): Pointer to string or fixnum hash
- **Value** (8 bytes): Current value binding (or 0xFFFFFFFFFFFFFFFF = unbound)
- **Function** (8 bytes): Function definition (or 0xFFFFFFFFFFFFFFFF = unbound)
- **Property list** (8 bytes): Cons-based plist

## Runtime Functions

### Interning
```lisp
(runtime-intern "FOO")     ; Returns symbol pointer, same for same name
(runtime-make-symbol "X")  ; Returns uninterned symbol
(runtime-gensym "G")       ; Returns unique uninterned symbol
```

### Accessors
```lisp
(runtime-symbol-name sym)      ; Get name string (for debugging)
(runtime-symbol-value sym)     ; Get value (errors if unbound)
(runtime-symbol-function sym)  ; Get function (errors if unbound)
(runtime-symbol-plist sym)     ; Get property list
```

### Setters
```lisp
(set-symbol-value sym val)     ; Set value
(set-symbol-function sym fn)   ; Set function
(set-symbol-plist sym plist)   ; Set property list
```

## Symbol Table

Global hash table mapping names to symbol pointers:
- Uses Common Lisp `equal` for string comparison
- Persistent across GC (table is in Lisp heap, not Habu heap)
- Cleared with `(clear-symbol-table)`

## GC Integration

Symbols are GC roots when:
1. Present in the global symbol table (interned)
2. Registered via `(register-gc-root sym)`

Symbol GC marks:
- Name pointer (if string)
- Value (if bound and pointer)
- Function (if bound and pointer)
- Property list (if cons)

## Bootstrap Mode Limitations

**Current status:**

✅ Symbol runtime fully implemented
✅ Interning and value/function bindings work
✅ GC correctly handles symbols
⚠️  No compiler operations yet (use runtime functions directly)

**Not yet available in compiled code:**
- `(intern "name")` as a compiler operation
- `(symbol-value 'foo)` as a compiler operation
- `(setq foo val)` for global symbols
- `(defun foo ...)` for global functions

**Workaround:**

Use runtime functions directly from Lisp:
```lisp
(let ((foo-sym (runtime-intern "FOO")))
  (set-symbol-value foo-sym (ash 42 4))  ; Set FOO = 42
  (runtime-symbol-value foo-sym))         ; Get FOO => 42 (as fixnum)
```

## Standalone Mode (Future)

In standalone mode:
- Symbol table will be in Habu heap (not Lisp heap)
- Need string support for symbol names
- Compiler will generate inline symbol operations
- `defun` will set symbol-function slots
- Global variables via symbol-value slots

## Testing

```bash
sbcl --load bootstrap/test-symbols.lisp
```

Tests:
- ✅ Symbol interning (same name => same symbol)
- ✅ Symbol value get/set
- ✅ Symbol name lookup
- ✅ Unbound detection

## Future Work

- [ ] Add `intern` as compiler operation
- [ ] Add `symbol-value` and `set` to compiler
- [ ] Implement `defvar` for global variables
- [ ] Implement `defun` for global functions
- [ ] Add proper string support for symbol names
- [ ] Implement symbol packages
- [ ] Add symbol printing/reading
