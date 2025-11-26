# Runtime Funcall Design

## Problem

Currently `funcall` only works at compile-time:
- Looks up functions in `*function-table*`
- Inlines the function body: `((lambda params body) args...)`
- Cannot call functions determined at runtime
- No true higher-order programming

## Goal

Enable runtime function calls via symbol-function slots:
```lisp
(defun add (x y) (+ x y))
(defun apply-to-5 (f) (funcall f 5 3))
(apply-to-5 'add)  ; Should work at runtime!
```

## Architecture (Phase 1: Bootstrap)

### Approach: Compiled Code Storage

**When defun is compiled:**
1. Compile the function body to machine code
2. Create an SBCL alien-callable wrapper
3. Get the function pointer address
4. Store address in symbol-function slot

**When funcall is called:**
1. Generate code to look up symbol
2. Read symbol-function slot (contains function pointer)
3. Call the function pointer with arguments

### Data Structures

**Symbol-function slot:**
- Currently stores hash marker (for future use)
- Will store actual function pointer (uint64)

**Function object structure (future):**
```
Header (8) + Code pointer (8) + Arity (8) + Environment (8) = 32 bytes
Tag: 0x7 (function)
```

For Phase 1, just store raw function pointer in symbol-function slot.

### Calling Convention

**System V AMD64 ABI:**
- Args: RDI, RSI, RDX, RCX, R8, R9
- Return: RAX
- Caller-saved: RAX, RCX, RDX, RSI, RDI, R8-R11
- Callee-saved: RBX, RBP, R12-R15

**Habu functions:**
- All values are tagged fixnums or pointers
- Arguments passed via registers (up to 6 args)
- Additional args on stack (future)
- Return value in RAX

### Implementation Steps

1. **Modify defun:**
   - Compile function body to machine code
   - Create alien-callable with correct signature
   - Store function pointer in symbol-function slot

2. **Add runtime-funcall:**
   - Look up symbol, get function pointer from symbol-function slot
   - Call with arguments
   - Return result

3. **Generate funcall code:**
   - Emit code to intern symbol
   - Read symbol-function slot
   - Setup arguments in registers
   - Call function pointer
   - Handle return value

## Example Flow

```lisp
;; 1. Define function
(defun square (x) (* x x))
```

What happens:
- Compile `(* x x)` to machine code
- Create alien-callable: `habu-function-square`
- Get pointer: `0x12345678`
- Store in symbol SQUARE's function slot

```lisp
;; 2. Call via funcall
(funcall 'square 5)
```

Generated code:
```asm
; Intern symbol 'square
mov rdi, <"SQUARE" hash>
call runtime-intern
; RAX now contains symbol pointer

; Read symbol-function slot
mov rax, [rax + 24]  ; offset 24 = function slot
; RAX now contains function pointer (0x12345678)

; Setup argument
mov rdi, 80  ; 5 << 4 (tagged fixnum)

; Call function
call rax
; RAX now contains result
```

## Limitations (Phase 1)

1. **Fixed arity:** Must know argument count at compile-time
2. **No varargs:** Variable argument lists not supported
3. **SBCL dependency:** Functions are SBCL alien-callables
4. **Memory:** Compiled functions stay in memory forever

## Future (Phase 2)

1. **Inline compilation:** Generate code directly, no SBCL
2. **Function objects:** Proper function type with metadata
3. **Varargs:** Rest parameters and apply
4. **Closures:** Capture environment
5. **GC:** Garbage collect unused functions

## Testing

Test cases:
1. Simple funcall: `(funcall 'add 3 4)`
2. Higher-order: `(mapcar 'square '(1 2 3))`
3. Passed as argument: `(apply-fn 'add '(5 7))`
4. Multiple arities: Functions with different arg counts
5. Nested calls: `(funcall (funcall 'get-adder) 5 3)`

## Success Criteria

- ✅ defun stores executable code pointer
- ✅ funcall works at runtime (not just compile-time)
- ✅ Higher-order functions work
- ✅ All existing tests still pass
- ✅ Performance acceptable (< 100ns per funcall)
