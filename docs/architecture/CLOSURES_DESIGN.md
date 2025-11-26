# Closures Design

## Problem

Currently, lambda functions can only access:
- Their parameters
- Lexically scoped variables from `let` bindings

They **cannot**:
- Return functions that capture variables
- Be stored as first-class values
- Access variables from enclosing scopes after the scope exits

Example that doesn't work:
```lisp
(defun make-adder (x)
  (lambda (y) (+ x y)))  ; x is captured, but currently not accessible

(let ((add-5 (make-adder 5)))
  (funcall add-5 3))  ; Should return 8, but x is not available
```

## Goal

Implement closures as first-class values that capture their lexical environment:
- Functions can capture variables from enclosing scopes
- Captured variables persist after the creating scope exits
- Closures can be stored in variables, passed as arguments, and returned from functions

## Architecture (Phase 1: Bootstrap)

### Closure Object Structure

A closure is a heap-allocated object containing:
1. **Header** (8 bytes): Type tag + size
2. **Code pointer** (8 bytes): Pointer to the function code
3. **Arity** (8 bytes): Number of parameters
4. **Environment size** (8 bytes): Number of captured variables
5. **Captured variables** (8 bytes each): Values of captured variables

```
Closure Object Layout:
[Header][Code Ptr][Arity][Env Size][Var1][Var2]...[VarN]
   8        8        8        8       8     8       8
```

**Tag:** 0x7 (closure/function type)

**Total size:** 32 + (N * 8) bytes, where N = number of captured variables

### Example Memory Layout

```lisp
(let ((x 10))
  (lambda (y) (+ x y)))
```

Creates closure object:
```
Offset  Field           Value
0       Header          0x00000028_00000007  ; size=40, tag=7
8       Code pointer    0x123456789ABC       ; pointer to compiled code
16      Arity           1                    ; takes 1 parameter (y)
24      Env size        1                    ; captures 1 variable
32      x value         160                  ; 10 << 4 (tagged fixnum)
```

### Creation Flow

1. **Parser identifies captured variables:**
   - Walk lambda body
   - Find free variables (used but not parameter or local)
   - Store list of captured variable names

2. **Compilation allocates closure:**
   - Calculate size: 32 + (num-captured * 8)
   - Allocate on heap
   - Set header with closure tag (0x7)
   - Store code pointer (from defun or compiled lambda)
   - Store arity
   - Store environment size
   - Copy captured variable values from current environment

3. **Generated code:**
   ```asm
   ; Allocate closure (40 bytes for 1 captured var)
   mov rdi, 40                  ; size
   call heap-allocate           ; returns pointer in RAX
   ; Set header (size=40, tag=7)
   mov qword [rax], 0x0000002800000007
   ; Set code pointer
   mov rbx, <function-code-addr>
   mov [rax + 8], rbx
   ; Set arity
   mov qword [rax + 16], 1
   ; Set env size
   mov qword [rax + 24], 1
   ; Copy x from environment
   mov rbx, [rsp + x-offset]
   mov [rax + 32], rbx
   ; RAX now points to closure object
   ```

### Call Flow

When calling a closure via funcall:

1. **Extract closure components:**
   - Verify it's a closure (check tag)
   - Get code pointer from offset 8
   - Get env size from offset 24
   - Get captured variables starting at offset 32

2. **Setup call:**
   - Push captured variables onto stack (so code can access them)
   - Push regular arguments
   - Call code pointer

3. **Generated code:**
   ```asm
   ; RAX contains closure pointer
   ; Verify it's a closure
   mov rbx, rax
   and rbx, 0xF
   cmp rbx, 0x7
   jne error

   ; Extract code pointer
   mov r10, [rax + 8]           ; code pointer

   ; Extract env size
   mov r11, [rax + 24]
   shr r11, 4                   ; untag

   ; Push captured variables
   mov r12, 0
   .loop:
     cmp r12, r11
     jge .done
     mov rbx, [rax + 32 + r12*8]
     push rbx
     inc r12
     jmp .loop
   .done:

   ; Setup regular arguments in registers
   ; (RDI, RSI, RDX)

   ; Call closure code
   call r10

   ; Clean up captured variables from stack
   imul r11, 8
   add rsp, r11
   ```

## Implementation Steps

### Step 1: Add Closure Type to IR

```lisp
;; New expr type
(make-expr :type 'closure
           :value code-pointer-or-name
           :args (list arity captured-vars))
```

### Step 2: Free Variable Analysis

Add function to find free variables in lambda body:
```lisp
(defun find-free-variables (body params)
  "Find variables used in body but not in params"
  (let ((used-vars (collect-used-variables body))
        (bound-vars params))
    (set-difference used-vars bound-vars)))
```

### Step 3: Modify Lambda Parser

When parsing `(lambda (params) body)`:
- Analyze body to find free variables
- Create closure IR node with captured variable list
- Store current environment for later capture

### Step 4: Closure Allocation Code Generation

Add `closure` case to `emit-x86_64`:
```lisp
(closure
 (let* ((code-ptr (expr-value expr))
        (arity (first (expr-args expr)))
        (captured-vars (second (expr-args expr)))
        (num-captured (length captured-vars))
        (size (+ 32 (* num-captured 8))))
   (append
    ;; Allocate closure
    (list #x48 #xBF) (int-to-bytes size 8)  ; mov rdi, size
    ;; Call heap-allocate
    ;; Set header, code pointer, arity, env size
    ;; Copy captured variables
    )))
```

### Step 5: Closure Call Code Generation

Modify `runtime-call` to handle closures:
- Check if value is closure (tag 0x7)
- Extract code pointer
- Setup environment and call

### Step 6: Store Closures in Variables

Enable:
```lisp
(let ((adder (lambda (x) (+ x 5))))
  (funcall adder 3))
```

## Examples

### Example 1: Simple Closure
```lisp
(let ((x 10))
  (lambda (y) (+ x y)))
```

Creates closure capturing `x`:
- Code: compiled `(+ x y)` with x from environment
- Arity: 1
- Captured: [x=160]

### Example 2: Closure Factory
```lisp
(defun make-adder (n)
  (lambda (x) (+ x n)))

(let ((add-5 (make-adder 5))
      (add-10 (make-adder 10)))
  (+ (funcall add-5 3)      ; => 8
     (funcall add-10 7)))   ; => 17
```

### Example 3: Closure with Multiple Captures
```lisp
(let ((x 10)
      (y 20))
  (lambda (z) (+ x (+ y z))))
```

Captures both `x` and `y`.

## Testing Strategy

### Test 1: Basic Closure
- Create closure capturing one variable
- Call it and verify captured value is accessible

### Test 2: Multiple Captures
- Closure capturing multiple variables
- Verify all are accessible

### Test 3: Closure Factory
- Function returning different closures
- Each closure captures different values
- Verify they don't interfere

### Test 4: Nested Closures
- Closure returning another closure
- Verify both levels of capture work

### Test 5: Closure in Data Structure
- Store closure in list
- Retrieve and call it
- Verify it still works

## Phase 1 Limitations

1. **No mutation of captured variables:** Captured values are copied, not referenced
2. **Fixed arity:** Closure arity must be known at creation time
3. **No varargs:** Rest parameters not supported
4. **Memory:** Closures persist (no GC of unused closures yet)

## Phase 2 Enhancements

1. **Mutable captures:** Share environment via indirection
2. **Varargs:** Support rest parameters
3. **Optimization:** Inline small closures
4. **GC:** Garbage collect unused closures

## Success Criteria

- ✅ Closures can capture lexical variables
- ✅ Captured variables persist after scope exits
- ✅ Functions can return closures
- ✅ Closures work with funcall
- ✅ Multiple closures don't interfere
- ✅ All existing tests still pass

## Integration with Existing Features

**With funcall:**
```lisp
(let ((f (lambda (x) (+ x 1))))
  (funcall f 5))  ; => 6
```

**With defun:**
```lisp
(defun make-counter (start)
  (lambda () (progn (set start (+ start 1)) start)))
```

**With lists:**
```lisp
(let ((funcs (list (lambda (x) (* x 2))
                   (lambda (x) (+ x 10)))))
  (funcall (car funcs) 5))  ; => 10
```
