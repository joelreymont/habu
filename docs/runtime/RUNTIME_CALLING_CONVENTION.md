# Habu Runtime Function Calling Convention

## Overview

Habu compiled code can call C runtime functions via the ARM64 BLR (Branch and Link to Register) instruction. This document describes the calling convention and available runtime functions.

## Address Table

Runtime function addresses are obtained via `bin/print-runtime-addrs`, which outputs addresses in the format:
```
HABU_CONS_ADDR=0x104cfab20
HABU_CAR_ADDR=0x104cf8c2c
...
```

These addresses are threaded through the compiler via the `runtime-addrs` parameter to code generation functions.

## ARM64 Calling Convention

All runtime functions follow the ARM64 C calling convention:

**Arguments**: x0, x1, x2, x3, x4, x5, x6, x7 (first 8 arguments)
**Return value**: x0
**Preserved registers**: x19-x29, sp (must be preserved by callee)
**Scratch registers**: x0-x18, x30 (caller must save if needed)

## Code Pattern for Runtime Calls

### Unary Operations (e.g., car, cdr)

```lisp
;;; 1. Compile argument into x0
(let ((arg-code (codegen-expr arg-ir runtime-addrs fn-offsets current-offset nil)))
  ;;; 2. Load runtime function address into x2
  (let ((load-addr (load-address-to-reg 2 function-address)))
    ;;; 3. Call via BLR x2
    (let ((call (arm64-blr 2)))
      ;;; 4. Result is in x0
      (append-code arg-code (append-code load-addr call)))))
```

### Binary Operations (e.g., cons)

```lisp
;;; 1. Compile first argument into x0
(let ((arg1-code (codegen-expr arg1-ir runtime-addrs fn-offsets current-offset nil)))
  ;;; 2. Save x0 to stack
  (let ((save-x0 (arm64-str 0 31 0)))
    ;;; 3. Compile second argument into x0
    (let ((arg2-code (codegen-expr arg2-ir runtime-addrs fn-offsets
                                   (+ current-offset (count-instrs arg1-code) 1) nil)))
      ;;; 4. Move x0 to x1 (second argument)
      (let ((mov-to-x1 (arm64-add-imm 1 0 0)))
        ;;; 5. Restore first argument from stack to x0
        (let ((restore-x0 (arm64-ldr 0 31 0)))
          ;;; 6. Load runtime function address into x2
          (let ((load-addr (load-address-to-reg 2 function-address)))
            ;;; 7. Call via BLR x2
            (let ((call (arm64-blr 2)))
              ;;; 8. Result is in x0
              (append-code arg1-code
                (append-code save-x0
                  (append-code arg2-code
                    (append-code mov-to-x1
                      (append-code restore-x0
                        (append-code load-addr call)))))))))))))
```

## Available Runtime Functions

### Memory Allocation (4 functions)

#### habu_cons
```c
habu_value_t habu_cons(habu_value_t car, habu_value_t cdr)
```
- **Arguments**: x0 = car (tagged), x1 = cdr (tagged)
- **Returns**: x0 = cons cell (tagged pointer)
- **Description**: Allocate new cons cell with given car and cdr

#### habu_make_vector
```c
habu_value_t habu_make_vector(size_t length)
```
- **Arguments**: x0 = length (raw size_t)
- **Returns**: x0 = vector (tagged pointer)
- **Description**: Allocate vector of given length

#### habu_make_string
```c
habu_value_t habu_make_string(const char *str, size_t length)
```
- **Arguments**: x0 = C string pointer, x1 = length
- **Returns**: x0 = string (tagged pointer)
- **Description**: Create Habu string from C string

#### habu_make_symbol
```c
habu_value_t habu_make_symbol(const char *name)
```
- **Arguments**: x0 = C string pointer (symbol name)
- **Returns**: x0 = symbol (tagged pointer)
- **Description**: Intern symbol with given name

### List Accessors (4 functions)

#### habu_car
```c
habu_value_t habu_car(habu_value_t cons)
```
- **Arguments**: x0 = cons cell (tagged)
- **Returns**: x0 = car value (tagged)
- **Description**: Get car of cons cell (returns NIL if argument is NIL)

#### habu_cdr
```c
habu_value_t habu_cdr(habu_value_t cons)
```
- **Arguments**: x0 = cons cell (tagged)
- **Returns**: x0 = cdr value (tagged)
- **Description**: Get cdr of cons cell (returns NIL if argument is NIL)

#### habu_set_car
```c
void habu_set_car(habu_value_t cons, habu_value_t value)
```
- **Arguments**: x0 = cons cell (tagged), x1 = new car value (tagged)
- **Returns**: (void)
- **Description**: Set car of cons cell (with write barrier for GC)

#### habu_set_cdr
```c
void habu_set_cdr(habu_value_t cons, habu_value_t value)
```
- **Arguments**: x0 = cons cell (tagged), x1 = new cdr value (tagged)
- **Returns**: (void)
- **Description**: Set cdr of cons cell (with write barrier for GC)

### Vector Operations (2 functions)

#### habu_vector_ref
```c
habu_value_t habu_vector_ref(habu_value_t vector, size_t index)
```
- **Arguments**: x0 = vector (tagged), x1 = index (raw size_t)
- **Returns**: x0 = element value (tagged), or NIL if out of bounds
- **Description**: Get element at index from vector

#### habu_vector_set
```c
void habu_vector_set(habu_value_t vector, size_t index, habu_value_t value)
```
- **Arguments**: x0 = vector (tagged), x1 = index (raw size_t), x2 = value (tagged)
- **Returns**: (void)
- **Description**: Set element at index in vector (with write barrier)

### String Operations (6 functions)

#### habu_string_ref
```c
habu_value_t habu_string_ref(habu_value_t str_val, size_t index)
```
- **Arguments**: x0 = string (tagged), x1 = index (raw size_t)
- **Returns**: x0 = character as fixnum, or NIL if out of bounds
- **Description**: Get character at index from string

#### habu_string_length_raw
```c
size_t habu_string_length_raw(habu_value_t str_val)
```
- **Arguments**: x0 = string (tagged)
- **Returns**: x0 = length (raw size_t)
- **Description**: Get length of string

#### habu_string_concat
```c
habu_value_t habu_string_concat(habu_value_t str1_val, habu_value_t str2_val)
```
- **Arguments**: x0 = first string (tagged), x1 = second string (tagged)
- **Returns**: x0 = concatenated string (tagged)
- **Description**: Concatenate two strings

#### habu_string_substring
```c
habu_value_t habu_string_substring(habu_value_t str_val, habu_value_t start_val, habu_value_t end_val)
```
- **Arguments**: x0 = string (tagged), x1 = start index (tagged fixnum), x2 = end index (tagged fixnum)
- **Returns**: x0 = substring (tagged)
- **Description**: Extract substring from start to end

#### habu_fixnum_to_string
```c
habu_value_t habu_fixnum_to_string(habu_value_t num_val)
```
- **Arguments**: x0 = number (tagged fixnum)
- **Returns**: x0 = string representation (tagged)
- **Description**: Convert fixnum to string

#### habu_make_string_from_vector
```c
habu_value_t habu_make_string_from_vector(habu_value_t vec_val)
```
- **Arguments**: x0 = vector of character codes (tagged)
- **Returns**: x0 = string (tagged)
- **Description**: Create string from vector of fixnum character codes

### Symbol Operations (2 functions)

#### habu_make_symbol_from_string
```c
habu_value_t habu_make_symbol_from_string(habu_value_t str_val)
```
- **Arguments**: x0 = string (tagged)
- **Returns**: x0 = symbol (tagged)
- **Description**: Intern symbol with name from string

#### habu_symbol_name
```c
habu_value_t habu_symbol_name(habu_value_t sym_val)
```
- **Arguments**: x0 = symbol (tagged)
- **Returns**: x0 = name string (tagged)
- **Description**: Get name of symbol as string

### Closure Operations (3 functions)

#### habu_make_closure
```c
habu_value_t habu_make_closure(void *code_ptr, habu_value_t env)
```
- **Arguments**: x0 = code pointer (raw pointer), x1 = environment (tagged)
- **Returns**: x0 = closure (tagged)
- **Description**: Create closure with code pointer and environment

#### habu_closure_code
```c
void *habu_closure_code(habu_value_t closure_val)
```
- **Arguments**: x0 = closure (tagged)
- **Returns**: x0 = code pointer (raw pointer), or NULL if not a closure
- **Description**: Get code pointer from closure

#### habu_closure_env
```c
habu_value_t habu_closure_env(habu_value_t closure_val)
```
- **Arguments**: x0 = closure (tagged)
- **Returns**: x0 = environment (tagged), or NIL if not a closure
- **Description**: Get environment from closure

### Type Operations (1 function)

#### habu_get_tag
```c
habu_value_t habu_get_tag(habu_value_t val)
```
- **Arguments**: x0 = any value (tagged)
- **Returns**: x0 = tag value as fixnum
- **Description**: Get type tag of value

### I/O Operations (4 functions)

#### habu_print
```c
void habu_print(const char *str)
```
- **Arguments**: x0 = C string pointer
- **Returns**: (void)
- **Description**: Print C string to stdout with flush

#### habu_write_byte
```c
void habu_write_byte(uint8_t byte)
```
- **Arguments**: x0 = byte value (raw uint8_t)
- **Returns**: (void)
- **Description**: Write single byte to stdout

#### habu_read_byte
```c
uint8_t habu_read_byte(void)
```
- **Arguments**: (none)
- **Returns**: x0 = byte value (raw uint8_t)
- **Description**: Read single byte from stdin

#### habu_fgets_line
```c
char* habu_fgets_line(void)
```
- **Arguments**: (none)
- **Returns**: x0 = C string pointer (malloc'd, caller must free)
- **Description**: Read line from stdin, returns NULL on EOF

## Value Tagging

Habu uses pointer tagging for type dispatch:

- **Fixnum**: LSB = 0b0000 (bottom 4 bits), value in upper 60 bits
- **Cons**: LSB = 0b0001, pointer in upper 60 bits (8-byte aligned)
- **Vector**: LSB = 0b0010
- **String**: LSB = 0b0011
- **Symbol**: LSB = 0b0100
- **Closure**: LSB = 0b0101
- **NIL**: Special value 0x0000000000000000

## GC Considerations

Many runtime functions allocate memory and may trigger garbage collection:
- `habu_cons`, `habu_make_vector`, `habu_make_string`, `habu_make_symbol`
- String operations that create new strings
- Symbol interning

When calling these functions, ensure any live values are either:
1. In preserved registers (x19-x29)
2. On the stack
3. Rooted in the GC root set

Write barriers are automatically applied by `habu_set_car`, `habu_set_cdr`, and `habu_vector_set`.

## Example: Implementing (car (cons 1 2))

```armasm
;;; Create cons cell (cons 1 2)
movz x0, #16        ; fixnum 1 (1 << 4)
movz x1, #32        ; fixnum 2 (2 << 4)
movz x2, #0x0000    ; Load habu_cons address (4 instructions)
movk x2, #0x0000, lsl #16
movk x2, #0x04cf, lsl #32
movk x2, #0x0104, lsl #48
blr x2              ; Call habu_cons - result in x0

;;; Get car of result
movz x2, #0x0000    ; Load habu_car address
movk x2, #0x0000, lsl #16
movk x2, #0x04cf, lsl #32
movk x2, #0x0104, lsl #48
blr x2              ; Call habu_car - result in x0

;;; Result in x0 is fixnum 1 (tagged as 16)
```

## Adding New Runtime Functions

To add a new runtime function callable from compiled code:

1. **Implement the function in runtime/**: Add to appropriate .c file (runtime.c, gc.c, etc.)
2. **Declare in runtime/habu.h**: Add function prototype
3. **Export address in runtime/runtime.c**: Add to `habu_print_runtime_addrs()`
4. **Export address in bin/print-runtime-addrs.c**: Add matching printf
5. **Rebuild**: `make bin/print-runtime-addrs`
6. **Update compiler**: Add codegen case in habu-arm64-codegen.lisp
7. **Test**: Create test case verifying the function works from compiled code

## Current Implementation Status

**Fully implemented in compiler**:
- ✅ habu_cons
- ✅ habu_car
- ✅ habu_cdr

**Exported but not yet implemented in compiler** (27 total, 3 done, 24 remaining):
- ⏳ All other runtime functions

These can be added incrementally as language features are implemented.
