# Habu Value Tagging Scheme

## Overview

Habu uses a hybrid 1+3 bit tagging scheme with 16-byte object alignment, optimized for:
- 63-bit fixnums (OCaml-competitive numeric range)
- Header-less cons cells (33% memory reduction)
- Fast type predicates (no memory access needed)
- Efficient GC (type known from pointer tag)

## Tagging Format

```
Bit 0 = 1: Fixnum (63-bit signed integer)
  Value = tagged_value >> 1
  Range: -4,611,686,018,427,387,904 to +4,611,686,018,427,387,903

Bit 0 = 0: Pointer OR nil
  If value == 0: nil
  Else:
    Type = (value >> 1) & 7  (3 bits, 8 types)
    Pointer = value & ~15    (16-byte aligned)
```

## Type Tags

| Tag | Binary   | Type     | Header? | Object Size |
|-----|----------|----------|---------|-------------|
| 0   | 0b0000   | Cons     | NO      | 16B fixed   |
| 2   | 0b0010   | Symbol   | YES     | 16B fixed   |
| 4   | 0b0100   | Vector   | YES     | 16B + N*8B  |
| 6   | 0b0110   | String   | YES     | 16B + chars |
| 8   | 0b1000   | Closure  | YES     | 16B fixed   |
| 10  | 0b1010   | Keyword  | NO      | 16B fixed   |
| 12  | 0b1100   | Reserved | -       | -           |
| 14  | 0b1110   | Forward  | -       | GC only     |

## Special Values

```
nil     = 0x0000000000000000  (all zeros)
t       = interned symbol "T"
fixnum 0 = 0x0000000000000001  (bit 0 set, value = 0)
fixnum 1 = 0x0000000000000003  (bit 0 set, value = 1)
fixnum -1 = 0xFFFFFFFFFFFFFFFF (bit 0 set, value = -1)
```

## Object Layouts

### Cons Cell (Tag 0, NO HEADER)
```
Offset  Size  Contents
0       8B    car (tagged value)
8       8B    cdr (tagged value)
Total: 16 bytes
```

Key optimization: No header means:
- 33% smaller than traditional 24-byte cons (header + car + cdr)
- 4 cons cells per 64-byte cache line (vs 2.67)
- GC doesn't need to read header to know object size

### Symbol (Tag 2)
```
Offset  Size  Contents
0       8B    header (hash, flags)
8       8B    name (tagged string pointer)
Total: 16 bytes
```

### Keyword (Tag 10, NO HEADER)
```
Offset  Size  Contents
0       8B    name hash
8       8B    name (tagged string pointer)
Total: 16 bytes
```

Keywords are like symbols but:
- Self-evaluating
- Interned in keyword table
- Own tag for O(1) `keywordp` check

### Vector (Tag 4)
```
Offset  Size  Contents
0       8B    header (length, element-type)
8       8B    element 0
16      8B    element 1
...
Total: 16 + N*8 bytes (padded to 16B boundary)
```

### String (Tag 6)
```
Offset  Size  Contents
0       8B    header (length, encoding)
8       NB    characters (UTF-8)
Total: 16 + len bytes (padded to 16B boundary)
```

### Closure (Tag 8)
```
Offset  Size  Contents
0       8B    header (arity, flags)
8       8B    code-ptr OR captured-env
Total: 16 bytes
```

## Type Predicates

All type checks are O(1) with no memory access:

```lisp
(defun fixnump (x)
  (= (logand x 1) 1))

(defun consp (x)
  (and (not (zerop x))
       (= (logand x #xF) 0)))

(defun symbolp (x)
  (= (logand x #xF) 2))

(defun vectorp (x)
  (= (logand x #xF) 4))

(defun stringp (x)
  (= (logand x #xF) 6))

(defun closurep (x)
  (= (logand x #xF) 8))

(defun keywordp (x)
  (= (logand x #xF) 10))

(defun null (x)
  (zerop x))
```

## Fixnum Operations

```lisp
;; Tag fixnum: shift left 1, set bit 0
(defun tag-fixnum (n)
  (logior (ash n 1) 1))

;; Untag fixnum: arithmetic shift right 1
(defun untag-fixnum (x)
  (ash x -1))

;; Fixnum addition (can operate on tagged values directly)
;; (a<<1|1) + (b<<1|1) - 1 = ((a+b)<<1|1)
(defun fixnum-add (a b)
  (- (+ a b) 1))

;; Fixnum subtraction
;; (a<<1|1) - (b<<1|1) + 1 = ((a-b)<<1|1)
(defun fixnum-sub (a b)
  (+ (- a b) 1))
```

## Pointer Operations

```lisp
;; Extract raw pointer (clear tag bits)
(defun untag-ptr (x)
  (logand x (lognot #xF)))

;; Tag a pointer
(defun tag-ptr (ptr tag)
  (logior ptr tag))

;; Get tag from tagged pointer
(defun get-tag (x)
  (logand x #xF))
```

## GC Considerations

### Cons Cells
Since cons cells have no header:
1. Object size is always 16 bytes (known from tag)
2. Forwarding pointer stored in car slot during copy
3. Detect forwarding by checking if car has tag 14

```lisp
(defun forwarded-p (cons-ptr)
  (let ((car-val (mem-load cons-ptr 0)))
    (= (logand car-val #xF) 14)))

(defun get-forwarding-addr (cons-ptr)
  (logand (mem-load cons-ptr 0) (lognot #xF)))
```

### Object Size Calculation
```lisp
(defun object-size (tagged-ptr)
  (case (get-tag tagged-ptr)
    (0  16)  ; cons - fixed
    (2  16)  ; symbol - fixed
    (4  (+ 16 (* 8 (vector-length tagged-ptr))))  ; vector
    (6  (+ 16 (align16 (string-length tagged-ptr))))  ; string
    (8  16)  ; closure - fixed
    (10 16)  ; keyword - fixed
    ))
```

## Why This Scheme?

### vs OCaml (1-bit tag, type in header)
- OCaml: 63-bit fixnums, but must dereference for type
- Habu: 63-bit fixnums AND type in pointer tag
- Winner: Habu for type-heavy dispatch

### vs Traditional Lisp (4-bit tag in low bits)
- Traditional: 60-bit fixnums, 24-byte cons cells
- Habu: 63-bit fixnums, 16-byte cons cells
- Winner: Habu for memory and cache efficiency

### 16-byte alignment cost
- Wastes at most 8 bytes per small object
- But cons cells (most common) are exactly 16 bytes
- SIMD-friendly for future vector operations
- Modern allocators (jemalloc, mimalloc) already 16-byte align

## ARM64 Implementation

### Type Check (2 instructions)
```asm
; Check if x0 is a cons
and x1, x0, #0xF
cbz x1, is_cons    ; tag 0 = cons (but check not nil first)
```

### Fixnum Check (2 instructions)
```asm
; Check if x0 is a fixnum
tst x0, #1
b.ne is_fixnum     ; bit 0 set = fixnum
```

### Nil Check (1 instruction)
```asm
; Check if x0 is nil
cbz x0, is_nil     ; nil = 0
```

### Cons Access (1 instruction each)
```asm
; car - cons tag is 0, so pointer IS the address
ldr x1, [x0]       ; car
ldr x2, [x0, #8]   ; cdr
```

## Migration Notes

### From 4-bit tags
Old scheme used bits 0-3 for tag, bits 4-63 for value/pointer.
New scheme uses bit 0 for fixnum flag, bits 1-3 for pointer type.

Key changes:
1. Fixnums: `val << 4` -> `(val << 1) | 1`
2. nil: `0x06` -> `0x00`
3. Cons: had header, now header-less
4. All pointer tags shifted to even numbers
