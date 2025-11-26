# cons/car/cdr Implementation Plan

## Goal
Add cons/car/cdr operations to habu-arm64-codegen.lisp so the compiler can generate machine code that calls C runtime functions.

## Strategy: BLR (Branch to Register)

Instead of BL (branch immediate) which requires offset calculation, use BLR which branches to an address in a register. This is perfect for JIT compilation.

## Required ARM64 Instructions

### 1. MOVK - Move with Keep
```
movk xd, #imm16, lsl #shift
Encoding: 1111 0010 1ss. .... iiii iiii iiid dddd
Base: 0xF2800000
  | (shift/16 << 21)  ; shift selector (0, 16, 32, 48)
  | (imm16 << 5)      ; immediate value
  | rd                ; destination register
```

### 2. BLR - Branch to Register
```
blr xn
Encoding: 1101 0110 0011 1111 0000 00nn nnn0 0000
Base: 0xD63F0000
  | (rn << 5)         ; register containing address
```

## Loading 64-bit Addresses

To load a 64-bit address like 0x123456789ABCDEF0:
```asm
movz x2, #0xDEF0              ; Load bits [15:0]
movk x2, #0x9ABC, lsl #16     ; Load bits [31:16], keep others
movk x2, #0x5678, lsl #32     ; Load bits [47:32], keep others
movk x2, #0x1234, lsl #48     ; Load bits [63:48], keep others
```

## Implementation Steps

### Step 1: Add ARM64 Encoders

Add to habu-arm64-codegen.lisp:

```lisp
(defun arm64-movk (rd imm shift)
  "MOVK Xd, #imm, LSL #shift - Move 16-bit immediate, keep other bits
   shift must be 0, 16, 32, or 48"
  (let ((base 0xF2800000))
    (let ((shift-sel (/ shift 16)))  ; 0, 1, 2, or 3
      (let ((encoded (+ base (+ (* shift-sel 2097152)  ; shift-sel << 21
                                (+ (* imm 32) rd)))))   ; imm << 5 | rd
        (encode-word encoded)))))

(defun arm64-blr (rn)
  "BLR Xn - Branch to address in register Xn
   Saves return address in x30 (LR)"
  (let ((base 0xD63F0000))
    (let ((encoded (+ base (* rn 32))))
      (encode-word encoded))))

(defun load-address-to-reg (rd addr)
  "Load 64-bit address into register rd using movz + movk sequence"
  (let ((bits0-15 (my-mod addr 65536)))
    (let ((bits16-31 (my-mod (/ addr 65536) 65536)))
      (let ((bits32-47 (my-mod (/ addr 4294967296) 65536)))
        (let ((bits48-63 (/ addr 281474976710656)))
          (append-code (arm64-movz rd bits0-15)
            (append-code (arm64-movk rd bits16-31 16)
              (append-code (arm64-movk rd bits32-47 32)
                (arm64-movk rd bits48-63 48)))))))))
```

### Step 2: Add Runtime Function Table

Need a way to pass runtime function addresses to the compiler. Options:

**Option A**: Generate C file with addresses
```c
// runtime-addrs.h (generated)
#define HABU_CONS_ADDR 0x104df2488ULL
#define HABU_CAR_ADDR  0x104df2500ULL
#define HABU_CDR_ADDR  0x104df2530ULL
```

**Option B**: Pass as compile-time parameters
```lisp
(compile-with-runtime-addrs expr
  (list (cons 'habu_cons 0x104df2488)
        (cons 'habu_car 0x104df2500)
        (cons 'habu_cdr 0x104df2530)))
```

**Preferred**: Option B - More flexible for JIT

### Step 3: Extend codegen-expr

Add recognition for cons/car/cdr in codegen-expr:

```lisp
(if (symbol=? op (quote cons))
  ;;; (cons a b) - binary operation
  (let ((code1 (codegen-expr arg1)))      ; arg1 → x0
    (let ((save-code (arm64-str 0 31 -16)))  ; push x0
      (let ((code2 (codegen-expr arg2)))     ; arg2 → x0
        (let ((move-code (arm64-mov 1 0)))   ; x0 → x1
          (let ((load-code (arm64-ldr-post 0 31 16)))  ; pop x0
            (let ((load-addr (load-address-to-reg 2 habu-cons-addr)))
              (let ((call-code (arm64-blr 2)))
                (append-code code1
                  (append-code save-code
                    (append-code code2
                      (append-code move-code
                        (append-code load-code
                          (append-code load-addr call-code))))))))))
```

Similar for car/cdr (simpler - only one arg).

### Step 4: Test with JIT Execution

Create test file:

```c
/* test-cons-jit.c */
habu_value_t result = compile_and_execute("(cons 1 2)");
verify(habu_car(result) == fixnum_to_value(1));
verify(habu_cdr(result) == fixnum_to_value(2));
```

## Challenges & Solutions

### Challenge 1: Address Space Layout Randomization (ASLR)

Problem: Runtime function addresses change on every run.

Solution: Generate addresses at JIT compile time, not ahead-of-time.

### Challenge 2: Hex Numbers in Habu Syntax

Problem: Habu uses 0xABCD, SBCL needs #xABCD.

Solution: Work directly in habu REPL, or convert file for SBCL testing.

### Challenge 3: 64-bit Arithmetic in Habu

Problem: Habu fixnums are 60-bit, may need 64-bit for addresses.

Solution: Break addresses into 16-bit chunks, use only small arithmetic.

## Testing Strategy

### Phase 1: Unit Tests
1. Test arm64-movk encoding
2. Test arm64-blr encoding
3. Test load-address-to-reg

### Phase 2: Integration Tests
1. Generate code for (cons 1 2)
2. Execute with JIT
3. Verify result with habu_car/habu_cdr

### Phase 3: Complex Tests
1. Nested cons: (cons (cons 1 2) 3)
2. Lists: (cons 1 (cons 2 (cons 3 nil)))
3. Mixed: (+ (car (cons 5 10)) 3)

## Time Estimate

- ARM64 encoders: 30 min
- Runtime address handling: 30 min
- codegen-expr extension: 1 hour
- Testing: 1-2 hours
- **Total: 3-4 hours**

## Success Criteria

✅ Can compile (cons 1 2) to machine code
✅ Generated code executes correctly
✅ habu_car and habu_cdr work on result
✅ Nested cons works
✅ Lists work

## Next Steps After This

1. Add more list operations (list, append, etc.)
2. Add load function to REPL
3. Test compiling the compiler itself
4. Achieve self-hosting!

---

**Status**: Ready to implement
**Priority**: Critical for self-hosting
**Estimated completion**: Today (2-4 hours of focused work)
