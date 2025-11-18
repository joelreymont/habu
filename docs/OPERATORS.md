# Habu Lisp - Operator Reference

## Overview

This document provides a comprehensive reference for all operators implemented in the Habu Lisp compiler. All operators are implemented for both x86_64 and ARM64 architectures and work with tagged fixnum integers.

**Total Operators**: 164 (136 core + 28 c[ad]r combinations)
**Test Coverage**: 378 compiler tests (100% passing)
**Architectures**: x86_64, ARM64
**Optimizations**: Constant folding, algebraic simplifications

## Fixnum Tagging

All integers are tagged fixnums:
- Value stored: `value << 4` (multiply by 16)
- Type tag: Low 4 bits = `0000` for fixnums
- Range: -2^59 to 2^59-1 on 64-bit systems

## Operator Categories

Summary:
- **Arithmetic** (31): +, -, *, /, mod, rem, min, max, abs, 1+, 1-, signum, gcd, lcm, isqrt, integer-length, expt, square, neg, log2, cube, double, half, avg, range, quot, reciprocal, sqr, lerp, median3, map-range
- **Rounding** (8): floor, ceiling, truncate, round, ffloor, fceiling, ftruncate, fround
- **Comparison** (12): <, >, =, <=, >=, /=, equal, eql, eq, min3, max3, sign
- **Boolean** (3): and, or, not
- **Bitwise** (24): logand, logior, logxor, lognot, ash, logcount, logtest, logbitp, lognand, lognor, logeqv, logandc1, logandc2, logorc1, logorc2, set-bit, clear-bit, toggle-bit, mask, low-bits, rotl, rotr, bit-field, bit-field-set
- **Predicates** (24): zerop, plusp, minusp, evenp, oddp, null, numberp, integerp, atom, listp, consp, symbolp, between, power-of-2?, aligned?, high-bit?, within?, outside?, same-sign?, nonzero?, divisible?, multiple-of?, divides?, coprime?
- **Scheme Aliases** (6): zero?, positive?, negative?, even?, odd?, number?
- **Control Flow** (9): if, cond, case, when, unless, progn, begin, if-let, when-let
- **Variables** (6): let, let*, let1, setq, incf, decf
- **Functions** (2): lambda, defun
- **Macros** (1): defmacro
- **Data** (5): quote, cons, car, cdr, list
- **Utility** (5): identity, clamp, align-up, align-down, constrain
- **C[ad]r** (28): caar, cadr, cdar, cddr, caaar, caadr, cadar, caddr, cdaar, cdadr, cddar, cdddr, caaaar, caaadr, caadar, caaddr, cadaar, cadadr, caddar, cadddr, cdaaar, cdaadr, cdadar, cdaddr, cddaar, cddadr, cdddar, cddddr

**Total: 136 core operators + 28 c[ad]r combinations = 164 operators**

### Arithmetic Operators (17 operators)

#### `+` - Addition
```lisp
(+ 2 3)          ; => 5
(+ 10 20 30)     ; => 60
(+ -5 10)        ; => 5
```
**Implementation**: Fixnum addition with tag preservation

#### `-` - Subtraction or Negation
```lisp
(- 10 3)         ; => 7
(- 5)            ; => -5 (negation)
(- 100 30 20)    ; => 50
```
**Implementation**: Fixnum subtraction or negation

#### `*` - Multiplication
```lisp
(* 3 4)          ; => 12
(* 2 3 4)        ; => 24
(* -2 5)         ; => -10
```
**Implementation**: Multiply with tag adjustment

#### `/` - Integer Division
```lisp
(/ 10 3)         ; => 3 (truncates toward zero)
(/ 20 4)         ; => 5
(/ -10 3)        ; => -3
```
**Implementation**: Hardware IDIV instruction

#### `mod` - Modulo
```lisp
(mod 17 5)       ; => 2
(mod -10 3)      ; => 2
(mod 10 3)       ; => 1
```
**Implementation**: Hardware division remainder, adjusted for sign

#### `rem` - Remainder
```lisp
(rem 17 5)       ; => 2
(rem -10 3)      ; => -1 (different from mod for negatives)
(rem 10 3)       ; => 1
```
**Implementation**: Hardware division remainder (RDX after IDIV)

#### `min` - Minimum
```lisp
(min 5 3)        ; => 3
(min 10 20 5)    ; => 5
(min -5 10)      ; => -5
```
**Implementation**: Conditional move (CMOVG/CSEL)

#### `max` - Maximum
```lisp
(max 5 3)        ; => 5
(max 10 20 5)    ; => 20
(max -5 10)      ; => 10
```
**Implementation**: Conditional move (CMOVL/CSEL)

#### `abs` - Absolute Value
```lisp
(abs -5)         ; => 5
(abs 10)         ; => 10
(abs 0)          ; => 0
```
**Implementation**: Conditional negation

#### `1+` - Increment by 1
```lisp
(1+ 5)           ; => 6
(1+ -1)          ; => 0
```
**Implementation**: Add tagged 1 (16)

#### `1-` - Decrement by 1
```lisp
(1- 5)           ; => 4
(1- 0)           ; => -1
```
**Implementation**: Subtract tagged 1 (16)

#### `signum` - Sign Function
```lisp
(signum -5)      ; => -1
(signum 0)       ; => 0
(signum 5)       ; => 1
```
**Implementation**: Test and conditional set
- x86_64: TEST, SETLE, conditional jumps
- ARM64: CMP, CSETM, CSET (branchless)

#### `gcd` - Greatest Common Divisor
```lisp
(gcd 48 18)      ; => 6
(gcd 100 35)     ; => 5
(gcd 17 13)      ; => 1
```
**Implementation**: Euclidean algorithm with loop

#### `lcm` - Least Common Multiple
```lisp
(lcm 12 18)      ; => 36
(lcm 4 6)        ; => 12
```
**Implementation**: Formula lcm(a,b) = |a*b|/gcd(a,b)

#### `isqrt` - Integer Square Root
```lisp
(isqrt 16)       ; => 4
(isqrt 17)       ; => 4
(isqrt 100)      ; => 10
```
**Implementation**: Newton's method iteration

#### `integer-length` - Bit Length
```lisp
(integer-length 7)    ; => 3
(integer-length 255)  ; => 8
(integer-length -1)   ; => 0
```
**Implementation**: BSR (bit scan reverse) instruction

#### `expt` - Integer Exponentiation
```lisp
(expt 2 10)      ; => 1024
(expt 3 4)       ; => 81
(expt 5 0)       ; => 1
```
**Implementation**: Repeated multiplication loop

### Rounding Operators (8 operators)

#### `floor` - Floor (Identity for Fixnums)
```lisp
(floor 5)        ; => 5 (integers are already floored)
(floor -3)       ; => -3
```
**Implementation**: Identity operation (fixnums are integers)

#### `ceiling` - Ceiling (Identity for Fixnums)
```lisp
(ceiling 5)      ; => 5 (integers are already ceiled)
(ceiling -3)     ; => -3
```
**Implementation**: Identity operation (fixnums are integers)

#### `truncate` - Truncate (Identity for Fixnums)
```lisp
(truncate 5)     ; => 5 (integers are already truncated)
(truncate -3)    ; => -3
```
**Implementation**: Identity operation (fixnums are integers)

#### `round` - Round (Identity for Fixnums)
```lisp
(round 5)        ; => 5 (integers are already rounded)
(round -3)       ; => -3
```
**Implementation**: Identity operation (fixnums are integers)

#### `ffloor` - Floor Division
```lisp
(ffloor 7 3)     ; => 2 (floor of 7/3)
(ffloor -7 3)    ; => -3 (rounds toward -infinity)
```
**Implementation**: Division with floor rounding adjustment

#### `fceiling` - Ceiling Division
```lisp
(fceiling 7 3)   ; => 3 (ceiling of 7/3)
(fceiling -7 3)  ; => -2 (rounds toward +infinity)
```
**Implementation**: Division with ceiling rounding adjustment

#### `ftruncate` - Truncating Division
```lisp
(ftruncate 7 3)  ; => 2 (truncates toward zero)
(ftruncate -7 3) ; => -2
```
**Implementation**: Standard integer division (IDIV)

#### `fround` - Rounding Division
```lisp
(fround 7 3)     ; => 2 (rounds to nearest)
(fround 8 3)     ; => 3
```
**Implementation**: Division with banker's rounding

### Comparison Operators (9 operators)

#### `<` - Less Than
```lisp
(< 3 5)          ; => 1 (true)
(< 5 3)          ; => 0 (false)
(< 5 5)          ; => 0
```
**Implementation**: CMP + SETL/CSET

#### `>` - Greater Than
```lisp
(> 5 3)          ; => 1 (true)
(> 3 5)          ; => 0 (false)
```
**Implementation**: CMP + SETG/CSET

#### `=` - Equal
```lisp
(= 5 5)          ; => 1 (true)
(= 5 3)          ; => 0 (false)
```
**Implementation**: CMP + SETE/CSET

#### `<=` - Less Than or Equal
```lisp
(<= 3 5)         ; => 1
(<= 5 5)         ; => 1
(<= 5 3)         ; => 0
```
**Implementation**: CMP + SETLE/CSET

#### `>=` - Greater Than or Equal
```lisp
(>= 5 3)         ; => 1
(>= 5 5)         ; => 1
(>= 3 5)         ; => 0
```
**Implementation**: CMP + SETGE/CSET

#### `/=` - Not Equal
```lisp
(/= 5 3)         ; => 1 (true)
(/= 5 5)         ; => 0 (false)
```
**Implementation**: CMP + SETNE/CSET

#### `equal` - Equality (alias for `=`)
```lisp
(equal 5 5)      ; => 1
```
**Implementation**: Same as `=`

#### `eql` - EQL Equality
```lisp
(eql 5 5)        ; => 1
(eql 5 3)        ; => 0
```
**Implementation**: Fixnum comparison (same as `=` for fixnums)

#### `eq` - EQ Identity
```lisp
(eq 5 5)         ; => 1
(eq 5 3)         ; => 0
```
**Implementation**: Pointer/fixnum comparison (same as `=` for fixnums)

### Boolean Operators (3 operators)

#### `and` - Logical AND
```lisp
(and 1 1)        ; => 1
(and 1 0)        ; => 0
(and 0 0)        ; => 0
```
**Implementation**: Short-circuit evaluation with conditional jumps
**Note**: Non-zero is true, 0 is false

#### `or` - Logical OR
```lisp
(or 1 0)         ; => 1
(or 0 0)         ; => 0
(or 5 0)         ; => 5 (returns first truthy value)
```
**Implementation**: Short-circuit evaluation with conditional jumps

#### `not` - Logical NOT
```lisp
(not 0)          ; => 1
(not 1)          ; => 0
(not 5)          ; => 0 (any non-zero is false)
```
**Implementation**: TEST + SETZ/CSET

### Bitwise Operators (11 operators)

#### `logand` - Bitwise AND
```lisp
(logand 12 10)   ; => 8  (1100 & 1010 = 1000)
(logand 7 3)     ; => 3  (0111 & 0011 = 0011)
```
**Implementation**: AND instruction on untagged values

#### `logior` - Bitwise OR
```lisp
(logior 12 10)   ; => 14 (1100 | 1010 = 1110)
(logior 1 2)     ; => 3  (01 | 10 = 11)
```
**Implementation**: OR instruction on untagged values

#### `logxor` - Bitwise XOR
```lisp
(logxor 12 10)   ; => 6  (1100 ^ 1010 = 0110)
(logxor 5 3)     ; => 6  (101 ^ 011 = 110)
```
**Implementation**: XOR instruction on untagged values

#### `lognot` - Bitwise NOT
```lisp
(lognot 5)       ; => -6 (two's complement)
(lognot -1)      ; => 0
```
**Implementation**: NOT instruction on untagged value

#### `ash` - Arithmetic Shift
```lisp
(ash 5 2)        ; => 20 (shift left by 2: 5 * 4)
(ash 20 -2)      ; => 5  (shift right by 2: 20 / 4)
```
**Implementation**: SAL/SHL for left, SAR for right (preserves sign)

#### `logcount` - Population Count
```lisp
(logcount 7)     ; => 3  (binary 111 has 3 bits set)
(logcount 8)     ; => 1  (binary 1000 has 1 bit set)
(logcount 0)     ; => 0
```
**Implementation**: Brian Kernighan's algorithm (loop clearing lowest set bit)
**Code**: `n = n & (n-1)` repeatedly counts set bits

#### `logtest` - Test Bits
```lisp
(logtest 7 3)    ; => 1 (0111 & 0011 != 0)
(logtest 8 4)    ; => 0 (1000 & 0100 = 0)
```
**Implementation**: AND + test for zero
**Returns**: 1 if any bits are set in both arguments, 0 otherwise

#### `logbitp` - Test Bit at Position
```lisp
(logbitp 2 7)    ; => 1 (bit 2 of 0111 is set)
(logbitp 3 7)    ; => 0 (bit 3 of 0111 is not set)
```
**Implementation**: Shift right by position, test lowest bit
**Returns**: 1 if bit at position is set, 0 otherwise

#### `lognand` - Bitwise NAND
```lisp
(lognand 15 7)   ; => -8 (NOT (1111 & 0111))
(lognand 3 3)    ; => -4 (NOT 0011)
```
**Implementation**: AND followed by NOT, with tag preservation

#### `lognor` - Bitwise NOR
```lisp
(lognor 8 4)     ; => -13 (NOT (1000 | 0100))
(lognor 0 0)     ; => -1 (NOT 0)
```
**Implementation**: OR followed by NOT, with tag preservation

#### `logeqv` - Bitwise Equivalence (XNOR)
```lisp
(logeqv 15 7)    ; => -7 (NOT (1111 ^ 0111))
(logeqv 5 5)     ; => -1 (NOT 0)
```
**Implementation**: XOR followed by NOT, with tag preservation

### Predicates (12 operators)

#### `zerop` - Test for Zero
```lisp
(zerop 0)        ; => 1 (true)
(zerop 5)        ; => 0 (false)
```
**Implementation**: TEST + SETZ/CSET

#### `plusp` - Test for Positive
```lisp
(plusp 5)        ; => 1 (true)
(plusp 0)        ; => 0 (false)
(plusp -5)       ; => 0 (false)
```
**Implementation**: TEST + SETG/CSET

#### `minusp` - Test for Negative
```lisp
(minusp -5)      ; => 1 (true)
(minusp 0)       ; => 0 (false)
(minusp 5)       ; => 0 (false)
```
**Implementation**: TEST + SETL/CSET

#### `evenp` - Test for Even
```lisp
(evenp 4)        ; => 1 (true)
(evenp 5)        ; => 0 (false)
(evenp 0)        ; => 1 (true)
```
**Implementation**: AND with 1 (after untag) + SETZ/CSET

#### `oddp` - Test for Odd
```lisp
(oddp 5)         ; => 1 (true)
(oddp 4)         ; => 0 (false)
```
**Implementation**: AND with 1 (after untag) + SETNZ/CSET

#### `null` - Test for NIL
```lisp
(null 0)         ; => 1 (true, 0 is nil)
(null 5)         ; => 0 (false)
```
**Implementation**: Same as `zerop`

#### `numberp` - Test for Number
```lisp
(numberp 5)      ; => 1 (always true for fixnums)
(numberp 0)      ; => 1
```
**Implementation**: Always returns 1 (fixnum-only system)

#### `integerp` - Test for Integer
```lisp
(integerp 5)     ; => 1 (always true for fixnums)
(integerp -10)   ; => 1
```
**Implementation**: Always returns 1 (fixnum-only system)

#### `atom` - Test for Atom
```lisp
(atom 5)         ; => 1 (always true for fixnums)
```
**Implementation**: Always returns 1 (no cons cells yet)

#### `listp` - Test for List
```lisp
(listp 5)        ; => 0 (always false for fixnums)
```
**Implementation**: Always returns 0 (fixnum-only system)

#### `consp` - Test for Cons Cell
```lisp
(consp 5)        ; => 0 (always false for fixnums)
```
**Implementation**: Always returns 0 (no cons cells yet)

#### `symbolp` - Test for Symbol
```lisp
(symbolp 5)      ; => 0 (always false for fixnums)
```
**Implementation**: Always returns 0 (fixnum-only system)

### Control Flow (7 operators)

#### `if` - Conditional
```lisp
(if (< 5 10) 100 200)     ; => 100
(if (> 5 10) 100 200)     ; => 200
(if 0 100 200)            ; => 200 (0 is false)
```
**Implementation**: Test condition + conditional jump

#### `cond` - Multi-branch Conditional
```lisp
(cond
  ((< x 0) -1)
  ((> x 0) 1)
  (t 0))
```
**Implementation**: Chain of if-statements

#### `case` - Pattern Matching
```lisp
(case x
  (1 'one)
  (2 'two)
  (t 'other))
```
**Implementation**: Series of comparisons with optional jump table

#### `when` - Conditional Execution
```lisp
(when (< 5 10)
  (+ 1 2)
  (+ 3 4))        ; => 7 (returns last expression)
```
**Implementation**: If without else branch

#### `unless` - Inverted Conditional
```lisp
(unless (> 5 10)
  (+ 1 2))        ; => 3 (executes when condition is false)
```
**Implementation**: If with inverted condition

#### `progn` - Sequential Execution
```lisp
(progn
  (+ 1 2)
  (+ 3 4)
  (+ 5 6))        ; => 11 (returns last value)
```
**Implementation**: Execute each expression, return last

#### `begin` - Sequential Execution (alias for `progn`)
```lisp
(begin
  (+ 1 2)
  (+ 3 4))        ; => 7
```
**Implementation**: Same as `progn`

### Variables and Scope (5 operators)

#### `let` - Local Variables
```lisp
(let ((x 5)
      (y 10))
  (+ x y))        ; => 15
```
**Implementation**: Stack allocation for local variables

#### `let*` - Sequential Local Variables
```lisp
(let* ((x 5)
       (y (+ x 3)))
  (+ x y))        ; => 13 (y can reference x)
```
**Implementation**: Each binding can reference previous bindings

#### `setq` - Variable Assignment
```lisp
(let ((x 5))
  (setq x 10)
  x)              ; => 10
```
**Implementation**: Stack slot assignment

#### `incf` - Increment Variable
```lisp
(let ((x 5))
  (incf x)
  x)              ; => 6

(let ((x 5))
  (incf x 3)
  x)              ; => 8
```
**Implementation**: Add to stack slot (default increment: 1)

#### `decf` - Decrement Variable
```lisp
(let ((x 5))
  (decf x)
  x)              ; => 4

(let ((x 5))
  (decf x 2)
  x)              ; => 3
```
**Implementation**: Subtract from stack slot (default decrement: 1)

### Functions (2 operators)

#### `lambda` - Anonymous Function
```lisp
((lambda (x) (+ x 1)) 5)           ; => 6
((lambda (x y) (* x y)) 3 4)       ; => 12
```
**Implementation**: Inline expansion at compile time
**Note**: No closure support yet

#### `defun` - Define Function
```lisp
(defun square (x) (* x x))
(square 5)                          ; => 25
```
**Implementation**: Inline expansion at compile time
**Note**: Recursive calls work but no TCO yet

### Macros (1 operator)

#### `defmacro` - Define Macro
```lisp
(defmacro when (test &rest body)
  `(if ,test (progn ,@body)))

(when (< 5 10) (+ 1 2))            ; => 3
```
**Implementation**: Compile-time expansion with backquote/comma
**Note**: Full Common Lisp macro system

### Data Operations (5 operators)

#### `quote` - Quote Expression
```lisp
(quote x)        ; => symbol 'x (not evaluated)
'x               ; => same as (quote x)
'(1 2 3)         ; => list (1 2 3)
```
**Implementation**: Prevent evaluation

#### `cons` - Construct Pair
```lisp
(cons 1 2)       ; => (1 . 2) - REPL only
```
**Implementation**: Heap allocation (REPL only, not in compiled code yet)
**Status**: Requires runtime integration

#### `car` - First of Pair
```lisp
(car (cons 1 2)) ; => 1 - REPL only
```
**Implementation**: Memory read (REPL only)
**Status**: Requires runtime integration

#### `cdr` - Rest of Pair
```lisp
(cdr (cons 1 2)) ; => 2 - REPL only
```
**Implementation**: Memory read (REPL only)
**Status**: Requires runtime integration

#### `list` - Construct List
```lisp
(list 1 2 3)     ; => (1 2 3) - REPL only
```
**Implementation**: Nested cons (REPL only)
**Status**: Requires runtime integration

### Utility (1 operator)

#### `identity` - Return Argument Unchanged
```lisp
(identity 5)     ; => 5
(identity 'foo)  ; => 'foo
```
**Implementation**: No-op that returns input

## Implementation Notes

### Architecture Support

All operators are implemented for both architectures:

**x86_64**:
- Standard System V AMD64 ABI
- Uses RAX, RBX, RCX, RDX registers
- Stack-based parameter passing
- IDIV for division
- Conditional moves (CMOV) for optimization

**ARM64**:
- ARM64 calling convention
- Uses X0-X9 registers
- Stack-based parameter passing
- SDIV for division, MSUB for remainder
- Conditional select (CSEL) for optimization

### Performance Characteristics

**Fast Operations** (1-5 instructions):
- Arithmetic: +, -, *, 1+, 1-
- Comparison: <, >, =, <=, >=, /=
- Bitwise: logand, logior, logxor, lognot
- Predicates: zerop, plusp, minusp
- Utility: identity

**Medium Operations** (6-15 instructions):
- Division: /, mod, rem
- Min/max: min, max
- Abs: abs
- Bitwise shift: ash
- Bitwise test: logtest

**Complex Operations** (16+ instructions):
- Sign: signum (~20 instructions)
- Population count: logcount (~15 instructions in loop)
- Control flow: if, cond, case (varies)
- Functions: lambda, defun (inline expansion)

### Compiler Optimizations

**Constant Folding** (implemented):
- Evaluates constant expressions at compile time
- Reduces code size and improves runtime performance
- Examples:
  - `(+ 2 3)` compiles directly to the constant `5`
  - `(* 10 (+ 3 2))` compiles to `50`
  - `(if 0 x y)` eliminates dead branch, compiles only `y`
- Supported operations: arithmetic, comparisons, bitwise, min/max, abs, gcd, lcm
- Recursive folding: nested constant expressions fully evaluated

**Benefits**:
- Smaller generated code (fewer machine code instructions)
- Faster compilation (less code generation needed)
- Better runtime performance (no overhead for constant computation)
- Dead code elimination for constant conditionals

### Limitations

1. **Fixnum only**: No floating point or bignum support
2. **No closures**: Lambda cannot capture variables
3. **No TCO**: Recursive functions use stack space
4. **Inline expansion**: All functions expanded at compile time
5. **No runtime heap**: cons/car/cdr work in REPL only

### Future Additions

Planned operators (see docs/NEXT_STEPS.md):
- Heap-based list operations: runtime integration needed
- Floating point: `sqrt`, `sin`, `cos`, `tan` (requires FPU support)
- String operations: requires heap and string objects
- `random` - Random number generation

## Testing

All operators have comprehensive test coverage:

**Test Categories**:
- Literals: 4 tests
- Arithmetic: 7 tests
- Comparison: 5 tests
- Boolean Operators: 6 tests
- Conditionals: 7 tests
- Variables and Let: 6 tests
- Let* Sequential Bindings: 5 tests
- Lambda and Functions: 6 tests
- Progn/Begin: 5 tests
- Quote: 4 tests
- Bitwise Operators: 7 tests
- Numeric Operators: 7 tests
- Predicates: 14 tests
- Utility Functions: 5 tests
- Case Pattern Matching: 3 tests
- Defun: 8 tests
- Setq: 8 tests
- Incf/Decf: 5 tests
- Additional Comparisons: 4 tests
- Complex Expressions: 6 tests
- Macros: 5 tests
- Error Handling: 2 tests

**Total**: 134 compiler tests (100% passing)

## Examples

### Factorial
```lisp
(defun factorial (n)
  (if (zerop n)
      1
      (* n (factorial (1- n)))))

(factorial 5)  ; => 120
```

### FizzBuzz Decision
```lisp
(defun fizzbuzz (n)
  (cond
    ((zerop (mod n 15)) 'fizzbuzz)
    ((zerop (mod n 3)) 'fizz)
    ((zerop (mod n 5)) 'buzz)
    (t n)))

(fizzbuzz 15)  ; => 'fizzbuzz
```

### Bit Manipulation
```lisp
(defun set-bit (n pos)
  (logior n (ash 1 pos)))

(defun clear-bit (n pos)
  (logand n (lognot (ash 1 pos))))

(defun test-bit (n pos)
  (logtest n (ash 1 pos)))
```

### Absolute Difference
```lisp
(defun abs-diff (a b)
  (abs (- a b)))

(abs-diff 10 3)   ; => 7
(abs-diff 3 10)   ; => 7
```

### Clamp Value
```lisp
(defun clamp (x min-val max-val)
  (min (max x min-val) max-val))

(clamp 15 0 10)   ; => 10
(clamp -5 0 10)   ; => 0
(clamp 5 0 10)    ; => 5
```

## References

- **Source**: bootstrap/compiler.lisp
- **Tests**: bootstrap/run-all-tests.lisp
- **Documentation**: docs/NEXT_STEPS.md, docs/TCO_DESIGN.md
- **Session Context**: SESSION_CONTEXT.md

---

**Last Updated**: 2025-11-18
**Version**: Habu Lisp v4
**Total Operators**: 66
**Test Pass Rate**: 100% (134/134 compiler, 166/166 runtime, 300/300 total)
