# Manual Bootstrap Plan - Phase 3.2

## Strategy: Hand-Write Compiler Core in ARM64

**Goal:** Create minimal compiler by hand-writing critical functions in ARM64 bytecode, then use it to compile the full compiler.

---

## Phase Overview

### Step 1: Hand-Write Compiler Core (3-4 days)
Write ~20-30 critical functions in ARM64 bytecode

### Step 2: Create Minimal Compiler Binary (1 day)
Link hand-written functions into executable

### Step 3: Bootstrap (2 days)
Use minimal compiler to compile full compiler, achieve fixed point

**Total Estimated Time:** 6-7 days

---

## Critical Functions to Hand-Write

### Tier 1: Essential Primitives (Day 1)
**Goal:** Basic building blocks

1. **List Operations** (5 functions)
   - `car` - Get first element
   - `cdr` - Get rest
   - `cons` - Build pair
   - `nil?` - Check for nil
   - `cons?` - Check for cons cell

2. **Arithmetic** (4 functions)
   - `+` - Addition
   - `-` - Subtraction
   - `*` - Multiplication
   - `/` - Division

3. **Comparison** (3 functions)
   - `=` - Equality
   - `<` - Less than
   - `>` - Greater than

**Complexity:** Simple - mostly direct runtime calls
**Lines of bytecode:** ~100-150 lines
**Verification:** Test each function individually

### Tier 2: ARM64 Encoders (Day 2)
**Goal:** Generate machine code

4. **Instruction Encoders** (8 functions)
   - `arm64-movz` - Load immediate
   - `arm64-add` - Add registers
   - `arm64-sub` - Subtract registers
   - `arm64-mul` - Multiply registers
   - `arm64-ldr` - Load from memory
   - `arm64-str` - Store to memory
   - `arm64-stp` - Store pair
   - `arm64-ldp` - Load pair

5. **Control Flow Encoders** (4 functions)
   - `arm64-b` - Branch
   - `arm64-bl` - Branch with link
   - `arm64-ret` - Return
   - `arm64-cmp` - Compare

**Complexity:** Medium - bit manipulation
**Lines of bytecode:** ~200-300 lines
**Verification:** Generate known instructions, compare with working patterns

### Tier 3: IR Generation (Day 3)
**Goal:** Parse Lisp → Internal Representation

6. **compile-expr** (1 large function)
   - Handle literals
   - Handle variables
   - Handle operations (+, -, *, /)
   - Handle if expressions
   - Handle let bindings
   - Handle function calls

**Complexity:** High - recursive, many cases
**Lines of bytecode:** ~150-250 lines
**Verification:** Compile simple expressions, check IR output

### Tier 4: Code Generation (Day 4)
**Goal:** IR → ARM64 bytecode

7. **codegen-expr** (1 large function)
   - Generate code for literals
   - Generate code for variables
   - Generate code for operations
   - Generate code for if
   - Generate code for let
   - Generate code for calls

8. **Helper Functions** (5 functions)
   - `codegen-binary-op` - Generate binary operations
   - `codegen-if` - Generate conditionals
   - `codegen-let` - Generate let bindings
   - `env-lookup` - Look up variables
   - `make-runtime-addrs` - Create address table

**Complexity:** High - generates complex instruction sequences
**Lines of bytecode:** ~200-300 lines
**Verification:** Compile simple programs, execute and verify results

---

## Function Dependency Graph

```
Tier 1: Primitives
  ├─ car, cdr, cons, nil?, cons?
  ├─ +, -, *, /
  └─ =, <, >

Tier 2: Encoders (depends on Tier 1 for arithmetic)
  ├─ arm64-movz, arm64-add, arm64-sub, arm64-mul
  ├─ arm64-ldr, arm64-str, arm64-stp, arm64-ldp
  └─ arm64-b, arm64-bl, arm64-ret, arm64-cmp

Tier 3: IR Generation (depends on Tier 1)
  └─ compile-expr
      ├─ Uses: car, cdr, cons, nil?, cons?
      └─ Recursive

Tier 4: Code Generation (depends on Tier 2 & 3)
  └─ codegen-expr
      ├─ Uses: All ARM64 encoders
      ├─ Uses: compile-expr output
      └─ Produces: Bytecode
```

---

## Implementation Strategy

### Day 1: Tier 1 - Primitives

**Morning:**
1. Create `bootstrap/primitives.c`
2. Implement car, cdr, cons
3. Test with simple list operations

**Afternoon:**
4. Implement arithmetic (+, -, *, /)
5. Implement comparisons (=, <, >)
6. Create test suite: `test-bootstrap-primitives.c`
7. Verify all Tier 1 functions work

**Deliverable:** 12 working primitive functions

### Day 2: Tier 2 - ARM64 Encoders

**Morning:**
1. Create `bootstrap/arm64-encoders.c`
2. Implement data movement (movz, ldr, str, stp, ldp)
3. Test: Generate known instructions, compare bytes

**Afternoon:**
4. Implement arithmetic (add, sub, mul)
5. Implement control flow (b, bl, ret, cmp)
6. Create test suite: `test-bootstrap-encoders.c`
7. Verify encodings match known patterns

**Deliverable:** 12 working encoder functions

### Day 3: Tier 3 - IR Generation

**Morning:**
1. Create `bootstrap/ir-generation.c`
2. Implement compile-expr structure
3. Handle literals and variables

**Afternoon:**
4. Handle operations (+, -, *, /)
5. Handle if expressions
6. Handle let bindings
7. Handle function calls
8. Create test suite: `test-bootstrap-ir.c`

**Deliverable:** Working compile-expr function

### Day 4: Tier 4 - Code Generation

**Morning:**
1. Create `bootstrap/code-generation.c`
2. Implement codegen-expr structure
3. Generate code for literals and variables

**Afternoon:**
4. Generate code for operations
5. Generate code for if
6. Generate code for let
7. Generate code for calls
8. Create test suite: `test-bootstrap-codegen.c`

**Deliverable:** Working codegen-expr function

---

## Implementation Approach

### For Each Function:

1. **Analyze Lisp Source**
   - Read original function from habu-arm64-codegen.lisp
   - Understand algorithm
   - Identify dependencies

2. **Design Bytecode**
   - Plan register usage
   - Plan stack layout
   - Identify control flow

3. **Write Bytecode**
   - Hand-write ARM64 instructions
   - Comment extensively
   - Match existing patterns

4. **Test**
   - Create unit test
   - Verify output
   - Compare with expected behavior

5. **Document**
   - Add to function registry
   - Document calling convention
   - Note any quirks

---

## Bytecode Template

Each function follows this pattern:

```c
/* Function: function-name
 * Args: x0, x1, ... (ARM64 ABI)
 * Returns: x0
 * Stack: N bytes
 */
uint8_t function_name_code[] = {
    /* Prologue - safe stack pattern */
    0xFF, 0x83, 0x00, 0xD1,  /* sub sp, sp, #32 */
    0xFD, 0x7B, 0x00, 0xA9,  /* stp x29, x30, [sp, #0] */
    0xFD, 0x03, 0x00, 0x91,  /* mov x29, sp */

    /* Function body */
    // ... implementation ...

    /* Epilogue */
    0xFD, 0x7B, 0x40, 0xA9,  /* ldp x29, x30, [sp, #0] */
    0xFF, 0x83, 0x00, 0x91,  /* add sp, sp, #32 */
    0xC0, 0x03, 0x5F, 0xD6,  /* ret */
};
```

---

## Test Strategy

### Unit Tests
Each function gets individual test:
- Test-driven: Write test first
- Verify correct output
- Test edge cases
- Test with known inputs

### Integration Tests
After each tier:
- Test tier together
- Verify interactions
- Test realistic scenarios

### End-to-End Test
After all tiers:
- Compile simple program
- Execute compiled code
- Verify result matches expected

---

## Milestone Checkpoints

### Checkpoint 1: After Day 1
- [ ] All Tier 1 primitives working
- [ ] Can perform basic list and arithmetic operations
- [ ] Unit tests passing

### Checkpoint 2: After Day 2
- [ ] All ARM64 encoders working
- [ ] Can generate known instruction patterns
- [ ] Bytecode matches verified examples

### Checkpoint 3: After Day 3
- [ ] compile-expr working
- [ ] Can parse Lisp to IR
- [ ] Handles all expression types

### Checkpoint 4: After Day 4 (Minimal Compiler Complete!)
- [ ] codegen-expr working
- [ ] Can generate ARM64 from IR
- [ ] Can compile simple programs end-to-end
- [ ] Compiled programs execute correctly

---

## Bootstrap Sequence (Days 5-6)

### Day 5: Use Minimal Compiler

**Morning:**
1. Create driver program
2. Load minimal compiler functions
3. Test compiling simple expressions
4. Test compiling simple functions

**Afternoon:**
5. Compile full compiler source (Stage 1)
6. Link compiled output
7. Create compiler1 binary
8. Test compiler1

**Deliverable:** compiler1 (compiled by minimal compiler)

### Day 6: Achieve Fixed Point

**Morning:**
1. compiler1 compiles full compiler → compiler2
2. Compare compiler1 and compiler2 bytecode
3. Debug any differences

**Afternoon:**
4. compiler2 compiles full compiler → compiler3
5. Verify: compiler2 == compiler3 (fixed point!)
6. 🎉 **SELF-HOSTING ACHIEVED**
7. Documentation and celebration

**Deliverable:** Self-hosting compiler, fixed point achieved

---

## Success Criteria

### Tier 1 Success:
- ✅ All primitive functions execute correctly
- ✅ Can perform list operations
- ✅ Arithmetic works
- ✅ Comparisons work

### Tier 2 Success:
- ✅ All encoders produce correct bytecode
- ✅ Generated instructions match known patterns
- ✅ Can build complex instruction sequences

### Tier 3 Success:
- ✅ compile-expr handles all expression types
- ✅ Produces correct IR
- ✅ Recursive compilation works

### Tier 4 Success:
- ✅ codegen-expr produces executable bytecode
- ✅ Simple programs compile and run
- ✅ Output matches expected results

### Final Success:
- ✅ Minimal compiler works end-to-end
- ✅ Can compile full compiler
- ✅ Bootstrap achieves fixed point
- ✅ Compiler can compile itself repeatedly

---

## Risk Mitigation

### Risk 1: Bytecode Errors
**Mitigation:**
- Write tests first
- Verify each instruction
- Compare with working examples
- Test incrementally

### Risk 2: Complex Functions
**Mitigation:**
- Break into smaller pieces
- Implement iteratively
- Test each piece
- Use helper functions

### Risk 3: Integration Issues
**Mitigation:**
- Test tiers together
- Verify interfaces
- Document calling conventions
- Create integration tests

### Risk 4: Time Overruns
**Mitigation:**
- Focus on essentials first
- Skip optimizations initially
- Iterate: working > perfect
- Adjust scope if needed

---

## File Organization

```
bootstrap/
├── primitives.c              # Tier 1: List ops, arithmetic, comparison
├── arm64-encoders.c          # Tier 2: Instruction encoders
├── ir-generation.c           # Tier 3: compile-expr
├── code-generation.c         # Tier 4: codegen-expr
├── minimal-compiler.c        # Main driver
├── runtime-integration.c     # Runtime address tables
└── tests/
    ├── test-primitives.c
    ├── test-encoders.c
    ├── test-ir.c
    ├── test-codegen.c
    └── test-integration.c
```

---

## Progress Tracking

### Daily Log Format:
```
Date: YYYY-MM-DD
Tier: N
Goal: What we're implementing
Progress:
  - [ ] Task 1
  - [ ] Task 2
  - [ ] Task 3
Completed:
  - [x] Completed task
Blockers:
  - Issue description
Notes:
  - Important observations
```

---

## Next Steps

1. **Create bootstrap directory structure**
2. **Start with Tier 1: Primitives**
3. **Implement car/cdr/cons first** (simplest)
4. **Test each function as we go**
5. **Build up systematically**

---

## Estimated Timeline

- **Day 1:** Tier 1 Primitives (12 functions)
- **Day 2:** Tier 2 ARM64 Encoders (12 functions)
- **Day 3:** Tier 3 IR Generation (1 large function)
- **Day 4:** Tier 4 Code Generation (6 functions)
- **Day 5:** Minimal Compiler Integration & Stage 1
- **Day 6:** Bootstrap to Fixed Point & Verification
- **Day 7:** Buffer for debugging & documentation

**Total:** 6-7 days to self-hosting

---

**Status:** Ready to begin Tier 1 implementation
**Next Action:** Create bootstrap directory and start with primitives

---

**End of Manual Bootstrap Plan**
**Date:** November 21, 2025
**Approach:** Manual Bootstrap (Option 3)
**Timeline:** 6-7 days to self-hosting
