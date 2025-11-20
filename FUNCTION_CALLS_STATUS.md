# Function Calls Implementation Status

## Overview

Work in progress to add function definitions (defun) and function calls with BL instruction to the Habu ARM64 compiler.

## Completed Work ✅

### 1. BL Instruction Encoder (arm64-bl)
- **Location**: `habu-arm64-codegen.lisp:269-277`
- **Implementation**: Branch with Link instruction
- **Encoding**: `0x94000000 | offset`
- **Status**: ✅ Complete

```lisp
(defun arm64-bl (offset)
  "BL <label> - Branch with link (function call)
   Base: 0x94000000 | offset"
  (let ((base 0x94000000))
    (let ((encoded (+ base offset)))
      (encode-word encoded))))
```

### 2. Function Environment Infrastructure
- **Location**: `habu-arm64-codegen.lisp:650-720`
- **Functions Added**:
  - `fenv-lookup` - Look up function in function environment
  - `fenv-extend` - Add function to environment
  - `compile-param-bindings` - Create parameter bindings for functions
  - `count-params` - Count function parameters
  - `codegen-save-params-helper` - Generate code to save x0-x2 to stack
  - `codegen-function` - Generate complete function with prologue/epilogue
- **Status**: ✅ Complete

### 3. Multi-Form Compilation
- **Location**: `habu-arm64-codegen.lisp:964-1079`
- **Functions Added**:
  - `compile-defun` - Compile a function definition to IR
  - `compile-forms-helper` - Separate defuns from main code
  - `compile-forms` - Entry point for multi-form compilation
  - `codegen-functions-helper` - Generate machine code for all functions
  - `codegen-function-with-params` - Generate function code with parameter handling
  - `compile-program-with-functions` - Complete pipeline for programs with functions
- **Status**: ✅ Complete

### 4. Function Environment Threading
- **Changed**: All `compile-expr` calls and related functions
- **Impact**: `compile-expr`, `compile-progn-list`, `compile-lambda-args`, `compile-let-bindings`, `compile-cond-clauses` all now take `fenv` parameter
- **Purpose**: Allow compile-expr to recognize defined functions vs built-in operators
- **Status**: ✅ Complete

### 5. Lambda Expression Support
- **Location**: `habu-arm64-codegen.lisp:813-932`
- **Features**:
  - `(lambda (x) body)` IR form
  - Lambda application: `((lambda (x) (+ x 1)) 5)`
  - Compiles to let-multi bindings (inline, no BL)
- **Status**: ✅ Complete

### 6. Function Call IR Form
- **Location**: `habu-arm64-codegen.lisp:945-948`
- **IR**: `(fncall fname (arg1-ir arg2-ir ...))`
- **Recognition**: In compile-expr, checks fenv to distinguish function calls from built-in operations
- **Status**: ✅ IR generation complete, codegen in progress

## In Progress ⏳

### 7. Function Call Code Generation
- **Location**: `habu-arm64-codegen.lisp:417-424`
- **Current State**: Placeholder added
- **Needs**:
  1. Generate code to evaluate each argument
  2. Place arguments in registers x0-x7 (ARM64 calling convention)
  3. Calculate BL offset to target function
  4. Generate BL instruction
  5. Result returns in x0 (already tagged)

**Pseudocode**:
```lisp
(defun codegen-fncall (fname args-ir fn-offsets current-offset)
  (let ((arg-code (codegen-eval-args-to-regs args-ir)))
    (let ((target-offset (lookup-fn-offset fname fn-offsets)))
      (let ((bl-offset (- target-offset current-offset)))
        (let ((bl-instr (arm64-bl bl-offset)))
          (append-code arg-code bl-instr))))))
```

## Remaining Work 📋

### 8. Argument Evaluation and Register Placement
- **Need**: `codegen-eval-args-to-regs` function
- **Requirements**:
  - Evaluate first arg → x0
  - Evaluate second arg → x1
  - Evaluate third arg → x2
  - etc. up to x7
  - Handle > 8 args via stack (future work)

### 9. Function Offset Tracking
- **Challenge**: During codegen, need to know:
  - Where each function is located (offset in instructions)
  - Current position in generated code
- **Solution**: Thread `fn-offsets` table through codegen functions
- **Impact**: Need to update codegen-expr signature and all calls

### 10. Offset Calculation
- **Challenge**: BL offset is signed 26-bit in instruction units
- **Formula**: `bl_offset = (target_instr_addr - current_instr_addr) / 4`
- **Requirement**: Track instruction counts accurately

### 11. Stack Depth Tracking (Optional for v1)
- **Issue**: For nested expressions with function calls, stack pointer moves
- **Impact**: Variable references during function call evaluation
- **Solution**: Use frame pointer (x29) for variable references, or track stack depth
- **Priority**: Medium (can workaround with simple examples first)

### 12. Testing
- Create tests for:
  - Simple function: `(defun inc (x) (+ x 1))` called with `(inc 5)`
  - Two parameters: `(defun add (x y) (+ x y))`
  - Nested calls: `(defun double (x) (+ x x))` then `(inc (double 3))`
  - Recursive functions (requires more work)

## Architecture Notes

### Compilation Pipeline

```
User Code:
  (defun inc (x) (+ x 1))
  (inc 5)

     ↓

compile-forms:
  functions: [(inc 1 (call + (var 0) (lit 1)))]
  main-ir: (fncall inc ((lit 5)))

     ↓

codegen-functions-helper:
  Generates code for all functions
  Tracks offsets: [(inc 0)]

     ↓

codegen-main (with fn-offsets):
  Generates main code
  fncall uses fn-offsets to calculate BL offset

     ↓

Machine Code:
  [function code...]
  [main code with BL to function]
```

### ARM64 Calling Convention

- **Parameters**: x0-x7 (first 8 arguments)
- **Return value**: x0
- **Preserved**: x19-x28, x29 (FP), x30 (LR)
- **Temporary**: x0-x18
- **Stack**: 16-byte aligned

### Current Stack Layout in Functions

```
After prologue + param saves:
  [sp + 0]  = param 0 (from x0)
  [sp + 16] = param 1 (from x1)
  [sp + 32] = param 2 (from x2)
  ...
```

## Design Decisions

1. **Functions compiled first**: Ensures known offsets for BL calls
2. **Parameters via registers**: Follow ARM64 ABI, save to stack in function prologue
3. **Tagged values throughout**: Even in registers, maintain fixnum tagging
4. **No stack overflow for args**: v1 limited to 3 params (x0-x2)
5. **No closures yet**: Functions don't capture free variables

## File Statistics

- **Total lines**: ~1079 (up from 750)
- **New functions**: 15+
- **Modified functions**: ~10
- **New IR forms**: `(fncall fname args)`, `(lambda params body)`

## Next Immediate Steps

1. ✅ Mark current progress in TODO
2. ⏳ Implement `codegen-eval-args-to-regs`
3. ⏳ Thread `fn-offsets` through codegen-expr
4. ⏳ Complete fncall codegen with BL
5. ⏳ Create simple test case
6. ⏳ Verify generated machine code

## Challenges Encountered

1. **Threading fenv**: Required updating ~30 function calls
2. **let-multi stack offsets**: Variable references complex during binary operations
3. **Offset calculation**: Need accurate instruction counting
4. **Testing without REPL**: habu interpreter hangs, using C test harnesses

## Status

**Overall**: ~60% complete for basic function calls
**Blockers**: None, implementation in progress
**Risk**: Complexity of threading offsets, testing without working REPL

---

**Last Updated**: 2025-11-20
**Compiler**: habu-arm64-codegen.lisp
**Target**: ARM64 (Apple Silicon)
