# Compiler Theory References for Habu

**Date**: December 5, 2025
**Status**: Design references and research materials

## Overview

This directory contains reference specifications for high-performance Lisp compiler design, adapted for potential integration into Habu. These documents describe a nanopass compiler architecture targeting ARM64 with C-like performance.

## Documents

### 1. NANOPASS_COMPILER_SPEC.md

Complete specification of a nanopass compiler pipeline with:
- Type inference and specialization
- Unboxing analysis
- Register allocation via linear scan
- GC safe points and stack maps
- ARM64 code generation

**Key opportunities for Habu**:
- Replace accumulator model with register allocation (5-20x speedup)
- Add type inference for arithmetic specialization (2-5x speedup)
- Implement unboxing for numeric loops (5-10x speedup)
- Total potential: 50-200x on numeric/loop-heavy code

### 2. LISPY_ENCODING.md

S-expression encodings for all compiler data structures:
- Type system (lattice with fixnum, double, pair, vector, etc.)
- High-level IR (ANF + CFG)
- Machine IR (virtual registers, operations)
- Metadata (type info, representation info, GC maps)
- Register allocation results

**Key benefit**: Natural Lisp representation enables meta-circular compilation

### 3. SPEC_CONVERSATION_RECORD.md

Summary of the design conversation that produced these specifications. Provides context and rationale for design decisions.

## Comparison: Spec vs. Habu Current

| Component | Spec | Habu Current | Gap |
|-----------|------|--------------|-----|
| **Tagged format** | 3-bit tags | 4-bit tags | Minor |
| **IR structure** | ANF + CFG | Tree | Major |
| **Type inference** | Full lattice | None | Major |
| **Unboxing** | Int64/double | None | Critical |
| **Register allocation** | Linear scan | Accumulator | Critical |
| **GC stack maps** | Precise | Basic | Medium |
| **TCO** | Formalized | Basic | Minor |

## Priority Recommendations

### Phase 1: Critical Performance (Highest ROI)

1. **Register allocation** (`bootstrap/reg-alloc.lisp` - in progress)
   - Move from accumulator to register model
   - Expected speedup: 5-20x
   - Status: Foundation in place, needs completion

2. **Type inference for arithmetic**
   - Track fixnum vs. other types
   - Specialize +, -, *, / for fixnums
   - Expected speedup: 2-5x
   - Complexity: Medium

3. **Basic unboxing**
   - Unbox fixnums in tight loops
   - Eliminate tagging overhead
   - Expected speedup: 5-10x on numeric code
   - Complexity: High (requires type inference)

### Phase 2: Infrastructure (Enables Future Opts)

4. **ANF conversion**
   - Convert tree IR to A-Normal Form
   - Enables dataflow analysis
   - Foundation for all other passes

5. **CFG construction**
   - Build control flow graph
   - Enables advanced optimizations
   - Required for loop analysis

6. **Precise GC stack maps**
   - Track all GC roots precisely
   - Better GC performance
   - Required for moving GC

### Phase 3: Advanced Optimizations

7. **Branch refinement**
   - Refine types based on predicates
   - Optimize type checks

8. **Loop optimizations**
   - Invariant code motion
   - Strength reduction
   - Unrolling

9. **Inlining**
   - Inline small functions
   - Eliminate call overhead

## Integration Strategy

### Incremental Approach

Rather than rewriting the entire compiler, integrate these ideas incrementally:

1. **Add new passes alongside existing code**
   - Keep current compiler working
   - Add optional optimization passes
   - Compare performance

2. **Use feature flags**
   - Enable new passes selectively
   - A/B test performance
   - Gradual rollout

3. **Measure everything**
   - Benchmark each pass
   - Verify correctness
   - Track regressions

### Migration Path

```
Current:
  Source → Expand → Compile → Codegen → ARM64

Phase 1 (RA):
  Source → Expand → Compile → MIR → RA → Codegen → ARM64

Phase 2 (Type Inference):
  Source → Expand → Compile → TypeInfer → Specialize → MIR → RA → Codegen → ARM64

Phase 3 (Full Pipeline):
  Source → Expand → ANF → CFG → TypeInfer → Refine → Specialize →
  ChooseRep → Box/Unbox → TCO → MIR → Liveness → RA → StackMaps → Codegen → ARM64
```

## Expected Performance Impact

### Current Habu Performance Characteristics

- **Accumulator model**: Every subexpression spills to stack
- **Fixed 2KB frames**: Even for simple functions
- **Fully tagged arithmetic**: 4-bit tag overhead on every operation
- **No specialization**: Generic dispatch for all operations

### With Phase 1 Improvements

- **Register allocation**: 10-100x fewer memory operations
- **Right-sized frames**: Save stack space, improve locality
- **Expected overall**: 5-20x speedup

### With Phase 1-2 Improvements

- **Type specialization**: Direct fixnum operations
- **Unboxed arithmetic**: No tagging in inner loops
- **Expected overall**: 50-200x speedup on numeric code
- **Match or exceed C performance** on hot loops

## Testing Strategy

1. **Correctness first**
   - Run full test suite after each pass
   - Compare output to current compiler
   - Verify all edge cases

2. **Performance validation**
   - Micro-benchmarks for each optimization
   - Real-world programs
   - Compare to SBCL, CCL

3. **Regression prevention**
   - Automated performance tests
   - Alert on slowdowns
   - Track metrics over time

## Related Habu Documentation

- `/Users/joel/Work/habu/docs/codegen/COMPILATION_ARCHITECTURE.md` - Current architecture
- `/Users/joel/Work/habu/bootstrap/reg-alloc.lisp` - Register allocation (in progress)
- `/Users/joel/Work/habu/bootstrap/compiler.lisp` - Current compiler
- `/Users/joel/Work/habu/bootstrap/codegen.lisp` - Current code generator
- `/Users/joel/Work/habu/docs/architecture/ARCHITECTURE.md` - Overall system architecture

## Future Work

### Short-term (1-3 months)

- [ ] Complete register allocation pass
- [ ] Basic type inference for fixnum operations
- [ ] Benchmark suite for compiler performance

### Medium-term (3-6 months)

- [ ] ANF conversion
- [ ] Full type inference with lattice
- [ ] Primitive specialization
- [ ] Unboxing pass

### Long-term (6-12 months)

- [ ] CFG construction
- [ ] Loop optimizations
- [ ] Inlining
- [ ] Full nanopass pipeline

## References

These specifications are based on:
- Chez Scheme's nanopass framework
- SBCL's compiler optimization strategies
- Modern compiler construction techniques (LLVM, GCC)
- Research on typed Lisps (Typed Racket, Chez Scheme, MLton)

The goal is to bring Habu from "interpreter-level performance" to "C-level performance" for compiled code.

## Contributing

When implementing these ideas:

1. Start small - one pass at a time
2. Measure performance impact
3. Document trade-offs
4. Keep it simple - complex optimizations can wait
5. Prioritize correctness over speed

The best compiler is one that works correctly and generates fast code, in that order.
