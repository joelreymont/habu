---
title: Optimize tag format to 3-bit tags
status: closed
priority: 4
issue-type: task
assignee: ""
created-at: "2025-12-05T21:03:07.013307+02:00"
closed-at: "2025-12-25 07:22:05"
close-reason: "Obsolete: Zig rewrite"
---

## Context

Habu currently uses 4-bit tags (value << 4), which wastes one bit compared to the optimal 3-bit tag scheme.

## Goal

Migrate from 4-bit to 3-bit tags, gaining one additional bit for fixnum range.

## Current Scheme (4-bit)

```
Value: xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xxxx (60 bits)
Tag:                                                          0001 (fixnum)
                                                             0000 (pointer)
```

Fixnum range: ±2^59

## Proposed Scheme (3-bit)

```
Value: xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx xxx (61 bits)
Tag:                                                               001 (fixnum)
                                                                  000 (pointer)
                                                                  010 (reserved)
                                                                  011 (reserved)
```

Fixnum range: ±2^60 (2x current)

## Benefits

1. **Larger fixnum range**: 2x (rarely matters in practice)
2. **Alignment with spec**: Matches standard Lisp implementation
3. **More tag space**: 2 additional tag patterns available

## Costs

1. **Breaking change**: Requires updating all tag operations
2. **Runtime changes**: Boxing/unboxing code
3. **Testing**: Comprehensive verification needed

## Implementation Tasks

1. **Update primitives**
   - Boxing: `(n << 3) | 1` instead of `(n << 4)`
   - Unboxing: `n >> 3` instead of `n >> 4`

2. **Update codegen**
   - ARM64 shift amounts: `lsl #3` instead of `lsl #4`
   - Tag checks: `and #0x7` instead of `and #0xF`

3. **Update runtime**
   - GC tag checking
   - Type predicates
   - Pointer alignment (8-byte instead of 16-byte)

4. **Update tests**
   - Verify all tag operations
   - Check edge cases (max/min fixnum)

## Migration Strategy

### Option 1: Flag-based migration
- Add compile flag: `TAG_BITS=3` or `TAG_BITS=4`
- Keep both implementations for a while
- Switch default after testing

### Option 2: Clean break
- Switch in one commit
- Update all code at once
- Simpler but riskier

## Pointer Alignment

3-bit tags require 8-byte alignment:
- Heap objects must be 8-byte aligned
- Current allocator already does this
- Should be compatible

## Testing

1. Unit tests for tag operations
2. Round-trip tests (box/unbox)
3. Edge cases (max fixnum, min fixnum, zero)
4. Full test suite compatibility

## References

- `docs/compiler-theory/NANOPASS_COMPILER_SPEC.md` - Section 1 on runtime
- Common Lisp implementations (SBCL uses similar scheme)
- Chez Scheme (uses 3-bit tags)

## Priority

**Low** - Nice to have, but minimal practical benefit
Only do after higher-priority items are complete
