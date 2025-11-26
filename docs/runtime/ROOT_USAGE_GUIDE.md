# Habu GC Root Usage Guide

**Version**: 1.0
**Date**: November 20, 2024
**Status**: Required reading for all Habu developers

---

## Overview

Habu uses a copying garbage collector that can relocate objects in memory. When GC runs, it moves live objects and updates all pointers. **If your pointer is not registered as a root, it won't be updated, leaving you with a dangling pointer.**

---

## The Golden Rule

**ANY habu_value_t that you need to keep across allocations MUST be rooted.**

If you:
1. Allocate object A
2. Call ANY function that might allocate (triggering GC)
3. Try to use object A again

Then object A **MUST** be rooted between steps 1 and 3.

---

## When GC Can Run

GC can be triggered by **any allocation**:
- `habu_cons()`
- `habu_make_vector()`
- `habu_make_string()`
- `habu_make_symbol()`
- `habu_make_closure()`
- Any function that calls the above

---

## How to Root Objects

### Method 1: Direct API (Verbose)

```c
habu_value_t obj = habu_cons(a, b);
habu_gc_add_root(&obj);  // Note: pass ADDRESS of variable

// ... do work, can allocate safely ...

habu_gc_remove_root(&obj);
```

**Important**: Pass `&obj` (address), not `obj` (value)!

### Method 2: Scoped Root Macros (Recommended)

```c
HABU_ROOT(obj, habu_cons(a, b));
// obj is now rooted for this scope

// ... do work, can allocate safely ...

HABU_UNROOT(obj);
```

### Method 3: Multiple Roots

```c
HABU_ROOT2(list, NIL,
           item, habu_cons(fixnum_to_value(42), NIL));

// Both list and item are rooted

// ... build up list ...
list = habu_cons(item, list);

HABU_UNROOT2(list, item);
```

---

## Common Patterns

### Pattern 1: Building a List

```c
HABU_ROOT(result, NIL);

for (int i = 0; i < n; i++) {
    HABU_ROOT(item, habu_cons(fixnum_to_value(i), NIL));
    result = habu_cons(item, result);  // Can trigger GC!
    HABU_UNROOT(item);
}

HABU_UNROOT(result);
return result;
```

### Pattern 2: Processing Arguments

```c
habu_value_t my_function(habu_value_t arg1, habu_value_t arg2) {
    // Root arguments if we'll allocate
    habu_gc_add_root(&arg1);
    habu_gc_add_root(&arg2);

    HABU_ROOT(result, process(arg1));  // Might allocate
    result = combine(result, arg2);    // Might allocate

    habu_gc_remove_root(&arg2);
    habu_gc_remove_root(&arg1);

    HABU_UNROOT(result);
    return result;
}
```

### Pattern 3: Temporary Values

```c
HABU_ROOT(temp, expensive_computation());  // Returns heap object

// Use temp multiple times
HABU_ROOT(result, transform(temp));  // Might allocate
result = combine(result, temp);       // temp still valid!

HABU_UNROOT(result);
HABU_UNROOT(temp);
return result;
```

---

## What NOT to Root

### Fixnums (Immediate Values)

```c
habu_value_t num = fixnum_to_value(42);
// NO need to root - fixnums are not heap allocated
```

### NIL

```c
habu_value_t empty = NIL;
// NO need to root - NIL is a constant
```

### Local Temporaries That Don't Survive Allocations

```c
habu_value_t temp = habu_cons(a, b);
return temp;  // OK - no allocations between create and return
```

**But if you call anything that might allocate:**

```c
habu_value_t temp = habu_cons(a, b);
print_value(temp);  // If print_value allocates, temp might be stale!
return temp;        // DANGER!
```

**Fix:**

```c
HABU_ROOT(temp, habu_cons(a, b));
print_value(temp);  // Safe - temp will be updated if moved
HABU_UNROOT(temp);
return temp;
```

---

## Common Mistakes

### Mistake 1: Forgetting to Root Loop Variables

```c
// WRONG - result not rooted
habu_value_t result = NIL;
for (int i = 0; i < 100; i++) {
    result = habu_cons(fixnum_to_value(i), result);  // Can trigger GC!
}
```

**Why it fails**: After first few iterations, GC runs and `result` becomes a dangling pointer.

```c
// CORRECT
HABU_ROOT(result, NIL);
for (int i = 0; i < 100; i++) {
    result = habu_cons(fixnum_to_value(i), result);
}
HABU_UNROOT(result);
```

### Mistake 2: Rooting Value Instead of Address

```c
habu_value_t obj = habu_cons(a, b);
habu_gc_add_root(obj);  // WRONG! Should be &obj
```

**Fix:**

```c
habu_gc_add_root(&obj);  // CORRECT
```

### Mistake 3: Forgetting to Unroot

```c
void leak_roots() {
    HABU_ROOT(obj, habu_cons(a, b));
    // ... use obj ...
    return;  // LEAK! obj still in root set
}
```

**Impact**: Root array grows forever, slows down GC

**Fix:**

```c
void correct() {
    HABU_ROOT(obj, habu_cons(a, b));
    // ... use obj ...
    HABU_UNROOT(obj);  // Always unroot before return
}
```

### Mistake 4: Returning Rooted Values

```c
habu_value_t wrong() {
    HABU_ROOT(obj, habu_cons(a, b));
    HABU_UNROOT(obj);
    return obj;  // DANGER if caller triggers GC before rooting!
}
```

**Fix**: Document that caller must root immediately:

```c
/* Returns a new cons cell.
 * CALLER MUST ROOT the result before any allocations! */
habu_value_t make_pair(habu_value_t a, habu_value_t b) {
    return habu_cons(a, b);
}

// Usage:
HABU_ROOT(pair, make_pair(x, y));
```

---

## Performance Considerations

### Root Registration Cost

- Adding root: O(n) where n = current roots (checks for duplicates)
- Removing root: O(n) (linear search + shift)
- During GC: O(roots) to update all roots

**Recommendation**: Minimize root churn. Root once, use many times, then unroot.

### Bad Pattern (Churning)

```c
for (int i = 0; i < 1000; i++) {
    HABU_ROOT(item, make_item(i));  // 1000 add_root calls
    process(item);
    HABU_UNROOT(item);              // 1000 remove_root calls
}
```

### Better Pattern

```c
HABU_ROOT(item, NIL);  // Root once
for (int i = 0; i < 1000; i++) {
    item = make_item(i);  // Root location updated automatically
    process(item);
}
HABU_UNROOT(item);  // Unroot once
```

---

## Stack-Based Root Guards (Future)

**Currently not implemented**, but planned:

```c
// Automatic unrooting when guard goes out of scope
HABU_SCOPED_ROOT(obj, habu_cons(a, b));
// ... use obj ...
// Automatic HABU_UNROOT(obj) at end of scope
```

This requires C++ or GCC cleanup attributes.

---

## Debugging Root Issues

### Symptoms of Missing Roots

- Crashes during or after GC
- Corrupted data structures
- Objects "disappearing" after allocations
- Intermittent failures (depends on when GC runs)

### How to Debug

1. **Enable verbose GC logging** (add to gc.c):
   ```c
   printf("GC: Updated root %p from %lx to %lx\n",
          root_location, old_value, new_value);
   ```

2. **Track root count**:
   ```c
   printf("Roots before: %zu, after: %zu\n",
          gc_heap->roots_size_before, gc_heap->roots_size_after);
   ```

3. **Add assertions**:
   ```c
   assert(get_tag(obj) == TAG_CONS);  // Verify object still valid
   ```

4. **Run under Valgrind/AddressSanitizer**

---

## Integration with Habu Runtime

### Current Status

- ✅ GC root system implemented (pointer-to-pointer)
- ✅ Test code uses roots correctly
- ✅ Internal functions (`habu_make_symbol`) use roots
- ❌ **REPL/compiled code doesn't use roots**
- ❌ **No automatic stack scanning**

### TODO for Production

1. **Add roots to REPL**: Top-level bindings must be rooted
2. **Add roots to compiled code**: Local variables in generated code
3. **Document rooting requirements**: Every public API function
4. **Add root stress tests**: Trigger GC frequently, verify no corruption

---

## Examples from Habu Codebase

### Good Example: habu_make_symbol

```c
habu_value_t habu_make_symbol(const char *name) {
    void *mem = habu_gc_alloc(sizeof(habu_symbol_t), TYPE_SYMBOL);
    if (!mem) return NIL;

    habu_symbol_t *sym = (habu_symbol_t *)mem;
    habu_value_t sym_value = tag_pointer(sym, TAG_SYMBOL);

    // ROOT before nested allocation
    habu_gc_add_root(&sym_value);

    // Safe to allocate string now
    sym = (habu_symbol_t *)untag_pointer(sym_value);
    sym->name = habu_make_string(name, strlen(name));
    sym->value = NIL;
    sym->plist = NIL;

    // UNROOT before returning
    habu_gc_remove_root(&sym_value);

    return sym_value;
}
```

### Good Example: Test Code

```c
TEST(gc_root_registration) {
    habu_value_t obj = habu_cons(fixnum_to_value(42),
                                  fixnum_to_value(43));

    habu_gc_add_root(&obj);  // Root it!

    // Allocate enough to trigger GC
    for (int i = 0; i < 10000; i++) {
        habu_cons(fixnum_to_value(i), NIL);
    }

    // obj was updated if it moved
    assert(get_tag(obj) == TAG_CONS);

    habu_gc_remove_root(&obj);
}
```

---

## Quick Reference

```c
/* Root single value */
HABU_ROOT(var, value);
// ... use var ...
HABU_UNROOT(var);

/* Root two values */
HABU_ROOT2(v1, val1, v2, val2);
// ... use v1 and v2 ...
HABU_UNROOT2(v1, v2);

/* Manual rooting */
habu_value_t var = value;
habu_gc_add_root(&var);
// ... use var ...
habu_gc_remove_root(&var);

/* Root function parameters */
habu_gc_add_root(&param1);
habu_gc_add_root(&param2);
// ... do work ...
habu_gc_remove_root(&param2);
habu_gc_remove_root(&param1);
```

---

## Summary

1. **Root ALL heap objects that survive allocations**
2. **Use macros for cleaner code** (`HABU_ROOT`, `HABU_UNROOT`)
3. **Pass address `&obj`, not value `obj`**
4. **Always unroot before returning**
5. **Don't root fixnums or NIL**
6. **Minimize root churn for performance**

**When in doubt, root it!** Better safe than corrupted.

---

**Document Maintainer**: Habu Development Team
**Last Updated**: November 20, 2024
**Related Bugs**: Bug 1.1, Bug 1.2, Bug 1.3
