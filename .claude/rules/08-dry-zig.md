# DRY and Efficiency (Zig)

## Table-Driven Dispatch

Replace repetitive if/switch chains with data tables:

```zig
// WRONG: Repetitive pattern
if (node.* == .consp) { return &t_cons; }
if (node.* == .symbolp) { return &t_symbol; }
if (node.* == .numberp) { return &t_fixnum; }

// RIGHT: Table-driven
const type_map = [_]struct { tag: Tag, ty: *const Type }{
    .{ .tag = .consp, .ty = &t_cons },
    .{ .tag = .symbolp, .ty = &t_symbol },
    .{ .tag = .numberp, .ty = &t_fixnum },
};
for (type_map) |e| if (tag == e.tag) return e.ty;
```

## Extract Common Patterns

When 3+ locations share logic, extract to a function:

```zig
// WRONG: Duplicated operand extraction
const op = node.consp.operand;
const op = node.symbolp.operand;
const op = node.numberp.operand;

// RIGHT: Single extraction function
fn getPredicateOperand(node: *const Ir) ?*const Ir {
    return switch (node.*) {
        .consp, .symbolp, .numberp => |p| p.operand,
        else => null,
    };
}
```

## Avoid Allocation When Possible

- Use stack arrays for small fixed-size data
- Prefer slices over ArrayList when size is known
- Use comptime for constant data

## Check Before Committing

Before committing Zig changes, scan for:
1. Repeated switch/if patterns (→ table)
2. Duplicated struct field access (→ helper function)
3. Similar test setup code (→ test helper)
