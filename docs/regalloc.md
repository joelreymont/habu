# Register Allocation Design (JIT)

This document sketches the design for a register allocator that enables
Cranelift-class codegen in the Habu JIT. It targets the SSA IR in
`src/jit/ir.zig` and AArch64 codegen first.

## Goals

- Correct register assignment for SSA temporaries.
- Minimal spills for hot paths.
- Deterministic output (stable across runs).
- Integrate with GC safepoints (stack maps).

## Constraints

- AArch64 only (initially).
- JIT code buffer is append-only; no relocation of emitted code yet.
- Runtime calls follow Zig error-union ABI; preserve scratch regs.

## Register classes

Define register classes in `src/jit/regalloc.zig`:

- GPR: x0-x28 (x29 FP, x30 LR reserved)
- SCRATCH: x0-x7 for call-clobbered
- CALLEE: x19-x28 for long-lived values

Reserve:

- x29 (FP), x30 (LR)
- x18 (platform reserved on macOS)
- x16/x17 (linkage scratch)

## ABI and calling convention

- Spill all caller-saved registers before runtime calls.
- Preserve callee-saved registers across JIT function boundaries.
- Use a fixed frame layout: [spills][locals][stack args].

## Liveness

Compute liveness per block:

1. Build def/use sets per block from SSA.
2. Backward dataflow for live-in/live-out.
3. Use interval ranges: first_def..last_use for each value.

## Allocation algorithm

Start with linear-scan:

- Sort intervals by start.
- Maintain active set ordered by end.
- When out of regs: spill the interval with latest end.
- Assign spill slots via frame allocator.

Follow-up: priority-based allocator (use counts + loop depth).

## Spills

- Spill slots are stack offsets in the frame.
- Insert reload/store in codegen at use/def.
- Track spills in `RegAllocResult` for stack map integration.

## Stack maps (safepoints)

- For each safepoint, record live stack slots + live registers.
- Use a compact bitset for regs and a sorted list for slots.
- Serialize into a side table keyed by code offset.

## Integration points

- `src/jit/ir.zig`: add per-value id, block order.
- `src/jit/verify.zig`: validate allocation correctness.
- `src/jit/jit.zig`: replace fixed reg emission with allocation results.

## Testing

- Unit tests for liveness and allocation (small IR graphs).
- End-to-end JIT parity tests for arithmetic + control flow.
- Bench: compare spill rate and compile time.
