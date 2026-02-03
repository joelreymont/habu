# JIT Relocation Design

This document outlines a relocation system for the JIT code buffer. The goal
is to support code movement and patching without re-emitting code.

## Goals

- Record patch sites for absolute addresses and branches.
- Enable code buffer growth and compaction.
- Preserve W^X and icache correctness.

## Relocation record

Each relocation entry records:

- `code_offset`: byte offset in the code buffer.
- `kind`: absolute64, branch26, branch19, ldr_literal, etc.
- `target`: address or symbol id.

Store in a side table owned by `patch.CodeBuffer` or `jit.Jit`.

## Patch lifecycle

1. Emit code with placeholder zeros.
2. Record relocation entries as we emit.
3. On finalize: resolve relocations for current buffer base.
4. On move: re-run relocations with new base.

## Code buffer growth

- Allocate a new RW buffer with larger size.
- Copy bytes; update buffer base.
- Re-apply relocations.
- Flip to RX and flush icache.

## Integration points

- `src/jit/patch.zig`: add relocation table and apply routines.
- `src/jit/jit.zig`: record relocations for stencils that embed addresses.
- `src/jit/stencils.zig`: annotate which holes need relocation records.

## Testing

- Unit test: move code buffer and verify branch targets.
- Property test: random relocation tables round-trip.
- Bench: relocation cost for N entries.
