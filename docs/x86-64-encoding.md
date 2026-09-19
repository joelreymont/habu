# x86-64 instruction construction and bounded emission

**Candidate implementation; native Habu acceptance is pending.** This is an additive
ISA/emission layer, not a selected compiler target or a hosted x86 Habu engine.
The supported encoding subset is 64-bit mode, little-endian, with scalar SSE2.
There is no AVX/FMA, x87, atomics, full instruction decoder, ELF writer or HIR lowerer.

## Layers

`src/targets/x86-64/isa/asm.f` (`X64ASM`) builds instruction values without writing
an output buffer. `src/targets/x86-64/codegen/emit.f` (`X64EMIT`) checks capacity
and writes them, or applies explicit low-level relative/absolute fixups. Neither
module allocates executable memory, changes page permissions or enters target code.

The implementation host must have eight-byte cells. A 32-bit host is refused.
The instruction value contains two packed integers and a nominal length: `low`
holds bytes 0–7, `high` holds bytes 8–14, and `size` is in 1–15. Bytes are always
serialized explicitly; no native-endian cell store defines the output format.
Generated `MAKE`/converters can construct values that have not passed the public
constructors. Consumers revalidate lengths and memory operands. `VALIDATE` checks
representation/length, **not that arbitrary forged bytes encode a legal instruction**.

`EMIT` and `EMIT2` check every documented refusal condition before their first
output write. A valid declared capacity still does not establish ownership of the
underlying writable allocation. The caller supplies that allocation and excludes
concurrent modification. No rollback is promised for invalid pointers, hardware
faults, asynchronous exceptions or concurrent writes.

## Operand conventions

Register operands are destination first. Store operations also place the data
register first, then the memory operand. GPR numbers follow x86 encoding order:
RAX=0, RCX=1, RDX=2, RBX=3, RSP=4, RBP=5, RSI=6, RDI=7, R8=8 … R15=15.
XMM0–15 have their corresponding numbers. Conditions are architectural condition
codes 0–15, not Habu booleans.

`BASE-MEM ( gpr displacement -- memory )` constructs base+disp addressing.
`INDEXED ( gpr gpr scale displacement -- memory )` takes base, index, scale, disp;
scales are 1, 2, 4 or 8. RSP is not an index; R12 is a valid extended index.
`RIP ( displacement -- memory )` is a distinct form. Displacements are signed32.
There is no no-base/index-only addressing form in this subset.

Byte operands use only the low byte of the selected GPR. GPR4–7 mean SPL/BPL/SIL/DIL
and force REX. There is no AH/CH/DH/BH constructor, avoiding ambiguous high-byte
registers when a REX prefix is required. Loads/stores choose a displacement size
that preserves the requested address, including zero displacement for RBP/R13 and
SIB for RSP/R12. A trailing immediate is part of the instruction end.

`MOVABS` takes a complete 64-bit bit pattern. Arithmetic `*-I32` values must be
signed32; the form sign-extends that operand to 64 bits. `STORE-I32` writes exactly
32 bits. `MOVZX8` writes the 32-bit view of its destination, clearing upper bits.
The immediate shift encoders accept raw unsigned8 counts; their hardware masking
is not a substitute for a future HIR shift-semantics contract.

`CALL32`, `JMP32` and `JCC32` always produce near rel32 forms. The caller supplies
an instruction-end-relative displacement. There is no automatic branch relaxation,
far-reference expansion, label resolution or code-model selection.

`DIV64`, `IDIV64` and `CQO` encode hardware operations only. The caller must model
implicit RAX/RDX inputs/outputs, possible faults, flags and result normalization.
Likewise `SD>GPR` encodes hardware truncation; it does not decide Habu's out-of-range
or NaN conversion semantics. `UCOMISD` exposes flags, not a canonical Habu Boolean.
Scalar FP forms preserve their actual x86 upper-lane and MXCSR effects; this layer
does not promise a uniform effect for all SSE instructions.

## Writer and fixup contracts

| Word | Input/output order and role |
|---|---|
| `EMIT` | `buffer capacity offset instruction -- next-offset` |
| `EMIT2` | `buffer capacity offset first second -- next-offset`; whole pair checked first |
| `DELTA32` | `target-address pc-address -- displacement`; unsigned address ordering and signed32 result bounds |
| `ADDRESS+` | `target-address offset -- target-address`; refuses unsigned overflow |
| `ELF-ADDEND` | `field-offset instruction-end-offset -- n`; answers `field - end`, absent an additional symbol addend |
| `PATCH-REL32` | `buffer capacity field-offset instruction-end-offset base-address target-address --` |
| `PATCH-ABS64` | `buffer capacity field-offset target-address --` |

All offsets and capacities are byte counts/coordinates, not host pointers. A
`target-address` may contain any unsigned64 bit pattern; canonical virtual-address,
section permissions, symbol ownership and object-format restrictions belong to the
consumer. `DELTA32` accepts +2^31−1 and −2^31, refuses their adjacent out-of-range
values, and handles addresses straddling the signed-host boundary correctly.

For `mov dword ptr [rip+disp32],imm32`, field=2 and end=10, hence ELF addend=−8.
Using field+4 as the PC would instead address four bytes beyond the intended target.
For a plain rel32 call, field=1 and end=5, hence −4. These helpers do not build an
ELF relocation record or choose PLT/GOT policy. When a logical symbol expression
already has addend A, the object writer must combine A with `field - end` using
checked arithmetic.

`PATCH-REL32` is intentionally a low-level bounded writer: it checks spans,
coordinate ordering, base+end overflow and displacement range. The future owned
emission must additionally certify that this field belongs to this instruction,
module, generation and relocation recipe. A bounded write is not that certificate.

## Example

```forth
require src/targets/x86-64/codegen/emit.f

package X64-EXAMPLE
public

: BUILD-CONSTANT ( ptr u8 X64EMIT:capacity -- X64EMIT:offset )
   {: out cap:X64EMIT:capacity :}
   out cap 0 X64EMIT:>OFFSET
   0 X64ASM:>GPR $1122334455667788 X64ASM:MOVABS
   X64ASM:RET X64EMIT:EMIT2 ;

;package
```

The caller provides at least 11 writable bytes. On success this writes a System V
scalar-return instruction sequence; it does not allocate, publish or invoke it.
The example still needs a real Habu load as part of the candidate acceptance gate.

## Validation

Apply the complete accompanying patch (including canonical errors) to the pinned
checkout, using a separate workspace and preserving the accepted engine. Then:

```sh
bin/hb --load test/compiler/x86-64-asm.f </dev/null
```

The fixture contains 72 instruction goldens, memory/register/immediate refusals,
nominal mismatches, pair-write canaries and relative/absolute fixup checks. It is
not yet registered in `test/run.f`: run it successfully through the real load path
before registering it and running the required full gate.

The companion package contains reproducible **out-of-tree reference experiments**.
They replay the two source modules' scalar algorithms with nominal types erased,
compare bytes to GNU and Clang assemblers, and execute small sequences on Linux
x86-64. They do not run the Habu checker, compiler, physical record representation,
exception machinery or native fixture. Never install the replay scripts as a Habu
build dependency, replacement interpreter or repository test runner.
