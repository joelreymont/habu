# Embedded instruction constructors

`src/arch/arm32/asm.f` (`A32ASM`) and `src/arch/tic6x/asm.f` (`C6XASM`)
construct instruction values in checked Habu. They do not emit host memory,
select a compiler target, lower IR, allocate registers, schedule instructions,
apply relocations, or produce an executable. Those operations belong to the
compiler layers that consume these modules.

Operands use nominal register, condition, offset, and instruction types.
Generated `>TYPE` converters make an explicit role conversion; constructors
then reject unsupported registers, out-of-range immediates, and misaligned
relative branches with their package's `E-OPERAND`. Offsets are measured in
bytes. Multi-operand arithmetic places the destination first, then sources.
Memory operations take the data register first, base register second, and
immediate offset last. Stores use that same order.

## ARM32

A32 ARM words, 16-bit Thumb instructions, and 32-bit Thumb instructions have
distinct types. The initial subset targets ARMv7-R A32 and ARMv7E-M Thumb-2.
It covers integer moves/constants, selected arithmetic and logic, comparisons,
immediate-offset loads/stores, indirect returns, and relative branches/calls.
The source's public stack effects define each operation's accepted form.

General data registers are R0–R14; PC data operands are outside this subset.
Short Thumb arithmetic and memory forms require R0–R7. Thumb MOVW/MOVT also
exclude SP. ARM arithmetic is unconditional and does not set flags except
CMP. Thumb MOVS/ADDS/SUBS/CMP set flags. ARM branch conditions accept 0–14;
short conditional Thumb branches accept 0–13. `AL` produces condition 14.

`ARM-B`/`ARM-BL` take displacement from the instruction address plus 8.
Thumb branches take displacement from the instruction address plus 4.
Callers must calculate those displacements after laying out instructions.

Serialize ARM words as little-endian 32-bit integers and short Thumb as
little-endian 16-bit integers. A `thumb32-instruction` holds the **first
halfword in bits 31–16** and the second in bits 15–0: emit each halfword
little-endian in that order. For example, `0 >GPR 0 THUMB-MOVW` is
`0xF2400000`, whose bytes are `40 F2 00 00`.

## C66x

`A-REG` and `B-REG` take indices 0–31. Their internal nominal register codes
are A0–A31 = 0–31 and B0–B31 = 32–63. Instructions serialize as little-endian
32-bit words. The scalar subset uses encodings documented in TI's
[SPRUGH7 ISA reference](https://www.ti.com/lit/ug/sprugh7/sprugh7.pdf).

The `.L` arithmetic unit follows the destination bank. The first register
source must share that bank; the second may use the cross path. Immediate
forms take destination, register source, then signed immediate. `ENC-MVK`
accepts signed 16-bit values. `ENC-MVKL` and `ENC-MVKH` take a complete
unsigned 32-bit constant; `ENC-MVKLH` takes only its unsigned upper halfword.
`ENC-MV-L` emits the canonical OR-with-zero form used by GNU assembly.

`ENC-LDW`/`ENC-STW` encode byte offsets from −124 to +124, divisible by four,
without updating the base. The base bank selects the `.D` unit; the data bank
selects the transfer side independently. `ENC-B-REG` uses `.S2`, with an A-bank
source selecting its cross path. `ENC-B-REL` takes side 0/1 and a byte offset
from the start of the containing **32-byte fetch packet**, not the instruction
address. Its displacement must be divisible by four.

Only A0–A2 and B0–B2 are accepted by `WHEN-NONZERO`/`WHEN-ZERO`. NOP has no
predicate field. `PARALLEL-NEXT` sets the p bit to join the following
instruction into the execute packet; it does not prove that packet legal.
`ENC-NOP` accepts 1–9 cycles. A consumer must enforce unit/cross-path/packet
constraints and latency rules, including four delay slots after LDW and five
after a branch. These constructors alone do not make a runnable DSP program.

The constructors added for the C6000 EABI helpers take a predicate as their
last operand: `ALWAYS`, or `IF-NONZERO`/`IF-ZERO` of A0–A2/B0–B2. They cover
the `.L` compares (`CMPGT`, `CMPGTU`, `CMPLT`, `CMPLTU` against a register, a
signed 5-bit or an unsigned 4-bit constant), `SUBC`, `ABS` and `NORM`; the
`.S` shifts (`SHL`, `SHR`, `SHRU` by a register on the destination bank or a
5-bit constant) and bit fields (`EXTU`, `EXT`, `SET`, `CLR` with constant
positions, no cross path); `.D` byte and doubleword access (`LDB`, `LDBU`,
`STB`, `LDDW`, `STDW`) with a signed byte offset scaled to the access size,
their `++` forms modifying the base afterwards, and `ADDAB`/`ADDAW` with a
register or 5-bit constant on the base bank. Doubleword access names the even
register of the pair. The suite pins every form to the word GNU `tic6x-elf-as`
2.47 produces for the same operands.

## Verification

Run focused tests through the normal native engine from the Habu checkout:

```sh
bin/hb --load test/compiler/tic6x-asm.f </dev/null
bin/hb --load test/compiler/arm32-asm.f </dev/null
python3 test/compiler/embedded-asm.py --tic6x-prefix /path/to/tic6x-elf-
```

The Python harness compares actual checked Habu results with independent
assemblers: LLVM `llvm-mc`/`ld.lld`/`llvm-objcopy` for ARM/Thumb and GNU
`tic6x-elf-as`/`ld`/`objcopy` for the shared C674x/C66x subset. All run natively
on the ARM64 development host. `--family arm` or `--family c6x` selects one
family. Generated files stay under ignored `tmp/embedded-asm/`.

On 2026-09-11, both focused tests passed through the current native host.
All 92 LLVM ARM/Thumb comparisons and 47 GNU C674x/C66x comparisons passed.
The local `immediate` operand name remains covered by the ARM constructors.
No target program has been executed on hardware; no full native compiler-suite
result is claimed for these isolated additive modules.
