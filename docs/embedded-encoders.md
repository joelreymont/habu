# Embedded instruction constructors

`src/arch/arm32/asm.f` (`A32ASM`) and `src/arch/tic6x/asm.f` (`C6XASM`)
construct instruction values in checked Habu. `src/arch/x86-64/asm.f`
(`X64ASM`) belongs to the same family but appends bytes to a sink instead of
answering a value, because an x86_64 instruction has no fixed width. None of
them emit host memory, select a compiler target, lower IR, allocate registers,
schedule instructions, apply relocations, or produce an executable. Those
operations belong to the compiler layers that consume these modules.

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

## x86_64

Four register files are four distinct nominal types — `r64`, `r32`, `r16` and
`r8` — so a 32-bit register where a 64-bit one is required is a checker
refusal, not a runtime throw. `imm8`, `imm32`, `imm64`, `condition`, `rel` and
`mem` are nominal too. The `r8` file is the REX-only one: 4–7 are `spl`, `bpl`,
`sil` and `dil`, never the legacy `ah`/`ch`/`dh`/`bh`, and any `r8` operand at
or above 4 forces a REX prefix. The 64-bit register names `RAX`–`R15` and the
sixteen condition names `C-O`–`C-G` are words of those types.

A `mem` is one packed cell built only by `MEM-AT` (base), `MEM-OFF` (base plus
displacement), `MEM-IDX` (base plus scaled index plus displacement, scale 1, 2,
4 or 8, index never `rsp`) and `MEM-RIP`. Its fields are re-screened on the way
into an encoder, so a value forged through the generated `>MEM` cast is refused
with `E-OPERAND` rather than reaching a ModRM field.

Every encoder takes the byte sink as its LAST operand and appends through
`lib/byte-buffer.f` (`BUF`); there is no instruction type, because the
instruction is the bytes. Operand order otherwise matches the ARM32 and C66x
sets: destination first, then sources, and a memory form takes the data
register first and the memory operand second, stores included.

Encoding is deterministic. The emitted length is a function of the word the
caller chose and of the memory operand's own displacement magnitude — never of
a peephole or a relaxation pass — so a caller that needs a size encodes into a
scratch buffer and reads the length. Branch width is the caller's choice:
`ENC-JCC-REL8` and `ENC-JCC-REL32` are separate words, and a `rel` is measured
in bytes from the END of the instruction, unlike ARM32's instruction address
plus 8 or 4.

Each operation has exactly one encoding. The accumulator short forms (`05 id`,
`A9 id`, `90+r`) and the `D1 /n` shift-by-one form are size optimisations and
are not implemented, so `llvm-mc`, which prefers them, is compared against the
general form with registers other than `rax` and shift counts other than one.
`ENC-MOV-RI64` is the one relocatable literal the x86_64 design names
(`docs/x86-64.md`); `MOV-RI64-IMM-OFF` is where its imm64 begins inside the
instruction, and the relocation writer patches there.

The set covers group-1 arithmetic and logic (`add or adc sbb and sub xor cmp`)
in five forms each, `test`, moves and loads and stores at 8, 16, 32 and 64
bits, `movzx`/`movsx`/`movsxd`, `lea`, the F7 and FF one-register groups
(`not neg mul imul div idiv inc dec`) with `cqo`, the non-widening `imul`,
shifts and rotates by imm8 and by `cl`, relative and indirect branches and
calls, `ret`, `setcc`, `cmovcc`, `push`, `pop`, `xchg` and `syscall`. The one
32-bit arithmetic form is `ENC-XOR32-RR`, the register-zeroing idiom; a 32-bit
result zero-extends into the whole 64-bit register.

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
bin/hb --load test/compiler/x86-64-asm.f </dev/null
python3 test/compiler/embedded-asm.py --tic6x-prefix /path/to/tic6x-elf-
```

The x86_64 suite needs no assembler at test time: each of its 152 forms is
pinned to a fixed byte string, and the `llvm-mc:` comment above each case is
the source line that produced it. The strings came from
`llvm-mc -triple=x86_64 -show-encoding` on 2026-09-17 (LLVM 22.1.8), and the
five relative branches from `llvm-mc -filetype=obj` plus
`llvm-objdump -d --triple=x86_64`, because llvm-mc leaves a fixup rather than
bytes for a symbolic branch target.

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

On 2026-09-17 the x86_64 suite passed on the ARM64 host: 152 byte-string
forms, 27 runtime refusals, 11 checker-refusal candidates. A second,
independent check disassembled a 99-byte buffer the encoders themselves
produced with `llvm-objdump -d --triple=x86_64`, and all 26 instructions read
back as the intended mnemonics. No x86_64 code has been executed anywhere; the
encoders are target-free Habu and no machine has run their output.

## C66x execute packets

`src/arch/tic6x/facts.f` (`C6XFACTS`) returns a `facts` record for one
instruction word from the constructed subset: the unit it occupies (`.L`, `.S` or `.D`, by side),
the register it reads through a cross path, the registers it reads, the
registers it writes at the end of its cycle, the registers a load fills after
four delay slots, the register file its memory data moves on, and whether it
branches or idles. The p bit is packet structure, not a fact. A word outside
the subset is refused with `E-DECODE`.

`src/arch/tic6x/sim.f` (`C6XSIM`) runs programs as execute packets: the
words chained by their p bits issue in one cycle, every instruction reads
before any writes, loads land four cycles later, a branch takes effect after
five packets, a multicycle `NOP n` idles `n - 1` cycles after its packet
unless a branch lands first, and a cross-path read of a register a non-load
wrote in the previous cycle costs one stall cycle. It refuses with
`E-CONFLICT` what SPRUGH7 section 3.8 forbids: two instructions on one unit,
two registers through one cross path, two memory accesses moving data on one
register file, two multicycle NOPs in a packet, two taken branches in a cycle,
and two writes landing on one register in one cycle, including a load landing
beside an ALU write issued four cycles after it. `CALL` returns the cycles.

`src/arch/tic6x/eabi.f` (`C6XEABI`) writes each helper as a sequential
program and schedules it. Instructions collect in a block until a label
(`HERE`, `RESOLVE`) or a branch (`BACK`, `FORWARD`, `RETURN`) closes it; a
list scheduler then places each instruction in the earliest cycle where its
unit, cross path and data path are free and the sequential meaning holds,
with cross-path reads one cycle later so the hardware never stalls. The
closing branch issues as late as its guard allows but early enough that its
delay slots hold the rest of the block and every load has landed when the
target runs, so blocks stay independent of one another. `EMIT` refuses NOPs
and branches; the scheduler owns idle cycles and the delay slots.
