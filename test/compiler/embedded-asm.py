#!/usr/bin/env python3
"""Compare checked instruction constructors with independent native assemblers."""

import argparse
from pathlib import Path
import struct
import subprocess


ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / "tmp/embedded-asm"


def run(command, **kwargs):
    result = subprocess.run(command, cwd=ROOT, text=True, capture_output=True, timeout=60, **kwargs)
    if result.returncode:
        raise RuntimeError(f"{command}:\n{result.stdout}{result.stderr}")
    return result.stdout


def habu_words(package, source, expressions, projection):
    path = OUT / f"{package.lower()}-values.f"
    lines = [f"require {source}", f"using {package}", ": VALUES ( -- )"]
    lines += [f"  {expression} {projection} ." for expression in expressions]
    lines += [";", "VALUES", ";using"]
    path.write_text("\n".join(lines) + "\n")
    values = run([str(ROOT / "bin/hb"), str(path)], input="")
    return [int(value) for value in values.split()]


def c6x_cases():
    cases = [
        ("mvk .S1 -32768,a0", "0 A-REG -32768 ENC-MVK"),
        ("mvk .S2 32767,b31", "31 B-REG 32767 ENC-MVK"),
        ("mvk .S1 42,a4", "4 A-REG 42 ENC-MVK"),
        ("mvkl .S2 0xffffffff,b0", "0 B-REG $FFFFFFFF ENC-MVKL"),
        ("mvkl .S1 0x12348000,a31", "31 A-REG $12348000 ENC-MVKL"),
        ("mvkh .S2 0xffffffff,b31", "31 B-REG $FFFFFFFF ENC-MVKH"),
        ("mvkh .S1 0x12345678,a1", "1 A-REG $12345678 ENC-MVKH"),
        ("mvklh .S1 0,a15", "15 A-REG 0 ENC-MVKLH"),
        ("mvklh .S2 65535,b31", "31 B-REG 65535 ENC-MVKLH"),
        ("add .L1 a30,a29,a31", "31 A-REG 30 A-REG 29 A-REG ENC-ADD-L"),
        ("add .L2 b1,b2,b0", "0 B-REG 1 B-REG 2 B-REG ENC-ADD-L"),
        ("add .L1X a1,b6,a4", "4 A-REG 1 A-REG 6 B-REG ENC-ADD-L"),
        ("add .L2X b31,a31,b31", "31 B-REG 31 B-REG 31 A-REG ENC-ADD-L"),
        ("add .L1X -16,b31,a5", "5 A-REG 31 B-REG -16 ENC-ADD-I5"),
        ("add .L2 15,b3,b4", "4 B-REG 3 B-REG 15 ENC-ADD-I5"),
        ("sub .L1X a1,b2,a3", "3 A-REG 1 A-REG 2 B-REG ENC-SUB-L"),
        ("and .L1 a1,a2,a3", "3 A-REG 1 A-REG 2 A-REG ENC-AND-L"),
        ("or .L2X b1,a2,b3", "3 B-REG 1 B-REG 2 A-REG ENC-OR-L"),
        ("xor .L1X a31,b31,a0", "0 A-REG 31 A-REG 31 B-REG ENC-XOR-L"),
        ("cmpeq .L1X a31,b31,a0", "0 A-REG 31 A-REG 31 B-REG ENC-CMPEQ-L"),
        ("mv .L2X a4,b8", "8 B-REG 4 A-REG ENC-MV-L"),
        ("ldw .D1T1 *+a4(124),a7", "7 A-REG 4 A-REG 124 >BYTE-OFFSET ENC-LDW"),
        ("ldw .D1T2 *-a0(4),b31", "31 B-REG 0 A-REG -4 >BYTE-OFFSET ENC-LDW"),
        ("ldw .D2T1 *b15,a0", "0 A-REG 15 B-REG 0 >BYTE-OFFSET ENC-LDW"),
        ("stw .D2T1 a6,*+b14(8)", "6 A-REG 14 B-REG 8 >BYTE-OFFSET ENC-STW"),
        ("stw .D1T2 b31,*-a31(124)", "31 B-REG 31 A-REG -124 >BYTE-OFFSET ENC-STW"),
        ("b .S2 b3", "3 B-REG ENC-B-REG"),
        ("b .S2X a31", "31 A-REG ENC-B-REG"),
        ("b .S1 .+4194300", "0 >SIDE 4194300 >BRANCH-OFFSET ENC-B-REL"),
        ("b .S2 .-4194304", "1 >SIDE -4194304 >BRANCH-OFFSET ENC-B-REL"),
        ("b .S1 .", "0 >SIDE 0 >BRANCH-OFFSET ENC-B-REL"),
        ("nop 1", "1 ENC-NOP"), ("nop 5", "5 ENC-NOP"), ("nop 9", "9 ENC-NOP"),
        ("mvk .S1 7,a0\n || add .L2 b1,b2,b3", "0 A-REG 7 ENC-MVK PARALLEL-NEXT"),
    ]
    for bank in ("a", "b"):
        for index in range(3):
            for inverted, word in ((False, "WHEN-NONZERO"), (True, "WHEN-ZERO")):
                guard = f"{'!' if inverted else ''}{bank}{index}"
                cases.append((f"[{guard}] b .S2 b3",
                              f"3 B-REG ENC-B-REG {index} {bank.upper()}-REG {word}"))
    return cases


def check_c6x(prefix):
    cases = c6x_cases()
    source = OUT / "c6x.s"
    # Request literal padding. GAS's default code alignment changes p bits to
    # extend the preceding execute packet, which would change the sample itself.
    source.write_text(".arch c674x\n.text\n.global _start\n_start:\n" +
                      "\n".join(".align 5,0\n " + assembly for assembly, _ in cases) + "\n")
    obj, elf, binary = (OUT / f"c6x.{suffix}" for suffix in ("o", "elf", "bin"))
    run([prefix + "as", "-no-pad-sections", "-o", str(obj), str(source)])
    run([prefix + "ld", "-Ttext=0x800000", "-e", "_start", "-o", str(elf), str(obj)])
    run([prefix + "objcopy", "-O", "binary", "-j", ".text", str(elf), str(binary)])
    words = habu_words("C6XASM", "src/arch/tic6x/asm.f",
                       [expression for _, expression in cases], "INSTRUCTION>N")
    reference = binary.read_bytes()
    assert len(words) == len(cases)
    for index, ((assembly, expression), word) in enumerate(zip(cases, words)):
        expected = reference[index * 32 : index * 32 + 4]
        actual = struct.pack("<I", word)
        assert actual == expected, (assembly, expression, actual.hex(), expected.hex())
    return len(cases)


def arm_cases():
    cases = [
        ("mov r0,r14", "0 >GPR 14 >GPR ARM-MOV"),
        ("mov sp,r1", "13 >GPR 1 >GPR ARM-MOV"),
        ("mov r0,#0", "0 >GPR 0 ARM-MOV-I8"),
        ("mov lr,#255", "14 >GPR 255 ARM-MOV-I8"),
        ("movw r4,#0xabcd", "4 >GPR $ABCD ARM-MOVW"),
        ("movt r14,#0xffff", "14 >GPR $FFFF ARM-MOVT"),
        ("add r4,r5,r6", "4 >GPR 5 >GPR 6 >GPR ARM-ADD"),
        ("sub r14,r13,r12", "14 >GPR 13 >GPR 12 >GPR ARM-SUB"),
        ("and r1,r2,r3", "1 >GPR 2 >GPR 3 >GPR ARM-AND"),
        ("orr r12,r0,r14", "12 >GPR 0 >GPR 14 >GPR ARM-ORR"),
        ("eor r4,r5,r6", "4 >GPR 5 >GPR 6 >GPR ARM-EOR"),
        ("add sp,sp,#255", "13 >GPR 13 >GPR 255 ARM-ADD-I8"),
        ("sub sp,sp,#0", "13 >GPR 13 >GPR 0 ARM-SUB-I8"),
        ("cmp r0,r14", "0 >GPR 14 >GPR ARM-CMP"),
        ("ldr r0,[r1,#4095]", "0 >GPR 1 >GPR 4095 ARM-LDR"),
        ("ldr r14,[r13,#-4095]", "14 >GPR 13 >GPR -4095 ARM-LDR"),
        ("str r12,[r11]", "12 >GPR 11 >GPR 0 ARM-STR"),
        ("ldrb r10,[r9,#-1]", "10 >GPR 9 >GPR -1 ARM-LDRB"),
        ("strb r8,[r7,#4095]", "8 >GPR 7 >GPR 4095 ARM-STRB"),
        ("bx r14", "14 >GPR ARM-BX"),
        ("b #33554428", "AL 33554428 >BRANCH-OFFSET ARM-B"),
        ("bl #-33554432", "AL -33554432 >BRANCH-OFFSET ARM-BL"),
    ]
    for cond, suffix in enumerate(("eq", "ne", "hs", "lo", "mi", "pl", "vs",
                                   "vc", "hi", "ls", "ge", "lt", "gt", "le", "al")):
        cases.append((f"b{suffix} #-4", f"{cond} >CONDITION -4 >BRANCH-OFFSET ARM-B"))
    return cases


def thumb16_cases():
    cases = [
        ("movs r0,#0", "0 >GPR 0 THUMB-MOVS"),
        ("movs r7,#255", "7 >GPR 255 THUMB-MOVS"),
        ("adds r0,r1,r7", "0 >GPR 1 >GPR 7 >GPR THUMB-ADDS"),
        ("subs r7,r6,r0", "7 >GPR 6 >GPR 0 >GPR THUMB-SUBS"),
        ("cmp r0,r7", "0 >GPR 7 >GPR THUMB-CMP"),
        ("ldr r7,[r6,#124]", "7 >GPR 6 >GPR 124 THUMB-LDR"),
        ("str r0,[r1]", "0 >GPR 1 >GPR 0 THUMB-STR"),
        ("bx lr", "14 >GPR THUMB-BX"),
        ("b.n #-2048", "-2048 >BRANCH-OFFSET THUMB-B-SHORT"),
        ("b.n #2046", "2046 >BRANCH-OFFSET THUMB-B-SHORT"),
        ("beq.n #-256", "0 >CONDITION -256 >BRANCH-OFFSET THUMB-B-COND"),
    ]
    for cond, suffix in enumerate(("eq", "ne", "hs", "lo", "mi", "pl", "vs",
                                   "vc", "hi", "ls", "ge", "lt", "gt", "le")):
        cases.append((f"b{suffix}.n #254", f"{cond} >CONDITION 254 >BRANCH-OFFSET THUMB-B-COND"))
    return cases


def thumb32_cases():
    cases = []
    for index, value in enumerate((0, 0x800, 0x1000, 0xabcd, 0xffff)):
        reg = (0, 7, 8, 12, 14)[index]
        for op in ("movw", "movt"):
            cases.append((f"{op} r{reg},#{value}", f"{reg} >GPR {value} THUMB-{op.upper()}"))
    cases += [
        ("ldr.w r14,[sp,#4095]", "14 >GPR 13 >GPR 4095 THUMB-LDR-WIDE"),
        ("str.w r8,[r12]", "8 >GPR 12 >GPR 0 THUMB-STR-WIDE"),
    ]
    for offset in (-16777216, -8388608, -4194304, -2, 0, 2, 4194304, 8388608, 16777214):
        for assembly, word in (("b.w", "THUMB-B-WIDE"), ("bl", "THUMB-BL")):
            cases.append((f"{assembly} #{offset}", f"{offset} >BRANCH-OFFSET {word}"))
    return cases


def arm_reference(name, cases, thumb):
    source = OUT / f"{name}.s"
    source.write_text(".syntax unified\n.text\n" + (".thumb\n" if thumb else ".arm\n") +
                      ".global _start\n_start:\n" +
                      "\n".join(".balign 16,0\n " + asm for asm, _ in cases) + "\n")
    obj, elf, binary = (OUT / f"{name}.{suffix}" for suffix in ("o", "elf", "bin"))
    triple, cpu = ("thumbv7em-none-eabi", "cortex-m4") if thumb else ("armv7-none-eabi", "cortex-r5")
    run(["llvm-mc", "-triple=" + triple, "-mcpu=" + cpu, "-filetype=obj", str(source), "-o", str(obj)])
    run(["ld.lld", "-Ttext=0x4000000", "-e", "_start", str(obj), "-o", str(elf)])
    run(["llvm-objcopy", "-O", "binary", "-j", ".text", str(elf), str(binary)])
    return binary.read_bytes()


def check_arm():
    families = (("arm", arm_cases(), "ARM-INSTRUCTION>N"),
                ("thumb16", thumb16_cases(), "THUMB16-INSTRUCTION>N"),
                ("thumb32", thumb32_cases(), "THUMB32-INSTRUCTION>N"))
    references = [arm_reference(name, cases, name != "arm") for name, cases, _ in families]
    count = 0
    for (name, cases, projection), reference in zip(families, references):
        words = habu_words("A32ASM", "src/arch/arm32/asm.f",
                           [expression for _, expression in cases], projection)
        assert len(words) == len(cases)
        for index, ((assembly, expression), word) in enumerate(zip(cases, words)):
            if name == "thumb32":
                actual = struct.pack("<HH", word >> 16, word & 0xffff)
            else:
                actual = struct.pack("<H" if name == "thumb16" else "<I", word)
            expected = reference[index * 16 : index * 16 + len(actual)]
            assert actual == expected, (assembly, expression, actual.hex(), expected.hex())
        count += len(cases)
    return count


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--tic6x-prefix", default="tic6x-elf-")
    parser.add_argument("--family", choices=("all", "arm", "c6x"), default="all")
    args = parser.parse_args()
    OUT.mkdir(parents=True, exist_ok=True)
    count = 0
    if args.family in ("all", "c6x"):
        count += check_c6x(args.tic6x_prefix)
    if args.family in ("all", "arm"):
        count += check_arm()
    print(f"embedded encoders: {count} native assembler comparisons passed")


if __name__ == "__main__":
    main()
