# ELF32 program-segment reader

`lib/elf32.f` provides the checked `ELF32` package for reading little-endian
ELF32 headers and bounded program segments. It does not allocate memory, load
code, interpret sections or relocations, or choose a processor ABI. A firmware
image builder can apply its machine and loading policy to these records.

The input is a live byte pointer and `CAD-NUM:byte-len`. The caller owns the
buffer and must provide its actual readable extent. `FILE-BYTES` checks a raw
nonnegative length; it cannot establish ownership of a pointer.

| Word | Effect | Validation |
| --- | --- | --- |
| `FILE-BYTES` | `n -- CAD-NUM:byte-len` | Rejects a negative byte count; zero is allowed. |
| `INFO` | `ptr u8 CAD-NUM:byte-len -- header` | Header, supported representation, and complete program-table bounds. |
| `PROGRAM` | `ptr u8 CAD-NUM:byte-len program-index -- segment` | Header/table, index, and the selected segment. |
| `VALIDATE` | `ptr u8 CAD-NUM:byte-len --` | Header/table and every program segment. |

`header` contains `kind`, `machine`, `entry`, `flags`, `os-abi`, `abi-version`,
and `programs`. `segment` contains `data`, `size`, `kind`, `virtual`, `physical`,
`memory-size`, `flags`, and `alignment`. The generated `ELF32-HEADER:UNMAKE`
and `ELF32-SEGMENT:UNMAKE` expose fields in that order. File kind, machine ID,
address, program count, program index, and segment kind have distinct nominal
types. Addresses are file values, never host pointers.

The segment's data span borrows the input. Do not release, mutate, or resize
the input while using a returned span. A zero-length segment returns an empty
span at the input start. `PT_NULL` also returns an empty span because its
remaining fields are undefined; its raw metadata is still available.

The reader accepts version 1, ELFCLASS32 and ELFDATA2LSB, and permits larger
declared headers and program-entry strides when their bounds are valid. A file
without program entries is valid input. Extended program counts (`PN_XNUM`)
are explicitly unsupported. Section tables are not consulted.

Every nonempty, non-null segment must fit within the input. `PT_LOAD` also
requires file size no greater than memory size, a virtual extent within the
32-bit address space, and valid alignment/congruence. Physical address policy
belongs to the consumer: the generic ABI leaves that field unspecified on
systems that do not use it. Unknown machine and segment IDs are preserved.

`VALIDATE` is not complete ELF ABI conformance validation. It does not check
program ordering, dynamic-linker constraints, permissions, entry-point
execution, section metadata, machine-specific flags, or relocation semantics.
The name refers to the header and segment checks described above.

Errors are `E-FORMAT`, `E-BOUNDS`, and `E-UNSUPPORTED`, backed by canonical
`E-ELF32-*` declarations in `lib/errors.f`. No error changes the input.

Run the native fixture through the supported loader:

```sh
bin/hb --load lib/elf32-test.f
```

It checks decoded fields and payload bytes, every truncated header length,
table/payload overflow, alignment and load extents, empty and extended headers,
null and BSS segments, selected-segment versus whole-table validation, and
nominal type refusals. It is included in the native suite's library fixtures.

Format references: [ELF header](https://gabi.xinuos.com/elf/02-eheader.html)
and [program headers](https://gabi.xinuos.com/elf/07-pheader.html).
