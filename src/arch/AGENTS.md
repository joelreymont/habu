# src/arch — instruction sets

One directory per instruction set.

- `arm64/` — encoders (`asm.f`), code buffer (`icode.f`), mnemonics,
  disassembler, machine description; LINKER's arm64
  part: the start-up and kernel a kernel build writes as an object
  (`kernel-object.f`, `boot.f`) and code placement (`link.f`). The shared
  native reader in `src/habu/link-kernel.f` adopts kernel objects.
- `x86-64/` — encoders (`asm.f`) into a byte sink, the code layer
  (`icode.f`), runtime emitters (`rt.f`), disassembler, kernel object writer
  (`kernel-object.f`) and code placement (`link.f`).
- `wasm/` — tier 0's Wasm linker (`link*.f`), kernel words, target
  profile, LEB128 (`leb.f`) and code bytes (`code.f`).
- `arm32/`, `tic6x/` — instruction constructors, no backend; `tic6x/` adds a
  C66x simulator, resource facts and EABI helpers.
