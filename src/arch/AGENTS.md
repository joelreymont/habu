# src/arch — instruction sets

One directory per instruction set. A backend registers its row in the pass
table (`src/compiler/native/backend.f`) from its `passes.f`.

- `arm64/` — encoders (`asm.f`), code buffer (`icode.f`), mnemonics,
  disassembler, machine description, `backend.f`, `passes.f`; LINKER's arm64
  part: the start-up and kernel a kernel build writes as an object
  (`kernel-object.f`, `boot.f`), their adoption by a build's link
  (`link-kernel.f`) and code placement (`link.f`).
- `x86-64/` — encoders (`asm.f`) into a byte sink, the code layer
  (`icode.f`), ABI, machine, runtime emitters (`rt.f`), disassembler.
- `wasm/` — tier 0's Wasm linker (`link*.f`), kernel words, target
  profile, LEB128 (`leb.f`) and code bytes (`code.f`).
- `arm32/`, `tic6x/` — instruction constructors, no backend; `tic6x/` adds a
  C66x simulator, resource facts and EABI helpers.
