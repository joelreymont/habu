# src/arch — instruction sets

One directory per instruction set. A backend registers its row in the pass
table (`src/compiler/native/backend.f`) from its `passes.f`.

- `arm64/` — encoders (`asm.f`, A64ASM), code buffer (`icode.f`), mnemonics,
  disassembler, machine description, `backend.f`, `passes.f`.
- `x86-64/` — encoders (`asm.f`, X64ASM) into a byte sink, the code layer
  (`icode.f`, X64CODE), ABI, machine, runtime emitters (`rt.f`), disassembler;
  `icode.f` and `rt.f` require `src/os/linux-x86-64` layout and syscalls.
- `wasm/` — the Wasm backend (`backend.f`, WBACK): dialect, selector,
  structured control, encoder, linker, kernel words, target profile.
- `arm32/`, `tic6x/` — instruction constructors, no backend; `tic6x/` adds a
  C66x simulator, resource facts and EABI helpers.
