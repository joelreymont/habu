# The first microcontroller target

Status: design for campaign C6 (`habu-campaign-c6-targets-86bb56bb`), child
`habu-design-the-first-61f718f0`. It fixes the shape of the first target so
that the cooperative kernel (C5) and the encoders already in the tree meet in
one place.

## What exists

`src/arch/arm32/asm.f` constructs A32 and Thumb instructions in checked Habu
with nominal register, condition and offset types; the Thumb subset targets
ARMv7E-M, the Cortex-M4 and M7 instruction set. Nothing lowers to it, lays
out an image for it, loads it, or talks to a board. `test/serial.py` and
`test/xmodem.py` are device peers that already stand in for a serial link and
an XMODEM transfer in the gate. SwiftX's cross-target workflow is described in
[tasking-models.md](tasking-models.md) section 2.1 and is the reference.

## Decisions

**Board and core.** Cortex-M4 with a USB serial console, because the encoder
subset is ARMv7E-M and a Cortex-M0+ board would need the Thumb-1-only subset.
Any vendor board works; the design names none, and the gate runs on QEMU's
`mps2-an386` machine (a Cortex-M4) so no hardware is needed for green.

**Host-resident compiler, target-resident monitor.** The checker and
compiler stay on the host; the target runs a monitor of a few kilobytes that
speaks one small protocol over the serial link: read bytes, write bytes, call
address, report registers, reset. This is SwiftX's model and Chuck Moore's:
the target is inspected and corrected from the live system on the host, and
generated code lands in a running target without a target-side interpreter.
A target-resident interpreter is out of scope until a program needs it.

**Image.** One flat image: vector table, reset handler that sets the stacks
and the user area, the primitive set from the machine-independent
specification in [x86-64.md](x86-64.md), the cooperative kernel, the
vectored terminal I/O over the UART, the monitor, then the application. No
heap by default; a program that wants one links the allocator explicitly.

**Kernel.** The cooperative round robin decided in
[tasking-models.md](tasking-models.md) "Decision": two-cell `STATUS`
(`WAKE`/`SLEEP` flag, then `FOLLOWER`), `PAUSE`, `STOP`, `WAKE`, `ACTIVATE`,
`GET`/`RELEASE`, the same public names as the hosted package. Interrupt
handlers do the time-critical work and store `WAKE` into the waiting task's
`STATUS`.

**I/O.** `EMIT`, `KEY`, `KEY?`, `TYPE`, `ACCEPT` dispatch through per-task
user-variable vectors as in SwiftX's `vio.f`; the UART driver is
interrupt-driven with a byte ring queue, and its blocking words `STOP` until
the interrupt wakes them.

**Loading.** Development: the monitor over serial, with XMODEM for whole
images through the existing peer. Production: the board's own flash tool
with the image file; not Habu's concern.

## The lowering

The native compiler's target-free front (`hir.f`, `elaborate.f`, `loop.f`)
is reused. The backend module for the target registers with the target
registry: a Thumb-2 machine IR, selection onto the encoder constructors, a
register allocator parameterised for `r0`..`r12` with the VM registers fixed
(`r7` user area, `r9` data stack, `r10` data base, `r11` code pointer, in
the same roles as the hosted VM), and an emitter producing a position-fixed
image. Spilling reuses the parameterised `spill.f` from the x86_64 work.

## Gate

- The encoder tests already in the tree.
- A QEMU device peer under `test/`, started like the serial peer, that boots
  the image on `mps2-an386` and exposes its UART as the serial link.
- The monitor protocol test: write bytes, read them back, call a word that
  toggles a memory cell, reset.
- The kernel test: two tasks and one interrupt-driven UART echo, run on QEMU.
- The demo: the SwiftX LED-blink shape, a background task driving a GPIO
  pattern while the console answers.

## Work breakdown (dots to open once accepted)

1. Primitive specification table shared with x86_64 (one dot, C6, shared).
2. Thumb-2 machine IR, selection and emission through the registry.
3. Image layout, vector table and reset for `mps2-an386`.
4. The monitor and its host client in `lib/`.
5. Cooperative kernel implementation on the target (C5 decision dot's child).
6. UART driver with ring queue and vectored terminal I/O.
7. QEMU device peer and the gate suites above.
