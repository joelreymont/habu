---
title: Emit x86 control bodies
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.568965+03:00"
blocks:
  - habu-boot-and-exit-367c46f5
  - habu-scaffold-the-x86-9af80979
---

Problem: the kernel has no control rows.
Acceptance: bodies for `execute run-in-stack catch throw finally die 2>r 2r> 2r@` and an `evaluate` stub until I9a; catch/throw restore machine stack, data stack and handler frame (`src/habu/habu1.f:2673-2830` semantics); native tests through routine images; the rows join the `docs/x86-64.md` kernel inventory.
Files: `src/habu/kernel-x64.f`, `test/x86-64-peer-routines.f`, `docs/x86-64.md` (kernel inventory).
Verify: ThinkPad: routine images (C8).
Depends: habu-boot-and-exit-367c46f5 (K3).
Route: direct.
Ownership: krait (Intel lane).
Claim: unassigned.
- From I4a: the body list includes `execute-floor ( n -- bool )` (call the xt, then clamp and report a stack below S0).

K-lane corrections (design 2026-09-30; these override the lines above where they differ):
- Depends add habu-scaffold-the-x86-9af80979 (the kernel scaffold). Files: replace `test/x86-64-peer-routines.f` with this leaf's `test/x86-64-kernel-<name>.f` from the scaffold. Bodies are hand-written through `X64ASM` (no allocator dependency). Every body reads DATA through rbp, so cases run in the booted harness (`test/x86-64-boot-harness.f`). The row table goes in this leaf's `docs/x86-64.md` subsection. Verify: host engine K3's product `264c829e…`; the ThinkPad runs the images natively, each with its negative twin. Base: the scaffold on master.
- Cited range: `habu1.f:1751-1784` (`RSTK-PUSH/POP`, `B2TOR/B2RFROM/B2RFETCH`), `1854-1874` (`BDIE`), `2721` (`BEXEC`), `2724-2752` (`BCATCH`), `2763-2806` (`BTHROW`), `2821-2872` (`GUARDED-EXTENT?`, `BRUNSTACK`), `2873-` (`BFINALLY`).
- Catch frame: the offsets of `habu1.f:2727-2729` (0 prev-HND, 8 data-sp, 16 machine-sp after pop, 24 resume, 32 return address, 40 RSP, 48 LOOPSP, 56 `CATCH-MAGIC`, 64 base, 72 cap; `STACK-ABI:CATCH-BYTES` $50), chained through `HND-CELL` ($8). `throw` with HND nonzero: validate (sentinel, depths within `RETURN-CELLS`/`LOOP-FRAMES`, `CHECK-CURSOR`), restore, resume; a corrupt frame writes `hb: catch frame corrupt` on fd 2 and exits 87 (`CATCH-STACK`). `STACK-GUARD:CHECK-CURSOR`/`EXIT-BOUNDS` twins (`src/habu/rt.f:80-90`) go in `src/arch/x86-64/rt.f`. `run-in-stack`: `GUARDED-EXTENT?` (`habu1.f:2821-2831`), then `E-STACK-UNGUARDED` through `throw`.
- Decision, no-handler `throw`: with `HND-CELL` zero, if `[rbp+UNCGH-CELL]` is nonzero, push the code and `call` it as an xt `( n -- )`, then fall through; a code in [1,255] exits with it, anything else exits `UNCAUGHT-RC` 67 (`habu1.f:2795-2802`). No `LREPLROUTE` probe and no `EVALD` arm: `LREPLROUTE`, `LUNCAUGHT` and `LEVALREC` are habu2.f routines (`habu2.f:1299-1302`, `9641`, `9719`) that never exist on x86; the Habu `evaluate` (I9a-c) recovers through `catch`, and I10b installs the reporter xt.
- Decision, `die ( ptr u8 n n -- )`: write the message and LF to fd 2 when n > 0; call the xt in `EXIT-HOOK-CELL` when nonzero (`(LEXITHOOK)`, `habu2.f:4438-4443`); an rc outside [0,255] becomes 67; `exit_group` (`habu1.f:1854-1874`).
- Decision, interpreter-bound rows: `evaluate`, `create`, `parse-name`, `num-parse` and `tok-imm?` are `X64KERNEL:REFUSE` rows in `CONTROL,` (fd-2 line, exit 76). All five jump into habu2.f's interpreter on ARM64 (`BCREATE` to `CREATEP-CELL` `habu1.f:1550`, `BTOKIMM` to `LFIND` `habu2.f:11087`, `BNUMPARSE` `3377`, `BPARSE-NAME` `3345`), which the x86 engine never has; lane I provides them in Habu (I4 scanner and immediate probe, I3 numbers, I7 `create`) through I9a's `FL-PREFIX-PROVIDED`, and `KEEP-BODY?` then skips the refusal in seeded builds. `COMPLETE` needs the registration meanwhile (`KEEP?` is true with `SHAKE?` off, `treeshake.f:42-44`).
- `execute-floor`: replace the I4a bullet: "when the base's `prims.f` carries `execute-floor`, add its body beside `execute` (call the xt; if r12 < `[rbp+S0-CELL]` set r12 := S0 and push true, else push false); otherwise omit. Not a dependency."
- Files: `src/habu/kernel-x64.f`, `src/arch/x86-64/rt.f`, `test/x86-64-kernel-control.f`, `docs/x86-64.md`.
