---
title: Scaffold the x86 kernel file and booted harness
status: open
priority: 2
issue-type: task
created-at: "2026-09-30T09:22:04.219804+03:00"
---

Problem: K6-K9 would each create `src/habu/kernel-x64.f` (an add/add conflict) with copies of the definer (`habu1.f:91-103`), the span guard (`PROT-GUARD:CALL`, `habu1.f:412-427`), `LPROTREC` (`habu1.f:4027-4028`) and `B-TASK-LIVE-GUARD` (`habu1.f:1401-1405`); and no test can run a kernel body: C8's `X64HARNESS:OPEN,` makes rbp a 1 KiB stack window, `CASE1,` resets r12 to rbp and `CLOSE,` pins r13-r15 (K3 `test/x86-64-peer-harness.f:121-129,133,162-165`), while every K7/K8/K9 body and half of K6's read DATA cells or move those registers.
Acceptance:
- `src/habu/kernel-x64.f`, package `X64KERNEL`; requires `lib/byte-buffer.f`, `src/habu/layout.f`, `stack-abi.f`, `primitive-registry.f`, `src/arch/x86-64/{asm,icode,rt}.f`, `src/os/linux-x86-64/sys.f` (loads before any file that privatises the seam, as `boot-x64.f` does). Publics:
  - `PRIM ( ptr u8 n [ -- ] -- )`: `ENGINE-PRIMS:SPEC-CHECK`, skip unless `KEEP?`, two labels, `ENGINE-PRIMS:ADD`, first label, body, `ret`, last label. One definer: `call` keeps the return address on the machine stack, so a body may call helpers without a frame (no `FPRIM`/`FPRIM-L` pair).
  - `PRIM-WID ( ptr u8 n [ -- ] n -- )`: twin of `FPRIM-WID` (`habu1.f:107-115`; `executable-build-enter/leave`, `habu1.f:3528-3529`).
  - `REFUSE ( ptr u8 n -- )`: registers a body that writes `hb: <name> is not in the x86-64 kernel` and LF on fd 2 and calls `exit_group(76)` (`ENGINE-PRIMS:SPEC-RC`, X7's `TARGET-UNKNOWN`); the text lies inside the record span.
  - `ENTRY-LABEL ( ptr u8 n -- label )`: the registered body's first label by name over `ENGINE-PRIMS:COUNT`/`NAME$`/`FIRST-LABEL` (public, `primitive-registry.f:43-70`); dies 76 when absent. Consumers: the harness, X4c.
  - `TASK-LIVE-GUARD, ( -- )`: `cmp qword [rbp+TASKS-LIVE-CELL], 0 / jne LTASKLIVE` (exit 79).
  - `PROT-SPAN-CALL, ( r64 r64 -- )`: addr to rdi, len to rsi (aliasing-safe as `PROT-GUARD:CALL`), `call (PROT-SPAN)`. The helper is `GUARD-SPAN`'s twin (`FRIEND-LATCH-CELL` zero passes; hull test; `BANDS-EMIT`; `GUARD:SPAN`; trap `exit_group(SEAL-VIOLATION 83)`), registered `(PROT-SPAN)` through `HELPER-REGISTER`. Clobbers rax rcx rdx rsi rdi r8-r11 only; VM registers survive.
  - `PROT-REC, ( r64 n -- )`: twin of `LPROTREC`: mprotect the `2*STACK-ABI:PAGE-BYTES` window at the register's address aligned down to `PAGE-BYTES`, prot n; the register survives.
  - `HELPERS, ( -- )` emits `(PROT-SPAN)`, `LPROTREC` and `LTASKLIVE` once; `KERNEL, ( -- )` is `HELPERS,` then `CONTROL,`; `CONTROL,` holds `s" evaluate" REFUSE` (K7's decision). Section banners for K6, K8, K9; each leaf adds its `<SECTION>,` word and appends it to `KERNEL,`.
- `test/x86-64-boot-harness.f` reopens `package X64HARNESS` (requires `src/habu/boot-x64.f`, `src/arch/x86-64/rt.f`, `src/habu/kernel-x64.f`, then `test/x86-64-peer-harness.f`, the order `test/x86-64-skel-image.f:13-15` needs):
  - `BOOT-OPEN, ( bool -- )`: `ASM-RESET`, fresh labels, `X64BOOT:START,`, `jmp ENTRY`, `X64KERNEL:KERNEL,`, `ENTRY LBL,`.
  - `PUSH, ( n -- )`; `PUSH-TEXT, ( ptr u8 n -- )` (bytes after a jump, `movabs`, push); `PUSH-SCRATCH, ( n -- )` (`lea rax, [rbp+SCRATCH-OFF+n]`, `SCRATCH-OFF` = `DATA-START + $10000`); `CELL!, ( n n -- )` (an immediate into a DATA cell: arms `FRIEND-LATCH-CELL`, `TASKS-LIVE-CELL`, `EXIT-HOOK-CELL`); `CALL-ROW, ( ptr u8 n -- )` (`call` the `ENTRY-LABEL`).
  - `EXPECT-POP, ( n -- )` (pop, `EXPECT,` semantics including `WRONG-AT`); `EXPECT-DEPTH, ( n -- )` (r12 = `[rbp+BASE-CELL]` + 8n); `EXPECT-BALANCED,` (rsp = `[rbp+ARGV-CELL]` - 8, from `boot-x64.f` `DATA-INIT,`); `EXPECT-SCRATCH, ( n n -- )`.
  - `BOOT-CLOSE, ( ptr u8 n -- )`: `edi 0`, `EXIT,`, `X64HARNESS:WRITE`. Negative images exit `FIRST-CASE` as today.
- `test/x86-64-kernel-{syscalls,control,atomics,engine}.f` (packages `X64K-SYSCALLS`, `X64K-CONTROL`, `X64K-ATOMICS`, `X64K-ENGINE`), each writing `$HB_TMP/hb-x64-kernel-<name>` and its `-negative` twin, with one kept case each:
  - syscalls: `PROT-SPAN-CALL,` passes with the latch off; a third image with `FRIEND-LATCH-CELL` armed on `[rbp+TIER-PROV:N-CELL]` exits 83 (`test/tier.f:470` is the precedent);
  - control: `evaluate` through `CALL-ROW,` exits 76 with its fd-2 line;
  - atomics: a `PUSH-SCRATCH,`/`EXPECT-SCRATCH,` round trip;
  - engine: `TASK-LIVE-GUARD,` passes, and the armed image exits 79.
  Four `SUITE` rows after `SUITE x86-64-skel-image` (K3 `test/gate-stdlib-cases.f:1268`), one suite each (the seam loads globally, as skel-image's does).
- `docs/x86-64.md` "Kernel inventory" (K3 `docs/x86-64.md:587-636`) gains `### Syscall rows`, `### Control rows`, `### Atomics and publication rows` and `### Engine-state rows`, each opening with the shared-helper and harness contract above; the leaves add their row tables.
Pre-change failing check: `fd kernel-x64.f src` is empty; `bin/hb --load test/x86-64-kernel-control.f` cannot open the file; a C8 routine reading `[rbp+TASKS-LIVE-CELL]` ($3C90) reads past the 1 KiB window (`test/x86-64-peer-harness.f:121-129`).
Files: `src/habu/kernel-x64.f`, `test/x86-64-boot-harness.f`, `test/x86-64-kernel-{syscalls,control,atomics,engine}.f`, `test/gate-stdlib-cases.f`, `docs/x86-64.md`.
Verify: host engine K3's product `264c829e…` (master's pre-K2 engine lacks `ENGINE-GPR:X64-DSTACK`): `--load` the four files; ThinkPad natively: `./hb-x64-kernel-control; echo $?` gives 76 and the fd-2 line, `-negative` 21; syscalls 0 and the latch image 83; engine 0 and 79; atomics 0. `layout.f` is untouched: no rebuild; the gate on spark.
Depends: habu-boot-and-exit-367c46f5 (K3). Base: master once K3 lands. Workspace `.jj-ws/<id>`.
Route: shared (`test/gate-stdlib-cases.f`): lands on master after the Linux gate; Alder pools the Mac gate.
Ownership: krait (Intel lane).
Claim: unassigned.
