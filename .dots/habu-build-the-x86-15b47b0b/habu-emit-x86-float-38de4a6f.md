---
title: Emit x86 float primitive bodies
status: closed
priority: 2
issue-type: task
created-at: "2026-09-29T13:12:28.905416+03:00"
closed-at: "2026-09-30T18:35:00+03:00"
close-reason: "done: 14 float rows through PRIM-HIR and a hand-written f.; test/x86-64-kernel-pure.f runs the float parity cases natively and hb-x64-kernel-pure-fdot matches BFDOT on eleven doubles; x86 proof and Mac shim green, 139 images."
blocks:
  - habu-emit-and-exec-a8536cf2
  - habu-emit-x86-pure-f70fb84b
---

Problem: the 15 float primitive rows have no x86 bodies; X5 and G2 need them.
Acceptance: `f+ f- f* f/ f< f= f> f0< f0= fabs fnegate fsqrt s>f f>s f.` as x86 bodies: K5's HIR route for the ops, a kernel body for `f.` over `lib/fmt` semantics or the existing printer contract; the float parity cases run natively; the rows join the `docs/x86-64.md` kernel inventory.
Files: `src/habu/kernel-x64.f`, `test/x86-64-peer-routines.f`, `docs/x86-64.md` (kernel inventory).
Verify: ThinkPad: routine images carrying the float parity cases.
Depends: habu-emit-and-exec-a8536cf2 (C7b), habu-emit-x86-pure-f70fb84b (K5).
Route: direct.
Ownership: krait (Intel lane).
Claim: unassigned.

Preflight corrections (2026-09-30; override the lines above where they differ):
- Rows: 14 go through `PRIM-HIR` on the word model's opcodes (`hir-word.f:1263-1284`), FADD to REALINT. Each stager crosses cell arguments with BITSREAL and a real answer with REALBITS, as `elaborate.f:679-695` does. `f.` is hand-written. All 15 go in `FLOAT-ROWS,`, called at the end of `PURE,`; `KERNEL,` is unchanged. The comment at `kernel-x64.f:2818-2828` and the docs "Pure rows" line say "four hand-written"; `f.` makes five.
- Seam: `X64KHIR:OP1`/`OP2` always add a cell result (`kernel-hir-x64.f:120,181-190`), and the verifier refuses any type other than the schema's (`verify.f:644-663`). Take the result type from the schema (`HIR:OPCODE 0 IR-BUILD:SCHEMA-RESULT@`).
- `f.` twin: `habu1.f:3587-3615` BFDOT. One raw `write(1)`, not `G-OUT` (so not in `PRINTERS,`), of: `-` iff bit 63 is set; the decimal of I = fcvtzs(|x|) (at least 2^63 or inf gives MAX-N, NaN gives 0); `.`; the low six digits, zero-padded, of fcvtzs((|x| - double(I)) * 1e6), truncated; `\n`.
- Cases: the float sets are inline at `prim-parity.f:613-684`. Move them verbatim to a new `test/prim-float-cases.f`, whose header says inputs go through `s>f` and real answers through `f>s`. `prim-parity.f` includes it in their place, and `x86-64-kernel-pure.f` includes it into both images. In float mode, NN-N N-N NN-F N-F convert each input with the kernel's `s>f` row (not for subject `s>f`) and each non-flag answer with `f>s` (not for subject `f>s`). `docs/x86-64.md:733-734` ("The float sets stay in `prim-parity.f`") changes to name the new file.
- Tests: `test/x86-64-kernel-pure.f` booted images, not `peer-routines.f`. A new `hb-x64-kernel-pure-fdot` (0) `dup2`s a pipe onto fd 1 (`pipe`, `dup2`, `read` rows at `kernel-x64.f:1233,1259-1260`). Each case pushes explicit bits (never a computed NaN), runs `f.` and `read`, then checks the count and bytes with `CHECK-RCX,` (byte-compare pattern: `SAME,` in `test/x86-64-kernel-crash.f:264-301`), and the image ends with `CLOSE-IMAGE`. Bits, then text: 0 `0.000000`; $8000000000000000 `-0.000000`; $BFF8000000000000 `-1.500000`; $3FF00001FE07017C `1.000001`; 1 `0.000000`; $43DFFFFFFFFFFFFF `9223372036854774784.000000`; $43E0000000000000 `9223372036854775807.000000`; $7FF0000000000000 `9223372036854775807.775807`; $FFF0000000000000 `-9223372036854775807.775807`; $7FF8000000000000 `0.000000`; $FFF8000000000000 `-0.000000`. At most 10 checks per image: split into two -fdot images if needed.
- Files: `src/habu/kernel-x64.f`, `src/habu/kernel-hir-x64.f`, `test/prim-float-cases.f` (new), `test/prim-parity.f`, `test/x86-64-kernel-pure.f`, and `docs/x86-64.md` (Pure rows and 733-734). None is baked. No new SUITE.
- Pre-change: a booted image calling `s" f+" X64HARNESS:CALL-ROW,` dies 76 with `x64kernel: no registered body named f+`.
- Verify (ThinkPad): `bin/hb --load test/prim-parity.f` gives rc 0 with 82 rows with cases and 472 assertions (unchanged). Build `test/x86-64-kernel-pure.f` and run every image natively (0, `-negative` 21, `-fdot` 0, three `-armed` 83), plus every kernel suite's images, because the kernel grows.
