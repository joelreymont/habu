---
title: Emit x86 float primitive bodies
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T13:12:28.905416+03:00"
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
