---
title: Inline primitive bodies in T0 as the JIT does
status: closed
priority: 1
issue-type: task
created-at: "2026-10-29T09:56:38.279578+02:00"
closed-at: "2026-10-29T12:42:17.435373+02:00"
close-reason: "Abandoned yrvwmwnm: zz run -0.01%, load +0.67%; T0's register rows already carry what the JIT's EMIT-CINLINE gave it, and it needed a new NDICT word plus a second lookup per call token (big-picture design /tmp/jerry-big/design.md)."
---

JIT: LCINLINE (src/habu/habu2.f) copies a straight-line engine primitive body in place, so the caller stays a leaf. T0 spills, bl's and saves x30 for one-cell -rot 2dup 2drop 2swap 2over, count, max, min, abs, open-rd, close. The stack words can take T0's existing wide-value path for one-cell values; the rest need front-end rows (t0.f).

Stack words: wuvppmol (jerry-t0s1) moves -rot 2dup 2drop 2swap 2over 2>r 2r> 2r@ over one-cell operands through MOVE-WIDE and the 2TOR compositions, with no call, under review. Body copy: lane jerry-t0i1 carries EMIT-CINLINE's rule as a seam op (arm64 copies, x86-64 calls) for the primitives T0 does not model. 0<> is the prelude word `0 <>`, not a primitive: neither the JIT nor this dot inlines it.
