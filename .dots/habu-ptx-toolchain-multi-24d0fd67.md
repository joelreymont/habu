---
title: PTX toolchain multi-artifact + softmax-gradcheck migration
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T11:09:08.077200+02:00"
---

tools/ptx/softmax-gradcheck.f still loads /tmp/softmax.cubin + /tmp/softmax-bwd.cubin (two cubins live SIMULTANEOUSLY: GC-SETUP loads both GC-MF fwd + GC-MB bwd modules, released together in GC-RELEASE). lib/ptx/toolchain.f (PTXTC) PREPARE builds a single kernel.ptx/kernel.cubin under the private root, so it cannot hold two cubins at once. Add a named-artifact capability to PTXTC (e.g. public ROOT$ + PTX-FOR/CUBIN-FOR building name-keyed paths under the root, and ASSEMBLE-TO taking explicit ptx/cubin paths; keep single-kernel PTX$/CUBIN$/ASSEMBLE as thin wrappers). Then migrate softmax-gradcheck.f to self-contained emit: emit softmax fwd (tools/ptx/softmax-cg.f, collective prelude) + softmax bwd (tools/ptx/softmax-bwd-cg.f, ad-dag prelude) to two private cubins, assemble each, load both, retire the /tmp names. Device-blocked (Orin) to fully run; land at the checker-clean bar like the other migrated device tools. Sentinel readbacks already added there.

UPDATE 2026-07-04 (Orin evidence): the deeper fix is MULTI-KERNEL MODULES, not
multi-cubin. tools/ptx/sum-cg.f emits TWO kernels (SUM_ROWS + SCATTER_ROWS),
each with its own module header, into one stream; ptxas rejects the second
header ("line 65: Parsing error near '.version 8.3'"), so the migrated
sum-launch fails its RS-PTXAS assert on device. The Mac ptx-stdlib gate never
catches this class: it asserts PTX TEXT patterns, no ptxas on macOS. Legal PTX
is one .version/.target/.address_size header + N .entry kernels per module - an
emitter capability (module-level emission: header once per stream, kernels
append). That same capability lets softmax-gradcheck load ONE module with fwd+bwd
entries, making the named-artifact PTXTC extension unnecessary for it. Also
found: RS-EMIT/SL-EMIT drop the spawned emit rc and never surface the ptxas
stderr buffer on failure (diagnostic hidden; the 0 T= assert catches but says
nothing). Fix together: module emission capability + sum-cg/sum-launch repair +
emit-rc check + ptxas-stderr surfacing + a Mac-side text regression (exactly one
.version per emitted stream). Device proof: cuda-launch/softmax/acc/redadd/
saxpy-v4-tail all PASS on Orin today; sum-launch is the one red.
