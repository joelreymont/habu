---
title: PTX toolchain multi-artifact + softmax-gradcheck migration
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T11:09:08.077200+02:00"
---

tools/ptx/softmax-gradcheck.f still loads /tmp/softmax.cubin + /tmp/softmax-bwd.cubin (two cubins live SIMULTANEOUSLY: GC-SETUP loads both GC-MF fwd + GC-MB bwd modules, released together in GC-RELEASE). lib/ptx/toolchain.f (PTXTC) PREPARE builds a single kernel.ptx/kernel.cubin under the private root, so it cannot hold two cubins at once. Add a named-artifact capability to PTXTC (e.g. public ROOT$ + PTX-FOR/CUBIN-FOR building name-keyed paths under the root, and ASSEMBLE-TO taking explicit ptx/cubin paths; keep single-kernel PTX$/CUBIN$/ASSEMBLE as thin wrappers). Then migrate softmax-gradcheck.f to self-contained emit: emit softmax fwd (tools/ptx/softmax-cg.f, collective prelude) + softmax bwd (tools/ptx/softmax-bwd-cg.f, ad-dag prelude) to two private cubins, assemble each, load both, retire the /tmp names. Device-blocked (Orin) to fully run; land at the checker-clean bar like the other migrated device tools. Sentinel readbacks already added there.
