---
title: "Infra: zed unreachable + habu-ldmx cleanup"
status: open
priority: 2
issue-type: task
created-at: "2026-07-06T00:26:46.881631+02:00"
---

2026-07-06: ssh zed times out (Orin offline - power/network unknown; JetPack untouched per the deferred decision). When it returns: (1) rm -rf ~/Work/habu-ldmx (isolated-copy leftover from the ldmatrix lane - the lane completed and committed before zed dropped; ~/Work/habu itself was verified untouched by the lane pre-drop); (2) git -C ~/Work/habu pull --ff-only to fable tip; (3) re-run maki/test.f + the device tests from the pushed tree as the standard post-merge verification that was skipped for the ldmx merge (Mac gates were green: 68/68 + full native).

UPDATE 2026-07-07 (user): zed will be unreachable for ~another week (est.
return ~2026-07-14). Stop per-wakeup SSH probes until then; resume probing
around that date. The pending-zed queue (this dot + habu-zed-ew-broadcast +
habu-zed-red-broadcast + fold-staged goldens + optimizer flip + mma-16x64-warp
+ cad-6-tune + rooflines + latency + SAXPY retirement device leg) stays parked;
nothing in it blocks host work.

PENDING-ZED ADDITION 2026-07-08 (subsystem-pkgs s3+s4): re-verify the packaged
device stack on the Orin - the 5 command blocks are recorded in full in dot
habu-maki-subsystem-pkgs s4 note (eval authoring prelude, device-golden demos,
checker ablation load, gpu cluster w/ saxpy.cubin prereq + goldens, smoke gate
leg). All files host-compile clean under qualified names; only device legs
remain.

PENDING-ZED ADDITION 2026-07-11 (autograd orchestration audit): the maki
tensor-op -> PTX-primitive-VJP LOWERING - each maki op's backward emitted as a
real device kernel and device-gradchecked, tying maki/adjoint.f adjoints to the
lib/ptx VJP surface (maki/autograd.f:5-8 marks it "later"; epic note in
habu-epic-maki-autograd marks device parity pending-zed). This is the C-vs-D
device seam; host side is complete (dot habu-maki-autograd-orchestration closed
with evidence 2026-07-11).

PENDING-ZED ADDITION 2026-07-11 (ad-dag broadened op set, fable d4668e98): the
AD DAG gained OP-MUL (`*.`, row x row via EMIT-MUL) and OP-ADD (`+.`, row x row
via EMIT-ADD) with host-gradchecked VJPs (lib/ptx/ad-dag-eval-test.f proves the
DAG SEMANTICS on host, incl. fan-out accumulation and mixed pipelines). The
DEVICE residue: emitted-PTX-equals-DAG-semantics for the NEW ops - assemble
AD-EMIT-BWD PTX for a MUL/ADD pipeline (e.g. DUP EXP MUL and DUP DUP MUL MUL)
through ptxas and gradcheck on the Orin against ADE-GRAD host references,
mirroring the softmax device run (50fb466). The primitive `*.` VJP math is
already device-proven via the older ad.f AG2 path; only the DAG emission wiring
for arbitrary MUL/ADD pipelines is unverified on device.
