---
title: "Read Triton's winning SASS for the issue-density gap"
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T10:08:34.201511+02:00"
---

Round-3 lever 2 scout (campaign habu-close-the-gb10-26b9f20e), read-only investigation. Round 2 concluded the tile is mma-issue-bound and Triton retires more FLOPs per issue slot from the same m16n8k8-class tensor core. Get ground truth: dump the SASS of Triton 3.8's winning tf32 kernel per shape on the GB10 (referee script /tmp/gemm-triton-gb10.py from the doc; TRITON_CACHE dir cubins, cuobjdump -sass, or triton's kernel.asm dict) and Habu's corresponding kernel (bin/hb emit + ptxas + cuobjdump), and compare: HMMA opcode/shape actually emitted (does sm_121a SASS fuse to wider HMMA than our m16n8k8 PTX suggests), HMMA density per loop body (HMMA count vs total instructions), LDS/STS traffic per HMMA, loop unroll depth, and register reuse pattern. Deliverable: a report (text, no doc commit) naming the concrete schedule difference and whether lever 2 is (a) a PTX-level change we can emit (wider k per mma, more unroll, operand reuse), or (b) a ptxas scheduling artifact we can only chase with different PTX shape, or (c) not reachable - kill the lever with evidence. GPU use is compile+dump only, no timing (the epilogue lane owns the GPU clock).
