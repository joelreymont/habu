---
title: "Zed: clean stale worktrees, provision ~/Work/habu, first on-device gate run"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-14T15:16:58.882850+02:00\""
blocks:
  - habu-perf-registry-re-6be03867
---

Problem: the Orin (zed) is available again but ~/Work has six stale habu trees from June/early-July sessions (habu-ffi, habu-maki, habu-ptx, odin-habu jj clones; habu-ldmx, habu-mac-gate-test non-VCS snapshots) and ~/Work/habu's engine is Jul 5 - far behind master. habu-maki carries UNCOMMITTED work (57 files, +2310/-1121: tools/ptx/softmax-gradcheck.f rewrite + autograd-end-to dot edit) that must be preserved, not deleted. Fix: (1) fetch in each jj clone and identify truly-unpushed commits; push habu-maki's dirty tree + any unpushed commits to a rescue bookmark (or tar to ~/Work/attic) and record what was preserved; (2) remove the stale trees; (3) sync ~/Work/habu to master@origin, rebuild bin/hb via the Linux-aarch64 fixpoint (Gforth recovery if the old engine cannot boot the tree), byte-identical x2; (4) run maki/test.f + test/run.f + the ptx suites ON-DEVICE so every device-FFI SKIP leg runs real for the first time on current master; record outcomes. Acceptance: ~/Work contains only live trees + attic; nothing uncommitted was destroyed (rescue bookmark/tar listed); on-device gates green or their failures dotted. Files: zed-side only; no repo source changes expected. Verify: on-device gate outputs. Depends: none. Ownership: zed host state. Claim: agent=fable-main workspace=zed:~/Work/habu (host-side task, no repo workspace).
