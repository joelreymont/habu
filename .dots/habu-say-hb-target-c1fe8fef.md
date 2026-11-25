---
title: Say HB-TARGET-LINUX? on every Linux
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T15:43:45.782985+01:00"
---

Problem: HB-TARGET-LINUX? is false on Linux x86-64 (src/os/linux-x86-64/target.f); it means Linux on arm64, while HB-TARGET-LINUX-KERNEL? means any Linux, and dots' main.f:882 reads it as any Linux, so its renames throw on x86. Acceptance: HB-TARGET-LINUX? is true on every Linux, HB-TARGET-LINUX-ARM64? is the arm64 one, HB-TARGET-LINUX-KERNEL? is gone, each of the 280 uses in src, tools, test and docs is the one it meant, both Linux hosts and macOS stage, and the install notice names the change. Depends: the require feature and the x86 lanes (they edit many of the same test files). Claim: unassigned.
