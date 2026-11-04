---
title: "Make outer-interpret's live-task package throw pass on x86-64"
status: open
priority: 2
issue-type: task
created-at: "2026-11-03T17:22:27.666660+02:00"
---

Problem: test/outer-interpret.f PACKAGE-RECOVERY-LIVE (:1185-1199, fixture oi-pkg-live-throw.f) fails on the x86-64 engine: stdout is the expected '42\n6\n' but the run exits 79 (unmapped), stderr empty. Same on the cross-built engine of master 04a4dffa and of the does> lane (bfac3067), run on joel@pulsar-ua-106 2026-11-03 (/tmp/carl-base-x86-t/outer-interpret.f.log), so it predates that lane; arm64 passes. The fixture closes OI-T, starts a task (OI-SPIN) and throws 42; the includer catches it. Cause not yet found: reduce on x86 (does the exit come from the live task at process end, or from the package-scope recovery?). Acceptance: the case passes on x86-64 or its x86 refusal is a recorded, intended difference; outer-interpret.f runs to its end on x86. Files: src/habu/kernel-x64.f, src/habu/outer.f, test/outer-interpret.f as the cause decides. Verify: x86 cross-build (bin/hb --load src/arch/x86-64/passes.f tools/native-build.f -- <out> --target linux-x86-64), then <out> --load test/outer-interpret.f on pulsar. Depends: none (x86-64 lanes wait on the arm64 linker, DECISIONS 'x86-64 builds'). Ownership: kernel-x64.f task exit, outer.f recovery. Claim: unassigned.

Census 2026-11-04: an acceptance case of its parent; fix it in the code that stays (named above), never in the machinery the parent replaces.
