---
title: Name the does> split refusal for what it is
status: closed
priority: 3
issue-type: task
created-at: "\"2026-10-02T19:07:57.015916+02:00\""
closed-at: "2026-10-14T19:44:52.945966+03:00"
close-reason: Current native feed.f validates does> cut and throws E-NFEED-CUT; compiler.f CHECK-DOES-SPLIT comment names that rule, and native-create-does.f tests the code.
---

Problem (lane 316 r4-tapename): src/compiler/native/compiler.f:288 CHECK-DOES-SPLIT throws E-NCOMP-TEXT ('definition source' too long) for a does> cut outside the source, which is an engine-state inconsistency, not a text-length fault, so its report names the wrong cause. Acceptance: its own code (or the existing internal-inconsistency code) with a message naming the cut; error-code-lint 0; a case if reachable from source, else a comment stating why not. Files: src/compiler/native/compiler.f, src/lib/errors.f.
